// Converter Traits
//
// These traits define the interface for converting between protocol-specific
// formats and the internal representation (IR).

use crate::error::ProxyError;
use super::types::*;
use async_trait::async_trait;
use futures::Stream;
use std::pin::Pin;

/// Result type for converter operations
pub type ConverterResult<T> = Result<T, ProxyError>;

/// Stream of IR chunks
pub type IRChunkStream = Pin<Box<dyn Stream<Item = ConverterResult<IRStreamChunk>> + Send>>;

// ============================================================================
// Frontend Converters (Client-facing)
// ============================================================================

/// Converts from protocol-specific request format to IR
///
/// Frontend converters handle incoming requests from clients:
/// - Parse protocol-specific request format
/// - Convert to universal IR
/// - Validate required fields
#[async_trait]
pub trait FrontendConverter: Send + Sync {
    /// Protocol name (e.g., "anthropic", "openai", "gemini")
    fn protocol_name(&self) -> &str;

    /// Parse request from bytes to IR
    async fn parse_request(&self, request_bytes: &[u8]) -> ConverterResult<IRRequest>;

    /// Format IR response to protocol-specific bytes
    async fn format_response(&self, ir_response: &IRResponse) -> ConverterResult<Vec<u8>>;

    /// Format IR stream chunk to protocol-specific SSE event
    fn format_stream_chunk(&self, chunk: &IRStreamChunk) -> ConverterResult<String>;

    /// Validate request is well-formed
    fn validate_request(&self, request: &IRRequest) -> ConverterResult<()> {
        // Default validation
        if request.model.is_empty() {
            return Err(ProxyError::InvalidRequest("model is required".into()));
        }
        if request.messages.is_empty() {
            return Err(ProxyError::InvalidRequest("messages cannot be empty".into()));
        }
        Ok(())
    }
}

// ============================================================================
// Backend Converters (Server-facing)
// ============================================================================

/// Converts from IR to protocol-specific backend format
///
/// Backend converters handle outgoing requests to backend services:
/// - Convert IR to protocol-specific format
/// - Send to backend API
/// - Parse response back to IR
#[async_trait]
pub trait BackendConverter: Send + Sync {
    /// Protocol name (e.g., "anthropic", "openai", "gemini")
    fn protocol_name(&self) -> &str;

    /// Format IR request to protocol-specific bytes
    async fn format_request(&self, ir_request: &IRRequest) -> ConverterResult<Vec<u8>>;

    /// Parse protocol-specific response bytes to IR
    async fn parse_response(&self, response_bytes: &[u8]) -> ConverterResult<IRResponse>;

    /// Parse protocol-specific SSE event to IR chunk
    fn parse_stream_chunk(&self, event_data: &[u8]) -> ConverterResult<Option<IRStreamChunk>>;

    /// Get required headers for this backend
    fn required_headers(&self, api_key: &str) -> Vec<(String, String)> {
        vec![
            ("authorization".into(), format!("Bearer {}", api_key)),
            ("content-type".into(), "application/json".into()),
        ]
    }

    /// Get backend endpoint URL
    fn endpoint_url(&self, base_url: &str, streaming: bool) -> String {
        format!("{}/{}", base_url, if streaming { "stream" } else { "chat" })
    }
}

// ============================================================================
// Converter Factory
// ============================================================================

/// Factory for creating protocol converters
pub trait ConverterFactory {
    /// Create frontend converter for given protocol
    fn create_frontend(&self, protocol: &str) -> ConverterResult<Box<dyn FrontendConverter>>;

    /// Create backend converter for given protocol
    fn create_backend(&self, protocol: &str) -> ConverterResult<Box<dyn BackendConverter>>;
}

// ============================================================================
// Helper Traits
// ============================================================================

/// Trait for types that can be converted to/from IR
pub trait ToIR {
    type IR;
    fn to_ir(&self) -> ConverterResult<Self::IR>;
}

pub trait FromIR<T> {
    fn from_ir(ir: &T) -> ConverterResult<Self>
    where
        Self: Sized;
}

// ============================================================================
// Stream Conversion Helpers
// ============================================================================

/// Convert a stream of protocol-specific events to IR chunks
pub fn convert_stream_to_ir<S, C>(
    stream: S,
    converter: std::sync::Arc<C>,
) -> IRChunkStream
where
    S: Stream<Item = Result<bytes::Bytes, reqwest::Error>> + Send + 'static,
    C: BackendConverter + 'static,
{
    use futures::StreamExt;

    Box::pin(stream.filter_map(move |result| {
        let converter = converter.clone();
        async move {
            match result {
                Ok(bytes) => {
                    match converter.parse_stream_chunk(&bytes) {
                        Ok(Some(chunk)) => Some(Ok(chunk)),
                        Ok(None) => None, // Skip empty chunks
                        Err(e) => Some(Err(e)),
                    }
                }
                Err(e) => Some(Err(ProxyError::BackendError(e))),
            }
        }
    }))
}

/// Convert a stream of IR chunks to protocol-specific SSE events
pub fn convert_ir_to_stream<C>(
    ir_stream: IRChunkStream,
    converter: std::sync::Arc<C>,
) -> Pin<Box<dyn Stream<Item = Result<String, ProxyError>> + Send>>
where
    C: FrontendConverter + 'static,
{
    use futures::StreamExt;

    Box::pin(ir_stream.map(move |result| {
        let converter = converter.clone();
        result.and_then(|chunk| converter.format_stream_chunk(&chunk))
    }))
}
