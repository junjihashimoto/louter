use serde::{Deserialize, Serialize};
use std::fs::OpenOptions;
use std::io::Write;
use std::collections::HashMap;
use chrono::Utc;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RequestResponseLog {
    pub timestamp: String,
    pub direction: String, // "client_request", "backend_request", "backend_response", "client_response", "error"
    pub format: String, // "gemini", "openai", "anthropic"
    pub endpoint: String,
    pub body: serde_json::Value,

    // HTTP metadata
    #[serde(skip_serializing_if = "Option::is_none")]
    pub method: Option<String>, // GET, POST, etc.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub status_code: Option<u16>, // 200, 404, 500, etc.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub headers: Option<HashMap<String, String>>, // Request/response headers

    // Error information
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<String>, // Error message
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error_type: Option<String>, // "parse_error", "backend_error", "conversion_error", etc.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub stack_trace: Option<String>, // Stack trace for debugging

    // Streaming information
    #[serde(skip_serializing_if = "Option::is_none")]
    pub is_streaming: Option<bool>, // Whether this is a streaming request
    #[serde(skip_serializing_if = "Option::is_none")]
    pub stream_events: Option<Vec<String>>, // List of SSE event types received

    // Performance metrics
    #[serde(skip_serializing_if = "Option::is_none")]
    pub ttft_ms: Option<f64>, // Time to first token in milliseconds
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tps: Option<f64>, // Tokens per second (output_tokens / generation_time)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub itl_ms: Option<f64>, // Inter-token latency in milliseconds
    #[serde(skip_serializing_if = "Option::is_none")]
    pub e2e_latency_ms: Option<f64>, // End-to-end latency in milliseconds
    #[serde(skip_serializing_if = "Option::is_none")]
    pub output_tokens: Option<i32>, // Total output tokens
    #[serde(skip_serializing_if = "Option::is_none")]
    pub input_tokens: Option<i32>, // Total input tokens

    // Correlation ID for request/response pairs
    #[serde(skip_serializing_if = "Option::is_none")]
    pub request_id: Option<String>, // Unique ID to correlate request/response
}

pub struct JsonLinesLogger {
    file_path: String,
}

impl JsonLinesLogger {
    pub fn new(file_path: String) -> Self {
        Self { file_path }
    }

    pub fn log(&self, entry: RequestResponseLog) -> Result<(), std::io::Error> {
        let mut file = OpenOptions::new()
            .create(true)
            .append(true)
            .open(&self.file_path)?;

        let json_line = serde_json::to_string(&entry)?;
        writeln!(file, "{}", json_line)?;
        Ok(())
    }

    pub fn log_client_request(&self, endpoint: &str, format: &str, body: &serde_json::Value) {
        let entry = RequestResponseLog {
            timestamp: Utc::now().to_rfc3339(),
            direction: "client_request".to_string(),
            format: format.to_string(),
            endpoint: endpoint.to_string(),
            body: body.clone(),
            method: None,
            status_code: None,
            headers: None,
            error: None,
            error_type: None,
            stack_trace: None,
            is_streaming: None,
            stream_events: None,
            ttft_ms: None,
            tps: None,
            itl_ms: None,
            e2e_latency_ms: None,
            output_tokens: None,
            input_tokens: None,
            request_id: None,
        };
        if let Err(e) = self.log(entry) {
            eprintln!("Failed to log client request: {}", e);
        }
    }

    /// Log client request with full details (method, headers, request_id)
    pub fn log_client_request_detailed(
        &self,
        endpoint: &str,
        format: &str,
        body: &serde_json::Value,
        method: &str,
        headers: HashMap<String, String>,
        request_id: String,
        is_streaming: bool,
    ) {
        let entry = RequestResponseLog {
            timestamp: Utc::now().to_rfc3339(),
            direction: "client_request".to_string(),
            format: format.to_string(),
            endpoint: endpoint.to_string(),
            body: body.clone(),
            method: Some(method.to_string()),
            status_code: None,
            headers: Some(headers),
            error: None,
            error_type: None,
            stack_trace: None,
            is_streaming: Some(is_streaming),
            stream_events: None,
            ttft_ms: None,
            tps: None,
            itl_ms: None,
            e2e_latency_ms: None,
            output_tokens: None,
            input_tokens: None,
            request_id: Some(request_id),
        };
        if let Err(e) = self.log(entry) {
            eprintln!("Failed to log client request: {}", e);
        }
    }

    pub fn log_backend_request(&self, endpoint: &str, format: &str, body: &serde_json::Value) {
        let entry = RequestResponseLog {
            timestamp: Utc::now().to_rfc3339(),
            direction: "backend_request".to_string(),
            format: format.to_string(),
            endpoint: endpoint.to_string(),
            body: body.clone(),
            method: None,
            status_code: None,
            headers: None,
            error: None,
            error_type: None,
            stack_trace: None,
            is_streaming: None,
            stream_events: None,
            ttft_ms: None,
            tps: None,
            itl_ms: None,
            e2e_latency_ms: None,
            output_tokens: None,
            input_tokens: None,
            request_id: None,
        };
        if let Err(e) = self.log(entry) {
            eprintln!("Failed to log backend request: {}", e);
        }
    }

    pub fn log_backend_response(&self, endpoint: &str, format: &str, body: &serde_json::Value) {
        let entry = RequestResponseLog {
            timestamp: Utc::now().to_rfc3339(),
            direction: "backend_response".to_string(),
            format: format.to_string(),
            endpoint: endpoint.to_string(),
            body: body.clone(),
            method: None,
            status_code: None,
            headers: None,
            error: None,
            error_type: None,
            stack_trace: None,
            is_streaming: None,
            stream_events: None,
            ttft_ms: None,
            tps: None,
            itl_ms: None,
            e2e_latency_ms: None,
            output_tokens: None,
            input_tokens: None,
            request_id: None,
        };
        if let Err(e) = self.log(entry) {
            eprintln!("Failed to log backend response: {}", e);
        }
    }

    pub fn log_client_response(&self, endpoint: &str, format: &str, body: &serde_json::Value) {
        let entry = RequestResponseLog {
            timestamp: Utc::now().to_rfc3339(),
            direction: "client_response".to_string(),
            format: format.to_string(),
            endpoint: endpoint.to_string(),
            body: body.clone(),
            method: None,
            status_code: None,
            headers: None,
            error: None,
            error_type: None,
            stack_trace: None,
            is_streaming: None,
            stream_events: None,
            ttft_ms: None,
            tps: None,
            itl_ms: None,
            e2e_latency_ms: None,
            output_tokens: None,
            input_tokens: None,
            request_id: None,
        };
        if let Err(e) = self.log(entry) {
            eprintln!("Failed to log client response: {}", e);
        }
    }

    pub fn log_client_response_with_metrics(
        &self,
        endpoint: &str,
        format: &str,
        body: &serde_json::Value,
        ttft_ms: Option<f64>,
        e2e_latency_ms: Option<f64>,
        output_tokens: Option<i32>,
        input_tokens: Option<i32>,
    ) {
        // Calculate TPS and ITL from the provided metrics
        // TPS = output_tokens / generation_time
        // For streaming: generation_time = e2e - ttft
        // For non-streaming: use total e2e time
        let tps = if let (Some(tokens), Some(e2e)) = (output_tokens, e2e_latency_ms) {
            if let Some(ttft) = ttft_ms {
                let generation_time_ms = e2e - ttft;
                if generation_time_ms > 0.001 { // > 1 microsecond
                    // Streaming: use decode time only
                    Some(tokens as f64 / (generation_time_ms / 1000.0))
                } else {
                    // Non-streaming: use total e2e time
                    Some(tokens as f64 / (e2e / 1000.0))
                }
            } else {
                // No TTFT, use total e2e time
                Some(tokens as f64 / (e2e / 1000.0))
            }
        } else {
            None
        };

        let itl_ms = if let (Some(tokens), Some(e2e), Some(ttft)) = (output_tokens, e2e_latency_ms, ttft_ms) {
            if tokens > 1 {
                Some((e2e - ttft) / (tokens as f64 - 1.0))
            } else {
                None
            }
        } else {
            None
        };

        let entry = RequestResponseLog {
            timestamp: Utc::now().to_rfc3339(),
            direction: "client_response".to_string(),
            format: format.to_string(),
            endpoint: endpoint.to_string(),
            body: body.clone(),
            method: None,
            status_code: None,
            headers: None,
            error: None,
            error_type: None,
            stack_trace: None,
            is_streaming: None,
            stream_events: None,
            ttft_ms,
            tps,
            itl_ms,
            e2e_latency_ms,
            output_tokens,
            input_tokens,
            request_id: None,
        };
        if let Err(e) = self.log(entry) {
            eprintln!("Failed to log client response: {}", e);
        }
    }

    /// Log an error with full context
    pub fn log_error(
        &self,
        endpoint: &str,
        format: &str,
        error_message: &str,
        error_type: &str,
        request_body: Option<&serde_json::Value>,
        request_id: Option<String>,
        status_code: Option<u16>,
    ) {
        let body = request_body.cloned().unwrap_or(serde_json::json!({}));
        let entry = RequestResponseLog {
            timestamp: Utc::now().to_rfc3339(),
            direction: "error".to_string(),
            format: format.to_string(),
            endpoint: endpoint.to_string(),
            body,
            method: None,
            status_code,
            headers: None,
            error: Some(error_message.to_string()),
            error_type: Some(error_type.to_string()),
            stack_trace: None, // Could add backtrace here
            is_streaming: None,
            stream_events: None,
            ttft_ms: None,
            tps: None,
            itl_ms: None,
            e2e_latency_ms: None,
            output_tokens: None,
            input_tokens: None,
            request_id,
        };
        if let Err(e) = self.log(entry) {
            eprintln!("Failed to log error: {}", e);
        }
    }

    /// Log streaming event sequence for debugging
    pub fn log_stream_events(
        &self,
        endpoint: &str,
        format: &str,
        events: Vec<String>,
        request_id: Option<String>,
    ) {
        let entry = RequestResponseLog {
            timestamp: Utc::now().to_rfc3339(),
            direction: "stream_events".to_string(),
            format: format.to_string(),
            endpoint: endpoint.to_string(),
            body: serde_json::json!({"event_count": events.len()}),
            method: None,
            status_code: None,
            headers: None,
            error: None,
            error_type: None,
            stack_trace: None,
            is_streaming: Some(true),
            stream_events: Some(events),
            ttft_ms: None,
            tps: None,
            itl_ms: None,
            e2e_latency_ms: None,
            output_tokens: None,
            input_tokens: None,
            request_id,
        };
        if let Err(e) = self.log(entry) {
            eprintln!("Failed to log stream events: {}", e);
        }
    }
}
