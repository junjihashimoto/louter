// Internal Representation Types
//
// These types represent the universal concepts across all LLM APIs.
// They are designed to be protocol-agnostic and capture all necessary information.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Represents a complete request to generate content
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IRRequest {
    pub model: String,
    pub messages: Vec<IRMessage>,
    pub system: Option<String>,
    pub max_tokens: Option<i32>,
    pub temperature: Option<f32>,
    pub top_p: Option<f32>,
    pub top_k: Option<i32>,
    pub stop_sequences: Vec<String>,
    pub tools: Vec<IRTool>,
    pub tool_choice: Option<IRToolChoice>,
    pub stream: bool,
    pub metadata: IRRequestMetadata,
}

/// Represents a complete response from content generation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IRResponse {
    pub id: String,
    pub model: String,
    pub role: IRRole,
    pub content: Vec<IRContent>,
    pub stop_reason: Option<IRStopReason>,
    pub usage: IRUsage,
    pub metadata: IRResponseMetadata,
}

/// A single message in the conversation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IRMessage {
    pub role: IRRole,
    pub content: Vec<IRContent>,
    pub name: Option<String>,
}

/// Role of the message sender
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum IRRole {
    User,
    Assistant,
    System,
}

/// Content block within a message
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum IRContent {
    Text {
        text: String,
    },
    Image {
        source: IRImageSource,
        #[serde(skip_serializing_if = "Option::is_none")]
        detail: Option<String>,
    },
    Audio {
        source: IRAudioSource,
    },
    Video {
        source: IRVideoSource,
    },
    Document {
        source: IRDocumentSource,
        #[serde(skip_serializing_if = "Option::is_none")]
        document_type: Option<String>,
    },
    ToolUse {
        id: String,
        name: String,
        input: serde_json::Value,
    },
    ToolResult {
        tool_use_id: String,
        content: String,
        #[serde(skip_serializing_if = "Option::is_none")]
        is_error: Option<bool>,
    },
    Thinking {
        thinking: String,
    },
}

/// Image source (URL or base64 data)
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum IRImageSource {
    Url { url: String },
    Base64 { media_type: String, data: String },
}

/// Audio source
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum IRAudioSource {
    Url { url: String },
    Base64 { media_type: String, data: String },
}

/// Video source
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum IRVideoSource {
    Url { url: String },
    Base64 { media_type: String, data: String },
}

/// Document source (PDF, etc.)
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum IRDocumentSource {
    Url { url: String },
    Base64 { media_type: String, data: String },
}

/// Tool/function definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IRTool {
    pub name: String,
    pub description: String,
    pub input_schema: serde_json::Value,
}

/// Tool choice configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum IRToolChoice {
    Auto,
    Required,
    None,
    Specific { name: String },
}

/// Stop reason for generation
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum IRStopReason {
    EndTurn,
    MaxTokens,
    StopSequence,
    ToolUse,
}

/// Token usage information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IRUsage {
    pub input_tokens: i32,
    pub output_tokens: i32,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cache_creation_input_tokens: Option<i32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cache_read_input_tokens: Option<i32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub thinking_tokens: Option<i32>,
}

/// Request metadata (protocol-specific info)
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct IRRequestMetadata {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub anthropic_version: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub user_id: Option<String>,
    #[serde(flatten)]
    pub extra: HashMap<String, serde_json::Value>,
}

/// Response metadata (protocol-specific info)
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct IRResponseMetadata {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub created_at: Option<i64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub finish_reason: Option<String>,
    #[serde(flatten)]
    pub extra: HashMap<String, serde_json::Value>,
}

// ============================================================================
// Streaming Types
// ============================================================================

/// Streaming chunk (SSE event)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IRStreamChunk {
    pub message_id: String,
    pub model: String,
    pub chunk_type: IRChunkType,
}

/// Type of streaming chunk
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum IRChunkType {
    MessageStart {
        message: IRMessage,
        usage: IRUsage,
    },
    ContentBlockStart {
        index: usize,
        content_block: IRContentBlockStart,
    },
    ContentBlockDelta {
        index: usize,
        delta: IRDelta,
    },
    ContentBlockStop {
        index: usize,
    },
    MessageDelta {
        delta: IRMessageDelta,
        usage: IRUsage,
    },
    MessageStop,
    Ping,
    Error {
        error: String,
    },
}

/// Content block start (for streaming)
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum IRContentBlockStart {
    Text,
    ToolUse { id: String, name: String },
    Thinking,
}

/// Delta update (for streaming)
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum IRDelta {
    TextDelta { text: String },
    InputJsonDelta { partial_json: String },
    ThinkingDelta { thinking: String },
}

/// Message delta (for streaming)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IRMessageDelta {
    pub stop_reason: Option<IRStopReason>,
    pub stop_sequence: Option<String>,
}

// ============================================================================
// Helper implementations
// ============================================================================

impl Default for IRRequest {
    fn default() -> Self {
        Self {
            model: String::new(),
            messages: Vec::new(),
            system: None,
            max_tokens: None,
            temperature: None,
            top_p: None,
            top_k: None,
            stop_sequences: Vec::new(),
            tools: Vec::new(),
            tool_choice: None,
            stream: false,
            metadata: IRRequestMetadata::default(),
        }
    }
}

impl Default for IRUsage {
    fn default() -> Self {
        Self {
            input_tokens: 0,
            output_tokens: 0,
            cache_creation_input_tokens: None,
            cache_read_input_tokens: None,
            thinking_tokens: None,
        }
    }
}

impl IRContent {
    /// Check if content is a text block
    pub fn is_text(&self) -> bool {
        matches!(self, IRContent::Text { .. })
    }

    /// Check if content is a tool use block
    pub fn is_tool_use(&self) -> bool {
        matches!(self, IRContent::ToolUse { .. })
    }

    /// Check if content is a tool result block
    pub fn is_tool_result(&self) -> bool {
        matches!(self, IRContent::ToolResult { .. })
    }

    /// Extract text from text block
    pub fn as_text(&self) -> Option<&str> {
        match self {
            IRContent::Text { text } => Some(text),
            _ => None,
        }
    }
}

impl IRMessage {
    /// Create a new user message with text
    pub fn user(text: impl Into<String>) -> Self {
        Self {
            role: IRRole::User,
            content: vec![IRContent::Text { text: text.into() }],
            name: None,
        }
    }

    /// Create a new assistant message with text
    pub fn assistant(text: impl Into<String>) -> Self {
        Self {
            role: IRRole::Assistant,
            content: vec![IRContent::Text { text: text.into() }],
            name: None,
        }
    }

    /// Check if message has any text content
    pub fn has_text(&self) -> bool {
        self.content.iter().any(|c| c.is_text())
    }

    /// Check if message has any tool uses
    pub fn has_tool_uses(&self) -> bool {
        self.content.iter().any(|c| c.is_tool_use())
    }
}
