// Anthropic Frontend Converter
//
// Converts Anthropic API format to/from Internal Representation (IR)

use crate::error::ProxyError;
use crate::ir::{
    IRRequest, IRResponse, IRMessage, IRContent, IRRole, IRStopReason,
    IRUsage, IRTool, IRToolChoice, IRStreamChunk, IRChunkType,
    IRContentBlockStart, IRDelta, IRMessageDelta, IRImageSource,
    IRRequestMetadata, IRResponseMetadata,
    FrontendConverter, ConverterResult,
};
use crate::models::anthropic::{
    MessagesRequest, MessagesResponse, Message, MessageContent,
    ContentBlock, ResponseContentBlock, Tool, ToolChoice,
    SystemPrompt, Usage, ImageSource,
};
use async_trait::async_trait;

/// Converter for Anthropic API (frontend)
pub struct AnthropicFrontendConverter;

impl AnthropicFrontendConverter {
    pub fn new() -> Self {
        Self
    }
}

#[async_trait]
impl FrontendConverter for AnthropicFrontendConverter {
    fn protocol_name(&self) -> &str {
        "anthropic"
    }

    async fn parse_request(&self, request_bytes: &[u8]) -> ConverterResult<IRRequest> {
        // Parse JSON to Anthropic request
        let request: MessagesRequest = serde_json::from_slice(request_bytes)
            .map_err(|e| ProxyError::SerializationError(e))?;

        // Convert to IR
        let mut ir_messages = Vec::new();
        for msg in request.messages {
            ir_messages.push(convert_message_to_ir(&msg)?);
        }

        // Convert tools
        let ir_tools = request.tools.unwrap_or_default()
            .into_iter()
            .map(convert_tool_to_ir)
            .collect();

        // Convert tool choice
        let ir_tool_choice = request.tool_choice
            .map(convert_tool_choice_to_ir);

        // Convert system prompt to string
        let system = request.system.map(|s| match s {
            SystemPrompt::Text(text) => text,
            SystemPrompt::Blocks(blocks) => {
                blocks.iter()
                    .map(|b| b.text.clone())
                    .collect::<Vec<_>>()
                    .join("\n")
            }
        });

        // Build metadata
        let mut metadata = IRRequestMetadata::default();
        metadata.anthropic_version = Some("2023-06-01".to_string());

        Ok(IRRequest {
            model: request.model,
            messages: ir_messages,
            system,
            max_tokens: Some(request.max_tokens),
            temperature: request.temperature,
            top_p: request.top_p,
            top_k: request.top_k,
            stop_sequences: request.stop_sequences.unwrap_or_default(),
            tools: ir_tools,
            tool_choice: ir_tool_choice,
            stream: request.stream.unwrap_or(false),
            metadata,
        })
    }

    async fn format_response(&self, ir_response: &IRResponse) -> ConverterResult<Vec<u8>> {
        // Convert IR to Anthropic response
        let content = ir_response.content.iter()
            .map(convert_content_from_ir)
            .collect::<Result<Vec<_>, _>>()?;

        let stop_reason = ir_response.stop_reason
            .map(convert_stop_reason_from_ir);

        let usage = Usage {
            input_tokens: ir_response.usage.input_tokens,
            output_tokens: ir_response.usage.output_tokens,
            cache_creation_input_tokens: ir_response.usage.cache_creation_input_tokens,
            cache_read_input_tokens: ir_response.usage.cache_read_input_tokens,
        };

        let response = MessagesResponse {
            id: ir_response.id.clone(),
            r#type: "message".to_string(),
            role: "assistant".to_string(),
            content,
            model: ir_response.model.clone(),
            stop_reason,
            stop_sequence: None,
            usage,
        };

        // Serialize to JSON
        serde_json::to_vec(&response)
            .map_err(|e| ProxyError::SerializationError(e))
    }

    fn format_stream_chunk(&self, chunk: &IRStreamChunk) -> ConverterResult<String> {
        // Convert IR chunk to Anthropic SSE format
        match &chunk.chunk_type {
            IRChunkType::MessageStart { message, usage } => {
                let event = serde_json::json!({
                    "type": "message_start",
                    "message": {
                        "id": chunk.message_id,
                        "type": "message",
                        "role": "assistant",
                        "content": [],
                        "model": chunk.model,
                        "stop_reason": null,
                        "stop_sequence": null,
                        "usage": {
                            "input_tokens": usage.input_tokens,
                            "output_tokens": 0,
                        }
                    }
                });
                Ok(format!("event: message_start\ndata: {}\n\n", event))
            }

            IRChunkType::ContentBlockStart { index, content_block } => {
                let content_block_json = match content_block {
                    IRContentBlockStart::Text => {
                        serde_json::json!({
                            "type": "text",
                            "text": ""
                        })
                    }
                    IRContentBlockStart::ToolUse { id, name } => {
                        serde_json::json!({
                            "type": "tool_use",
                            "id": id,
                            "name": name,
                            "input": {}
                        })
                    }
                    IRContentBlockStart::Thinking => {
                        serde_json::json!({
                            "type": "thinking",
                            "thinking": ""
                        })
                    }
                };

                let event = serde_json::json!({
                    "type": "content_block_start",
                    "index": index,
                    "content_block": content_block_json
                });
                Ok(format!("event: content_block_start\ndata: {}\n\n", event))
            }

            IRChunkType::ContentBlockDelta { index, delta } => {
                let delta_json = match delta {
                    IRDelta::TextDelta { text } => {
                        serde_json::json!({
                            "type": "text_delta",
                            "text": text
                        })
                    }
                    IRDelta::InputJsonDelta { partial_json } => {
                        serde_json::json!({
                            "type": "input_json_delta",
                            "partial_json": partial_json
                        })
                    }
                    IRDelta::ThinkingDelta { thinking } => {
                        serde_json::json!({
                            "type": "thinking_delta",
                            "thinking": thinking
                        })
                    }
                };

                let event = serde_json::json!({
                    "type": "content_block_delta",
                    "index": index,
                    "delta": delta_json
                });
                Ok(format!("event: content_block_delta\ndata: {}\n\n", event))
            }

            IRChunkType::ContentBlockStop { index } => {
                let event = serde_json::json!({
                    "type": "content_block_stop",
                    "index": index
                });
                Ok(format!("event: content_block_stop\ndata: {}\n\n", event))
            }

            IRChunkType::MessageDelta { delta, usage } => {
                let event = serde_json::json!({
                    "type": "message_delta",
                    "delta": {
                        "stop_reason": delta.stop_reason.map(convert_stop_reason_from_ir),
                        "stop_sequence": delta.stop_sequence,
                    },
                    "usage": {
                        "output_tokens": usage.output_tokens,
                    }
                });
                Ok(format!("event: message_delta\ndata: {}\n\n", event))
            }

            IRChunkType::MessageStop => {
                let event = serde_json::json!({
                    "type": "message_stop"
                });
                Ok(format!("event: message_stop\ndata: {}\n\n", event))
            }

            IRChunkType::Ping => {
                Ok("event: ping\ndata: {\"type\": \"ping\"}\n\n".to_string())
            }

            IRChunkType::Error { error } => {
                let event = serde_json::json!({
                    "type": "error",
                    "error": {
                        "type": "api_error",
                        "message": error
                    }
                });
                Ok(format!("event: error\ndata: {}\n\n", event))
            }
        }
    }
}

// ============================================================================
// Helper conversion functions
// ============================================================================

fn convert_message_to_ir(msg: &Message) -> ConverterResult<IRMessage> {
    let role = match msg.role.as_str() {
        "user" => IRRole::User,
        "assistant" => IRRole::Assistant,
        _ => return Err(ProxyError::ConversionError(format!("Unknown role: {}", msg.role))),
    };

    let content = match &msg.content {
        MessageContent::Text(text) => {
            vec![IRContent::Text { text: text.clone() }]
        }
        MessageContent::Blocks(blocks) => {
            blocks.iter()
                .map(convert_content_block_to_ir)
                .collect::<Result<Vec<_>, _>>()?
        }
    };

    Ok(IRMessage {
        role,
        content,
        name: None,
    })
}

fn convert_content_block_to_ir(block: &ContentBlock) -> ConverterResult<IRContent> {
    match block {
        ContentBlock::Text { text } => {
            Ok(IRContent::Text { text: text.clone() })
        }
        ContentBlock::Image { source } => {
            let ir_source = IRImageSource::Base64 {
                media_type: source.media_type.clone(),
                data: source.data.clone(),
            };
            Ok(IRContent::Image {
                source: ir_source,
                detail: None,
            })
        }
        ContentBlock::ToolUse { id, name, input } => {
            Ok(IRContent::ToolUse {
                id: id.clone(),
                name: name.clone(),
                input: input.clone(),
            })
        }
        ContentBlock::ToolResult { tool_use_id, content } => {
            let content_str = match content {
                Some(crate::models::anthropic::ToolResultContent::Text(t)) => t.clone(),
                Some(crate::models::anthropic::ToolResultContent::Blocks(_)) => {
                    // For now, convert blocks to empty string
                    // TODO: Handle block content properly
                    String::new()
                }
                None => String::new(),
            };
            Ok(IRContent::ToolResult {
                tool_use_id: tool_use_id.clone(),
                content: content_str,
                is_error: None,
            })
        }
    }
}

fn convert_content_from_ir(content: &IRContent) -> ConverterResult<ResponseContentBlock> {
    match content {
        IRContent::Text { text } => {
            Ok(ResponseContentBlock::Text { text: text.clone() })
        }
        IRContent::ToolUse { id, name, input } => {
            Ok(ResponseContentBlock::ToolUse {
                id: id.clone(),
                name: name.clone(),
                input: input.clone(),
            })
        }
        _ => {
            // Only Text and ToolUse are valid in responses
            Err(ProxyError::ConversionError(
                "Unsupported content type for Anthropic response".to_string()
            ))
        }
    }
}

fn convert_tool_to_ir(tool: Tool) -> IRTool {
    IRTool {
        name: tool.name,
        description: tool.description.unwrap_or_default(),
        input_schema: tool.input_schema,
    }
}

fn convert_tool_choice_to_ir(choice: ToolChoice) -> IRToolChoice {
    match choice {
        ToolChoice::Auto => IRToolChoice::Auto,
        ToolChoice::Any => IRToolChoice::Required,
        ToolChoice::Tool { name } => IRToolChoice::Specific { name },
    }
}

fn convert_stop_reason_from_ir(reason: IRStopReason) -> String {
    match reason {
        IRStopReason::EndTurn => "end_turn".to_string(),
        IRStopReason::MaxTokens => "max_tokens".to_string(),
        IRStopReason::StopSequence => "stop_sequence".to_string(),
        IRStopReason::ToolUse => "tool_use".to_string(),
    }
}
