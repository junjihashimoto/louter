// OpenAI Backend Converter
//
// Converts Internal Representation (IR) to/from OpenAI API format

use crate::error::ProxyError;
use crate::ir::{
    IRRequest, IRResponse, IRMessage, IRContent, IRRole, IRStopReason,
    IRUsage, IRTool, IRToolChoice, IRStreamChunk, IRChunkType,
    IRContentBlockStart, IRDelta, IRMessageDelta, IRImageSource,
    BackendConverter, ConverterResult,
};
use crate::models::openai::{
    OpenAIRequest, OpenAIResponse, Message,
    MessageContent as OpenAIMessageContent, ContentPart, ImageUrl,
    Tool as OpenAITool, Function, ToolCall, FunctionCall, ToolChoice as OpenAIToolChoice,
    Usage, Choice, ResponseMessage, FunctionChoice,
    OpenAIStreamResponse, StreamChoice, Delta, StreamingToolCall,
};
use async_trait::async_trait;

/// Converter for OpenAI API (backend)
pub struct OpenAIBackendConverter;

impl OpenAIBackendConverter {
    pub fn new() -> Self {
        Self
    }
}

#[async_trait]
impl BackendConverter for OpenAIBackendConverter {
    fn protocol_name(&self) -> &str {
        "openai"
    }

    async fn format_request(&self, ir_request: &IRRequest) -> ConverterResult<Vec<u8>> {
        // Convert IR messages to OpenAI format
        let mut messages = Vec::new();

        // Add system message if present
        if let Some(system) = &ir_request.system {
            messages.push(Message::System {
                role: "system".to_string(),
                content: system.clone(),
            });
        }

        // Convert user/assistant messages
        for msg in &ir_request.messages {
            messages.extend(convert_message_to_openai(msg)?);
        }

        // Convert tools
        let tools = if !ir_request.tools.is_empty() {
            Some(ir_request.tools.iter().map(convert_tool_to_openai).collect())
        } else {
            None
        };

        // Convert tool choice
        let tool_choice = ir_request.tool_choice.as_ref()
            .map(convert_tool_choice_to_openai);

        // Build OpenAI request
        let request = OpenAIRequest {
            model: ir_request.model.clone(),
            messages,
            temperature: ir_request.temperature.map(|t| t as f64),
            top_p: ir_request.top_p.map(|t| t as f64),
            n: None,
            stream: Some(ir_request.stream),
            stop: if ir_request.stop_sequences.is_empty() {
                None
            } else {
                Some(ir_request.stop_sequences.clone())
            },
            max_tokens: ir_request.max_tokens,
            max_completion_tokens: None,
            presence_penalty: None,
            frequency_penalty: None,
            logit_bias: None,
            user: None,
            tools,
            tool_choice,
        };

        // Serialize to JSON
        serde_json::to_vec(&request)
            .map_err(|e| ProxyError::SerializationError(e))
    }

    async fn parse_response(&self, response_bytes: &[u8]) -> ConverterResult<IRResponse> {
        // Parse OpenAI response
        let response: OpenAIResponse = serde_json::from_slice(response_bytes)
            .map_err(|e| ProxyError::SerializationError(e))?;

        // Get first choice
        let choice = response.choices.first()
            .ok_or_else(|| ProxyError::ConversionError("No choices in OpenAI response".to_string()))?;

        // Convert message content
        let content = convert_openai_message_to_ir_content(&choice.message)?;

        // Convert finish reason
        let stop_reason = choice.finish_reason.as_ref()
            .and_then(|r| convert_finish_reason_to_ir(r));

        // Convert usage
        let usage = IRUsage {
            input_tokens: response.usage.prompt_tokens,
            output_tokens: response.usage.completion_tokens,
            cache_creation_input_tokens: None,
            cache_read_input_tokens: None,
            thinking_tokens: None,
        };

        Ok(IRResponse {
            id: response.id,
            model: response.model,
            role: IRRole::Assistant,
            content,
            stop_reason,
            usage,
            metadata: Default::default(),
        })
    }

    fn parse_stream_chunk(&self, event_data: &[u8]) -> ConverterResult<Option<IRStreamChunk>> {
        // Parse SSE data line
        let data_str = std::str::from_utf8(event_data)
            .map_err(|e| ProxyError::ConversionError(format!("Invalid UTF-8: {}", e)))?;

        // Skip [DONE] marker
        if data_str.trim() == "[DONE]" {
            return Ok(None);
        }

        // Parse JSON
        let chunk: OpenAIStreamResponse = serde_json::from_str(data_str)
            .map_err(|e| ProxyError::SerializationError(e))?;

        // Get first choice
        let choice = match chunk.choices.first() {
            Some(c) => c,
            None => return Ok(None),
        };

        // Determine chunk type based on delta content
        let chunk_type = if choice.finish_reason.is_some() {
            // Message end
            let stop_reason = choice.finish_reason.as_ref()
                .and_then(|r| convert_finish_reason_to_ir(r));

            IRChunkType::MessageDelta {
                delta: IRMessageDelta {
                    stop_reason,
                    stop_sequence: None,
                },
                usage: IRUsage::default(),
            }
        } else if let Some(content) = &choice.delta.content {
            // Text delta
            IRChunkType::ContentBlockDelta {
                index: 0,
                delta: IRDelta::TextDelta {
                    text: content.clone(),
                },
            }
        } else if let Some(tool_calls) = &choice.delta.tool_calls {
            // Tool call delta
            if let Some(tool_call) = tool_calls.first() {
                if let Some(name) = &tool_call.function.name {
                    // Start of tool call
                    IRChunkType::ContentBlockStart {
                        index: tool_call.index.unwrap_or(0),
                        content_block: IRContentBlockStart::ToolUse {
                            id: tool_call.id.clone().unwrap_or_default(),
                            name: name.clone(),
                        },
                    }
                } else if !tool_call.function.arguments.is_empty() {
                    // Tool call arguments delta
                    IRChunkType::ContentBlockDelta {
                        index: tool_call.index.unwrap_or(0),
                        delta: IRDelta::InputJsonDelta {
                            partial_json: tool_call.function.arguments.clone(),
                        },
                    }
                } else {
                    return Ok(None);
                }
            } else {
                return Ok(None);
            }
        } else if choice.delta.role.is_some() {
            // First chunk - message start
            IRChunkType::MessageStart {
                message: IRMessage {
                    role: IRRole::Assistant,
                    content: Vec::new(),
                    name: None,
                },
                usage: IRUsage::default(),
            }
        } else {
            return Ok(None);
        };

        Ok(Some(IRStreamChunk {
            message_id: chunk.id,
            model: chunk.model,
            chunk_type,
        }))
    }

    fn required_headers(&self, api_key: &str) -> Vec<(String, String)> {
        vec![
            ("authorization".into(), format!("Bearer {}", api_key)),
            ("content-type".into(), "application/json".into()),
        ]
    }

    fn endpoint_url(&self, base_url: &str, _streaming: bool) -> String {
        format!("{}/v1/chat/completions", base_url)
    }
}

// ============================================================================
// Helper conversion functions
// ============================================================================

fn convert_message_to_openai(msg: &IRMessage) -> ConverterResult<Vec<Message>> {
    let mut messages = Vec::new();
    let mut text_parts = Vec::new();
    let mut tool_calls = Vec::new();
    let mut tool_results = Vec::new();

    // Separate content types
    for block in &msg.content {
        match block {
            IRContent::Text { text } => {
                text_parts.push(text.clone());
            }
            IRContent::ToolUse { id, name, input } => {
                tool_calls.push(ToolCall {
                    id: id.clone(),
                    tool_type: "function".to_string(),
                    function: FunctionCall {
                        name: name.clone(),
                        arguments: serde_json::to_string(input)
                            .unwrap_or_else(|_| "{}".to_string()),
                    },
                });
            }
            IRContent::ToolResult { tool_use_id, content, .. } => {
                tool_results.push((tool_use_id.clone(), content.clone()));
            }
            _ => {}
        }
    }

    // Create message(s) based on role
    match msg.role {
        IRRole::User => {
            let content = if text_parts.is_empty() {
                OpenAIMessageContent::Text(String::new())
            } else {
                OpenAIMessageContent::Text(text_parts.join("\n"))
            };

            messages.push(Message::User {
                role: "user".to_string(),
                content,
            });

            // Tool results as separate messages
            for (tool_call_id, result_content) in tool_results {
                messages.push(Message::Tool {
                    role: "tool".to_string(),
                    content: result_content,
                    tool_call_id,
                });
            }
        }
        IRRole::Assistant => {
            messages.push(Message::Assistant {
                role: "assistant".to_string(),
                content: if text_parts.is_empty() {
                    None
                } else {
                    Some(text_parts.join("\n"))
                },
                tool_calls: if tool_calls.is_empty() { None } else { Some(tool_calls) },
            });
        }
        IRRole::System => {
            messages.push(Message::System {
                role: "system".to_string(),
                content: text_parts.join("\n"),
            });
        }
    }

    Ok(messages)
}

fn convert_openai_message_to_ir_content(message: &ResponseMessage) -> ConverterResult<Vec<IRContent>> {
    let mut content = Vec::new();

    // Convert text content
    if let Some(text) = &message.content {
        if !text.is_empty() {
            content.push(IRContent::Text { text: text.clone() });
        }
    }

    // Convert tool calls
    if let Some(tool_calls) = &message.tool_calls {
        for tool_call in tool_calls {
            let input: serde_json::Value = serde_json::from_str(&tool_call.function.arguments)
                .unwrap_or(serde_json::json!({}));

            content.push(IRContent::ToolUse {
                id: tool_call.id.clone(),
                name: tool_call.function.name.clone(),
                input,
            });
        }
    }

    Ok(content)
}

fn convert_tool_to_openai(tool: &IRTool) -> OpenAITool {
    OpenAITool {
        tool_type: "function".to_string(),
        function: Function {
            name: tool.name.clone(),
            description: Some(tool.description.clone()),
            parameters: Some(tool.input_schema.clone()),
        },
    }
}

fn convert_tool_choice_to_openai(choice: &IRToolChoice) -> OpenAIToolChoice {
    match choice {
        IRToolChoice::Auto => OpenAIToolChoice::String("auto".to_string()),
        IRToolChoice::Required => OpenAIToolChoice::String("required".to_string()),
        IRToolChoice::None => OpenAIToolChoice::String("none".to_string()),
        IRToolChoice::Specific { name } => OpenAIToolChoice::Object {
            r#type: "function".to_string(),
            function: FunctionChoice {
                name: name.clone(),
            },
        },
    }
}

fn convert_finish_reason_to_ir(reason: &str) -> Option<IRStopReason> {
    match reason {
        "stop" => Some(IRStopReason::EndTurn),
        "length" => Some(IRStopReason::MaxTokens),
        "tool_calls" => Some(IRStopReason::ToolUse),
        "content_filter" => Some(IRStopReason::StopSequence),
        _ => None,
    }
}
