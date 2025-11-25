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
    Usage, Choice, ResponseMessage,
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
            messages.push(ChatMessage {
                role: "system".to_string(),
                content: Some(OpenAIMessageContent::Text(system.clone())),
                name: None,
                tool_calls: None,
                tool_call_id: None,
            });
        }

        // Convert user/assistant messages
        for msg in &ir_request.messages {
            messages.push(convert_message_to_openai(&msg)?);
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
        let request = ChatCompletionRequest {
            model: ir_request.model.clone(),
            messages,
            temperature: ir_request.temperature,
            top_p: ir_request.top_p,
            n: None,
            stream: Some(ir_request.stream),
            stop: if ir_request.stop_sequences.is_empty() {
                None
            } else {
                Some(ir_request.stop_sequences.clone())
            },
            max_tokens: ir_request.max_tokens,
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
        let response: ChatCompletionResponse = serde_json::from_slice(response_bytes)
            .map_err(|e| ProxyError::SerializationError(e))?;

        // Get first choice (we only support single choice for now)
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
        let chunk: ChatCompletionChunk = serde_json::from_str(data_str)
            .map_err(|e| ProxyError::SerializationError(e))?;

        // Get first choice
        let choice = match chunk.choices.first() {
            Some(c) => c,
            None => return Ok(None), // Empty chunk
        };

        // Determine chunk type based on delta content
        let chunk_type = if choice.finish_reason.is_some() {
            // Message end
            let stop_reason = choice.finish_reason.as_ref()
                .and_then(|r| convert_finish_reason_to_ir(r));

            let usage = IRUsage {
                input_tokens: 0,
                output_tokens: 0,
                cache_creation_input_tokens: None,
                cache_read_input_tokens: None,
                thinking_tokens: None,
            };

            IRChunkType::MessageDelta {
                delta: IRMessageDelta {
                    stop_reason,
                    stop_sequence: None,
                },
                usage,
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
                if let Some(function) = &tool_call.function {
                    if function.name.is_some() {
                        // Start of tool call
                        IRChunkType::ContentBlockStart {
                            index: tool_call.index.unwrap_or(0),
                            content_block: IRContentBlockStart::ToolUse {
                                id: tool_call.id.clone().unwrap_or_default(),
                                name: function.name.clone().unwrap_or_default(),
                            },
                        }
                    } else if let Some(args) = &function.arguments {
                        // Tool call arguments delta
                        IRChunkType::ContentBlockDelta {
                            index: tool_call.index.unwrap_or(0),
                            delta: IRDelta::InputJsonDelta {
                                partial_json: args.clone(),
                            },
                        }
                    } else {
                        return Ok(None); // Empty delta
                    }
                } else {
                    return Ok(None);
                }
            } else {
                return Ok(None);
            }
        } else {
            // First chunk - message start
            let usage = IRUsage::default();
            let message = IRMessage {
                role: IRRole::Assistant,
                content: Vec::new(),
                name: None,
            };

            IRChunkType::MessageStart { message, usage }
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

fn convert_message_to_openai(msg: &IRMessage) -> ConverterResult<ChatMessage> {
    let role = match msg.role {
        IRRole::User => "user",
        IRRole::Assistant => "assistant",
        IRRole::System => "system",
    };

    // Convert content
    let (content, tool_calls) = convert_ir_content_to_openai(&msg.content)?;

    Ok(ChatMessage {
        role: role.to_string(),
        content,
        name: msg.name.clone(),
        tool_calls,
        tool_call_id: None,
    })
}

fn convert_ir_content_to_openai(
    content: &[IRContent],
) -> ConverterResult<(Option<OpenAIMessageContent>, Option<Vec<ToolCall>>)> {
    let mut text_parts = Vec::new();
    let mut image_parts = Vec::new();
    let mut tool_calls = Vec::new();

    for block in content {
        match block {
            IRContent::Text { text } => {
                text_parts.push(text.clone());
            }
            IRContent::Image { source, detail } => {
                let url = match source {
                    IRImageSource::Url { url } => url.clone(),
                    IRImageSource::Base64 { media_type, data } => {
                        format!("data:{};base64,{}", media_type, data)
                    }
                };
                image_parts.push(ContentPart::ImageUrl {
                    image_url: ImageUrl {
                        url,
                        detail: detail.clone(),
                    },
                });
            }
            IRContent::ToolUse { id, name, input } => {
                tool_calls.push(ToolCall {
                    id: id.clone(),
                    r#type: "function".to_string(),
                    function: ToolFunction {
                        name: name.clone(),
                        arguments: serde_json::to_string(input)
                            .map_err(|e| ProxyError::SerializationError(e))?,
                    },
                    index: None,
                });
            }
            IRContent::ToolResult { tool_use_id, content, .. } => {
                // Tool results go in a separate message
                // For now, convert to text
                text_parts.push(format!("Tool result ({}): {}", tool_use_id, content));
            }
            _ => {
                // Skip unsupported content types
            }
        }
    }

    // Build content
    let content = if !image_parts.is_empty() || text_parts.len() > 1 {
        // Multi-part content
        let mut parts = Vec::new();
        for text in text_parts {
            parts.push(ContentPart::Text { text });
        }
        parts.extend(image_parts);
        Some(OpenAIMessageContent::Parts(parts))
    } else if text_parts.len() == 1 {
        // Simple text content
        Some(OpenAIMessageContent::Text(text_parts[0].clone()))
    } else if !tool_calls.is_empty() {
        // Tool calls with no text
        None
    } else {
        // Empty content
        Some(OpenAIMessageContent::Text(String::new()))
    };

    let tool_calls = if tool_calls.is_empty() {
        None
    } else {
        Some(tool_calls)
    };

    Ok((content, tool_calls))
}

fn convert_openai_message_to_ir_content(message: &ChatMessage) -> ConverterResult<Vec<IRContent>> {
    let mut content = Vec::new();

    // Convert text content
    if let Some(msg_content) = &message.content {
        match msg_content {
            OpenAIMessageContent::Text(text) => {
                content.push(IRContent::Text { text: text.clone() });
            }
            OpenAIMessageContent::Parts(parts) => {
                for part in parts {
                    match part {
                        ContentPart::Text { text } => {
                            content.push(IRContent::Text { text: text.clone() });
                        }
                        ContentPart::ImageUrl { image_url } => {
                            // Parse data URL or regular URL
                            let source = if image_url.url.starts_with("data:") {
                                // Parse base64 data URL
                                let parts: Vec<&str> = image_url.url.splitn(2, ',').collect();
                                if parts.len() == 2 {
                                    let header = parts[0].trim_start_matches("data:");
                                    let media_type = header.split(';').next().unwrap_or("image/png");
                                    IRImageSource::Base64 {
                                        media_type: media_type.to_string(),
                                        data: parts[1].to_string(),
                                    }
                                } else {
                                    IRImageSource::Url { url: image_url.url.clone() }
                                }
                            } else {
                                IRImageSource::Url { url: image_url.url.clone() }
                            };

                            content.push(IRContent::Image {
                                source,
                                detail: image_url.detail.clone(),
                            });
                        }
                    }
                }
            }
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
        r#type: "function".to_string(),
        function: ToolFunction {
            name: tool.name.clone(),
            arguments: serde_json::to_string(&tool.input_schema).unwrap_or_default(),
        },
    }
}

fn convert_tool_choice_to_openai(choice: &IRToolChoice) -> OpenAIToolChoice {
    match choice {
        IRToolChoice::Auto => OpenAIToolChoice::Auto,
        IRToolChoice::Required => OpenAIToolChoice::Required,
        IRToolChoice::None => OpenAIToolChoice::None,
        IRToolChoice::Specific { name } => OpenAIToolChoice::Function {
            name: name.clone(),
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
