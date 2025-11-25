use async_trait::async_trait;
use crate::ir::types::*;
use crate::ir::traits::{FrontendConverter, ConverterResult};
use crate::models::openai::{
    OpenAIRequest, OpenAIResponse, Message, Tool as OpenAITool,
    Function, ToolCall, FunctionCall, ToolChoice as OpenAIToolChoice,
    OpenAIStreamResponse, StreamChoice, ResponseMessage,
};
use crate::error::ProxyError;

pub struct OpenAIFrontendConverter;

impl OpenAIFrontendConverter {
    pub fn new() -> Self {
        Self
    }
}

#[async_trait]
impl FrontendConverter for OpenAIFrontendConverter {
    fn protocol_name(&self) -> &str {
        "openai"
    }

    async fn parse_request(&self, request_bytes: &[u8]) -> ConverterResult<IRRequest> {
        let request: OpenAIRequest = serde_json::from_slice(request_bytes)?;

        let mut ir_messages = Vec::new();
        let mut system_instruction = None;

        // Convert OpenAI messages to IR
        for msg in request.messages {
            match msg {
                Message::System { role: _, content } => {
                    // Extract system message
                    system_instruction = Some(content);
                },
                Message::User { role: _, content } => {
                    let text = match content {
                        crate::models::openai::MessageContent::Text(s) => s,
                        crate::models::openai::MessageContent::Array(parts) => {
                            // For now, just concatenate text parts
                            parts.iter()
                                .filter_map(|p| match p {
                                    crate::models::openai::ContentPart::Text { text } => Some(text.as_str()),
                                    _ => None,
                                })
                                .collect::<Vec<_>>()
                                .join("\n")
                        }
                    };
                    ir_messages.push(IRMessage {
                        role: IRRole::User,
                        content: vec![IRContent::Text { text }],
                        name: None,
                    });
                },
                Message::Assistant { role: _, content, tool_calls } => {
                    let mut content_items = Vec::new();

                    if let Some(text) = content {
                        content_items.push(IRContent::Text { text });
                    }

                    if let Some(calls) = tool_calls {
                        for call in calls {
                            content_items.push(IRContent::ToolUse {
                                id: call.id,
                                name: call.function.name,
                                input: serde_json::from_str(&call.function.arguments)
                                    .unwrap_or(serde_json::json!({})),
                            });
                        }
                    }

                    ir_messages.push(IRMessage {
                        role: IRRole::Assistant,
                        content: content_items,
                        name: None,
                    });
                },
                Message::Tool { role: _, content, tool_call_id } => {
                    ir_messages.push(IRMessage {
                        role: IRRole::User,
                        content: vec![IRContent::ToolResult {
                            tool_use_id: tool_call_id,
                            content,
                            is_error: None,
                        }],
                        name: None,
                    });
                },
            }
        }

        // Convert tools
        let tools = if let Some(openai_tools) = request.tools {
            openai_tools.into_iter()
                .map(|t| IRTool {
                    name: t.function.name,
                    description: t.function.description.unwrap_or_default(),
                    input_schema: t.function.parameters.unwrap_or(serde_json::json!({})),
                })
                .collect()
        } else {
            Vec::new()
        };

        // Convert tool choice
        let tool_choice = request.tool_choice.and_then(|tc| match tc {
            crate::models::openai::ToolChoice::String(s) => {
                match s.as_str() {
                    "auto" => Some(IRToolChoice::Auto),
                    "required" => Some(IRToolChoice::Required),
                    "none" => Some(IRToolChoice::None),
                    _ => Some(IRToolChoice::Auto),
                }
            },
            crate::models::openai::ToolChoice::Object { r#type: _, function } => {
                Some(IRToolChoice::Specific { name: function.name })
            },
        });

        Ok(IRRequest {
            model: request.model,
            messages: ir_messages,
            system: system_instruction,
            max_tokens: request.max_tokens,
            temperature: request.temperature.map(|t| t as f32),
            top_p: request.top_p.map(|t| t as f32),
            top_k: None,
            stop_sequences: request.stop.unwrap_or_default(),
            tools,
            tool_choice,
            stream: request.stream.unwrap_or(false),
            metadata: Default::default(),
        })
    }

    async fn format_response(&self, ir_response: &IRResponse) -> ConverterResult<Vec<u8>> {
        // Convert IR content to OpenAI message
        let mut content = None;
        let mut tool_calls = Vec::new();

        for ir_content in &ir_response.content {
            match ir_content {
                IRContent::Text { text } => {
                    content = Some(text.clone());
                },
                IRContent::ToolUse { id, name, input } => {
                    tool_calls.push(ToolCall {
                        id: id.clone(),
                        tool_type: "function".to_string(),
                        function: FunctionCall {
                            name: name.clone(),
                            arguments: input.to_string(),
                        },
                    });
                },
                _ => {
                    // Skip other content types for now
                }
            }
        }

        let message = ResponseMessage {
            role: "assistant".to_string(),
            content,
            tool_calls: if tool_calls.is_empty() { None } else { Some(tool_calls) },
            reasoning_content: None,
        };

        let finish_reason = ir_response.stop_reason.as_ref().map(|sr| match sr {
            IRStopReason::EndTurn => "stop",
            IRStopReason::MaxTokens => "length",
            IRStopReason::StopSequence => "stop",
            IRStopReason::ToolUse => "tool_calls",
        }.to_string());

        let response = OpenAIResponse {
            id: ir_response.id.clone(),
            object: "chat.completion".to_string(),
            created: chrono::Utc::now().timestamp() as i64,
            model: ir_response.model.clone(),
            choices: vec![crate::models::openai::Choice {
                index: 0,
                message,
                finish_reason,
            }],
            usage: crate::models::openai::Usage {
                prompt_tokens: ir_response.usage.input_tokens,
                completion_tokens: ir_response.usage.output_tokens,
                total_tokens: ir_response.usage.input_tokens + ir_response.usage.output_tokens,
            },
        };

        serde_json::to_vec(&response).map_err(|e| ProxyError::SerializationError(e))
    }

    fn format_stream_chunk(&self, chunk: &IRStreamChunk) -> ConverterResult<String> {
        use crate::models::openai::Delta;

        let delta = match &chunk.chunk_type {
            IRChunkType::MessageStart { message: _, usage: _ } => {
                // First chunk - role only
                Delta {
                    role: Some("assistant".to_string()),
                    content: None,
                    tool_calls: None,
                    reasoning_content: None,
                }
            },
            IRChunkType::ContentBlockStart { index: _, content_block: _ } => {
                // Block start - empty delta
                Delta {
                    role: None,
                    content: None,
                    tool_calls: None,
                    reasoning_content: None,
                }
            },
            IRChunkType::ContentBlockDelta { index: _, delta } => {
                match delta {
                    IRDelta::TextDelta { text } => Delta {
                        role: None,
                        content: Some(text.clone()),
                        tool_calls: None,
                        reasoning_content: None,
                    },
                    IRDelta::InputJsonDelta { partial_json } => {
                        // For tool calls, we need to format as tool call delta
                        Delta {
                            role: None,
                            content: None,
                            tool_calls: Some(vec![crate::models::openai::StreamingToolCall {
                                index: Some(0),
                                id: None,
                                tool_type: Some("function".to_string()),
                                function: crate::models::openai::StreamingFunctionCall {
                                    name: None,
                                    arguments: partial_json.clone(),
                                },
                            }]),
                            reasoning_content: None,
                        }
                    },
                    IRDelta::ThinkingDelta { thinking } => {
                        // Map thinking to reasoning_content (o1 models)
                        Delta {
                            role: None,
                            content: None,
                            tool_calls: None,
                            reasoning_content: Some(thinking.clone()),
                        }
                    }
                }
            },
            IRChunkType::ContentBlockStop { index: _ } => {
                Delta {
                    role: None,
                    content: None,
                    tool_calls: None,
                    reasoning_content: None,
                }
            },
            IRChunkType::MessageDelta { delta: _, usage: _ } => {
                // Message delta with stop reason - no content
                Delta {
                    role: None,
                    content: None,
                    tool_calls: None,
                    reasoning_content: None,
                }
            },
            IRChunkType::MessageStop => {
                Delta {
                    role: None,
                    content: None,
                    tool_calls: None,
                    reasoning_content: None,
                }
            },
            IRChunkType::Ping => {
                return Ok(String::new()); // Skip ping events
            },
            IRChunkType::Error { error } => {
                return Err(ProxyError::ConversionError(format!("Stream error: {}", error)));
            },
        };

        let finish_reason = match &chunk.chunk_type {
            IRChunkType::MessageDelta { delta: msg_delta, usage: _ } => {
                msg_delta.stop_reason.as_ref().map(|sr| match sr {
                    IRStopReason::EndTurn => "stop",
                    IRStopReason::MaxTokens => "length",
                    IRStopReason::StopSequence => "stop",
                    IRStopReason::ToolUse => "tool_calls",
                }.to_string())
            },
            _ => None,
        };

        let stream_response = OpenAIStreamResponse {
            id: chunk.message_id.clone(),
            object: "chat.completion.chunk".to_string(),
            created: chrono::Utc::now().timestamp() as i64,
            model: chunk.model.clone(),
            choices: vec![StreamChoice {
                index: 0,
                delta,
                finish_reason,
            }],
        };

        let json = serde_json::to_string(&stream_response)?;
        Ok(format!("data: {}\n\n", json))
    }

    fn validate_request(&self, _request: &IRRequest) -> ConverterResult<()> {
        // OpenAI validation is minimal
        Ok(())
    }
}
