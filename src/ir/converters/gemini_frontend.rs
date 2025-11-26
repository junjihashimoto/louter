use async_trait::async_trait;
use crate::ir::types::*;
use crate::ir::traits::{FrontendConverter, ConverterResult};
use crate::models::gemini::{
    GeminiRequest, GeminiResponse, Content, Part, GenerationConfig,
    Tool, FunctionDeclaration, ToolConfig, GeminiStreamResponse,
    InlineData, FunctionCall as GeminiFunctionCall,
};
use crate::error::ProxyError;

pub struct GeminiFrontendConverter;

impl GeminiFrontendConverter {
    pub fn new() -> Self {
        Self
    }
}

#[async_trait]
impl FrontendConverter for GeminiFrontendConverter {
    fn protocol_name(&self) -> &str {
        "gemini"
    }

    async fn parse_request(&self, request_bytes: &[u8]) -> ConverterResult<IRRequest> {
        let request: GeminiRequest = serde_json::from_slice(request_bytes)?;

        // Convert Gemini contents to IR messages
        let mut ir_messages = Vec::new();
        for content in request.contents {
            ir_messages.push(convert_gemini_content_to_ir_message(&content)?);
        }

        // Extract system instruction
        let system = request.system_instruction.map(|si| {
            si.parts.iter()
                .filter_map(|part| match part {
                    Part::Text { text } => Some(text.as_str()),
                    _ => None,
                })
                .collect::<Vec<_>>()
                .join("\n")
        });

        // Extract generation config
        let (max_tokens, temperature, top_p, top_k, stop_sequences) = if let Some(config) = request.generation_config {
            (
                config.max_output_tokens,
                config.temperature.map(|t| t as f32),
                config.top_p.map(|t| t as f32),
                config.top_k,
                config.stop_sequences.unwrap_or_default(),
            )
        } else {
            (None, None, None, None, vec![])
        };

        // Convert tools
        let tools = if let Some(gemini_tools) = request.tools {
            gemini_tools.into_iter()
                .flat_map(|tool| tool.function_declarations)
                .map(convert_gemini_function_to_ir_tool)
                .collect()
        } else {
            Vec::new()
        };

        // Convert tool config
        let tool_choice = request.tool_config.and_then(|tc| {
            match tc.function_calling_config.mode.as_str() {
                "AUTO" => Some(IRToolChoice::Auto),
                "ANY" => Some(IRToolChoice::Required),
                "NONE" => Some(IRToolChoice::None),
                _ => Some(IRToolChoice::Auto),
            }
        });

        Ok(IRRequest {
            model: "gemini".to_string(), // Will be set by handler
            messages: ir_messages,
            system,
            max_tokens,
            temperature,
            top_p,
            top_k,
            stop_sequences,
            tools,
            tool_choice,
            stream: false, // Will be determined by endpoint
            metadata: Default::default(),
        })
    }

    async fn format_response(&self, ir_response: &IRResponse) -> ConverterResult<Vec<u8>> {
        // Convert IR content to Gemini parts
        let parts = ir_response.content.iter()
            .map(convert_ir_content_to_gemini_part)
            .collect::<Result<Vec<_>, _>>()?;

        let content = Content {
            role: "model".to_string(),
            parts,
        };

        // Convert stop reason
        let finish_reason = ir_response.stop_reason.as_ref()
            .map(convert_ir_stop_reason_to_gemini);

        let response = GeminiResponse {
            candidates: vec![crate::models::gemini::Candidate {
                content,
                finish_reason,
                index: Some(0),
                safety_ratings: None,
                citation_metadata: None,
            }],
            usage_metadata: Some(crate::models::gemini::UsageMetadata {
                prompt_token_count: ir_response.usage.input_tokens,
                candidates_token_count: Some(ir_response.usage.output_tokens),
                total_token_count: ir_response.usage.input_tokens + ir_response.usage.output_tokens,
                thoughts_token_count: ir_response.usage.thinking_tokens,
            }),
        };

        serde_json::to_vec(&response).map_err(|e| ProxyError::SerializationError(e))
    }

    fn format_stream_chunk(&self, chunk: &IRStreamChunk) -> ConverterResult<String> {
        // Convert IR chunk to Gemini stream format
        // Extract common fields for all chunks
        let model_version = Some(chunk.model.clone());
        let response_id = Some(chunk.message_id.clone());

        let stream_response = match &chunk.chunk_type {
            IRChunkType::MessageStart { message: _, usage } => {
                // Initial chunk with usage
                GeminiStreamResponse {
                    candidates: vec![],
                    usage_metadata: Some(crate::models::gemini::UsageMetadata {
                        prompt_token_count: usage.input_tokens,
                        candidates_token_count: Some(0),
                        total_token_count: usage.input_tokens,
                        thoughts_token_count: None,
                    }),
                    model_version,
                    response_id,
                }
            },
            IRChunkType::ContentBlockStart { index: _, content_block } => {
                // Block start - send empty parts for now
                GeminiStreamResponse {
                    candidates: vec![crate::models::gemini::Candidate {
                        content: Content {
                            role: "model".to_string(),
                            parts: vec![],
                        },
                        finish_reason: None,
                        index: Some(0),
                        safety_ratings: None,
                        citation_metadata: None,
                    }],
                    usage_metadata: None,
                    model_version,
                    response_id,
                }
            },
            IRChunkType::ContentBlockDelta { index: _, delta } => {
                // Delta - convert to Gemini part
                let part = match delta {
                    IRDelta::TextDelta { text } => Part::Text { text: text.clone() },
                    IRDelta::InputJsonDelta { partial_json } => {
                        // For function calls, we'll send partial JSON as text for now
                        Part::Text { text: partial_json.clone() }
                    },
                    IRDelta::ThinkingDelta { thinking } => {
                        Part::Text { text: format!("[Thinking: {}]", thinking) }
                    }
                };

                GeminiStreamResponse {
                    candidates: vec![crate::models::gemini::Candidate {
                        content: Content {
                            role: "model".to_string(),
                            parts: vec![part],
                        },
                        finish_reason: None,
                        index: Some(0),
                        safety_ratings: None,
                        citation_metadata: None,
                    }],
                    usage_metadata: None,
                    model_version,
                    response_id,
                }
            },
            IRChunkType::ContentBlockStop { index: _ } => {
                // Block stop - empty response
                GeminiStreamResponse {
                    candidates: vec![],
                    usage_metadata: None,
                    model_version,
                    response_id,
                }
            },
            IRChunkType::MessageDelta { delta, usage } => {
                // Message delta with stop reason
                let finish_reason = delta.stop_reason.as_ref()
                    .map(convert_ir_stop_reason_to_gemini);

                GeminiStreamResponse {
                    candidates: vec![crate::models::gemini::Candidate {
                        content: Content {
                            role: "model".to_string(),
                            parts: vec![],
                        },
                        finish_reason,
                        index: Some(0),
                        safety_ratings: None,
                        citation_metadata: None,
                    }],
                    usage_metadata: Some(crate::models::gemini::UsageMetadata {
                        prompt_token_count: usage.input_tokens,
                        candidates_token_count: Some(usage.output_tokens),
                        total_token_count: usage.input_tokens + usage.output_tokens,
                        thoughts_token_count: usage.thinking_tokens,
                    }),
                    model_version,
                    response_id,
                }
            },
            IRChunkType::MessageStop => {
                // Final stop
                GeminiStreamResponse {
                    candidates: vec![],
                    usage_metadata: None,
                    model_version,
                    response_id,
                }
            },
            IRChunkType::Ping => {
                return Ok(String::new()); // Skip ping
            },
            IRChunkType::Error { error } => {
                return Err(ProxyError::ConversionError(format!("Stream error: {}", error)));
            },
        };

        // Gemini uses JSON lines format (not SSE)
        let json = serde_json::to_string(&stream_response)?;
        Ok(json)
    }

    fn validate_request(&self, request: &IRRequest) -> ConverterResult<()> {
        // Gemini-specific validations
        if request.messages.is_empty() {
            return Err(ProxyError::InvalidRequest("Messages cannot be empty".to_string()));
        }
        Ok(())
    }
}

// Helper functions

fn convert_gemini_content_to_ir_message(content: &Content) -> Result<IRMessage, ProxyError> {
    let role = match content.role.as_str() {
        "user" => IRRole::User,
        "model" => IRRole::Assistant,
        _ => IRRole::User, // Default to user
    };

    let ir_content = content.parts.iter()
        .map(convert_gemini_part_to_ir_content)
        .collect::<Result<Vec<_>, _>>()?;

    Ok(IRMessage {
        role,
        content: ir_content,
        name: None,
    })
}

fn convert_gemini_part_to_ir_content(part: &Part) -> Result<IRContent, ProxyError> {
    match part {
        Part::Text { text } => Ok(IRContent::Text { text: text.clone() }),
        Part::InlineData { inline_data } => Ok(IRContent::Image {
            source: IRImageSource::Base64 {
                media_type: inline_data.mime_type.clone(),
                data: inline_data.data.clone(),
            },
            detail: None,
        }),
        Part::FunctionCall { function_call } => Ok(IRContent::ToolUse {
            id: uuid::Uuid::new_v4().to_string(),
            name: function_call.name.clone(),
            input: function_call.args.clone(),
        }),
        Part::FunctionResponse { function_response } => {
            let tool_use_id = if let Some(id) = &function_response.id {
                id.clone()
            } else {
                String::new()
            };
            Ok(IRContent::ToolResult {
                tool_use_id,
                content: function_response.response.to_string(),
                is_error: None,
            })
        },
    }
}

fn convert_gemini_function_to_ir_tool(func: FunctionDeclaration) -> IRTool {
    // Try parameters_json_schema first, then fall back to parameters
    let mut input_schema = func.parameters_json_schema
        .or(func.parameters)
        .unwrap_or(serde_json::json!({}));

    // Convert Protobuf Schema (numeric type enums) to JSON Schema (string types)
    convert_protobuf_schema_to_json_schema(&mut input_schema);

    IRTool {
        name: func.name,
        description: func.description,
        input_schema,
    }
}

/// Converts Protobuf Schema format to JSON Schema format
/// Protobuf uses numeric type enums: 1=STRING, 2=NUMBER, 3=INTEGER, 4=BOOLEAN, 5=ARRAY, 6=OBJECT
/// JSON Schema uses string types: "string", "number", "integer", "boolean", "array", "object"
fn convert_protobuf_schema_to_json_schema(schema: &mut serde_json::Value) {
    match schema {
        serde_json::Value::Object(map) => {
            // Convert "type" field from number to string
            if let Some(type_val) = map.get("type") {
                if let Some(type_num) = type_val.as_i64() {
                    let type_str = match type_num {
                        1 => "string",
                        2 => "number",
                        3 => "integer",
                        4 => "boolean",
                        5 => "array",
                        6 => "object",
                        _ => "string", // default
                    };
                    map.insert("type".to_string(), serde_json::Value::String(type_str.to_string()));
                }
            }

            // Recursively process nested objects
            for (_, value) in map.iter_mut() {
                convert_protobuf_schema_to_json_schema(value);
            }
        }
        serde_json::Value::Array(arr) => {
            // Recursively process array elements
            for item in arr.iter_mut() {
                convert_protobuf_schema_to_json_schema(item);
            }
        }
        _ => {}
    }
}

fn convert_ir_content_to_gemini_part(ir_content: &IRContent) -> Result<Part, ProxyError> {
    match ir_content {
        IRContent::Text { text } => Ok(Part::Text { text: text.clone() }),
        IRContent::Image { source, detail: _ } => {
            match source {
                IRImageSource::Base64 { media_type, data } => Ok(Part::InlineData {
                    inline_data: InlineData {
                        mime_type: media_type.clone(),
                        data: data.clone(),
                    },
                }),
                IRImageSource::Url { url: _ } => {
                    Err(ProxyError::ConversionError("Gemini doesn't support image URLs".to_string()))
                }
            }
        },
        IRContent::ToolUse { id: _, name, input } => Ok(Part::FunctionCall {
            function_call: GeminiFunctionCall {
                name: name.clone(),
                args: input.clone(),
            },
        }),
        IRContent::ToolResult { tool_use_id, content, is_error: _ } => {
            Ok(Part::FunctionResponse {
                function_response: crate::models::gemini::FunctionResponse {
                    id: Some(tool_use_id.clone()),
                    name: "unknown".to_string(), // Gemini requires name but IR doesn't store it
                    response: serde_json::json!({ "result": content }),
                },
            })
        },
        IRContent::Thinking { thinking } => {
            // Gemini doesn't have explicit thinking, represent as text
            Ok(Part::Text { text: format!("[Thinking: {}]", thinking) })
        },
        IRContent::Audio { source: _, .. } |
        IRContent::Video { source: _, .. } |
        IRContent::Document { source: _, .. } => {
            Err(ProxyError::ConversionError("Audio/Video/Document not yet implemented".to_string()))
        }
    }
}

fn convert_ir_stop_reason_to_gemini(reason: &IRStopReason) -> String {
    match reason {
        IRStopReason::EndTurn => "STOP".to_string(),
        IRStopReason::MaxTokens => "MAX_TOKENS".to_string(),
        IRStopReason::StopSequence => "STOP".to_string(),
        IRStopReason::ToolUse => "STOP".to_string(), // Gemini doesn't have explicit tool stop
    }
}
