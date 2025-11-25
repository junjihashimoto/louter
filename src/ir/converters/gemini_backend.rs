use async_trait::async_trait;
use crate::ir::types::*;
use crate::ir::traits::{BackendConverter, ConverterResult};
use crate::models::gemini::{
    GeminiRequest, GeminiResponse, Content, Part, GenerationConfig,
    Tool, FunctionDeclaration, ToolConfig, FunctionCallingConfig,
    GeminiStreamResponse, InlineData, FunctionCall as GeminiFunctionCall,
    FunctionResponse as GeminiFunctionResponse,
};
use crate::error::ProxyError;

pub struct GeminiBackendConverter;

impl GeminiBackendConverter {
    pub fn new() -> Self {
        Self
    }
}

#[async_trait]
impl BackendConverter for GeminiBackendConverter {
    fn protocol_name(&self) -> &str {
        "gemini"
    }

    async fn format_request(&self, ir_request: &IRRequest) -> ConverterResult<Vec<u8>> {
        // Convert IR messages to Gemini contents
        let mut contents = Vec::new();
        for ir_msg in &ir_request.messages {
            contents.push(convert_ir_message_to_gemini(ir_msg)?);
        }

        // System instruction as Content
        let system_instruction = ir_request.system.as_ref().map(|text| Content {
            role: "user".to_string(),
            parts: vec![Part::Text { text: text.clone() }],
        });

        // Generation config
        let generation_config = Some(GenerationConfig {
            temperature: ir_request.temperature.map(|t| t as f64),
            top_p: None,
            top_k: None,
            max_output_tokens: ir_request.max_tokens,
            stop_sequences: None,
        });

        // Tools
        let tools = if !ir_request.tools.is_empty() {
            Some(vec![Tool {
                function_declarations: ir_request.tools.iter()
                    .map(convert_ir_tool_to_gemini)
                    .collect(),
            }])
        } else {
            None
        };

        // Tool config
        let tool_config = match &ir_request.tool_choice {
            Some(IRToolChoice::Required) => Some(ToolConfig {
                function_calling_config: FunctionCallingConfig {
                    mode: "ANY".to_string(),
                },
            }),
            Some(IRToolChoice::Auto) => Some(ToolConfig {
                function_calling_config: FunctionCallingConfig {
                    mode: "AUTO".to_string(),
                },
            }),
            Some(IRToolChoice::None) => Some(ToolConfig {
                function_calling_config: FunctionCallingConfig {
                    mode: "NONE".to_string(),
                },
            }),
            Some(IRToolChoice::Specific { name: _ }) => Some(ToolConfig {
                function_calling_config: FunctionCallingConfig {
                    mode: "ANY".to_string(),
                },
            }),
            None => None,
        };

        let request = GeminiRequest {
            contents,
            system_instruction,
            safety_settings: None,
            generation_config,
            tools,
            tool_config,
        };

        serde_json::to_vec(&request).map_err(|e| ProxyError::SerializationError(e))
    }

    async fn parse_response(&self, response_bytes: &[u8]) -> ConverterResult<IRResponse> {
        let response: GeminiResponse = serde_json::from_slice(response_bytes)?;

        let candidate = response.candidates.first()
            .ok_or_else(|| ProxyError::ConversionError("No candidates in Gemini response".to_string()))?;

        // Convert content parts to IR
        let content = candidate.content.parts.iter()
            .map(convert_gemini_part_to_ir)
            .collect::<Result<Vec<_>, _>>()?;

        // Stop reason
        let stop_reason = candidate.finish_reason.as_ref()
            .and_then(|r| convert_gemini_finish_reason_to_ir(r));

        // Usage
        let usage = if let Some(usage_meta) = &response.usage_metadata {
            IRUsage {
                input_tokens: usage_meta.prompt_token_count,
                output_tokens: usage_meta.candidates_token_count.unwrap_or(0),
                cache_creation_input_tokens: None,
                cache_read_input_tokens: None,
                thinking_tokens: usage_meta.thoughts_token_count,
            }
        } else {
            IRUsage {
                input_tokens: 0,
                output_tokens: 0,
                cache_creation_input_tokens: None,
                cache_read_input_tokens: None,
                thinking_tokens: None,
            }
        };

        Ok(IRResponse {
            id: format!("gemini-{}", uuid::Uuid::new_v4()),
            model: "gemini".to_string(), // Will be overridden by caller
            role: IRRole::Assistant,
            content,
            stop_reason,
            usage,
            metadata: Default::default(),
        })
    }

    fn parse_stream_chunk(&self, event_data: &[u8]) -> ConverterResult<Option<IRStreamChunk>> {
        // Try to parse as Gemini stream response
        let stream_response: GeminiStreamResponse = match serde_json::from_slice(event_data) {
            Ok(r) => r,
            Err(_) => return Ok(None), // Skip invalid chunks
        };

        let candidate = match stream_response.candidates.first() {
            Some(c) => c,
            None => return Ok(None),
        };

        // Determine chunk type based on content
        let chunk_type = if let Some(finish_reason) = &candidate.finish_reason {
            // Message delta with stop reason, then stop
            let usage = stream_response.usage_metadata.as_ref().map(|u| IRUsage {
                input_tokens: u.prompt_token_count,
                output_tokens: u.candidates_token_count.unwrap_or(0),
                cache_creation_input_tokens: None,
                cache_read_input_tokens: None,
                thinking_tokens: u.thoughts_token_count,
            }).unwrap_or(IRUsage {
                input_tokens: 0,
                output_tokens: 0,
                cache_creation_input_tokens: None,
                cache_read_input_tokens: None,
                thinking_tokens: None,
            });

            let stop_reason = convert_gemini_finish_reason_to_ir(finish_reason);
            IRChunkType::MessageDelta {
                delta: IRMessageDelta {
                    stop_reason,
                    stop_sequence: None,
                },
                usage,
            }
        } else if !candidate.content.parts.is_empty() {
            // Content delta
            let delta = convert_gemini_parts_to_delta(&candidate.content.parts)?;
            IRChunkType::ContentBlockDelta { index: 0, delta }
        } else {
            // Unknown chunk type
            return Ok(None);
        };

        Ok(Some(IRStreamChunk {
            message_id: format!("gemini-stream-{}", uuid::Uuid::new_v4()),
            model: "gemini".to_string(),
            chunk_type,
        }))
    }

    fn required_headers(&self, api_key: &str) -> Vec<(String, String)> {
        vec![
            ("Content-Type".to_string(), "application/json".to_string()),
        ]
    }

    fn endpoint_url(&self, base_url: &str, streaming: bool) -> String {
        if streaming {
            format!("{}:streamGenerateContent", base_url)
        } else {
            format!("{}:generateContent", base_url)
        }
    }
}

// Helper functions

fn convert_ir_message_to_gemini(ir_msg: &IRMessage) -> Result<Content, ProxyError> {
    let role = match ir_msg.role {
        IRRole::User => "user",
        IRRole::Assistant => "model",
        IRRole::System => "user", // Gemini doesn't have system role in contents
    }.to_string();

    let parts = ir_msg.content.iter()
        .map(convert_ir_content_to_gemini_part)
        .collect::<Result<Vec<_>, _>>()?;

    Ok(Content { role, parts })
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
                    Err(ProxyError::ConversionError("Gemini doesn't support image URLs, use base64".to_string()))
                }
            }
        },
        IRContent::ToolUse { id: _, name, input } => Ok(Part::FunctionCall {
            function_call: GeminiFunctionCall {
                name: name.clone(),
                args: input.clone(),
            },
        }),
        IRContent::ToolResult { tool_use_id: _, content, is_error: _ } => Ok(Part::FunctionResponse {
            function_response: GeminiFunctionResponse {
                id: None,
                name: "unknown".to_string(), // Gemini requires name but we don't have it in IR
                response: serde_json::json!({ "result": content }),
            },
        }),
        IRContent::Thinking { thinking: _ } => {
            // Gemini doesn't have explicit thinking content type
            Err(ProxyError::ConversionError("Gemini doesn't support thinking content".to_string()))
        }
        IRContent::Audio { source: _, .. } |
        IRContent::Video { source: _, .. } |
        IRContent::Document { source: _, .. } => {
            // Gemini supports these but needs more implementation
            Err(ProxyError::ConversionError("Audio/Video/Document not yet implemented for Gemini".to_string()))
        }
    }
}

fn convert_ir_tool_to_gemini(ir_tool: &IRTool) -> FunctionDeclaration {
    FunctionDeclaration {
        name: ir_tool.name.clone(),
        description: ir_tool.description.clone(),
        parameters: None,
        parameters_json_schema: Some(ir_tool.input_schema.clone()),
    }
}

fn convert_gemini_part_to_ir(part: &Part) -> Result<IRContent, ProxyError> {
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

fn convert_gemini_finish_reason_to_ir(reason: &str) -> Option<IRStopReason> {
    match reason {
        "STOP" => Some(IRStopReason::EndTurn),
        "MAX_TOKENS" => Some(IRStopReason::MaxTokens),
        "SAFETY" => Some(IRStopReason::StopSequence), // Map SAFETY to StopSequence as closest match
        _ => Some(IRStopReason::EndTurn), // Default to EndTurn for unknown reasons
    }
}

fn convert_gemini_parts_to_delta(parts: &[Part]) -> Result<IRDelta, ProxyError> {
    // For streaming, parts usually contain one text delta
    if let Some(Part::Text { text }) = parts.first() {
        Ok(IRDelta::TextDelta { text: text.clone() })
    } else if let Some(Part::FunctionCall { function_call }) = parts.first() {
        // Map function calls to input JSON delta (function call arguments as JSON)
        Ok(IRDelta::InputJsonDelta {
            partial_json: function_call.args.to_string(),
        })
    } else {
        Ok(IRDelta::TextDelta { text: String::new() })
    }
}
