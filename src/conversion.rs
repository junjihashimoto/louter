use crate::config::Config;
use crate::error::ProxyError;
use crate::models::gemini::{self as gemini, *};
use crate::models::openai::{self as openai, *};
use axum::response::sse::Event;
use futures_util::{Stream, StreamExt};
use std::pin::Pin;
use regex::Regex;

pub fn gemini_to_openai_request(
    gemini_request: GeminiRequest,
    model: &str,
    backend_name: &str,
    config: &Config,
) -> Result<OpenAIRequest, ProxyError> {
    let backend_config = config
        .get_backend(backend_name)
        .ok_or_else(|| ProxyError::ConfigError(format!("Backend '{}' not configured", backend_name)))?;

    let openai_model = config
        .map_model(backend_name, model)
        .unwrap_or_else(|| model.to_string());

    let mut messages = Vec::new();

    // Handle system instruction with priority: override > per-backend custom (with mode) > global custom
    let system_instruction = if let Some(override_instr) = &config.performance.override_system_instruction {
        // Override takes highest priority (replaces everything)
        Some(override_instr.clone())
    } else {
        // Get original system instruction from Gemini request
        let original = gemini_request.system_instruction.as_ref().map(|gemini_instr| {
            gemini_instr.parts.iter()
                .filter_map(|part| match part {
                    Part::Text { text } => Some(text.as_str()),
                    _ => None,
                })
                .collect::<Vec<_>>()
                .join(" ")
        });

        // Apply per-backend custom instruction with the configured mode (override/prepend/append)
        if let Some(result) = config.apply_custom_instruction(backend_name, original.as_deref()) {
            Some(result)
        } else {
            // Fall back to global custom instructions if no per-backend instruction
            config.custom_instructions.clone().or(original)
        }
    };

    if let Some(instr) = system_instruction {
        messages.push(Message::System {
            role: "system".to_string(),
            content: instr,
        });
    }

    // Convert contents to messages, merging consecutive messages with the same role
    let mut i = 0;
    while i < gemini_request.contents.len() {
        let content = &gemini_request.contents[i];
        let role = content.role.as_str();
        
        // Collect consecutive messages with the same role
        let mut consecutive_contents = vec![content];
        let mut j = i + 1;
        while j < gemini_request.contents.len() && gemini_request.contents[j].role == content.role {
            consecutive_contents.push(&gemini_request.contents[j]);
            j += 1;
        }
        
        let message = match role {
            "user" => {
                // Check if any content contains function responses - if so, treat as tool messages
                let has_function_response = consecutive_contents.iter().any(|content| {
                    content.parts.iter().any(|part| matches!(part, Part::FunctionResponse { .. }))
                });
                
                if has_function_response {
                    // Handle as tool messages even though role is "user" 
                    for content in consecutive_contents {
                        let mut tool_responses = String::new();
                        let mut tool_call_id = String::new();
                        
                        for part in &content.parts {
                            match part {
                                Part::FunctionResponse { function_response } => {
                                    tool_call_id = format!("call_{}", function_response.name);
                                    tool_responses = serde_json::to_string(&function_response.response)
                                        .unwrap_or_else(|_| "{}".to_string());
                                }
                                _ => {
                                    // Ignore non-function response parts in tool messages
                                }
                            }
                        }
                        
                        if !tool_call_id.is_empty() {
                            messages.push(Message::Tool {
                                role: "tool".to_string(),
                                content: tool_responses,
                                tool_call_id,
                            });
                        }
                    }
                    i = j;
                    continue; // Skip the regular push since we handled these as tool messages
                } else {
                    // Normal user message handling
                    let mut all_parts = Vec::new();
                    for content in consecutive_contents {
                        all_parts.extend(content.parts.clone());
                    }
                    let message_content = convert_gemini_parts_to_openai_content(all_parts)?;
                    Message::User {
                        role: "user".to_string(),
                        content: message_content,
                    }
                }
            }
            "model" => {
                // Merge all assistant message parts
                let mut text_content = String::new();
                let mut tool_calls = Vec::new();

                for content in consecutive_contents {
                    for part in &content.parts {
                        match part {
                            Part::Text { text } => {
                                if !text_content.is_empty() {
                                    text_content.push(' ');
                                }
                                text_content.push_str(&text);
                            }
                            Part::FunctionCall { function_call } => {
                                tool_calls.push(openai::ToolCall {
                                    id: format!("call_{}", tool_calls.len()),
                                    tool_type: "function".to_string(),
                                    function: openai::FunctionCall {
                                        name: function_call.name.clone(),
                                        arguments: serde_json::to_string(&function_call.args)
                                            .unwrap_or_else(|_| "{}".to_string()),
                                    },
                                });
                            }
                            _ => {
                                return Err(ProxyError::ConversionError(
                                    "Unsupported part type in model message".to_string(),
                                ));
                            }
                        }
                    }
                }

                Message::Assistant {
                    role: "assistant".to_string(),
                    content: if text_content.is_empty() { None } else { Some(text_content) },
                    tool_calls: if tool_calls.is_empty() { None } else { Some(tool_calls) },
                }
            }
            "tool" => {
                // For tool messages, we need to handle them individually as OpenAI expects separate tool messages
                for content in consecutive_contents {
                    let mut tool_responses = String::new();
                    let mut tool_call_id = String::new();
                    
                    for part in &content.parts {
                        match part {
                            Part::FunctionResponse { function_response } => {
                                tool_call_id = format!("call_{}", function_response.name);
                                tool_responses = serde_json::to_string(&function_response.response)
                                    .unwrap_or_else(|_| "{}".to_string());
                            }
                            _ => {
                                return Err(ProxyError::ConversionError(
                                    "Unsupported part type in tool message".to_string(),
                                ));
                            }
                        }
                    }
                    
                    messages.push(Message::Tool {
                        role: "tool".to_string(),
                        content: tool_responses,
                        tool_call_id,
                    });
                }
                i = j;
                continue; // Skip the regular push since we handled tool messages individually
            }
            _ => {
                return Err(ProxyError::ConversionError(format!(
                    "Unsupported role: {}",
                    role
                )));
            }
        };

        messages.push(message);
        i = j;
    }

    // Limit conversation history if configured (helps smaller models)
    if let Some(max_turns) = config.performance.max_conversation_turns {
        // Keep system message + last N messages
        let system_count = messages.iter().filter(|m| matches!(m, Message::System { .. })).count();
        let total_to_keep = system_count + max_turns;
        if messages.len() > total_to_keep {
            let to_skip = messages.len() - total_to_keep;
            eprintln!("Limiting conversation: {} messages -> {} messages (keeping last {} turns)",
                messages.len(), total_to_keep, max_turns);
            // Keep system messages + last N non-system messages
            let mut kept_messages = Vec::new();
            for msg in &messages {
                if matches!(msg, Message::System { .. }) {
                    kept_messages.push(msg.clone());
                }
            }
            kept_messages.extend(messages.iter().skip(system_count + to_skip).cloned());
            messages = kept_messages;
        }
    }

    let tools = gemini_request.tools.map(|tools| {
        let mut converted_tools: Vec<openai::Tool> = tools.into_iter()
            .flat_map(|tool_declaration| {
                tool_declaration.function_declarations.into_iter().map(|func| {
                    openai::Tool {
                        tool_type: "function".to_string(),
                        function: openai::Function {
                            name: func.name,
                            description: Some(func.description),
                            parameters: func.parameters
                                .or(func.parameters_json_schema)
                                .or_else(|| {
                                    // Provide empty object if no parameters specified
                                    Some(serde_json::json!({
                                        "type": "object",
                                        "properties": {},
                                        "required": []
                                    }))
                                }),
                        },
                    }
                })
            })
            .collect();

        // Debug: Print all tool names before filtering
        eprintln!("All tool names from client: {:?}",
            converted_tools.iter().map(|t| &t.function.name).collect::<Vec<_>>());

        // Filter tools if allowed_tools is configured
        if let Some(allowed_tools) = &config.performance.allowed_tools {
            eprintln!("Filtering tools. Before: {}, Allowed: {:?}", converted_tools.len(), allowed_tools);
            converted_tools.retain(|tool| allowed_tools.contains(&tool.function.name));
            eprintln!("After filtering: {} tools", converted_tools.len());
            eprintln!("Remaining tools: {:?}", converted_tools.iter().map(|t| &t.function.name).collect::<Vec<_>>());
        } else {
            eprintln!("No tool filtering configured, sending {} tools", converted_tools.len());
        }

        converted_tools
    });

    let tool_choice = if tools.is_some() {
        Some(ToolChoice::String("auto".to_string()))
    } else {
        None
    };

    // Extract max_output_tokens from Gemini request's generation_config
    let max_output_tokens = gemini_request.generation_config
        .as_ref()
        .and_then(|gc| gc.max_output_tokens)
        .or(backend_config.max_tokens);

    // Set max_tokens fields based on backend config
    let (max_tokens, max_completion_tokens) = match backend_config.max_tokens_field.as_str() {
        "max_tokens" => (max_output_tokens, None),
        "max_completion_tokens" => (None, max_output_tokens),
        "both" | _ => (None, max_output_tokens), // Default to max_completion_tokens (newer OpenAI standard)
    };

    // Handle temperature: use backend override if configured, otherwise use request temperature
    let temperature = if backend_config.temperature_override.unwrap_or(false) {
        // Override enabled: use backend's temperature (or None if not set)
        backend_config.temperature
    } else {
        // No override: prefer request temperature, fall back to backend temperature
        gemini_request.generation_config
            .as_ref()
            .and_then(|gc| gc.temperature)
            .or(backend_config.temperature)
    };

    Ok(OpenAIRequest {
        model: openai_model,
        messages,
        temperature,
        top_p: None,
        n: None,
        stream: None,
        stop: None,
        max_tokens,
        max_completion_tokens,
        presence_penalty: None,
        frequency_penalty: None,
        logit_bias: None,
        user: None,
        tools,
        tool_choice,
    })
}

fn convert_gemini_parts_to_openai_content(parts: Vec<Part>) -> Result<MessageContent, ProxyError> {
    let mut text_parts = Vec::new();
    let mut content_parts = Vec::new();

    for part in parts {
        match part {
            Part::Text { text } => {
                if content_parts.is_empty() {
                    text_parts.push(text);
                } else {
                    content_parts.push(ContentPart::Text { text });
                }
            }
            Part::InlineData { inline_data } => {
                if !text_parts.is_empty() {
                    content_parts.push(ContentPart::Text { 
                        text: text_parts.join(" ") 
                    });
                    text_parts.clear();
                }
                
                let image_url = ImageUrl {
                    url: format!("data:{};base64,{}", inline_data.mime_type, inline_data.data),
                    detail: None,
                };
                
                content_parts.push(ContentPart::ImageUrl { image_url });
            }
            _ => {
                return Err(ProxyError::ConversionError(
                    "Unsupported part type in user message".to_string()
                ));
            }
        }
    }

    if !text_parts.is_empty() {
        content_parts.push(ContentPart::Text { 
            text: text_parts.join(" ") 
        });
    }

    if content_parts.len() == 1 {
        if let ContentPart::Text { text } = &content_parts[0] {
            Ok(MessageContent::Text(text.clone()))
        } else {
            Ok(MessageContent::Array(content_parts))
        }
    } else if content_parts.is_empty() {
        Ok(MessageContent::Text(String::new()))
    } else {
        Ok(MessageContent::Array(content_parts))
    }
}

pub fn openai_to_gemini_response(
    openai_response: OpenAIResponse,
) -> Result<GeminiResponse, ProxyError> {
    let mut candidates = Vec::new();

    for choice in openai_response.choices {
        let content = Content {
            parts: convert_openai_choice_to_gemini_parts(choice.message)?,
            role: "model".to_string(),
        };

        let finish_reason = choice.finish_reason.map(|reason| match reason.as_str() {
            "stop" => "STOP".to_string(),
            "length" => "MAX_TOKENS".to_string(),
            "content_filter" => "SAFETY".to_string(),
            "tool_calls" => "STOP".to_string(),
            "terminated" => "STOP".to_string(),  // Map terminated to STOP
            _ => "STOP".to_string(),  // Default to STOP for unknown reasons
        });

        candidates.push(Candidate {
            content,
            finish_reason,
            safety_ratings: None,
            citation_metadata: None,
        });
    }

    // Convert OpenAI token counts to Gemini format
    // OpenAI's completion_tokens includes all generated tokens (visible + thinking)
    // We map it to candidatesTokenCount since we can't separate visible from thinking tokens
    // thoughts_token_count is left as None (OpenAI doesn't provide this breakdown)
    let usage_metadata = Some(UsageMetadata {
        prompt_token_count: openai_response.usage.prompt_tokens,
        candidates_token_count: Some(openai_response.usage.completion_tokens),
        total_token_count: openai_response.usage.total_tokens,
        thoughts_token_count: None,  // OpenAI doesn't separate thinking tokens
    });

    Ok(GeminiResponse {
        candidates,
        usage_metadata,
    })
}

/// Parse alternative function call formats from text content
fn parse_function_call_from_text(text: &str) -> Option<(String, serde_json::Value)> {
    // Try XML-style format: <tool_call><function=name<parameter=key>value</parameter>...
    if text.contains("<tool_call>") && text.contains("<function=") {
        eprintln!("⚠️  WARNING: Detected non-standard XML-style function call format");
        eprintln!("   This indicates the model is not following OpenAI function calling format.");
        eprintln!("   Attempting to parse and convert to standard format...");

        // Extract function name
        if let Some(func_start) = text.find("<function=") {
            let after_func = &text[func_start + 10..];
            if let Some(func_end) = after_func.find(|c: char| c == '<' || c == '\n') {
                let func_name = after_func[..func_end].trim().to_string();

                // Extract parameters
                let mut args = serde_json::Map::new();
                let param_pattern = "<parameter=";
                let mut search_pos = 0;

                while let Some(param_start) = text[search_pos..].find(param_pattern) {
                    let abs_param_start = search_pos + param_start + param_pattern.len();
                    if let Some(param_name_end) = text[abs_param_start..].find('>') {
                        let param_name = text[abs_param_start..abs_param_start + param_name_end].trim().to_string();
                        let value_start = abs_param_start + param_name_end + 1;

                        if let Some(param_end) = text[value_start..].find("</parameter>") {
                            let param_value = text[value_start..value_start + param_end].trim().to_string();
                            args.insert(param_name, serde_json::Value::String(param_value));
                            search_pos = value_start + param_end;
                        } else {
                            break;
                        }
                    } else {
                        break;
                    }
                }

                eprintln!("✓ Successfully parsed XML function call: {} with {} args", func_name, args.len());
                return Some((func_name, serde_json::Value::Object(args)));
            }
        }
    }

    // Try JSON format in text: look for {"name": "...", "arguments": {...}}
    if let Ok(parsed) = serde_json::from_str::<serde_json::Value>(text) {
        if let Some(obj) = parsed.as_object() {
            if let (Some(name), Some(arguments)) = (obj.get("name"), obj.get("arguments")) {
                if let Some(name_str) = name.as_str() {
                    eprintln!("⚠️  WARNING: Detected non-standard JSON function call format in text content");
                    eprintln!("   Expected tool_calls field, but found JSON in content field.");
                    eprintln!("✓ Successfully parsed JSON function call: {}", name_str);
                    return Some((name_str.to_string(), arguments.clone()));
                }
            }
        }
    }

    None
}

fn convert_openai_choice_to_gemini_parts(mut choice_message: ResponseMessage) -> Result<Vec<Part>, ProxyError> {
    let mut parts = Vec::new();

    // Try to parse function calls from content if tool_calls is empty
    if choice_message.tool_calls.is_none() {
        if let Some(ref content) = choice_message.content {
            if let Some((func_name, args)) = parse_function_call_from_text(content) {
                // Create synthetic tool_call
                choice_message.tool_calls = Some(vec![openai::ToolCall {
                    id: "parsed_call_0".to_string(),
                    tool_type: "function".to_string(),
                    function: openai::FunctionCall {
                        name: func_name,
                        arguments: serde_json::to_string(&args).unwrap_or_else(|_| "{}".to_string()),
                    },
                }]);
                eprintln!("✓ Converted alternative format to standard tool_calls");
            }
        }
    }

    if let Some(content) = choice_message.content {
        if !content.is_empty() {
            // Only add as text if we didn't parse it as a function call
            if choice_message.tool_calls.is_none() {
                parts.push(Part::Text { text: content });
            }
        }
    }

    if let Some(tool_calls) = choice_message.tool_calls {
        for tool_call in tool_calls {
            eprintln!("📞 Received tool call: {} with arguments: {}",
                tool_call.function.name, tool_call.function.arguments);

            let args = serde_json::from_str(&tool_call.function.arguments)
                .unwrap_or_else(|e| {
                    eprintln!("⚠️  Failed to parse arguments as JSON: {}", e);
                    serde_json::Value::Object(serde_json::Map::new())
                });

            eprintln!("📦 Parsed args: {}", serde_json::to_string(&args).unwrap_or_default());

            parts.push(Part::FunctionCall {
                function_call: gemini::FunctionCall {
                    name: tool_call.function.name,
                    args,
                },
            });
        }
    }

    if parts.is_empty() {
        parts.push(Part::Text { text: String::new() });
    }

    Ok(parts)
}

// State for accumulating tool calls across stream chunks
#[derive(Debug, Clone, Default)]
struct ToolCallAccumulator {
    // Map of tool call index to (id, name, accumulated_arguments)
    calls: std::collections::HashMap<usize, (String, String, String)>,
}

pub fn openai_stream_to_gemini_sse(
    stream: Pin<Box<dyn Stream<Item = Result<OpenAIStreamResponse, ProxyError>> + Send>>,
) -> impl Stream<Item = Result<Event, axum::Error>> {
    use futures_util::stream::StreamExt;

    // Use scan to maintain state across chunks
    stream.scan(ToolCallAccumulator::default(), |accumulator, result| {
        use futures_util::future::ready;

        match result {
                Ok(mut openai_chunk) => {
                    // Accumulate tool call arguments
                    for choice in &mut openai_chunk.choices {
                        if let Some(tool_calls) = &mut choice.delta.tool_calls {
                            for tool_call in tool_calls {
                                // Use index to track which tool call this chunk belongs to
                                let index = tool_call.index.unwrap_or(0);

                                // Get or create entry for this tool call (id, name, accumulated_args)
                                let entry = accumulator.calls.entry(index).or_insert((String::new(), String::new(), String::new()));

                                // Accumulate ID (first non-empty id wins)
                                if let Some(id) = &tool_call.id {
                                    if !id.is_empty() && entry.0.is_empty() {
                                        entry.0 = id.clone();
                                        eprintln!("📌 STREAM: Tool call [{}] - id: {}", index, id);
                                    }
                                }

                                // Accumulate function name
                                if let Some(name) = &tool_call.function.name {
                                    if !name.is_empty() {
                                        entry.1 = name.clone();
                                        eprintln!("📞 STREAM: Tool call [{}] - name: {}", index, name);
                                    }
                                }

                                // Accumulate arguments
                                if !tool_call.function.arguments.is_empty() {
                                    entry.2.push_str(&tool_call.function.arguments);
                                    eprintln!("📝 STREAM: Tool call [{}] - accumulated args so far: {:?}", index, &entry.2);
                                }
                            }
                        }

                        // If this is the final chunk (has finish_reason), emit complete tool calls
                        if choice.finish_reason.is_some() {
                            eprintln!("✅ STREAM: Final chunk detected with finish_reason: {:?}", choice.finish_reason);

                            // Replace partial tool_calls with complete ones
                            if !accumulator.calls.is_empty() {
                                let mut complete_calls = Vec::new();
                                for (index, (id, name, args)) in &accumulator.calls {
                                    eprintln!("🔧 STREAM: Building complete tool call [{}] - id: {}, name: {}, args: {:?}", index, id, name, args);
                                    complete_calls.push(openai::StreamingToolCall {
                                        index: Some(*index),
                                        id: Some(id.clone()),
                                        tool_type: Some("function".to_string()),
                                        function: openai::StreamingFunctionCall {
                                            name: Some(name.clone()),
                                            arguments: args.clone(),
                                        },
                                    });
                                }
                                choice.delta.tool_calls = Some(complete_calls);
                            }
                        }
                    }

                ready(Some(Ok(openai_chunk)))
            }
            Err(e) => ready(Some(Err(e))),
        }
    }).map(|result| {
        match result {
            Ok(openai_chunk) => {
                match convert_openai_stream_to_gemini(openai_chunk) {
                    Ok(gemini_chunk) => {
                        match serde_json::to_string(&gemini_chunk) {
                            Ok(json) => Ok(Event::default().data(json)),
                            Err(e) => {
                                let error_chunk = GeminiStreamResponse {
                                    candidates: vec![Candidate {
                                        content: Content {
                                            parts: vec![Part::Text { 
                                                text: format!("Serialization error: {}", e) 
                                            }],
                                            role: "model".to_string(),
                                        },
                                        finish_reason: Some("ERROR".to_string()),
                                        safety_ratings: None,
                                        citation_metadata: None,
                                    }],
                                    usage_metadata: None,
                                };
                                match serde_json::to_string(&error_chunk) {
                                    Ok(error_json) => Ok(Event::default().data(error_json)),
                                    Err(_) => Ok(Event::default().data(r#"{"error":"Failed to serialize error"}"#)),
                                }
                            }
                        }
                    }
                    Err(e) => {
                        let error_chunk = GeminiStreamResponse {
                            candidates: vec![Candidate {
                                content: Content {
                                    parts: vec![Part::Text { 
                                        text: format!("Conversion error: {}", e) 
                                    }],
                                    role: "model".to_string(),
                                },
                                finish_reason: Some("ERROR".to_string()),
                                safety_ratings: None,
                                citation_metadata: None,
                            }],
                            usage_metadata: None,
                        };
                        match serde_json::to_string(&error_chunk) {
                            Ok(error_json) => Ok(Event::default().data(error_json)),
                            Err(_) => Ok(Event::default().data(r#"{"error":"Failed to serialize error"}"#)),
                        }
                    }
                }
            }
            Err(e) => {
                // Check if this is a skippable error that we should filter out
                let error_msg = e.to_string();
                if error_msg.contains("skipping") || error_msg.contains("Empty line") || error_msg.contains("Stream ended") {
                    // Don't send error chunks for skippable errors - just log and skip
                    eprintln!("Debug: Skipping error in stream conversion: {}", e);
                    // Return an error that will be filtered out
                    return Err(axum::Error::new(std::io::Error::new(
                        std::io::ErrorKind::Other,
                        "Skippable error"
                    )));
                }

                // For non-skippable errors, send an error chunk
                let error_chunk = GeminiStreamResponse {
                    candidates: vec![Candidate {
                        content: Content {
                            parts: vec![Part::Text {
                                text: format!("Stream error: {}", e)
                            }],
                            role: "model".to_string(),
                        },
                        finish_reason: Some("ERROR".to_string()),
                        safety_ratings: None,
                        citation_metadata: None,
                    }],
                    usage_metadata: None,
                };
                match serde_json::to_string(&error_chunk) {
                    Ok(error_json) => Ok(Event::default().data(error_json)),
                    Err(_) => Ok(Event::default().data(r#"{"error":"Failed to serialize error"}"#)),
                }
            }
        }
    })
}

fn convert_openai_stream_to_gemini(
    openai_chunk: OpenAIStreamResponse,
) -> Result<GeminiStreamResponse, ProxyError> {
    let mut candidates = Vec::new();

    for choice in openai_chunk.choices {
        let mut parts = Vec::new();

        let delta = &choice.delta;
        {
            // Process delta content
            if let Some(content) = &delta.content {
                if !content.is_empty() {
                    parts.push(Part::Text { text: content.clone() });
                }
            }

            if let Some(tool_calls) = &delta.tool_calls {
                for tool_call in tool_calls {
                    if let Some(name) = &tool_call.function.name {
                        eprintln!("📞 STREAM: Received tool call: {} with arguments: {:?}",
                            name, tool_call.function.arguments);

                        // Try to parse arguments - only emit function call if we have valid, complete JSON
                        match serde_json::from_str::<serde_json::Value>(&tool_call.function.arguments) {
                            Ok(args) if args.is_object() && !args.as_object().unwrap().is_empty() => {
                                // We have complete, valid, non-empty arguments
                                eprintln!("✅ STREAM: Successfully parsed complete args: {}", serde_json::to_string(&args).unwrap_or_default());

                                parts.push(Part::FunctionCall {
                                    function_call: gemini::FunctionCall {
                                        name: name.clone(),
                                        args,
                                    },
                                });
                            }
                            Ok(args) => {
                                eprintln!("⚠️  STREAM: Skipping function call - empty or invalid args: {:?}", args);
                                // Skip empty args - these are incomplete chunks
                            }
                            Err(e) => {
                                eprintln!("⚠️  STREAM: Skipping function call - failed to parse (input: {:?}): {}", tool_call.function.arguments, e);
                                // Skip unparseable args - these are incomplete chunks
                            }
                        }
                    }
                }
            }
        }

        if parts.is_empty() {
            parts.push(Part::Text { text: String::new() });
        }

        let finish_reason = choice.finish_reason.map(|reason| match reason.as_str() {
            "stop" => "STOP".to_string(),
            "length" => "MAX_TOKENS".to_string(),
            "content_filter" => "SAFETY".to_string(),
            "tool_calls" => "STOP".to_string(),
            "terminated" => "STOP".to_string(),  // Map terminated to STOP
            _ => "STOP".to_string(),  // Default to STOP for unknown reasons
        });

        candidates.push(Candidate {
            content: Content {
                parts,
                role: "model".to_string(),
            },
            finish_reason,
            safety_ratings: None,
            citation_metadata: None,
        });
    }

    let usage_metadata = None; // Stream responses typically don't have usage metadata per chunk

    Ok(GeminiStreamResponse {
        candidates,
        usage_metadata,
    })
}

// ========== OpenAI Stream to SSE Helper ==========

pub fn openai_stream_to_sse(
    stream: Pin<Box<dyn Stream<Item = Result<OpenAIStreamResponse, ProxyError>> + Send>>,
) -> impl Stream<Item = Result<Event, axum::Error>> {
    stream.map(|result| {
        match result {
            Ok(chunk) => {
                match serde_json::to_string(&chunk) {
                    Ok(json) => Ok(Event::default().data(json)),
                    Err(e) => Ok(Event::default().data(format!(r#"{{"error":"Serialization error: {}"}}"#, e))),
                }
            }
            Err(e) => Ok(Event::default().data(format!(r#"{{"error":"Stream error: {}"}}"#, e))),
        }
    })
}

// ========== OpenAI Frontend to Gemini Backend Conversions ==========

pub fn openai_to_gemini_request(
    openai_request: OpenAIRequest,
    _model: &str,
    _config: &Config,
) -> Result<GeminiRequest, ProxyError> {
    let mut contents = Vec::new();

    for message in openai_request.messages {
        match message {
            Message::System { content, .. } => {
                // Convert system message to user message as Gemini doesn't have a system role
                // We'll prepend this as the first user message
                contents.push(Content {
                    parts: vec![Part::Text { text: content }],
                    role: "user".to_string(),
                });
            }
            Message::User { content, .. } => {
                let parts = convert_openai_content_to_gemini_parts(content)?;
                contents.push(Content {
                    parts,
                    role: "user".to_string(),
                });
            }
            Message::Assistant { content, tool_calls, .. } => {
                let mut parts = Vec::new();

                if let Some(text) = content {
                    if !text.is_empty() {
                        parts.push(Part::Text { text });
                    }
                }

                if let Some(calls) = tool_calls {
                    for call in calls {
                        parts.push(Part::FunctionCall {
                            function_call: gemini::FunctionCall {
                                name: call.function.name,
                                args: serde_json::from_str(&call.function.arguments)
                                    .unwrap_or_else(|_| serde_json::Value::Object(serde_json::Map::new())),
                            },
                        });
                    }
                }

                if parts.is_empty() {
                    parts.push(Part::Text { text: String::new() });
                }

                contents.push(Content {
                    parts,
                    role: "model".to_string(),
                });
            }
            Message::Tool { content, tool_call_id, .. } => {
                // Extract function name from tool_call_id (format: "call_<name>" or just use generic name)
                let function_name = tool_call_id.strip_prefix("call_")
                    .unwrap_or(&tool_call_id)
                    .to_string();

                let response: serde_json::Value = serde_json::from_str(&content)
                    .unwrap_or_else(|_| serde_json::json!({"result": content}));

                contents.push(Content {
                    parts: vec![Part::FunctionResponse {
                        function_response: gemini::FunctionResponse {
                            id: None,
                            name: function_name,
                            response,
                        },
                    }],
                    role: "user".to_string(), // Gemini uses "user" role for function responses
                });
            }
        }
    }

    let tools = openai_request.tools.map(|tools| {
        tools.into_iter().map(|tool| {
            gemini::Tool {
                function_declarations: vec![gemini::FunctionDeclaration {
                    name: tool.function.name,
                    description: tool.function.description.unwrap_or_default(),
                    parameters: tool.function.parameters,
                    parameters_json_schema: None,
                }],
            }
        }).collect()
    });

    let generation_config = Some(gemini::GenerationConfig {
        temperature: openai_request.temperature,
        top_p: openai_request.top_p,
        top_k: None,
        max_output_tokens: openai_request.max_completion_tokens.or(openai_request.max_tokens),
        stop_sequences: openai_request.stop,
    });

    Ok(GeminiRequest {
        contents,
        system_instruction: None,
        safety_settings: None,
        generation_config,
        tools,
        tool_config: None,
    })
}

fn convert_openai_content_to_gemini_parts(content: MessageContent) -> Result<Vec<Part>, ProxyError> {
    match content {
        MessageContent::Text(text) => Ok(vec![Part::Text { text }]),
        MessageContent::Array(parts) => {
            let mut gemini_parts = Vec::new();
            for part in parts {
                match part {
                    ContentPart::Text { text } => {
                        gemini_parts.push(Part::Text { text });
                    }
                    ContentPart::ImageUrl { image_url } => {
                        // Parse data URL: "data:<mime_type>;base64,<data>"
                        if let Some(data_url) = image_url.url.strip_prefix("data:") {
                            if let Some((mime_type, base64_data)) = data_url.split_once(";base64,") {
                                gemini_parts.push(Part::InlineData {
                                    inline_data: gemini::InlineData {
                                        mime_type: mime_type.to_string(),
                                        data: base64_data.to_string(),
                                    },
                                });
                            } else {
                                return Err(ProxyError::ConversionError(
                                    "Invalid data URL format".to_string()
                                ));
                            }
                        } else {
                            return Err(ProxyError::ConversionError(
                                "Only data URLs are supported for images".to_string()
                            ));
                        }
                    }
                }
            }
            Ok(gemini_parts)
        }
    }
}

pub fn gemini_to_openai_response(
    gemini_response: GeminiResponse,
    model: &str,
) -> Result<OpenAIResponse, ProxyError> {
    let mut choices = Vec::new();

    for (index, candidate) in gemini_response.candidates.into_iter().enumerate() {
        let message = convert_gemini_candidate_to_openai_message(candidate.content)?;

        let finish_reason = candidate.finish_reason.map(|reason| match reason.as_str() {
            "STOP" => "stop".to_string(),
            "MAX_TOKENS" => "length".to_string(),
            "SAFETY" => "content_filter".to_string(),
            _ => reason.to_lowercase(),
        });

        choices.push(openai::Choice {
            index: index as i32,
            message,
            finish_reason,
        });
    }

    let usage = gemini_response.usage_metadata.map(|metadata| {
        // Convert Gemini token counts to OpenAI format
        // completion_tokens = candidatesTokenCount (visible) + thoughtsTokenCount (internal reasoning)
        let visible_tokens = metadata.candidates_token_count.unwrap_or(0);
        let thinking_tokens = metadata.thoughts_token_count.unwrap_or(0);

        openai::Usage {
            prompt_tokens: metadata.prompt_token_count,
            completion_tokens: visible_tokens + thinking_tokens,
            total_tokens: metadata.total_token_count,
        }
    }).unwrap_or_else(|| openai::Usage {
        prompt_tokens: 0,
        completion_tokens: 0,
        total_tokens: 0,
    });

    Ok(OpenAIResponse {
        id: format!("chatcmpl-{}", uuid::Uuid::new_v4().to_string()),
        object: "chat.completion".to_string(),
        created: std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_secs() as i64,
        model: model.to_string(),
        choices,
        usage,
    })
}

fn convert_gemini_candidate_to_openai_message(content: Content) -> Result<ResponseMessage, ProxyError> {
    let mut text_content = String::new();
    let mut tool_calls = Vec::new();

    for part in content.parts {
        match part {
            Part::Text { text } => {
                if !text_content.is_empty() {
                    text_content.push(' ');
                }
                text_content.push_str(&text);
            }
            Part::FunctionCall { function_call } => {
                tool_calls.push(openai::ToolCall {
                    id: format!("call_{}", function_call.name),
                    tool_type: "function".to_string(),
                    function: openai::FunctionCall {
                        name: function_call.name,
                        arguments: serde_json::to_string(&function_call.args)
                            .unwrap_or_else(|_| "{}".to_string()),
                    },
                });
            }
            _ => {
                return Err(ProxyError::ConversionError(
                    "Unsupported part type in Gemini response".to_string(),
                ));
            }
        }
    }

    Ok(ResponseMessage {
        role: "assistant".to_string(),
        content: if text_content.is_empty() { None } else { Some(text_content) },
        tool_calls: if tool_calls.is_empty() { None } else { Some(tool_calls) },
        reasoning_content: None,
    })
}

pub fn gemini_raw_stream_to_openai_sse(
    stream: Pin<Box<dyn Stream<Item = Result<String, ProxyError>> + Send>>,
    model: &str,
) -> impl Stream<Item = Result<Event, axum::Error>> {
    let model = model.to_string();
    stream.map(move |result| {
        match result {
            Ok(raw_data) => {
                // Parse the raw SSE data into GeminiStreamResponse
                match serde_json::from_str::<GeminiStreamResponse>(&raw_data) {
                    Ok(gemini_chunk) => {
                        match convert_gemini_stream_to_openai(gemini_chunk, &model) {
                            Ok(openai_chunk) => {
                                match serde_json::to_string(&openai_chunk) {
                                    Ok(json) => Ok(Event::default().data(json)),
                                    Err(e) => Ok(Event::default().data(format!(r#"{{"error":"Serialization error: {}"}}"#, e))),
                                }
                            }
                            Err(e) => Ok(Event::default().data(format!(r#"{{"error":"Conversion error: {}"}}"#, e))),
                        }
                    }
                    Err(e) => Ok(Event::default().data(format!(r#"{{"error":"Parse error: {}"}}"#, e))),
                }
            }
            Err(e) => Ok(Event::default().data(format!(r#"{{"error":"Stream error: {}"}}"#, e))),
        }
    })
}

pub fn gemini_stream_to_openai_sse(
    stream: Pin<Box<dyn Stream<Item = Result<GeminiStreamResponse, ProxyError>> + Send>>,
    model: &str,
) -> impl Stream<Item = Result<Event, axum::Error>> {
    let model = model.to_string();
    stream.map(move |result| {
        match result {
            Ok(gemini_chunk) => {
                match convert_gemini_stream_to_openai(gemini_chunk, &model) {
                    Ok(openai_chunk) => {
                        match serde_json::to_string(&openai_chunk) {
                            Ok(json) => Ok(Event::default().data(json)),
                            Err(e) => {
                                let error_response = openai::OpenAIStreamResponse {
                                    id: format!("chatcmpl-error-{}", uuid::Uuid::new_v4().to_string()),
                                    object: "chat.completion.chunk".to_string(),
                                    created: std::time::SystemTime::now()
                                        .duration_since(std::time::UNIX_EPOCH)
                                        .unwrap()
                                        .as_secs() as i64,
                                    model: model.clone(),
                                    choices: vec![openai::StreamChoice {
                                        index: 0,
                                        delta: openai::Delta {
                                            role: Some("assistant".to_string()),
                                            content: Some(format!("Serialization error: {}", e)),
                                            tool_calls: None,
                                            reasoning_content: None,
                                        },
                                        finish_reason: Some("error".to_string()),
                                    }],
                                };
                                match serde_json::to_string(&error_response) {
                                    Ok(error_json) => Ok(Event::default().data(error_json)),
                                    Err(_) => Ok(Event::default().data(r#"{"error":"Failed to serialize error"}"#)),
                                }
                            }
                        }
                    }
                    Err(e) => {
                        let error_response = openai::OpenAIStreamResponse {
                            id: format!("chatcmpl-error-{}", uuid::Uuid::new_v4().to_string()),
                            object: "chat.completion.chunk".to_string(),
                            created: std::time::SystemTime::now()
                                .duration_since(std::time::UNIX_EPOCH)
                                .unwrap()
                                .as_secs() as i64,
                            model: model.clone(),
                            choices: vec![openai::StreamChoice {
                                index: 0,
                                delta: openai::Delta {
                                    role: Some("assistant".to_string()),
                                    content: Some(format!("Conversion error: {}", e)),
                                    tool_calls: None,
                                    reasoning_content: None,
                                },
                                finish_reason: Some("error".to_string()),
                            }],
                        };
                        match serde_json::to_string(&error_response) {
                            Ok(error_json) => Ok(Event::default().data(error_json)),
                            Err(_) => Ok(Event::default().data(r#"{"error":"Failed to serialize error"}"#)),
                        }
                    }
                }
            }
            Err(e) => {
                let error_response = openai::OpenAIStreamResponse {
                    id: format!("chatcmpl-error-{}", uuid::Uuid::new_v4().to_string()),
                    object: "chat.completion.chunk".to_string(),
                    created: std::time::SystemTime::now()
                        .duration_since(std::time::UNIX_EPOCH)
                        .unwrap()
                        .as_secs() as i64,
                    model: model.clone(),
                    choices: vec![openai::StreamChoice {
                        index: 0,
                        delta: openai::Delta {
                            role: Some("assistant".to_string()),
                            content: Some(format!("Stream error: {}", e)),
                            tool_calls: None,
                            reasoning_content: None,
                        },
                        finish_reason: Some("error".to_string()),
                    }],
                };
                match serde_json::to_string(&error_response) {
                    Ok(error_json) => Ok(Event::default().data(error_json)),
                    Err(_) => Ok(Event::default().data(r#"{"error":"Failed to serialize error"}"#)),
                }
            }
        }
    })
}

fn convert_gemini_stream_to_openai(
    gemini_chunk: GeminiStreamResponse,
    model: &str,
) -> Result<OpenAIStreamResponse, ProxyError> {
    let mut choices = Vec::new();

    for candidate in gemini_chunk.candidates {
        let mut delta_content = String::new();
        let mut delta_tool_calls = Vec::new();

        for part in candidate.content.parts {
            match part {
                Part::Text { text } => {
                    if !delta_content.is_empty() {
                        delta_content.push(' ');
                    }
                    delta_content.push_str(&text);
                }
                Part::FunctionCall { function_call } => {
                    delta_tool_calls.push(openai::StreamingToolCall {
                        index: Some(delta_tool_calls.len()),
                        id: Some(format!("call_{}", function_call.name)),
                        tool_type: Some("function".to_string()),
                        function: openai::StreamingFunctionCall {
                            name: Some(function_call.name),
                            arguments: serde_json::to_string(&function_call.args)
                                .unwrap_or_else(|_| "{}".to_string()),
                        },
                    });
                }
                _ => {} // Ignore other part types in streaming
            }
        }

        let finish_reason = candidate.finish_reason.map(|reason| match reason.as_str() {
            "STOP" => "stop".to_string(),
            "MAX_TOKENS" => "length".to_string(),
            "SAFETY" => "content_filter".to_string(),
            _ => reason.to_lowercase(),
        });

        choices.push(openai::StreamChoice {
            index: 0,
            delta: openai::Delta {
                role: Some("assistant".to_string()),
                content: if delta_content.is_empty() { None } else { Some(delta_content) },
                tool_calls: if delta_tool_calls.is_empty() { None } else { Some(delta_tool_calls) },
                reasoning_content: None,
            },
            finish_reason,
        });
    }

    Ok(OpenAIStreamResponse {
        id: format!("chatcmpl-{}", uuid::Uuid::new_v4().to_string()),
        object: "chat.completion.chunk".to_string(),
        created: std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_secs() as i64,
        model: model.to_string(),
        choices,
    })
}

/// Parse XML tool calls from text content (Qwen3-Coder format)
///
/// Format: <tool_call><function=NAME><parameter=PARAM>VALUE</parameter></function></tool_call>
///
/// Returns: Vec<(function_name, args_json)>
fn parse_xml_tool_calls(text: &str) -> Vec<(String, serde_json::Value)> {
    let mut tool_calls = Vec::new();

    // Pattern: <tool_call>...</tool_call>
    let tool_call_re = Regex::new(r"<tool_call>(.*?)</tool_call>").unwrap();

    for tool_cap in tool_call_re.captures_iter(text) {
        let tool_content = &tool_cap[1];

        // Extract function name: <function=NAME>
        let function_re = Regex::new(r"<function=([^>]+)>").unwrap();
        if let Some(func_cap) = function_re.captures(tool_content) {
            let function_name = func_cap[1].trim().to_string();

            // Extract parameters: <parameter=NAME>VALUE</parameter>
            let param_re = Regex::new(r"<parameter=([^>]+)>(.*?)</parameter>").unwrap();
            let mut args = serde_json::Map::new();

            for param_cap in param_re.captures_iter(tool_content) {
                let param_name = param_cap[1].trim().to_string();
                let param_value = param_cap[2].trim();

                // Try to parse as JSON, otherwise use as string
                let value = if let Ok(json_val) = serde_json::from_str::<serde_json::Value>(param_value) {
                    json_val
                } else {
                    serde_json::Value::String(param_value.to_string())
                };

                args.insert(param_name, value);
            }

            tool_calls.push((function_name, serde_json::Value::Object(args)));

            eprintln!("🔧 XML Parser: Found tool call - function: {}, args: {:?}",
                tool_calls.last().unwrap().0, tool_calls.last().unwrap().1);
        }
    }

    tool_calls
}

/// Extract text content and replace XML tool calls with JSON format
/// Used when tool_format = "xml" to convert model output to standard format
pub fn extract_and_convert_xml_tool_calls(content: &str) -> (String, Vec<openai::ToolCall>) {
    let xml_tools = parse_xml_tool_calls(content);

    // Remove XML tool calls from text
    let tool_call_re = Regex::new(r"<tool_call>.*?</tool_call>").unwrap();
    let cleaned_text = tool_call_re.replace_all(content, "").trim().to_string();

    // Convert to OpenAI tool call format
    let tool_calls: Vec<openai::ToolCall> = xml_tools.into_iter().enumerate().map(|(idx, (name, args))| {
        openai::ToolCall {
            id: format!("call_{}", idx),
            tool_type: "function".to_string(),
            function: openai::FunctionCall {
                name,
                arguments: serde_json::to_string(&args).unwrap_or_else(|_| "{}".to_string()),
            },
        }
    }).collect();

    eprintln!("🔄 XML Conversion: Extracted {} tool calls, cleaned text: {:?}",
        tool_calls.len(), &cleaned_text[..cleaned_text.len().min(100)]);

    (cleaned_text, tool_calls)
}
