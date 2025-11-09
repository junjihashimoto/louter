use crate::config::{Config, BackendConfig};
use crate::backends::BackendClient;
use crate::models::openai::{OpenAIRequest, Message, MessageContent, Tool, Function, ToolChoice, ContentPart, ImageUrl};
use crate::models::gemini::{GeminiRequest, Content, Part, FunctionDeclaration, Tool as GeminiTool, InlineData};
use crate::routing::Capability;
use std::collections::{HashMap, HashSet};
use tracing::{info, warn, debug};

/// Result of a capability test
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct CapabilityTestResult {
    pub capability: Capability,
    pub supported: bool,
    pub error: Option<String>,
    pub streaming: bool,
}

/// Diagnostic results for a backend
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct BackendDiagnostics {
    pub backend_name: String,
    pub url: String,
    pub reachable: bool,
    pub detected_capabilities: HashSet<Capability>,
    pub test_results: Vec<CapabilityTestResult>,
    pub error: Option<String>,
}

/// Test if a backend supports basic text capability
async fn test_text_capability(
    client: &BackendClient,
    backend_name: &str,
    backend_config: &BackendConfig,
    api_key: &str,
) -> CapabilityTestResult {
    debug!("Testing text capability for backend: {}", backend_name);

    // Get a model name from the backend's model mapping
    let test_model = match backend_config.model_mapping.values().next() {
        Some(model) => model.clone(),
        None => {
            return CapabilityTestResult {
                capability: Capability::Text,
                supported: false,
                error: Some(format!(
                    "Backend '{}' has no model_mapping configured. Please add at least one model mapping in the config file.",
                    backend_name
                )),
                streaming: false,
            };
        }
    };

    let test_request = OpenAIRequest {
        model: test_model,
        messages: vec![
            Message::User {
                role: "user".to_string(),
                content: MessageContent::Text("Hi".to_string()),
            }
        ],
        max_tokens: None,  // Older models use this
        max_completion_tokens: Some(10),  // Newer models (gpt-5-nano, gpt-4o, etc) require this
        temperature: None,  // Don't set - some newer models only support default
        top_p: None,
        n: None,
        stream: Some(false),
        stop: None,
        presence_penalty: None,
        frequency_penalty: None,
        logit_bias: None,
        user: None,
        tools: None,
        tool_choice: None,
    };

    match client.chat_completion(backend_name, test_request, api_key).await {
        Ok(_) => {
            info!("✓ Backend {} supports TEXT capability", backend_name);
            CapabilityTestResult {
                capability: Capability::Text,
                supported: true,
                error: None,
                streaming: false,
            }
        },
        Err(e) => {
            warn!("✗ Backend {} TEXT test failed: {}", backend_name, e);
            CapabilityTestResult {
                capability: Capability::Text,
                supported: false,
                error: Some(e.to_string()),
                streaming: false,
            }
        }
    }
}

/// Test if a backend supports vision capability (images)
async fn test_vision_capability(
    client: &BackendClient,
    backend_name: &str,
    backend_config: &BackendConfig,
    api_key: &str,
) -> CapabilityTestResult {
    debug!("Testing vision capability for backend: {}", backend_name);

    // Get a model name from the backend's model mapping
    let test_model = match backend_config.model_mapping.values().next() {
        Some(model) => model.clone(),
        None => {
            return CapabilityTestResult {
                capability: Capability::Vision,
                supported: false,
                error: Some(format!(
                    "Backend '{}' has no model_mapping configured. Please add at least one model mapping in the config file.",
                    backend_name
                )),
                streaming: false,
            };
        }
    };

    // Blue square test image (8x8 JPEG from test-resources/blue_square.jpg)
    let test_image = "data:image/jpeg;base64,/9j/4AAQSkZJRgABAQEAYABgAAD/2wBDAAEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQH/2wBDAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQH/wAARCAAIAAgDAREAAhEBAxEB/8QAFQABAQAAAAAAAAAAAAAAAAAAAAv/xAAUEAEAAAAAAAAAAAAAAAAAAAAA/8QAFQEBAQAAAAAAAAAAAAAAAAAAAAX/xAAUEQEAAAAAAAAAAAAAAAAAAAAA/9oADAMBAAIRAxEAPwCwAA8A/9k=";

    let test_request = OpenAIRequest {
        model: test_model,
        messages: vec![
            Message::User {
                role: "user".to_string(),
                content: MessageContent::Array(vec![
                    ContentPart::Text {
                        text: "What's in this image?".to_string(),
                    },
                    ContentPart::ImageUrl {
                        image_url: ImageUrl {
                            url: test_image.to_string(),
                            detail: None,
                        },
                    },
                ]),
            }
        ],
        max_tokens: None,  // Older models use this
        max_completion_tokens: Some(10),  // Newer models (gpt-5-nano, gpt-4o, etc) require this
        temperature: None,  // Don't set - some newer models only support default
        top_p: None,
        n: None,
        stream: Some(false),
        stop: None,
        presence_penalty: None,
        frequency_penalty: None,
        logit_bias: None,
        user: None,
        tools: None,
        tool_choice: None,
    };

    match client.chat_completion(backend_name, test_request, api_key).await {
        Ok(_) => {
            info!("✓ Backend {} supports VISION capability", backend_name);
            CapabilityTestResult {
                capability: Capability::Vision,
                supported: true,
                error: None,
                streaming: false,
            }
        },
        Err(e) => {
            let error_msg = e.to_string();
            // Check if error indicates unsupported capability vs other error
            let is_unsupported = error_msg.contains("vision")
                || error_msg.contains("image")
                || error_msg.contains("multimodal")
                || error_msg.contains("not supported");

            if is_unsupported {
                info!("○ Backend {} does not support VISION capability", backend_name);
            } else {
                warn!("✗ Backend {} VISION test failed: {}", backend_name, e);
            }

            CapabilityTestResult {
                capability: Capability::Vision,
                supported: false,
                error: Some(error_msg),
                streaming: false,
            }
        }
    }
}

/// Test if a backend supports function calling
async fn test_function_calling_capability(
    client: &BackendClient,
    backend_name: &str,
    backend_config: &BackendConfig,
    api_key: &str,
) -> CapabilityTestResult {
    debug!("Testing function calling capability for backend: {}", backend_name);

    // Get a model name from the backend's model mapping
    let test_model = match backend_config.model_mapping.values().next() {
        Some(model) => model.clone(),
        None => {
            return CapabilityTestResult {
                capability: Capability::FunctionCalling,
                supported: false,
                error: Some(format!(
                    "Backend '{}' has no model_mapping configured. Please add at least one model mapping in the config file.",
                    backend_name
                )),
                streaming: false,
            };
        }
    };

    let mut parameters = HashMap::new();
    parameters.insert("type".to_string(), serde_json::json!("object"));
    parameters.insert("properties".to_string(), serde_json::json!({
        "query": {
            "type": "string",
            "description": "Search query"
        }
    }));
    parameters.insert("required".to_string(), serde_json::json!(["query"]));

    let test_request = OpenAIRequest {
        model: test_model,
        messages: vec![
            Message::User {
                role: "user".to_string(),
                content: MessageContent::Text("Search for hello".to_string()),
            }
        ],
        max_tokens: None,  // Older models use this
        max_completion_tokens: Some(10),  // Newer models (gpt-5-nano, gpt-4o, etc) require this
        temperature: None,  // Don't set - some newer models only support default
        top_p: None,
        n: None,
        stream: Some(false),
        stop: None,
        presence_penalty: None,
        frequency_penalty: None,
        logit_bias: None,
        user: None,
        tools: Some(vec![
            Tool {
                tool_type: "function".to_string(),
                function: Function {
                    name: "search".to_string(),
                    description: Some("Search function".to_string()),
                    parameters: Some(serde_json::Value::Object(
                        parameters.into_iter().collect()
                    )),
                },
            }
        ]),
        tool_choice: Some(ToolChoice::String("auto".to_string())),
    };

    match client.chat_completion(backend_name, test_request, api_key).await {
        Ok(_) => {
            info!("✓ Backend {} supports FUNCTION_CALLING capability", backend_name);
            CapabilityTestResult {
                capability: Capability::FunctionCalling,
                supported: true,
                error: None,
                streaming: false,
            }
        },
        Err(e) => {
            let error_msg = e.to_string();
            let is_unsupported = error_msg.contains("tool")
                || error_msg.contains("function")
                || error_msg.contains("not supported");

            if is_unsupported {
                info!("○ Backend {} does not support FUNCTION_CALLING capability", backend_name);
            } else {
                warn!("✗ Backend {} FUNCTION_CALLING test failed: {}", backend_name, e);
            }

            CapabilityTestResult {
                capability: Capability::FunctionCalling,
                supported: false,
                error: Some(error_msg),
                streaming: false,
            }
        }
    }
}

/// Test if a Gemini backend supports basic text capability
async fn test_gemini_text_capability(
    client: &BackendClient,
    backend_name: &str,
    backend_config: &BackendConfig,
    api_key: &str,
) -> CapabilityTestResult {
    debug!("Testing text capability for Gemini backend: {}", backend_name);

    // Get a model name from the backend's model mapping
    let test_model = match backend_config.model_mapping.values().next() {
        Some(model) => model.clone(),
        None => {
            return CapabilityTestResult {
                capability: Capability::Text,
                supported: false,
                error: Some(format!(
                    "Backend '{}' has no model_mapping configured. Please add at least one model mapping in the config file.",
                    backend_name
                )),
                streaming: false,
            };
        }
    };

    let test_request = GeminiRequest {
        contents: vec![Content {
            parts: vec![Part::Text { text: "Hi".to_string() }],
            role: "user".to_string(),
        }],
        tools: None,
        tool_config: None,
        safety_settings: None,
        generation_config: None,
        system_instruction: None,
    };

    match client.gemini_generate_content(test_request, &test_model, api_key).await {
        Ok(_) => {
            info!("✓ Backend {} supports TEXT capability", backend_name);
            CapabilityTestResult {
                capability: Capability::Text,
                supported: true,
                error: None,
                streaming: false,
            }
        },
        Err(e) => {
            warn!("✗ Backend {} TEXT test failed: {}", backend_name, e);
            CapabilityTestResult {
                capability: Capability::Text,
                supported: false,
                error: Some(e.to_string()),
                streaming: false,
            }
        }
    }
}

/// Test if a Gemini backend supports vision capability
async fn test_gemini_vision_capability(
    client: &BackendClient,
    backend_name: &str,
    backend_config: &BackendConfig,
    api_key: &str,
) -> CapabilityTestResult {
    debug!("Testing vision capability for Gemini backend: {}", backend_name);

    // Get a model name from the backend's model mapping
    let test_model = match backend_config.model_mapping.values().next() {
        Some(model) => model.clone(),
        None => {
            return CapabilityTestResult {
                capability: Capability::Vision,
                supported: false,
                error: Some(format!(
                    "Backend '{}' has no model_mapping configured. Please add at least one model mapping in the config file.",
                    backend_name
                )),
                streaming: false,
            };
        }
    };

    // Blue square test image (8x8 JPEG)
    let test_image_base64 = "/9j/4AAQSkZJRgABAQEAYABgAAD/2wBDAAEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQH/2wBDAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQH/wAARCAAIAAgDAREAAhEBAxEB/8QAFQABAQAAAAAAAAAAAAAAAAAAAAv/xAAUEAEAAAAAAAAAAAAAAAAAAAAA/8QAFQEBAQAAAAAAAAAAAAAAAAAAAAX/xAAUEQEAAAAAAAAAAAAAAAAAAAAA/9oADAMBAAIRAxEAPwCwAA8A/9k=";

    let test_request = GeminiRequest {
        contents: vec![Content {
            parts: vec![
                Part::Text { text: "What's in this image?".to_string() },
                Part::InlineData {
                    inline_data: InlineData {
                        mime_type: "image/jpeg".to_string(),
                        data: test_image_base64.to_string(),
                    },
                },
            ],
            role: "user".to_string(),
        }],
        tools: None,
        tool_config: None,
        safety_settings: None,
        generation_config: None,
        system_instruction: None,
    };

    match client.gemini_generate_content(test_request, &test_model, api_key).await {
        Ok(_) => {
            info!("✓ Backend {} supports VISION capability", backend_name);
            CapabilityTestResult {
                capability: Capability::Vision,
                supported: true,
                error: None,
                streaming: false,
            }
        },
        Err(e) => {
            let error_msg = e.to_string();
            let is_unsupported = error_msg.contains("vision")
                || error_msg.contains("image")
                || error_msg.contains("multimodal")
                || error_msg.contains("not supported");

            if is_unsupported {
                info!("○ Backend {} does not support VISION capability", backend_name);
            } else {
                warn!("✗ Backend {} VISION test failed: {}", backend_name, e);
            }

            CapabilityTestResult {
                capability: Capability::Vision,
                supported: false,
                error: Some(error_msg),
                streaming: false,
            }
        }
    }
}

/// Test if a Gemini backend supports function calling
async fn test_gemini_function_calling_capability(
    client: &BackendClient,
    backend_name: &str,
    backend_config: &BackendConfig,
    api_key: &str,
) -> CapabilityTestResult {
    debug!("Testing function calling capability for Gemini backend: {}", backend_name);

    // Get a model name from the backend's model mapping
    let test_model = match backend_config.model_mapping.values().next() {
        Some(model) => model.clone(),
        None => {
            return CapabilityTestResult {
                capability: Capability::FunctionCalling,
                supported: false,
                error: Some(format!(
                    "Backend '{}' has no model_mapping configured. Please add at least one model mapping in the config file.",
                    backend_name
                )),
                streaming: false,
            };
        }
    };

    let test_request = GeminiRequest {
        contents: vec![Content {
            parts: vec![Part::Text { text: "Search for hello".to_string() }],
            role: "user".to_string(),
        }],
        tools: Some(vec![GeminiTool {
            function_declarations: vec![FunctionDeclaration {
                name: "search".to_string(),
                description: "Search function".to_string(),
                parameters: Some(serde_json::json!({
                    "type": "object",
                    "properties": {
                        "query": {
                            "type": "string",
                            "description": "Search query"
                        }
                    },
                    "required": ["query"]
                })),
                parameters_json_schema: None,
            }],
        }]),
        tool_config: None,
        safety_settings: None,
        generation_config: None,
        system_instruction: None,
    };

    match client.gemini_generate_content(test_request, &test_model, api_key).await {
        Ok(_) => {
            info!("✓ Backend {} supports FUNCTION_CALLING capability", backend_name);
            CapabilityTestResult {
                capability: Capability::FunctionCalling,
                supported: true,
                error: None,
                streaming: false,
            }
        },
        Err(e) => {
            let error_msg = e.to_string();
            let is_unsupported = error_msg.contains("tool")
                || error_msg.contains("function")
                || error_msg.contains("not supported");

            if is_unsupported {
                info!("○ Backend {} does not support FUNCTION_CALLING capability", backend_name);
            } else {
                warn!("✗ Backend {} FUNCTION_CALLING test failed: {}", backend_name, e);
            }

            CapabilityTestResult {
                capability: Capability::FunctionCalling,
                supported: false,
                error: Some(error_msg),
                streaming: false,
            }
        }
    }
}

/// Test if an OpenAI backend supports streaming capability
async fn test_streaming_capability(
    client: &BackendClient,
    backend_name: &str,
    backend_config: &BackendConfig,
    api_key: &str,
) -> CapabilityTestResult {
    debug!("Testing streaming capability for OpenAI backend: {}", backend_name);

    // Get a model name from the backend's model mapping
    let test_model = match backend_config.model_mapping.values().next() {
        Some(model) => model.clone(),
        None => {
            return CapabilityTestResult {
                capability: Capability::Text,
                supported: false,
                error: Some(format!(
                    "Backend '{}' has no model_mapping configured. Please add at least one model mapping in the config file.",
                    backend_name
                )),
                streaming: false,
            };
        }
    };

    let test_request = OpenAIRequest {
        model: test_model.clone(),
        messages: vec![
            Message::User {
                role: "user".to_string(),
                content: MessageContent::Text("Count from 1 to 5".to_string()),
            }
        ],
        max_tokens: None,
        max_completion_tokens: Some(500),
        temperature: None,
        top_p: None,
        n: None,
        stream: Some(true),
        stop: None,
        presence_penalty: None,
        frequency_penalty: None,
        logit_bias: None,
        user: None,
        tools: None,
        tool_choice: None,
    };

    match client.chat_completion_stream(backend_name, test_request, api_key).await {
        Ok(mut stream) => {
            // Try to consume at least one chunk to verify streaming works
            use futures::StreamExt;
            match stream.next().await {
                Some(Ok(_chunk)) => {
                    info!("✓ Backend {} supports STREAMING capability", backend_name);
                    CapabilityTestResult {
                        capability: Capability::Text,
                        supported: true,
                        error: None,
                        streaming: true,
                    }
                },
                Some(Err(e)) => {
                    warn!("✗ Backend {} STREAMING test failed: {}", backend_name, e);
                    CapabilityTestResult {
                        capability: Capability::Text,
                        supported: false,
                        error: Some(format!("Streaming error: {}", e)),
                        streaming: true,
                    }
                },
                None => {
                    warn!("✗ Backend {} STREAMING test returned empty stream", backend_name);
                    CapabilityTestResult {
                        capability: Capability::Text,
                        supported: false,
                        error: Some("Empty stream".to_string()),
                        streaming: true,
                    }
                }
            }
        },
        Err(e) => {
            warn!("✗ Backend {} STREAMING test failed: {}", backend_name, e);
            CapabilityTestResult {
                capability: Capability::Text,
                supported: false,
                error: Some(e.to_string()),
                streaming: false,
            }
        }
    }
}

/// Test if a Gemini backend supports streaming capability
async fn test_gemini_streaming_capability(
    client: &BackendClient,
    backend_name: &str,
    backend_config: &BackendConfig,
    api_key: &str,
) -> CapabilityTestResult {
    debug!("Testing streaming capability for Gemini backend: {}", backend_name);

    // Get a model name from the backend's model mapping
    let test_model = match backend_config.model_mapping.values().next() {
        Some(model) => model.clone(),
        None => {
            return CapabilityTestResult {
                capability: Capability::Text,
                supported: false,
                error: Some(format!(
                    "Backend '{}' has no model_mapping configured. Please add at least one model mapping in the config file.",
                    backend_name
                )),
                streaming: false,
            };
        }
    };

    let test_request = GeminiRequest {
        contents: vec![Content {
            parts: vec![Part::Text { text: "Count from 1 to 5".to_string() }],
            role: "user".to_string(),
        }],
        tools: None,
        tool_config: None,
        safety_settings: None,
        generation_config: None,
        system_instruction: None,
    };

    match client.gemini_stream_generate_content(test_request, &test_model, api_key).await {
        Ok(mut stream) => {
            // Try to consume at least one chunk to verify streaming works
            use futures::StreamExt;
            match stream.next().await {
                Some(Ok(_chunk)) => {
                    info!("✓ Backend {} supports STREAMING capability", backend_name);
                    CapabilityTestResult {
                        capability: Capability::Text,
                        supported: true,
                        error: None,
                        streaming: true,
                    }
                },
                Some(Err(e)) => {
                    warn!("✗ Backend {} STREAMING test failed: {}", backend_name, e);
                    CapabilityTestResult {
                        capability: Capability::Text,
                        supported: false,
                        error: Some(format!("Streaming error: {}", e)),
                        streaming: true,
                    }
                },
                None => {
                    warn!("✗ Backend {} STREAMING test returned empty stream", backend_name);
                    CapabilityTestResult {
                        capability: Capability::Text,
                        supported: false,
                        error: Some("Empty stream".to_string()),
                        streaming: true,
                    }
                }
            }
        },
        Err(e) => {
            warn!("✗ Backend {} STREAMING test failed: {}", backend_name, e);
            CapabilityTestResult {
                capability: Capability::Text,
                supported: false,
                error: Some(e.to_string()),
                streaming: false,
            }
        }
    }
}

/// Test if a Gemini backend supports audio capability
async fn test_gemini_audio_capability(
    client: &BackendClient,
    backend_name: &str,
    backend_config: &BackendConfig,
    api_key: &str,
) -> CapabilityTestResult {
    debug!("Testing audio capability for Gemini backend: {}", backend_name);

    // Get a model name from the backend's model mapping
    let test_model = match backend_config.model_mapping.values().next() {
        Some(model) => model.clone(),
        None => {
            return CapabilityTestResult {
                capability: Capability::Audio,
                supported: false,
                error: Some(format!(
                    "Backend '{}' has no model_mapping configured. Please add at least one model mapping in the config file.",
                    backend_name
                )),
                streaming: false,
            };
        }
    };

    // Minimal audio sample (base64-encoded WAV)
    let audio_base64 = "UklGRiQAAABXQVZFZm10IBAAAAABAAEARKwAAIhYAQACABAAZGF0YQAAAAA=";

    let test_request = GeminiRequest {
        contents: vec![Content {
            parts: vec![
                Part::Text { text: "What do you hear in this audio?".to_string() },
                Part::InlineData {
                    inline_data: InlineData {
                        mime_type: "audio/wav".to_string(),
                        data: audio_base64.to_string(),
                    }
                }
            ],
            role: "user".to_string(),
        }],
        tools: None,
        tool_config: None,
        safety_settings: None,
        generation_config: None,
        system_instruction: None,
    };

    match client.gemini_generate_content(test_request, &test_model, api_key).await {
        Ok(_) => {
            info!("✓ Backend {} supports AUDIO capability", backend_name);
            CapabilityTestResult {
                capability: Capability::Audio,
                supported: true,
                error: None,
                streaming: false,
            }
        },
        Err(e) => {
            let error_msg = e.to_string();
            let is_unsupported = error_msg.contains("audio")
                || error_msg.contains("not supported")
                || error_msg.contains("unsupported");

            if is_unsupported {
                info!("○ Backend {} does not support AUDIO capability", backend_name);
            } else {
                warn!("✗ Backend {} AUDIO test failed: {}", backend_name, e);
            }

            CapabilityTestResult {
                capability: Capability::Audio,
                supported: false,
                error: Some(error_msg),
                streaming: false,
            }
        }
    }
}

/// Run diagnostic tests on a backend to detect its capabilities
pub async fn diagnose_backend(
    backend_name: &str,
    backend_config: &BackendConfig,
    config: &Config,
) -> BackendDiagnostics {
    info!("Running diagnostics for backend: {} ({})", backend_name, backend_config.url);

    // Create a temporary backend client for testing
    let client = match BackendClient::new(backend_name, config).await {
        Ok(client) => client,
        Err(e) => {
            return BackendDiagnostics {
                backend_name: backend_name.to_string(),
                url: backend_config.url.clone(),
                reachable: false,
                detected_capabilities: HashSet::new(),
                test_results: vec![],
                error: Some(format!("Failed to create client: {}", e)),
            };
        }
    };

    // Get API key based on backend protocol
    let api_key_env_var = if backend_config.protocol == "gemini" {
        "GEMINI_API_KEY"
    } else {
        "OPENAI_API_KEY"
    };

    let api_key = config.get_api_key(backend_name, api_key_env_var)
        .unwrap_or_else(|| "test-key".to_string());

    // Run capability tests
    let mut test_results = Vec::new();
    let mut detected_capabilities = HashSet::new();

    // Use protocol-specific test functions
    let is_gemini_protocol = backend_config.protocol == "gemini";

    // Test text capability (required for all backends)
    let text_result = if is_gemini_protocol {
        test_gemini_text_capability(&client, backend_name, backend_config, &api_key).await
    } else {
        test_text_capability(&client, backend_name, backend_config, &api_key).await
    };

    if text_result.supported {
        detected_capabilities.insert(Capability::Text);
    }
    let backend_reachable = text_result.supported;
    test_results.push(text_result);

    // Only test other capabilities if backend is reachable
    if backend_reachable {
        // Test vision capability
        let vision_result = if is_gemini_protocol {
            test_gemini_vision_capability(&client, backend_name, backend_config, &api_key).await
        } else {
            test_vision_capability(&client, backend_name, backend_config, &api_key).await
        };

        if vision_result.supported {
            detected_capabilities.insert(Capability::Vision);
        }
        test_results.push(vision_result);

        // Test function calling capability
        let function_result = if is_gemini_protocol {
            test_gemini_function_calling_capability(&client, backend_name, backend_config, &api_key).await
        } else {
            test_function_calling_capability(&client, backend_name, backend_config, &api_key).await
        };

        if function_result.supported {
            detected_capabilities.insert(Capability::FunctionCalling);
        }
        test_results.push(function_result);

        // Test streaming capability
        let streaming_result = if is_gemini_protocol {
            test_gemini_streaming_capability(&client, backend_name, backend_config, &api_key).await
        } else {
            test_streaming_capability(&client, backend_name, backend_config, &api_key).await
        };
        test_results.push(streaming_result);

        // Test audio capability (Gemini only)
        if is_gemini_protocol {
            let audio_result = test_gemini_audio_capability(&client, backend_name, backend_config, &api_key).await;
            if audio_result.supported {
                detected_capabilities.insert(Capability::Audio);
            }
            test_results.push(audio_result);
        }

        // Note: Video testing would require actual video data
        // For now, we'll skip that and rely on configuration
    }

    BackendDiagnostics {
        backend_name: backend_name.to_string(),
        url: backend_config.url.clone(),
        reachable: backend_reachable,
        detected_capabilities,
        test_results,
        error: None,
    }
}

/// Run diagnostics on all configured backends
pub async fn diagnose_all_backends(config: &Config) -> HashMap<String, BackendDiagnostics> {
    info!("Running diagnostics on all backends...");

    let mut results = HashMap::new();

    for (backend_name, backend_config) in config.backends.iter() {
        let diagnostics = diagnose_backend(backend_name, backend_config, config).await;
        results.insert(backend_name.clone(), diagnostics);
    }

    info!("Diagnostics complete for {} backends", results.len());
    results
}

/// Print diagnostic results to console
pub fn print_diagnostics(config: &Config, results: &HashMap<String, BackendDiagnostics>) {
    println!("\n╔════════════════════════════════════════════════════════════╗");
    println!("║          Backend Capability Diagnostics                   ║");
    println!("╚════════════════════════════════════════════════════════════╝\n");

    for (backend_name, diagnostics) in results.iter() {
        let backend_config = config.backends.get(backend_name);
        let is_auto_mode = backend_config.map(|c| c.capabilities.is_empty()).unwrap_or(false);

        println!("Backend: {} ({})", backend_name, diagnostics.url);

        if !diagnostics.reachable {
            println!("  Status: ✗ UNREACHABLE");
            if let Some(error) = &diagnostics.error {
                println!("  Error: {}", error);
            }
            println!();
            continue;
        }

        println!("  Status: ✓ REACHABLE");

        if is_auto_mode {
            println!("  Mode: AUTO (accepts all request types)");
        }

        println!("  Detected Capabilities:");

        for capability in &diagnostics.detected_capabilities {
            println!("    ✓ {}", capability.as_str());
        }

        // Show capabilities that were NOT detected
        let all_caps = vec![
            Capability::Text,
            Capability::Vision,
            Capability::FunctionCalling,
        ];

        for cap in all_caps {
            if !diagnostics.detected_capabilities.contains(&cap) {
                println!("    ○ {} (not supported)", cap.as_str());
            }
        }

        println!();
    }
}

/// Compare detected capabilities with configured capabilities
pub fn verify_configured_capabilities(
    config: &Config,
    diagnostics: &HashMap<String, BackendDiagnostics>,
) -> Vec<String> {
    let mut warnings = Vec::new();

    for (backend_name, backend_config) in config.backends.iter() {
        if let Some(diag) = diagnostics.get(backend_name) {
            if !diag.reachable {
                continue; // Skip unreachable backends
            }

            // Skip auto-mode backends (empty capabilities = accepts all)
            if backend_config.capabilities.is_empty() {
                continue;
            }

            // Check if configured capabilities match detected ones
            let configured: HashSet<Capability> = backend_config.capabilities
                .iter()
                .filter_map(|s| Capability::from_str(s))
                .collect();

            // Warn about configured but not detected capabilities
            for cap in &configured {
                if !diag.detected_capabilities.contains(cap) {
                    let warning = format!(
                        "Backend '{}' is configured with '{}' capability but it was not detected during testing",
                        backend_name, cap.as_str()
                    );
                    warnings.push(warning);
                }
            }

            // Suggest adding detected but not configured capabilities
            for cap in &diag.detected_capabilities {
                if !configured.contains(cap) {
                    let warning = format!(
                        "Backend '{}' supports '{}' capability but it's not configured. Consider adding it to the config.",
                        backend_name, cap.as_str()
                    );
                    warnings.push(warning);
                }
            }
        }
    }

    warnings
}

// ========== Frontend Diagnostics ==========

/// Result of a frontend API test
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct FrontendTestResult {
    pub test_name: String,
    pub endpoint: String,
    pub passed: bool,
    pub error: Option<String>,
    pub response_time_ms: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub backend_used: Option<String>,  // Which backend handled the request
    #[serde(skip_serializing_if = "Option::is_none")]
    pub capability: Option<String>,  // Which capability was tested (text, vision, function_calling)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub conversion_mode: Option<String>,  // "native" or "conversion"
}

/// Model-specific test result
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ModelTestResult {
    pub model_name: String,
    pub backend: String,
    pub passed: bool,
    pub error: Option<String>,
    pub response_time_ms: Option<u64>,
}

/// Frontend API information
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct FrontendApiInfo {
    pub api_name: String,
    pub supported_capabilities: Vec<String>,
    pub passed_capabilities: Vec<String>,  // Capabilities that passed tests
    pub endpoints: Vec<String>,
    pub test_results: Vec<FrontendTestResult>,
    pub model_tests: Vec<ModelTestResult>,  // Per-model test results
}

/// Frontend diagnostic results
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct FrontendDiagnostics {
    pub gemini_api: FrontendApiInfo,
    pub openai_api: FrontendApiInfo,
}

/// Combined diagnostics (backend + frontend)
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct CompleteDiagnostics {
    pub backends: HashMap<String, BackendDiagnostics>,
    pub frontends: FrontendDiagnostics,
}

/// Test a specific model on Gemini API
async fn test_gemini_model(port: u16, model_name: &str) -> ModelTestResult {
    let client = reqwest::Client::new();
    let start = std::time::Instant::now();

    // Sanitize model name for Gemini API (replace : with -)
    let sanitized_model = model_name.replace(':', "-");
    let url = format!("http://localhost:{}/v1beta/models/{}:generateContent", port, sanitized_model);

    let body = serde_json::json!({
        "contents": [{
            "parts": [{"text": "Hi"}],
            "role": "user"
        }],
        "generationConfig": {
            "maxOutputTokens": 200,
            "temperature": 0.0
        }
    });

    info!("Testing Gemini model '{}' (sanitized: '{}') via diagnostics API", model_name, sanitized_model);

    match client.post(&url).json(&body).send().await {
        Ok(response) => {
            let response_time = start.elapsed().as_millis() as u64;
            let status = response.status();

            if status.is_success() {
                // Try to extract backend from response or default to "unknown"
                info!("✓ Model '{}' test passed ({} ms)", model_name, response_time);
                ModelTestResult {
                    model_name: model_name.to_string(),
                    backend: "unknown".to_string(),  // Could be extracted from logs
                    passed: true,
                    error: None,
                    response_time_ms: Some(response_time),
                }
            } else {
                let error_text = response.text().await.unwrap_or_else(|_| format!("HTTP {}", status));
                ModelTestResult {
                    model_name: model_name.to_string(),
                    backend: "unknown".to_string(),
                    passed: false,
                    error: Some(error_text),
                    response_time_ms: Some(response_time),
                }
            }
        },
        Err(e) => {
            ModelTestResult {
                model_name: model_name.to_string(),
                backend: "unknown".to_string(),
                passed: false,
                error: Some(e.to_string()),
                response_time_ms: None,
            }
        }
    }
}

/// Test Gemini API with cross-protocol backend support
async fn test_gemini_api_with_backend(
    port: u16,
    model_name: &str,
    backend_name: &str,
    backend_protocol: &str,
    capability: &str,
    test_vision: bool,
    test_function: bool,
) -> FrontendTestResult {
    let client = reqwest::Client::new();
    let start = std::time::Instant::now();

    // Sanitize model name for Gemini API (replace : with -)
    let sanitized_model = model_name.replace(':', "-");
    let url = format!("http://localhost:{}/v1beta/models/{}:generateContent", port, sanitized_model);

    // Determine conversion mode
    let conversion_mode = if backend_protocol == "gemini" {
        "native"
    } else {
        "conversion"
    };

    // Build request based on capability
    let body = if test_function {
        serde_json::json!({
            "contents": [{
                "parts": [{"text": "Search for hello"}],
                "role": "user"
            }],
            "tools": [{
                "functionDeclarations": [{
                    "name": "search",
                    "description": "Search function",
                    "parameters": {
                        "type": "object",
                        "properties": {
                            "query": {
                                "type": "string",
                                "description": "Search query"
                            }
                        },
                        "required": ["query"]
                    }
                }]
            }],
            "generationConfig": {
                "maxOutputTokens": 500  // Increased to accommodate thinking tokens
            }
        })
    } else if test_vision {
        // Blue square test image (8x8 JPEG)
        let test_image = "/9j/4AAQSkZJRgABAQEAYABgAAD/2wBDAAEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQH/2wBDAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQH/wAARCAAIAAgDAREAAhEBAxEB/8QAFQABAQAAAAAAAAAAAAAAAAAAAAv/xAAUEAEAAAAAAAAAAAAAAAAAAAAA/8QAFQEBAQAAAAAAAAAAAAAAAAAAAAX/xAAUEQEAAAAAAAAAAAAAAAAAAAAA/9oADAMBAAIRAxEAPwCwAA8A/9k=";
        serde_json::json!({
            "contents": [{
                "parts": [
                    {"text": "What's in this image?"},
                    {"inlineData": {"mimeType": "image/jpeg", "data": test_image}}
                ],
                "role": "user"
            }],
            "generationConfig": {
                "maxOutputTokens": 1000  // Increased to accommodate thinking tokens
            }
        })
    } else {
        serde_json::json!({
            "contents": [{
                "parts": [{"text": "Hi"}],
                "role": "user"
            }],
            "generationConfig": {
                "maxOutputTokens": 500  // Increased to accommodate thinking tokens
            }
        })
    };

    let test_type = if test_function {
        "Function Calling"
    } else if test_vision {
        "Vision"
    } else {
        "Text"
    };

    info!("Testing Gemini API → {} backend ({}) - {} capability",
          backend_name, conversion_mode, test_type);

    match client.post(&url).json(&body).send().await {
        Ok(response) => {
            let response_time = start.elapsed().as_millis() as u64;
            let status = response.status();

            if status.is_success() {
                info!("✓ Gemini API → {} ({}) - {} test passed ({} ms)",
                      backend_name, conversion_mode, test_type, response_time);
                FrontendTestResult {
                    test_name: format!("Gemini API → {} backend: {}", backend_name, test_type),
                    endpoint: "/v1beta/models/*:generateContent".to_string(),
                    passed: true,
                    error: None,
                    response_time_ms: Some(response_time),
                    backend_used: Some(backend_name.to_string()),
                    capability: Some(capability.to_string()),
                    conversion_mode: Some(conversion_mode.to_string()),
                }
            } else {
                let error_text = response.text().await.unwrap_or_else(|_| format!("HTTP {}", status));
                warn!("✗ Gemini API → {} ({}) - {} test failed: {}",
                      backend_name, conversion_mode, test_type, error_text);
                FrontendTestResult {
                    test_name: format!("Gemini API → {} backend: {}", backend_name, test_type),
                    endpoint: "/v1beta/models/*:generateContent".to_string(),
                    passed: false,
                    error: Some(error_text),
                    response_time_ms: Some(response_time),
                    backend_used: Some(backend_name.to_string()),
                    capability: Some(capability.to_string()),
                    conversion_mode: Some(conversion_mode.to_string()),
                }
            }
        },
        Err(e) => {
            warn!("✗ Gemini API → {} ({}) - {} test error: {}",
                  backend_name, conversion_mode, test_type, e);
            FrontendTestResult {
                test_name: format!("Gemini API → {} backend: {}", backend_name, test_type),
                endpoint: "/v1beta/models/*:generateContent".to_string(),
                passed: false,
                error: Some(e.to_string()),
                response_time_ms: None,
                backend_used: Some(backend_name.to_string()),
                capability: Some(capability.to_string()),
                conversion_mode: Some(conversion_mode.to_string()),
            }
        }
    }
}

/// Test Gemini API frontend endpoint
pub async fn test_gemini_frontend(port: u16, config: &Config) -> Vec<FrontendTestResult> {
    let mut results = Vec::new();

    // Test ALL backends (both Gemini and OpenAI protocol) through Gemini API
    for (backend_name, backend_config) in config.backends.iter() {
        // Skip unreachable backends
        if backend_config.url.is_empty() {
            continue;
        }

        // Get model from this backend's model_mapping
        let model_name = match backend_config.model_mapping.keys().next() {
            Some(model) => model.clone(),
            None => {
                warn!("Backend '{}' has no model_mapping, skipping", backend_name);
                continue;
            }
        };

        // Test text capability (all backends should support this)
        let text_result = test_gemini_api_with_backend(
            port,
            &model_name,
            backend_name,
            &backend_config.protocol,
            "text",
            false,
            false,
        ).await;
        results.push(text_result);

        // Test vision capability if configured
        if backend_config.capabilities.is_empty() ||
           backend_config.capabilities.iter().any(|c| c == "vision") {
            let vision_result = test_gemini_api_with_backend(
                port,
                &model_name,
                backend_name,
                &backend_config.protocol,
                "vision",
                true,
                false,
            ).await;
            results.push(vision_result);
        }

        // Test function calling capability if configured
        if backend_config.capabilities.is_empty() ||
           backend_config.capabilities.iter().any(|c| c == "function_calling") {
            let function_result = test_gemini_api_with_backend(
                port,
                &model_name,
                backend_name,
                &backend_config.protocol,
                "function_calling",
                false,
                true,
            ).await;
            results.push(function_result);
        }
    }

    results
}

/// Test a specific model on OpenAI API
async fn test_openai_model(port: u16, model_name: &str) -> ModelTestResult {
    let client = reqwest::Client::new();
    let start = std::time::Instant::now();
    let url = format!("http://localhost:{}/v1/chat/completions", port);

    let body = serde_json::json!({
        "model": model_name,
        "messages": [{"role": "user", "content": "Hi"}],
        "max_completion_tokens": 50  // Newer OpenAI models require this
    });

    info!("Testing OpenAI model '{}' via diagnostics API", model_name);

    match client.post(&url).json(&body).send().await {
        Ok(response) => {
            let response_time = start.elapsed().as_millis() as u64;
            let status = response.status();

            if status.is_success() {
                info!("✓ Model '{}' test passed ({} ms)", model_name, response_time);
                ModelTestResult {
                    model_name: model_name.to_string(),
                    backend: "unknown".to_string(),
                    passed: true,
                    error: None,
                    response_time_ms: Some(response_time),
                }
            } else {
                let error_text = response.text().await.unwrap_or_else(|_| format!("HTTP {}", status));
                warn!("✗ Model '{}' test failed: HTTP {}", model_name, status);
                ModelTestResult {
                    model_name: model_name.to_string(),
                    backend: "unknown".to_string(),
                    passed: false,
                    error: Some(error_text),
                    response_time_ms: Some(response_time),
                }
            }
        },
        Err(e) => {
            ModelTestResult {
                model_name: model_name.to_string(),
                backend: "unknown".to_string(),
                passed: false,
                error: Some(e.to_string()),
                response_time_ms: None,
            }
        }
    }
}

/// Test OpenAI API with cross-protocol backend support
async fn test_openai_api_with_backend(
    port: u16,
    model_name: &str,
    backend_name: &str,
    backend_protocol: &str,
    capability: &str,
    test_vision: bool,
    test_function: bool,
) -> FrontendTestResult {
    let client = reqwest::Client::new();
    let start = std::time::Instant::now();
    let url = format!("http://localhost:{}/v1/chat/completions", port);

    // Determine conversion mode
    let conversion_mode = if backend_protocol == "openai" {
        "native"
    } else {
        "conversion"
    };

    // Build request based on capability
    let body = if test_function {
        serde_json::json!({
            "model": model_name,
            "messages": [{"role": "user", "content": "Search for hello"}],
            "tools": [{
                "type": "function",
                "function": {
                    "name": "search",
                    "description": "Search function",
                    "parameters": {
                        "type": "object",
                        "properties": {
                            "query": {
                                "type": "string",
                                "description": "Search query"
                            }
                        },
                        "required": ["query"]
                    }
                }
            }],
            "tool_choice": "auto",
            "max_completion_tokens": 50
        })
    } else if test_vision {
        // Blue square test image (8x8 JPEG)
        let test_image = "data:image/jpeg;base64,/9j/4AAQSkZJRgABAQEAYABgAAD/2wBDAAEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQH/2wBDAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQH/wAARCAAIAAgDAREAAhEBAxEB/8QAFQABAQAAAAAAAAAAAAAAAAAAAAv/xAAUEAEAAAAAAAAAAAAAAAAAAAAA/8QAFQEBAQAAAAAAAAAAAAAAAAAAAAX/xAAUEQEAAAAAAAAAAAAAAAAAAAAA/9oADAMBAAIRAxEAPwCwAA8A/9k=";
        serde_json::json!({
            "model": model_name,
            "messages": [{
                "role": "user",
                "content": [
                    {"type": "text", "text": "What's in this image?"},
                    {"type": "image_url", "image_url": {"url": test_image}}
                ]
            }],
            "max_completion_tokens": 50
        })
    } else {
        serde_json::json!({
            "model": model_name,
            "messages": [{"role": "user", "content": "Hi"}],
            "max_completion_tokens": 50
        })
    };

    let test_type = if test_function {
        "Function Calling"
    } else if test_vision {
        "Vision"
    } else {
        "Text"
    };

    info!("Testing OpenAI API → {} backend ({}) - {} capability",
          backend_name, conversion_mode, test_type);

    match client.post(&url).json(&body).send().await {
        Ok(response) => {
            let response_time = start.elapsed().as_millis() as u64;
            let status = response.status();

            if status.is_success() {
                info!("✓ OpenAI API → {} ({}) - {} test passed ({} ms)",
                      backend_name, conversion_mode, test_type, response_time);
                FrontendTestResult {
                    test_name: format!("OpenAI API → {} backend: {}", backend_name, test_type),
                    endpoint: "/v1/chat/completions".to_string(),
                    passed: true,
                    error: None,
                    response_time_ms: Some(response_time),
                    backend_used: Some(backend_name.to_string()),
                    capability: Some(capability.to_string()),
                    conversion_mode: Some(conversion_mode.to_string()),
                }
            } else {
                let error_text = response.text().await.unwrap_or_else(|_| format!("HTTP {}", status));
                warn!("✗ OpenAI API → {} ({}) - {} test failed: {}",
                      backend_name, conversion_mode, test_type, error_text);
                FrontendTestResult {
                    test_name: format!("OpenAI API → {} backend: {}", backend_name, test_type),
                    endpoint: "/v1/chat/completions".to_string(),
                    passed: false,
                    error: Some(error_text),
                    response_time_ms: Some(response_time),
                    backend_used: Some(backend_name.to_string()),
                    capability: Some(capability.to_string()),
                    conversion_mode: Some(conversion_mode.to_string()),
                }
            }
        },
        Err(e) => {
            warn!("✗ OpenAI API → {} ({}) - {} test error: {}",
                  backend_name, conversion_mode, test_type, e);
            FrontendTestResult {
                test_name: format!("OpenAI API → {} backend: {}", backend_name, test_type),
                endpoint: "/v1/chat/completions".to_string(),
                passed: false,
                error: Some(e.to_string()),
                response_time_ms: None,
                backend_used: Some(backend_name.to_string()),
                capability: Some(capability.to_string()),
                conversion_mode: Some(conversion_mode.to_string()),
            }
        }
    }
}

/// Test OpenAI API frontend endpoint
pub async fn test_openai_frontend(port: u16, config: &Config) -> Vec<FrontendTestResult> {
    let mut results = Vec::new();

    // Test ALL backends (both OpenAI and Gemini protocol) through OpenAI API
    for (backend_name, backend_config) in config.backends.iter() {
        // Skip unreachable backends
        if backend_config.url.is_empty() {
            continue;
        }

        // Get model from this backend's model_mapping
        let model_name = match backend_config.model_mapping.keys().next() {
            Some(model) => model.clone(),
            None => {
                warn!("Backend '{}' has no model_mapping, skipping", backend_name);
                continue;
            }
        };

        // Test text capability (all backends should support this)
        let text_result = test_openai_api_with_backend(
            port,
            &model_name,
            backend_name,
            &backend_config.protocol,
            "text",
            false,
            false,
        ).await;
        results.push(text_result);

        // Test vision capability if configured
        if backend_config.capabilities.is_empty() ||
           backend_config.capabilities.iter().any(|c| c == "vision") {
            let vision_result = test_openai_api_with_backend(
                port,
                &model_name,
                backend_name,
                &backend_config.protocol,
                "vision",
                true,
                false,
            ).await;
            results.push(vision_result);
        }

        // Test function calling capability if configured
        if backend_config.capabilities.is_empty() ||
           backend_config.capabilities.iter().any(|c| c == "function_calling") {
            let function_result = test_openai_api_with_backend(
                port,
                &model_name,
                backend_name,
                &backend_config.protocol,
                "function_calling",
                false,
                true,
            ).await;
            results.push(function_result);
        }
    }

    results
}

/// Determine which capabilities passed based on test results
fn get_passed_capabilities(test_results: &[FrontendTestResult]) -> Vec<String> {
    let mut passed = Vec::new();

    // Check if basic text test passed
    if test_results.iter().any(|t| t.test_name.contains("Text") && t.passed) {
        passed.push("text".to_string());
    }

    // Check if vision test passed
    if test_results.iter().any(|t| t.test_name.contains("Vision") && t.passed) {
        passed.push("vision".to_string());
    }

    // Check if function calling test passed
    if test_results.iter().any(|t| t.test_name.contains("Function") && t.passed) {
        passed.push("function_calling".to_string());
    }

    // Check if streaming test passed
    if test_results.iter().any(|t| t.test_name.contains("Streaming") && t.passed) {
        passed.push("streaming".to_string());
    }

    passed
}

/// Run complete diagnostics (backend + frontend)
pub async fn run_complete_diagnostics(
    port: u16,
    config: &Config,
    backends: HashMap<String, BackendDiagnostics>,
) -> CompleteDiagnostics {
    let gemini_tests = test_gemini_frontend(port, config).await;
    let openai_tests = test_openai_frontend(port, config).await;

    let gemini_passed = get_passed_capabilities(&gemini_tests);
    let openai_passed = get_passed_capabilities(&openai_tests);

    // Test each backend model through both frontend APIs
    let mut gemini_model_tests = Vec::new();
    let mut openai_model_tests = Vec::new();

    info!("Testing {} models through frontend APIs...", backends.len());

    for (backend_name, backend_diag) in backends.iter() {
        // Only test reachable backends
        if !backend_diag.reachable {
            continue;
        }

        // Get model from this backend's model_mapping
        let backend_config = config.backends.get(backend_name);
        if backend_config.is_none() {
            warn!("Backend '{}' not found in config, skipping model tests", backend_name);
            continue;
        }

        let backend_cfg = backend_config.unwrap();
        let test_model = backend_cfg.model_mapping.values().next();
        if test_model.is_none() {
            warn!("Backend '{}' has no model_mapping, skipping model tests", backend_name);
            continue;
        }
        let test_model = test_model.unwrap().clone();

        // Only test with matching protocol
        if backend_cfg.protocol == "gemini" {
            info!("Testing backend '{}' (protocol: gemini) with model '{}' through Gemini API...", backend_name, test_model);
            let mut gemini_result = test_gemini_model(port, &test_model).await;
            gemini_result.backend = backend_name.clone();
            gemini_result.model_name = format!("{} (tested via {})", backend_name, test_model);
            gemini_model_tests.push(gemini_result);
        } else if backend_cfg.protocol == "openai" {
            info!("Testing backend '{}' (protocol: openai) with model '{}' through OpenAI API...", backend_name, test_model);
            let mut openai_result = test_openai_model(port, &test_model).await;
            openai_result.backend = backend_name.clone();
            openai_result.model_name = format!("{} (tested via {})", backend_name, test_model);
            openai_model_tests.push(openai_result);
        }
    }

    CompleteDiagnostics {
        backends,
        frontends: FrontendDiagnostics {
            gemini_api: FrontendApiInfo {
                api_name: "Gemini API".to_string(),
                supported_capabilities: vec![
                    "text".to_string(),
                    "vision".to_string(),
                    "audio".to_string(),
                    "video".to_string(),
                    "function_calling".to_string(),
                ],
                passed_capabilities: gemini_passed,
                endpoints: vec![
                    "/v1beta/models/{model}:generateContent".to_string(),
                    "/v1beta/models/{model}:streamGenerateContent".to_string(),
                    "/v1beta/models/{model}:countTokens".to_string(),
                ],
                test_results: gemini_tests,
                model_tests: gemini_model_tests,
            },
            openai_api: FrontendApiInfo {
                api_name: "OpenAI API".to_string(),
                supported_capabilities: vec![
                    "text".to_string(),
                    "vision".to_string(),
                    "function_calling".to_string(),
                ],
                passed_capabilities: openai_passed,
                endpoints: vec![
                    "/v1/chat/completions".to_string(),
                ],
                test_results: openai_tests,
                model_tests: openai_model_tests,
            },
        },
    }
}
