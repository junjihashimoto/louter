// Unit tests for Gemini and OpenAI converters

use louter::ir::{
    GeminiBackendConverter, OpenAIFrontendConverter,
    FrontendConverter, BackendConverter,
    IRRequest, IRResponse, IRMessage, IRContent, IRRole, IRStopReason, IRUsage, IRTool,
};

// ============================================================================
// GeminiBackendConverter Tests
// ============================================================================

#[tokio::test]
async fn test_gemini_format_simple_request() {
    let converter = GeminiBackendConverter::new();

    let ir_request = IRRequest {
        model: "gemini-2.0-flash".to_string(),
        messages: vec![
            IRMessage {
                role: IRRole::User,
                content: vec![IRContent::Text { text: "Hello!".to_string() }],
                name: None,
            }
        ],
        system: None,
        max_tokens: Some(100),
        temperature: Some(0.7),
        top_p: None,
        top_k: None,
        stop_sequences: vec![],
        tools: vec![],
        tool_choice: None,
        stream: false,
        metadata: Default::default(),
    };

    let request_bytes = converter.format_request(&ir_request).await.unwrap();
    let request_json: serde_json::Value = serde_json::from_slice(&request_bytes).unwrap();

    assert_eq!(request_json["contents"][0]["role"], "user");
    assert_eq!(request_json["contents"][0]["parts"][0]["text"], "Hello!");
    assert_eq!(request_json["generationConfig"]["maxOutputTokens"], 100);
    assert!((request_json["generationConfig"]["temperature"].as_f64().unwrap() - 0.7).abs() < 0.01);
}

#[tokio::test]
async fn test_gemini_format_request_with_system() {
    let converter = GeminiBackendConverter::new();

    let ir_request = IRRequest {
        model: "gemini-2.0-flash".to_string(),
        messages: vec![
            IRMessage {
                role: IRRole::User,
                content: vec![IRContent::Text { text: "Hi".to_string() }],
                name: None,
            }
        ],
        system: Some("You are a helpful assistant.".to_string()),
        max_tokens: Some(100),
        temperature: None,
        top_p: None,
        top_k: None,
        stop_sequences: vec![],
        tools: vec![],
        tool_choice: None,
        stream: false,
        metadata: Default::default(),
    };

    let request_bytes = converter.format_request(&ir_request).await.unwrap();
    let request_json: serde_json::Value = serde_json::from_slice(&request_bytes).unwrap();

    assert_eq!(request_json["systemInstruction"]["parts"][0]["text"], "You are a helpful assistant.");
}

#[tokio::test]
async fn test_gemini_format_request_with_tools() {
    let converter = GeminiBackendConverter::new();

    let ir_request = IRRequest {
        model: "gemini-2.0-flash".to_string(),
        messages: vec![
            IRMessage {
                role: IRRole::User,
                content: vec![IRContent::Text { text: "What's the weather?".to_string() }],
                name: None,
            }
        ],
        system: None,
        max_tokens: Some(100),
        temperature: None,
        top_p: None,
        top_k: None,
        stop_sequences: vec![],
        tools: vec![
            IRTool {
                name: "get_weather".to_string(),
                description: "Get current weather".to_string(),
                input_schema: serde_json::json!({
                    "type": "object",
                    "properties": {
                        "location": {"type": "string"}
                    }
                }),
            }
        ],
        tool_choice: None,
        stream: false,
        metadata: Default::default(),
    };

    let request_bytes = converter.format_request(&ir_request).await.unwrap();
    let request_json: serde_json::Value = serde_json::from_slice(&request_bytes).unwrap();

    assert_eq!(request_json["tools"][0]["functionDeclarations"][0]["name"], "get_weather");
    assert_eq!(request_json["tools"][0]["functionDeclarations"][0]["description"], "Get current weather");
}

#[tokio::test]
async fn test_gemini_parse_simple_response() {
    let converter = GeminiBackendConverter::new();

    let response_json = r#"{
        "candidates": [
            {
                "content": {
                    "role": "model",
                    "parts": [
                        {"text": "Hello! How can I help you?"}
                    ]
                },
                "finishReason": "STOP"
            }
        ],
        "usageMetadata": {
            "promptTokenCount": 5,
            "candidatesTokenCount": 8,
            "totalTokenCount": 13
        }
    }"#;

    let ir_response = converter.parse_response(response_json.as_bytes()).await.unwrap();

    assert_eq!(ir_response.role, IRRole::Assistant);
    assert_eq!(ir_response.content.len(), 1);
    match &ir_response.content[0] {
        IRContent::Text { text } => assert_eq!(text, "Hello! How can I help you?"),
        _ => panic!("Expected text content"),
    }
    assert_eq!(ir_response.stop_reason, Some(IRStopReason::EndTurn));
    assert_eq!(ir_response.usage.input_tokens, 5);
    assert_eq!(ir_response.usage.output_tokens, 8);
}

#[tokio::test]
async fn test_gemini_parse_response_with_thinking_tokens() {
    let converter = GeminiBackendConverter::new();

    let response_json = r#"{
        "candidates": [
            {
                "content": {
                    "role": "model",
                    "parts": [
                        {"text": "The answer is 4."}
                    ]
                },
                "finishReason": "STOP"
            }
        ],
        "usageMetadata": {
            "promptTokenCount": 10,
            "candidatesTokenCount": 5,
            "thoughtsTokenCount": 15,
            "totalTokenCount": 30
        }
    }"#;

    let ir_response = converter.parse_response(response_json.as_bytes()).await.unwrap();

    assert_eq!(ir_response.usage.input_tokens, 10);
    assert_eq!(ir_response.usage.output_tokens, 5);
    assert_eq!(ir_response.usage.thinking_tokens, Some(15));
}

// ============================================================================
// OpenAIFrontendConverter Tests
// ============================================================================

#[tokio::test]
async fn test_openai_frontend_parse_simple_request() {
    let converter = OpenAIFrontendConverter::new();

    // Note: OpenAI User message content can be string or array
    // When it's a simple string, it gets deserialized as MessageContent::Text
    let request_json = r#"{
        "model": "gpt-4",
        "messages": [
            {"role": "user", "content": [{"type": "text", "text": "Hello!"}]}
        ],
        "max_tokens": 100,
        "temperature": 0.7
    }"#;

    let ir_request = converter.parse_request(request_json.as_bytes()).await.unwrap();

    assert_eq!(ir_request.model, "gpt-4");
    assert_eq!(ir_request.max_tokens, Some(100));
    assert!((ir_request.temperature.unwrap() - 0.7).abs() < 0.01);
    assert_eq!(ir_request.messages.len(), 1);
    assert_eq!(ir_request.messages[0].role, IRRole::User);
}

#[tokio::test]
async fn test_openai_frontend_parse_with_system() {
    let converter = OpenAIFrontendConverter::new();

    let request_json = r#"{
        "model": "gpt-4",
        "messages": [
            {"role": "system", "content": "You are a helpful assistant."},
            {"role": "user", "content": [{"type": "text", "text": "Hi"}]}
        ]
    }"#;

    let ir_request = converter.parse_request(request_json.as_bytes()).await.unwrap();

    assert_eq!(ir_request.system, Some("You are a helpful assistant.".to_string()));
    assert_eq!(ir_request.messages.len(), 1); // System extracted separately
}

#[tokio::test]
async fn test_openai_frontend_parse_with_tools() {
    let converter = OpenAIFrontendConverter::new();

    let request_json = r#"{
        "model": "gpt-4",
        "messages": [
            {"role": "user", "content": [{"type": "text", "text": "What's the weather?"}]}
        ],
        "tools": [
            {
                "type": "function",
                "function": {
                    "name": "get_weather",
                    "description": "Get current weather",
                    "parameters": {
                        "type": "object",
                        "properties": {
                            "location": {"type": "string"}
                        }
                    }
                }
            }
        ],
        "tool_choice": "auto"
    }"#;

    let ir_request = converter.parse_request(request_json.as_bytes()).await.unwrap();

    assert_eq!(ir_request.tools.len(), 1);
    assert_eq!(ir_request.tools[0].name, "get_weather");
    assert_eq!(ir_request.tools[0].description, "Get current weather");
}

#[tokio::test]
async fn test_openai_frontend_format_simple_response() {
    let converter = OpenAIFrontendConverter::new();

    let ir_response = IRResponse {
        id: "test-123".to_string(),
        model: "gpt-4".to_string(),
        role: IRRole::Assistant,
        content: vec![
            IRContent::Text { text: "Hello! How can I help?".to_string() }
        ],
        stop_reason: Some(IRStopReason::EndTurn),
        usage: IRUsage {
            input_tokens: 10,
            output_tokens: 8,
            cache_creation_input_tokens: None,
            cache_read_input_tokens: None,
            thinking_tokens: None,
        },
        metadata: Default::default(),
    };

    let response_bytes = converter.format_response(&ir_response).await.unwrap();
    let response_json: serde_json::Value = serde_json::from_slice(&response_bytes).unwrap();

    assert_eq!(response_json["id"], "test-123");
    assert_eq!(response_json["model"], "gpt-4");
    assert_eq!(response_json["choices"][0]["message"]["content"], "Hello! How can I help?");
    assert_eq!(response_json["choices"][0]["finish_reason"], "stop");
    assert_eq!(response_json["usage"]["prompt_tokens"], 10);
    assert_eq!(response_json["usage"]["completion_tokens"], 8);
}

#[tokio::test]
async fn test_openai_frontend_format_response_with_tool_calls() {
    let converter = OpenAIFrontendConverter::new();

    let ir_response = IRResponse {
        id: "test-456".to_string(),
        model: "gpt-4".to_string(),
        role: IRRole::Assistant,
        content: vec![
            IRContent::ToolUse {
                id: "call_123".to_string(),
                name: "get_weather".to_string(),
                input: serde_json::json!({"location": "San Francisco"}),
            }
        ],
        stop_reason: Some(IRStopReason::ToolUse),
        usage: IRUsage {
            input_tokens: 15,
            output_tokens: 10,
            cache_creation_input_tokens: None,
            cache_read_input_tokens: None,
            thinking_tokens: None,
        },
        metadata: Default::default(),
    };

    let response_bytes = converter.format_response(&ir_response).await.unwrap();
    let response_json: serde_json::Value = serde_json::from_slice(&response_bytes).unwrap();

    assert_eq!(response_json["choices"][0]["finish_reason"], "tool_calls");
    assert_eq!(response_json["choices"][0]["message"]["tool_calls"][0]["id"], "call_123");
    assert_eq!(response_json["choices"][0]["message"]["tool_calls"][0]["function"]["name"], "get_weather");
}

// ============================================================================
// End-to-End: OpenAI → Gemini Conversion
// ============================================================================

#[tokio::test]
async fn test_openai_to_gemini_conversion() {
    let frontend = OpenAIFrontendConverter::new();
    let backend = GeminiBackendConverter::new();

    // OpenAI request
    let openai_request = r#"{
        "model": "gpt-4",
        "messages": [
            {"role": "user", "content": [{"type": "text", "text": "What is 2+2?"}]}
        ],
        "max_tokens": 100,
        "temperature": 0.7
    }"#;

    // Parse to IR
    let ir_request = frontend.parse_request(openai_request.as_bytes()).await.unwrap();

    assert_eq!(ir_request.model, "gpt-4");
    assert_eq!(ir_request.messages.len(), 1);
    assert_eq!(ir_request.messages[0].role, IRRole::User);

    // Convert to Gemini
    let gemini_request_bytes = backend.format_request(&ir_request).await.unwrap();
    let gemini_request: serde_json::Value = serde_json::from_slice(&gemini_request_bytes).unwrap();

    // Verify Gemini request
    assert_eq!(gemini_request["contents"][0]["role"], "user");
    assert_eq!(gemini_request["contents"][0]["parts"][0]["text"], "What is 2+2?");
    assert_eq!(gemini_request["generationConfig"]["maxOutputTokens"], 100);

    // Simulate Gemini response
    let gemini_response = r#"{
        "candidates": [
            {
                "content": {
                    "role": "model",
                    "parts": [
                        {"text": "2+2 equals 4."}
                    ]
                },
                "finishReason": "STOP"
            }
        ],
        "usageMetadata": {
            "promptTokenCount": 10,
            "candidatesTokenCount": 5,
            "totalTokenCount": 15
        }
    }"#;

    // Parse Gemini response to IR
    let ir_response = backend.parse_response(gemini_response.as_bytes()).await.unwrap();

    assert_eq!(ir_response.role, IRRole::Assistant);

    // Convert to OpenAI response
    let openai_response_bytes = frontend.format_response(&ir_response).await.unwrap();
    let openai_response: serde_json::Value = serde_json::from_slice(&openai_response_bytes).unwrap();

    // Verify OpenAI response
    assert_eq!(openai_response["choices"][0]["message"]["content"], "2+2 equals 4.");
    assert_eq!(openai_response["choices"][0]["finish_reason"], "stop");
    assert_eq!(openai_response["usage"]["prompt_tokens"], 10);
    assert_eq!(openai_response["usage"]["completion_tokens"], 5);
}

#[tokio::test]
async fn test_openai_to_gemini_with_tools() {
    let frontend = OpenAIFrontendConverter::new();
    let backend = GeminiBackendConverter::new();

    // OpenAI request with tools
    let openai_request = r#"{
        "model": "gpt-4",
        "messages": [
            {"role": "user", "content": [{"type": "text", "text": "What's the weather in Paris?"}]}
        ],
        "tools": [
            {
                "type": "function",
                "function": {
                    "name": "get_weather",
                    "description": "Get current weather",
                    "parameters": {
                        "type": "object",
                        "properties": {
                            "location": {"type": "string"}
                        }
                    }
                }
            }
        ],
        "tool_choice": "auto"
    }"#;

    // Parse to IR
    let ir_request = frontend.parse_request(openai_request.as_bytes()).await.unwrap();

    assert_eq!(ir_request.tools.len(), 1);
    assert_eq!(ir_request.tools[0].name, "get_weather");

    // Convert to Gemini
    let gemini_request_bytes = backend.format_request(&ir_request).await.unwrap();
    let gemini_request: serde_json::Value = serde_json::from_slice(&gemini_request_bytes).unwrap();

    // Verify Gemini has tools
    assert_eq!(gemini_request["tools"][0]["functionDeclarations"][0]["name"], "get_weather");
    assert_eq!(gemini_request["toolConfig"]["functionCallingConfig"]["mode"], "AUTO");
}

#[tokio::test]
async fn test_round_trip_openai_gemini_openai() {
    let frontend = OpenAIFrontendConverter::new();
    let backend = GeminiBackendConverter::new();

    // Start with OpenAI request
    let original_request = r#"{
        "model": "gpt-4",
        "messages": [
            {"role": "system", "content": "You are helpful."},
            {"role": "user", "content": [{"type": "text", "text": "Hello"}]}
        ],
        "max_tokens": 50,
        "temperature": 0.8
    }"#;

    // OpenAI → IR
    let ir_request1 = frontend.parse_request(original_request.as_bytes()).await.unwrap();

    // IR → Gemini
    let gemini_bytes = backend.format_request(&ir_request1).await.unwrap();

    // Verify temperature preserved (with f32→f64 conversion tolerance)
    let gemini_json: serde_json::Value = serde_json::from_slice(&gemini_bytes).unwrap();
    let temp = gemini_json["generationConfig"]["temperature"].as_f64().unwrap();
    assert!((temp - 0.8).abs() < 0.01);

    // Simulate Gemini response
    let gemini_response = r#"{
        "candidates": [{
            "content": {
                "role": "model",
                "parts": [{"text": "Hi there!"}]
            },
            "finishReason": "STOP"
        }],
        "usageMetadata": {
            "promptTokenCount": 8,
            "candidatesTokenCount": 3,
            "totalTokenCount": 11
        }
    }"#;

    // Gemini → IR
    let ir_response = backend.parse_response(gemini_response.as_bytes()).await.unwrap();

    // IR → OpenAI
    let openai_response_bytes = frontend.format_response(&ir_response).await.unwrap();
    let openai_response: serde_json::Value = serde_json::from_slice(&openai_response_bytes).unwrap();

    // Verify response format
    assert_eq!(openai_response["choices"][0]["message"]["content"], "Hi there!");
    assert_eq!(openai_response["usage"]["prompt_tokens"], 8);
    assert_eq!(openai_response["usage"]["completion_tokens"], 3);
}
