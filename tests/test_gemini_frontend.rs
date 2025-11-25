// Unit tests for GeminiFrontendConverter

use louter::ir::{
    GeminiFrontendConverter, FrontendConverter,
    IRRequest, IRResponse, IRRole, IRStopReason, IRUsage, IRContent, IRTool,
};

#[tokio::test]
async fn test_gemini_frontend_parse_simple_request() {
    let converter = GeminiFrontendConverter::new();

    let request_json = r#"{
        "contents": [
            {
                "role": "user",
                "parts": [
                    {"text": "Hello, Gemini!"}
                ]
            }
        ],
        "generationConfig": {
            "maxOutputTokens": 100,
            "temperature": 0.7
        }
    }"#;

    let ir_request = converter.parse_request(request_json.as_bytes()).await.unwrap();

    assert_eq!(ir_request.messages.len(), 1);
    assert_eq!(ir_request.messages[0].role, IRRole::User);
    assert_eq!(ir_request.max_tokens, Some(100));
    assert!((ir_request.temperature.unwrap() - 0.7).abs() < 0.01);

    match &ir_request.messages[0].content[0] {
        IRContent::Text { text } => assert_eq!(text, "Hello, Gemini!"),
        _ => panic!("Expected text content"),
    }
}

#[tokio::test]
async fn test_gemini_frontend_parse_with_system() {
    let converter = GeminiFrontendConverter::new();

    let request_json = r#"{
        "contents": [
            {
                "role": "user",
                "parts": [
                    {"text": "Hi"}
                ]
            }
        ],
        "systemInstruction": {
            "parts": [
                {"text": "You are a helpful assistant."}
            ]
        }
    }"#;

    let ir_request = converter.parse_request(request_json.as_bytes()).await.unwrap();

    assert_eq!(ir_request.system, Some("You are a helpful assistant.".to_string()));
    assert_eq!(ir_request.messages.len(), 1);
}

#[tokio::test]
async fn test_gemini_frontend_parse_with_tools() {
    let converter = GeminiFrontendConverter::new();

    let request_json = r#"{
        "contents": [
            {
                "role": "user",
                "parts": [
                    {"text": "What's the weather?"}
                ]
            }
        ],
        "tools": [
            {
                "functionDeclarations": [
                    {
                        "name": "get_weather",
                        "description": "Get current weather",
                        "parametersJsonSchema": {
                            "type": "object",
                            "properties": {
                                "location": {"type": "string"}
                            }
                        }
                    }
                ]
            }
        ],
        "toolConfig": {
            "functionCallingConfig": {
                "mode": "AUTO"
            }
        }
    }"#;

    let ir_request = converter.parse_request(request_json.as_bytes()).await.unwrap();

    assert_eq!(ir_request.tools.len(), 1);
    assert_eq!(ir_request.tools[0].name, "get_weather");
    assert_eq!(ir_request.tools[0].description, "Get current weather");
}

#[tokio::test]
async fn test_gemini_frontend_parse_multimodal() {
    let converter = GeminiFrontendConverter::new();

    let request_json = r#"{
        "contents": [
            {
                "role": "user",
                "parts": [
                    {"text": "What's in this image?"},
                    {
                        "inlineData": {
                            "mimeType": "image/jpeg",
                            "data": "base64encodeddata"
                        }
                    }
                ]
            }
        ]
    }"#;

    let ir_request = converter.parse_request(request_json.as_bytes()).await.unwrap();

    assert_eq!(ir_request.messages[0].content.len(), 2);

    match &ir_request.messages[0].content[0] {
        IRContent::Text { text } => assert_eq!(text, "What's in this image?"),
        _ => panic!("Expected text content"),
    }

    match &ir_request.messages[0].content[1] {
        IRContent::Image { source, .. } => {
            match source {
                louter::ir::IRImageSource::Base64 { media_type, data } => {
                    assert_eq!(media_type, "image/jpeg");
                    assert_eq!(data, "base64encodeddata");
                },
                _ => panic!("Expected base64 image"),
            }
        },
        _ => panic!("Expected image content"),
    }
}

#[tokio::test]
async fn test_gemini_frontend_format_simple_response() {
    let converter = GeminiFrontendConverter::new();

    let ir_response = IRResponse {
        id: "gemini-123".to_string(),
        model: "gemini-2.0-flash".to_string(),
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

    assert_eq!(response_json["candidates"][0]["content"]["parts"][0]["text"], "Hello! How can I help?");
    assert_eq!(response_json["candidates"][0]["finishReason"], "STOP");
    assert_eq!(response_json["usageMetadata"]["promptTokenCount"], 10);
    assert_eq!(response_json["usageMetadata"]["candidatesTokenCount"], 8);
}

#[tokio::test]
async fn test_gemini_frontend_format_with_thinking_tokens() {
    let converter = GeminiFrontendConverter::new();

    let ir_response = IRResponse {
        id: "gemini-456".to_string(),
        model: "gemini-2.5-flash".to_string(),
        role: IRRole::Assistant,
        content: vec![
            IRContent::Text { text: "The answer is 4.".to_string() }
        ],
        stop_reason: Some(IRStopReason::EndTurn),
        usage: IRUsage {
            input_tokens: 10,
            output_tokens: 5,
            cache_creation_input_tokens: None,
            cache_read_input_tokens: None,
            thinking_tokens: Some(15),
        },
        metadata: Default::default(),
    };

    let response_bytes = converter.format_response(&ir_response).await.unwrap();
    let response_json: serde_json::Value = serde_json::from_slice(&response_bytes).unwrap();

    assert_eq!(response_json["usageMetadata"]["thoughtsTokenCount"], 15);
}

#[tokio::test]
async fn test_gemini_frontend_format_with_function_call() {
    let converter = GeminiFrontendConverter::new();

    let ir_response = IRResponse {
        id: "gemini-789".to_string(),
        model: "gemini-2.0-flash".to_string(),
        role: IRRole::Assistant,
        content: vec![
            IRContent::ToolUse {
                id: "call_123".to_string(),
                name: "get_weather".to_string(),
                input: serde_json::json!({"location": "Tokyo"}),
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

    assert_eq!(response_json["candidates"][0]["content"]["parts"][0]["functionCall"]["name"], "get_weather");
    assert_eq!(response_json["candidates"][0]["content"]["parts"][0]["functionCall"]["args"]["location"], "Tokyo");
}

// End-to-end test: Gemini → IR → Gemini round-trip
#[tokio::test]
async fn test_gemini_round_trip() {
    let converter = GeminiFrontendConverter::new();

    let original_request = r#"{
        "contents": [
            {
                "role": "user",
                "parts": [
                    {"text": "What is 2+2?"}
                ]
            }
        ],
        "generationConfig": {
            "maxOutputTokens": 100,
            "temperature": 0.8
        }
    }"#;

    // Gemini → IR
    let ir_request = converter.parse_request(original_request.as_bytes()).await.unwrap();

    assert_eq!(ir_request.messages.len(), 1);
    assert_eq!(ir_request.max_tokens, Some(100));
    assert!((ir_request.temperature.unwrap() - 0.8).abs() < 0.01);

    // Simulate IR response
    let ir_response = IRResponse {
        id: "test-123".to_string(),
        model: "gemini-2.0-flash".to_string(),
        role: IRRole::Assistant,
        content: vec![
            IRContent::Text { text: "2+2 equals 4.".to_string() }
        ],
        stop_reason: Some(IRStopReason::EndTurn),
        usage: IRUsage {
            input_tokens: 8,
            output_tokens: 6,
            cache_creation_input_tokens: None,
            cache_read_input_tokens: None,
            thinking_tokens: None,
        },
        metadata: Default::default(),
    };

    // IR → Gemini
    let response_bytes = converter.format_response(&ir_response).await.unwrap();
    let response_json: serde_json::Value = serde_json::from_slice(&response_bytes).unwrap();

    // Verify response format
    assert_eq!(response_json["candidates"][0]["content"]["parts"][0]["text"], "2+2 equals 4.");
    assert_eq!(response_json["candidates"][0]["finishReason"], "STOP");
    assert_eq!(response_json["usageMetadata"]["promptTokenCount"], 8);
    assert_eq!(response_json["usageMetadata"]["candidatesTokenCount"], 6);
}
