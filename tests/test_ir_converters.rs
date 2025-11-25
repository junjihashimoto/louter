// Unit tests for IR converters

use louter::ir::{
    AnthropicFrontendConverter, OpenAIBackendConverter,
    FrontendConverter, BackendConverter,
    IRRequest, IRResponse, IRMessage, IRContent, IRRole, IRStopReason, IRUsage,
};

#[tokio::test]
async fn test_anthropic_parse_simple_request() {
    let converter = AnthropicFrontendConverter::new();

    let request_json = r#"{
        "model": "claude-3-haiku-20240307",
        "max_tokens": 100,
        "messages": [
            {"role": "user", "content": "Hello, world!"}
        ]
    }"#;

    let ir_request = converter.parse_request(request_json.as_bytes()).await.unwrap();

    assert_eq!(ir_request.model, "claude-3-haiku-20240307");
    assert_eq!(ir_request.max_tokens, Some(100));
    assert_eq!(ir_request.messages.len(), 1);
    assert_eq!(ir_request.messages[0].role, IRRole::User);

    match &ir_request.messages[0].content[0] {
        IRContent::Text { text } => assert_eq!(text, "Hello, world!"),
        _ => panic!("Expected text content"),
    }
}

#[tokio::test]
async fn test_anthropic_parse_with_system() {
    let converter = AnthropicFrontendConverter::new();

    let request_json = r#"{
        "model": "claude-3-haiku-20240307",
        "max_tokens": 100,
        "system": "You are a helpful assistant.",
        "messages": [
            {"role": "user", "content": "Hi"}
        ]
    }"#;

    let ir_request = converter.parse_request(request_json.as_bytes()).await.unwrap();

    assert_eq!(ir_request.system, Some("You are a helpful assistant.".to_string()));
}

#[tokio::test]
async fn test_anthropic_parse_with_tools() {
    let converter = AnthropicFrontendConverter::new();

    let request_json = r#"{
        "model": "claude-3-haiku-20240307",
        "max_tokens": 100,
        "messages": [
            {"role": "user", "content": "Calculate 2+2"}
        ],
        "tools": [
            {
                "name": "calculator",
                "description": "A simple calculator",
                "input_schema": {
                    "type": "object",
                    "properties": {
                        "expression": {"type": "string"}
                    }
                }
            }
        ]
    }"#;

    let ir_request = converter.parse_request(request_json.as_bytes()).await.unwrap();

    assert_eq!(ir_request.tools.len(), 1);
    assert_eq!(ir_request.tools[0].name, "calculator");
    assert_eq!(ir_request.tools[0].description, "A simple calculator");
}

#[tokio::test]
async fn test_anthropic_format_simple_response() {
    let converter = AnthropicFrontendConverter::new();

    let ir_response = IRResponse {
        id: "msg_123".to_string(),
        model: "claude-3-haiku-20240307".to_string(),
        role: IRRole::Assistant,
        content: vec![
            IRContent::Text { text: "Hello! How can I help you?".to_string() }
        ],
        stop_reason: Some(IRStopReason::EndTurn),
        usage: IRUsage {
            input_tokens: 10,
            output_tokens: 20,
            cache_creation_input_tokens: None,
            cache_read_input_tokens: None,
            thinking_tokens: None,
        },
        metadata: Default::default(),
    };

    let response_bytes = converter.format_response(&ir_response).await.unwrap();
    let response_json: serde_json::Value = serde_json::from_slice(&response_bytes).unwrap();

    assert_eq!(response_json["id"], "msg_123");
    assert_eq!(response_json["type"], "message");
    assert_eq!(response_json["role"], "assistant");
    assert_eq!(response_json["model"], "claude-3-haiku-20240307");
    assert_eq!(response_json["stop_reason"], "end_turn");
    assert_eq!(response_json["usage"]["input_tokens"], 10);
    assert_eq!(response_json["usage"]["output_tokens"], 20);
    assert_eq!(response_json["content"][0]["type"], "text");
    assert_eq!(response_json["content"][0]["text"], "Hello! How can I help you?");
}

#[tokio::test]
async fn test_openai_format_simple_request() {
    let converter = OpenAIBackendConverter::new();

    let ir_request = IRRequest {
        model: "gpt-4".to_string(),
        messages: vec![
            IRMessage {
                role: IRRole::User,
                content: vec![
                    IRContent::Text { text: "Hello, GPT!".to_string() }
                ],
                name: None,
            }
        ],
        system: None,
        max_tokens: Some(50),
        temperature: Some(0.7),
        top_p: Some(0.9),
        top_k: None,
        stop_sequences: vec![],
        tools: vec![],
        tool_choice: None,
        stream: false,
        metadata: Default::default(),
    };

    let request_bytes = converter.format_request(&ir_request).await.unwrap();
    let request_json: serde_json::Value = serde_json::from_slice(&request_bytes).unwrap();

    assert_eq!(request_json["model"], "gpt-4");
    assert_eq!(request_json["max_tokens"], 50);
    // Temperature is f32 -> f64 conversion, so use approximate comparison
    assert!((request_json["temperature"].as_f64().unwrap() - 0.7).abs() < 0.01);
    assert!((request_json["top_p"].as_f64().unwrap() - 0.9).abs() < 0.01);
    assert_eq!(request_json["stream"], false);
    assert_eq!(request_json["messages"][0]["role"], "user");
    assert_eq!(request_json["messages"][0]["content"], "Hello, GPT!");
}

#[tokio::test]
async fn test_openai_format_request_with_system() {
    let converter = OpenAIBackendConverter::new();

    let ir_request = IRRequest {
        model: "gpt-4".to_string(),
        messages: vec![
            IRMessage {
                role: IRRole::User,
                content: vec![
                    IRContent::Text { text: "Hi".to_string() }
                ],
                name: None,
            }
        ],
        system: Some("You are helpful.".to_string()),
        max_tokens: Some(50),
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

    assert_eq!(request_json["messages"].as_array().unwrap().len(), 2);
    assert_eq!(request_json["messages"][0]["role"], "system");
    assert_eq!(request_json["messages"][0]["content"], "You are helpful.");
    assert_eq!(request_json["messages"][1]["role"], "user");
}

#[tokio::test]
async fn test_openai_parse_simple_response() {
    let converter = OpenAIBackendConverter::new();

    let response_json = r#"{
        "id": "chatcmpl-123",
        "object": "chat.completion",
        "created": 1677652288,
        "model": "gpt-4",
        "choices": [
            {
                "index": 0,
                "message": {
                    "role": "assistant",
                    "content": "Hello! I'm here to help."
                },
                "finish_reason": "stop"
            }
        ],
        "usage": {
            "prompt_tokens": 10,
            "completion_tokens": 15,
            "total_tokens": 25
        }
    }"#;

    let ir_response = converter.parse_response(response_json.as_bytes()).await.unwrap();

    assert_eq!(ir_response.id, "chatcmpl-123");
    assert_eq!(ir_response.model, "gpt-4");
    assert_eq!(ir_response.role, IRRole::Assistant);
    assert_eq!(ir_response.stop_reason, Some(IRStopReason::EndTurn));
    assert_eq!(ir_response.usage.input_tokens, 10);
    assert_eq!(ir_response.usage.output_tokens, 15);

    match &ir_response.content[0] {
        IRContent::Text { text } => assert_eq!(text, "Hello! I'm here to help."),
        _ => panic!("Expected text content"),
    }
}

#[tokio::test]
async fn test_openai_parse_response_with_tool_calls() {
    let converter = OpenAIBackendConverter::new();

    let response_json = r#"{
        "id": "chatcmpl-123",
        "object": "chat.completion",
        "created": 1677652288,
        "model": "gpt-4",
        "choices": [
            {
                "index": 0,
                "message": {
                    "role": "assistant",
                    "content": "Let me calculate that.",
                    "tool_calls": [
                        {
                            "id": "call_abc123",
                            "type": "function",
                            "function": {
                                "name": "calculator",
                                "arguments": "{\"expression\": \"2+2\"}"
                            }
                        }
                    ]
                },
                "finish_reason": "tool_calls"
            }
        ],
        "usage": {
            "prompt_tokens": 10,
            "completion_tokens": 15,
            "total_tokens": 25
        }
    }"#;

    let ir_response = converter.parse_response(response_json.as_bytes()).await.unwrap();

    assert_eq!(ir_response.content.len(), 2);
    assert_eq!(ir_response.stop_reason, Some(IRStopReason::ToolUse));

    match &ir_response.content[0] {
        IRContent::Text { text } => assert_eq!(text, "Let me calculate that."),
        _ => panic!("Expected text content"),
    }

    match &ir_response.content[1] {
        IRContent::ToolUse { id, name, input } => {
            assert_eq!(id, "call_abc123");
            assert_eq!(name, "calculator");
            assert_eq!(input["expression"], "2+2");
        },
        _ => panic!("Expected tool use content"),
    }
}

#[tokio::test]
async fn test_end_to_end_conversion() {
    let frontend = AnthropicFrontendConverter::new();
    let backend = OpenAIBackendConverter::new();

    // Anthropic request
    let anthropic_request = r#"{
        "model": "claude-3-haiku-20240307",
        "max_tokens": 100,
        "messages": [
            {"role": "user", "content": "What is 2+2?"}
        ]
    }"#;

    // Parse to IR
    let ir_request = frontend.parse_request(anthropic_request.as_bytes()).await.unwrap();

    // Convert to OpenAI
    let openai_request_bytes = backend.format_request(&ir_request).await.unwrap();
    let openai_request: serde_json::Value = serde_json::from_slice(&openai_request_bytes).unwrap();

    // Verify OpenAI request
    assert_eq!(openai_request["model"], "claude-3-haiku-20240307");
    assert_eq!(openai_request["max_tokens"], 100);
    assert_eq!(openai_request["messages"][0]["content"], "What is 2+2?");

    // Simulate OpenAI response
    let openai_response = r#"{
        "id": "chatcmpl-123",
        "object": "chat.completion",
        "created": 1677652288,
        "model": "claude-3-haiku-20240307",
        "choices": [
            {
                "index": 0,
                "message": {
                    "role": "assistant",
                    "content": "2+2 equals 4."
                },
                "finish_reason": "stop"
            }
        ],
        "usage": {
            "prompt_tokens": 10,
            "completion_tokens": 8,
            "total_tokens": 18
        }
    }"#;

    // Parse OpenAI response to IR
    let ir_response = backend.parse_response(openai_response.as_bytes()).await.unwrap();

    // Convert to Anthropic response
    let anthropic_response_bytes = frontend.format_response(&ir_response).await.unwrap();
    let anthropic_response: serde_json::Value = serde_json::from_slice(&anthropic_response_bytes).unwrap();

    // Verify Anthropic response
    assert_eq!(anthropic_response["id"], "chatcmpl-123");
    assert_eq!(anthropic_response["type"], "message");
    assert_eq!(anthropic_response["model"], "claude-3-haiku-20240307");
    assert_eq!(anthropic_response["content"][0]["text"], "2+2 equals 4.");
    assert_eq!(anthropic_response["stop_reason"], "end_turn");
    assert_eq!(anthropic_response["usage"]["input_tokens"], 10);
    assert_eq!(anthropic_response["usage"]["output_tokens"], 8);
}

#[tokio::test]
async fn test_round_trip_preservation() {
    let frontend = AnthropicFrontendConverter::new();

    // Original Anthropic request
    let original_request = r#"{
        "model": "claude-3-haiku-20240307",
        "max_tokens": 100,
        "temperature": 0.7,
        "system": "Be helpful.",
        "messages": [
            {"role": "user", "content": "Hello"}
        ]
    }"#;

    // Parse to IR
    let ir_request = frontend.parse_request(original_request.as_bytes()).await.unwrap();

    // Verify all fields preserved
    assert_eq!(ir_request.model, "claude-3-haiku-20240307");
    assert_eq!(ir_request.max_tokens, Some(100));
    assert_eq!(ir_request.temperature, Some(0.7));
    assert_eq!(ir_request.system, Some("Be helpful.".to_string()));
    assert_eq!(ir_request.messages.len(), 1);
}
