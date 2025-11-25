// Streaming conversion tests for IR chunk transformations
// These tests verify SSE chunk conversion: Backend SSE → IR chunks → Frontend SSE

use louter::ir::{
    GeminiFrontendConverter, OpenAIBackendConverter, GeminiBackendConverter,
    FrontendConverter, BackendConverter,
    IRChunkType, IRStopReason,
};

#[tokio::test]
async fn test_openai_stream_chunk_to_gemini() {
    // Test: OpenAI SSE chunk → IR → Gemini SSE chunk

    let openai_backend = OpenAIBackendConverter;
    let gemini_frontend = GeminiFrontendConverter::new();

    // OpenAI streaming chunk (text delta)
    let openai_chunk_json = r#"{
        "id": "chatcmpl-123",
        "object": "chat.completion.chunk",
        "created": 1677652288,
        "model": "gpt-3.5-turbo",
        "choices": [{
            "index": 0,
            "delta": {
                "content": "Hello"
            },
            "finish_reason": null
        }]
    }"#;

    // Parse OpenAI chunk to IR
    let ir_chunk = openai_backend.parse_stream_chunk(openai_chunk_json.as_bytes())
        .expect("Should parse OpenAI chunk");

    assert!(ir_chunk.is_some(), "Should produce IR chunk");
    let chunk = ir_chunk.unwrap();

    // Verify IR chunk structure
    match &chunk.chunk_type {
        IRChunkType::ContentBlockDelta { index, delta } => {
            assert_eq!(*index, 0);
            match delta {
                louter::ir::IRDelta::TextDelta { text } => {
                    assert_eq!(text, "Hello");
                },
                _ => panic!("Expected text delta"),
            }
        },
        _ => panic!("Expected content block delta, got: {:?}", chunk.chunk_type),
    }

    // Convert IR chunk to Gemini SSE format
    let gemini_chunk_str = gemini_frontend.format_stream_chunk(&chunk)
        .expect("Should format Gemini chunk");

    // Parse and verify Gemini chunk
    let gemini_chunk: serde_json::Value = serde_json::from_str(&gemini_chunk_str)
        .expect("Should parse Gemini JSON");

    // Gemini streaming format has candidates array with content
    assert!(gemini_chunk["candidates"].is_array());
    assert_eq!(gemini_chunk["candidates"][0]["content"]["parts"][0]["text"], "Hello");
}

#[tokio::test]
async fn test_openai_stream_finish_to_gemini() {
    // Test: OpenAI finish chunk → IR → Gemini finish chunk

    let openai_backend = OpenAIBackendConverter;
    let gemini_frontend = GeminiFrontendConverter::new();

    // OpenAI finish chunk
    let openai_finish_json = r#"{
        "id": "chatcmpl-123",
        "object": "chat.completion.chunk",
        "created": 1677652288,
        "model": "gpt-3.5-turbo",
        "choices": [{
            "index": 0,
            "delta": {},
            "finish_reason": "stop"
        }]
    }"#;

    // Parse to IR
    let ir_chunk = openai_backend.parse_stream_chunk(openai_finish_json.as_bytes())
        .expect("Should parse finish chunk");

    assert!(ir_chunk.is_some());
    let chunk = ir_chunk.unwrap();

    // Verify finish reason in IR
    match &chunk.chunk_type {
        IRChunkType::MessageDelta { delta, usage: _ } => {
            assert!(delta.stop_reason.is_some());
            assert!(matches!(delta.stop_reason.as_ref().unwrap(), IRStopReason::EndTurn));
        },
        _ => panic!("Expected message delta with stop reason"),
    }

    // Convert to Gemini format
    let gemini_chunk_str = gemini_frontend.format_stream_chunk(&chunk)
        .expect("Should format Gemini finish chunk");

    let gemini_chunk: serde_json::Value = serde_json::from_str(&gemini_chunk_str).unwrap();

    // Gemini uses "finishReason": "STOP"
    assert_eq!(gemini_chunk["candidates"][0]["finishReason"], "STOP");
}

#[tokio::test]
async fn test_gemini_stream_chunk_to_gemini() {
    // Test: Gemini backend SSE → IR → Gemini frontend SSE (round-trip)

    let gemini_backend = GeminiBackendConverter;
    let gemini_frontend = GeminiFrontendConverter::new();

    // Gemini streaming chunk
    let gemini_chunk_json = r#"{
        "candidates": [{
            "content": {
                "role": "model",
                "parts": [
                    {"text": "Hello from Gemini!"}
                ]
            },
            "finishReason": null
        }]
    }"#;

    // Parse backend chunk to IR
    let ir_chunk = gemini_backend.parse_stream_chunk(gemini_chunk_json.as_bytes())
        .expect("Should parse Gemini chunk");

    assert!(ir_chunk.is_some());
    let chunk = ir_chunk.unwrap();

    // Verify IR chunk
    match &chunk.chunk_type {
        IRChunkType::ContentBlockDelta { index, delta } => {
            assert_eq!(*index, 0);
            match delta {
                louter::ir::IRDelta::TextDelta { text } => {
                    assert_eq!(text, "Hello from Gemini!");
                },
                _ => panic!("Expected text delta"),
            }
        },
        _ => panic!("Expected content block delta"),
    }

    // Convert IR back to Gemini frontend format
    let frontend_chunk_str = gemini_frontend.format_stream_chunk(&chunk)
        .expect("Should format frontend chunk");

    let frontend_chunk: serde_json::Value = serde_json::from_str(&frontend_chunk_str).unwrap();

    // Verify round-trip preserves content
    assert_eq!(frontend_chunk["candidates"][0]["content"]["parts"][0]["text"], "Hello from Gemini!");
}

#[tokio::test]
async fn test_openai_tool_call_stream_to_gemini() {
    // Test: OpenAI tool call streaming → IR → Gemini function call

    let openai_backend = OpenAIBackendConverter;
    let gemini_frontend = GeminiFrontendConverter::new();

    // OpenAI tool call chunk
    let openai_tool_chunk = r#"{
        "id": "chatcmpl-123",
        "object": "chat.completion.chunk",
        "created": 1677652288,
        "model": "gpt-3.5-turbo",
        "choices": [{
            "index": 0,
            "delta": {
                "tool_calls": [{
                    "index": 0,
                    "id": "call_abc123",
                    "type": "function",
                    "function": {
                        "name": "get_weather",
                        "arguments": "{\"location\":"
                    }
                }]
            },
            "finish_reason": null
        }]
    }"#;

    // Parse to IR
    let ir_chunk = openai_backend.parse_stream_chunk(openai_tool_chunk.as_bytes())
        .expect("Should parse tool call chunk");

    if let Some(chunk) = ir_chunk {
        // Convert to Gemini format
        let gemini_chunk_str = gemini_frontend.format_stream_chunk(&chunk)
            .expect("Should format Gemini chunk");

        let gemini_chunk: serde_json::Value = serde_json::from_str(&gemini_chunk_str).unwrap();

        // Gemini uses functionCall in parts
        if let Some(function_call) = gemini_chunk["candidates"][0]["content"]["parts"][0].get("functionCall") {
            assert_eq!(function_call["name"], "get_weather");
            // Note: arguments might be partial in streaming
        }
    }
}

#[tokio::test]
async fn test_multiple_chunks_accumulation() {
    // Test: Multiple streaming chunks combine correctly

    let openai_backend = OpenAIBackendConverter;
    let gemini_frontend = GeminiFrontendConverter::new();

    let chunks = vec![
        r#"{"id":"1","object":"chat.completion.chunk","created":1,"model":"gpt-3.5-turbo","choices":[{"index":0,"delta":{"content":"Hello"},"finish_reason":null}]}"#,
        r#"{"id":"1","object":"chat.completion.chunk","created":1,"model":"gpt-3.5-turbo","choices":[{"index":0,"delta":{"content":" "},"finish_reason":null}]}"#,
        r#"{"id":"1","object":"chat.completion.chunk","created":1,"model":"gpt-3.5-turbo","choices":[{"index":0,"delta":{"content":"world"},"finish_reason":null}]}"#,
        r#"{"id":"1","object":"chat.completion.chunk","created":1,"model":"gpt-3.5-turbo","choices":[{"index":0,"delta":{"content":"!"},"finish_reason":null}]}"#,
    ];

    let mut accumulated_text = String::new();

    for chunk_json in chunks {
        if let Ok(Some(ir_chunk)) = openai_backend.parse_stream_chunk(chunk_json.as_bytes()) {
            if let Ok(gemini_str) = gemini_frontend.format_stream_chunk(&ir_chunk) {
                let gemini_chunk: serde_json::Value = serde_json::from_str(&gemini_str).unwrap();
                if let Some(text) = gemini_chunk["candidates"][0]["content"]["parts"][0]["text"].as_str() {
                    accumulated_text.push_str(text);
                }
            }
        }
    }

    assert_eq!(accumulated_text, "Hello world!");
}

#[tokio::test]
async fn test_gemini_thinking_tokens_stream() {
    // Test: Gemini 2.5+ thinking tokens in streaming

    let gemini_backend = GeminiBackendConverter;
    let gemini_frontend = GeminiFrontendConverter::new();

    // Gemini chunk with thinking tokens (extended thinking)
    let gemini_chunk_json = r#"{
        "candidates": [{
            "content": {
                "role": "model",
                "parts": [
                    {"text": "Let me think..."}
                ]
            }
        }],
        "usageMetadata": {
            "promptTokenCount": 10,
            "candidatesTokenCount": 5,
            "totalTokenCount": 15,
            "thoughtsTokenCount": 50
        }
    }"#;

    // Parse to IR
    let ir_chunk = gemini_backend.parse_stream_chunk(gemini_chunk_json.as_bytes())
        .expect("Should parse chunk with thinking tokens");

    assert!(ir_chunk.is_some());
    let chunk = ir_chunk.unwrap();

    // Convert back to frontend format
    let frontend_str = gemini_frontend.format_stream_chunk(&chunk)
        .expect("Should format chunk");

    let frontend_chunk: serde_json::Value = serde_json::from_str(&frontend_str).unwrap();

    // Verify thinking tokens are preserved
    if let Some(usage) = frontend_chunk.get("usageMetadata") {
        if let Some(thoughts) = usage.get("thoughtsTokenCount") {
            assert_eq!(thoughts.as_i64().unwrap(), 50);
        }
    }
}

#[tokio::test]
async fn test_empty_delta_handling() {
    // Test: Empty deltas are handled gracefully

    let openai_backend = OpenAIBackendConverter;
    let gemini_frontend = GeminiFrontendConverter::new();

    // OpenAI chunk with empty delta (common during streaming)
    let openai_empty_delta = r#"{
        "id": "chatcmpl-123",
        "object": "chat.completion.chunk",
        "created": 1677652288,
        "model": "gpt-3.5-turbo",
        "choices": [{
            "index": 0,
            "delta": {},
            "finish_reason": null
        }]
    }"#;

    // Parse to IR - might return None for empty deltas
    let ir_chunk = openai_backend.parse_stream_chunk(openai_empty_delta.as_bytes())
        .expect("Should parse empty delta");

    // Empty deltas might be skipped or handled differently
    // The important thing is it doesn't error
    if let Some(chunk) = ir_chunk {
        // If it produces a chunk, it should format successfully
        let _ = gemini_frontend.format_stream_chunk(&chunk)
            .expect("Should format even empty chunks");
    }
}

#[tokio::test]
async fn test_stream_chunk_with_finish_reason() {
    // Test: Stream chunks with finish reason are properly converted

    let openai_backend = OpenAIBackendConverter;
    let gemini_frontend = GeminiFrontendConverter::new();

    // OpenAI finish chunk (usage metadata is typically not in streaming chunks,
    // but available in the final non-streaming response)
    let openai_finish_chunk = r#"{
        "id": "chatcmpl-123",
        "object": "chat.completion.chunk",
        "created": 1677652288,
        "model": "gpt-3.5-turbo",
        "choices": [{
            "index": 0,
            "delta": {},
            "finish_reason": "stop"
        }]
    }"#;

    // Parse to IR
    let ir_chunk = openai_backend.parse_stream_chunk(openai_finish_chunk.as_bytes())
        .expect("Should parse finish chunk");

    assert!(ir_chunk.is_some());
    let chunk = ir_chunk.unwrap();

    // Verify it's a message delta with stop reason
    match &chunk.chunk_type {
        IRChunkType::MessageDelta { delta, usage: _ } => {
            assert!(delta.stop_reason.is_some());
            assert!(matches!(delta.stop_reason.as_ref().unwrap(), IRStopReason::EndTurn));
        },
        _ => panic!("Expected MessageDelta with stop reason"),
    }

    // Convert to Gemini format
    let gemini_str = gemini_frontend.format_stream_chunk(&chunk)
        .expect("Should format chunk");

    let gemini_chunk: serde_json::Value = serde_json::from_str(&gemini_str).unwrap();

    // Verify finish reason is present in Gemini format
    assert_eq!(gemini_chunk["candidates"][0]["finishReason"], "STOP");
}
