use crate::models::{gemini::*, openai::*, common::*};
use serde_json::json;
use std::time::Duration;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sse_parsing() {
        let sse_data = "data: {\"candidates\":[{\"content\":{\"role\":\"model\",\"parts\":[{\"text\":\"Hello\"}]}}]}\n\n";
        
        let data_line = sse_data.strip_prefix("data: ").unwrap().trim();
        let parsed: GeminiStreamResponse = serde_json::from_str(data_line).unwrap();
        
        assert_eq!(parsed.candidates.len(), 1);
        match &parsed.candidates[0].content.parts[0] {
            Part::Text { text } => assert_eq!(text, "Hello"),
            _ => panic!("Expected text part"),
        }
    }

    #[test]
    fn test_openai_sse_format() {
        let sse_data = "data: {\"id\":\"chatcmpl-123\",\"object\":\"chat.completion.chunk\",\"created\":1677858242,\"model\":\"gpt-4\",\"choices\":[{\"index\":0,\"delta\":{\"content\":\"Hello\"},\"finish_reason\":null}]}\n\n";
        
        let data_line = sse_data.strip_prefix("data: ").unwrap().trim();
        let parsed: OpenAIStreamResponse = serde_json::from_str(data_line).unwrap();
        
        assert_eq!(parsed.id, "chatcmpl-123");
        assert_eq!(parsed.choices[0].delta.content.as_ref().unwrap(), "Hello");
    }

    #[test]
    fn test_sse_done_signal() {
        let sse_done = "data: [DONE]\n\n";
        let data_line = sse_done.strip_prefix("data: ").unwrap().trim();
        assert_eq!(data_line, "[DONE]");
    }

    #[test]
    fn test_performance_metrics_ttft() {
        let mut metrics = PerformanceMetrics::new();
        
        std::thread::sleep(Duration::from_millis(50));
        metrics.record_first_token();
        
        assert!(metrics.first_token_time.is_some());
        assert!(metrics.ttft_ms.is_some());
        assert!(metrics.ttft_ms.unwrap() >= 50);
        assert_eq!(metrics.token_count, 0);
    }

    #[test]
    fn test_performance_metrics_tps() {
        let mut metrics = PerformanceMetrics::new();
        
        metrics.record_token();
        std::thread::sleep(Duration::from_millis(100));
        
        for _ in 0..9 {
            metrics.record_token();
        }
        
        metrics.finalize();
        
        assert_eq!(metrics.token_count, 10);
        assert!(metrics.tokens_per_second.is_some());
        let tps = metrics.tokens_per_second.unwrap();
        assert!(tps > 80.0 && tps < 120.0, "TPS should be around 100, got {}", tps);
    }

    #[test]
    fn test_streaming_gemini_chunks() {
        let chunks = vec![
            json!({
                "candidates": [{
                    "content": {
                        "role": "model",
                        "parts": [{"text": "The "}]
                    }
                }]
            }),
            json!({
                "candidates": [{
                    "content": {
                        "role": "model",
                        "parts": [{"text": "weather "}]
                    }
                }]
            }),
            json!({
                "candidates": [{
                    "content": {
                        "role": "model",
                        "parts": [{"text": "is nice"}]
                    },
                    "finishReason": "STOP"
                }],
                "usageMetadata": {
                    "promptTokenCount": 5,
                    "candidatesTokenCount": 4,
                    "totalTokenCount": 9
                }
            }),
        ];

        let mut full_text = String::new();
        let mut final_usage = None;

        for chunk_json in chunks {
            let chunk: GeminiStreamResponse = serde_json::from_value(chunk_json).unwrap();
            
            if let Some(candidate) = chunk.candidates.first() {
                if let Part::Text { text } = &candidate.content.parts[0] {
                    full_text.push_str(text);
                }
            }
            
            if let Some(usage) = chunk.usage_metadata {
                final_usage = Some(usage);
            }
        }

        assert_eq!(full_text, "The weather is nice");
        assert!(final_usage.is_some());
        assert_eq!(final_usage.unwrap().total_token_count, 9);
    }

    #[test]
    fn test_streaming_openai_chunks() {
        let chunks = vec![
            json!({
                "id": "chatcmpl-123",
                "object": "chat.completion.chunk",
                "created": 1677858242,
                "model": "gpt-4",
                "choices": [{
                    "index": 0,
                    "delta": {"role": "assistant"},
                    "finish_reason": null
                }]
            }),
            json!({
                "id": "chatcmpl-123",
                "object": "chat.completion.chunk",
                "created": 1677858242,
                "model": "gpt-4",
                "choices": [{
                    "index": 0,
                    "delta": {"content": "Hello"},
                    "finish_reason": null
                }]
            }),
            json!({
                "id": "chatcmpl-123",
                "object": "chat.completion.chunk",
                "created": 1677858242,
                "model": "gpt-4",
                "choices": [{
                    "index": 0,
                    "delta": {"content": " world"},
                    "finish_reason": null
                }]
            }),
            json!({
                "id": "chatcmpl-123",
                "object": "chat.completion.chunk",
                "created": 1677858242,
                "model": "gpt-4",
                "choices": [{
                    "index": 0,
                    "delta": {},
                    "finish_reason": "stop"
                }]
            }),
        ];

        let mut full_text = String::new();
        let mut role = None;

        for chunk_json in chunks {
            let chunk: OpenAIStreamResponse = serde_json::from_value(chunk_json).unwrap();
            
            if let Some(r) = &chunk.choices[0].delta.role {
                role = Some(r.clone());
            }
            
            if let Some(content) = &chunk.choices[0].delta.content {
                full_text.push_str(content);
            }
        }

        assert_eq!(role.unwrap(), "assistant");
        assert_eq!(full_text, "Hello world");
    }

    #[test]
    fn test_streaming_function_call() {
        let chunks = vec![
            json!({
                "id": "chatcmpl-123",
                "object": "chat.completion.chunk",
                "created": 1677858242,
                "model": "gpt-4",
                "choices": [{
                    "index": 0,
                    "delta": {
                        "tool_calls": [{
                            "id": "call_123",
                            "type": "function",
                            "function": {
                                "name": "get_weather",
                                "arguments": ""
                            }
                        }]
                    },
                    "finish_reason": null
                }]
            }),
            json!({
                "id": "chatcmpl-123",
                "object": "chat.completion.chunk",
                "created": 1677858242,
                "model": "gpt-4",
                "choices": [{
                    "index": 0,
                    "delta": {
                        "tool_calls": [{
                            "function": {
                                "arguments": "{\"location\""
                            }
                        }]
                    },
                    "finish_reason": null
                }]
            }),
            json!({
                "id": "chatcmpl-123",
                "object": "chat.completion.chunk",
                "created": 1677858242,
                "model": "gpt-4",
                "choices": [{
                    "index": 0,
                    "delta": {
                        "tool_calls": [{
                            "function": {
                                "arguments": ": \"Boston\"}"
                            }
                        }]
                    },
                    "finish_reason": "tool_calls"
                }]
            }),
        ];

        let mut function_name = String::new();
        let mut function_args = String::new();

        for chunk_json in chunks {
            let chunk: OpenAIStreamResponse = serde_json::from_value(chunk_json).unwrap();
            
            if let Some(tool_calls) = &chunk.choices[0].delta.tool_calls {
                if let Some(call) = tool_calls.first() {
                    if let Some(name) = &call.function.name {
                        if !name.is_empty() {
                            function_name = name.clone();
                        }
                    }
                    function_args.push_str(&call.function.arguments);
                }
            }
        }

        assert_eq!(function_name, "get_weather");
        assert_eq!(function_args, "{\"location\": \"Boston\"}");
    }

    #[test]
    fn test_multipart_sse_buffer() {
        let partial_sse = "data: {\"candidates\":[{\"content\":{\"role\":\"model\",";
        let remaining = "\"parts\":[{\"text\":\"Hello\"}]}}]}\n\ndata: {\"candidates\":[{\"content\":{\"role\":\"model\",\"parts\":[{\"text\":\" world\"}]}}]}\n\n";
        
        let full_buffer = format!("{}{}", partial_sse, remaining);
        let events: Vec<&str> = full_buffer
            .split("\n\n")
            .filter(|s| !s.is_empty())
            .collect();
        
        assert_eq!(events.len(), 2);
        
        for event in events {
            if let Some(data) = event.strip_prefix("data: ") {
                let parsed: Result<GeminiStreamResponse, _> = serde_json::from_str(data);
                assert!(parsed.is_ok());
            }
        }
    }
}