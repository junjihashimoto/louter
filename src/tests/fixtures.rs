use serde_json::json;

pub fn gemini_text_response() -> serde_json::Value {
    json!({
        "candidates": [{
            "content": {
                "role": "model",
                "parts": [{
                    "text": "This is a test response from Gemini."
                }]
            },
            "finishReason": "STOP",
            "safetyRatings": [
                {
                    "category": "HARM_CATEGORY_HARASSMENT",
                    "probability": "NEGLIGIBLE"
                },
                {
                    "category": "HARM_CATEGORY_HATE_SPEECH",
                    "probability": "NEGLIGIBLE"
                }
            ]
        }],
        "usageMetadata": {
            "promptTokenCount": 10,
            "candidatesTokenCount": 8,
            "totalTokenCount": 18
        }
    })
}

pub fn openai_text_response() -> serde_json::Value {
    json!({
        "id": "chatcmpl-test123",
        "object": "chat.completion",
        "created": 1700000000,
        "model": "gpt-4",
        "choices": [{
            "index": 0,
            "message": {
                "role": "assistant",
                "content": "This is a test response from OpenAI."
            },
            "finish_reason": "stop"
        }],
        "usage": {
            "prompt_tokens": 10,
            "completion_tokens": 8,
            "total_tokens": 18
        }
    })
}

pub fn gemini_function_call_response() -> serde_json::Value {
    json!({
        "candidates": [{
            "content": {
                "role": "model",
                "parts": [{
                    "functionCall": {
                        "name": "get_weather",
                        "args": {
                            "location": "San Francisco",
                            "unit": "celsius"
                        }
                    }
                }]
            },
            "finishReason": "STOP"
        }]
    })
}

pub fn openai_function_call_response() -> serde_json::Value {
    json!({
        "id": "chatcmpl-test456",
        "object": "chat.completion",
        "created": 1700000000,
        "model": "gpt-4",
        "choices": [{
            "index": 0,
            "message": {
                "role": "assistant",
                "content": null,
                "tool_calls": [{
                    "id": "call_test789",
                    "type": "function",
                    "function": {
                        "name": "get_weather",
                        "arguments": "{\"location\": \"San Francisco\", \"unit\": \"celsius\"}"
                    }
                }]
            },
            "finish_reason": "tool_calls"
        }],
        "usage": {
            "prompt_tokens": 50,
            "completion_tokens": 20,
            "total_tokens": 70
        }
    })
}

pub fn gemini_error_response() -> serde_json::Value {
    json!({
        "error": {
            "code": 400,
            "message": "Invalid API key provided",
            "status": "INVALID_ARGUMENT"
        }
    })
}

pub fn openai_error_response() -> serde_json::Value {
    json!({
        "error": {
            "type": "invalid_request_error",
            "message": "Invalid API key provided",
            "param": null,
            "code": "invalid_api_key"
        }
    })
}

pub fn gemini_stream_chunks() -> Vec<String> {
    vec![
        format!("data: {}\n\n", json!({
            "candidates": [{
                "content": {
                    "role": "model",
                    "parts": [{"text": "Hello"}]
                }
            }]
        })),
        format!("data: {}\n\n", json!({
            "candidates": [{
                "content": {
                    "role": "model",
                    "parts": [{"text": ", "}]
                }
            }]
        })),
        format!("data: {}\n\n", json!({
            "candidates": [{
                "content": {
                    "role": "model",
                    "parts": [{"text": "world!"}]
                },
                "finishReason": "STOP"
            }],
            "usageMetadata": {
                "promptTokenCount": 5,
                "candidatesTokenCount": 3,
                "totalTokenCount": 8
            }
        })),
    ]
}

pub fn openai_stream_chunks() -> Vec<String> {
    vec![
        format!("data: {}\n\n", json!({
            "id": "chatcmpl-stream123",
            "object": "chat.completion.chunk",
            "created": 1700000000,
            "model": "gpt-4",
            "choices": [{
                "index": 0,
                "delta": {"role": "assistant"},
                "finish_reason": null
            }]
        })),
        format!("data: {}\n\n", json!({
            "id": "chatcmpl-stream123",
            "object": "chat.completion.chunk",
            "created": 1700000000,
            "model": "gpt-4",
            "choices": [{
                "index": 0,
                "delta": {"content": "Hello"},
                "finish_reason": null
            }]
        })),
        format!("data: {}\n\n", json!({
            "id": "chatcmpl-stream123",
            "object": "chat.completion.chunk",
            "created": 1700000000,
            "model": "gpt-4",
            "choices": [{
                "index": 0,
                "delta": {"content": ", world!"},
                "finish_reason": null
            }]
        })),
        format!("data: {}\n\n", json!({
            "id": "chatcmpl-stream123",
            "object": "chat.completion.chunk",
            "created": 1700000000,
            "model": "gpt-4",
            "choices": [{
                "index": 0,
                "delta": {},
                "finish_reason": "stop"
            }]
        })),
        "data: [DONE]\n\n".to_string(),
    ]
}

pub fn gemini_multimedia_response() -> serde_json::Value {
    json!({
        "candidates": [{
            "content": {
                "role": "model",
                "parts": [{
                    "text": "I can see a beautiful sunset over the ocean in this image."
                }]
            },
            "finishReason": "STOP",
            "safetyRatings": [
                {
                    "category": "HARM_CATEGORY_DANGEROUS_CONTENT",
                    "probability": "NEGLIGIBLE"
                }
            ]
        }],
        "usageMetadata": {
            "promptTokenCount": 258,
            "candidatesTokenCount": 12,
            "totalTokenCount": 270
        }
    })
}

pub fn openai_multimedia_response() -> serde_json::Value {
    json!({
        "id": "chatcmpl-vision123",
        "object": "chat.completion",
        "created": 1700000000,
        "model": "gpt-4-vision-preview",
        "choices": [{
            "index": 0,
            "message": {
                "role": "assistant",
                "content": "I can see a beautiful sunset over the ocean in this image."
            },
            "finish_reason": "stop"
        }],
        "usage": {
            "prompt_tokens": 265,
            "completion_tokens": 12,
            "total_tokens": 277
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::models::{gemini::*, openai::*};

    #[test]
    fn test_fixtures_valid_json() {
        let _: GeminiResponse = serde_json::from_value(gemini_text_response()).unwrap();
        let _: OpenAIResponse = serde_json::from_value(openai_text_response()).unwrap();
        let _: GeminiResponse = serde_json::from_value(gemini_function_call_response()).unwrap();
        let _: OpenAIResponse = serde_json::from_value(openai_function_call_response()).unwrap();
        let _: GeminiResponse = serde_json::from_value(gemini_multimedia_response()).unwrap();
        let _: OpenAIResponse = serde_json::from_value(openai_multimedia_response()).unwrap();
    }
}