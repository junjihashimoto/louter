use crate::models::gemini::*;
use serde_json::json;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_gemini_request_serialization() {
        let request = GeminiRequest {
            contents: vec![
                Content {
                    role: "user".to_string(),
                    parts: vec![Part::Text { text: "Hello, how are you?".to_string() }],
                },
            ],
            system_instruction: None,
            safety_settings: None,
            generation_config: Some(GenerationConfig {
                temperature: Some(0.7),
                top_p: Some(0.95),
                top_k: Some(40),
                max_output_tokens: Some(1024),
                stop_sequences: None,
            }),
            tools: None,
            tool_config: None,
        };

        let json = serde_json::to_value(&request).unwrap();
        assert_eq!(json["contents"][0]["role"], "user");
        assert_eq!(json["contents"][0]["parts"][0]["text"], "Hello, how are you?");
        assert_eq!(json["generationConfig"]["temperature"], 0.7);
    }

    #[test]
    fn test_gemini_response_deserialization() {
        let json_response = json!({
            "candidates": [{
                "content": {
                    "role": "model",
                    "parts": [{"text": "I'm doing well, thank you!"}]
                },
                "finishReason": "STOP",
                "safetyRatings": [
                    {
                        "category": "HARM_CATEGORY_HARASSMENT",
                        "probability": "NEGLIGIBLE"
                    }
                ]
            }],
            "usageMetadata": {
                "promptTokenCount": 6,
                "candidatesTokenCount": 8,
                "totalTokenCount": 14
            }
        });

        let response: GeminiResponse = serde_json::from_value(json_response).unwrap();
        assert_eq!(response.candidates.len(), 1);
        assert_eq!(response.candidates[0].content.role, "model");
        assert_eq!(response.usage_metadata.as_ref().unwrap().total_token_count, 14);
    }

    #[test]
    fn test_multimodal_request() {
        let request = GeminiRequest {
            contents: vec![
                Content {
                    role: "user".to_string(),
                    parts: vec![
                        Part::Text { text: "What's in this image?".to_string() },
                        Part::InlineData {
                            inline_data: InlineData {
                                mime_type: "image/jpeg".to_string(),
                                data: "base64encodeddata".to_string(),
                            }
                        },
                    ],
                },
            ],
            system_instruction: None,
            safety_settings: None,
            generation_config: None,
            tools: None,
            tool_config: None,
        };

        let json = serde_json::to_value(&request).unwrap();
        assert_eq!(json["contents"][0]["parts"].as_array().unwrap().len(), 2);
        assert_eq!(json["contents"][0]["parts"][1]["inlineData"]["mimeType"], "image/jpeg");
    }

    #[test]
    fn test_function_calling_request() {
        let request = GeminiRequest {
            contents: vec![
                Content {
                    role: "user".to_string(),
                    parts: vec![Part::Text { text: "What's the weather in Tokyo?".to_string() }],
                },
            ],
            system_instruction: None,
            safety_settings: None,
            generation_config: None,
            tools: Some(vec![
                Tool {
                    function_declarations: vec![
                        FunctionDeclaration {
                            name: "get_weather".to_string(),
                            description: "Get weather for a location".to_string(),
                            parameters: Some(json!({
                                "type": "object",
                                "properties": {
                                    "location": {
                                        "type": "string",
                                        "description": "The city name"
                                    }
                                },
                                "required": ["location"]
                            })),
                            parameters_json_schema: None,
                        }
                    ],
                }
            ]),
            tool_config: None,
        };

        let json = serde_json::to_value(&request).unwrap();
        assert!(json["tools"].is_array());
        assert_eq!(json["tools"][0]["functionDeclarations"][0]["name"], "get_weather");
    }

    #[test]
    fn test_function_call_response() {
        let json_response = json!({
            "candidates": [{
                "content": {
                    "role": "model",
                    "parts": [{
                        "functionCall": {
                            "name": "get_weather",
                            "args": {
                                "location": "Tokyo"
                            }
                        }
                    }]
                },
                "finishReason": "STOP"
            }]
        });

        let response: GeminiResponse = serde_json::from_value(json_response).unwrap();
        match &response.candidates[0].content.parts[0] {
            Part::FunctionCall { function_call } => {
                assert_eq!(function_call.name, "get_weather");
                assert_eq!(function_call.args["location"], "Tokyo");
            }
            _ => panic!("Expected function call"),
        }
    }

    #[test]
    fn test_streaming_response() {
        let json_chunk = json!({
            "candidates": [{
                "content": {
                    "role": "model",
                    "parts": [{"text": "Hello"}]
                }
            }]
        });

        let chunk: GeminiStreamResponse = serde_json::from_value(json_chunk).unwrap();
        assert_eq!(chunk.candidates[0].content.parts.len(), 1);
        match &chunk.candidates[0].content.parts[0] {
            Part::Text { text } => assert_eq!(text, "Hello"),
            _ => panic!("Expected text part"),
        }
    }

    #[test]
    fn test_safety_settings() {
        let request = GeminiRequest {
            contents: vec![
                Content {
                    role: "user".to_string(),
                    parts: vec![Part::Text { text: "Test".to_string() }],
                },
            ],
            system_instruction: None,
            safety_settings: Some(vec![
                SafetySetting {
                    category: "HARM_CATEGORY_HARASSMENT".to_string(),
                    threshold: "BLOCK_MEDIUM_AND_ABOVE".to_string(),
                },
                SafetySetting {
                    category: "HARM_CATEGORY_HATE_SPEECH".to_string(),
                    threshold: "BLOCK_LOW_AND_ABOVE".to_string(),
                },
            ]),
            generation_config: None,
            tools: None,
            tool_config: None,
        };

        let json = serde_json::to_value(&request).unwrap();
        assert_eq!(json["safetySettings"].as_array().unwrap().len(), 2);
        assert_eq!(json["safetySettings"][0]["category"], "HARM_CATEGORY_HARASSMENT");
    }

    #[test]
    fn test_multiple_turn_conversation() {
        let request = GeminiRequest {
            contents: vec![
                Content {
                    role: "user".to_string(),
                    parts: vec![Part::Text { text: "What's 2+2?".to_string() }],
                },
                Content {
                    role: "model".to_string(),
                    parts: vec![Part::Text { text: "2+2 equals 4.".to_string() }],
                },
                Content {
                    role: "user".to_string(),
                    parts: vec![Part::Text { text: "What about 3+3?".to_string() }],
                },
            ],
            system_instruction: None,
            safety_settings: None,
            generation_config: None,
            tools: None,
            tool_config: None,
        };

        let json = serde_json::to_value(&request).unwrap();
        assert_eq!(json["contents"].as_array().unwrap().len(), 3);
        assert_eq!(json["contents"][0]["role"], "user");
        assert_eq!(json["contents"][1]["role"], "model");
        assert_eq!(json["contents"][2]["role"], "user");
    }

    #[test]
    fn test_multimedia_with_multiple_images() {
        let request = GeminiRequest {
            contents: vec![
                Content {
                    role: "user".to_string(),
                    parts: vec![
                        Part::Text { text: "Compare these two images:".to_string() },
                        Part::InlineData {
                            inline_data: InlineData {
                                mime_type: "image/jpeg".to_string(),
                                data: "base64_image1_data".to_string(),
                            }
                        },
                        Part::InlineData {
                            inline_data: InlineData {
                                mime_type: "image/png".to_string(),
                                data: "base64_image2_data".to_string(),
                            }
                        },
                        Part::Text { text: "What are the differences?".to_string() },
                    ],
                },
            ],
            system_instruction: None,
            safety_settings: None,
            generation_config: None,
            tools: None,
            tool_config: None,
        };

        let json = serde_json::to_value(&request).unwrap();
        assert_eq!(json["contents"][0]["parts"].as_array().unwrap().len(), 4);
        assert_eq!(json["contents"][0]["parts"][0]["text"], "Compare these two images:");
        assert_eq!(json["contents"][0]["parts"][1]["inlineData"]["mimeType"], "image/jpeg");
        assert_eq!(json["contents"][0]["parts"][2]["inlineData"]["mimeType"], "image/png");
        assert_eq!(json["contents"][0]["parts"][3]["text"], "What are the differences?");
    }

    #[test]
    fn test_multimedia_video_support() {
        let request = GeminiRequest {
            contents: vec![
                Content {
                    role: "user".to_string(),
                    parts: vec![
                        Part::Text { text: "Analyze this video:".to_string() },
                        Part::InlineData {
                            inline_data: InlineData {
                                mime_type: "video/mp4".to_string(),
                                data: "base64_video_data".to_string(),
                            }
                        },
                    ],
                },
            ],
            system_instruction: None,
            safety_settings: None,
            generation_config: None,
            tools: None,
            tool_config: None,
        };

        let json = serde_json::to_value(&request).unwrap();
        assert_eq!(json["contents"][0]["parts"][1]["inlineData"]["mimeType"], "video/mp4");
    }

    #[test]
    fn test_multimedia_audio_support() {
        let request = GeminiRequest {
            contents: vec![
                Content {
                    role: "user".to_string(),
                    parts: vec![
                        Part::Text { text: "Transcribe this audio:".to_string() },
                        Part::InlineData { 
                            inline_data: InlineData {
                                mime_type: "audio/mp3".to_string(),
                                data: "base64_audio_data".to_string(),
                            }
                        },
                    ],
                },
            ],
            system_instruction: None,
            safety_settings: None,
            generation_config: None,
            tools: None,
            tool_config: None,
        };

        let json = serde_json::to_value(&request).unwrap();
        assert_eq!(json["contents"][0]["parts"][1]["inlineData"]["mimeType"], "audio/mp3");
    }

    #[test]
    fn test_multimedia_pdf_support() {
        let request = GeminiRequest {
            contents: vec![
                Content {
                    role: "user".to_string(),
                    parts: vec![
                        Part::InlineData { 
                            inline_data: InlineData {
                                mime_type: "application/pdf".to_string(),
                                data: "base64_pdf_data".to_string(),
                            }
                        },
                        Part::Text { text: "Summarize this document.".to_string() },
                    ],
                },
            ],
            system_instruction: None,
            safety_settings: None,
            generation_config: None,
            tools: None,
            tool_config: None,
        };

        let json = serde_json::to_value(&request).unwrap();
        assert_eq!(json["contents"][0]["parts"][0]["inlineData"]["mimeType"], "application/pdf");
    }

    #[test]
    fn test_multimedia_in_conversation() {
        let request = GeminiRequest {
            contents: vec![
                Content {
                    role: "user".to_string(),
                    parts: vec![
                        Part::Text { text: "Look at this image:".to_string() },
                        Part::InlineData { 
                            inline_data: InlineData {
                                mime_type: "image/jpeg".to_string(),
                                data: "base64_image_data".to_string(),
                            }
                        },
                    ],
                },
                Content {
                    role: "model".to_string(),
                    parts: vec![Part::Text { text: "I can see an image of a cat.".to_string() }],
                },
                Content {
                    role: "user".to_string(),
                    parts: vec![
                        Part::Text { text: "Now compare it with this one:".to_string() },
                        Part::InlineData { 
                            inline_data: InlineData {
                                mime_type: "image/jpeg".to_string(),
                                data: "base64_another_image_data".to_string(),
                            }
                        },
                    ],
                },
            ],
            system_instruction: None,
            safety_settings: None,
            generation_config: None,
            tools: None,
            tool_config: None,
        };

        let json = serde_json::to_value(&request).unwrap();
        assert_eq!(json["contents"].as_array().unwrap().len(), 3);
        assert!(json["contents"][0]["parts"][1]["inlineData"].is_object());
        assert!(json["contents"][2]["parts"][1]["inlineData"].is_object());
    }

    #[test]
    fn test_multimedia_with_function_calling() {
        let request = GeminiRequest {
            contents: vec![
                Content {
                    role: "user".to_string(),
                    parts: vec![
                        Part::InlineData { 
                            inline_data: InlineData {
                                mime_type: "image/jpeg".to_string(),
                                data: "base64_receipt_image".to_string(),
                            }
                        },
                        Part::Text { text: "Extract the total amount from this receipt and save it.".to_string() },
                    ],
                },
            ],
            system_instruction: None,
            safety_settings: None,
            generation_config: None,
            tools: Some(vec![
                Tool {
                    function_declarations: vec![
                        FunctionDeclaration {
                            name: "save_expense".to_string(),
                            description: "Save an expense entry".to_string(),
                            parameters: Some(json!({
                                "type": "object",
                                "properties": {
                                    "amount": {
                                        "type": "number",
                                        "description": "The expense amount"
                                    },
                                    "description": {
                                        "type": "string",
                                        "description": "Description of the expense"
                                    }
                                },
                                "required": ["amount"]
                            })),
                            parameters_json_schema: None,
                        }
                    ],
                }
            ]),
            tool_config: None,
        };

        let json = serde_json::to_value(&request).unwrap();
        assert!(json["contents"][0]["parts"][0]["inlineData"].is_object());
        assert!(json["tools"].is_array());
    }
}