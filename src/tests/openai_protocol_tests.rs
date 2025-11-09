use crate::models::openai::*;
use serde_json::json;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_openai_request_serialization() {
        let request = OpenAIRequest {
            model: "gpt-4".to_string(),
            messages: vec![
                Message::System {
                    role: "system".to_string(),
                    content: "You are a helpful assistant.".to_string(),
                },
                Message::User {
                    role: "user".to_string(),
                    content: MessageContent::Text("Hello!".to_string()),
                },
            ],
            temperature: Some(0.7),
            top_p: Some(0.95),
            n: None,
            stream: Some(false),
            stop: None,
            max_tokens: Some(150),
            max_completion_tokens: None,
            presence_penalty: None,
            frequency_penalty: None,
            logit_bias: None,
            user: None,
            tools: None,
            tool_choice: None,
        };

        let json = serde_json::to_value(&request).unwrap();
        assert_eq!(json["model"], "gpt-4");
        assert_eq!(json["messages"][0]["role"], "system");
        assert_eq!(json["messages"][1]["content"], "Hello!");
        assert_eq!(json["temperature"], 0.7);
    }

    #[test]
    fn test_openai_response_deserialization() {
        let json_response = json!({
            "id": "chatcmpl-123",
            "object": "chat.completion",
            "created": 1677858242,
            "model": "gpt-4",
            "choices": [{
                "index": 0,
                "message": {
                    "role": "assistant",
                    "content": "Hello! How can I help you today?"
                },
                "finish_reason": "stop"
            }],
            "usage": {
                "prompt_tokens": 13,
                "completion_tokens": 9,
                "total_tokens": 22
            }
        });

        let response: OpenAIResponse = serde_json::from_value(json_response).unwrap();
        assert_eq!(response.id, "chatcmpl-123");
        assert_eq!(response.choices[0].message.content.as_ref().unwrap(), "Hello! How can I help you today?");
        assert_eq!(response.usage.total_tokens, 22);
    }

    #[test]
    fn test_multimodal_message() {
        let request = OpenAIRequest {
            model: "gpt-4-vision-preview".to_string(),
            messages: vec![
                Message::User {
                    role: "user".to_string(),
                    content: MessageContent::Array(vec![
                        ContentPart::Text { text: "What's in this image?".to_string() },
                        ContentPart::ImageUrl { 
                            image_url: ImageUrl {
                                url: "https://example.com/image.jpg".to_string(),
                                detail: Some("high".to_string()),
                            }
                        },
                    ]),
                },
            ],
            temperature: None,
            top_p: None,
            n: None,
            stream: None,
            stop: None,
            max_tokens: Some(300),
            max_completion_tokens: None,
            presence_penalty: None,
            frequency_penalty: None,
            logit_bias: None,
            user: None,
            tools: None,
            tool_choice: None,
        };

        let json = serde_json::to_value(&request).unwrap();
        assert_eq!(json["model"], "gpt-4-vision-preview");
        assert_eq!(json["messages"][0]["content"][0]["type"], "text");
        assert_eq!(json["messages"][0]["content"][1]["type"], "image_url");
        assert_eq!(json["messages"][0]["content"][1]["image_url"]["detail"], "high");
    }

    #[test]
    fn test_function_calling() {
        let request = OpenAIRequest {
            model: "gpt-4".to_string(),
            messages: vec![
                Message::User {
                    role: "user".to_string(),
                    content: MessageContent::Text("What's the weather in Boston?".to_string()),
                },
            ],
            temperature: None,
            top_p: None,
            n: None,
            stream: None,
            stop: None,
            max_tokens: None,
            max_completion_tokens: None,
            presence_penalty: None,
            frequency_penalty: None,
            logit_bias: None,
            user: None,
            tools: Some(vec![
                Tool {
                    tool_type: "function".to_string(),
                    function: Function {
                        name: "get_current_weather".to_string(),
                        description: Some("Get the current weather in a given location".to_string()),
                        parameters: Some(json!({
                            "type": "object",
                            "properties": {
                                "location": {
                                    "type": "string",
                                    "description": "The city and state"
                                },
                                "unit": {
                                    "type": "string",
                                    "enum": ["celsius", "fahrenheit"]
                                }
                            },
                            "required": ["location"]
                        })),
                    },
                }
            ]),
            tool_choice: Some(ToolChoice::String("auto".to_string())),
        };

        let json = serde_json::to_value(&request).unwrap();
        assert!(json["tools"].is_array());
        assert_eq!(json["tools"][0]["type"], "function");
        assert_eq!(json["tools"][0]["function"]["name"], "get_current_weather");
    }

    #[test]
    fn test_function_call_response() {
        let json_response = json!({
            "id": "chatcmpl-123",
            "object": "chat.completion",
            "created": 1677858242,
            "model": "gpt-4",
            "choices": [{
                "index": 0,
                "message": {
                    "role": "assistant",
                    "content": null,
                    "tool_calls": [{
                        "id": "call_123",
                        "type": "function",
                        "function": {
                            "name": "get_current_weather",
                            "arguments": "{\"location\": \"Boston, MA\"}"
                        }
                    }]
                },
                "finish_reason": "tool_calls"
            }],
            "usage": {
                "prompt_tokens": 82,
                "completion_tokens": 17,
                "total_tokens": 99
            }
        });

        let response: OpenAIResponse = serde_json::from_value(json_response).unwrap();
        assert_eq!(response.choices[0].finish_reason.as_ref().unwrap(), "tool_calls");
        let tool_calls = response.choices[0].message.tool_calls.as_ref().unwrap();
        assert_eq!(tool_calls[0].function.name, "get_current_weather");
        assert_eq!(tool_calls[0].function.arguments, "{\"location\": \"Boston, MA\"}");
    }

    #[test]
    fn test_streaming_response() {
        let json_chunk = json!({
            "id": "chatcmpl-123",
            "object": "chat.completion.chunk",
            "created": 1677858242,
            "model": "gpt-4",
            "choices": [{
                "index": 0,
                "delta": {
                    "content": "Hello"
                },
                "finish_reason": null
            }]
        });

        let chunk: OpenAIStreamResponse = serde_json::from_value(json_chunk).unwrap();
        assert_eq!(chunk.object, "chat.completion.chunk");
        assert_eq!(chunk.choices[0].delta.content.as_ref().unwrap(), "Hello");
    }

    #[test]
    fn test_streaming_with_role() {
        let json_chunk = json!({
            "id": "chatcmpl-123",
            "object": "chat.completion.chunk",
            "created": 1677858242,
            "model": "gpt-4",
            "choices": [{
                "index": 0,
                "delta": {
                    "role": "assistant"
                },
                "finish_reason": null
            }]
        });

        let chunk: OpenAIStreamResponse = serde_json::from_value(json_chunk).unwrap();
        assert_eq!(chunk.choices[0].delta.role.as_ref().unwrap(), "assistant");
    }

    #[test]
    fn test_tool_message() {
        let request = OpenAIRequest {
            model: "gpt-4".to_string(),
            messages: vec![
                Message::User {
                    role: "user".to_string(),
                    content: MessageContent::Text("What's the weather?".to_string()),
                },
                Message::Assistant {
                    role: "assistant".to_string(),
                    content: None,
                    tool_calls: Some(vec![
                        ToolCall {
                            id: "call_123".to_string(),
                            tool_type: "function".to_string(),
                            function: FunctionCall {
                                name: "get_weather".to_string(),
                                arguments: "{\"location\": \"Boston\"}".to_string(),
                            },
                        }
                    ]),
                },
                Message::Tool {
                    role: "tool".to_string(),
                    content: "72 degrees and sunny".to_string(),
                    tool_call_id: "call_123".to_string(),
                },
            ],
            temperature: None,
            top_p: None,
            n: None,
            stream: None,
            stop: None,
            max_tokens: None,
            max_completion_tokens: None,
            presence_penalty: None,
            frequency_penalty: None,
            logit_bias: None,
            user: None,
            tools: None,
            tool_choice: None,
        };

        let json = serde_json::to_value(&request).unwrap();
        assert_eq!(json["messages"][2]["role"], "tool");
        assert_eq!(json["messages"][2]["tool_call_id"], "call_123");
    }

    #[test]
    fn test_stop_sequences() {
        let request = OpenAIRequest {
            model: "gpt-4".to_string(),
            messages: vec![
                Message::User {
                    role: "user".to_string(),
                    content: MessageContent::Text("Count to 10".to_string()),
                },
            ],
            temperature: None,
            top_p: None,
            n: None,
            stream: None,
            stop: Some(vec!["\n".to_string(), "5".to_string()]),
            max_tokens: None,
            max_completion_tokens: None,
            presence_penalty: None,
            frequency_penalty: None,
            logit_bias: None,
            user: None,
            tools: None,
            tool_choice: None,
        };

        let json = serde_json::to_value(&request).unwrap();
        assert_eq!(json["stop"], json!(["\n", "5"]));
    }

    #[test]
    fn test_multimedia_base64_image() {
        let request = OpenAIRequest {
            model: "gpt-4-vision-preview".to_string(),
            messages: vec![
                Message::User {
                    role: "user".to_string(),
                    content: MessageContent::Array(vec![
                        ContentPart::Text { text: "What's in this image?".to_string() },
                        ContentPart::ImageUrl { 
                            image_url: ImageUrl {
                                url: "data:image/jpeg;base64,/9j/4AAQSkZJRg...".to_string(),
                                detail: Some("high".to_string()),
                            }
                        },
                    ]),
                },
            ],
            temperature: None,
            top_p: None,
            n: None,
            stream: None,
            stop: None,
            max_tokens: None,
            max_completion_tokens: None,
            presence_penalty: None,
            frequency_penalty: None,
            logit_bias: None,
            user: None,
            tools: None,
            tool_choice: None,
        };

        let json = serde_json::to_value(&request).unwrap();
        assert!(json["messages"][0]["content"][1]["image_url"]["url"].as_str().unwrap().starts_with("data:image/jpeg;base64,"));
    }

    #[test]
    fn test_multimedia_multiple_images() {
        let request = OpenAIRequest {
            model: "gpt-4-vision-preview".to_string(),
            messages: vec![
                Message::User {
                    role: "user".to_string(),
                    content: MessageContent::Array(vec![
                        ContentPart::Text { text: "Compare these images:".to_string() },
                        ContentPart::ImageUrl { 
                            image_url: ImageUrl {
                                url: "https://example.com/image1.jpg".to_string(),
                                detail: Some("low".to_string()),
                            }
                        },
                        ContentPart::ImageUrl { 
                            image_url: ImageUrl {
                                url: "https://example.com/image2.jpg".to_string(),
                                detail: Some("high".to_string()),
                            }
                        },
                        ContentPart::Text { text: "What are the main differences?".to_string() },
                    ]),
                },
            ],
            temperature: None,
            top_p: None,
            n: None,
            stream: None,
            stop: None,
            max_tokens: Some(500),
            max_completion_tokens: None,
            presence_penalty: None,
            frequency_penalty: None,
            logit_bias: None,
            user: None,
            tools: None,
            tool_choice: None,
        };

        let json = serde_json::to_value(&request).unwrap();
        assert_eq!(json["messages"][0]["content"].as_array().unwrap().len(), 4);
        assert_eq!(json["messages"][0]["content"][1]["image_url"]["detail"], "low");
        assert_eq!(json["messages"][0]["content"][2]["image_url"]["detail"], "high");
    }

    #[test]
    fn test_multimedia_conversation() {
        let request = OpenAIRequest {
            model: "gpt-4-vision-preview".to_string(),
            messages: vec![
                Message::User {
                    role: "user".to_string(),
                    content: MessageContent::Array(vec![
                        ContentPart::Text { text: "Look at this chart:".to_string() },
                        ContentPart::ImageUrl { 
                            image_url: ImageUrl {
                                url: "https://example.com/chart.png".to_string(),
                                detail: None,
                            }
                        },
                    ]),
                },
                Message::Assistant {
                    role: "assistant".to_string(),
                    content: Some("I can see a bar chart showing quarterly sales data.".to_string()),
                    tool_calls: None,
                },
                Message::User {
                    role: "user".to_string(),
                    content: MessageContent::Array(vec![
                        ContentPart::Text { text: "Now compare it with this one:".to_string() },
                        ContentPart::ImageUrl { 
                            image_url: ImageUrl {
                                url: "data:image/png;base64,iVBORw0KGgo...".to_string(),
                                detail: Some("high".to_string()),
                            }
                        },
                    ]),
                },
            ],
            temperature: None,
            top_p: None,
            n: None,
            stream: None,
            stop: None,
            max_tokens: None,
            max_completion_tokens: None,
            presence_penalty: None,
            frequency_penalty: None,
            logit_bias: None,
            user: None,
            tools: None,
            tool_choice: None,
        };

        let json = serde_json::to_value(&request).unwrap();
        assert_eq!(json["messages"].as_array().unwrap().len(), 3);
        assert_eq!(json["messages"][1]["role"], "assistant");
        assert!(json["messages"][2]["content"][1]["image_url"]["url"].as_str().unwrap().starts_with("data:image/png;base64,"));
    }

    #[test]
    fn test_multimedia_with_function_calling() {
        let request = OpenAIRequest {
            model: "gpt-4-vision-preview".to_string(),
            messages: vec![
                Message::User {
                    role: "user".to_string(),
                    content: MessageContent::Array(vec![
                        ContentPart::ImageUrl { 
                            image_url: ImageUrl {
                                url: "https://example.com/product.jpg".to_string(),
                                detail: Some("auto".to_string()),
                            }
                        },
                        ContentPart::Text { text: "Identify this product and check its price.".to_string() },
                    ]),
                },
            ],
            temperature: None,
            top_p: None,
            n: None,
            stream: None,
            stop: None,
            max_tokens: None,
            max_completion_tokens: None,
            presence_penalty: None,
            frequency_penalty: None,
            logit_bias: None,
            user: None,
            tools: Some(vec![
                Tool {
                    tool_type: "function".to_string(),
                    function: Function {
                        name: "search_product".to_string(),
                        description: Some("Search for a product by name or description".to_string()),
                        parameters: Some(json!({
                            "type": "object",
                            "properties": {
                                "product_name": {
                                    "type": "string",
                                    "description": "Name or description of the product"
                                }
                            },
                            "required": ["product_name"]
                        })),
                    },
                },
                Tool {
                    tool_type: "function".to_string(),
                    function: Function {
                        name: "get_price".to_string(),
                        description: Some("Get the price of a product".to_string()),
                        parameters: Some(json!({
                            "type": "object",
                            "properties": {
                                "product_id": {
                                    "type": "string",
                                    "description": "Product ID"
                                }
                            },
                            "required": ["product_id"]
                        })),
                    },
                }
            ]),
            tool_choice: None,
        };

        let json = serde_json::to_value(&request).unwrap();
        assert_eq!(json["messages"][0]["content"][0]["type"], "image_url");
        assert_eq!(json["messages"][0]["content"][0]["image_url"]["detail"], "auto");
        assert_eq!(json["tools"].as_array().unwrap().len(), 2);
    }

    #[test]
    fn test_detail_levels() {
        let test_cases = vec![
            ("low", "Low quality for faster processing"),
            ("high", "High quality for detailed analysis"),
            ("auto", "Automatic quality selection"),
        ];

        for (detail, _description) in test_cases {
            let request = OpenAIRequest {
                model: "gpt-4-vision-preview".to_string(),
                messages: vec![
                    Message::User {
                        role: "user".to_string(),
                        content: MessageContent::Array(vec![
                            ContentPart::ImageUrl { 
                                image_url: ImageUrl {
                                    url: "https://example.com/test.jpg".to_string(),
                                    detail: Some(detail.to_string()),
                                }
                            },
                        ]),
                    },
                ],
                temperature: None,
                top_p: None,
                n: None,
                stream: None,
                stop: None,
                max_tokens: None,
                max_completion_tokens: None,
                presence_penalty: None,
                frequency_penalty: None,
                logit_bias: None,
                user: None,
                tools: None,
                tool_choice: None,
            };

            let json = serde_json::to_value(&request).unwrap();
            assert_eq!(json["messages"][0]["content"][0]["image_url"]["detail"], detail);
        }
    }
}