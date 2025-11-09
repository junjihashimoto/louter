use crate::models::{gemini::*, openai::*};
use crate::tests::test_resources::*;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_gemini_request_with_actual_images() {
        let png_data = load_test_image_as_base64("red_pixel.png");
        let jpg_data = load_test_image_as_base64("blue_square.jpg");
        
        let request = GeminiRequest {
            contents: vec![
                Content {
                    role: "user".to_string(),
                    parts: vec![
                        Part::Text { text: "Compare these test images:".to_string() },
                        Part::InlineData { 
                            inline_data: InlineData {
                                mime_type: get_mime_type("red_pixel.png").to_string(),
                                data: png_data.clone(),
                            }
                        },
                        Part::InlineData { 
                            inline_data: InlineData {
                                mime_type: get_mime_type("blue_square.jpg").to_string(),
                                data: jpg_data.clone(),
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

        // Verify the structure
        assert_eq!(json["contents"][0]["parts"].as_array().unwrap().len(), 3);
        assert_eq!(json["contents"][0]["parts"][1]["inlineData"]["data"], png_data);
        assert_eq!(json["contents"][0]["parts"][2]["inlineData"]["data"], jpg_data);
    }

    #[test]
    fn test_openai_request_with_actual_images() {
        let png_data = load_test_image_as_base64("red_pixel.png");
        let jpg_data = load_test_image_as_base64("blue_square.jpg");
        
        let request = OpenAIRequest {
            model: "gpt-4-vision-preview".to_string(),
            messages: vec![
                Message::User {
                    role: "user".to_string(),
                    content: MessageContent::Array(vec![
                        ContentPart::Text { text: "Analyze these images:".to_string() },
                        ContentPart::ImageUrl { 
                            image_url: ImageUrl {
                                url: format!("data:image/png;base64,{}", png_data),
                                detail: Some("high".to_string()),
                            }
                        },
                        ContentPart::ImageUrl { 
                            image_url: ImageUrl {
                                url: format!("data:image/jpeg;base64,{}", jpg_data),
                                detail: Some("low".to_string()),
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

        // Verify the structure
        assert_eq!(json["messages"][0]["content"].as_array().unwrap().len(), 3);
        assert!(json["messages"][0]["content"][1]["image_url"]["url"]
            .as_str().unwrap().contains(&png_data));
        assert!(json["messages"][0]["content"][2]["image_url"]["url"]
            .as_str().unwrap().contains(&jpg_data));
    }

    #[test]
    fn test_gemini_with_pdf_and_audio() {
        let pdf_data = load_test_pdf_as_base64();
        let audio_data = load_test_audio_as_base64();
        
        let request = GeminiRequest {
            contents: vec![
                Content {
                    role: "user".to_string(),
                    parts: vec![
                        Part::InlineData { 
                            inline_data: InlineData {
                                mime_type: "application/pdf".to_string(),
                                data: pdf_data.clone(),
                            }
                        },
                        Part::Text { text: "Summarize this document and transcribe the audio:".to_string() },
                        Part::InlineData { 
                            inline_data: InlineData {
                                mime_type: "audio/wav".to_string(),
                                data: audio_data.clone(),
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

        // Verify the PDF data
        assert_eq!(
            json["contents"][0]["parts"][0]["inlineData"]["mimeType"],
            "application/pdf"
        );
        assert_eq!(
            json["contents"][0]["parts"][0]["inlineData"]["data"],
            pdf_data
        );

        // Verify the audio data
        assert_eq!(
            json["contents"][0]["parts"][2]["inlineData"]["mimeType"],
            "audio/wav"
        );
        assert_eq!(
            json["contents"][0]["parts"][2]["inlineData"]["data"],
            audio_data
        );
    }

    #[test]
    fn test_data_url_format() {
        let png_data = load_test_image_as_base64("red_pixel.png");
        let data_url = format!("data:image/png;base64,{}", png_data);
        
        // Verify the data URL format
        assert!(data_url.starts_with("data:image/png;base64,"));
        assert!(data_url.len() > 25); // Should have actual data
        
        // Verify we can create a valid OpenAI message with it
        let message = Message::User {
            role: "user".to_string(),
            content: MessageContent::Array(vec![
                ContentPart::ImageUrl { 
                    image_url: ImageUrl {
                        url: data_url.clone(),
                        detail: None,
                    }
                },
            ]),
        };
        
        let json = serde_json::to_value(&message).unwrap();
        assert_eq!(json["content"][0]["image_url"]["url"], data_url);
    }

    #[test]
    fn test_mixed_media_conversation() {
        let img_data = load_test_image_as_base64("colors.bmp");
        let pdf_data = load_test_pdf_as_base64();
        
        let request = GeminiRequest {
            contents: vec![
                Content {
                    role: "user".to_string(),
                    parts: vec![
                        Part::Text { text: "Look at this image:".to_string() },
                        Part::InlineData { 
                            inline_data: InlineData {
                                mime_type: "image/bmp".to_string(),
                                data: img_data.clone(),
                            }
                        },
                    ],
                },
                Content {
                    role: "model".to_string(),
                    parts: vec![
                        Part::Text { text: "I see a colorful 2x2 pixel image.".to_string() },
                    ],
                },
                Content {
                    role: "user".to_string(),
                    parts: vec![
                        Part::Text { text: "Now analyze this document:".to_string() },
                        Part::InlineData { 
                            inline_data: InlineData {
                                mime_type: "application/pdf".to_string(),
                                data: pdf_data.clone(),
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

        // Verify serialization works
        let json = serde_json::to_value(&request).unwrap();
        assert_eq!(json["contents"].as_array().unwrap().len(), 3);
        
        // Verify we can round-trip the request
        let deserialized: GeminiRequest = serde_json::from_value(json).unwrap();
        assert_eq!(deserialized.contents.len(), 3);
    }
}