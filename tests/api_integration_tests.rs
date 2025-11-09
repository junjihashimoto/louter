use std::env;
use tokio;
use reqwest;
use louter::models::{gemini::*, openai::*};
use std::time::Instant;
use base64::{Engine as _, engine::general_purpose::STANDARD as BASE64};

fn get_gemini_api_key() -> Option<String> {
    env::var("GEMINI_API_KEY").ok()
}

fn get_openai_api_key() -> Option<String> {
    env::var("OPENAI_API_KEY").ok()
}

fn get_gemini_base_url() -> String {
    env::var("GEMINI_BASE_URL")
        .unwrap_or_else(|_| "https://generativelanguage.googleapis.com".to_string())
}

fn get_openai_base_url() -> String {
    env::var("OPENAI_BASE_URL")
        .unwrap_or_else(|_| "https://api.openai.com".to_string())
}

fn load_test_image_base64(filename: &str) -> String {
    let data = std::fs::read(format!("test-resources/{}", filename)).unwrap();
    BASE64.encode(data)
}

#[tokio::test]
async fn test_gemini_text_request() {
    let api_key = match get_gemini_api_key() {
        Some(key) => key,
        None => {
            println!("Skipping Gemini test: GEMINI_API_KEY not set");
            return;
        }
    };

    let client = reqwest::Client::new();
    let base_url = get_gemini_base_url();
    let request = GeminiRequest {
        contents: vec![
            Content {
                role: "user".to_string(),
                parts: vec![Part::Text { text: "Say hello in exactly 3 words".to_string() }],
            }
        ],
        system_instruction: None,
        safety_settings: None,
        generation_config: Some(GenerationConfig {
            temperature: Some(0.1),
            top_p: None,
            top_k: None,
            max_output_tokens: Some(10),
            stop_sequences: None,
        }),
        tools: None,
        tool_config: None,
    };

    let start = Instant::now();
    let response = client
        .post(&format!("{}/v1beta/models/gemini-2.0-flash:generateContent", base_url))
        .query(&[("key", &api_key)])
        .json(&request)
        .send()
        .await;

    let elapsed = start.elapsed();
    println!("Gemini request took: {:?}", elapsed);

    match response {
        Ok(resp) => {
            let status = resp.status();
            let body = resp.text().await.unwrap();
            println!("Gemini response status: {}", status);
            println!("Gemini response body: {}", body);
            
            if status.is_success() {
                let parsed: Result<GeminiResponse, _> = serde_json::from_str(&body);
                match parsed {
                    Ok(gemini_resp) => {
                        println!("✅ Gemini text request successful");
                        assert!(!gemini_resp.candidates.is_empty());
                        if let Some(usage) = &gemini_resp.usage_metadata {
                            println!("Token usage: {}/{}", usage.prompt_token_count, usage.total_token_count);
                        }
                    },
                    Err(e) => {
                        panic!("Failed to parse Gemini response: {}", e);
                    }
                }
            } else {
                panic!("Gemini API error: {}", body);
            }
        },
        Err(e) => {
            panic!("Gemini request failed: {}", e);
        }
    }
}

#[tokio::test]
async fn test_gemini_vision_request() {
    let api_key = match get_gemini_api_key() {
        Some(key) => key,
        None => {
            println!("Skipping Gemini vision test: GEMINI_API_KEY not set");
            return;
        }
    };

    let image_data = load_test_image_base64("red_pixel.png");
    let client = reqwest::Client::new();
    let base_url = get_gemini_base_url();
    
    let request = GeminiRequest {
        contents: vec![
            Content {
                role: "user".to_string(),
                parts: vec![
                    Part::Text { text: "Describe this image in 5 words or less".to_string() },
                    Part::InlineData { 
                        inline_data: InlineData {
                            mime_type: "image/png".to_string(),
                            data: image_data,
                        }
                    },
                ],
            }
        ],
        safety_settings: None,
        generation_config: Some(GenerationConfig {
            temperature: Some(0.1),
            top_p: None,
            top_k: None,
            max_output_tokens: Some(20),
            stop_sequences: None,
        }),
        tools: None,
        system_instruction: None,
        tool_config: None,
    };

    let start = Instant::now();
    let response = client
        .post(&format!("{}/v1beta/models/gemini-2.0-flash:generateContent", base_url))
        .query(&[("key", &api_key)])
        .json(&request)
        .send()
        .await;

    let elapsed = start.elapsed();
    println!("Gemini vision request took: {:?}", elapsed);

    match response {
        Ok(resp) => {
            let status = resp.status();
            let body = resp.text().await.unwrap();
            println!("Gemini vision response: {}", body);
            
            if status.is_success() {
                let parsed: Result<GeminiResponse, _> = serde_json::from_str(&body);
                match parsed {
                    Ok(gemini_resp) => {
                        println!("✅ Gemini vision request successful");
                        assert!(!gemini_resp.candidates.is_empty());
                    },
                    Err(e) => {
                        println!("⚠️ Failed to parse Gemini vision response: {}", e);
                    }
                }
            } else {
                println!("❌ Gemini vision API error: {}", body);
            }
        },
        Err(e) => {
            println!("❌ Gemini vision request failed: {}", e);
        }
    }
}

#[tokio::test]
async fn test_gemini_streaming_request() {
    let api_key = match get_gemini_api_key() {
        Some(key) => key,
        None => {
            println!("Skipping Gemini streaming test: GEMINI_API_KEY not set");
            return;
        }
    };

    let client = reqwest::Client::new();
    let base_url = get_gemini_base_url();
    let request = GeminiRequest {
        contents: vec![
            Content {
                role: "user".to_string(),
                parts: vec![Part::Text { text: "Count from 1 to 5".to_string() }],
            }
        ],
        system_instruction: None,
        safety_settings: None,
        generation_config: Some(GenerationConfig {
            temperature: Some(0.1),
            top_p: None,
            top_k: None,
            max_output_tokens: Some(50),
            stop_sequences: None,
        }),
        tools: None,
        tool_config: None,
    };

    let start = Instant::now();
    let response = client
        .post(&format!("{}/v1beta/models/gemini-2.0-flash:streamGenerateContent", base_url))
        .query(&[("key", api_key.as_str()), ("alt", "sse")])
        .json(&request)
        .send()
        .await;

    match response {
        Ok(resp) => {
            let status = resp.status();
            if status.is_success() {
                let body = resp.text().await.unwrap();
                let elapsed = start.elapsed();
                println!("Gemini streaming response received in: {:?}", elapsed);
                println!("Streaming response: {}", body);
                
                // Parse SSE events
                let lines: Vec<&str> = body.lines().collect();
                let mut chunk_count = 0;
                for line in lines {
                    if line.starts_with("data: ") {
                        let json_data = &line[6..];
                        if json_data != "[DONE]" {
                            match serde_json::from_str::<GeminiStreamResponse>(json_data) {
                                Ok(_chunk) => {
                                    chunk_count += 1;
                                },
                                Err(e) => {
                                    println!("Failed to parse chunk: {} - {}", e, json_data);
                                }
                            }
                        }
                    }
                }
                println!("✅ Gemini streaming: received {} chunks", chunk_count);
                assert!(chunk_count > 0, "Should receive at least one chunk");
            } else {
                let body = resp.text().await.unwrap();
                println!("❌ Gemini streaming error: {}", body);
            }
        },
        Err(e) => {
            println!("❌ Gemini streaming request failed: {}", e);
        }
    }
}

#[tokio::test]
async fn test_openai_text_request() {
    let api_key = match get_openai_api_key() {
        Some(key) => key,
        None => {
            println!("Skipping OpenAI test: OPENAI_API_KEY not set");
            return;
        }
    };

    let client = reqwest::Client::new();
    let base_url = get_openai_base_url();
    let request = OpenAIRequest {
        model: "gpt-3.5-turbo".to_string(),
        messages: vec![
            Message::User {
                role: "user".to_string(),
                content: MessageContent::Text("Say hello in exactly 3 words".to_string()),
            }
        ],
        temperature: Some(0.1),
        top_p: None,
        n: None,
        stream: Some(false),
        stop: None,
        max_tokens: Some(10),
        max_completion_tokens: None,
        presence_penalty: None,
        frequency_penalty: None,
        logit_bias: None,
        user: None,
        tools: None,
        tool_choice: None,
    };

    let start = Instant::now();
    let response = client
        .post(&format!("{}/v1/chat/completions", base_url))
        .header("Authorization", format!("Bearer {}", api_key))
        .header("Content-Type", "application/json")
        .json(&request)
        .send()
        .await;

    let elapsed = start.elapsed();
    println!("OpenAI request took: {:?}", elapsed);

    match response {
        Ok(resp) => {
            let status = resp.status();
            let body = resp.text().await.unwrap();
            println!("OpenAI response: {}", body);
            
            if status.is_success() {
                let parsed: Result<OpenAIResponse, _> = serde_json::from_str(&body);
                match parsed {
                    Ok(openai_resp) => {
                        println!("✅ OpenAI text request successful");
                        assert!(!openai_resp.choices.is_empty());
                        println!("Token usage: {}/{}", openai_resp.usage.prompt_tokens, openai_resp.usage.total_tokens);
                    },
                    Err(e) => {
                        panic!("Failed to parse OpenAI response: {}", e);
                    }
                }
            } else {
                panic!("OpenAI API error: {}", body);
            }
        },
        Err(e) => {
            panic!("OpenAI request failed: {}", e);
        }
    }
}

#[tokio::test]
async fn test_openai_vision_request() {
    let api_key = match get_openai_api_key() {
        Some(key) => key,
        None => {
            println!("Skipping OpenAI vision test: OPENAI_API_KEY not set");
            return;
        }
    };

    let image_data = load_test_image_base64("red_pixel.png");
    let client = reqwest::Client::new();
    let base_url = get_openai_base_url();
    
    let request = OpenAIRequest {
        model: "gpt-4-vision-preview".to_string(),
        messages: vec![
            Message::User {
                role: "user".to_string(),
                content: MessageContent::Array(vec![
                    ContentPart::Text { text: "Describe this image in 5 words".to_string() },
                    ContentPart::ImageUrl { 
                        image_url: ImageUrl {
                            url: format!("data:image/png;base64,{}", image_data),
                            detail: Some("low".to_string()),
                        }
                    },
                ]),
            }
        ],
        temperature: Some(0.1),
        top_p: None,
        n: None,
        stream: Some(false),
        stop: None,
        max_tokens: Some(20),
        max_completion_tokens: None,
        presence_penalty: None,
        frequency_penalty: None,
        logit_bias: None,
        user: None,
        tools: None,
        tool_choice: None,
    };

    let start = Instant::now();
    let response = client
        .post(&format!("{}/v1/chat/completions", base_url))
        .header("Authorization", format!("Bearer {}", api_key))
        .header("Content-Type", "application/json")
        .json(&request)
        .send()
        .await;

    let elapsed = start.elapsed();
    println!("OpenAI vision request took: {:?}", elapsed);

    match response {
        Ok(resp) => {
            let status = resp.status();
            let body = resp.text().await.unwrap();
            println!("OpenAI vision response: {}", body);
            
            if status.is_success() {
                let parsed: Result<OpenAIResponse, _> = serde_json::from_str(&body);
                match parsed {
                    Ok(openai_resp) => {
                        println!("✅ OpenAI vision request successful");
                        assert!(!openai_resp.choices.is_empty());
                    },
                    Err(e) => {
                        println!("⚠️ Failed to parse OpenAI vision response: {}", e);
                    }
                }
            } else {
                println!("❌ OpenAI vision API error: {}", body);
            }
        },
        Err(e) => {
            println!("❌ OpenAI vision request failed: {}", e);
        }
    }
}

#[tokio::test]
async fn test_openai_streaming_request() {
    let api_key = match get_openai_api_key() {
        Some(key) => key,
        None => {
            println!("Skipping OpenAI streaming test: OPENAI_API_KEY not set");
            return;
        }
    };

    let client = reqwest::Client::new();
    let base_url = get_openai_base_url();
    let request = OpenAIRequest {
        model: "gpt-3.5-turbo".to_string(),
        messages: vec![
            Message::User {
                role: "user".to_string(),
                content: MessageContent::Text("Count from 1 to 5".to_string()),
            }
        ],
        temperature: Some(0.1),
        top_p: None,
        n: None,
        stream: Some(true),
        stop: None,
        max_tokens: Some(50),
        max_completion_tokens: None,
        presence_penalty: None,
        frequency_penalty: None,
        logit_bias: None,
        user: None,
        tools: None,
        tool_choice: None,
    };

    let start = Instant::now();
    let response = client
        .post(&format!("{}/v1/chat/completions", base_url))
        .header("Authorization", format!("Bearer {}", api_key))
        .header("Content-Type", "application/json")
        .json(&request)
        .send()
        .await;

    match response {
        Ok(resp) => {
            let status = resp.status();
            if status.is_success() {
                let body = resp.text().await.unwrap();
                let elapsed = start.elapsed();
                println!("OpenAI streaming response received in: {:?}", elapsed);
                
                // Parse SSE events
                let lines: Vec<&str> = body.lines().collect();
                let mut chunk_count = 0;
                for line in lines {
                    if line.starts_with("data: ") {
                        let json_data = &line[6..];
                        if json_data != "[DONE]" {
                            match serde_json::from_str::<OpenAIStreamResponse>(json_data) {
                                Ok(_chunk) => {
                                    chunk_count += 1;
                                },
                                Err(e) => {
                                    println!("Failed to parse chunk: {} - {}", e, json_data);
                                }
                            }
                        }
                    }
                }
                println!("✅ OpenAI streaming: received {} chunks", chunk_count);
                assert!(chunk_count > 0, "Should receive at least one chunk");
            } else {
                let body = resp.text().await.unwrap();
                println!("❌ OpenAI streaming error: {}", body);
            }
        },
        Err(e) => {
            println!("❌ OpenAI streaming request failed: {}", e);
        }
    }
}