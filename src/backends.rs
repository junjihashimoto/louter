use crate::config::Config;
use crate::error::ProxyError;
use crate::models::openai::{OpenAIRequest, OpenAIResponse, OpenAIStreamResponse};
use crate::models::gemini::{GeminiRequest, GeminiResponse, CountTokensRequest, CountTokensResponse};
use reqwest::Client;
use futures_util::stream::Stream;
use std::pin::Pin;
use tokio_stream::wrappers::LinesStream;
use tokio_util::io::StreamReader;
use futures_util::StreamExt;
use tokio::io::AsyncBufReadExt;

#[derive(Clone)]
pub struct BackendClient {
    client: Client,
    _backend_type: String,
    config: Config,
}

impl BackendClient {
    pub async fn new(backend_type: &str, config: &Config) -> Result<Self, ProxyError> {
        let client = Client::builder()
            .timeout(std::time::Duration::from_secs(config.performance.timeout_seconds))
            .build()
            .map_err(|e| ProxyError::ConfigError(format!("Failed to create HTTP client: {}", e)))?;

        Ok(Self {
            client,
            _backend_type: backend_type.to_string(),
            config: config.clone(),
        })
    }

    pub async fn chat_completion(
        &self,
        backend_name: &str,  // Add backend_name parameter
        request: OpenAIRequest,
        api_key: &str,
    ) -> Result<OpenAIResponse, ProxyError> {
        // Note: backend_name should be an OpenAI-compatible backend
        // Gemini backend conversion should be handled in the caller

        let backend_config = self.config
            .get_backend(backend_name)  // Use passed backend_name
            .ok_or_else(|| ProxyError::ConfigError(format!("Backend '{}' not configured", backend_name)))?;

        let url = format!("{}/v1/chat/completions", backend_config.url);
        
        let response = self.client
            .post(&url)
            .header("Authorization", format!("Bearer {}", api_key))
            .header("Content-Type", "application/json")
            .json(&request)
            .send()
            .await?;

        if !response.status().is_success() {
            let error_text = response.text().await.unwrap_or_else(|_| "Unknown error".to_string());
            return Err(ProxyError::ConversionError(format!("Backend error: {}", error_text)));
        }

        let mut openai_response: OpenAIResponse = response.json().await?;

        // If backend uses XML tool format, convert XML tool calls to JSON
        if backend_config.tool_format == "xml" {
            for choice in &mut openai_response.choices {
                if let Some(ref content) = choice.message.content {
                    let (cleaned_text, tool_calls) = crate::conversion::extract_and_convert_xml_tool_calls(content);

                    // Update message with cleaned text (XML removed) and extracted tool calls
                    if !tool_calls.is_empty() {
                        choice.message.content = if cleaned_text.is_empty() { None } else { Some(cleaned_text) };
                        choice.message.tool_calls = Some(tool_calls);
                        choice.finish_reason = Some("tool_calls".to_string());
                    }
                }
            }
        }

        Ok(openai_response)
    }

    pub async fn chat_completion_stream(
        &self,
        backend_name: &str,  // Add backend_name parameter
        request: OpenAIRequest,
        api_key: &str,
    ) -> Result<Pin<Box<dyn Stream<Item = Result<OpenAIStreamResponse, ProxyError>> + Send>>, ProxyError> {
        // Note: backend_name should be an OpenAI-compatible backend

        let backend_config = self.config
            .get_backend(backend_name)  // Use passed backend_name
            .ok_or_else(|| ProxyError::ConfigError(format!("Backend '{}' not configured", backend_name)))?;

        let url = format!("{}/v1/chat/completions", backend_config.url);
        
        let response = self.client
            .post(&url)
            .header("Authorization", format!("Bearer {}", api_key))
            .header("Content-Type", "application/json")
            .json(&request)
            .send()
            .await?;

        if !response.status().is_success() {
            let error_text = response.text().await.unwrap_or_else(|_| "Unknown error".to_string());
            return Err(ProxyError::ConversionError(format!("Backend error: {}", error_text)));
        }

        let stream = response.bytes_stream();
        let reader = StreamReader::new(stream.map(|result| {
            result.map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))
        }));
        
        let lines_stream = LinesStream::new(tokio::io::BufReader::new(reader).lines());
        
        let parsed_stream = lines_stream.map(|line_result| {
            match line_result {
                Ok(line) => {
                    if line.starts_with("data: ") {
                        let data = &line[6..];
                        if data == "[DONE]" {
                            return Err(ProxyError::ConversionError("Stream ended".to_string()));
                        }
                        match serde_json::from_str::<OpenAIStreamResponse>(data) {
                            Ok(response) => Ok(response),
                            Err(e) => {
                                // Log the problematic chunk for debugging but don't break the stream
                                eprintln!("Warning: Failed to parse SSE chunk (skipping): {} - Data: {}", e, data);
                                Err(ProxyError::ConversionError("Skipping malformed chunk".to_string()))
                            }
                        }
                    } else if line.is_empty() {
                        // Skip empty lines
                        Err(ProxyError::ConversionError("Empty line".to_string()))
                    } else {
                        // Skip non-data lines (comments, etc.)
                        Err(ProxyError::ConversionError("Non-data line".to_string()))
                    }
                },
                Err(e) => {
                    // Log stream read errors but don't break the entire stream
                    eprintln!("Warning: Stream read error (skipping): {}", e);
                    Err(ProxyError::ConversionError("Stream read error - skipping".to_string()))
                },
            }
        })
        .filter_map(|result| async move {
            match result {
                Ok(response) => Some(Ok(response)),
                // Skip errors instead of propagating them - this allows the stream to continue
                Err(ProxyError::ConversionError(msg)) if msg.contains("Empty line")
                    || msg.contains("Stream ended")
                    || msg.contains("Skipping")
                    || msg.contains("Non-data line") => None,
                Err(e) => {
                    // Only propagate truly fatal errors
                    eprintln!("Fatal stream error: {}", e);
                    Some(Err(e))
                },
            }
        });

        Ok(Box::pin(parsed_stream))
    }

    // Gemini API methods for pass-through mode
    pub async fn gemini_generate_content(
        &self,
        request: GeminiRequest,
        model: &str,
        api_key: &str,
    ) -> Result<GeminiResponse, ProxyError> {
        let backend_config = self.config
            .get_backend("gemini")
            .ok_or_else(|| ProxyError::ConfigError("Gemini backend not configured".to_string()))?;
        
        let mapped_model = self.config.map_model("gemini", model).unwrap_or(model.to_string());
        let url = format!("{}/v1beta/models/{}:generateContent", backend_config.url, mapped_model);
        
        let response = self.client
            .post(&url)
            .query(&[("key", api_key)])
            .json(&request)
            .send()
            .await
            .map_err(|e| ProxyError::BackendError(e))?;

        if !response.status().is_success() {
            let error_text = response.text().await.unwrap_or_else(|_| "Unknown error".to_string());
            return Err(ProxyError::ConversionError(format!("Gemini API error: {}", error_text)));
        }

        let response_text = response.text().await
            .map_err(|e| ProxyError::BackendError(e))?;

        let gemini_response: GeminiResponse = serde_json::from_str(&response_text)
            .map_err(|e| {
                eprintln!("Failed to parse Gemini response: {}", e);
                eprintln!("Response body: {}", response_text);
                ProxyError::ConversionError(format!("Failed to parse Gemini response: {}. Body: {}", e, response_text))
            })?;

        Ok(gemini_response)
    }

    pub async fn gemini_stream_generate_content(
        &self,
        request: GeminiRequest,
        model: &str,
        api_key: &str,
    ) -> Result<Pin<Box<dyn Stream<Item = Result<String, ProxyError>> + Send>>, ProxyError> {
        let backend_config = self.config
            .get_backend("gemini")
            .ok_or_else(|| ProxyError::ConfigError("Gemini backend not configured".to_string()))?;
        
        let mapped_model = self.config.map_model("gemini", model).unwrap_or(model.to_string());
        let url = format!("{}/v1beta/models/{}:streamGenerateContent", backend_config.url, mapped_model);
        
        let response = self.client
            .post(&url)
            .query(&[("key", api_key), ("alt", "sse")])
            .json(&request)
            .send()
            .await
            .map_err(|e| ProxyError::BackendError(e))?;

        if !response.status().is_success() {
            let error_text = response.text().await.unwrap_or_else(|_| "Unknown error".to_string());
            return Err(ProxyError::ConversionError(format!("Gemini streaming error: {}", error_text)));
        }

        // Convert response body to a stream of lines
        let stream = response.bytes_stream();
        let reader = StreamReader::new(stream.map(|result| {
            result.map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))
        }));
        let lines_stream = LinesStream::new(tokio::io::BufReader::new(reader).lines());

        // Parse SSE lines and return raw SSE data
        let parsed_stream = lines_stream.map(|result| {
            match result {
                Ok(line) => {
                    if line.starts_with("data: ") || line.is_empty() {
                        Ok(line) // Return SSE lines as-is for pass-through
                    } else {
                        Ok(line) // Return other lines as-is
                    }
                },
                Err(e) => Err(ProxyError::ConversionError(format!("Stream read error: {}", e))),
            }
        });

        Ok(Box::pin(parsed_stream))
    }

    pub async fn gemini_count_tokens(
        &self,
        request: CountTokensRequest,
        model: &str,
        api_key: &str,
    ) -> Result<CountTokensResponse, ProxyError> {
        let backend_config = self.config
            .get_backend("gemini")
            .ok_or_else(|| ProxyError::ConfigError("Gemini backend not configured".to_string()))?;
        
        let mapped_model = self.config.map_model("gemini", model).unwrap_or(model.to_string());
        let url = format!("{}/v1beta/models/{}:countTokens", backend_config.url, mapped_model);
        
        let response = self.client
            .post(&url)
            .query(&[("key", api_key)])
            .json(&request)
            .send()
            .await
            .map_err(|e| ProxyError::BackendError(e))?;

        if !response.status().is_success() {
            let error_text = response.text().await.unwrap_or_else(|_| "Unknown error".to_string());
            return Err(ProxyError::ConversionError(format!("Gemini countTokens error: {}", error_text)));
        }

        let count_response: CountTokensResponse = response
            .json()
            .await
            .map_err(|e| ProxyError::BackendError(e))?;

        Ok(count_response)
    }
}