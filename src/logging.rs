use serde::{Deserialize, Serialize};
use std::fs::OpenOptions;
use std::io::Write;
use chrono::Utc;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RequestResponseLog {
    pub timestamp: String,
    pub direction: String, // "client_request", "backend_request", "backend_response", "client_response"
    pub format: String, // "gemini" or "openai"
    pub endpoint: String,
    pub body: serde_json::Value,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub ttft_ms: Option<f64>, // Time to first token in milliseconds
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tps: Option<f64>, // Tokens per second (output_tokens / generation_time)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub itl_ms: Option<f64>, // Inter-token latency in milliseconds
    #[serde(skip_serializing_if = "Option::is_none")]
    pub e2e_latency_ms: Option<f64>, // End-to-end latency in milliseconds
    #[serde(skip_serializing_if = "Option::is_none")]
    pub output_tokens: Option<i32>, // Total output tokens
    #[serde(skip_serializing_if = "Option::is_none")]
    pub input_tokens: Option<i32>, // Total input tokens
}

pub struct JsonLinesLogger {
    file_path: String,
}

impl JsonLinesLogger {
    pub fn new(file_path: String) -> Self {
        Self { file_path }
    }

    pub fn log(&self, entry: RequestResponseLog) -> Result<(), std::io::Error> {
        let mut file = OpenOptions::new()
            .create(true)
            .append(true)
            .open(&self.file_path)?;

        let json_line = serde_json::to_string(&entry)?;
        writeln!(file, "{}", json_line)?;
        Ok(())
    }

    pub fn log_client_request(&self, endpoint: &str, format: &str, body: &serde_json::Value) {
        let entry = RequestResponseLog {
            timestamp: Utc::now().to_rfc3339(),
            direction: "client_request".to_string(),
            format: format.to_string(),
            endpoint: endpoint.to_string(),
            body: body.clone(),
            ttft_ms: None,
            tps: None,
            itl_ms: None,
            e2e_latency_ms: None,
            output_tokens: None,
            input_tokens: None,
        };
        if let Err(e) = self.log(entry) {
            eprintln!("Failed to log client request: {}", e);
        }
    }

    pub fn log_backend_request(&self, endpoint: &str, format: &str, body: &serde_json::Value) {
        let entry = RequestResponseLog {
            timestamp: Utc::now().to_rfc3339(),
            direction: "backend_request".to_string(),
            format: format.to_string(),
            endpoint: endpoint.to_string(),
            body: body.clone(),
            ttft_ms: None,
            tps: None,
            itl_ms: None,
            e2e_latency_ms: None,
            output_tokens: None,
            input_tokens: None,
        };
        if let Err(e) = self.log(entry) {
            eprintln!("Failed to log backend request: {}", e);
        }
    }

    pub fn log_backend_response(&self, endpoint: &str, format: &str, body: &serde_json::Value) {
        let entry = RequestResponseLog {
            timestamp: Utc::now().to_rfc3339(),
            direction: "backend_response".to_string(),
            format: format.to_string(),
            endpoint: endpoint.to_string(),
            body: body.clone(),
            ttft_ms: None,
            tps: None,
            itl_ms: None,
            e2e_latency_ms: None,
            output_tokens: None,
            input_tokens: None,
        };
        if let Err(e) = self.log(entry) {
            eprintln!("Failed to log backend response: {}", e);
        }
    }

    pub fn log_client_response(&self, endpoint: &str, format: &str, body: &serde_json::Value) {
        let entry = RequestResponseLog {
            timestamp: Utc::now().to_rfc3339(),
            direction: "client_response".to_string(),
            format: format.to_string(),
            endpoint: endpoint.to_string(),
            body: body.clone(),
            ttft_ms: None,
            tps: None,
            itl_ms: None,
            e2e_latency_ms: None,
            output_tokens: None,
            input_tokens: None,
        };
        if let Err(e) = self.log(entry) {
            eprintln!("Failed to log client response: {}", e);
        }
    }

    pub fn log_client_response_with_metrics(
        &self,
        endpoint: &str,
        format: &str,
        body: &serde_json::Value,
        ttft_ms: Option<f64>,
        e2e_latency_ms: Option<f64>,
        output_tokens: Option<i32>,
        input_tokens: Option<i32>,
    ) {
        // Calculate TPS and ITL from the provided metrics
        // TPS = output_tokens / generation_time
        // For streaming: generation_time = e2e - ttft
        // For non-streaming: use total e2e time
        let tps = if let (Some(tokens), Some(e2e)) = (output_tokens, e2e_latency_ms) {
            if let Some(ttft) = ttft_ms {
                let generation_time_ms = e2e - ttft;
                if generation_time_ms > 0.001 { // > 1 microsecond
                    // Streaming: use decode time only
                    Some(tokens as f64 / (generation_time_ms / 1000.0))
                } else {
                    // Non-streaming: use total e2e time
                    Some(tokens as f64 / (e2e / 1000.0))
                }
            } else {
                // No TTFT, use total e2e time
                Some(tokens as f64 / (e2e / 1000.0))
            }
        } else {
            None
        };

        let itl_ms = if let (Some(tokens), Some(e2e), Some(ttft)) = (output_tokens, e2e_latency_ms, ttft_ms) {
            if tokens > 1 {
                Some((e2e - ttft) / (tokens as f64 - 1.0))
            } else {
                None
            }
        } else {
            None
        };

        let entry = RequestResponseLog {
            timestamp: Utc::now().to_rfc3339(),
            direction: "client_response".to_string(),
            format: format.to_string(),
            endpoint: endpoint.to_string(),
            body: body.clone(),
            ttft_ms,
            tps,
            itl_ms,
            e2e_latency_ms,
            output_tokens,
            input_tokens,
        };
        if let Err(e) = self.log(entry) {
            eprintln!("Failed to log client response: {}", e);
        }
    }
}
