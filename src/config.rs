use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Config {
    pub backends: HashMap<String, BackendConfig>,
    pub custom_instructions: Option<String>,
    pub performance: PerformanceConfig,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BackendConfig {
    pub url: String,
    pub model_mapping: HashMap<String, String>,
    #[serde(default = "default_protocol")]
    pub protocol: String, // "openai" or "gemini" - API protocol this backend uses
    pub max_tokens: Option<i32>,
    pub temperature: Option<f64>,
    pub temperature_override: Option<bool>, // If true, override client's temperature with backend's temperature
    pub weight: Option<f64>, // For load balancing
    pub api_key: Option<String>, // API key with higher priority than env vars
    #[serde(default = "default_tool_format")]
    pub tool_format: String, // "json" or "xml" - tool calling format for this backend
    #[serde(default = "default_max_tokens_field")]
    pub max_tokens_field: String, // "max_tokens", "max_completion_tokens", or "both"
    pub custom_instruction: Option<String>, // Per-backend custom instruction
    #[serde(default = "default_custom_instruction_mode")]
    pub custom_instruction_mode: String, // "override", "prepend", or "append"
    #[serde(default)]
    pub capabilities: Vec<String>, // ["text", "vision", "audio", "function_calling"]
    #[serde(default = "default_priority")]
    pub priority: u32, // Lower number = higher priority (for fallback ordering)
    pub fallback: Option<String>, // Backend name to fallback to if capabilities don't match
}

fn default_priority() -> u32 {
    100 // Default priority (lower is better)
}

fn default_protocol() -> String {
    "openai".to_string() // Default to OpenAI protocol (most common)
}

fn default_tool_format() -> String {
    "json".to_string()
}

fn default_max_tokens_field() -> String {
    "both".to_string() // Default to setting both fields for maximum compatibility
}

fn default_custom_instruction_mode() -> String {
    "append".to_string()
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PerformanceConfig {
    pub enable_metrics: bool,
    pub log_requests: bool,
    pub timeout_seconds: u64,
    pub openai_token_counting_mode: Option<String>, // "estimate" or "chat"
    pub allowed_tools: Option<Vec<String>>, // Whitelist of tool names to send (helps smaller models)
    pub override_system_instruction: Option<String>, // Replace client's system instruction (helps smaller models)
    pub max_conversation_turns: Option<usize>, // Limit conversation history (helps smaller models avoid confusion)
}

impl Default for Config {
    fn default() -> Self {
        let mut backends = HashMap::new();
        backends.insert(
            "openai".to_string(),
            BackendConfig {
                url: "https://api.openai.com".to_string(),
                model_mapping: {
                    let mut mapping = HashMap::new();
                    mapping.insert("gemini-2.0-flash".to_string(), "gpt-4".to_string());
                    mapping.insert("gemini-pro".to_string(), "gpt-3.5-turbo".to_string());
                    mapping
                },
                protocol: "openai".to_string(),
                max_tokens: Some(4096),
                temperature: Some(0.7),
                temperature_override: None, // Don't override by default
                weight: Some(1.0),
                api_key: None, // Use environment variable by default
                tool_format: "json".to_string(),
                max_tokens_field: "both".to_string(),
                custom_instruction: None,
                custom_instruction_mode: "append".to_string(),
                capabilities: vec![], // Auto mode: empty capabilities = accepts all types
                priority: 1,
                fallback: None, // No fallback by default
            },
        );

        Self {
            backends,
            custom_instructions: None,
            performance: PerformanceConfig {
                enable_metrics: true,
                log_requests: true,
                timeout_seconds: 30,
                openai_token_counting_mode: Some("estimate".to_string()),
                allowed_tools: None, // No filter by default (send all tools)
                override_system_instruction: None, // No override by default
                max_conversation_turns: None, // No limit by default
            },
        }
    }
}

impl Config {
    pub fn load(path: &str) -> Result<Self, Box<dyn std::error::Error>> {
        let content = std::fs::read_to_string(path)?;
        let config = toml::from_str(&content)?;
        Ok(config)
    }

    pub fn get_backend(&self, backend_type: &str) -> Option<&BackendConfig> {
        self.backends.get(backend_type)
    }

    pub fn map_model(&self, backend_type: &str, gemini_model: &str) -> Option<String> {
        self.backends
            .get(backend_type)?
            .model_mapping
            .get(gemini_model)
            .cloned()
    }

    pub fn get_api_key(&self, backend_type: &str, env_var_name: &str) -> Option<String> {
        // Priority 1: Config file API key
        if let Some(backend) = self.backends.get(backend_type) {
            if let Some(api_key) = &backend.api_key {
                return Some(api_key.clone());
            }
        }

        // Priority 2: Environment variable
        std::env::var(env_var_name).ok()
    }

    /// Apply per-backend custom instruction to an existing system instruction
    pub fn apply_custom_instruction(&self, backend_type: &str, original: Option<&str>) -> Option<String> {
        let backend = self.backends.get(backend_type)?;
        let custom = backend.custom_instruction.as_ref()?;

        match backend.custom_instruction_mode.as_str() {
            "override" => Some(custom.clone()),
            "prepend" => match original {
                Some(orig) => Some(format!("{}\n\n{}", custom, orig)),
                None => Some(custom.clone()),
            },
            "append" | _ => match original {
                Some(orig) => Some(format!("{}\n\n{}", orig, custom)),
                None => Some(custom.clone()),
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_config(mode: &str, custom_instr: Option<&str>) -> Config {
        let mut backends = HashMap::new();
        backends.insert(
            "test_backend".to_string(),
            BackendConfig {
                url: "http://localhost:8080".to_string(),
                model_mapping: HashMap::new(),
                protocol: "openai".to_string(),
                max_tokens: Some(100),
                temperature: Some(0.7),
                temperature_override: None,
                weight: Some(1.0),
                api_key: None,
                tool_format: "json".to_string(),
                max_tokens_field: "max_tokens".to_string(),
                custom_instruction: custom_instr.map(|s| s.to_string()),
                custom_instruction_mode: mode.to_string(),
                capabilities: vec!["text".to_string()],
                priority: 1,
                fallback: None,
            },
        );

        Config {
            backends,
            custom_instructions: None,
            performance: PerformanceConfig {
                enable_metrics: false,
                log_requests: false,
                timeout_seconds: 30,
                openai_token_counting_mode: None,
                allowed_tools: None,
                override_system_instruction: None,
                max_conversation_turns: None,
            },
        }
    }

    #[test]
    fn test_append_mode_with_original() {
        let config = create_test_config("append", Some("Custom instruction here."));
        let result = config.apply_custom_instruction("test_backend", Some("Original instruction."));
        assert_eq!(result, Some("Original instruction.\n\nCustom instruction here.".to_string()));
    }

    #[test]
    fn test_append_mode_without_original() {
        let config = create_test_config("append", Some("Custom instruction here."));
        let result = config.apply_custom_instruction("test_backend", None);
        assert_eq!(result, Some("Custom instruction here.".to_string()));
    }

    #[test]
    fn test_prepend_mode_with_original() {
        let config = create_test_config("prepend", Some("Custom instruction here."));
        let result = config.apply_custom_instruction("test_backend", Some("Original instruction."));
        assert_eq!(result, Some("Custom instruction here.\n\nOriginal instruction.".to_string()));
    }

    #[test]
    fn test_prepend_mode_without_original() {
        let config = create_test_config("prepend", Some("Custom instruction here."));
        let result = config.apply_custom_instruction("test_backend", None);
        assert_eq!(result, Some("Custom instruction here.".to_string()));
    }

    #[test]
    fn test_override_mode_with_original() {
        let config = create_test_config("override", Some("Custom instruction here."));
        let result = config.apply_custom_instruction("test_backend", Some("Original instruction."));
        // Override mode ignores original and returns only custom instruction
        assert_eq!(result, Some("Custom instruction here.".to_string()));
    }

    #[test]
    fn test_override_mode_without_original() {
        let config = create_test_config("override", Some("Custom instruction here."));
        let result = config.apply_custom_instruction("test_backend", None);
        assert_eq!(result, Some("Custom instruction here.".to_string()));
    }

    #[test]
    fn test_no_custom_instruction() {
        let config = create_test_config("append", None);
        let result = config.apply_custom_instruction("test_backend", Some("Original instruction."));
        // Should return None when no custom instruction is configured
        assert_eq!(result, None);
    }

    #[test]
    fn test_invalid_backend() {
        let config = create_test_config("append", Some("Custom instruction here."));
        let result = config.apply_custom_instruction("nonexistent_backend", Some("Original instruction."));
        // Should return None when backend doesn't exist
        assert_eq!(result, None);
    }

    #[test]
    fn test_default_mode_is_append() {
        // Test that default mode behaves like append
        let mut backends = HashMap::new();
        backends.insert(
            "test_backend".to_string(),
            BackendConfig {
                url: "http://localhost:8080".to_string(),
                model_mapping: HashMap::new(),
                protocol: "openai".to_string(),
                max_tokens: Some(100),
                temperature: Some(0.7),
                temperature_override: None,
                weight: Some(1.0),
                api_key: None,
                tool_format: "json".to_string(),
                max_tokens_field: "max_tokens".to_string(),
                custom_instruction: Some("Custom instruction.".to_string()),
                custom_instruction_mode: "unknown_mode".to_string(), // Invalid mode should default to append
                capabilities: vec!["text".to_string()],
                priority: 1,
                fallback: None,
            },
        );

        let config = Config {
            backends,
            custom_instructions: None,
            performance: PerformanceConfig {
                enable_metrics: false,
                log_requests: false,
                timeout_seconds: 30,
                openai_token_counting_mode: None,
                allowed_tools: None,
                override_system_instruction: None,
                max_conversation_turns: None,
            },
        };

        let result = config.apply_custom_instruction("test_backend", Some("Original."));
        // Unknown modes should default to append behavior
        assert_eq!(result, Some("Original.\n\nCustom instruction.".to_string()));
    }
}