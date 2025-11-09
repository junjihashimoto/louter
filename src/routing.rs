use crate::config::{Config, BackendConfig};
use crate::error::ProxyError;
use crate::models::gemini::{GeminiRequest, Part};
use crate::models::openai::{OpenAIRequest, Message, MessageContent, ContentPart};
use std::collections::HashSet;

/// Capability types that backends can support
#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub enum Capability {
    Text,
    Vision,
    Audio,
    Video,
    FunctionCalling,
}

impl Capability {
    /// Convert capability to string representation
    pub fn as_str(&self) -> &str {
        match self {
            Capability::Text => "text",
            Capability::Vision => "vision",
            Capability::Audio => "audio",
            Capability::Video => "video",
            Capability::FunctionCalling => "function_calling",
        }
    }

    /// Create capability from string
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "text" => Some(Capability::Text),
            "vision" => Some(Capability::Vision),
            "audio" => Some(Capability::Audio),
            "video" => Some(Capability::Video),
            "function_calling" => Some(Capability::FunctionCalling),
            _ => None,
        }
    }
}

/// Detect required capabilities from a Gemini request
pub fn detect_capabilities_gemini(request: &GeminiRequest) -> HashSet<Capability> {
    let mut caps = HashSet::new();
    caps.insert(Capability::Text); // Always need text capability

    // Check for vision (images), audio, and video
    for content in &request.contents {
        for part in &content.parts {
            match part {
                Part::InlineData { inline_data } => {
                    if inline_data.mime_type.starts_with("image/") {
                        caps.insert(Capability::Vision);
                    } else if inline_data.mime_type.starts_with("audio/") {
                        caps.insert(Capability::Audio);
                    } else if inline_data.mime_type.starts_with("video/") {
                        caps.insert(Capability::Video);
                    }
                },
                _ => {}
            }
        }
    }

    // Check for function calling
    if request.tools.is_some() {
        caps.insert(Capability::FunctionCalling);
    }

    caps
}

/// Detect required capabilities from an OpenAI request
pub fn detect_capabilities_openai(request: &OpenAIRequest) -> HashSet<Capability> {
    let mut caps = HashSet::new();
    caps.insert(Capability::Text); // Always need text capability

    // Check messages for multimodal content
    for message in &request.messages {
        match message {
            Message::User { content, .. } => {
                match content {
                    MessageContent::Array(parts) => {
                        for part in parts {
                            match part {
                                ContentPart::ImageUrl { .. } => {
                                    caps.insert(Capability::Vision);
                                },
                                _ => {}
                            }
                        }
                    },
                    MessageContent::Text(text) => {
                        // Check for base64 image data in text
                        if text.contains("data:image/") {
                            caps.insert(Capability::Vision);
                        }
                    }
                }
            },
            _ => {}
        }
    }

    // Check for function calling
    if request.tools.is_some() {
        caps.insert(Capability::FunctionCalling);
    }

    caps
}

/// Check if a backend supports all required capabilities
pub fn backend_supports_capabilities(backend: &BackendConfig, required: &HashSet<Capability>) -> bool {
    // Auto mode: empty capabilities means "accept all types"
    if backend.capabilities.is_empty() {
        return true;
    }

    // Check if backend has all required capabilities
    for cap in required {
        if !backend.capabilities.contains(&cap.as_str().to_string()) {
            return false;
        }
    }
    true
}

/// Select the best backend for a request based on capabilities and priority
/// Supports fallback: returns a list of backends ordered by priority
pub fn select_backends_for_request(
    required_caps: &HashSet<Capability>,
    config: &Config,
) -> Result<Vec<(String, BackendConfig)>, ProxyError> {
    // Find all backends that support the required capabilities
    let mut compatible_backends: Vec<(String, BackendConfig)> = config.backends
        .iter()
        .filter(|(_, backend)| backend_supports_capabilities(backend, required_caps))
        .map(|(name, backend)| (name.clone(), backend.clone()))
        .collect();

    if compatible_backends.is_empty() {
        let required_str: Vec<_> = required_caps.iter().map(|c| c.as_str()).collect();
        return Err(ProxyError::ConfigError(
            format!("No backend found with required capabilities: {:?}", required_str)
        ));
    }

    // Sort by priority (lower number = higher priority)
    compatible_backends.sort_by_key(|(_, backend)| backend.priority);

    Ok(compatible_backends)
}

/// Select the primary backend for a request (for simple cases without fallback)
pub fn select_backend_for_request(
    required_caps: &HashSet<Capability>,
    config: &Config,
) -> Result<(String, BackendConfig), ProxyError> {
    let backends = select_backends_for_request(required_caps, config)?;
    Ok(backends.into_iter().next().unwrap()) // Safe because we already checked for empty
}

/// Select backend with explicit fallback chain
/// Follows the fallback configuration to find a compatible backend
pub fn select_backend_with_fallback(
    backend_name: &str,
    required_caps: &HashSet<Capability>,
    config: &Config,
) -> Result<(String, BackendConfig), ProxyError> {
    let mut visited = HashSet::new(); // Prevent circular fallback loops
    let mut current_backend_name = backend_name.to_string();

    loop {
        // Prevent infinite loops
        if visited.contains(&current_backend_name) {
            return Err(ProxyError::ConfigError(
                format!("Circular fallback detected in backend chain: {}", current_backend_name)
            ));
        }
        visited.insert(current_backend_name.clone());

        // Get the current backend
        let current_backend = config.backends.get(&current_backend_name)
            .ok_or_else(|| ProxyError::ConfigError(
                format!("Backend '{}' not found in configuration", current_backend_name)
            ))?;

        // Check if this backend supports the required capabilities
        if backend_supports_capabilities(current_backend, required_caps) {
            return Ok((current_backend_name.clone(), current_backend.clone()));
        }

        // Backend doesn't support capabilities - try fallback
        if let Some(fallback_name) = &current_backend.fallback {
            current_backend_name = fallback_name.clone();
        } else {
            // No fallback configured - return error
            let required_str: Vec<_> = required_caps.iter().map(|c| c.as_str()).collect();
            return Err(ProxyError::ConfigError(
                format!(
                    "Backend '{}' doesn't support required capabilities {:?} and has no fallback configured",
                    backend_name,
                    required_str
                )
            ));
        }
    }
}

/// Find which backend a model belongs to by checking model_mapping
/// Returns the backend name if the model is found in any backend's model_mapping
pub fn find_backend_for_model(model: &str, config: &Config) -> Option<String> {
    for (backend_name, backend_config) in &config.backends {
        if backend_config.model_mapping.contains_key(model) {
            return Some(backend_name.clone());
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;
    use crate::config::{Config, PerformanceConfig};

    fn create_test_config() -> Config {
        let mut backends = HashMap::new();

        // Text-only backend (priority 1)
        backends.insert(
            "text-fast".to_string(),
            BackendConfig {
                url: "http://localhost:11211".to_string(),
                model_mapping: HashMap::new(),
                protocol: "openai".to_string(),
                max_tokens: Some(4096),
                temperature: Some(0.7),
                temperature_override: None,
                weight: Some(1.0),
                api_key: None,
                tool_format: "json".to_string(),
                max_tokens_field: "max_tokens".to_string(),
                custom_instruction: None,
                custom_instruction_mode: "append".to_string(),
                capabilities: vec!["text".to_string(), "function_calling".to_string()],
                priority: 1,
                fallback: None,
            },
        );

        // Vision-capable backend (priority 2)
        backends.insert(
            "vision-capable".to_string(),
            BackendConfig {
                url: "http://localhost:11212".to_string(),
                model_mapping: HashMap::new(),
                protocol: "openai".to_string(),
                max_tokens: Some(8192),
                temperature: Some(0.7),
                temperature_override: None,
                weight: Some(1.0),
                api_key: None,
                tool_format: "xml".to_string(),
                max_tokens_field: "max_tokens".to_string(),
                custom_instruction: None,
                custom_instruction_mode: "append".to_string(),
                capabilities: vec!["text".to_string(), "vision".to_string(), "function_calling".to_string()],
                priority: 2,
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
    fn test_text_only_selects_text_backend() {
        let config = create_test_config();
        let mut caps = HashSet::new();
        caps.insert(Capability::Text);

        let (name, _) = select_backend_for_request(&caps, &config).unwrap();
        assert_eq!(name, "text-fast");
    }

    #[test]
    fn test_vision_selects_vision_backend() {
        let config = create_test_config();
        let mut caps = HashSet::new();
        caps.insert(Capability::Text);
        caps.insert(Capability::Vision);

        let (name, _) = select_backend_for_request(&caps, &config).unwrap();
        assert_eq!(name, "vision-capable");
    }

    #[test]
    fn test_fallback_list_ordered_by_priority() {
        let config = create_test_config();
        let mut caps = HashSet::new();
        caps.insert(Capability::Text);
        caps.insert(Capability::FunctionCalling);

        let backends = select_backends_for_request(&caps, &config).unwrap();
        // Both backends support text + function_calling, so should get both ordered by priority
        assert_eq!(backends.len(), 2);
        assert_eq!(backends[0].0, "text-fast");  // Priority 1
        assert_eq!(backends[1].0, "vision-capable");  // Priority 2
    }

    #[test]
    fn test_auto_mode_accepts_all_capabilities() {
        let mut backends = HashMap::new();

        // Auto mode backend (empty capabilities)
        backends.insert(
            "auto-backend".to_string(),
            BackendConfig {
                url: "http://localhost:11213".to_string(),
                model_mapping: HashMap::new(),
                protocol: "openai".to_string(),
                max_tokens: Some(4096),
                temperature: Some(0.7),
                temperature_override: None,
                weight: Some(1.0),
                api_key: None,
                tool_format: "json".to_string(),
                max_tokens_field: "max_tokens".to_string(),
                custom_instruction: None,
                custom_instruction_mode: "append".to_string(),
                capabilities: vec![], // Empty = auto mode
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

        // Test that auto mode backend accepts text-only requests
        let mut caps = HashSet::new();
        caps.insert(Capability::Text);
        let (name, _) = select_backend_for_request(&caps, &config).unwrap();
        assert_eq!(name, "auto-backend");

        // Test that auto mode backend accepts vision requests
        let mut caps = HashSet::new();
        caps.insert(Capability::Text);
        caps.insert(Capability::Vision);
        let (name, _) = select_backend_for_request(&caps, &config).unwrap();
        assert_eq!(name, "auto-backend");

        // Test that auto mode backend accepts all capabilities
        let mut caps = HashSet::new();
        caps.insert(Capability::Text);
        caps.insert(Capability::Vision);
        caps.insert(Capability::Audio);
        caps.insert(Capability::FunctionCalling);
        let (name, _) = select_backend_for_request(&caps, &config).unwrap();
        assert_eq!(name, "auto-backend");
    }
}
