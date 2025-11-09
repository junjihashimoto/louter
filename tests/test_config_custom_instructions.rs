use louter::config::{Config, BackendConfig, PerformanceConfig};
use std::collections::HashMap;

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
    // Test that unknown modes default to append behavior
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
