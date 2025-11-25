// Protocol Converters
//
// Implementations of FrontendConverter and BackendConverter traits
// for different LLM API protocols

pub mod anthropic_frontend;

pub use anthropic_frontend::AnthropicFrontendConverter;
