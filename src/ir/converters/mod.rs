// Protocol Converters
//
// Implementations of FrontendConverter and BackendConverter traits
// for different LLM API protocols

pub mod anthropic_frontend;
pub mod openai_backend;

pub use anthropic_frontend::AnthropicFrontendConverter;
pub use openai_backend::OpenAIBackendConverter;
