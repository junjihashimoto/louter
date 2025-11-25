// Internal Representation (IR) Module
//
// This module defines the intermediate representation for converting between
// different LLM API protocols (Anthropic, OpenAI, Gemini).
//
// Architecture:
// - IR types represent the universal concepts (messages, content, tools, etc.)
// - Converter traits define protocol-specific conversion logic
// - Each protocol has Frontend and Backend converters

pub mod types;
pub mod traits;
pub mod converters;

pub use types::*;
pub use traits::*;
pub use converters::*;
