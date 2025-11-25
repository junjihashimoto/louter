# Converter Implementation Status

**Date:** 2025-11-25
**Status:** In Progress

## Overview

Implementing the IR (Internal Representation) architecture with protocol converters for bidirectional API translation.

## Completed ✅

### 1. IR Types (`src/ir/types.rs`) - 483 lines
- ✅ Universal request/response types
- ✅ Message and content types
- ✅ Tool/function calling support
- ✅ Streaming event types
- ✅ Multi-media support (images, audio, video, documents)
- ✅ Thinking tokens support (Gemini 2.5+)

### 2. Converter Traits (`src/ir/traits.rs`) - 168 lines
- ✅ `FrontendConverter` trait
- ✅ `BackendConverter` trait
- ✅ Stream conversion helpers
- ✅ Async trait support

### 3. AnthropicFrontendConverter (`src/ir/converters/anthropic_frontend.rs`) - 360+ lines
- ✅ Parse Anthropic requests to IR
- ✅ Format IR responses to Anthropic
- ✅ Format IR stream chunks to SSE
- ✅ Handle system prompts (text and blocks)
- ✅ Convert tools and tool choice
- ✅ Support images, text, tool use/results
- ✅ **Compiles successfully**

## In Progress 🔄

### 4. OpenAIBackendConverter (`src/ir/converters/openai_backend.rs`)
- ✅ Initial implementation (460+ lines)
- 🔄 **Type mismatches** - Need to fix:
  - `Message` enum variants (System, User, Assistant, Tool)
  - `MessageContent::Array` instead of `MessageContent::Parts`
  - `Tool` uses `tool_type` and `function` fields
  - `ToolChoice` is String or Object, not enum variants
  - `ToolCall` uses `tool_type` and `function` fields
  - `OpenAIStreamResponse` instead of `ChatCompletionChunk`
  - `StreamChoice` and `Delta` for streaming

**Fixes needed:**
1. Update message conversion to handle enum variants properly
2. Fix tool/function structure (use Function type, not ToolFunction)
3. Handle ToolChoice as String("auto"/"none"/"required") or Object
4. Use correct streaming types (StreamingToolCall, StreamingFunctionCall)
5. Update response parsing for OpenAIResponse structure

## Implementation Plan

### Phase 1: Fix OpenAI Backend Converter (Current)
**Tasks:**
1. Read OpenAI model definitions completely
2. Update all type references in openai_backend.rs
3. Fix message conversion functions
4. Fix tool/function conversion
5. Fix streaming chunk parsing
6. Test compilation

**Files to modify:**
- `src/ir/converters/openai_backend.rs`

### Phase 2: Create Simple Test
**Purpose:** Validate the conversion flow end-to-end

```rust
// Test: Anthropic request → IR → OpenAI request → OpenAI response → IR → Anthropic response
#[tokio::test]
async fn test_anthropic_to_openai_flow() {
    // Create converters
    let frontend = AnthropicFrontendConverter::new();
    let backend = OpenAIBackendConverter::new();

    // Parse Anthropic request
    let anthropic_request = r#"{"model":"claude-3-haiku","messages":[{"role":"user","content":"Hello"}],"max_tokens":100}"#;
    let ir_request = frontend.parse_request(anthropic_request.as_bytes()).await.unwrap();

    // Convert to OpenAI
    let openai_request = backend.format_request(&ir_request).await.unwrap();

    // Verify OpenAI request structure
    let openai_req: OpenAIRequest = serde_json::from_slice(&openai_request).unwrap();
    assert_eq!(openai_req.model, "claude-3-haiku");
    assert_eq!(openai_req.messages.len(), 1);
}
```

### Phase 3: Integration into main.rs
**Replace existing conversion code with converters:**

```rust
// OLD CODE (in handle_anthropic_messages):
let openai_request = conversion::anthropic_to_openai_request(request.clone(), &selected_backend, &state.config)?;

// NEW CODE:
let frontend = AnthropicFrontendConverter::new();
let backend = OpenAIBackendConverter::new();

// Parse to IR
let request_bytes = serde_json::to_vec(&request)?;
let ir_request = frontend.parse_request(&request_bytes).await?;

// Convert to backend format
let backend_request = backend.format_request(&ir_request).await?;
```

### Phase 4: Add More Converters
1. **OpenAIFrontendConverter** - Parse OpenAI requests
2. **GeminiFrontendConverter** - Parse Gemini requests
3. **GeminiBackendConverter** - Format Gemini requests
4. **AnthropicBackendConverter** - Format Anthropic requests

### Phase 5: Cleanup
1. Mark `conversion.rs` as deprecated
2. Remove direct conversion calls
3. Update documentation
4. Performance testing

## Type Mapping Reference

### Anthropic → IR → OpenAI

| Anthropic | IR | OpenAI |
|-----------|-----|---------|
| `Message { role, content: MessageContent }` | `IRMessage { role, content: Vec<IRContent> }` | `Message::User/Assistant { role, content }` |
| `ContentBlock::Text` | `IRContent::Text` | `MessageContent::Text` or `ContentPart::Text` |
| `ContentBlock::Image` | `IRContent::Image` | `ContentPart::ImageUrl` |
| `ContentBlock::ToolUse` | `IRContent::ToolUse` | `ToolCall { function }` in assistant message |
| `ContentBlock::ToolResult` | `IRContent::ToolResult` | `Message::Tool { content, tool_call_id }` |
| `Tool { name, description, input_schema }` | `IRTool` | `Tool { tool_type: "function", function: Function }` |
| `ToolChoice::Auto/Any/Tool` | `IRToolChoice` | `ToolChoice::String("auto"/"required")` or `Object` |
| `stop_reason: "end_turn"` | `IRStopReason::EndTurn` | `finish_reason: "stop"` |
| `stop_reason: "max_tokens"` | `IRStopReason::MaxTokens` | `finish_reason: "length"` |
| `stop_reason: "tool_use"` | `IRStopReason::ToolUse` | `finish_reason: "tool_calls"` |

### OpenAI Message Structure

```rust
enum Message {
    System { role, content: String },
    User { role, content: MessageContent },
    Assistant { role, content: Option<String>, tool_calls: Option<Vec<ToolCall>> },
    Tool { role, content: String, tool_call_id: String },
}

enum MessageContent {
    Text(String),
    Array(Vec<ContentPart>),  // NOT "Parts"
}

enum ContentPart {
    Text { text },
    ImageUrl { image_url },
}
```

### OpenAI Tool Structure

```rust
struct Tool {
    tool_type: String,  // "function", NOT r#type
    function: Function,
}

struct Function {
    name: String,
    description: Option<String>,
    parameters: Option<Value>,  // NOT input_schema
}

enum ToolChoice {
    String(String),  // "auto", "none", "required"
    Object { r#type: String, function: FunctionChoice },
}
```

## Testing Strategy

### Unit Tests
- [ ] Test IR type serialization/deserialization
- [ ] Test Anthropic request parsing
- [ ] Test Anthropic response formatting
- [ ] Test OpenAI request formatting
- [ ] Test OpenAI response parsing
- [ ] Test streaming conversions

### Integration Tests
- [ ] Anthropic → OpenAI full flow
- [ ] OpenAI → Anthropic full flow
- [ ] Tool calling conversion
- [ ] Multi-modal content (images)
- [ ] Streaming end-to-end

### Regression Tests
- [ ] Run existing 14/14 Anthropic streaming tests
- [ ] Verify backwards compatibility
- [ ] Performance benchmarks

## Current Status Summary

**Lines of Code:**
- IR types: 483 lines ✅
- Converter traits: 168 lines ✅
- Anthropic frontend: 360+ lines ✅
- OpenAI backend: 460+ lines 🔄 (needs type fixes)
- **Total: ~1,470 lines**

**Build Status:**
- Library with IR types: ✅ Compiles
- Anthropic frontend converter: ✅ Compiles
- OpenAI backend converter: ❌ Type mismatches (in progress)

**Next Step:**
Fix OpenAI backend converter type mismatches to match actual OpenAI model structure.

## Related Documentation
- **IR Architecture:** `docs/design/ir-architecture-implementation.md`
- **Architecture Plan:** `docs/plans/internal-expression-architecture.md`
- **Session Summary:** `docs/resolved/2025-11-25-session-summary.md`
