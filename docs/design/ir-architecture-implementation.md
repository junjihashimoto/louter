# Internal Representation (IR) Architecture - Implementation

**Status:** ✅ Foundation Complete
**Date:** 2025-11-25

## Overview

Implemented the Internal Representation (IR) architecture for protocol conversion. This creates a universal intermediate format for converting between Anthropic, OpenAI, and Gemini APIs.

## Architecture

```
┌─────────────┐     ┌──────────────┐     ┌─────────────┐
│  Anthropic  │────▶│              │────▶│   OpenAI    │
│   Client    │     │              │     │   Backend   │
└─────────────┘     │              │     └─────────────┘
                    │              │
┌─────────────┐     │      IR      │     ┌─────────────┐
│   OpenAI    │────▶│              │────▶│   Gemini    │
│   Client    │     │  (Universal  │     │   Backend   │
└─────────────┘     │   Format)    │     └─────────────┘
                    │              │
┌─────────────┐     │              │     ┌─────────────┐
│   Gemini    │────▶│              │────▶│  Anthropic  │
│   Client    │     │              │     │   Backend   │
└─────────────┘     └──────────────┘     └─────────────┘

Frontend Converters        IR Types         Backend Converters
(Protocol → IR)         (Universal)         (IR → Protocol)
```

## Files Created

### 1. `src/ir/mod.rs`
Module entry point, exports types and traits.

### 2. `src/ir/types.rs` (483 lines)
Core IR type definitions:

#### Request/Response Types
- **`IRRequest`** - Universal request format
  - `model`, `messages`, `system`, `max_tokens`
  - `temperature`, `top_p`, `top_k`
  - `tools`, `tool_choice`, `stream`
  - `metadata` for protocol-specific fields

- **`IRResponse`** - Universal response format
  - `id`, `model`, `role`, `content`
  - `stop_reason`, `usage`, `metadata`

- **`IRMessage`** - Single conversation message
  - `role` (user/assistant/system)
  - `content` (array of content blocks)
  - `name` (optional)

#### Content Types
- **`IRContent`** - Content block enum
  - `Text` - Plain text
  - `Image` - Image with URL or base64
  - `Audio` - Audio content
  - `Video` - Video content
  - `Document` - PDF/document content
  - `ToolUse` - Function call
  - `ToolResult` - Function result
  - `Thinking` - Extended thinking (Gemini 2.5+)

#### Tool Types
- **`IRTool`** - Tool/function definition
- **`IRToolChoice`** - Tool selection mode (auto/required/none/specific)

#### Streaming Types
- **`IRStreamChunk`** - SSE event wrapper
- **`IRChunkType`** - Event type enum
  - `MessageStart`, `ContentBlockStart`, `ContentBlockDelta`
  - `ContentBlockStop`, `MessageDelta`, `MessageStop`
  - `Ping`, `Error`

- **`IRContentBlockStart`** - Block start types
- **`IRDelta`** - Incremental updates
- **`IRMessageDelta`** - Final message updates

#### Supporting Types
- **`IRRole`** - Message sender role
- **`IRStopReason`** - Generation stop reason
- **`IRUsage`** - Token usage tracking
- **`IRImageSource`** / `IRAudioSource` / etc. - Media sources
- **`IRRequestMetadata`** / **`IRResponseMetadata`** - Protocol-specific metadata

### 3. `src/ir/traits.rs` (168 lines)
Converter trait definitions:

#### Frontend Converter Trait
```rust
#[async_trait]
pub trait FrontendConverter: Send + Sync {
    fn protocol_name(&self) -> &str;
    async fn parse_request(&self, request_bytes: &[u8]) -> ConverterResult<IRRequest>;
    async fn format_response(&self, ir_response: &IRResponse) -> ConverterResult<Vec<u8>>;
    fn format_stream_chunk(&self, chunk: &IRStreamChunk) -> ConverterResult<String>;
    fn validate_request(&self, request: &IRRequest) -> ConverterResult<()>;
}
```

**Purpose:** Convert client requests from protocol-specific format to IR

**Implementations needed:**
- `AnthropicFrontendConverter` - Anthropic API → IR
- `OpenAIFrontendConverter` - OpenAI API → IR
- `GeminiFrontendConverter` - Gemini API → IR

#### Backend Converter Trait
```rust
#[async_trait]
pub trait BackendConverter: Send + Sync {
    fn protocol_name(&self) -> &str;
    async fn format_request(&self, ir_request: &IRRequest) -> ConverterResult<Vec<u8>>;
    async fn parse_response(&self, response_bytes: &[u8]) -> ConverterResult<IRResponse>;
    fn parse_stream_chunk(&self, event_data: &[u8]) -> ConverterResult<Option<IRStreamChunk>>;
    fn required_headers(&self, api_key: &str) -> Vec<(String, String)>;
    fn endpoint_url(&self, base_url: &str, streaming: bool) -> String;
}
```

**Purpose:** Convert IR to backend-specific format and parse responses

**Implementations needed:**
- `AnthropicBackendConverter` - IR → Anthropic API
- `OpenAIBackendConverter` - IR → OpenAI API
- `GeminiBackendConverter` - IR → Gemini API

#### Helper Functions
- `convert_stream_to_ir()` - Convert protocol stream to IR chunks
- `convert_ir_to_stream()` - Convert IR chunks to protocol stream

## Design Decisions

### 1. Protocol-Agnostic Types
IR types represent universal concepts, not tied to any specific protocol:
- ✅ **Anthropic** → `IRContent::ToolUse` → **OpenAI** `tool_calls`
- ✅ **OpenAI** `messages` → `IRMessage` → **Gemini** `contents`
- ✅ **Gemini** `thinkingTokenCount` → `IRUsage::thinking_tokens` → **Anthropic** (not exposed)

### 2. Metadata for Protocol-Specific Fields
Each protocol has unique fields that don't map cleanly:
- `IRRequestMetadata::extra` - HashMap for arbitrary fields
- `IRResponseMetadata::extra` - Preserves protocol-specific info
- Example: `anthropic_version`, `user_id`, `created_at`

### 3. Comprehensive Content Support
IR supports all media types:
- Text, Image, Audio, Video, Document
- Tool use/results
- Thinking tokens (Gemini 2.5+)

### 4. Streaming First-Class Support
Streaming is built into the IR:
- `IRStreamChunk` represents SSE events
- `IRChunkType` covers all event types
- Stream conversion helpers included

### 5. Async Traits
Converters use `async_trait` for flexibility:
- Non-blocking I/O
- Future-proof for async backends
- Compatible with tokio/axum

### 6. Arc-based Stream Conversion
Stream helpers use `Arc<Converter>` for thread-safe sharing:
- Avoids move/borrow issues in closures
- Enables converter reuse across stream items
- Zero-cost at runtime (reference counting)

## Benefits

### 1. Maintainability
- **Single source of truth** - IR defines all concepts
- **Easier debugging** - Clear conversion boundaries
- **Testable** - Each converter independently testable

### 2. Extensibility
- **New protocols** - Add new Frontend/Backend converters
- **New features** - Extend IR types, converters adapt
- **Protocol evolution** - IR insulates from API changes

### 3. Flexibility
- **Any-to-any conversion** - 3×3=9 protocol combinations
- **Mix and match** - Anthropic client → Gemini backend
- **Future-proof** - Easy to add Claude, Cohere, etc.

### 4. Type Safety
- **Compile-time checks** - Rust's type system enforces correctness
- **No JSON hacks** - Proper types for all fields
- **Clear contracts** - Traits define exact behavior

## Implementation Status

### ✅ Completed
- [x] IR type definitions (`src/ir/types.rs`)
- [x] Converter traits (`src/ir/traits.rs`)
- [x] Module structure (`src/ir/mod.rs`)
- [x] Stream conversion helpers
- [x] Library compilation verified

### 📋 Next Steps
1. **Implement AnthropicFrontendConverter**
   - Parse Anthropic requests to IR
   - Format IR responses to Anthropic
   - Handle streaming events

2. **Implement OpenAIBackendConverter**
   - Format IR requests to OpenAI
   - Parse OpenAI responses to IR
   - Handle streaming responses

3. **Write unit tests**
   - Test type serialization/deserialization
   - Test converter implementations
   - Test streaming conversion

4. **Integrate into main.rs**
   - Replace direct conversion calls
   - Use converter traits
   - Maintain backwards compatibility

5. **Add remaining converters**
   - OpenAI frontend
   - Gemini frontend/backend
   - Anthropic backend

6. **Deprecate conversion.rs**
   - Mark as deprecated
   - Add migration notes
   - Remove after full migration

## Usage Example (Future)

Once converters are implemented:

```rust
// Frontend: Parse Anthropic request to IR
let anthropic_frontend = AnthropicFrontendConverter::new();
let ir_request = anthropic_frontend.parse_request(&request_bytes).await?;

// Backend: Convert IR to OpenAI request
let openai_backend = OpenAIBackendConverter::new();
let backend_request = openai_backend.format_request(&ir_request).await?;

// Send to backend, get response
let backend_response = send_to_backend(backend_request).await?;

// Backend: Parse OpenAI response to IR
let ir_response = openai_backend.parse_response(&backend_response).await?;

// Frontend: Format IR response as Anthropic
let response_bytes = anthropic_frontend.format_response(&ir_response).await?;
```

## Streaming Example (Future)

```rust
// Backend stream to IR
let ir_stream = convert_stream_to_ir(
    backend_stream,
    Arc::new(openai_backend),
);

// IR stream to frontend format
let sse_stream = convert_ir_to_stream(
    ir_stream,
    Arc::new(anthropic_frontend),
);
```

## Testing Strategy

### Unit Tests
- Serialize/deserialize IR types
- Convert protocol → IR → protocol (round-trip)
- Validate required fields
- Test edge cases (empty content, multiple tools, etc.)

### Integration Tests
- Full request flow through converters
- Streaming conversion end-to-end
- Error handling and recovery
- Performance benchmarks

### Regression Tests
- Existing 14/14 Anthropic streaming tests
- OpenAI compatibility tests
- Gemini protocol tests

## Migration Plan

### Phase 1: Anthropic Frontend (Week 1)
- Implement `AnthropicFrontendConverter`
- Test with existing Anthropic tests
- Ensure 14/14 tests still pass

### Phase 2: OpenAI Backend (Week 2)
- Implement `OpenAIBackendConverter`
- Replace direct OpenAI conversion calls
- Test Anthropic → OpenAI flow

### Phase 3: Integration (Week 3)
- Update `main.rs` to use converters
- Add converter factory
- Maintain backwards compatibility

### Phase 4: Additional Converters (Week 4-5)
- OpenAI frontend
- Gemini frontend/backend
- Anthropic backend

### Phase 5: Cleanup (Week 6)
- Deprecate `conversion.rs`
- Update documentation
- Performance optimization

## Related Documentation

- **Architecture Plan:** `docs/plans/internal-expression-architecture.md`
- **Original Issue:** `docs/issues/claude-code-integration-issue.md`
- **Session Summary:** `docs/resolved/2025-11-25-session-summary.md`

## Files in This Implementation

1. **`src/ir/mod.rs`** - Module entry point
2. **`src/ir/types.rs`** - IR type definitions (483 lines)
3. **`src/ir/traits.rs`** - Converter traits (168 lines)
4. **`src/lib.rs`** - Added `pub mod ir`

**Total:** 651+ lines of new code

## Success Criteria

- [x] IR types compile successfully
- [x] Converter traits well-defined
- [x] Stream helpers implemented
- [x] Library builds without errors
- [ ] First converter implemented (next)
- [ ] Tests pass with converter
- [ ] Documentation complete
- [ ] Migration path clear

## Conclusion

The IR architecture foundation is complete and ready for converter implementations. The design provides:

- **Clean separation** between protocols
- **Type-safe** conversions
- **Extensible** for new protocols
- **Testable** at every layer
- **Future-proof** architecture

Next step: Implement the first converter (AnthropicFrontendConverter) to validate the design with real-world usage.
