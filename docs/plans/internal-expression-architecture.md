# Internal Expression Architecture for Protocol Conversion

**Status:** 📋 PLANNED
**Priority:** HIGH
**Date:** 2025-11-24

## Problem Statement

Currently, the proxy uses **direct protocol mapping**:
```
Gemini Request → OpenAI Request → OpenAI Backend
OpenAI Response → Anthropic Response → Client
```

This approach has several issues:
1. **Tight coupling** between protocols
2. **Difficult to maintain** as protocols evolve
3. **Hard to add new protocols** (Claude, Cohere, Azure)
4. **Conversion logic scattered** across multiple functions
5. **Loss of information** in direct mapping (e.g., thinking tokens)

## Proposed Solution

Introduce an **Internal Expression (IR)** layer that decouples protocols:

```
Frontend Protocol → Internal Expression → Backend Protocol
      ↓                     ↓                    ↓
   Gemini              Universal IR         OpenAI
   OpenAI                  ↓               Gemini
   Anthropic               ↓               Anthropic
   Claude                  ↓               Claude
```

### Benefits

1. **Decoupling** - Each protocol only needs conversion to/from IR
2. **Maintainability** - Protocol changes only affect one converter
3. **Extensibility** - Adding new protocol = add one converter
4. **Information preservation** - IR can represent all protocol features
5. **Testing** - Can test each converter independently
6. **Logging** - Can log IR for debugging

## Architecture Design

### 1. Internal Expression Structure

```rust
// Core message structure
pub struct IRMessage {
    pub id: String,
    pub role: IRRole,
    pub content: Vec<IRContent>,
    pub metadata: IRMetadata,
}

pub enum IRRole {
    User,
    Assistant,
    System,
}

pub enum IRContent {
    Text { text: String },
    Image { source: IRImageSource, media_type: String },
    ToolUse { id: String, name: String, input: serde_json::Value },
    ToolResult { id: String, content: String, is_error: bool },
}

pub struct IRMetadata {
    pub model: String,
    pub stop_reason: Option<IRStopReason>,
    pub usage: IRUsage,
    pub thinking_tokens: Option<i32>,  // For Claude 2.5+ extended thinking
}

pub enum IRStopReason {
    EndTurn,
    MaxTokens,
    StopSequence,
    ToolUse,
}

pub struct IRUsage {
    pub input_tokens: i32,
    pub output_tokens: i32,
    pub cache_creation_tokens: Option<i32>,
    pub cache_read_tokens: Option<i32>,
}

// Streaming structure
pub struct IRStreamChunk {
    pub chunk_type: IRChunkType,
    pub message_id: String,
}

pub enum IRChunkType {
    MessageStart { message: IRMessage },
    ContentBlockStart { index: usize, content: IRContent },
    ContentBlockDelta { index: usize, delta: IRDelta },
    ContentBlockStop { index: usize },
    MessageDelta { stop_reason: Option<IRStopReason>, usage: IRUsage },
    MessageStop,
    Ping,
}

pub enum IRDelta {
    TextDelta { text: String },
    InputJsonDelta { partial_json: String },
}
```

### 2. Converter Traits

```rust
// Frontend converter: Protocol → IR
pub trait FrontendConverter {
    fn parse_request(&self, request: &[u8]) -> Result<IRRequest>;
    fn format_response(&self, ir_response: IRResponse) -> Result<Vec<u8>>;
    fn format_stream_chunk(&self, chunk: IRStreamChunk) -> Result<Vec<u8>>;
}

// Backend converter: IR → Protocol
pub trait BackendConverter {
    fn format_request(&self, ir_request: IRRequest) -> Result<Vec<u8>>;
    fn parse_response(&self, response: &[u8]) -> Result<IRResponse>;
    fn parse_stream_chunk(&self, chunk: &[u8]) -> Result<Option<IRStreamChunk>>;
}
```

### 3. Converter Implementations

```rust
// Anthropic (Claude) Frontend
pub struct AnthropicFrontendConverter;
impl FrontendConverter for AnthropicFrontendConverter {
    fn parse_request(&self, request: &[u8]) -> Result<IRRequest> {
        // Parse Anthropic JSON → IR
    }

    fn format_response(&self, ir_response: IRResponse) -> Result<Vec<u8>> {
        // IR → Anthropic JSON
    }

    fn format_stream_chunk(&self, chunk: IRStreamChunk) -> Result<Vec<u8>> {
        // IR chunk → Anthropic SSE event
    }
}

// OpenAI Backend
pub struct OpenAIBackendConverter;
impl BackendConverter for OpenAIBackendConverter {
    fn format_request(&self, ir_request: IRRequest) -> Result<Vec<u8>> {
        // IR → OpenAI JSON
    }

    fn parse_response(&self, response: &[u8]) -> Result<IRResponse> {
        // OpenAI JSON → IR
    }

    fn parse_stream_chunk(&self, chunk: &[u8]) -> Result<Option<IRStreamChunk>> {
        // OpenAI SSE event → IR chunk
    }
}

// Gemini Frontend
pub struct GeminiFrontendConverter;
// Gemini Backend
pub struct GeminiBackendConverter;

// Future: Claude Backend, Cohere, etc.
```

### 4. Main Request Flow

```rust
pub async fn handle_request(
    frontend_protocol: &str,
    backend_protocol: &str,
    request: Vec<u8>,
) -> Result<Response> {
    // 1. Get converters
    let frontend_converter = get_frontend_converter(frontend_protocol)?;
    let backend_converter = get_backend_converter(backend_protocol)?;

    // 2. Parse request to IR
    let ir_request = frontend_converter.parse_request(&request)?;

    // 3. Convert IR to backend format
    let backend_request = backend_converter.format_request(ir_request)?;

    // 4. Call backend
    let backend_response = call_backend(backend_request).await?;

    // 5. Parse backend response to IR
    let ir_response = backend_converter.parse_response(&backend_response)?;

    // 6. Convert IR to frontend format
    let frontend_response = frontend_converter.format_response(ir_response)?;

    Ok(frontend_response)
}
```

## File Structure

```
src/
├── ir/
│   ├── mod.rs              # IR types and traits
│   ├── message.rs          # IRMessage, IRContent, etc.
│   ├── streaming.rs        # IRStreamChunk, IRDelta
│   ├── usage.rs            # IRUsage, IRStopReason
│   └── converter.rs        # Converter traits
│
├── converters/
│   ├── mod.rs              # Converter registry
│   │
│   ├── anthropic/
│   │   ├── mod.rs
│   │   ├── frontend.rs     # AnthropicFrontendConverter
│   │   └── backend.rs      # AnthropicBackendConverter (future)
│   │
│   ├── openai/
│   │   ├── mod.rs
│   │   ├── frontend.rs     # OpenAIFrontendConverter
│   │   └── backend.rs      # OpenAIBackendConverter
│   │
│   └── gemini/
│       ├── mod.rs
│       ├── frontend.rs     # GeminiFrontendConverter
│       └── backend.rs      # GeminiBackendConverter
│
├── models/                 # Keep for protocol-specific types
│   ├── anthropic.rs
│   ├── openai.rs
│   └── gemini.rs
│
├── conversion.rs           # DEPRECATED - to be replaced
├── main.rs                 # Update to use converters
└── config.rs               # Add frontend_protocol field
```

## Migration Plan

### Phase 1: Create IR Types (Week 1)
- [ ] Define `IRMessage`, `IRContent`, `IRRole`
- [ ] Define `IRStreamChunk`, `IRDelta`
- [ ] Define `IRUsage`, `IRStopReason`, `IRMetadata`
- [ ] Define converter traits
- [ ] Write unit tests for IR types

### Phase 2: Implement Converters (Week 2-3)
- [ ] Implement `AnthropicFrontendConverter`
  - [ ] Request parsing
  - [ ] Response formatting
  - [ ] Stream chunk formatting
- [ ] Implement `OpenAIBackendConverter`
  - [ ] Request formatting
  - [ ] Response parsing
  - [ ] Stream chunk parsing
- [ ] Write unit tests for each converter
- [ ] Integration tests (IR round-trip)

### Phase 3: Update Main Logic (Week 4)
- [ ] Update `main.rs` to use converters
- [ ] Update routing to use converter registry
- [ ] Deprecate old `conversion.rs` functions
- [ ] Run existing test suite (should still pass)

### Phase 4: Add Remaining Converters (Week 5-6)
- [ ] Implement `OpenAIFrontendConverter`
- [ ] Implement `GeminiFrontendConverter`
- [ ] Implement `GeminiBackendConverter`
- [ ] Update all routes to use converters

### Phase 5: Cleanup (Week 7)
- [ ] Remove deprecated `conversion.rs`
- [ ] Remove unused model types
- [ ] Update documentation
- [ ] Performance optimization

## Testing Strategy

### Unit Tests
- Test each converter independently
- Test IR → Protocol conversion
- Test Protocol → IR conversion
- Test round-trip (Protocol → IR → Protocol)

### Integration Tests
- Test full request flow with converters
- Test streaming with converters
- Test tool calls with converters
- Test error handling

### Regression Tests
- Ensure existing test suite still passes
- Ensure Anthropic SDK tests still pass (14/14)
- Ensure Claude Code works (after fixing)

## Performance Considerations

### Potential Overhead
- Extra serialization/deserialization step
- Memory allocation for IR types

### Optimizations
- Use `serde_json::RawValue` for passthrough fields
- Implement `Clone` only where needed
- Use references in converters where possible
- Stream IR chunks without buffering

### Benchmarks
- Measure latency overhead (target: <5ms)
- Measure throughput impact (target: <10%)
- Measure memory overhead (target: <1MB per request)

## Benefits After Migration

### For Developers
- **Clearer code structure** - One file per protocol
- **Easier to test** - Test converters independently
- **Easier to debug** - Can inspect IR between conversions
- **Easier to add features** - Add to IR, update converters

### For Users
- **More protocols** - Can add Claude, Cohere, Azure easily
- **Better error messages** - IR validation can catch issues
- **Better logging** - Can log IR for debugging
- **Consistent behavior** - All protocols use same IR

### For Maintenance
- **Protocol updates** - Only update relevant converter
- **Breaking changes** - Isolated to one converter
- **Testing** - Independent converter tests
- **Documentation** - Each converter self-contained

## Risks and Mitigation

### Risk 1: Performance Regression
**Mitigation:** Benchmark before/after, optimize hot paths

### Risk 2: Breaking Changes During Migration
**Mitigation:** Keep old code alongside new, gradual migration

### Risk 3: IR Doesn't Capture All Features
**Mitigation:** Design IR to be extensible with `metadata: HashMap`

### Risk 4: Increased Complexity
**Mitigation:** Clear documentation, strong typing, good tests

## Success Criteria

- [ ] All existing tests pass with new architecture
- [ ] New architecture easier to understand
- [ ] Adding new protocol takes <1 day
- [ ] Performance overhead <5%
- [ ] Code coverage >80%
- [ ] Documentation complete

## Dependencies

### Must Complete First
1. **Fix Claude Code integration** - Need working baseline to test against
2. **Document current behavior** - Need to ensure compatibility

### Blocks
1. Adding new protocols (Claude, Cohere)
2. Advanced features (extended thinking, batch API)
3. Performance optimizations

## References

- **LLVM IR** - Inspiration for intermediate representation design
- **gRPC** - Protocol-agnostic message format
- **GraphQL** - Internal schema for external APIs
- **Babel** - Plugin architecture for transformations
