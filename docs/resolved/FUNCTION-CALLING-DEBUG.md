# Function Calling Debugging Guide

## Summary
Successfully implemented streaming function call support for bidirectional proxy (Gemini ↔ OpenAI API).

## The Problem
When using gemini-cli with llama-server through the proxy, function calls had empty arguments:
```
WriteFile {}  // ❌ Missing file_path and content parameters
```

## Root Causes Discovered

### 1. **Streaming Sends Partial JSON Chunks**
**Issue**: In OpenAI streaming, function call arguments arrive in fragments:
- Chunk 1: `{"`
- Chunk 2: `file_path`
- Chunk 3: `":`
- Chunk 4: `"test.txt"`
- Chunk 5: `","`
- etc.

**Why it fails**: Each chunk tried to parse incomplete JSON → parsing errors → empty `{}`

### 2. **ID Changes Across Chunks**
**Issue**: Each streaming chunk had a DIFFERENT id:
```
Chunk 1: id="eL537Ly3..." args="{\""
Chunk 2: id="call_0"    args="file_path"
Chunk 3: id="call_1"    args=":\""
```

**Why it fails**: Accumulator treated each chunk as a separate tool call instead of parts of the same call.

### 3. **Missing `reasoning_content` Field**
**Issue**: gpt-oss model returns a special field:
```json
{
  "reasoning_content": "We need to write a file...",
  "tool_calls": [...]
}
```

**Why it fails**: Proxy couldn't deserialize responses → "terminated" errors

## The Solution

### Core Fix: Stateful Chunk Accumulation with Index Tracking

**Key Insight**: OpenAI streaming uses `index` (not `id`) to identify which tool call each chunk belongs to.

```rust
// State accumulator
struct ToolCallAccumulator {
    // Map: index → (id, name, accumulated_arguments)
    calls: HashMap<usize, (String, String, String)>,
}
```

**Algorithm**:
1. **Accumulate**: Use `index` to track and concatenate argument chunks
   ```rust
   for tool_call in delta.tool_calls {
       let index = tool_call.index.unwrap_or(0);
       let entry = accumulator.calls.entry(index);
       entry.2.push_str(&tool_call.function.arguments); // Accumulate args
   }
   ```

2. **Detect completion**: Wait for `finish_reason` in final chunk
   ```rust
   if choice.finish_reason.is_some() {
       // Now we have complete arguments!
   }
   ```

3. **Emit complete function call**: Only when JSON is complete
   ```rust
   match serde_json::from_str::<Value>(&args) {
       Ok(args) if !args.is_empty() => {
           // Emit function call with complete args
       }
       _ => {
           // Skip incomplete chunks
       }
   }
   ```

## Implementation Details

### 1. Add `index` Field to StreamingToolCall
**File**: `src/models/openai.rs`

```rust
pub struct StreamingToolCall {
    pub index: Option<usize>,  // ← Added this
    pub id: Option<String>,
    pub tool_type: Option<String>,
    pub function: StreamingFunctionCall,
}
```

### 2. Add `reasoning_content` Field
**File**: `src/models/openai.rs`

```rust
pub struct ResponseMessage {
    pub role: String,
    pub content: Option<String>,
    pub tool_calls: Option<Vec<ToolCall>>,
    pub reasoning_content: Option<String>, // ← Added this
}

pub struct Delta {
    pub role: Option<String>,
    pub content: Option<String>,
    pub tool_calls: Option<Vec<StreamingToolCall>>,
    pub reasoning_content: Option<String>, // ← Added this
}
```

### 3. Implement Stateful Stream Processing
**File**: `src/conversion.rs`

**Using `scan` for state**:
```rust
stream.scan(ToolCallAccumulator::default(), |accumulator, result| {
    // Accumulate arguments across chunks
    for tool_call in tool_calls {
        let index = tool_call.index.unwrap_or(0);
        accumulator.calls.entry(index).or_insert(...);
        // Concatenate arguments
    }

    // On final chunk, build complete tool calls
    if finish_reason.is_some() {
        for (index, (id, name, args)) in &accumulator.calls {
            complete_calls.push(StreamingToolCall {
                index: Some(*index),
                id: Some(id.clone()),
                function: StreamingFunctionCall {
                    name: Some(name.clone()),
                    arguments: args.clone(), // Complete JSON!
                },
            });
        }
    }
})
```

### 4. Skip Incomplete Function Calls
**File**: `src/conversion.rs`

```rust
// Only emit function calls with valid, complete arguments
match serde_json::from_str::<Value>(&tool_call.arguments) {
    Ok(args) if args.is_object() && !args.is_empty() => {
        // Emit function call
    }
    _ => {
        // Skip incomplete chunks
    }
}
```

## Testing Strategy

### Direct llama-server Test
```bash
curl http://localhost:11211/v1/chat/completions \
  -H "Content-Type: application/json" \
  -d '{
    "model": "gpt-oss",
    "messages": [{"role": "user", "content": "Write test.txt"}],
    "tools": [{"type": "function", "function": {...}}]
  }'
```

**Expected**: Complete function call with arguments
```json
{
  "tool_calls": [{
    "function": {
      "name": "write_file",
      "arguments": "{\"file_path\":\"test.txt\",\"content\":\"hello\"}"
    }
  }]
}
```

### Through Proxy Test
```bash
curl -N http://localhost:8080/v1beta/models/gemma3-4ba:streamGenerateContent \
  -H "Content-Type: application/json" \
  -d '{
    "contents": [{"parts": [{"text": "Write test.txt"}], "role": "user"}],
    "tools": [{"functionDeclarations": [...]}]
  }'
```

**Expected**: Streaming response ending with complete function call

### With gemini-cli
```bash
GEMINI_DEFAULT_AUTH_TYPE=gemini-api-key \
GOOGLE_GEMINI_BASE_URL=http://localhost:8080 \
node ../gemini-cli/scripts/start.js --model gemma3-4ba
```

**Expected**: Function call executes successfully

## Debug Logging

Key log markers to look for:

```
📌 STREAM: Tool call [0] - id: xyz123          # ID captured
📞 STREAM: Tool call [0] - name: write_file    # Name captured
📝 STREAM: Tool call [0] - accumulated args: {"file_path":"test.txt",...}  # Args accumulating
✅ STREAM: Final chunk detected with finish_reason: Some("tool_calls")     # Complete!
🔧 STREAM: Building complete tool call [0] - id: xyz, name: write_file    # Building final
✅ STREAM: Successfully parsed complete args: {...}                        # Success!
```

## Common Issues and Solutions

### Issue: Empty Arguments `{}`
**Symptom**: `WriteFile {}`
**Cause**: Parsing incomplete JSON chunks
**Solution**: Use accumulator + skip incomplete chunks

### Issue: Multiple Undefined Tool Names
**Symptom**: `undefined_tool_name ","`, `undefined_tool_name {}`
**Cause**: Using `id` instead of `index` to track chunks
**Solution**: Use `index` field for chunk identification

### Issue: "terminated" Error
**Symptom**: `[API Error: terminated]`
**Cause**: Missing `reasoning_content` field in response models
**Solution**: Add `reasoning_content: Option<String>` to ResponseMessage and Delta

### Issue: "Empty reply from server"
**Symptom**: Proxy crashes when handling request
**Cause**: Unhandled deserialization error or missing field
**Solution**: Check logs for panic/error, add missing fields to models

## Performance Considerations

### Why Not Disable Streaming?
We considered forcing `stream: false` when tools are present, but this has downsides:
- Changes API behavior (client expects streaming)
- Loses streaming benefits (progressive responses)
- Requires complex non-streaming → streaming conversion

**Better approach**: Properly handle streaming with accumulation

### Memory Usage
The accumulator stores partial arguments in memory. For typical function calls (~1KB), this is negligible. For very large arguments, consider:
- Setting max argument size limits
- Streaming validation

## Future Enhancements

### 1. Multi-Format Parser (Already Implemented)
Supports alternative formats:
- XML-style: `<tool_call><function=name<parameter=key>value`
- JSON-in-text: `{"name": "...", "arguments": {...}}`

### 2. Validation
- Validate accumulated JSON before emitting
- Provide clear error messages for malformed arguments

### 3. Metrics
- Track accumulation success rate
- Monitor chunk counts per tool call
- Measure end-to-end latency

## Key Learnings

1. **Streaming is stateful**: Can't treat each chunk independently
2. **Use `index` not `id`**: IDs change across chunks, indices don't
3. **Wait for `finish_reason`**: Only emit complete data
4. **Model-specific fields**: Different models add custom fields (like `reasoning_content`)
5. **Test at all layers**: Direct backend, through proxy, with real client

## Files Modified

### Core Implementation
- `src/models/openai.rs`: Added `index` and `reasoning_content` fields
- `src/conversion.rs`: Implemented stateful accumulation with `scan`

### Configuration
- `llama-cpp-config.toml`: Backend configuration

### Documentation
- `DEBUGGING-NOTES.md`: Debug session notes
- `FUNCTION-CALLING-DEBUG.md`: This document

## Success Criteria

✅ Function calls have complete arguments
✅ No "empty {}" errors
✅ No "undefined_tool_name" errors
✅ No "terminated" errors
✅ Works with gemini-cli end-to-end
✅ Proper streaming behavior maintained

## Verification Command

```bash
# Start proxy
cargo run --bin louter -- --backend openai --port 8080 --config llama-cpp-config.toml

# Test with gemini-cli
GEMINI_DEFAULT_AUTH_TYPE=gemini-api-key \
GOOGLE_GEMINI_BASE_URL=http://localhost:8080 \
node ../gemini-cli/scripts/start.js --model gemma3-4ba

# Try: "Write a test.txt file with content Hello World"
# Expected: ✓ WriteFile - file created successfully
```

## Conclusion

The key to fixing streaming function calls was understanding that:
1. Arguments arrive in fragments that must be accumulated
2. The `index` field (not `id`) identifies which tool call chunks belong to
3. Complete arguments should only be emitted when `finish_reason` appears

This approach maintains streaming benefits while ensuring complete, valid function calls.
