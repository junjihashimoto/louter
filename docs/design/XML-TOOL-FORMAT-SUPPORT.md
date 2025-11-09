# XML Tool Format Support for Qwen3-Coder

**Status**: ✅ Implemented (Non-streaming)
**Date**: 2025-11-19

## Overview

Added support for models that output tool calls in XML format (like Qwen3-Coder) while maintaining backward compatibility with JSON format models (like gpt-oss).

## Architecture

### Tool Format Configuration

Each backend can specify its tool format via `tool_format` field in config:

```toml
[backends.gpt-oss]
url = "http://localhost:11211"
tool_format = "json"  # Standard OpenAI JSON format

[backends.qwen]
url = "http://localhost:11212"
tool_format = "xml"   # Qwen3-Coder XML format
```

### XML Tool Call Format (Qwen3-Coder)

**Input**: JSON tools via OpenAI API (llama-server with `--jinja` handles conversion)

**Output**: XML format in model response:
```xml
<tool_call>
  <function=WriteFile>
    <parameter=file_path>
    touch.yaml
    </parameter>
    <parameter=content>
    file content here
    </parameter>
  </function>
</tool_call>
```

### Conversion Flow

```
Model Response (XML) → Backend → XML Parser → JSON Tool Calls → Gemini/OpenAI Format
```

## Implementation Details

### 1. Config Changes (src/config.rs)

Added `tool_format` field to `BackendConfig`:

```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BackendConfig {
    pub url: String,
    pub model_mapping: HashMap<String, String>,
    pub max_tokens: Option<i32>,
    pub temperature: Option<f64>,
    pub weight: Option<f64>,
    pub api_key: Option<String>,
    #[serde(default = "default_tool_format")]
    pub tool_format: String,  // "json" or "xml"
}

fn default_tool_format() -> String {
    "json".to_string()  // Default to JSON for backward compatibility
}
```

### 2. XML Parser (src/conversion.rs)

**parse_xml_tool_calls()**: Extracts tool calls from XML text

```rust
fn parse_xml_tool_calls(text: &str) -> Vec<(String, serde_json::Value)> {
    // Regex patterns:
    // - <tool_call>...</tool_call> - wrapper
    // - <function=NAME> - function name
    // - <parameter=NAME>VALUE</parameter> - parameters

    // Returns: Vec<(function_name, args_as_json)>
}
```

**extract_and_convert_xml_tool_calls()**: Public API for conversion

```rust
pub fn extract_and_convert_xml_tool_calls(content: &str)
    -> (String, Vec<openai::ToolCall>)
{
    // 1. Parse XML tool calls
    // 2. Remove XML from text content
    // 3. Convert to OpenAI ToolCall format
    // Returns: (cleaned_text, tool_calls)
}
```

### 3. Backend Integration (src/backends.rs)

**Non-streaming**:

```rust
let mut openai_response: OpenAIResponse = response.json().await?;

// If backend uses XML tool format, convert XML tool calls to JSON
if backend_config.tool_format == "xml" {
    for choice in &mut openai_response.choices {
        if let Some(ref content) = choice.message.content {
            let (cleaned_text, tool_calls) =
                crate::conversion::extract_and_convert_xml_tool_calls(content);

            if !tool_calls.is_empty() {
                choice.message.content =
                    if cleaned_text.is_empty() { None } else { Some(cleaned_text) };
                choice.message.tool_calls = Some(tool_calls);
                choice.finish_reason = Some("tool_calls".to_string());
            }
        }
    }
}
```

**Streaming**: Not yet implemented (see Future Work)

## Configuration Example

### dual-backend-config.toml

```toml
# Backend 1: gpt-oss (JSON format)
[backends.gpt-oss]
url = "http://localhost:11211"
tool_format = "json"

[backends.gpt-oss.model_mapping]
"gpt-oss-20b" = "gpt-oss:20b"
"gemini-flash" = "gpt-oss:20b"

# Backend 2: Qwen3-Coder (XML format)
[backends.qwen]
url = "http://localhost:11212"
tool_format = "xml"

[backends.qwen.model_mapping]
"qwen3-coder" = "Qwen3-Coder-30B-A3B-Instruct"
"gemini-pro" = "Qwen3-Coder-30B-A3B-Instruct"
```

## Usage

### Starting Both Backends

```bash
# Terminal 1: gpt-oss on port 11211
cd ~/git/llama.cpp/build/
./bin/llama-server -m ./gpt-oss-20b.gguf --port 11211 -c 65536

# Terminal 2: Qwen3-Coder on port 11212
cd ~/git/llama.cpp/build/
./bin/llama-server -m ./Qwen3-Coder-30B-A3B-Instruct-UD-Q4_K_XL.gguf \
  --port 11212 --jinja -c 65536

# Terminal 3: Proxy
cargo run -- --backend gpt-oss --port 8080 \
  --config dual-backend-config.toml --log-file proxy.jsonl
```

### Testing

```bash
# Test gpt-oss (JSON format) - should work as before
curl -X POST 'http://localhost:8080/v1beta/models/gpt-oss-20b:generateContent' \
  -H "Content-Type: application/json" \
  -d '{
    "contents": [{"parts": [{"text": "Write file test.txt"}], "role": "user"}],
    "tools": [{"functionDeclarations": [{"name": "WriteFile", ...}]}]
  }'

# Test Qwen3-Coder (XML format) - should parse XML and convert to JSON
curl -X POST 'http://localhost:8080/v1beta/models/qwen3-coder:generateContent' \
  -H "Content-Type: application/json" \
  -d '{
    "contents": [{"parts": [{"text": "Write file test.txt"}], "role": "user"}],
    "tools": [{"functionDeclarations": [{"name": "WriteFile", ...}]}]
  }'
```

## XML Format Details

### Complete Example

**Model Output**:
```xml
Let me help you with that.

<tool_call>
  <function=WriteFile>
    <parameter=file_path>
    test.txt
    </parameter>
    <parameter=content>
    Hello, World!
    </parameter>
  </function>
</tool_call>

I've created the file for you.
```

**After Conversion**:
```json
{
  "message": {
    "role": "assistant",
    "content": "Let me help you with that.\n\nI've created the file for you.",
    "tool_calls": [{
      "id": "call_0",
      "type": "function",
      "function": {
        "name": "WriteFile",
        "arguments": "{\"file_path\":\"test.txt\",\"content\":\"Hello, World!\"}"
      }
    }]
  },
  "finish_reason": "tool_calls"
}
```

## Parameter Type Handling

The XML parser attempts to preserve types:

```xml
<parameter=count>42</parameter>          → {"count": 42}
<parameter=enabled>true</parameter>      → {"enabled": true}
<parameter=items>["a","b","c"]</parameter> → {"items": ["a","b","c"]}
<parameter=name>John</parameter>         → {"name": "John"}
```

Falls back to string if JSON parsing fails:
```xml
<parameter=data>not valid json</parameter> → {"data": "not valid json"}
```

## Debug Logging

When `verbose = true`, the XML parser logs:

```
🔧 XML Parser: Found tool call - function: WriteFile, args: Object {"file_path": String("test.txt"), ...}
🔄 XML Conversion: Extracted 1 tool calls, cleaned text: "Let me help you with that.\n\nI've created..."
```

## Future Work

### Streaming XML Support

Currently, streaming is not fully implemented for XML format because:

1. **Partial Tags**: XML tags may span multiple SSE chunks
2. **State Management**: Need to accumulate text until `</tool_call>` is seen
3. **Complexity**: Similar to existing JSON chunk accumulator but for XML

**Planned Approach**:
- Add XML content accumulator to streaming pipeline
- Detect `<tool_call>` start tags
- Buffer content until `</tool_call>` closing tag
- Parse complete XML and emit as complete tool call
- Similar to existing JSON streaming accumulator

### Additional Features

- **Mixed Format Support**: Some models might output both JSON and XML
- **Validation**: Schema validation for XML tool calls
- **Error Recovery**: Handle malformed XML gracefully
- **Performance**: Optimize regex patterns for large responses

## Testing Checklist

- ✅ XML parser extracts function name correctly
- ✅ XML parser extracts all parameters
- ✅ XML parser handles multiple parameters
- ✅ Type conversion (numbers, booleans, JSON objects)
- ✅ Fallback to string for invalid JSON
- ✅ Multiple tool calls in single response
- ✅ Text content cleaning (XML removal)
- ✅ Integration with backend response processing
- ⏳ Non-streaming function calling (pending test)
- ⏳ Streaming function calling (not implemented)
- ⏳ Mixed JSON/XML responses (edge case)

## Known Limitations

1. **Streaming**: XML format not yet supported for streaming responses
2. **Multi-line Parameters**: Newlines in parameter values might need special handling
3. **CDATA**: Does not currently handle CDATA sections
4. **Nested XML**: Does not support nested `<tool_call>` tags
5. **Namespaces**: No XML namespace support

## Dependencies

- **regex**: `1.10` - Pattern matching for XML parsing

## Backward Compatibility

- ✅ Existing JSON format backends work unchanged
- ✅ Default `tool_format = "json"` maintains compatibility
- ✅ No changes required to existing configs (auto-defaults to JSON)
- ✅ Existing tests continue to pass

## References

- Qwen3-Coder Tool Parser: https://huggingface.co/Qwen/Qwen3-Coder-30B-A3B-Instruct/blob/main/qwen3coder_tool_parser.py
- Qwen3-Coder Model Card: https://huggingface.co/Qwen/Qwen3-Coder-30B-A3B-Instruct
