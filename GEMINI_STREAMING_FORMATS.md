# Gemini Streaming Formats Implementation

The Louter proxy now supports **both** Gemini API streaming formats as documented in the official API.

## Supported Formats

### 1. SSE Format (Server-Sent Events) - **Default & Recommended**

**Activation**: Default, or explicitly use `?alt=sse`

**URL Example**:
```
http://localhost:9000/v1beta/models/gemini-2.0-flash:streamGenerateContent
http://localhost:9000/v1beta/models/gemini-2.0-flash:streamGenerateContent?alt=sse
```

**Response Format**:
```
data: {"candidates":[...],"usageMetadata":{...}}

data: {"candidates":[...],"usageMetadata":{...}}

data: {"candidates":[...],"finishReason":"STOP"}

```

**✅ Compatible With**:
- Google Generative AI Python SDK (`google.generativeai`)
- JavaScript EventSource API
- All SSE-compatible HTTP clients

**Test Results**: ✅ 3/3 tests passing (`tests/test_gemini_sse_streaming.py`)

---

### 2. JSON Array Format - **For HTTP Clients**

**Activation**: Use `?alt=json` query parameter

**URL Example**:
```
http://localhost:9000/v1beta/models/gemini-2.0-flash:streamGenerateContent?alt=json
```

**Response Format**:
```json
[
{"candidates":[...],"usageMetadata":{...}}
,
{"candidates":[...],"usageMetadata":{...}}
,
{"candidates":[...],"finishReason":"STOP"}
]
```

**✅ Compatible With**:
- curl and other HTTP clients
- Custom parsers that handle streaming JSON arrays
- Tools that can parse complete JSON

**❌ NOT Compatible With**:
- Google Generative AI Python SDK (known SDK limitation)

**Test Results**: ✅ Produces valid JSON, tested with curl

---

## Implementation Details

### Code Location

- **SSE Handler**: `Louter.Protocol.GeminiStreaming` (existing)
- **JSON Array Handler**: `Louter.Protocol.GeminiStreamingJsonArray` (new)
- **Router**: `app/Main.hs` - `handleGeminiStreaming` function

### Format Detection

```haskell
-- In handleGeminiStreaming
let queryParams = queryString req
    altParam = lookup "alt" queryParams
    streamFormat = case altParam of
      Just (Just "json") -> "json"
      Just (Just "sse") -> "sse"
      _ -> "sse"  -- Default to SSE
```

### Conversion Flow

```
OpenAI SSE Stream
      ↓
[Format Detection]
      ↓
   ┌──┴──┐
   │     │
  SSE   JSON
   │     │
   └──┬──┘
      ↓
Gemini Response
```

---

## Testing

### SSE Format (Recommended)
```bash
env GOOGLE_GEMINI_BASE_URL=http://localhost:9000 python3 tests/test_gemini_sse_streaming.py
```

### JSON Array Format (curl)
```bash
curl -X POST 'http://localhost:9000/v1beta/models/gpt-oss:streamGenerateContent?alt=json' \
  -H 'Content-Type: application/json' \
  -d '{"contents":[{"role":"user","parts":[{"text":"Hello"}]}]}'
```

---

## Key Points

1. **SSE is the default and recommended format** for all clients, especially Python SDK
2. **JSON array format works correctly** but has Python SDK compatibility issues (same as real Gemini API)
3. **Both formats are fully implemented** in the Louter proxy
4. **Backward compatibility maintained** - existing code continues to work
5. **Format detection is automatic** based on `?alt=` parameter

---

## References

- Test Data: `test-data/gemini/streaming_sse/` and `test-data/gemini/streaming_jsonarray/`
- Documentation: `test-data/gemini/README.md`
- Tests: `tests/test_gemini_sse_streaming.py`
