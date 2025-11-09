# Cross-Protocol Conversion Test Results

**Test Date**: 2025-11-21
**Proxy Port**: 9001
**Mock Server Port**: 8888
**Config**: config-mock.toml

## Test Environment

```bash
# Mock server with all 8 response scenarios
cargo run --bin mock-server -- --port 8888 --data-dir test-data --verbose

# Proxy with mock backends
cargo run --bin louter -- --backend openai --port 9001 --config config-mock.toml --verbose
```

## Test Results Summary

| Scenario | Frontend API | Backend API | Model | Status |
|----------|-------------|-------------|-------|--------|
| 1. Text | Gemini | OpenAI | gpt-5-nano | ✅ WORKING |
| 2. Function Calling | Gemini | OpenAI | gpt-5-nano | ✅ WORKING |
| 3. Vision | Gemini | OpenAI | gpt-5-nano | ✅ WORKING |
| 4. Text | OpenAI | Gemini | gemini-2.5-flash | ❌ NOT WORKING |

## Detailed Test Results

### ✅ TEST 1: Gemini API → OpenAI Backend (Text)

**Request**:
```bash
curl -X POST 'http://localhost:9001/v1beta/models/gpt-5-nano:generateContent' \
  -H "Content-Type: application/json" \
  -d '{"contents":[{"parts":[{"text":"こんにちは"}],"role":"user"}]}'
```

**Response**:
```json
{
  "candidates": [{
    "content": {
      "role": "model",
      "parts": [{"text": "Hello! How can I help you today?\n"}]
    },
    "finishReason": "STOP"
  }],
  "usageMetadata": {
    "promptTokenCount": 2,
    "candidatesTokenCount": 10,
    "totalTokenCount": 12
  }
}
```

**Result**: ✅ **PASS**
- Proxy correctly detected gpt-5-nano as OpenAI model
- Converted Gemini request → OpenAI format
- Routed to OpenAI backend (mock server returned openai/text response)
- Converted OpenAI response → Gemini format
- Returned valid Gemini response

---

### ✅ TEST 2: Gemini API → OpenAI Backend (Function Calling)

**Request**:
```bash
curl -X POST 'http://localhost:9001/v1beta/models/gpt-5-nano:generateContent' \
  -H "Content-Type: application/json" \
  -d '{
    "contents": [{"parts": [{"text": "Search for weather"}], "role": "user"}],
    "tools": [{"functionDeclarations": [{"name": "search", "description": "Search"}]}]
  }'
```

**Response**:
```json
{
  "candidates": [{
    "content": {
      "role": "model",
      "parts": [{
        "functionCall": {
          "name": "search",
          "args": {"query": "hello"}
        }
      }]
    },
    "finishReason": "STOP"
  }],
  "usageMetadata": {
    "promptTokenCount": 10,
    "candidatesTokenCount": 3,
    "totalTokenCount": 13
  }
}
```

**Result**: ✅ **PASS**
- Converted Gemini `tools.functionDeclarations` → OpenAI `tools.function`
- Routed to OpenAI backend with function calling
- Converted OpenAI `tool_calls` → Gemini `functionCall`
- Function name and args correctly converted

---

### ✅ TEST 3: Gemini API → OpenAI Backend (Vision)

**Request**:
```bash
curl -X POST 'http://localhost:9001/v1beta/models/gpt-5-nano:generateContent' \
  -H "Content-Type: application/json" \
  -d '{
    "contents": [{
      "parts": [
        {"text": "What do you see?"},
        {"inlineData": {"mimeType": "image/jpeg", "data": "ABC"}}
      ],
      "role": "user"
    }]
  }'
```

**Response**:
```json
{
  "candidates": [{
    "content": {
      "role": "model",
      "parts": [{
        "text": "The image appears to be completely white. There are no objects, shapes, or patterns visible.\n"
      }]
    },
    "finishReason": "STOP"
  }],
  "usageMetadata": {
    "promptTokenCount": 264,
    "candidatesTokenCount": 20,
    "totalTokenCount": 284
  }
}
```

**Result**: ✅ **PASS**
- Converted Gemini `inlineData` → OpenAI `image_url` with base64 data
- Routed to OpenAI backend with vision capability
- Returned vision analysis correctly

---

### ❌ TEST 4: OpenAI API → Gemini Backend (Text)

**Request**:
```bash
curl -X POST 'http://localhost:9001/v1/chat/completions' \
  -H "Content-Type: application/json" \
  -d '{"model": "gemini-2.5-flash", "messages": [{"role": "user", "content": "こんにちは"}]}'
```

**Response**:
```json
{
  "id": "chatcmpl-4fb00a13-f3b5-4b81-b567-ff509552148c",
  "object": "chat.completion",
  "created": 1763698707,
  "model": "gpt-5-nano",
  "choices": [{
    "index": 0,
    "message": {
      "role": "assistant",
      "content": "Hello! How can I help you today?\n"
    },
    "finish_reason": "stop"
  }],
  "usage": {
    "prompt_tokens": 2,
    "completion_tokens": 10,
    "total_tokens": 12
  }
}
```

**Result**: ❌ **FAIL**
- Mock server logs show: `Detected scenario: openai/text`
- Proxy routed to **OpenAI backend** instead of Gemini backend
- Response shows `"model": "gpt-5-nano"` (OpenAI mock response)
- Should have converted to Gemini format and routed to Gemini backend

**Expected Behavior**:
1. Proxy should detect "gemini-2.5-flash" is a Gemini model
2. Convert OpenAI request → Gemini format
3. Route to Gemini backend (mock server should see gemini/text)
4. Convert Gemini response → OpenAI format
5. Return OpenAI-formatted response

## Analysis

### What's Working ✅

**Gemini API → OpenAI Backend** (all capabilities)
- Model detection: Correctly identifies OpenAI models (gpt-5-nano)
- Request conversion: Gemini → OpenAI format conversion works perfectly
  - Text messages ✅
  - Function declarations → Tools ✅
  - Inline data → Image URLs ✅
- Backend routing: Routes to correct OpenAI backend
- Response conversion: OpenAI → Gemini format works perfectly
  - Message content ✅
  - Tool calls → Function calls ✅
  - Usage metadata ✅

### What's Not Working ❌

**OpenAI API → Gemini Backend**
- Model detection: Does NOT recognize Gemini models from OpenAI API requests
- Routes all OpenAI API requests to OpenAI backend regardless of model name
- No conversion to Gemini format happening

## Root Cause Investigation Needed

The proxy needs to:
1. **Detect model backend** from model name in OpenAI API requests
   - Currently only works for Gemini API requests
   - Need to check model mappings for OpenAI API frontend

2. **Route based on model, not just API format**
   - OpenAI API request for "gemini-2.5-flash" should route to Gemini backend
   - Currently routes based on default backend or API format only

## Next Steps

1. Investigate routing logic in src/main.rs
2. Check how model-to-backend mapping works for OpenAI API frontend
3. Fix routing to support cross-protocol conversion in both directions
4. Add tests for OpenAI → Gemini conversion (function calling, vision)

## Success Criteria

All 8 scenarios should work:
1. ✅ Gemini API → OpenAI Backend (Text)
2. ✅ Gemini API → OpenAI Backend (Function Calling)
3. ✅ Gemini API → OpenAI Backend (Vision)
4. ✅ Gemini API → Gemini Backend (Native, already working)
5. ❌ OpenAI API → Gemini Backend (Text) - **TO BE FIXED**
6. ❌ OpenAI API → Gemini Backend (Function Calling) - **TO BE FIXED**
7. ❌ OpenAI API → Gemini Backend (Vision) - **TO BE FIXED**
8. ✅ OpenAI API → OpenAI Backend (Native, already working)
