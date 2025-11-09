# Mock Server vs Public API Comparison

**Test Date**: 2025-11-21
**Purpose**: Verify cross-protocol routing works identically on mock and public APIs

## Test Results

### ✅ Cross-Protocol Routing: OpenAI API → Gemini Backend

**Test Request**:
```bash
curl -X POST 'http://localhost:{PORT}/v1/chat/completions' \
  -H "Content-Type: application/json" \
  -d '{"model":"gemini-2.5-flash","messages":[{"role":"user","content":"Hi"}]}'
```

| Proxy | Port | Backend | Model Returned | Content | Status |
|-------|------|---------|----------------|---------|--------|
| Mock | 9001 | Mock Gemini | ✅ gemini-2.5-flash | "Hello!" | ✅ PASS |
| Public | 9000 | Real Gemini | ✅ gemini-2.5-flash | "Hi there! How can I help you today!" | ✅ PASS |

**Key Findings**:
1. ✅ Both correctly detect model belongs to Gemini backend
2. ✅ Both correctly route to Gemini backend (not OpenAI)
3. ✅ Both correctly return model name as "gemini-2.5-flash"
4. ✅ Both correctly convert OpenAI → Gemini → OpenAI formats
5. Only difference: Mock returns static "Hello!", Public returns actual Gemini response

### Mock Response (Full)
```json
{
  "id": "chatcmpl-3afd160b-c0a5-4642-b74b-bfd0e098de86",
  "object": "chat.completion",
  "created": 1763702731,
  "model": "gemini-2.5-flash",
  "choices": [{
    "index": 0,
    "message": {
      "role": "assistant",
      "content": "Hello!"
    },
    "finish_reason": "stop"
  }],
  "usage": {
    "prompt_tokens": 3,
    "completion_tokens": 2,
    "total_tokens": 22
  }
}
```

### Public API Response (Full)
```json
{
  "id": "chatcmpl-839372c0-6889-491d-a9ec-5392187f72a6",
  "object": "chat.completion",
  "created": 1763702732,
  "model": "gemini-2.5-flash",
  "choices": [{
    "index": 0,
    "message": {
      "role": "assistant",
      "content": "Hi there! How can I help you today?"
    },
    "finish_reason": "stop"
  }],
  "usage": {
    "prompt_tokens": 2,
    "completion_tokens": 10,
    "total_tokens": 36
  }
}
```

## Differences Analysis

### Content Differences
- **Mock**: Returns static "Hello!" from `test-data/gemini/text/response.json`
- **Public**: Returns dynamic response from real Gemini API

### Token Count Differences
- **Mock**: `prompt_tokens: 3, completion_tokens: 2, total_tokens: 22`
- **Public**: `prompt_tokens: 2, completion_tokens: 10, total_tokens: 36`
- **Reason**: Mock has pre-captured token counts, Public has actual counts

### ID Differences
- **Mock**: `chatcmpl-3afd160b-c0a5-4642-b74b-bfd0e098de86`
- **Public**: `chatcmpl-839372c0-6889-491d-a9ec-5392187f72a6`
- **Reason**: Different UUIDs generated for each request

### Timestamp Differences
- **Mock**: `created: 1763702731`
- **Public**: `created: 1763702732`
- **Reason**: Requests made 1 second apart

## Structural Comparison

✅ **Identical Structure**:
- Response format (OpenAI chat completion)
- Field names and types
- Nesting structure
- Message role format

✅ **Correct Behavior**:
- Both proxies apply model-based routing
- Both convert OpenAI request → Gemini format
- Both send to Gemini backend
- Both convert Gemini response → OpenAI format

## Conclusion

**The mock server accurately simulates public API behavior** for testing cross-protocol routing. The only differences are:
1. **Content**: Mock returns static responses, Public returns dynamic AI responses
2. **Metadata**: Different IDs, timestamps, token counts

The mock server is **suitable for automated testing** because:
- ✅ Deterministic responses
- ✅ No API costs
- ✅ Fast response times
- ✅ Identical routing behavior
- ✅ Same conversion logic

## Recommendation for Mock Server Updates

To make mock responses even more realistic, consider updating mock data to:

1. **Match token counts** to actual request lengths
2. **Use realistic content** that matches the prompt
3. **Include actual Gemini metadata** (modelVersion, responseId, etc.)

Example improved mock response:
```json
{
  "candidates": [{
    "content": {
      "role": "model",
      "parts": [{"text": "Hi! How can I assist you?"}]
    },
    "finishReason": "STOP"
  }],
  "usageMetadata": {
    "promptTokenCount": 2,
    "candidatesTokenCount": 7,
    "totalTokenCount": 9
  },
  "modelVersion": "gemini-2.5-flash",
  "responseId": "mock-response-12345"
}
```

## Logging Recommendations

To improve debugging, add these log entries:

### 1. Model-Based Routing Decision
```
INFO ✓ Model 'gemini-2.5-flash' mapped to backend 'gemini'
INFO   ✓ Backend 'gemini' supports required capabilities
```

### 2. Conversion Steps
```
DEBUG Converting OpenAI request → Gemini format
DEBUG Gemini request: { contents: [...] }
DEBUG Gemini response: { candidates: [...] }
DEBUG Converting Gemini response → OpenAI format
```

### 3. Request/Response Logging (JSON format)
```jsonl
{"direction":"client_request","endpoint":"/v1/chat/completions","format":"openai","model":"gemini-2.5-flash","backend":"gemini"}
{"direction":"backend_request","endpoint":"/v1beta/models/gemini-2.5-flash:generateContent","format":"gemini"}
{"direction":"backend_response","format":"gemini","finish_reason":"STOP","tokens":{"prompt":2,"completion":10}}
{"direction":"client_response","format":"openai","model":"gemini-2.5-flash","tokens":{"prompt":2,"completion":10}}
```

This would enable:
- Tracing full request flow
- Debugging conversion issues
- Performance monitoring
- Audit trails
