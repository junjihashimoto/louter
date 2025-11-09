# Testing Configurations for Louter (LLM Router)

## Phase 1: Direct API Testing ✅
Test the real APIs directly to validate our protocol models.

```bash
# Use real API endpoints (default)
unset GEMINI_BASE_URL OPENAI_BASE_URL
cargo test --test api_integration_tests
```

**Expected:** All tests pass, validates our models work with real APIs.

---

## Phase 2: Proxy with OpenAI Backend
Test that your proxy can forward OpenAI requests to OpenAI API without breaking them.

### Proxy Configuration
Your proxy should:
- Accept requests at `/v1/chat/completions` 
- Forward to OpenAI API unchanged (or with minimal processing)
- Return OpenAI responses as-is

### Test Configuration
```bash
# Start your proxy in OpenAI passthrough mode
cargo run -- --backend openai --port 8080

# Test proxy with OpenAI backend
export OPENAI_BASE_URL="http://localhost:8080"
export GEMINI_BASE_URL="http://localhost:8080"  # Won't be used, but needed for Gemini tests
cargo test --test api_integration_tests
```

### Expected Results
- ✅ `test_openai_text_request` - Should pass (proxy forwards to OpenAI)
- ✅ `test_openai_vision_request` - Should pass 
- ✅ `test_openai_streaming_request` - Should pass
- ❌ `test_gemini_*` - Will fail (proxy doesn't handle Gemini format)

### Key Validations
- Proxy correctly handles OpenAI request format
- Streaming works through proxy
- Error handling works
- Token counting preserved

---

## Phase 3: Proxy with Gemini Backend
Test that your proxy can convert OpenAI requests to Gemini format.

### Proxy Configuration  
Your proxy should:
- Accept OpenAI requests at `/v1/chat/completions`
- Convert to Gemini format
- Forward to Gemini API
- Convert Gemini responses back to OpenAI format

### Test Configuration
```bash
# Start your proxy in Gemini backend mode
cargo run -- --backend gemini --port 8080

# Test proxy with Gemini backend
export OPENAI_BASE_URL="http://localhost:8080"
unset GEMINI_BASE_URL  # Use real Gemini API for forwarding
cargo test --test api_integration_tests test_openai_text_request test_openai_vision_request test_openai_streaming_request
```

### Expected Results
- ✅ `test_openai_text_request` - Should pass (OpenAI→Gemini→OpenAI conversion)
- ✅ `test_openai_vision_request` - Should pass (image conversion)
- ✅ `test_openai_streaming_request` - Should pass (SSE conversion)
- ❌ `test_gemini_*` - Will fail (tests send Gemini format, proxy expects OpenAI)

### Key Validations
- OpenAI → Gemini request conversion
- Gemini → OpenAI response conversion  
- Streaming SSE format conversion
- Image/multimedia handling
- Function calling translation
- Token usage mapping

---

## Test Commands Summary

```bash
# Phase 1: Direct APIs
cargo test --test api_integration_tests

# Phase 2: Proxy + OpenAI Backend  
OPENAI_BASE_URL="http://localhost:8080" cargo test --test api_integration_tests

# Phase 3: Proxy + Gemini Backend
OPENAI_BASE_URL="http://localhost:8080" cargo test --test api_integration_tests test_openai_text_request test_openai_vision_request test_openai_streaming_request
```

## What Each Phase Validates

| Phase | What It Tests | Expected Passing Tests |
|-------|---------------|----------------------|
| 1 | Protocol models correctness | All 6 tests |
| 2 | Proxy infrastructure | 3 OpenAI tests |  
| 3 | Core transformation logic | 3 OpenAI tests (via Gemini) |

After all phases pass, your proxy will be fully functional! 🎉