# Error Map: Typical Cases, Debugging & Fixes

**Purpose:** Quick reference for common errors, their causes, and solutions
**Last Updated:** 2025-11-21

## How to Use This Guide

1. Find your error by **symptom** or **error message**
2. Follow the **debugging steps**
3. Apply the **fix**
4. See **real example** if available

---

## Category 1: Configuration Errors

### Error 1.1: "Backend not configured"

**Symptoms:**
```
Error: Backend 'openai' not configured
Status: 500
```

**Cause:** Missing backend in config file or wrong backend name

**Debug Steps:**
```bash
# 1. Check config file
cat config-public-api.toml | grep "\[backends\."

# 2. Check what you're requesting
grep "backend_name" /tmp/proxy.jsonl | jq .
```

**Fix:**
```toml
# Add missing backend to config
[backends.openai]
url = "https://api.openai.com"
protocol = "openai"
model_mapping."gpt-4" = "gpt-4"
```

**Prevention:** Always define backends before using them

---

### Error 1.2: "No model_mapping configured"

**Symptoms:**
```
Error: No model_mapping configured for backend 'openai'
Status: 500
```

**Cause:** Empty or missing `model_mapping` section

**Debug Steps:**
```bash
# Check if model_mapping exists
cat config-public-api.toml | grep -A 3 "model_mapping"
```

**Fix:**
```toml
[backends.openai.model_mapping]
"gpt-4" = "gpt-4"
"gpt-3.5-turbo" = "gpt-3.5-turbo"
```

**Prevention:** Every backend MUST have at least one model mapping

---

### Error 1.3: "API key not found"

**Symptoms:**
```
Error: OpenAI API key not found in config or OPENAI_API_KEY environment variable
Status: 500
```

**Cause:** Missing API key in config or environment

**Debug Steps:**
```bash
# 1. Check environment variable
echo $OPENAI_API_KEY

# 2. Check config file
cat config-public-api.toml | grep api_key

# 3. Check which backend proxy is trying to use
grep "get_api_key" /tmp/proxy.jsonl | jq .
```

**Fix Option 1 - Environment Variable:**
```bash
export OPENAI_API_KEY="sk-..."
export GEMINI_API_KEY="..."
```

**Fix Option 2 - Config File:**
```toml
[backends.openai]
api_key = "sk-..."
```

**Prevention:** Set API keys before starting proxy

---

## Category 2: Protocol Conversion Errors

### Error 2.1: "Unsupported parameter: 'max_tokens'"

**Symptoms:**
```json
{
  "error": {
    "message": "Unsupported value: 'max_tokens' is not supported with this model. Use 'max_completion_tokens' instead.",
    "type": "invalid_request_error",
    "param": "max_tokens"
  }
}
```

**Cause:** Newer OpenAI models require `max_completion_tokens`, not `max_tokens`

**Debug Steps:**
```bash
# 1. Check diagnostics
curl -s http://localhost:9000/api/diagnostics > diag.json
cat diag.json | jq '.frontends[].test_results[] | select(.error | contains("max_tokens"))'

# 2. Check what's being sent
grep "max_tokens" /tmp/proxy.jsonl | jq .

# 3. Check backend config
cat config-public-api.toml | grep max_tokens_field
```

**Fix:**
```toml
[backends.openai]
max_tokens_field = "max_completion_tokens"  # For newer OpenAI models

[backends.gpt-oss]
max_tokens_field = "max_tokens"  # For llama.cpp
```

**Real Example:** [api-type-error-max-tokens.md](api-type-error-max-tokens.md)

**Prevention:** Configure `max_tokens_field` per backend based on their requirements

---

### Error 2.2: "Unsupported value: 'temperature'"

**Symptoms:**
```json
{
  "error": {
    "message": "Unsupported value: 'temperature' does not support 0.7 with this model. Only the default (1) value is supported.",
    "param": "temperature"
  }
}
```

**Cause:** Some models (e.g., gpt-5-nano) don't accept custom temperature

**Debug Steps:**
```bash
# 1. Check which backend/model
cat diag.json | jq '.frontends[].test_results[] | select(.error | contains("temperature"))'

# 2. Check temperature config
cat config-public-api.toml | grep temperature
```

**Fix:**
```toml
[backends.openai]
temperature_override = true  # Override client's temperature
# Don't set temperature = use backend's default
```

**Prevention:** Use `temperature_override = true` for restrictive models

---

### Error 2.3: "Failed to parse response: missing field `candidatesTokenCount`"

**Symptoms:**
```
Error: Failed to parse Gemini response: missing field `candidatesTokenCount` at line 18
```

**Cause:** Gemini response was blocked or had no output (optional field treated as required)

**Debug Steps:**
```bash
# 1. Check the actual response
grep "candidatesTokenCount" /tmp/proxy.jsonl | jq .

# 2. Look for blockReason
grep "blockReason" /tmp/proxy.jsonl | jq .
```

**Fix (Code):**
```rust
// In src/models/gemini.rs
pub struct UsageMetadata {
    pub prompt_token_count: i32,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub candidates_token_count: Option<i32>,  // Make optional
    pub total_token_count: i32,
}
```

**Fix (Usage):**
```rust
// Handle missing field
let completion_tokens = metadata.candidates_token_count.unwrap_or(0);
```

**Prevention:** Make optional fields `Option<T>` in struct definitions

---

### Error 2.4: "Failed to parse response: missing field `parts`"

**Symptoms:**
```
Error: Failed to parse Gemini response: missing field `parts` at line 6
finishReason: "MAX_TOKENS"
```

**Cause:** Gemini used all tokens for thinking, returned empty `parts` array

**Debug Steps:**
```bash
# 1. Check for MAX_TOKENS
grep "MAX_TOKENS" /tmp/proxy.jsonl | jq .

# 2. Check thinking tokens
grep "thoughtsTokenCount" /tmp/proxy.jsonl | jq .
```

**Fix (Code):**
```rust
// In src/models/gemini.rs
pub struct Content {
    pub role: String,
    #[serde(default)]  // Default to empty vec
    pub parts: Vec<Part>,
}
```

**Fix (Config - Increase Limits):**
```rust
// In src/diagnostic.rs
"generationConfig": {
    "maxOutputTokens": 1000  // Increased from 400
}
```

**Prevention:** Use higher token limits to accommodate thinking tokens

---

## Category 3: Backend Errors

### Error 3.1: Backend Unreachable (Connection Refused)

**Symptoms:**
```
Error: Connection refused
Backend: http://localhost:11211
Status: ○ UNREACHABLE
```

**Debug Steps:**
```bash
# 1. Check if backend is running
curl http://localhost:11211/health

# 2. Check diagnostics
curl -s http://localhost:9000/api/diagnostics | jq '.backends[].reachable'

# 3. Check backend logs
tail -f /tmp/llama-server.log
```

**Fix:**
```bash
# Start the backend (example: llama.cpp)
./llama-server -m model.gguf --port 11211 &

# Or start Ollama
ollama serve
```

**Prevention:** Start backends before starting proxy, use health checks

---

### Error 3.2: Quota Exceeded (429)

**Symptoms:**
```json
{
  "error": {
    "code": 429,
    "message": "You exceeded your current quota...",
    "status": "RESOURCE_EXHAUSTED"
  }
}
```

**Cause:** API rate limits or quota exhausted

**Debug Steps:**
```bash
# 1. Check which backend
grep "429" /tmp/proxy.jsonl | jq .backend_used

# 2. Check quota details
grep "RESOURCE_EXHAUSTED" /tmp/proxy.jsonl | jq .
```

**Fix:**
- **Immediate:** Wait for quota reset or use different API key
- **Long-term:** Implement rate limiting, use multiple backends with fallback

**Prevention:** Monitor usage, implement response caching

---

### Error 3.3: Model Not Found (404)

**Symptoms:**
```json
{
  "error": {
    "code": 404,
    "message": "The model `gpt-oss-20b` does not exist"
  }
}
```

**Cause:** Wrong model name in request or model_mapping

**Debug Steps:**
```bash
# 1. Check what model was requested
grep "model.*gpt-oss" /tmp/proxy.jsonl | jq .

# 2. Check model_mapping
cat config-public-api.toml | grep -A 5 "model_mapping"

# 3. List available models on backend
curl http://localhost:11211/v1/models
```

**Fix:**
```toml
# Correct the model_mapping
[backends.gpt-oss.model_mapping]
"gpt-oss" = "gpt-oss:20b"  # Frontend name → Backend name
```

**Prevention:** Use correct backend model names, test with diagnostics

---

## Category 4: Routing Errors

### Error 4.1: No Backend Supports Required Capability

**Symptoms:**
```
Error: No backend supports required capabilities: [vision, function_calling]
Available backends: text-only (text)
```

**Cause:** No configured backend has the requested capabilities

**Debug Steps:**
```bash
# 1. Check diagnostics for capabilities
curl -s http://localhost:9000/api/diagnostics | jq '.backends[].detected_capabilities'

# 2. Check config capabilities
cat config-public-api.toml | grep capabilities

# 3. Check what was requested
grep "required_capabilities" /tmp/proxy.jsonl | jq .
```

**Fix:**
```toml
# Add backend with required capabilities
[backends.multimodal]
url = "https://api.openai.com"
capabilities = ["text", "vision", "function_calling"]
model_mapping."gpt-4-vision" = "gpt-4-vision-preview"
```

**Prevention:** Configure backends with appropriate capabilities, use AUTO mode

---

### Error 4.2: Model Not in Any Backend's model_mapping

**Symptoms:**
```
Error: Model 'gemini-pro' not found in any backend configuration
```

**Cause:** Requested model not defined in any backend's model_mapping

**Debug Steps:**
```bash
# 1. Check what model was requested
echo "Client requested: gemini-pro"

# 2. Check all model_mappings
grep -A 3 "model_mapping" config-public-api.toml

# 3. Check routing decision
grep "routing.*model" /tmp/proxy.jsonl | jq .
```

**Fix:**
```toml
# Add model to appropriate backend
[backends.gemini.model_mapping]
"gemini-pro" = "gemini-1.5-pro"  # Map to actual backend model
```

**Prevention:** Define all frontend model names in model_mapping

---

## Category 5: Performance Issues

### Error 5.1: High TTFT (Time to First Token)

**Symptoms:**
```
TTFT: 5000ms (expected: <500ms)
Backend: gemini
```

**Debug Steps:**
```bash
# 1. Check metrics
curl http://localhost:9000/api/metrics | grep ttft

# 2. Check logs for timing
grep "ttft_ms" /tmp/proxy.jsonl | jq .

# 3. Test backend directly
time curl -X POST http://backend/v1/chat/completions -d '{...}'
```

**Causes & Fixes:**

| Cause | Fix |
|-------|-----|
| Cold start | Warm up backend with health checks |
| Network latency | Use local/regional backend |
| Large context | Reduce input tokens, implement caching |
| Backend overload | Add load balancing, scale backend |

**Prevention:** Monitor TTFT metrics, set up alerts

---

### Error 5.2: Request Timeout

**Symptoms:**
```
Error: Request timeout after 30 seconds
```

**Debug Steps:**
```bash
# 1. Check timeout config
cat config-public-api.toml | grep timeout

# 2. Check backend response time
grep "response_time_ms" /tmp/proxy.jsonl | jq .
```

**Fix:**
```toml
[performance]
timeout_seconds = 120  # Increase timeout
```

**Prevention:** Set appropriate timeouts based on expected response times

---

## Quick Diagnosis Flowchart

```
Error Occurred
    │
    ├─ Check error code
    │   ├─ 400: Bad request → Check request format
    │   ├─ 401/403: Auth error → Check API keys
    │   ├─ 404: Not found → Check model_mapping
    │   ├─ 429: Rate limit → Check quota/rate limits
    │   └─ 500: Server error → Check logs
    │
    ├─ Check error message
    │   ├─ "not supported" → Check backend compatibility
    │   ├─ "missing field" → Check optional fields
    │   ├─ "not found" → Check configuration
    │   └─ "timeout" → Check performance
    │
    └─ Check context
        ├─ Which backend? → Check backend-specific issues
        ├─ Which capability? → Check capability support
        ├─ Native or conversion? → Check protocol conversion
        └─ Which test? → Check test-specific requirements
```

## Debugging Priority Order

1. **Check error message** - What does it say exactly?
2. **Check diagnostics** - Is backend reachable? Capabilities detected?
3. **Check logs** - What was sent? What was received?
4. **Check config** - Is backend configured correctly?
5. **Check backend directly** - Does backend work without proxy?
6. **Check code** - Is there a bug in conversion logic?

## Common Debugging Commands

```bash
# Full diagnostic
curl -s http://localhost:9000/api/diagnostics | jq . > /tmp/d.json

# Check backend health
curl -s http://localhost:9000/api/diagnostics | jq '.backends[].reachable'

# Find failed tests
cat /tmp/d.json | jq '.frontends[].test_results[] | select(.passed == false)'

# Check specific error
cat /tmp/d.json | jq '.frontends[].test_results[] | select(.error | contains("max_tokens"))'

# View recent errors
grep -i error /tmp/proxy.jsonl | tail -10 | jq .

# Check token counts
grep "candidatesTokenCount\|thoughtsTokenCount" /tmp/proxy.jsonl | jq .
```

## Related Documentation

- [standard-debugging-workflow.md](standard-debugging-workflow.md) - Step-by-step debugging process
- [api-type-error-max-tokens.md](api-type-error-max-tokens.md) - Real debugging example
- [using-diagnostic-api.md](using-diagnostic-api.md) - How to use diagnostics
- [analyzing-json-logs.md](analyzing-json-logs.md) - Understanding logs
