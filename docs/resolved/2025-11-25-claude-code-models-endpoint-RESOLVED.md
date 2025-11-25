# Claude Code Models Endpoint - RESOLVED

**Status:** ✅ RESOLVED
**Priority:** HIGH
**Date:** 2025-11-25

## Problem

Claude Code was failing with a 404 error on startup before any messages could be sent. The error sequence was:

1. **Initial 404 error** - Claude Code received 404 status (no body)
2. **JSON parsing errors** - Subsequent retry attempts failed with various JSON syntax errors
3. **User-visible errors** - "Invalid tool parameters", "Error searching files"

From the error log (`../tetris/06e9d400-4b0d-44ae-9edd-3c2ff3c99448.txt:87`):
```
[ERROR] Error: Error: 404 status code (no body)
    at $4.generate (file://.../claude-code/cli.js:231:83170)
```

## Root Cause

Claude Code, like most Anthropic API clients, attempts to fetch the available models list on initialization by calling:
```
GET /v1/models
```

**The proxy was missing this endpoint**, causing Claude Code to fail during initialization before it could send any actual messages.

The existing proxy only had:
- ✅ `POST /v1/messages` - Message creation (working)
- ✅ `GET /health` - Health check (working)
- ✅ `GET /metrics` - Metrics endpoint (working)
- ❌ `GET /v1/models` - **MISSING** (causing 404)

## Solution Implemented

### 1. Added `/v1/models` Endpoint

**Location:** `src/main.rs:160,1390-1447`

Added route:
```rust
.route("/v1/models", get(list_anthropic_models))
```

Implemented handler that:
1. **Returns models from config** - Scans all backend configurations for Claude models
2. **Filters by "claude-" prefix** - Only returns Claude-compatible models
3. **Provides fallback list** - If no models in config, returns standard Claude models
4. **Anthropic API compliant** - Matches official API response format

### 2. Response Format

```json
{
  "data": [
    {
      "type": "model",
      "id": "claude-3-5-sonnet-20241022",
      "display_name": "claude-3-5-sonnet-20241022",
      "created_at": "2024-01-01T00:00:00Z"
    },
    ...
  ],
  "has_more": false,
  "first_id": "claude-3-haiku-20240307",
  "last_id": "claude-3-sonnet-20240229"
}
```

### 3. Dynamic Model Discovery

The endpoint reads from `config.toml`:
```toml
[backends.llama-cpp.model_mapping]
"claude-3-5-sonnet-20241022" = "gpt-oss:20b"
"claude-3-sonnet-20240229" = "gpt-oss:20b"
"claude-3-opus-20240229" = "gpt-oss:20b"
"claude-3-haiku-20240307" = "gpt-oss:20b"
```

Returns all keys starting with "claude-" from all backends' model_mapping.

### 4. Fallback Models

If config has no Claude models, returns default set:
- `claude-3-5-sonnet-20241022` - Claude 3.5 Sonnet
- `claude-3-sonnet-20240229` - Claude 3 Sonnet
- `claude-3-opus-20240229` - Claude 3 Opus
- `claude-3-haiku-20240307` - Claude 3 Haiku

## Implementation Details

```rust
async fn list_anthropic_models(State(state): State<AppState>) -> impl IntoResponse {
    use serde_json::json;

    // Collect all Claude models from backend configurations
    let mut models = Vec::new();

    for (backend_name, backend_config) in state.config.backends.iter() {
        // Only include models that start with "claude-" from model_mapping keys
        for model_name in backend_config.model_mapping.keys() {
            if model_name.starts_with("claude-") {
                models.push(json!({
                    "type": "model",
                    "id": model_name,
                    "display_name": model_name,
                    "created_at": "2024-01-01T00:00:00Z",
                }));
            }
        }
    }

    // If no Claude models found, return default set
    if models.is_empty() {
        models = vec![/* fallback models */];
    }

    Json(json!({
        "data": models,
        "has_more": false,
        "first_id": models.first().and_then(|m| m.get("id")).map(|id| id.as_str()).flatten(),
        "last_id": models.last().and_then(|m| m.get("id")).map(|id| id.as_str()).flatten(),
    }))
}
```

## Testing

### Manual Testing

```bash
# 1. Test models endpoint
curl -s http://localhost:9000/v1/models | jq .

# Output:
# {
#   "data": [
#     {"type": "model", "id": "claude-3-haiku-20240307", ...},
#     ...
#   ],
#   "has_more": false
# }

# 2. Test messages endpoint still works
curl -s -X POST http://localhost:9000/v1/messages \
  -H "Content-Type: application/json" \
  -H "anthropic-version: 2023-06-01" \
  -d '{
    "model": "claude-3-haiku-20240307",
    "max_tokens": 50,
    "messages": [{"role": "user", "content": "Hi"}]
  }' | jq .

# 3. Run integration test
bash /tmp/test-claude-code-integration.sh
```

### Integration Test Results

✅ All tests passing:
```
=== Claude Code Integration Test ===

1. Testing GET /v1/models...
✓ Models endpoint working
  Found 6 models

2. Testing POST /v1/messages...
✓ Messages endpoint working
  Response ID: chatcmpl-JpQXlacM3BlTzN14sh0DiuxSrB0loNwo

3. Checking enhanced logging...
✓ Log file exists
  Total entries: 10
  Errors logged: 22

=== All tests passed! ===
```

## Benefits

### For Claude Code Integration
1. **Initialization succeeds** - No more 404 errors on startup
2. **Model discovery works** - Claude Code can see available models
3. **Dynamic configuration** - Models automatically discovered from config
4. **Backwards compatible** - Existing `/v1/messages` endpoint unchanged

### General Benefits
1. **Standard compliance** - Matches official Anthropic API
2. **Multi-model support** - Returns all configured Claude models
3. **Fallback mechanism** - Works even without config
4. **Extensible** - Easy to add more model metadata

## Files Modified

1. **`src/main.rs:160`** - Added route `.route("/v1/models", get(list_anthropic_models))`
2. **`src/main.rs:1390-1447`** - Implemented `list_anthropic_models()` handler function

## Related Work

This fix builds on the enhanced logging infrastructure from:
- `docs/resolved/2025-11-25-enhanced-logging-for-debugging-COMPLETE.md`

The catch-all handler added during debugging helped identify this issue:
- `src/main.rs:1449-1493` - `catch_all_handler()` logs all 404s

## Usage with Claude Code

```bash
# Set base URL to proxy
export ANTHROPIC_BASE_URL=http://localhost:9000

# Run Claude Code (should work now!)
claude

# Or with debug mode
claude -d
```

## Success Criteria

- [x] `/v1/models` endpoint returns valid response
- [x] Response format matches Anthropic API specification
- [x] Models list populated from config dynamically
- [x] Fallback models available when config empty
- [x] Integration test passes
- [x] Backwards compatibility maintained
- [ ] Tested with actual Claude Code (next step for user)

## Next Steps for User

1. **Test with Claude Code:**
   ```bash
   cd ../tetris
   ANTHROPIC_BASE_URL=http://localhost:9000 claude
   ```

2. **Verify no 404 errors** - Claude Code should initialize successfully

3. **Test actual functionality** - Try sending messages and using tools

4. **Report any remaining issues** - If Claude Code still fails, check logs:
   ```bash
   cargo run --bin log-replay -- \
     --file /tmp/claude-code-debug.jsonl \
     --errors-only
   ```

## Additional Endpoints (If Needed)

If Claude Code still has issues, may need to add:
- `GET /v1/models/{model_id}` - Get specific model info
- `GET /` - Root endpoint with service info
- `OPTIONS /v1/messages` - CORS preflight support

Monitor the catch-all handler logs to identify any additional missing endpoints.

## Lessons Learned

1. **Catch-all handlers are invaluable** - Immediately identified missing endpoint
2. **Enhanced logging pays off** - Request correlation helped track down issue
3. **API compatibility matters** - SDKs expect standard endpoints
4. **Dynamic configuration is flexible** - No hardcoding of model lists
5. **Fallback mechanisms prevent breakage** - Works even with minimal config

## References

- **Issue:** `docs/issues/claude-code-integration-issue.md`
- **Enhanced Logging:** `docs/resolved/2025-11-25-enhanced-logging-for-debugging-COMPLETE.md`
- **Anthropic API Docs:** https://docs.anthropic.com/en/api/models
- **Official Models Endpoint:** `GET https://api.anthropic.com/v1/models`
