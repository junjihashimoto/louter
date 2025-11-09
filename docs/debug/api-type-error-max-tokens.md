# Debugging: API Type Error - max_tokens Field

**Date:** 2025-11-21
**Problem:** Gemini API → OpenAI backend failing with "Unsupported parameter: 'max_tokens'"
**Resolution:** Added configurable `max_tokens_field` per backend

## The Error

Frontend diagnostics showed:
```json
{
  "error": {
    "message": "Unsupported value: 'max_tokens' is not supported with this model. Use 'max_completion_tokens' instead.",
    "type": "invalid_request_error",
    "param": "max_tokens",
    "code": "unsupported_value"
  }
}
```

## Step-by-Step Debugging Process

### Step 1: Run Diagnostics to Capture Error

```bash
# Start the proxy
cargo run --bin louter -- --host localhost --port 9000 \
  --config config-public-api.toml --log-file /tmp/proxy.jsonl --verbose

# Run diagnostics and save output
curl -s http://localhost:9000/api/diagnostics > /tmp/diagnostics.json
```

### Step 2: Read the Full Error (Don't Filter!)

**CRITICAL:** Always save full output first, then analyze.

```bash
# Save full output to file
cat /tmp/diagnostics.json | jq . > diagnostics-9000.json

# Now read the full file
cat diagnostics-9000.json
```

**Lesson learned:** Don't filter with `jq` immediately - you might miss important context!

### Step 3: Identify the Failing Test

Found in diagnostics output:
```json
{
  "test_name": "Gemini API → openai backend: Text",
  "passed": false,
  "error": "{\"error\":{...\"max_tokens\" is not supported...}}"
}
```

**Key insight:** The issue is with Gemini API → OpenAI backend (cross-protocol conversion).

### Step 4: Check the Proxy Logs

```bash
# Look at the last few requests in the log file
tail -20 /tmp/proxy.jsonl | jq .
```

The logs showed the request being sent to OpenAI backend with `max_tokens` field.

### Step 5: Trace the Code - Where is max_tokens Set?

**Search for where we set max_tokens in OpenAI requests:**

```bash
grep -n "max_tokens:" src/conversion.rs
```

Found at `src/conversion.rs:279`:
```rust
max_tokens: backend_config.max_tokens,
max_completion_tokens: None,  // This was the problem!
```

### Step 6: Understand the Root Cause

**Problem identified:**
- Older OpenAI models use `max_tokens`
- Newer OpenAI models (like gpt-5-nano) use `max_completion_tokens`
- llama.cpp servers use `max_tokens`
- Our code was hardcoded to only use `max_tokens`

### Step 7: Design the Solution

**Decision:** Make it configurable per-backend

```toml
[backends.openai]
max_tokens_field = "max_completion_tokens"  # For newer OpenAI

[backends.gpt-oss]
max_tokens_field = "max_tokens"  # For llama.cpp
```

### Step 8: Implement the Fix

**Files changed:**

1. **src/config.rs** - Add config field:
```rust
pub max_tokens_field: String,  // "max_tokens", "max_completion_tokens", or "both"
```

2. **src/conversion.rs:278-284** - Use config to set fields:
```rust
let (max_tokens, max_completion_tokens) = match backend_config.max_tokens_field.as_str() {
    "max_tokens" => (max_output_tokens, None),
    "max_completion_tokens" => (None, max_output_tokens),
    "both" => (max_output_tokens, max_output_tokens),
    _ => (max_output_tokens, max_output_tokens),
};
```

3. **config-public-api.toml** - Configure per backend:
```toml
[backends.openai]
max_tokens_field = "max_completion_tokens"

[backends.gpt-oss]
max_tokens_field = "max_tokens"
```

### Step 9: Rebuild and Test

```bash
# Rebuild
cargo build --bin louter

# Restart proxy
pkill -f "louter.*9000"
cargo run --bin louter -- --host localhost --port 9000 \
  --config config-public-api.toml --log-file /tmp/proxy-test.jsonl --verbose &

# Wait for startup
sleep 5

# Test with diagnostics
curl -s http://localhost:9000/api/diagnostics > /tmp/diagnostics-fixed.json
```

### Step 10: Verify the Fix

```bash
# Check if the error is gone
cat /tmp/diagnostics-fixed.json | jq '.frontends.gemini_api.test_results[] |
  select(.backend_used == "openai") |
  "\(.test_name): \(if .passed then "PASS" else "FAIL" end)"'
```

**Result:**
```
Gemini API → openai backend: Text: PASS ✅
Gemini API → openai backend: Vision: PASS ✅
Gemini API → openai backend: Function Calling: PASS ✅
```

## Key Debugging Techniques Used

1. **Save full output before filtering** - Don't use `jq` filters initially
2. **Read error messages carefully** - "Use 'max_completion_tokens' instead" told us exactly what to do
3. **Check logs** - Proxy logs show what was actually sent to backend
4. **Trace code with grep** - Find where parameters are set
5. **Understand the ecosystem** - Different backends have different requirements
6. **Make it configurable** - Don't hardcode assumptions

## Files to Check When Debugging Similar Issues

| File | What to Check |
|------|---------------|
| `/tmp/proxy.jsonl` | Request/response logs |
| `src/conversion.rs` | Protocol conversion logic |
| `config-public-api.toml` | Backend configuration |
| `/tmp/diagnostics.json` | Test results with errors |

## Common Mistakes to Avoid

❌ **Filtering too early:** Using `jq` immediately loses context
❌ **Not reading full errors:** Missing important details like "Use X instead"
❌ **Assuming all backends are the same:** OpenAI, llama.cpp, etc. have differences
❌ **Hardcoding values:** Make backend-specific settings configurable

## Related Documentation

- [design/token-count-mapping.md](../design/token-count-mapping.md) - Why we have multiple token fields
- [tutorial/backend-configuration.md](../tutorial/backend-configuration.md) - How to configure backends
