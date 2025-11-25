# Claude Code Integration Issue

**Status:** 🔴 OPEN
**Priority:** HIGH
**Date:** 2025-11-24

## Problem Summary

Claude Code fails to work with the proxy, encountering JSON parsing errors and 404 responses.

## Error Symptoms

### From Debug Logs (`06e9d400-4b0d-44ae-9edd-3c2ff3c99448.txt`)

1. **Initial 404 Error (Line 87-95)**
```
[ERROR] Error: 404 status code (no body)
    at $4.generate (file:///.../cli.js:231:83170)
```

2. **Repeated JSON Parsing Errors (Lines 120-369)**
```
SyntaxError: Expected property name or '}' in JSON at position 1
SyntaxError: Expected ',' or '}' after property value in JSON at position 300
SyntaxError: Unterminated string in JSON at position 261
SyntaxError: Expected ',' or ']' after array element in JSON at position 291
```

3. **Console Output**
```
⎿  Initializing…
⎿  Invalid tool parameters
⎿  Error searching files
```

## Investigation Results

### ✅ SSE Stream Format is Valid
Testing with curl shows the proxy produces **correct SSE format**:
- All JSON objects are complete and well-formed
- Event names are correct (`message_start`, `content_block_delta`, etc.)
- Headers are correct (`text/event-stream`)
- Tool calls work correctly in tests (14/14 tests passing)

### ❌ Something Fails Before Streaming
The 404 error suggests Claude Code's **initial request** is failing before any streaming begins.

## Possible Root Causes

### 1. Missing Endpoint
Claude Code might be calling endpoints that don't exist:
- `GET /v1/models` - List available models
- `GET /health` or `GET /` - Health check
- Other initialization endpoints

### 2. Missing Required Headers
Claude Code 2.0.37 might require specific headers:
- `anthropic-version: 2023-06-01` (possibly enforced strictly)
- `anthropic-beta` headers for certain features
- Custom Claude Code identification headers

### 3. Model Discovery
Claude Code might:
- Try to verify the model exists before making requests
- Request model capabilities/metadata
- Use a different model name than expected

### 4. Request Format Differences
Claude Code might send slightly different request formats:
- Additional fields not handled by the proxy
- Different field ordering or nesting
- Extended thinking parameters (Claude 2.5+)

## Debug Steps Needed

### Step 1: Capture First Request
```bash
# Terminal 1: Monitor logs
tail -f /tmp/claude-debug.jsonl | jq .

# Terminal 2: Run Claude Code
cd ../tetris
ANTHROPIC_BASE_URL=http://localhost:9000 claude -d
```

**What to check:**
- Endpoint path in first request
- HTTP method (GET/POST)
- Headers present
- Model name requested
- Body structure

### Step 2: Check Proxy Endpoints
Review what endpoints are currently implemented:
```bash
grep -n "Router::new" src/main.rs
grep -n "\.route" src/main.rs
```

### Step 3: Compare with Working Setup
Test with the Python proxy that works:
```bash
# What endpoints does it implement?
# What headers does it require?
```

## Known Working Configuration

The Anthropic SDK Python tests work perfectly (14/14 passing), which means:
- ✅ The core streaming protocol is correct
- ✅ Tool calls work properly
- ✅ Event sequence is spec-compliant
- ✅ JSON formatting is valid

This suggests the issue is **not** in the core conversion logic, but in:
- Initial handshake/setup
- Endpoint routing
- Request validation
- Header requirements

## Files to Investigate

1. **`src/main.rs`**
   - Check all `.route()` definitions
   - Look for missing endpoints
   - Review header validation

2. **`src/config.rs`**
   - Check if model validation is too strict
   - Review backend selection logic

3. **`src/conversion.rs`**
   - Check for request field validation
   - Look for missing field handling

## Temporary Workarounds

None available - proxy fundamentally doesn't work with Claude Code.

## Related Issues

- This issue blocks the refactoring to internal expression format
- Cannot test internal expression architecture until Claude Code integration works

## Next Steps

1. **Capture actual failing request** from Claude Code
2. **Identify missing endpoint** or failing validation
3. **Add missing endpoint** or fix validation
4. **Test with Claude Code** to verify fix
5. **Document the fix** in resolved/ directory
6. **Then proceed** with internal expression refactoring

## Success Criteria

- [ ] Claude Code initializes without 404 error
- [ ] Claude Code can send prompts
- [ ] Claude Code receives streaming responses
- [ ] Tool calls work in Claude Code
- [ ] No JSON parsing errors
- [ ] No "Invalid tool parameters" errors
