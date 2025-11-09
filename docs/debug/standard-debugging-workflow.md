# Standard Debugging Workflow

**Purpose:** Systematic approach to debugging any issue in louter
**Based on:** Real debugging sessions (2025-11-21)

## Core Principle

**ALWAYS follow this order:**
1. **Observe** - Capture the full error/behavior
2. **Locate** - Find where in the code it happens
3. **Understand** - Learn why it happens
4. **Fix** - Make the minimal change
5. **Verify** - Prove the fix works

## Step-by-Step Workflow

### Step 1: Capture Full Diagnostics Output

**DO THIS FIRST - Before any filtering or analysis!**

```bash
# Start proxy with logging
cargo run --bin louter -- --host localhost --port 9000 \
  --config config-public-api.toml \
  --log-file /tmp/proxy-debug.jsonl \
  --verbose &

# Wait for startup
sleep 5

# Capture FULL diagnostics output to file
curl -s http://localhost:9000/api/diagnostics > /tmp/diagnostics-raw.json

# Pretty print to readable file
cat /tmp/diagnostics-raw.json | jq . > /tmp/diagnostics.json
```

**⚠️ CRITICAL RULE:** Save raw output BEFORE filtering!
- ❌ DON'T: `curl ... | jq '.some.filter'` (loses context)
- ✅ DO: `curl ... > file.json` then analyze file

### Step 2: Read the Full Error Message

```bash
# Read the entire diagnostics file
cat /tmp/diagnostics.json

# Look for failed tests
cat /tmp/diagnostics.json | jq '.frontends[].test_results[] | select(.passed == false)'
```

**What to look for:**
- Exact error message (don't paraphrase!)
- Which test failed (backend, frontend, capability)
- Error codes (400, 429, 500, etc.)
- Any hints in the message ("Use X instead", "Field Y missing")

### Step 3: Check Proxy Logs

```bash
# View recent requests
tail -50 /tmp/proxy-debug.jsonl | jq .

# Filter for errors
grep -i error /tmp/proxy-debug.jsonl | jq .

# Find specific request by looking for the test
grep "What's in this image" /tmp/proxy-debug.jsonl | jq .
```

**What logs tell you:**
- What was sent to the backend
- What the backend returned
- Which backend handled the request
- Request/response timing

### Step 4: Locate the Code

**Use grep to find relevant code:**

```bash
# Find where error occurs
grep -rn "error_text" src/

# Find where parameter is set
grep -rn "max_tokens:" src/

# Find conversion logic
grep -rn "gemini_to_openai" src/

# Find where field is used
grep -rn "candidates_token_count" src/
```

**Read the code:**
```bash
# View specific file section
cat src/conversion.rs | head -300 | tail -50
```

### Step 5: Understand the Root Cause

**Ask these questions:**

1. **What is actually happening?**
   - Read the code path
   - Trace the data flow
   - Check what values are being set

2. **Why is it happening?**
   - Is it a wrong assumption?
   - Is it a missing case?
   - Is it hardcoded when it should be configurable?

3. **What should happen instead?**
   - What does the backend expect?
   - What does the API spec say?
   - What do the logs show working cases doing?

### Step 6: Design the Fix

**Before coding, decide:**

1. **Scope:** Minimal change or broader fix?
2. **Approach:** Hardcode fix vs configurable vs conditional?
3. **Impact:** What else might this affect?

**Good fix characteristics:**
- ✅ Solves the root cause, not symptoms
- ✅ Configurable when backends differ
- ✅ Backwards compatible
- ✅ Well documented in code

### Step 7: Implement the Fix

**Files typically involved:**

| Issue Type | Files to Change |
|------------|-----------------|
| Config parameter | `src/config.rs`, `config-*.toml` |
| API conversion | `src/conversion.rs` |
| Request handling | `src/main.rs` |
| Data models | `src/models/gemini.rs`, `src/models/openai.rs` |
| Diagnostics | `src/diagnostic.rs` |

**Implementation checklist:**
- [ ] Update struct definitions if needed
- [ ] Update conversion logic
- [ ] Update config file with new settings
- [ ] Add comments explaining the fix
- [ ] Update default values if needed

### Step 8: Rebuild and Restart

```bash
# Kill old proxy
pkill -f "louter.*9000"

# Rebuild
cargo build --bin louter 2>&1 | grep -E "(Compiling|Finished|error)"

# Start with new code
cargo run --bin louter -- --host localhost --port 9000 \
  --config config-public-api.toml \
  --log-file /tmp/proxy-fixed.jsonl \
  --verbose > /tmp/startup.log 2>&1 &

# Check startup
sleep 5
tail /tmp/startup.log
```

### Step 9: Verify the Fix

```bash
# Run diagnostics again
curl -s http://localhost:9000/api/diagnostics > /tmp/diagnostics-after-fix.json

# Check if issue is resolved
cat /tmp/diagnostics-after-fix.json | jq '.frontends[].test_results[] |
  select(.test_name | contains("openai")) |
  "\(.test_name): \(if .passed then "✅ PASS" else "❌ FAIL" end)"'
```

**Verify checklist:**
- [ ] Error is gone
- [ ] Test passes
- [ ] No new errors introduced
- [ ] Logs show correct behavior

### Step 10: Document the Fix

**Create a resolved issue document:**

```bash
# Create documentation
cat > docs/resolved/$(date +%Y-%m-%d)-issue-name-RESOLVED.md << 'EOF'
# Issue Name - RESOLVED

**Resolved:** 2025-11-21
**Root Cause:** [Brief description]
**Solution:** [Brief description]

## Problem
[What was broken]

## Debugging Process
[Steps taken to find the issue]

## Root Cause
[What was actually wrong]

## Solution
[What was changed]

## Files Changed
- src/file.rs - [changes]

## Verification
[How we proved it works]
EOF
```

## Quick Reference Commands

```bash
# Standard debugging session
alias debug-start='cargo run --bin louter -- --host localhost --port 9000 --config config-public-api.toml --log-file /tmp/debug.jsonl --verbose'
alias debug-logs='tail -f /tmp/debug.jsonl | jq .'
alias debug-test='curl -s http://localhost:9000/api/diagnostics | jq . > /tmp/diag-$(date +%s).json'

# Common log queries
alias show-errors='grep -i error /tmp/debug.jsonl | jq .'
alias show-backends='grep backend_used /tmp/debug.jsonl | jq .'
alias show-conversions='grep conversion_mode /tmp/debug.jsonl | jq .'
```

## Common Debugging Patterns

### Pattern 1: "Request Failed" - Unknown Reason

```bash
# 1. Get diagnostics
curl -s http://localhost:9000/api/diagnostics > /tmp/d.json

# 2. Find failed test
cat /tmp/d.json | jq '.frontends[].test_results[] | select(.passed == false)'

# 3. Check logs for that test
grep "test_name_here" /tmp/debug.jsonl | jq .

# 4. Check backend response
grep "backend_response" /tmp/debug.jsonl | jq .
```

### Pattern 2: "Type Error" - Wrong Field Type

```bash
# 1. Get error message
cat /tmp/d.json | jq '.frontends[].test_results[] | select(.passed == false) | .error'

# 2. Find where field is set
grep -rn "field_name:" src/

# 3. Check conversion logic
cat src/conversion.rs | grep -A 10 "field_name"

# 4. Fix: Usually in conversion.rs or models
```

### Pattern 3: "MAX_TOKENS" - Not Enough Output Space

```bash
# 1. Check error response
cat /tmp/d.json | jq '.frontends[].test_results[] | select(.error | contains("MAX_TOKENS"))'

# 2. Check thinking tokens
grep thoughtsTokenCount /tmp/debug.jsonl | jq .

# 3. Fix: Increase token limits in diagnostic.rs
```

## Debugging Checklist

Before asking for help or declaring "it's broken":

- [ ] Did you save the FULL diagnostics output?
- [ ] Did you read the COMPLETE error message?
- [ ] Did you check the proxy logs?
- [ ] Did you grep the source code for relevant terms?
- [ ] Did you understand WHY it's failing (not just THAT it's failing)?
- [ ] Did you check if it's a known issue in `docs/issues/`?
- [ ] Did you check if it was fixed before in `docs/resolved/`?

## When to Create New Documentation

After fixing an issue, create docs if:

1. **The issue took >30 minutes to debug** → Create resolved/ document
2. **The debugging technique was useful** → Update this workflow
3. **The issue might recur** → Create debug/ guide
4. **The fix changes behavior** → Update design/ docs

## Related Documentation

- [api-type-error-max-tokens.md](api-type-error-max-tokens.md) - Real example of this workflow
- [debugging-protocol-conversion.md](debugging-protocol-conversion.md) - Conversion-specific debugging
- [using-diagnostic-api.md](using-diagnostic-api.md) - How to use /api/diagnostics
- [analyzing-json-logs.md](analyzing-json-logs.md) - Understanding log structure
