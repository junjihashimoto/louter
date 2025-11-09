# Cross-Protocol Routing Fix - Summary

**Date**: 2025-11-21
**Issue**: OpenAI API requests for Gemini models were not routing to Gemini backend
**Status**: ✅ **FIXED**

## Problem

When an OpenAI API request came in for a Gemini model (e.g., `gemini-2.5-flash`), the proxy would route it to the OpenAI backend instead of:
1. Detecting that the model belongs to the Gemini backend
2. Converting the OpenAI request to Gemini format
3. Routing to the Gemini backend
4. Converting the response back to OpenAI format

### Root Cause

The routing logic in `handle_openai_chat_completions` (src/main.rs:825) only considered **capabilities**, not the **model name**:

```rust
// OLD CODE (capability-based routing only)
let selected_backend = routing::select_backend_for_request(&required_caps, &state.config)?;
```

This function didn't receive the model name, so it had no way to know which backend the model belonged to.

## Solution

Added **model-based routing** before capability-based routing:

### 1. New Function: `find_backend_for_model` (src/routing.rs:211-220)

```rust
/// Find which backend a model belongs to by checking model_mapping
pub fn find_backend_for_model(model: &str, config: &Config) -> Option<String> {
    for (backend_name, backend_config) in &config.backends {
        if backend_config.model_mapping.contains_key(model) {
            return Some(backend_name.clone());
        }
    }
    None
}
```

### 2. Made `backend_supports_capabilities` Public (src/routing.rs:114)

Changed from `fn` to `pub fn` to allow capability verification from main.rs.

### 3. Updated Routing Logic (src/main.rs:824-916)

New routing strategy:

```
1. Try model-based routing first
   ├─ Find backend by model name in model_mapping
   ├─ Verify backend supports required capabilities
   └─ If yes, use that backend

2. Fall back to capability-based routing
   ├─ Select backend by capabilities and priority
   └─ Use first compatible backend

3. Last resort: Fall back to command-line --backend option
```

## Testing

### Test Setup

- **Mock Server**: Port 8888 with 8 response scenarios
- **Proxy**: Port 9001 with `config-mock.toml`
- **Test Script**: `test-cross-protocol.sh`

### Test Results

All 4 cross-protocol scenarios now working:

| # | Frontend API | Backend API | Model | Capability | Status |
|---|--------------|-------------|-------|------------|--------|
| 1 | Gemini | OpenAI | gpt-5-nano | Text | ✅ PASS |
| 2 | Gemini | OpenAI | gpt-5-nano | Function Calling | ✅ PASS |
| 3 | OpenAI | Gemini | gemini-2.5-flash | Text | ✅ PASS |
| 4 | OpenAI | Gemini | gemini-2.5-flash | Function Calling | ✅ PASS |

### Test Output

```bash
$ bash test-cross-protocol.sh
========================================
Cross-Protocol Conversion Tests
========================================

1. Gemini API → OpenAI Backend (Text)
Hello! How can I help you today?
---

2. Gemini API → OpenAI Backend (Function Calling)
search
---

3. OpenAI API → Gemini Backend (Text)
Hello!
---

4. OpenAI API → Gemini Backend (Function Calling)
search
---

✅ All Tests Complete!
```

### Mock Server Logs Confirm Correct Routing

```
Detected scenario: openai/text          # Test 1: Gemini→OpenAI
Detected scenario: openai/function_calling  # Test 2: Gemini→OpenAI
Detected scenario: gemini/text          # Test 3: OpenAI→Gemini  ← FIXED!
Detected scenario: gemini/function_calling  # Test 4: OpenAI→Gemini  ← FIXED!
```

**Before fix**: Tests 3 & 4 were routed to `openai/*` (wrong)
**After fix**: Tests 3 & 4 correctly route to `gemini/*` ✅

## Proxy Logs Show Model-Based Routing

```
INFO OpenAI chat completions request for model: gemini-2.5-flash
INFO ✓ Model 'gemini-2.5-flash' mapped to backend 'gemini'
INFO   ✓ Backend 'gemini' supports required capabilities
```

## Files Changed

### src/routing.rs
- **Added** `find_backend_for_model()` - Searches all backends for model mapping
- **Changed** `backend_supports_capabilities()` from private to public

### src/main.rs
- **Updated** `handle_openai_chat_completions()` routing logic (lines 824-916)
- Added model-based routing before capability-based routing
- Added capability verification for model-mapped backends
- Added detailed logging for routing decisions

## Backwards Compatibility

✅ **Fully backwards compatible**

- Existing capability-based routing still works as fallback
- No config changes required
- Existing deployments continue to work
- New model-based routing is opt-in (via model_mapping)

## Configuration Example

```toml
[backends.openai]
url = "https://api.openai.com"
protocol = "openai"
[backends.openai.model_mapping]
"gpt-5-nano" = "gpt-5-nano"
"gpt-4" = "gpt-4"

[backends.gemini]
url = "https://generativelanguage.googleapis.com"
protocol = "gemini"
[backends.gemini.model_mapping]
"gemini-2.5-flash" = "gemini-2.5-flash"
"gemini-2.0-flash" = "gemini-2.0-flash-exp"
```

## Benefits

1. **Cross-Protocol Conversion**: Full bidirectional support
   - Gemini API → OpenAI backend ✅
   - OpenAI API → Gemini backend ✅

2. **Model-Based Routing**: Explicit control over which backend handles which model

3. **Intelligent Fallback**: If model mapping not found, falls back to capability-based routing

4. **Better Logging**: Clear visibility into routing decisions

## Next Steps

- ✅ **Done**: Model-based routing for OpenAI API frontend
- 📋 **TODO**: Consider adding same logic to Gemini API frontend for consistency
- 📋 **TODO**: Add routing metrics (model_mapping vs capability vs fallback)
- 📋 **TODO**: Add unit tests for `find_backend_for_model()`

## Metrics

**Routing Decision Modes** (tracked in Prometheus):
- `model_mapping` - Model found in backend's model_mapping
- `capability` - Fallback to capability-based routing
- `explicit` - Capability routing with explicit backend capabilities
- `auto` - Capability routing with auto mode backend
- `failed` - Routing failed, using command-line backend

## Conclusion

The cross-protocol routing fix enables seamless bidirectional conversion between Gemini and OpenAI API formats. Users can now:
- Access OpenAI models through Gemini API syntax
- Access Gemini models through OpenAI API syntax
- Use model_mapping for explicit backend assignment
- Rely on automatic capability-based routing as fallback

**All 4 cross-protocol scenarios are now fully functional!** 🎉
