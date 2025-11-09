# Debugging Notes - Function Calling Issue

## Current Issue
gemini-cli + louter + llama.cpp function calling is failing. The model generates incorrect function call format.

## Problem Details

### Expected Behavior
Model should generate JSON function calls like:
```json
{
  "functionCall": {
    "name": "write_file",
    "args": {
      "absolute_path": "/path/to/file",
      "content": "file content"
    }
  }
}
```

### Actual Behavior
Model generates XML-style format:
```xml
<tool_call>
<function=write_file
<parameter=absolute_path>
/Users/junji.hashimoto/git/gemini-cli/touch.yaml
</parameter>
<parameter=content>
...
</parameter>
</
```

## Root Causes Identified

### 1. Tool Filtering Bug (CRITICAL)
**Config**: `allowed_tools = ["write_file", "read_file", "run_shell_command"]`

**Expected**: Keep all 3 tools
**Actual**: Only keeps `["read_file"]`

**Log Evidence**:
```
Filtering tools. Before: 8, Allowed: ["write_file", "read_file", "run_shell_command"]
After filtering: 1 tools
Remaining tools: ["read_file"]
```

**Diagnosis**: Tool names in config don't match actual tool names sent by gemini-cli

### 2. Model Generates Wrong Format
Even when only `read_file` is available, model generates XML-style parameters instead of JSON.

## What Works

### Direct llama.cpp Test (✅ SUCCESS)
```bash
cargo run --bin test-runner --quiet -- openai --url http://localhost:11211 --test-type function
```

**Result**: Correctly generates `{"arguments": "{\"location\":\"Tokyo\"}"}`

**Conclusion**: llama.cpp CAN generate proper function arguments when:
- Simple tool set (1 function)
- Clean conversation history
- Proper system instructions

## Optimizations Implemented

### 1. Tool Filtering (src/conversion.rs:220-229)
Reduces tools from 11 → 3 to help smaller models.
**Status**: Implemented but has tool name mismatch bug

### 2. System Instruction Override (src/conversion.rs:24-48)
Replaces gemini-cli's 8000+ token instruction with:
```
"You are a helpful CLI assistant. When asked to write a file, use write_file with absolute file_path and content parameters."
```
**Status**: Implemented and working

### 3. Conversation History Limiting (src/conversion.rs:195-214)
Keeps only system message + last 2 messages to prevent error accumulation.
**Config**: `max_conversation_turns = 2`
**Status**: Implemented and working

## Testing Commands

### Start Proxy
```bash
cargo run --bin louter -- --backend openai --port 8080 --config llama-cpp-config.toml 2>&1 | tee /tmp/proxy-debug.log
```

### Test with gemini-cli
```bash
cd /Users/junji.hashimoto/git/gemini-cli
GEMINI_DEFAULT_AUTH_TYPE=gemini-api-key \
GOOGLE_GEMINI_BASE_URL=http://localhost:8080 \
node scripts/start.js --model gemma3-4b -p "Write the touch.yaml file"
```

### Test llama.cpp Directly
```bash
cargo run --bin test-runner --quiet -- openai --url http://localhost:11211 --test-type function
```

### Check Logs
```bash
# See filtering in action
grep "Filtering tools" /tmp/proxy-debug.log

# See tool names
grep -o '"name":"[^"]*"' /tmp/proxy-debug.log | sort -u

# Check full request
tail -100 /tmp/proxy-debug.log
```

## Completed Steps ✅

### 1. Discovered Actual Tool Names ✅
gemini-cli sends these 8 tools:
```
["list_directory", "read_file", "search_file_content", "glob", "web_fetch", "read_many_files", "save_memory", "google_web_search"]
```

**Problem Found**: Config had `["write_file", "read_file", "run_shell_command"]` but only `read_file` matched!

### 2. Fixed Tool Filtering ✅
Updated `llama-cpp-config.toml`:
```toml
allowed_tools = ["list_directory", "read_file", "save_memory"]
```
Reduced from 8 tools → 3 tools to help smaller model.

### 3. Added Multi-Format Function Call Parser ✅
Implemented in `src/conversion.rs` (lines 376-434):
- Detects XML-style format: `<tool_call><function=name<parameter=key>value</parameter>`
- Detects JSON-in-text format: `{"name": "...", "arguments": {...}}`
- Converts alternative formats to standard OpenAI tool_calls
- Shows warnings when non-standard formats are detected

**Benefits**:
- More robust - doesn't rely on model following instructions perfectly
- Supports multiple model output formats
- Clear warnings when alternative formats are used

### 4. Current Status
- ✅ Tool filtering working (8 → 3 tools)
- ✅ System instruction override working
- ✅ Conversation history limiting working
- ✅ Multi-format parser implemented
- ⚠️  Model still struggles to generate complete function calls (generates incomplete XML)

## ✅ RESOLVED - Function Calling Now Works!

### Final Solution: Stateful Chunk Accumulation with Index Tracking
- **Root cause**: OpenAI streaming sends partial JSON chunks with different IDs
- **Key insight**: Use `index` field (not `id`) to track chunks of same tool call
- **Implementation**: Stateful accumulator using `stream.scan()` to concatenate fragments
- **Result**: ✅ Complete function calls with all parameters!

### Working End-to-End
- ✅ gpt-oss model generates complete function calls
- ✅ Proxy accumulates streaming chunks by index
- ✅ Emits function calls only when JSON is complete
- ✅ gemini-cli successfully executes function calls

### See Full Documentation
📖 **[FUNCTION-CALLING-DEBUG.md](./FUNCTION-CALLING-DEBUG.md)** - Comprehensive debugging guide

## Key Files

- **src/conversion.rs**: Conversion logic with filtering and limiting
- **src/config.rs**: Configuration structure
- **llama-cpp-config.toml**: Current configuration
- **/tmp/proxy-debug.log**: Runtime logs

## Log Files
- `/tmp/proxy-optimized.log` - Latest test run
- `/tmp/proxy-filtered.log` - Previous test with 11 tools visible
- Test logs in `/Users/junji.hashimoto/git/louter/*.jsonl`

## Hypothesis

The smaller model (gemma3-4b) needs:
1. ✅ Reduced tool count (3 instead of 11)
2. ✅ Short system instructions (not 8000+ tokens)
3. ✅ Clean conversation history (no error accumulation)
4. ❌ **BLOCKED**: Tool filtering removes wrong tools due to name mismatch

Once tool filtering is fixed, all three optimizations should work together to enable successful function calling.
