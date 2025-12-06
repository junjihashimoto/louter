# Quick Start Guide - Testing Your Proxy

This guide shows you how to run comprehensive protocol compliance tests against your proxy using official Python SDKs.

## 1. Install Dependencies

```bash
pip install openai anthropic google-generativeai
```

## 2. Start Your Proxy

Start the Haskell proxy server (or your Rust proxy):

```bash
# Haskell proxy
cd /path/to/gemini-proxy
cabal run louter-server -- -c ./examples/llama-cpp-config.toml

# Or run the mock server for testing
cd haskell
stack run openai-mock -- --test-data ../test-data --port 11211
```

The proxy should be running on `http://localhost:9000`.

## 3. Run Tests

### Option A: Run All Tests (Recommended)

```bash
# Run all 38 tests across all protocols
python tests/run_all_tests.py

# Or run in parallel for faster execution
python tests/run_all_tests.py --parallel
```

Expected output:
```
======================================================================
Multi-Protocol LLM Proxy Test Runner
======================================================================
Test suites: OpenAI API, Anthropic API, Gemini API
Mode: Sequential
Verbose: False

======================================================================
Running OpenAI API Tests
======================================================================
=== Test 1: Text-Only Streaming ===
✅ PASS: Text-only streaming
   Chunks: 5
   Content: "Hello! How can I assist you today?..."
   Finish reason: stop

...

======================================================================
Test Summary
======================================================================
✅ PASS: OpenAI API
✅ PASS: Anthropic API
✅ PASS: Gemini API

3/3 test suites passed
```

### Option B: Run Individual Protocol Tests

```bash
# Test only OpenAI compatibility
python tests/run_all_tests.py --openai

# Test only Anthropic compatibility
python tests/run_all_tests.py --anthropic

# Test only Gemini compatibility
python tests/run_all_tests.py --gemini
```

### Option C: Run Test Files Directly

```bash
# Run OpenAI tests (13 tests)
python tests/test_openai_streaming.py

# Run Anthropic tests (15 tests)
python tests/test_anthropic_streaming.py

# Run Gemini tests (10 tests)
python tests/test_gemini_streaming.py
```

## 4. Understanding Test Results

### Pass ✅
```
✅ PASS: Text-only streaming
   Chunks: 5
   Content: "Hello! How can I assist you today?..."
```
The test passed! Your proxy correctly handles this feature.

### Skip ⚠️
```
⚠️  SKIP: Backend did not return tool call (returned text instead)
```
The test was skipped because the backend made a different choice (e.g., returned text instead of using a tool). This is **not a failure** - it's expected backend-specific behavior.

### Fail ❌
```
❌ FAIL: No content or reasoning received
```
The test failed. Check proxy logs for errors. Common issues:
- Protocol conversion error
- SSE parsing error
- Missing required fields

## 5. Test Coverage Summary

| Protocol | Tests | Coverage |
|----------|-------|----------|
| **OpenAI** | 13 | Text streaming, reasoning tokens, function calling (single & parallel), tool roundtrip, parameters |
| **Anthropic** | 15 | Event sequences, content blocks, tool use, token tracking, stop reasons |
| **Gemini** | 10 | Text streaming, function calling, tool roundtrip, system instructions, chat history |
| **Total** | **38** | Comprehensive protocol compliance |

## 6. Testing Against llama-server

If you're testing against a local llama-server:

```bash
# 1. Start llama-server
llama-server -m /path/to/model.gguf --port 11211

# 2. Configure environment
export OPENAI_BASE_URL="http://localhost:11211"
export OPENAI_API_KEY=""  # Empty for llama-server

# 3. Run OpenAI tests only (llama-server is OpenAI-compatible)
python tests/test_openai_streaming.py
```

## 7. Common Issues

### Connection Refused
**Problem:** Tests fail with connection errors
**Solution:** Make sure proxy is running on the expected port (default: 9000)

```bash
# Check if proxy is running
curl http://localhost:9000/health || echo "Proxy not responding"
```

### Import Errors
**Problem:** `ModuleNotFoundError: No module named 'openai'`
**Solution:** Install the required SDK

```bash
pip install openai anthropic google-generativeai
```

### Many Tests Skipped
**Problem:** Most function calling tests show `⚠️  SKIP`
**Solution:** This is normal if your backend model doesn't support function calling or prefers to answer with text. Try with a model that supports tools (e.g., gpt-4, claude-3, gemini-pro).

### All Tests Fail
**Problem:** Every test fails immediately
**Solution:** Check that:
1. Proxy is running (`http://localhost:9000`)
2. Backend is configured correctly
3. Environment variables are set if needed

## 8. Debugging Failed Tests

### Verbose Mode
Run with `--verbose` to see full test output:

```bash
python tests/run_all_tests.py --verbose
```

### Check Proxy Logs
Look at your proxy's console output for error messages.

### Run Single Test
Run a specific test file to isolate the issue:

```bash
python tests/test_openai_streaming.py
```

### Minimal Test
Create a minimal test to debug:

```python
from openai import OpenAI

client = OpenAI(api_key="test-key", base_url="http://localhost:9000")
response = client.chat.completions.create(
    model="gpt-4",
    messages=[{"role": "user", "content": "Hi"}],
    max_tokens=50
)
print(response.choices[0].message.content)
```

## 9. CI/CD Integration

Add to your GitHub Actions workflow:

```yaml
name: Protocol Tests
on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: actions/setup-python@v4
        with:
          python-version: '3.11'

      - name: Install dependencies
        run: pip install openai anthropic google-generativeai

      - name: Start proxy
        run: |
          cargo run --release -- --port 9000 &
          sleep 5

      - name: Run tests
        run: python tests/run_all_tests.py --parallel
```

## 10. Next Steps

Once all tests pass:
1. ✅ Your proxy correctly implements all three protocols
2. ✅ It can handle streaming, function calling, and various parameters
3. ✅ Real coding agents (Claude Code, Cursor, etc.) should work with it

If some tests fail:
1. Check which specific features are failing
2. Review the protocol conversion code for those features
3. Compare proxy output with expected SDK behavior
4. Use `--verbose` mode and proxy logs to debug

## Need Help?

- Check `tests/README.md` for detailed documentation
- Review individual test files to understand what they verify
- Open an issue with test output and proxy logs
