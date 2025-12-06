# Testing Louter (LLM Router)

This directory contains comprehensive integration tests using official Python SDKs to verify protocol compliance for OpenAI, Anthropic, and Gemini APIs.

## Test Suites

### Python SDK Tests (New!)
Comprehensive protocol compliance tests using official SDKs:
- **test_openai_streaming.py** - OpenAI API compliance (13 tests)
- **test_anthropic_streaming.py** - Anthropic API compliance (15 tests)
- **test_gemini_streaming.py** - Gemini API compliance (10 tests)
- **run_all_tests.py** - Master test runner

See [Python SDK Tests](#python-sdk-tests-recommended) section below for details.

### Legacy Tests
This directory also contains older integration tests for testing both real APIs and your proxy implementation.

## Environment Variables

### Testing Real APIs (Default)
```bash
export GEMINI_API_KEY="your_gemini_api_key"
export OPENAI_API_KEY="your_openai_api_key"
```

### Testing Your Proxy
To test your proxy running on localhost:

```bash
# Point tests to your proxy instead of real APIs
export GEMINI_BASE_URL="http://localhost:8080"
export OPENAI_BASE_URL="http://localhost:8080"

# You may still need API keys depending on your proxy implementation
export GEMINI_API_KEY="your_gemini_api_key"
export OPENAI_API_KEY="your_openai_api_key"
```

## Running Tests

### Test Real APIs
```bash
# Test all integration tests against real APIs
cargo test --test api_integration_tests

# Test specific endpoint
cargo test --test api_integration_tests test_gemini_text_request
cargo test --test api_integration_tests test_openai_text_request
```

### Test Your Proxy
```bash
# Start your proxy server
cargo run -- --port 8080

# In another terminal, test against proxy
export GEMINI_BASE_URL="http://localhost:8080"
export OPENAI_BASE_URL="http://localhost:8080"
cargo test --test api_integration_tests
```

## Expected Proxy Behavior

Your proxy should:

1. **Accept Gemini-formatted requests** at Gemini endpoints
2. **Convert them to OpenAI format** and forward to real OpenAI API
3. **Convert OpenAI responses back** to Gemini format
4. **Support streaming** via SSE
5. **Handle multimedia** (images, etc.)
6. **Support function calling**

## Test Endpoints

The tests will hit these endpoints on your proxy:

### Gemini-Compatible Endpoints (what your proxy should expose):
- `POST /v1beta/models/gemini-2.0-flash:generateContent` - Text completions
- `POST /v1beta/models/gemini-2.0-flash:generateContent` - Vision requests (with images)
- `POST /v1beta/models/gemini-2.0-flash:streamGenerateContent` - Streaming

### OpenAI Endpoints (what your proxy should forward to):
- `POST /v1/chat/completions` - Text, vision, streaming

## Test Data

The tests use small multimedia files from `test-resources/`:
- `red_pixel.png` (70 bytes) - 1x1 red pixel
- `blue_square.jpg` (287 bytes) - 8x8 blue square  
- `test.pdf` (496 bytes) - Simple PDF with text
- `silence.wav` (844 bytes) - 0.1 second audio
- `colors.bmp` (70 bytes) - 2x2 pixel bitmap

These ensure your proxy correctly handles base64-encoded multimedia data.

## 3-Phase Testing Strategy (Public APIs)

### Phase 1: Direct API Testing ✅
```bash
./test-phase1.sh
```
Validates protocol models work with real Gemini and OpenAI APIs (should already pass).

### Phase 2: Proxy + OpenAI Backend
```bash
# Start proxy with OpenAI backend
cargo run -- --backend openai --port 8080

# Test in another terminal
./test-phase2.sh
```
Tests **Gemini → OpenAI conversion**: Gemini requests through proxy to real OpenAI API.

### Phase 3: Proxy + Gemini Backend  
```bash
# Start proxy with Gemini backend
cargo run -- --backend gemini --port 8080

# Test in another terminal
./test-phase3.sh
```
Tests **Gemini passthrough**: Gemini requests through proxy to real Gemini API.

## Development Priority

**Focus on public APIs first:**
1. ✅ Phase 1: Real API validation 
2. 🔄 Phase 2: Gemini → OpenAI conversion
3. 🔄 Phase 3: Gemini → Gemini passthrough

**Local model testing comes later** after the core proxy functionality is working with real APIs.

## Success Criteria

| Phase | Expected Results |
|-------|------------------|
| 1 | 6/6 tests pass (real APIs) |
| 2 | 3/6 tests pass (OpenAI through proxy) |
| 3 | 3/3 tests pass (OpenAI→Gemini conversion) |

When all phases pass, your proxy is fully functional! 🎉

---

## Python SDK Tests (Recommended)

### Quick Start

Install dependencies:
```bash
pip install openai anthropic google-generativeai
```

Run all tests:
```bash
# Sequential (default)
python tests/run_all_tests.py

# Parallel (faster)
python tests/run_all_tests.py --parallel

# Verbose output
python tests/run_all_tests.py --verbose
```

Run specific suites:
```bash
python tests/run_all_tests.py --openai      # OpenAI only
python tests/run_all_tests.py --anthropic   # Anthropic only
python tests/run_all_tests.py --gemini      # Gemini only
```

### Test Coverage

**OpenAI (13 tests)**
- Text streaming, reasoning tokens, function calling (single & parallel)
- Tool use round-trip, parameters (temperature, top_p, stop, max_tokens)
- System messages, role handling, chunk ID consistency

**Anthropic (15 tests)**
- Event sequence validation, content block indexing
- Tool use with id/name, multiple tools, round-trip
- Token tracking (input/output), stop reasons
- System messages, ping events, temperature

**Gemini (10 tests)**
- Text streaming, function calling, streaming consistency
- Tool use round-trip, multiple functions
- System instructions, temperature, max_output_tokens
- Chat history, empty response handling

### Environment Configuration

```bash
# OpenAI tests
export OPENAI_BASE_URL="http://localhost:9000"
export OPENAI_API_KEY="test-key"
export OPENAI_MODEL="gpt-4"

# Anthropic tests
export ANTHROPIC_BASE_URL="http://localhost:9000"
export ANTHROPIC_API_KEY="test-key"

# Gemini tests
export GOOGLE_GEMINI_BASE_URL="http://localhost:9000"
export GOOGLE_API_KEY="test-key"
```

### Individual Test Files

```bash
python tests/test_openai_streaming.py
python tests/test_anthropic_streaming.py
python tests/test_gemini_streaming.py
```

### Why Python SDK Tests?

1. **Real-world simulation** - Tests use the same SDKs that Claude Code, Cursor, and other agents use
2. **Protocol compliance** - Verifies that proxy implements specs correctly
3. **Comprehensive** - 38 total tests covering streaming, tools, parameters, edge cases
4. **Fast development** - Python tests are easier to write and debug than Rust integration tests
5. **CI/CD ready** - Can run in parallel for fast feedback

---

## Unit Tests

### XML Conversion Tests (`test_xml_conversion.rs`)
Tests XML to JSON tool call conversion for backends like Qwen3-Coder.

**When to run**:
- Before committing changes to `src/conversion.rs` (XML parsing)
- Before committing changes to `src/backends.rs` (tool format handling)
- Before creating a pull request
- After modifying tool format support

**How to run**:
```bash
# Run XML conversion tests
cargo test test_xml_conversion

# Run with output
cargo test test_xml_conversion -- --nocapture

# Run all unit tests
cargo test
```

**What it tests**:
- Single and multiple tool call extraction
- Complex parameters in XML format
- Text cleanup (XML tag removal)
- OpenAI format compatibility
- Edge cases (no tools, plain text)

---

## Integration Tests

### XML Conversion Integration Test (`test-xml-conversion.sh`)
End-to-end test for XML to JSON conversion with a real backend.

**When to run**:
- Before releases
- When testing with real Qwen3-Coder backend
- After major changes to XML conversion logic
- To verify complete XML→JSON workflow

**Prerequisites**:
```bash
# Optional: Start llama-server with Qwen3-Coder
./llama-server -m qwen3-coder.gguf --port 11212 --jinja
```

**How to run**:
```bash
# Run the integration test
./tests/test-xml-conversion.sh

# Test will work with or without a real backend
# - With backend: Full integration test
# - Without backend: Shows test setup and instructions
```

**What it tests**:
- Full proxy startup with XML configuration
- Real request/response with XML backend
- XML to JSON conversion in production
- Log parsing and verification
- Response format validation

---

## When to Run Tests

### Before Every Commit (Required)
```bash
cargo test
```
Runs all unit tests (fast, <10 seconds).

### Before Creating PR (Required)
```bash
# All unit tests
cargo test

# Check formatting and linting
cargo fmt --check
cargo clippy
```

### Before Release (Recommended)
```bash
# 1. All unit tests
cargo test

# 2. Integration tests (if backends available)
./tests/test-xml-conversion.sh

# 3. Build all binaries
cargo build --release --all-targets

# 4. Run diagnostic tool
cargo run --bin diagnostic -- --backend openai --port 9000

# 5. Check CI pipeline passes
```

### During Development (As Needed)
```bash
# Test specific feature
cargo test test_xml_to_json_conversion_single_tool -- --exact

# Test with verbose output
cargo test test_name -- --nocapture

# Test with backtrace
RUST_BACKTRACE=1 cargo test
```

---

## Test Files

```
tests/
├── README.md                      ← This file
├── api_integration_tests.rs       ← Integration tests for real APIs
├── test_xml_conversion.rs         ← Unit tests for XML conversion
├── test-xml-conversion.sh         ← Integration test for XML conversion
└── test-configs.md                ← Configuration examples for tests
```