# Test Data - API Response Captures

This directory contains real API responses captured from all 3 backends.
These responses are used by the mock server for testing conversion logic.

## Directory Structure

```
test-data/
├── openai/                 # Public OpenAI API (gpt-5-nano)
│   ├── text/
│   ├── function_calling/
│   └── vision/
├── gemini/                 # Public Gemini API (gemini-2.5-flash)
│   ├── text/
│   ├── function_calling/
│   └── vision/
└── local-openai/          # Local llama-server (gpt-oss:20b)
    ├── text/
    └── function_calling/
```

## Captured Scenarios

### ✅ OpenAI Backend (3 scenarios)
1. Text generation - Simple text response
2. Function calling - Tool invocation with arguments
3. Vision - Image input processing

### ✅ Gemini Backend (3 scenarios)
1. Text generation - Simple text response
2. Function calling - functionCall format
3. Vision - inlineData with base64 image

### ✅ Local OpenAI Backend (2 scenarios)
1. Text generation - Local model response
2. Function calling - Tool support via llama-server

## Usage

### Capture New Responses
```bash
./scripts/capture-api-responses.sh
```

### Start Mock Server
```bash
python3 scripts/mock-server.py --port 8888
```

### Test with Mock Server
```bash
# Update config to point to mock server
# Run diagnostics against mock server
cargo run --bin diagnostic -- --backend mock --port 9001 --config config-mock.toml
```

## File Format

Each scenario has two files:
- `request.json` - The API request payload
- `response.json` - The API response payload

These can be used to:
1. Reproduce exact API behavior
2. Test conversion logic in isolation  
3. Debug format mismatches
4. Create regression tests
