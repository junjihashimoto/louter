# Mock Server Status

## ✅ Mock Server Running Successfully!

**Port**: 8888
**Loaded Responses**: 8
**Status**: ✅ ALL TESTS PASSING

## Test Results

### All 8 Scenarios Working

```bash
$ bash test-mock-server.sh
```

1. ✅ **OpenAI - Text Generation**: "Hello! How can I help you today?"
2. ✅ **OpenAI - Function Calling**: Returns "search" function call
3. ✅ **OpenAI - Vision**: Describes white image
4. ✅ **Gemini - Text Generation**: "Hello!"
5. ✅ **Gemini - Function Calling**: Returns "search" function call
6. ✅ **Gemini - Vision**: Describes green image
7. ✅ **Local OpenAI - Text Generation**: "Hello there! How can I help you today?"
8. ✅ **Local OpenAI - Function Calling**: Returns "search" function call

## What Was Fixed

### Issue
Gemini function_calling and vision responses had empty `parts` arrays due to `maxOutputTokens: 50` being too small.

### Solution
Recaptured responses with `maxOutputTokens: 200`:
- `test-data/gemini/function_calling/response.json` - Now has actual functionCall
- `test-data/gemini/vision/response.json` - Now has actual text description

## Loaded Mock Responses

All responses successfully loaded from `test-data/`:

1. ✅ openai/text
2. ✅ openai/function_calling
3. ✅ openai/vision
4. ✅ gemini/text
5. ✅ gemini/function_calling
6. ✅ gemini/vision
7. ✅ local-openai/text
8. ✅ local-openai/function_calling

## Mock Server Architecture

```
Mock Server (port 8888)
├── Serves all 3 backends:
│   ├── OpenAI responses (gpt-5-nano)
│   ├── Gemini responses (gemini-2.5-flash)
│   └── Local OpenAI responses (gpt-oss:20b)
│
├── Request Detection:
│   ├── Analyzes request payload
│   ├── Detects backend (model name)
│   ├── Detects capability (tools, vision, text)
│   └── Returns matching response from test-data/
│
└── Response Files:
    └── test-data/{backend}/{capability}/response.json
```

## Usage

### Start Mock Server
```bash
cargo run --bin mock-server -- --port 8888 --data-dir test-data --verbose
```

### Test All Endpoints
```bash
bash test-mock-server.sh
```

### Test Individual Endpoints

```bash
# OpenAI text
curl -X POST http://localhost:8888/v1/chat/completions \
  -H "Content-Type: application/json" \
  -d '{"model":"gpt-5-nano","messages":[{"role":"user","content":"Hi"}]}'

# Gemini text
curl -X POST http://localhost:8888/v1beta/models/gemini-2.5-flash:generateContent \
  -H "Content-Type: application/json" \
  -d '{"contents":[{"parts":[{"text":"Hi"}],"role":"user"}]}'

# Gemini function calling
curl -X POST http://localhost:8888/v1beta/models/gemini-2.5-flash:generateContent \
  -H "Content-Type: application/json" \
  -d '{"contents":[{"parts":[{"text":"Search"}],"role":"user"}],"tools":[{"functionDeclarations":[{"name":"search"}]}]}'
```

## Next Steps: Frontend Debugging

Now that mock server is fully operational, we can use it to debug the **Gemini API → OpenAI backend** conversion issue.

### Known Issue to Debug

**Error**: `models/gpt-5-nano is not found for API version v1beta`

**What's happening**: When a Gemini API request comes in for model "gpt-5-nano", the proxy tries to route it to a Gemini backend instead of:
1. Detecting that gpt-5-nano is an OpenAI model
2. Converting the Gemini request to OpenAI format
3. Routing to the OpenAI backend

### Debug Approach

1. **Configure proxy to use mock backends**:
   ```toml
   # config-mock.toml
   [backends.openai]
   url = "http://localhost:8888"

   [backends.gemini]
   url = "http://localhost:8888"
   ```

2. **Start proxy with mock config**:
   ```bash
   cargo run --bin louter -- --backend openai --port 9001 --config config-mock.toml --verbose
   ```

3. **Send Gemini API request for OpenAI model**:
   ```bash
   curl -X POST 'http://localhost:9001/v1beta/models/gpt-5-nano:generateContent' \
     -H "Content-Type: application/json" \
     -d '{"contents":[{"parts":[{"text":"Hi"}],"role":"user"}]}'
   ```

4. **Expected behavior**:
   - Proxy detects gpt-5-nano is an OpenAI model
   - Converts Gemini request → OpenAI format
   - Routes to OpenAI backend (mock server)
   - Mock server returns OpenAI response
   - Proxy converts OpenAI response → Gemini format
   - Returns Gemini-formatted response to client

## Benefits

✅ **Deterministic** - Same request always returns same response
✅ **Fast** - No network latency
✅ **Free** - No API costs
✅ **Reproducible** - Can debug exact conversion logic
✅ **Isolated** - Test without affecting real APIs
✅ **Complete** - All 8 scenarios tested and working
