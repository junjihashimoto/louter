# Mock Server Test Plan

## Goal
Debug cross-protocol conversion logic using mock servers instead of real APIs.
Store actual request/response pairs for systematic testing.

## Test Matrix

### 3 Backends
1. **Public OpenAI** (https://api.openai.com) - Protocol: OpenAI
2. **Public Gemini** (https://generativelanguage.googleapis.com) - Protocol: Gemini
3. **Local OpenAI** (http://localhost:11211) - Protocol: OpenAI (llama-server)

### 3 Capabilities
1. **Text** - Simple text generation
2. **Function Calling** - Tool/function invocation
3. **Vision** - Image input processing

### Test Scenarios (3 backends × 3 capabilities = 9 scenarios)

#### OpenAI Backend Tests
- ✅ OpenAI API → OpenAI backend: Text (native)
- ✅ OpenAI API → OpenAI backend: Function Calling (native)
- ✅ OpenAI API → OpenAI backend: Vision (native)
- ❌ Gemini API → OpenAI backend: Text (conversion)
- ❌ Gemini API → OpenAI backend: Function Calling (conversion)
- ❌ Gemini API → OpenAI backend: Vision (conversion)

#### Gemini Backend Tests
- ✅ Gemini API → Gemini backend: Text (native)
- ✅ Gemini API → Gemini backend: Function Calling (native)
- ✅ Gemini API → Gemini backend: Vision (native)
- ✅ OpenAI API → Gemini backend: Text (conversion)
- ✅ OpenAI API → Gemini backend: Function Calling (conversion)
- ✅ OpenAI API → Gemini backend: Vision (conversion)

#### Local OpenAI Backend Tests
- ✅ OpenAI API → Local OpenAI: Text (native)
- ✅ OpenAI API → Local OpenAI: Function Calling (native)
- ❌ Gemini API → Local OpenAI: Text (conversion)
- ❌ Gemini API → Local OpenAI: Function Calling (conversion)

## Test Data Structure

```
test-data/
├── openai/           # Public OpenAI responses
│   ├── text/
│   │   ├── request.json      # OpenAI format request
│   │   └── response.json     # OpenAI format response
│   ├── function_calling/
│   │   ├── request.json
│   │   └── response.json
│   └── vision/
│       ├── request.json
│       └── response.json
├── gemini/           # Public Gemini responses
│   ├── text/
│   │   ├── request.json      # Gemini format request
│   │   └── response.json     # Gemini format response
│   ├── function_calling/
│   │   ├── request.json
│   │   └── response.json
│   └── vision/
│       ├── request.json
│       └── response.json
└── local-openai/     # Local llama-server responses
    ├── text/
    │   ├── request.json
    │   └── response.json
    └── function_calling/
        ├── request.json
        └── response.json
```

## Mock Server Implementation

### Approach 1: Python Flask Mock Server
- Single script that serves all 3 backends
- Routes based on path and backend type
- Returns stored response files

### Approach 2: Multiple Mock Servers
- One server per backend (3 servers)
- Each on different port
- More realistic simulation

## Conversion Testing Strategy

1. **Capture Real Responses** - Store actual API responses from each backend
2. **Mock Server Returns Real Data** - Serve captured responses
3. **Test Conversion Logic** - Verify proxy converts correctly between formats
4. **Debug Issues** - Compare expected vs actual conversions

## Implementation Steps

1. ✅ Create test data directory structure
2. ⏳ Capture real API responses for each scenario
3. ⏳ Create mock server script
4. ⏳ Update diagnostic tests to use mock server
5. ⏳ Debug and fix conversion issues
6. ⏳ Document all working scenarios

## Known Issues to Debug

### Gemini API → OpenAI Backend (Conversion Mode)
**Error**: `models/gpt-5-nano is not found for API version v1beta`

**Root Cause**: When Gemini API request comes in, the proxy tries to send it to Gemini backend instead of converting to OpenAI format and routing to OpenAI backend.

**Fix Needed**: Update routing logic to:
1. Detect request format (Gemini vs OpenAI)
2. Look up backend protocol from model name
3. Convert request format if needed
4. Route to correct backend

### Gemini Native Mode - Edge Cases
**Error**: `missing field 'content'` or `missing field 'parts'`

**Root Cause**: Gemini API returns different response structures for:
- MAX_TOKENS finish reason (empty parts)
- Safety blocks (missing content)
- Malformed function calls (missing content)

**Fix**: ✅ Make Content.parts and Candidate.content optional with defaults
