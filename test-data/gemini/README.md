# Gemini API Test Data

Gemini supports **two streaming formats**:

## 1. JSON Array Format (`alt=json`)

**Directory:** `streaming_jsonarray/`

**Format:** Returns a JSON array where each element is a complete response chunk:
```json
[
  {"candidates": [...], "usageMetadata": {...}},
  {"candidates": [...], "finishReason": "STOP"}
]
```

**Used by:**
- Direct API calls with `?alt=json`
- HTTP clients that can parse streaming JSON arrays

**⚠️ Python SDK Compatibility:**
The official Google Generative AI Python SDK has known issues with JSON array streaming format due to HTTP chunked transfer encoding. Use SSE format (`alt=sse`) with the Python SDK instead.

**Example request:**
```bash
curl "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:streamGenerateContent?alt=json" \
  -H "x-goog-api-key: $GEMINI_API_KEY" \
  -H 'Content-Type: application/json' \
  -d '{"contents": [{"parts": [{"text": "Hello"}]}]}'
```

## 2. Server-Sent Events Format (`alt=sse`)

**Directory:** `streaming_sse/`

**Format:** Returns SSE (Server-Sent Events) with `data:` prefix:
```
data: {"candidates": [...], "usageMetadata": {...}}

data: {"candidates": [...], "finishReason": "STOP"}

```

**Used by:**
- **Python SDK (recommended)** - Works reliably with streaming
- Web applications
- Direct API calls with `?alt=sse`
- EventSource API in browsers

**Example request:**
```bash
curl "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:streamGenerateContent?alt=sse" \
  -H "x-goog-api-key: $GEMINI_API_KEY" \
  -H 'Content-Type: application/json' \
  -d '{"contents": [{"parts": [{"text": "Hello"}]}]}'
```

## Key Differences

| Feature | JSON Array | SSE |
|---------|-----------|-----|
| Content-Type | application/json | text/event-stream |
| Format | `[{...}, {...}]` | `data: {...}\n\ndata: {...}\n\n` |
| Parsing | Parse entire JSON array | Parse line-by-line SSE |
| Browser API | Fetch API | EventSource API |
| Python SDK | ❌ Not compatible | ✅ Works reliably |

## Implementation Notes

- **louter proxy** must support BOTH formats
- Format is determined by `?alt=` query parameter
- Both formats contain identical JSON payload structure
- Only the transport/wrapping differs
- SSE requires proper newline handling (`\n\n` between events)
- JSON array requires proper array parsing (comma separation)

## Test Files

### JSON Array Format (`streaming_jsonarray/`)
- `sample_request.json` - Example request payload
- `sample_response.txt` - Example streaming response (JSON array)
- `test.sh` - Test script using curl

### SSE Format (`streaming_sse/`)
- `sample_request.json` - Example request payload
- `sample_response.txt` - Example streaming response (SSE)
- `test.sh` - Test script using curl

## Test Coverage

- ✅ `streaming_jsonarray/` - JSON array format test data (curl validation)
- ✅ `streaming_sse/` - SSE format test data
- ✅ `tests/test_gemini_sse_streaming.py` - Python SDK tests (SSE format)
- ✅ Both formats implemented in louter proxy

## Usage

Test JSON array format:
```bash
cd test-data/gemini/streaming_jsonarray
./test.sh
```

Test SSE format:
```bash
cd test-data/gemini/streaming_sse
./test.sh
```
