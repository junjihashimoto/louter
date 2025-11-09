# JSON Lines Log Format

**Purpose:** Document the schema for proxy request/response logging
**Format:** JSON Lines (one JSON object per line)

## Log Entry Schema

```json
{
  "timestamp": "2025-11-22T10:30:45.123Z",
  "direction": "client_request|backend_request|backend_response|client_response",
  "format": "gemini|openai",
  "endpoint": "/v1beta/models/gemini-2.0-flash:generateContent",
  "body": { /* Full request/response body */ },

  // Performance metrics (only in responses)
  "ttft_ms": 123.45,           // Time to first token (milliseconds)
  "tps": 42.5,                 // Tokens per second
  "itl_ms": 25.3,              // Inter-token latency (milliseconds)
  "e2e_latency_ms": 1234.56,   // End-to-end latency (milliseconds)
  "output_tokens": 150,        // Number of output tokens
  "input_tokens": 50,          // Number of input tokens

  // Routing information (only in backend requests)
  "backend_used": "openai",    // Which backend handled this request

  // Error information (only when errors occur)
  "error": "Error message"
}
```

## Field Descriptions

### Core Fields (Always Present)

| Field | Type | Description | Example |
|-------|------|-------------|---------|
| `timestamp` | string | ISO 8601 timestamp with milliseconds | `"2025-11-22T10:30:45.123Z"` |
| `direction` | string | Request/response flow stage | `"client_request"` |
| `format` | string | API protocol format | `"gemini"` or `"openai"` |
| `endpoint` | string | API endpoint path | `"/v1beta/models/gemini-2.0-flash:generateContent"` |
| `body` | object | Full JSON request/response payload | See API docs |

### Direction Values

| Value | Description | When Logged |
|-------|-------------|-------------|
| `client_request` | Request from client to proxy | When proxy receives request |
| `backend_request` | Request from proxy to backend | Before sending to LLM backend |
| `backend_response` | Response from backend to proxy | After receiving from LLM backend |
| `client_response` | Response from proxy to client | Before sending response to client |

### Performance Metrics (Response Only)

| Field | Type | Unit | Description |
|-------|------|------|-------------|
| `ttft_ms` | float | milliseconds | Time from request start to first token |
| `tps` | float | tokens/sec | Token generation throughput |
| `itl_ms` | float | milliseconds | Average time between tokens |
| `e2e_latency_ms` | float | milliseconds | Total request duration |
| `output_tokens` | integer | tokens | Number of generated tokens |
| `input_tokens` | integer | tokens | Number of input tokens |

### Routing Information

| Field | Type | Description |
|-------|------|-------------|
| `backend_used` | string | Name of backend that handled the request |

### Error Information

| Field | Type | Description |
|-------|------|-------------|
| `error` | string | Error message (only present when error occurs) |

## Common Query Patterns

### Using jq (Manual)

```bash
# Show recent requests
tail -20 proxy.jsonl | jq .

# Filter by direction
cat proxy.jsonl | jq 'select(.direction == "client_request")'

# Show only errors
cat proxy.jsonl | jq 'select(.error != null)'

# Calculate average TPS
cat proxy.jsonl | jq -s '[.[] | select(.tps != null) | .tps] | add / length'

# Group by backend
cat proxy.jsonl | jq -s 'group_by(.backend_used) | map({backend: .[0].backend_used, count: length})'
```

### Using log-parser (Recommended)

```bash
# Show all entries
cargo run --bin log-parser -- --file proxy.jsonl all

# Show statistics
cargo run --bin log-parser -- --file proxy.jsonl stats

# Show only function calling entries
cargo run --bin log-parser -- --file proxy.jsonl functions

# Filter by direction and format
cargo run --bin log-parser -- --file proxy.jsonl filter --direction client_request --format gemini

# Show request/response pairs
cargo run --bin log-parser -- --file proxy.jsonl pairs

# Show only pairs with function calls
cargo run --bin log-parser -- --file proxy.jsonl pairs --functions-only
```

## Example Log Entries

### Client Request (Gemini API)
```json
{
  "timestamp": "2025-11-22T10:30:45.123Z",
  "direction": "client_request",
  "format": "gemini",
  "endpoint": "/v1beta/models/gemini-2.0-flash:generateContent",
  "body": {
    "contents": [{
      "parts": [{"text": "What is the capital of France?"}],
      "role": "user"
    }]
  }
}
```

### Backend Response (OpenAI API)
```json
{
  "timestamp": "2025-11-22T10:30:46.789Z",
  "direction": "backend_response",
  "format": "openai",
  "endpoint": "/v1/chat/completions",
  "backend_used": "openai",
  "ttft_ms": 245.3,
  "tps": 35.2,
  "itl_ms": 28.4,
  "e2e_latency_ms": 1666.5,
  "output_tokens": 58,
  "input_tokens": 12,
  "body": {
    "id": "chatcmpl-123",
    "choices": [{
      "message": {
        "role": "assistant",
        "content": "The capital of France is Paris."
      },
      "finish_reason": "stop"
    }],
    "usage": {
      "prompt_tokens": 12,
      "completion_tokens": 58,
      "total_tokens": 70
    }
  }
}
```

### Error Response
```json
{
  "timestamp": "2025-11-22T10:31:00.000Z",
  "direction": "client_response",
  "format": "gemini",
  "endpoint": "/v1beta/models/unknown-model:generateContent",
  "error": "Model not found: unknown-model",
  "body": {
    "error": {
      "code": 404,
      "message": "Model not found: unknown-model",
      "status": "NOT_FOUND"
    }
  }
}
```

## See Also

- [Standard Debugging Workflow](../debug/standard-debugging-workflow.md) - How to use logs for debugging
- [Error Map & Solutions](../debug/error-map-and-solutions.md) - Common errors in logs
- `src/bin/log-parser.rs` - Log parser implementation
