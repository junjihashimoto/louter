# Enhanced Logging for Error Debugging

**Status:** ✅ COMPLETE
**Priority:** HIGH
**Date:** 2025-11-25

## Problem Statement

The existing logging system captured basic request/response bodies but lacked:
- **HTTP metadata** (method, status codes, headers)
- **Error context** (error types, messages, stack traces)
- **Request correlation** (tracking request/response pairs)
- **Streaming event sequences** (for debugging SSE issues)
- **Replay capability** (reproducing failures from logs)

This made it difficult to debug issues like the Claude Code 404 error because we couldn't see:
- What headers Claude Code was sending
- What HTTP method was used
- What status code the proxy returned
- How to correlate request/response pairs

## Solution Implemented

### 1. Extended Log Structure (`src/logging.rs`)

Enhanced `RequestResponseLog` struct with new fields:

```rust
pub struct RequestResponseLog {
    // Existing fields
    pub timestamp: String,
    pub direction: String,
    pub format: String,
    pub endpoint: String,
    pub body: serde_json::Value,

    // NEW: HTTP metadata
    pub method: Option<String>,           // GET, POST, etc.
    pub status_code: Option<u16>,         // 200, 404, 500, etc.
    pub headers: Option<HashMap<String, String>>,  // Request/response headers

    // NEW: Error information
    pub error: Option<String>,            // Error message
    pub error_type: Option<String>,       // "parse_error", "backend_error", etc.
    pub stack_trace: Option<String>,      // Stack trace for debugging

    // NEW: Streaming information
    pub is_streaming: Option<bool>,       // Whether this is a streaming request
    pub stream_events: Option<Vec<String>>,  // List of SSE event types received

    // Existing performance metrics
    pub ttft_ms: Option<f64>,
    pub tps: Option<f64>,
    pub itl_ms: Option<f64>,
    pub e2e_latency_ms: Option<f64>,
    pub output_tokens: Option<i32>,
    pub input_tokens: Option<i32>,

    // NEW: Correlation
    pub request_id: Option<String>,       // Unique ID to correlate request/response
}
```

### 2. New Logging Methods

#### `log_client_request_detailed()`
Captures complete request context including:
- HTTP method (POST, GET, etc.)
- All request headers
- Unique request ID (UUID v4)
- Streaming flag

**Location:** `src/logging.rs:105-140`

#### `log_error()`
Dedicated error logging with:
- Error message and type
- Request body that caused the error
- Request ID for correlation
- HTTP status code

**Location:** `src/logging.rs:295-332`

#### `log_stream_events()`
Tracks SSE event sequences:
- List of event types received
- Request ID for correlation
- Event count

**Location:** `src/logging.rs:335-367`

### 3. Enhanced Request Handler (`src/main.rs`)

Updated `handle_anthropic_messages()` function:

1. **Generate request ID** (line 1143):
```rust
let request_id = uuid::Uuid::new_v4().to_string();
```

2. **Capture headers** (lines 1151-1157):
```rust
let mut headers_map = std::collections::HashMap::new();
for (key, value) in headers.iter() {
    if let Ok(value_str) = value.to_str() {
        headers_map.insert(key.to_string(), value_str.to_string());
    }
}
```

3. **Log with full context** (lines 1159-1167):
```rust
logger.log_client_request_detailed(
    "/v1/messages",
    "anthropic",
    &request_json,
    "POST",
    headers_map,
    request_id.clone(),
    request.stream.unwrap_or(false),
);
```

4. **Error logging at every failure point**:
   - Conversion errors (lines 1214-1228)
   - Config errors (lines 1237-1251)
   - Backend errors - streaming (lines 1273-1286)
   - Backend errors - non-streaming (lines 1299-1313)
   - Response conversion errors (lines 1328-1342)

**Location:** `src/main.rs:1134-1378`

### 4. Log Replay Tool (`src/bin/log-replay.rs`)

Created CLI tool for analyzing and replaying logged requests.

#### Features:

1. **Filtering**:
   - By error type: `--error-type backend_error`
   - By endpoint: `--endpoint /v1/messages`
   - By request ID: `--request-id abc-123`
   - Show only errors: `--errors-only`

2. **Output Formats**:
   - **summary** (default): Statistics and error summaries
   - **json**: Pretty-printed JSON for programmatic analysis
   - **curl**: Generate curl commands to reproduce requests

3. **Replay Mode**:
   - Execute logged requests against proxy
   - `--dry-run` to preview without executing
   - `--proxy-url` to specify target (default: http://localhost:9000)

#### Usage Examples:

```bash
# View summary of all errors
log-replay --file /tmp/proxy.jsonl --errors-only

# Generate curl commands for backend errors
log-replay --file /tmp/proxy.jsonl \
  --error-type backend_error \
  --output curl

# Replay a specific request
log-replay --file /tmp/proxy.jsonl \
  --request-id "abc-123-def-456" \
  --proxy-url http://localhost:9000

# Filter streaming errors
log-replay --file /tmp/proxy.jsonl \
  --errors-only \
  --endpoint /v1/messages
```

**Location:** `src/bin/log-replay.rs:1-307`

### 5. Cargo Configuration

Added blocking feature to reqwest and registered log-replay binary:

```toml
# Cargo.toml
reqwest = { version = "0.12", features = ["stream", "json", "blocking"] }

[[bin]]
name = "log-replay"
path = "src/bin/log-replay.rs"
```

**Location:** `Cargo.toml:21,15-16`

## Log Format Example

### Request with full metadata:
```json
{
  "timestamp": "2025-11-25T10:30:45Z",
  "direction": "client_request",
  "format": "anthropic",
  "endpoint": "/v1/messages",
  "body": {
    "model": "claude-3-haiku",
    "messages": [{"role": "user", "content": "Hello"}]
  },
  "method": "POST",
  "headers": {
    "content-type": "application/json",
    "anthropic-version": "2023-06-01",
    "x-api-key": "***"
  },
  "request_id": "550e8400-e29b-41d4-a716-446655440000",
  "is_streaming": false
}
```

### Error with full context:
```json
{
  "timestamp": "2025-11-25T10:30:46Z",
  "direction": "error",
  "format": "anthropic",
  "endpoint": "/v1/messages",
  "body": {
    "model": "claude-3-haiku",
    "messages": [{"role": "user", "content": "Hello"}]
  },
  "error": "Backend error: connection refused",
  "error_type": "backend_error",
  "request_id": "550e8400-e29b-41d4-a716-446655440000",
  "status_code": 502
}
```

## Benefits

### For Debugging Claude Code Issue:
1. **Capture exact headers** - See what Claude Code sends
2. **Track request flow** - Correlate request → response → error
3. **Identify 404 source** - See which endpoint returns 404
4. **Replay failures** - Reproduce the exact request

### General Benefits:
1. **Complete audit trail** - Every request tracked with metadata
2. **Error diagnosis** - Full context for every failure
3. **Performance analysis** - Track metrics per request
4. **Compliance** - Headers and status codes logged
5. **Testing** - Replay production issues locally

## Next Steps

1. **Run Claude Code with enhanced logging**:
   ```bash
   cargo run --bin louter -- \
     --host localhost --port 9000 \
     --config examples/llama-cpp-config.toml \
     --log-file /tmp/claude-code-debug.jsonl \
     --verbose

   cd ../tetris
   ANTHROPIC_BASE_URL=http://localhost:9000 claude -d
   ```

2. **Analyze the failure**:
   ```bash
   # View all errors
   cargo run --bin log-replay -- \
     --file /tmp/claude-code-debug.jsonl \
     --errors-only

   # Find the 404 error
   grep '"status_code":404' /tmp/claude-code-debug.jsonl | jq .

   # Check what endpoint was called
   cargo run --bin log-replay -- \
     --file /tmp/claude-code-debug.jsonl \
     --errors-only \
     --output json | jq '.[] | select(.status_code == 404) | .endpoint'
   ```

3. **Reproduce with curl**:
   ```bash
   cargo run --bin log-replay -- \
     --file /tmp/claude-code-debug.jsonl \
     --errors-only \
     --output curl > reproduce.sh

   bash reproduce.sh
   ```

## Files Modified

1. **`src/logging.rs`** - Extended RequestResponseLog struct, added new methods
2. **`src/main.rs`** - Updated handle_anthropic_messages with detailed logging
3. **`src/bin/log-replay.rs`** - New CLI tool for log analysis and replay
4. **`Cargo.toml`** - Added blocking feature, registered log-replay binary

## Testing

Compilation successful:
- ✅ `cargo build --bin louter` - No errors
- ✅ `cargo build --bin log-replay` - No errors
- ⏳ Runtime testing pending (need to run with Claude Code)

## Success Criteria

- [x] Extended log structure with HTTP metadata
- [x] Request correlation with unique IDs
- [x] Error logging with full context
- [x] Log replay tool implementation
- [x] All code compiles successfully
- [ ] Tested with Claude Code (next step)
- [ ] Successfully captured 404 error details (next step)
- [ ] Identified missing endpoint (next step)

## Related Issues

- **Blocks:** `docs/issues/claude-code-integration-issue.md` - Need to capture failing request
- **Enables:** Debugging and fixing the Claude Code 404 error
- **Prerequisite for:** Internal Expression architecture refactoring
