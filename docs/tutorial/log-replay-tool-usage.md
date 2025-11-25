# Log Replay Tool Usage Guide

**Tool:** `log-replay`
**Purpose:** Analyze and replay HTTP requests from JSON Lines logs for debugging

## Quick Start

```bash
# Build the tool
cargo build --bin log-replay

# View summary of all logged requests
cargo run --bin log-replay -- --file /tmp/proxy.jsonl

# Show only errors
cargo run --bin log-replay -- --file /tmp/proxy.jsonl --errors-only
```

## Command-Line Options

```
log-replay [OPTIONS] --file <FILE>

Options:
  -f, --file <FILE>              JSON Lines log file to replay
  -e, --error-type <TYPE>        Filter by error type (backend_error, conversion_error, etc.)
  -p, --endpoint <ENDPOINT>      Filter by endpoint (/v1/messages, /v1/chat/completions, etc.)
  -E, --errors-only              Show only errors
  -r, --request-id <ID>          Filter by specific request ID
  -u, --proxy-url <URL>          Proxy URL to replay requests to [default: http://localhost:9000]
  -d, --dry-run                  Only show requests, don't execute
  -o, --output <FORMAT>          Output format: json, curl, summary [default: summary]
  -h, --help                     Print help
```

## Output Formats

### 1. Summary (Default)

Shows statistics and grouped information:

```bash
cargo run --bin log-replay -- --file /tmp/proxy.jsonl

=== Log Summary ===
Total requests: 42
Total errors: 3
Matching entries: 45

By direction:
  client_request: 20
  backend_request: 20
  client_response: 19
  error: 3

=== Errors (3) ===

Error #1
  Timestamp: 2025-11-25T10:30:45Z
  Endpoint: /v1/messages
  Type: backend_error
  Message: connection refused
  Request ID: abc-123-def-456
  Status Code: 502

...
```

### 2. JSON Format

Pretty-printed JSON for programmatic analysis:

```bash
cargo run --bin log-replay -- \
  --file /tmp/proxy.jsonl \
  --errors-only \
  --output json > errors.json

# Then process with jq
cat errors.json | jq '.[] | select(.status_code == 404)'
```

### 3. cURL Format

Generate curl commands to reproduce requests:

```bash
cargo run --bin log-replay -- \
  --file /tmp/proxy.jsonl \
  --output curl

# 2025-11-25T10:30:45Z - /v1/messages
curl -X POST 'http://localhost:9000/v1/messages' \
  -H 'Content-Type: application/json' \
  -H 'anthropic-version: 2023-06-01' \
  -d '{"model":"claude-3-haiku","messages":[...]}'

# Save to file and execute
cargo run --bin log-replay -- \
  --file /tmp/proxy.jsonl \
  --output curl > reproduce.sh
bash reproduce.sh
```

## Common Use Cases

### 1. Find All Errors

```bash
cargo run --bin log-replay -- \
  --file /tmp/proxy.jsonl \
  --errors-only
```

### 2. Filter by Error Type

```bash
# Backend errors only
cargo run --bin log-replay -- \
  --file /tmp/proxy.jsonl \
  --error-type backend_error

# Conversion errors only
cargo run --bin log-replay -- \
  --file /tmp/proxy.jsonl \
  --error-type conversion_error
```

### 3. Filter by Endpoint

```bash
# Anthropic API errors
cargo run --bin log-replay -- \
  --file /tmp/proxy.jsonl \
  --endpoint /v1/messages \
  --errors-only

# OpenAI API errors
cargo run --bin log-replay -- \
  --file /tmp/proxy.jsonl \
  --endpoint /v1/chat/completions \
  --errors-only
```

### 4. Trace Specific Request

```bash
# First, find the request ID from summary
cargo run --bin log-replay -- \
  --file /tmp/proxy.jsonl \
  --errors-only

# Then filter by request ID to see full flow
cargo run --bin log-replay -- \
  --file /tmp/proxy.jsonl \
  --request-id "550e8400-e29b-41d4-a716-446655440000" \
  --output json
```

### 5. Debug 404 Errors

```bash
# Find all 404 errors
grep '"status_code":404' /tmp/proxy.jsonl | jq .

# Or use log-replay with jq
cargo run --bin log-replay -- \
  --file /tmp/proxy.jsonl \
  --errors-only \
  --output json | jq '.[] | select(.status_code == 404)'

# Get curl command to reproduce
cargo run --bin log-replay -- \
  --file /tmp/proxy.jsonl \
  --errors-only \
  --output curl | grep -A5 "404"
```

### 6. Replay Failed Request

```bash
# Replay a specific failed request
cargo run --bin log-replay -- \
  --file /tmp/proxy.jsonl \
  --request-id "abc-123-def-456" \
  --proxy-url http://localhost:9000

# Dry run first to preview
cargo run --bin log-replay -- \
  --file /tmp/proxy.jsonl \
  --request-id "abc-123-def-456" \
  --dry-run
```

### 7. Analyze Streaming Issues

```bash
# Find streaming errors
cargo run --bin log-replay -- \
  --file /tmp/proxy.jsonl \
  --errors-only \
  --output json | jq '.[] | select(.is_streaming == true)'

# View stream events
cargo run --bin log-replay -- \
  --file /tmp/proxy.jsonl \
  --output json | jq '.[] | select(.stream_events != null)'
```

## Debugging Workflow

### Step 1: Reproduce the Issue with Logging

```bash
# Start proxy with logging
cargo run --bin louter -- \
  --host localhost --port 9000 \
  --config config.toml \
  --log-file /tmp/debug.jsonl \
  --verbose

# Make the failing request
# (e.g., run Claude Code, or use curl)
```

### Step 2: Analyze the Logs

```bash
# Get overview
cargo run --bin log-replay -- --file /tmp/debug.jsonl

# Find errors
cargo run --bin log-replay -- \
  --file /tmp/debug.jsonl \
  --errors-only

# Get detailed JSON
cargo run --bin log-replay -- \
  --file /tmp/debug.jsonl \
  --errors-only \
  --output json > errors.json
```

### Step 3: Identify Root Cause

```bash
# Check error types
cat errors.json | jq '.[] | .error_type' | sort | uniq -c

# Check affected endpoints
cat errors.json | jq '.[] | .endpoint' | sort | uniq -c

# Check status codes
cat errors.json | jq '.[] | .status_code' | sort | uniq -c

# View full error details
cat errors.json | jq '.[] | {
  timestamp,
  endpoint,
  error_type,
  error,
  status_code,
  request_id
}'
```

### Step 4: Reproduce with cURL

```bash
# Generate curl commands
cargo run --bin log-replay -- \
  --file /tmp/debug.jsonl \
  --errors-only \
  --output curl > reproduce.sh

# Execute to verify
bash reproduce.sh

# Or run individual curl command
cargo run --bin log-replay -- \
  --file /tmp/debug.jsonl \
  --request-id "abc-123" \
  --output curl | bash
```

### Step 5: Replay with Fixes

```bash
# After fixing the code, replay the same request
cargo run --bin log-replay -- \
  --file /tmp/debug.jsonl \
  --request-id "abc-123" \
  --proxy-url http://localhost:9000
```

## Log Format Reference

Each log entry has this structure:

```json
{
  "timestamp": "2025-11-25T10:30:45Z",
  "direction": "client_request | backend_request | client_response | backend_response | error",
  "format": "anthropic | openai | gemini",
  "endpoint": "/v1/messages",
  "body": { ... },

  // Optional HTTP metadata
  "method": "POST",
  "status_code": 200,
  "headers": { ... },

  // Optional error information
  "error": "Error message",
  "error_type": "backend_error | conversion_error | ...",
  "stack_trace": "...",

  // Optional streaming information
  "is_streaming": true,
  "stream_events": ["message_start", "content_block_delta", ...],

  // Optional performance metrics
  "ttft_ms": 123.45,
  "tps": 45.67,
  "itl_ms": 12.34,
  "e2e_latency_ms": 234.56,
  "output_tokens": 100,
  "input_tokens": 50,

  // Optional correlation
  "request_id": "550e8400-e29b-41d4-a716-446655440000"
}
```

## Error Types

Common error types you'll see:

- **`backend_error`** - Failed to connect to backend or backend returned error
- **`conversion_error`** - Failed to convert between protocols (Anthropic ↔ OpenAI)
- **`config_error`** - Configuration issue (missing API key, invalid model mapping)
- **`invalid_request`** - Malformed request from client
- **`serialization_error`** - Failed to serialize/deserialize JSON

## Tips

1. **Always start with summary** - Get overview before diving into details
2. **Use jq for filtering** - JSON output format works great with jq
3. **Save curl scripts** - Useful for regression testing
4. **Correlate by request_id** - Track full request→response→error flow
5. **Check timestamps** - Identify timing-related issues
6. **Dry-run replays** - Preview before executing

## Examples

### Debug Claude Code 404 Error

```bash
# 1. Run Claude Code with logging
cargo run --bin louter -- \
  --config examples/llama-cpp-config.toml \
  --log-file /tmp/claude-debug.jsonl \
  --verbose &

cd ../tetris
ANTHROPIC_BASE_URL=http://localhost:9000 claude -d

# 2. Find the 404 error
cargo run --bin log-replay -- \
  --file /tmp/claude-debug.jsonl \
  --errors-only \
  --output json | jq '.[] | select(.status_code == 404)'

# 3. Check what endpoint was called
cargo run --bin log-replay -- \
  --file /tmp/claude-debug.jsonl \
  --errors-only \
  --output json | jq '.[] | select(.status_code == 404) | .endpoint'

# 4. Generate curl to reproduce
cargo run --bin log-replay -- \
  --file /tmp/claude-debug.jsonl \
  --errors-only \
  --output curl | grep -A10 "404"
```

### Analyze Performance Issues

```bash
# Find slow requests (>1000ms e2e latency)
cargo run --bin log-replay -- \
  --file /tmp/proxy.jsonl \
  --output json | jq '.[] | select(.e2e_latency_ms > 1000)'

# Calculate average TTFT
cargo run --bin log-replay -- \
  --file /tmp/proxy.jsonl \
  --output json | jq '[.[] | select(.ttft_ms != null) | .ttft_ms] | add/length'

# Find requests with low TPS
cargo run --bin log-replay -- \
  --file /tmp/proxy.jsonl \
  --output json | jq '.[] | select(.tps != null and .tps < 10)'
```

## See Also

- **`log-parser`** - Statistical analysis tool for logs
- **`docs/design/log-format.md`** - Detailed log format specification
- **`docs/resolved/2025-11-25-enhanced-logging-for-debugging-COMPLETE.md`** - Implementation details
