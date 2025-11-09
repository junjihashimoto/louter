# Louter - LLM Router

A high-performance, intelligent routing system for Large Language Models. Louter bridges multiple LLM APIs (Gemini, OpenAI, and OpenAI-compatible backends) with automatic protocol conversion, content-based routing, and capability detection. Built in Rust with Axum for maximum performance and reliability.

## Features

### Core Capabilities
- **Dual Frontend Support**: Accept both Gemini API and OpenAI API requests
- **Dual Backend Support**: Route to OpenAI or Gemini backends with automatic format conversion
- **Content-Based Routing**: Automatic backend selection based on request capabilities (text, vision, audio, function calling)
- **Auto Mode**: Backends accept all request types by default (empty capabilities = universal support)
- **Priority-Based Fallback**: Configurable backend priority with automatic failover
- **Full API Coverage**: `generateContent`, `streamGenerateContent`, `countTokens`, `/v1/chat/completions`
- **Streaming Support**: Server-Sent Events (SSE) for real-time responses
- **Function Calling**: Full support for tool/function calling with automatic conversion
- **Multimedia Support**: Images, audio, video, PDF processing

### Development & Debugging
- **Web UI Dashboard**: Real-time log viewer with performance metrics visualization
- **Backend Diagnostics**: Automatic capability detection and health checks at startup
- **Routing Metrics**: Track backend selection decisions with Prometheus metrics
- **JSON Lines Logging**: Complete request/response logging with TTFT, TPS, and ITL metrics
- **Prometheus Metrics**: Industry-standard metrics for monitoring and alerting
- **Performance Metrics**: NVIDIA NIM-compliant TTFT, TPS, ITL, and e2e latency tracking
- **Log Parser**: Analyze logs with filtering, function call extraction, and statistics
- **Diagnostic Tool**: Automated testing suite with detailed pass/fail reports
- **Test Suite**: Routing verification script (`test-routing.sh`)
- **Flexible Configuration**: TOML-based with environment variable fallback

## Quick Start

### Installation

```bash
git clone <repository-url>
cd louter
cargo build --release
```

### Configuration

Copy an example configuration file from `examples/` directory:

```bash
cp examples/config.toml config.toml
# Or use the public API example:
# cp examples/config-public-api.toml config.toml
```

Edit `config.toml` to set your API keys and preferences:

```toml
# API keys (higher priority than environment variables)
[backends.openai]
url = "https://api.openai.com"
# api_key = "your-openai-api-key-here"  # Uncomment to override OPENAI_API_KEY

[backends.gemini]
url = "https://generativelanguage.googleapis.com"
# api_key = "your-gemini-api-key-here"  # Uncomment to override GEMINI_API_KEY

# Token counting mode
[performance]
openai_token_counting_mode = "estimate"  # or "chat" for accurate API-based counting
```

### Environment Variables

Set your API keys (if not using config file):

```bash
export OPENAI_API_KEY="your-openai-api-key"
export GEMINI_API_KEY="your-gemini-api-key"
```

### Public API Configuration

To use Google Gemini API and OpenAI API public endpoints directly:

**1. Copy the public API example config:**

```bash
cp examples/config-public-api.toml config.toml
```

**Example configuration:**

```toml
[backends.gemini]
url = "https://generativelanguage.googleapis.com"
protocol = "gemini"
capabilities = ["text", "vision", "audio", "video", "function_calling"]
priority = 1

[backends.gemini.model_mapping]
"gemini-pro" = "gemini-2.5-flash"
"gemini-flash" = "gemini-2.5-flash"
"gpt-4" = "gemini-2.5-flash"
"gpt-3.5-turbo" = "gemini-2.5-flash"

[backends.openai]
url = "https://api.openai.com"
protocol = "openai"
capabilities = ["text", "vision", "function_calling"]
priority = 2

[backends.openai.model_mapping]
"gemini-2.5-flash" = "gpt-4"
"gemini-pro" = "gpt-3.5-turbo"

[performance]
enable_metrics = true
log_requests = true
timeout_seconds = 30
```

**2. Set environment variables:**

```bash
export GEMINI_API_KEY="your-google-gemini-api-key"
export OPENAI_API_KEY="your-openai-api-key"
```

**3. Start the proxy:**

```bash
cargo run -- --host localhost --port 8080 --config config-public-api.toml --verbose
```

**4. Test with curl:**

```bash
# Test Gemini API endpoint (query parameter auth)
curl -s 'http://localhost:8080/v1beta/models/gemini-2.5-flash:generateContent?key='$GEMINI_API_KEY \
  -H 'Content-Type: application/json' \
  -X POST \
  -d '{"contents":[{"parts":[{"text":"Say hello in one word"}]}]}'

# Test Gemini API endpoint (header auth)
curl -s 'http://localhost:8080/v1beta/models/gemini-2.5-flash:generateContent' \
  -H "x-goog-api-key: $GEMINI_API_KEY" \
  -H 'Content-Type: application/json' \
  -X POST \
  -d '{"contents":[{"parts":[{"text":"Say hello"}],"role":"user"}]}'

# Test OpenAI API endpoint
curl -s 'http://localhost:8080/v1/chat/completions' \
  -H "Authorization: Bearer $OPENAI_API_KEY" \
  -H 'Content-Type: application/json' \
  -d '{
    "model": "gpt-3.5-turbo",
    "messages": [{"role": "user", "content": "Say hello in one word"}],
    "max_tokens": 10
  }'
```

**Working direct API calls (for verification):**

```bash
# Direct Gemini API (query parameter)
curl -s 'https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent?key='$GEMINI_API_KEY \
  -H 'Content-Type: application/json' \
  -X POST \
  -d '{"contents":[{"parts":[{"text":"Say hello in one word"}]}]}'

# Direct Gemini API (header)
curl -s 'https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent' \
  -H "x-goog-api-key: $GEMINI_API_KEY" \
  -H 'Content-Type: application/json' \
  -X POST \
  -d '{"contents":[{"parts":[{"text":"Say hello"}],"role":"user"}]}'

# Direct OpenAI API
curl -s 'https://api.openai.com/v1/chat/completions' \
  -H "Authorization: Bearer $OPENAI_API_KEY" \
  -H 'Content-Type: application/json' \
  -d '{
    "model": "gpt-3.5-turbo",
    "messages": [{"role": "user", "content": "Say hello in one word"}],
    "max_tokens": 10
  }'
```

### Running the Proxy

```bash
cargo run -- --host localhost --port 8080 --config config.toml --verbose
```

### Health Check

```bash
curl http://localhost:8080/health
```

### Web UI Dashboard

Access the real-time debug dashboard at `http://localhost:8080/ui`

**Features:**
- **Logs Tab**: View request/response logs with filtering and search
  - Filter by direction (client_request, backend_request, backend_response, client_response)
  - Search across all log fields
  - Pagination controls (50/100/200/500 entries)
  - Color-coded entries by type
  - JSON payload display with syntax highlighting
  - Auto-refresh every 5 seconds

- **Metrics Tab**: View Prometheus metrics and performance statistics
  - Summary cards: Total Requests, Tokens, Function Calls, Errors
  - Complete metrics list with labels and values
  - Search/filter capability

**Performance Metrics** (NVIDIA NIM-compliant):
- **TTFT** (Time to First Token): Latency from request to first token
- **TPS** (Tokens Per Second): Token generation throughput
- **ITL** (Inter-token Latency): Average time between consecutive tokens
- **e2e_latency**: End-to-end request duration
- **Token counts**: Input and output tokens tracked

**Endpoints:**
- `/ui` - Web dashboard (HTML)
- `/api/logs` - JSON logs API with filtering
- `/api/metrics` - JSON metrics API
- `/metrics` - Prometheus metrics endpoint

```bash
# View in browser
open http://localhost:8080/ui

# Query APIs directly
curl "http://localhost:8080/api/logs?limit=10&direction=client_response"
curl "http://localhost:8080/api/metrics"
curl "http://localhost:8080/metrics"  # Prometheus format
```

## Content-Based Routing

The proxy automatically routes requests to appropriate backends based on detected capabilities, enabling cost optimization and performance tuning.

### How It Works

1. **Request Analysis**: Proxy detects required capabilities (text, vision, audio, function calling)
2. **Backend Selection**: Finds compatible backends, sorted by priority
3. **Automatic Routing**: Routes to the best backend for the request type
4. **Fallback Support**: Falls back to next priority backend if primary fails

### Configuration

**Auto Mode** (Default - accepts all request types):
```toml
[backends.general]
url = "http://localhost:11211"
capabilities = []  # Empty = AUTO MODE (accepts everything)
priority = 1
```

**Explicit Mode** (Capability-specific routing):
```toml
# Fast text-only backend (cheaper/faster)
[backends.text-fast]
url = "http://localhost:11211"
capabilities = ["text", "function_calling"]
priority = 1  # Higher priority (lower number)

# Multimodal backend (more expensive)
[backends.vision-capable]
url = "http://localhost:11212"
capabilities = ["text", "vision", "audio", "function_calling"]
priority = 2  # Lower priority (higher number)
```

### Supported Capabilities

- `text` - Text generation
- `vision` - Image processing
- `audio` - Audio processing
- `video` - Video processing
- `function_calling` - Tool/function calling

### Backend Diagnostics

On startup, the proxy automatically probes each backend:

```
╔════════════════════════════════════════════════════════════╗
║          Backend Capability Diagnostics                   ║
╚════════════════════════════════════════════════════════════╝

Backend: text-fast (http://localhost:11211)
  Status: ✓ REACHABLE
  Mode: EXPLICIT
  Detected Capabilities:
    ✓ text
    ✓ function_calling
    ○ vision (not supported)
```

### Routing Logs

Every routing decision is logged:

```
📋 Request requires capabilities: [text]
✓ Routing selected: 'text-fast' (mode: EXPLICIT, url: http://localhost:11211, priority: 1)
```

For vision requests:
```
📋 Request requires capabilities: [text, vision]
✓ Routing selected: 'vision-capable' (mode: EXPLICIT, url: http://localhost:11212, priority: 2)
```

### Routing Metrics

Track routing decisions with Prometheus:

```bash
# View routing metrics
curl http://localhost:8080/metrics | grep routing_decisions_total
```

**Example metrics:**
```
routing_decisions_total{backend="text-fast",mode="explicit",capabilities="text",success="true"} 42
routing_decisions_total{backend="vision-capable",mode="explicit",capabilities="text,vision",success="true"} 8
```

**Prometheus queries:**
```promql
# Routing decisions by backend
sum by (backend) (routing_decisions_total{success="true"})

# Routing decisions by capability
sum by (capabilities) (routing_decisions_total{success="true"})
```

### Testing Routing

Run the included test script:

```bash
./test-routing.sh
```

This tests:
- Text-only requests → routes to text-capable backend
- Vision requests → routes to vision-capable backend
- Function calling → routes to function-capable backend
- Metrics collection

### Use Cases

**Cost Optimization:**
```toml
# Cheap text model (e.g., llama-3.2-3b)
[backends.cheap-text]
capabilities = ["text"]
priority = 1

# Expensive vision model (e.g., llava)
[backends.expensive-vision]
capabilities = ["text", "vision"]
priority = 2
```
→ Save 80% on text-only requests

**Performance Optimization:**
```toml
# Fast 7B model for simple queries
[backends.fast]
capabilities = ["text"]
priority = 1

# Powerful 70B model for complex queries
[backends.powerful]
capabilities = ["text", "function_calling"]
priority = 2
```
→ Fast responses for simple queries, accuracy for complex ones

**Development Setup:**
```toml
[backends.dev]
url = "http://localhost:11211"
capabilities = []  # AUTO MODE: accepts everything
priority = 1
```
→ Simple setup, no capability management needed

## For Developers

### Development Workflow

**Core Flow:** Implement → Build → Test → Debug → Document

When implementing any feature, follow this simple checklist:

1. **Implement** - Write code following [CRITICAL CODING RULES](CLAUDE.md#critical-coding-rules)
2. **Build** - `cargo build --bin louter`
3. **Test** - Run diagnostics: `curl http://localhost:9000/api/diagnostics > /tmp/diag.json`
4. **Debug** - If tests fail, see [docs/debug/standard-debugging-workflow.md](docs/debug/standard-debugging-workflow.md)
5. **Document** - Add to appropriate `docs/` directory

### Quick Debug Guide

**When tests fail:**

```bash
# 1. Save full diagnostics output
curl -s http://localhost:9000/api/diagnostics > /tmp/diagnostics.json

# 2. Check which tests failed
cat /tmp/diagnostics.json | jq '.frontends[].test_results[] |
  select(.passed == false) | {test: .test_name, error: .error}'

# 3. Check proxy logs
tail -50 /tmp/proxy.jsonl | jq .

# 4. Look up error in error map
# See: docs/debug/error-map-and-solutions.md
```

**Common errors and solutions:**

| Error | Quick Fix | Documentation |
|-------|-----------|---------------|
| "Unsupported parameter: max_tokens" | Configure `max_tokens_field` per backend | [Error 2.1](docs/debug/error-map-and-solutions.md#error-21) |
| "Missing field: candidatesTokenCount" | Make field optional in struct | [Error 2.3](docs/debug/error-map-and-solutions.md#error-23) |
| "Backend not configured" | Add backend to config.toml | [Error 1.1](docs/debug/error-map-and-solutions.md#error-11) |
| MAX_TOKENS errors | Increase token limits | [Error 2.4](docs/debug/error-map-and-solutions.md#error-24) |

### Documentation Structure

```
docs/
├── README.md                          # Documentation hub
├── tutorial/                          # For new users (gemini-cli + codex)
├── design/                            # Architecture & design decisions
├── issues/                            # Current known issues
├── resolved/                          # Fixed issues with solutions
├── debug/                             # ⭐ Debugging guides
│   ├── standard-debugging-workflow.md # Systematic debugging process
│   ├── error-map-and-solutions.md     # Error catalog with fixes
│   └── api-type-error-max-tokens.md   # Real debugging example
└── plans/                             # Future roadmap
```

**Key principle:** Start simple (CLAUDE.md), dig deeper when needed (docs/debug/)

### Primary Use Case for New Users

**Goal:** Use a local LLM with both `gemini-cli` and `codex` (OpenAI coding agent)

**Setup:**
1. Run local LLM (llama.cpp, Ollama, vLLM)
2. Configure louter for dual API access
3. Point `gemini-cli` to louter's Gemini API endpoint
4. Point `codex` to louter's OpenAI API endpoint
5. Both tools access the same local model!

**Tutorial:** See [docs/tutorial/README.md](docs/tutorial/README.md)

### When Things Go Wrong

**"I'm stuck debugging"**
→ Follow [standard-debugging-workflow.md](docs/debug/standard-debugging-workflow.md) step-by-step

**"I don't know what's causing this error"**
→ Look it up in [error-map-and-solutions.md](docs/debug/error-map-and-solutions.md)

**"I want to see a real debugging example"**
→ Read [api-type-error-max-tokens.md](docs/debug/api-type-error-max-tokens.md)

### Development Files

- **CLAUDE.md** - Quick developer reference with critical coding rules
- **docs/** - Comprehensive documentation by category
- **config-public-api.toml** - Example config for public APIs
- **config-mock.toml** - Example config for local testing

## API Usage

### Gemini API Format

#### Generate Content

```bash
curl -X POST 'http://localhost:8080/v1beta/models/gemini-2.0-flash:generateContent?key=YOUR_API_KEY' \
  -H "Content-Type: application/json" \
  -d '{
    "contents": [{
      "parts": [{"text": "Hello, world! How are you today?"}],
      "role": "user"
    }]
  }'
```

#### Stream Generate Content

```bash
curl -X POST 'http://localhost:8080/v1beta/models/gemini-2.0-flash:streamGenerateContent?key=YOUR_API_KEY&alt=sse' \
  -H "Content-Type: application/json" \
  -d '{
    "contents": [{
      "parts": [{"text": "Write a short story about a robot."}],
      "role": "user"
    }]
  }'
```

#### Count Tokens

```bash
curl -X POST 'http://localhost:8080/v1beta/models/gemini-2.0-flash:countTokens?key=YOUR_API_KEY' \
  -H "Content-Type: application/json" \
  -d '{
    "contents": [{
      "parts": [{"text": "How many tokens is this?"}],
      "role": "user"
    }]
  }'
```

#### Multimedia Support

```bash
curl -X POST 'http://localhost:8080/v1beta/models/gemini-2.0-flash:generateContent?key=YOUR_API_KEY' \
  -H "Content-Type: application/json" \
  -d '{
    "contents": [{
      "parts": [
        {"text": "Describe this image:"},
        {
          "inlineData": {
            "mimeType": "image/jpeg",
            "data": "base64-encoded-image-data"
          }
        }
      ],
      "role": "user"
    }]
  }'
```

### OpenAI API Format

#### Chat Completions

```bash
curl -X POST 'http://localhost:8080/v1/chat/completions' \
  -H "Content-Type: application/json" \
  -d '{
    "model": "gemma3-4b",
    "messages": [
      {"role": "user", "content": "Hello, how are you?"}
    ],
    "max_tokens": 100,
    "temperature": 0.7
  }'
```

#### Function Calling

```bash
curl -X POST 'http://localhost:8080/v1/chat/completions' \
  -H "Content-Type: application/json" \
  -d '{
    "model": "gemma3-4b",
    "messages": [
      {"role": "user", "content": "What'\''s the weather in Tokyo?"}
    ],
    "tools": [{
      "type": "function",
      "function": {
        "name": "getWeather",
        "description": "Get weather for a city",
        "parameters": {
          "type": "object",
          "properties": {
            "location": {"type": "string", "description": "City name"}
          },
          "required": ["location"]
        }
      }
    }],
    "tool_choice": "auto"
  }'
```

#### Streaming

```bash
curl -X POST 'http://localhost:8080/v1/chat/completions' \
  -H "Content-Type: application/json" \
  -d '{
    "model": "gemma3-4b",
    "messages": [
      {"role": "user", "content": "Tell me a story"}
    ],
    "stream": true
  }'
```

#### Multimedia (Vision)

```bash
curl -X POST 'http://localhost:8080/v1/chat/completions' \
  -H "Content-Type: application/json" \
  -d '{
    "model": "gemma3-4b",
    "messages": [{
      "role": "user",
      "content": [
        {"type": "text", "text": "What'\''s in this image?"},
        {
          "type": "image_url",
          "image_url": {
            "url": "data:image/jpeg;base64,/9j/4AAQSkZJRg..."
          }
        }
      ]
    }]
  }'
```

## Configuration

### Backend Configuration

The proxy supports multiple backends with flexible configuration:

```toml
[backends.openai]
url = "https://api.openai.com"
max_tokens = 4096
temperature = 0.7
weight = 1.0
api_key = "optional-api-key"
tool_format = "json"  # "json" or "xml" - tool calling format

[backends.openai.model_mapping]
"gemini-2.0-flash" = "gpt-4"
"gemini-pro" = "gpt-3.5-turbo"
"gemini-1.5-pro" = "gpt-4"
"gemini-1.5-flash" = "gpt-4o-mini"
```

### XML Tool Format Support (Qwen3-Coder)

The proxy supports models that output tool calls in XML format (like Qwen3-Coder) while maintaining compatibility with standard JSON format models.

```toml
# Backend for gpt-oss (JSON format)
[backends.gpt-oss]
url = "http://localhost:11211"
tool_format = "json"  # Standard JSON tool format

[backends.gpt-oss.model_mapping]
"gpt-oss-20b" = "gpt-oss:20b"

# Backend for Qwen3-Coder (XML format)
[backends.qwen]
url = "http://localhost:11212"
tool_format = "xml"  # Qwen3-Coder uses XML format

[backends.qwen.model_mapping]
"qwen3-coder" = "Qwen3-Coder-30B-A3B-Instruct"
```

**How it works:**
- **Input**: Tools are sent in JSON format via OpenAI API (all backends)
- **Output**:
  - JSON format models: Standard `tool_calls` response
  - XML format models: Proxy automatically parses XML `<tool_call>` tags and converts to JSON

**Example XML Tool Call (Qwen3-Coder):**
```xml
<tool_call>
  <function=WriteFile>
    <parameter=file_path>
    test.txt
    </parameter>
    <parameter=content>
    Hello, World!
    </parameter>
  </function>
</tool_call>
```

This is automatically converted to standard JSON format for compatibility with both Gemini and OpenAI API clients.

### Per-Backend Custom Instructions

Configure custom instructions for each backend independently with flexible injection modes:

```toml
[backends.openai]
url = "http://localhost:11211"
custom_instruction = "You are a helpful coding assistant. Always provide clear, concise answers."
custom_instruction_mode = "append"  # Options: "override", "prepend", "append" (default)

[backends.gemini]
url = "http://localhost:1111"
custom_instruction = "Focus on providing detailed technical explanations."
custom_instruction_mode = "prepend"
```

**Modes:**
- **`append`** (default): Adds custom instruction after the client's system instruction
  - Format: `{client_instruction}\n\n{custom_instruction}`
- **`prepend`**: Adds custom instruction before the client's system instruction
  - Format: `{custom_instruction}\n\n{client_instruction}`
- **`override`**: Replaces the client's system instruction entirely
  - Format: `{custom_instruction}` (client instruction ignored)

**Priority Order:**
1. `override_system_instruction` (from `[performance]` section) - highest priority
2. Per-backend `custom_instruction` with configured mode
3. Global `custom_instructions` (deprecated)
4. Original client system instruction

**Use Cases:**
- Add backend-specific guidelines (e.g., "always use JSON format")
- Enforce safety/compliance instructions per backend
- Customize behavior for different model capabilities
- Override client instructions for controlled environments

### Performance Settings

```toml
[performance]
enable_metrics = true
log_requests = true
timeout_seconds = 30
openai_token_counting_mode = "estimate"  # "estimate" or "chat"
```

### Token Counting Modes

- **`estimate`**: Fast, free token estimation (default)
- **`chat`**: Accurate counting using real OpenAI API calls

## Architecture

The proxy supports 4 operating modes with automatic format conversion:

### Mode 1: Gemini Frontend → OpenAI Backend (Conversion)
```
Client → Gemini Request → Proxy → OpenAI API → Proxy → Gemini Response → Client
         (camelCase)              (snake_case)        (camelCase)
```

### Mode 2: Gemini Frontend → Gemini Backend (Pass-through)
```
Client → Gemini Request → Proxy → Gemini API → Gemini Response → Client
         (camelCase)              (camelCase)        (camelCase)
```

### Mode 3: OpenAI Frontend → OpenAI Backend (Pass-through)
```
Client → OpenAI Request → Proxy → OpenAI API → OpenAI Response → Client
         (snake_case)             (snake_case)       (snake_case)
```

### Mode 4: OpenAI Frontend → Gemini Backend (Conversion)
```
Client → OpenAI Request → Proxy → Gemini API → Proxy → OpenAI Response → Client
         (snake_case)             (camelCase)        (snake_case)
```

All conversions handle:
- Field name formatting (camelCase ↔ snake_case)
- Function calling format (functionCall ↔ tool_calls)
- Message structure differences
- Token usage metadata

## Development Tools

### Diagnostic Tool

Automatically test all proxy features and show a detailed summary:

```bash
# Start the proxy
cargo run -- --host localhost --port 9000 --config config.toml --log-file /tmp/proxy.jsonl --verbose

# Run diagnostics API (in another terminal)
curl -s http://localhost:9000/api/diagnostics > diagnostics.json

# View results
cat diagnostics.json | jq .

# Check for failures
cat diagnostics.json | jq '.frontends[].test_results[] | select(.passed == false)'
```

**Diagnostics output includes:**
- ✅/❌ Pass/fail for each feature (text, vision, function calling, streaming)
- Backend capability detection and reachability
- Cross-protocol conversion tests (Gemini API → OpenAI backend, etc.)
- Error messages with details for failures

### Log Parser

Analyze JSON Lines logs with structured queries (no need to know the log schema):

```bash
# Show all log entries
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

**Features:**
- Filter by direction (client_request, backend_request, backend_response, client_response)
- Filter by format (gemini, openai)
- Extract function calls and tool definitions
- Show request/response pairs
- Generate statistics
- Pretty-print JSON output

**Log Schema:** See [docs/design/log-format.md](docs/design/log-format.md) for complete schema documentation and manual jq queries

### Running Tests

```bash
# Unit tests
cargo test

# Integration tests (requires API keys)
cargo test --features integration

# Test with real APIs
./test-phase1.sh  # Unit tests
./test-phase2.sh  # OpenAI backend integration
./test-phase3.sh  # Gemini backend integration
```

### Project Structure

```
src/
├── main.rs           # Main server and request handlers (Gemini & OpenAI frontends)
├── config.rs         # Configuration management
├── conversion.rs     # Bidirectional Gemini ↔ OpenAI conversion logic
├── backends.rs       # Backend client implementation
├── logging.rs        # JSON Lines logger for request/response tracking
├── error.rs          # Error handling
├── lib.rs            # Library exports
├── models/
│   ├── gemini.rs     # Gemini API models (camelCase)
│   └── openai.rs     # OpenAI API models (snake_case)
└── bin/
    ├── test-runner.rs   # CLI tool for testing APIs
    ├── log-parser.rs    # CLI tool for analyzing logs
    └── diagnostic.rs    # Automated testing suite
```

## Performance

- **Streaming**: Full SSE support with real-time token generation
- **Concurrency**: Async/await with Tokio runtime
- **Memory**: Zero-copy where possible, efficient buffering
- **Monitoring**: Request timing, token metrics, error tracking

## API Compatibility

### Supported Endpoints

**Gemini API Format:**
- ✅ `/v1beta/models/{model}:generateContent`
- ✅ `/v1beta/models/{model}:streamGenerateContent`
- ✅ `/v1beta/models/{model}:countTokens`

**OpenAI API Format:**
- ✅ `/v1/chat/completions` (streaming and non-streaming)

### Supported Models

| Gemini Model | OpenAI Mapping |
|--------------|-----------------|
| gemini-2.0-flash | gpt-4 |
| gemini-pro | gpt-3.5-turbo |
| gemini-1.5-pro | gpt-4 |
| gemini-1.5-flash | gpt-4o-mini |

### Supported Features

- ✅ Text generation
- ✅ Function/tool calling
- ✅ Multimedia (images, audio, video, PDF)
- ✅ Streaming responses
- ✅ System instructions
- ✅ Safety settings
- ✅ Generation configuration

## CLI Options

### Main Proxy Server

```
Usage: louter [OPTIONS]

Options:
      --backend <BACKEND>    Backend type: "openai" or "gemini" [default: openai]
      --port <PORT>          Port to listen on [default: 8080]
      --config <CONFIG>      Configuration file path [default: config.toml]
      --log-file <FILE>      Enable JSON Lines logging to file
      --verbose              Enable verbose debug logging
  -h, --help                 Print help
```

### Diagnostic Tool

```
Usage: diagnostic [OPTIONS]

Options:
      --backend <BACKEND>        Backend type: "openai" or "gemini"
      --port <PORT>              Port to run proxy on [default: 9000]
      --config <CONFIG>          Configuration file path [default: llama-cpp-config.toml]
      --skip-gemini-frontend     Skip Gemini API tests (only test OpenAI frontend)
      --skip-openai-frontend     Skip OpenAI API tests (only test Gemini frontend)
  -h, --help                     Print help
```

### Log Parser

```
Usage: log-parser --file <FILE> [COMMAND]

Commands:
  all       Show all log entries
  filter    Filter entries by direction and/or format
  functions Show only function calling related entries
  stats     Show statistics
  pairs     Show request/response pairs
  help      Print this message or the help of the given subcommand(s)

Options:
      --file <FILE>  Path to JSON Lines log file
  -h, --help         Print help
```

## Token Handling

### API Key Management

The proxy uses API keys from **environment variables or config file** for all backend requests. This design supports **large-scale deployments** with centralized API key management.

```rust
// Implementation: src/main.rs ~948, ~870
let gemini_api_key = state.config.get_api_key(&selected_backend, "GEMINI_API_KEY")
    .ok_or_else(|| ProxyError::ConfigError(...))?;
```

**Token Source Priority:**
1. Config file `api_key` field (highest priority)
2. Environment variables (`GEMINI_API_KEY`, `OPENAI_API_KEY`)

**Client Authorization headers are ignored** - the proxy manages backend authentication centrally.

**Use Cases:**
- **Large-scale production deployments** - Handle high request volumes with centralized API key management
- **Internal API gateway** - Single point of control for backend API access
- **Cost control** - Centralized billing and usage tracking under one API key
- **Security** - Backend API keys never exposed to clients

**Not Designed For:**
- Multi-tenant deployments (different users with different API keys)
- Client-provided API key passthrough

### Backend Authentication Methods

The proxy authenticates to backends using:

**Gemini API:**
- Query parameter: `?key=<api-key>`
- Header: `x-goog-api-key: <api-key>`

**OpenAI API:**
- Header: `Authorization: Bearer <api-key>`

## Future Features

This project has an extensive roadmap of planned enhancements. Here are the key features organized by priority:

**Recently Completed**:
- ✅ **Prometheus Metrics** - TTFT, TPS, ITL histograms with NVIDIA NIM compliance
- ✅ **Web UI Dashboard** - Real-time log viewer with performance metrics visualization
- ✅ **Performance Monitoring** - Complete TTFT, TPS, ITL, e2e latency tracking in logs and UI
- ✅ **Public API Support** - Gemini API and OpenAI API public endpoint integration
- ✅ **Gemini Response Fields** - All fields for gemini-2.5-flash compatibility
- ✅ **Centralized API Key Management** - Large-scale deployment support with config/env keys

**High Priority** (Production Essentials):
- 📋 **Response Caching** - Reduce costs by 40-80% with Redis/in-memory cache
- 📋 **Rate Limiting** - Per-IP and global limits for abuse prevention and cost control
- 🔄 **Load Balancing & Failover** - Distribute traffic with health checks and circuit breakers
- 📋 **Cost Tracking & Budgets** - Track token usage × pricing with budget enforcement

**Medium Priority** (Production Readiness):
- 📋 **Usage Tracking** - Per-endpoint and per-model usage analytics
- 📋 **Distributed Tracing** - OpenTelemetry integration for debugging
- 📋 **TLS/HTTPS Support** - Secure communication with automatic cert renewal
- 📋 **Request Validation** - Input sanitization and schema validation
- 📋 **Graceful Shutdown** - Handle in-flight requests during deployments
- 📋 **Admin API** - Runtime config updates and operations management

**Low Priority** (Enhancements):
- 📋 **Mock Backend Mode** - Testing without API keys
- 📋 **Hot Config Reload** - Update configuration without restart
- 📋 **Additional API Formats** - Support for Claude, Cohere, Azure OpenAI
- 📋 **WebSocket Support** - Alternative to SSE for streaming
- 📋 **Token Optimization** - Smart conversation truncation to reduce costs
- 📋 **Request Replay Tool** - Debug issues by replaying logged requests

**See [FEATURE-ROADMAP.md](docs/plans/FEATURE-ROADMAP.md) for complete feature list with detailed descriptions, implementation timelines, complexity estimates, and configuration examples.**

**Legend**: ✅ Completed | 🔄 Partially Designed | 📋 Planned

---

## License

MIT License - see LICENSE file for details.

## Contributing

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Ensure all tests pass
5. Submit a pull request

## Troubleshooting & Validation

### Validation Workflow

**ALWAYS validate changes with diagnostics API:**

```bash
# 1. Start proxy with logging
cargo run --bin louter -- --host localhost --port 9000 \
  --config config-public-api.toml \
  --log-file /tmp/proxy.jsonl \
  --verbose

# 2. Run full diagnostics (tests all features)
curl -s http://localhost:9000/api/diagnostics > /tmp/diagnostics.json

# 3. Check for failures
cat /tmp/diagnostics.json | jq '.frontends[].test_results[] |
  select(.passed == false) |
  {test: .test_name, error: .error}'
```

**If all tests pass:** ✅ Your changes are working!
**If any test fails:** ❌ Follow the debugging workflow below

### Systematic Debugging Process

When something doesn't work, follow this order:

**Step 1: Capture Full Output**
```bash
# Save FULL diagnostics (don't filter yet!)
curl -s http://localhost:9000/api/diagnostics > /tmp/diag.json
cat /tmp/diag.json | jq . > /tmp/diagnostics-readable.json
```

**Step 2: Identify the Error**
```bash
# Find which test failed
cat /tmp/diagnostics-readable.json | jq '.frontends[].test_results[] |
  select(.passed == false)'
```

**Step 3: Check Proxy Logs**
```bash
# View recent requests
tail -50 /tmp/proxy.jsonl | jq .

# Find errors
grep -i error /tmp/proxy.jsonl | jq .
```

**Step 4: Look Up Error in Documentation**

Check [docs/debug/error-map-and-solutions.md](docs/debug/error-map-and-solutions.md) for your specific error.

### Common Issues & Quick Fixes

| Issue | Symptoms | Quick Fix | Documentation |
|-------|----------|-----------|---------------|
| **API Key Not Found** | `Error: API key not found` | Set `OPENAI_API_KEY` or `GEMINI_API_KEY` env var, or add `api_key` to config | [Error 1.3](docs/debug/error-map-and-solutions.md#error-13) |
| **Model Not Found** | `404: model does not exist` | Check `model_mapping` in config matches backend's actual model names | [Error 3.3](docs/debug/error-map-and-solutions.md#error-33) |
| **max_tokens Error** | `Unsupported parameter: max_tokens` | Add `max_tokens_field = "max_completion_tokens"` to backend config | [Error 2.1](docs/debug/error-map-and-solutions.md#error-21) |
| **Temperature Error** | `temperature not supported` | Add `temperature_override = true` to backend config | [Error 2.2](docs/debug/error-map-and-solutions.md#error-22) |
| **MAX_TOKENS** | `finishReason: MAX_TOKENS` | Increase `maxOutputTokens` in request or config | [Error 2.4](docs/debug/error-map-and-solutions.md#error-24) |
| **Backend Unreachable** | `Connection refused` | Start the backend service (llama.cpp, Ollama, etc.) | [Error 3.1](docs/debug/error-map-and-solutions.md#error-31) |
| **Quota Exceeded** | `429: RESOURCE_EXHAUSTED` | Wait for quota reset or use different API key | [Error 3.2](docs/debug/error-map-and-solutions.md#error-32) |
| **Missing Field** | `Failed to parse: missing field` | Make field optional in struct with `Option<T>` | [Error 2.3](docs/debug/error-map-and-solutions.md#error-23) |

### Debugging Resources

**For systematic debugging:**
- [Standard Debugging Workflow](docs/debug/standard-debugging-workflow.md) - 10-step process for any issue

**For specific errors:**
- [Error Map & Solutions](docs/debug/error-map-and-solutions.md) - Comprehensive error catalog

**For real examples:**
- [API Type Error Debug Session](docs/debug/api-type-error-max-tokens.md) - Actual debugging walkthrough

### Validation Checklist

Before declaring "it works":

- [ ] Run diagnostics API: `curl http://localhost:9000/api/diagnostics`
- [ ] All backend tests pass (3/3 backends reachable)
- [ ] All frontend tests pass (Gemini API + OpenAI API)
- [ ] Check logs for errors: `grep -i error /tmp/proxy.jsonl`
- [ ] Test cross-protocol scenarios (Gemini→OpenAI, OpenAI→Gemini)
- [ ] Verify thinking tokens handled correctly (Gemini 2.5+)

### Debug Mode & Logging

**Enable verbose logging:**
```bash
cargo run --bin louter -- --host localhost --port 9000 \
  --config config-public-api.toml \
  --log-file /tmp/debug.jsonl \
  --verbose
```

**Analyze logs:**
```bash
# Show recent activity
tail -20 /tmp/debug.jsonl | jq .

# Filter by error
cat /tmp/debug.jsonl | jq 'select(.error != null)'

# Check specific test
grep "What's in this image" /tmp/debug.jsonl | jq .
```

**Use Web UI for real-time debugging:**
```bash
# Open in browser
open http://localhost:9000/ui

# Features:
# - Real-time log viewer with filtering
# - Performance metrics (TTFT, TPS, ITL)
# - Search and pagination
# - Auto-refresh every 5 seconds
```

### When to Use Each Tool

| Tool | Use When | Output |
|------|----------|--------|
| **Diagnostics API** | Testing all features at once | JSON with pass/fail results |
| **Logs (`--log-file`)** | Debugging specific requests | JSON Lines with full request/response |
| **Web UI (`/ui`)** | Real-time monitoring | Visual dashboard with metrics |
| **Verbose (`--verbose`)** | Debugging protocol conversion | Detailed console output |

### Still Stuck?

1. **Check if it's a known issue:** [docs/issues/](docs/issues/)
2. **Look for similar resolved issues:** [docs/resolved/](docs/resolved/)
3. **Follow complete workflow:** [docs/debug/standard-debugging-workflow.md](docs/debug/standard-debugging-workflow.md)
4. **Read CRITICAL CODING RULES:** [CLAUDE.md](CLAUDE.md#critical-coding-rules)

## Documentation

For detailed documentation, see the **[docs/](docs/)** directory:

### Quick Links
- **[Getting Started](docs/tutorial/README.md)** - Tutorials and quick start guides
- **[Debugging Guide](docs/debug/standard-debugging-workflow.md)** - Step-by-step debugging workflow
- **[Error Solutions](docs/debug/error-map-and-solutions.md)** - Common errors and fixes
- **[Feature Roadmap](docs/plans/FEATURE-ROADMAP.md)** - Planned features and timeline
- **[Development Guide](CLAUDE.md)** - Developer workflow and critical coding rules

### Documentation Structure
- **[docs/tutorial/](docs/tutorial/)** - Getting started guides and tutorials
- **[docs/debug/](docs/debug/)** - Debugging techniques and error solutions
- **[docs/design/](docs/design/)** - Architecture and design decisions
- **[docs/plans/](docs/plans/)** - Future roadmap and feature plans
- **[docs/issues/](docs/issues/)** - Current known issues
- **[docs/resolved/](docs/resolved/)** - Historical fixes and debugging sessions

## Support

- GitHub Issues: Report bugs and feature requests
- Documentation: See [docs/](docs/) for detailed guides
- Examples: Check test files for usage patterns