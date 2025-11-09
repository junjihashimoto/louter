# Louter - LLM Router

## Goal
- Create an intelligent routing system for Large Language Models supporting multiple API formats
- **Large-scale deployment support** with centralized API key management (not multi-tenant)
- Allow Gemini API clients to access OpenAI models through familiar Gemini interface
- Allow OpenAI API clients to access Gemini models through familiar OpenAI interface
- Support all combinations: Gemini→OpenAI, Gemini→Gemini, OpenAI→OpenAI, OpenAI→Gemini
- Content-based routing with automatic backend selection based on capabilities
- Support multiple backends with fallback chains and load balancing

# Architecture
- **Dual Frontend Support**: Accept both Gemini API and OpenAI API requests
- **Dual Backend Support**: Route to either OpenAI or Gemini backends
- **Bidirectional Conversion**: Automatic format conversion (camelCase ↔ snake_case)
- **Centralized API Key Management**: Backend API keys from config/env (client headers ignored)
- **Complete Logging**: JSON Lines logging for all request/response pairs
- **Performance Metrics**: TTFT, TPS, ITL, e2e latency tracking (NVIDIA NIM-compliant)
- **Web UI Dashboard**: Real-time log viewer and metrics visualization

# Technology Stack
- Rust with Axum web framework
- TOML config files for backend configuration
- SSE (Server-Sent Events) for streaming
- Prometheus for metrics export
- Domain models for both Gemini and OpenAI APIs

# 🔄 Development Workflow

**Core Flow:** Implement → Test → Debug → Document

## Quick Checklist

When implementing any feature:

- [ ] **1. Implement** - Write the code following CRITICAL CODING RULES below
- [ ] **2. Build** - `cargo build --bin louter`
- [ ] **3. Test** - Run diagnostics API to verify
- [ ] **4. Debug** - If fails, follow standard debugging workflow
- [ ] **5. Document** - Add to appropriate `docs/` directory

## Detailed Steps

### Step 1: Implement Feature

**Files you'll typically modify:**
- `src/config.rs` - If adding config options
- `src/conversion.rs` - If changing protocol conversion
- `src/main.rs` - If adding endpoints or routing logic
- `src/models/` - If changing data structures
- `src/diagnostic.rs` - If adding diagnostic tests (for `/api/diagnostics`)

**Follow:** CRITICAL CODING RULES (below)

### Step 2: Build & Start Proxy

```bash
# Build
cargo build --bin louter

# Start with logging
cargo run --bin louter -- --host localhost --port 9000 \
  --config config-public-api.toml \
  --log-file /tmp/proxy.jsonl \
  --verbose
```

### Step 3: Test with Diagnostics

```bash
# Run diagnostics (saves to file automatically)
curl -s http://localhost:9000/api/diagnostics > /tmp/diagnostics.json

# Check results
cat /tmp/diagnostics.json | jq '.frontends[].test_results[] |
  select(.passed == false) |
  {test: .test_name, error: .error}'
```

**If all tests pass:** ✅ Done! Go to Step 5 (Document)
**If any test fails:** ❌ Go to Step 4 (Debug)

### Step 4: Debug Issues

**ALWAYS follow this order:**

1. **Save full diagnostics** - Already done in Step 3
2. **Read error message** - `cat /tmp/diagnostics.json`
3. **Check proxy logs** - `tail -50 /tmp/proxy.jsonl | jq .`
4. **Follow standard workflow** - See [`docs/debug/standard-debugging-workflow.md`](docs/debug/standard-debugging-workflow.md)

**Quick debug reference:**

| Error Type | Debug Guide |
|------------|-------------|
| "Unsupported parameter" | [`docs/debug/error-map-and-solutions.md#error-21`](docs/debug/error-map-and-solutions.md) |
| "Missing field" | [`docs/debug/error-map-and-solutions.md#error-23`](docs/debug/error-map-and-solutions.md) |
| "Backend not configured" | [`docs/debug/error-map-and-solutions.md#error-11`](docs/debug/error-map-and-solutions.md) |
| MAX_TOKENS errors | [`docs/debug/error-map-and-solutions.md#error-24`](docs/debug/error-map-and-solutions.md) |
| Any other error | [`docs/debug/standard-debugging-workflow.md`](docs/debug/standard-debugging-workflow.md) |

### Step 5: Document Changes

**If you fixed a bug:**
```bash
# Create resolved issue document
cat > docs/resolved/$(date +%Y-%m-%d)-issue-name-RESOLVED.md
```

**If you added a feature:**
- Update `CLAUDE.md` (this file) if it's a critical rule
- Add tutorial to `docs/tutorial/` if users need to know
- Add design doc to `docs/design/` if architecture changed

**See:** [`docs/README.md`](docs/README.md) for documentation organization

## When Things Go Wrong

**"I'm stuck debugging"**
→ Read [`docs/debug/standard-debugging-workflow.md`](docs/debug/standard-debugging-workflow.md)

**"I don't know what's causing the error"**
→ Check [`docs/debug/error-map-and-solutions.md`](docs/debug/error-map-and-solutions.md)

**"Test passes locally but fails in diagnostics"**
→ Compare logs: `grep "test_name" /tmp/proxy.jsonl | jq .`

**"Backend works directly but fails through proxy"**
→ Check protocol conversion in logs

## ⚠️ CRITICAL CODING RULES

### 1. NEVER Hardcode Model Names
**RULE:** All model names MUST come from `config.toml` file's `model_mapping` section.

**❌ WRONG:**
```rust
let model = "gpt-3.5-turbo";  // NEVER hardcode!
let model = "gemini-pro";      // NEVER hardcode!
```

**✅ CORRECT:**
```rust
// Get model from backend's model_mapping
let model = backend_config.model_mapping.values().next()
    .cloned()
    .ok_or("No model_mapping configured")?;
```

**Why:** Different deployments use different models. Hardcoding breaks flexibility.

### 2. Vision Tests Use Base64 Format
**RULE:** Vision capability tests MUST use base64-encoded images, not external URLs.

**Note:** The enum name `ImageUrl` is misleading - it accepts base64 data URLs.

### 3. Frontend Diagnostics MUST Test Cross-Protocol Scenarios
**RULE:** Frontend diagnostics must test ALL 4 protocol combinations to verify bidirectional conversion.

**Required Test Matrix:**
```
Frontend API    Backend Protocol    Test Name
-----------    ----------------    ---------
Gemini API  →  Gemini backend     (native)
Gemini API  →  OpenAI backend     (conversion)
OpenAI API  →  OpenAI backend     (native)
OpenAI API  →  Gemini backend     (conversion)
```

**Why:** Cross-protocol conversion is a core feature. Must verify Gemini↔OpenAI conversion works in both directions.

### 4. Token Count Mapping (Gemini ↔ OpenAI)
**RULE:** Gemini separates output tokens into visible and thinking tokens. OpenAI combines them.

**Gemini → OpenAI:**
```rust
completion_tokens = candidates_token_count + thoughts_token_count
```

**OpenAI → Gemini:**
```rust
candidates_token_count = completion_tokens
thoughts_token_count = None  // OpenAI doesn't separate them
```

See `src/models/gemini.rs:156-185` for detailed documentation.

# Implementation Status

## Core Features - ✅ Completed
- ✅ Domain models (Gemini & OpenAI APIs with proper camelCase/snake_case)
- ✅ Bidirectional conversion logic (Gemini ↔ OpenAI)
- ✅ Streaming SSE support for both formats
- ✅ Configuration management (TOML)
- ✅ Multi-backend support with flexible configuration
- ✅ Function calling conversion (functionCall ↔ tool_calls)
- ✅ XML tool format support (Qwen3-Coder compatibility)
- ✅ Multimedia support (images, audio, video, PDF)
- ✅ Content-based routing with automatic capability detection
- ✅ Performance metrics (TTFT, TPS, ITL, e2e)
- ✅ Prometheus metrics export
- ✅ Web UI Dashboard with real-time visualization
- ✅ JSON Lines logging for all request/response pairs
- ✅ Automated diagnostic testing

## Recent Fixes (2025-11-21)

### ✅ Issue 1: Configurable Temperature Per-Backend
**Problem:** Some backends (e.g., OpenAI gpt-5-nano) don't support custom temperature values.

**Solution:** Added `temperature_override` config option.
```toml
[backends.openai]
temperature_override = true  # Override client's temperature with backend's value
# temperature not set = use default
```

**Files:** `src/config.rs`, `src/conversion.rs:286-296`, `config-public-api.toml`

### ✅ Issue 2: MAX_TOKENS Errors with Thinking Tokens
**Problem:** Gemini 2.5+ uses thinking tokens, causing MAX_TOKENS errors with low limits.

**Solution:** Increased diagnostic test token limits.
- Vision tests: 400 → 1000 tokens
- Text/function tests: 200 → 500 tokens

**Files:** `src/diagnostic.rs:886,896`

### ✅ Issue 3: Optional Fields in Gemini Responses
**Problem:** Gemini responses missing `candidatesTokenCount` or `parts` when blocked or MAX_TOKENS.

**Solution:** Made fields optional with proper defaults.
- `candidates_token_count: Option<i32>` (missing when blocked)
- `parts: Vec<Part>` with `#[serde(default)]` (empty when MAX_TOKENS)
- `thoughts_token_count: Option<i32>` (Gemini 2.5+ thinking tokens)

**Files:** `src/models/gemini.rs:156-185`, `src/conversion.rs:400-409,970-985`

### ✅ Issue 4: max_tokens vs max_completion_tokens
**Problem:** OpenAI models have different token field requirements (older: `max_tokens`, newer: `max_completion_tokens`).

**Solution:** Added `max_tokens_field` config option.
```toml
[backends.openai]
max_tokens_field = "max_completion_tokens"  # For newer OpenAI models

[backends.gpt-oss]
max_tokens_field = "max_tokens"  # For llama.cpp server
```

**Files:** `src/config.rs`, `src/conversion.rs:278-284`, `config-public-api.toml`

## Per-Backend Configuration Options

### Backend-Specific Settings
```toml
[backends.<name>]
url = "https://api.example.com"
protocol = "openai"  # or "gemini"
temperature_override = false  # Override client's temperature
max_tokens_field = "both"  # "max_tokens", "max_completion_tokens", or "both"
tool_format = "json"  # or "xml" (for Qwen3-Coder)
custom_instruction = "..."  # Per-backend system instruction
custom_instruction_mode = "append"  # "override", "prepend", or "append"

# Routing configuration
capabilities = ["text", "vision", "function_calling"]
priority = 1  # Lower number = higher priority
fallback = "other-backend"  # Fallback backend name

[backends.<name>.model_mapping]
"frontend-model-name" = "backend-model-name"
```

## API Key Management (Design Decision)

**Centralized API Key Management:** The proxy uses API keys from environment variables or config file for all backend requests. This is the intended design for **large-scale deployments**.

**Design Goals:**
- **Large-scale production** - Handle high request volumes with centralized API key management
- **Single point of control** - Backend API keys managed by proxy operator, not clients
- **Cost control** - Centralized billing and usage tracking under one API key
- **Security** - Backend API keys never exposed to clients

**Client Authorization headers are ignored** - this is intentional, not a bug.

**Not designed for:**
- Multi-tenant deployments (different users with different API keys)
- Client-provided API key passthrough

## Future Enhancements

**High Priority** (Production Essentials):
- 📋 **Response Caching** - Reduce costs by 40-80% with Redis/in-memory cache
- 📋 **Rate Limiting** - Per-IP and global limits for abuse prevention and cost control
- 🔄 **Load Balancing & Failover** - Distribute traffic with health checks and circuit breakers
- 📋 **Cost Tracking & Budgets** - Track token usage × pricing with budget enforcement

**Medium Priority** (Production Readiness):
- 📋 **Usage Tracking** - Per-endpoint and per-model usage analytics
- 📋 **Distributed Tracing** - OpenTelemetry integration for debugging
- 📋 **TLS/HTTPS Support** - Secure communication with automatic cert renewal

**Low Priority** (Enhancements):
- 📋 **Hot Config Reload** - Update configuration without restart
- 📋 **Additional API Formats** - Support for Claude, Cohere, Azure OpenAI
- 📋 **WebSocket Support** - Alternative to SSE for streaming

Legend: ✅ Completed | 🔄 Partially Designed | 📋 Planned

# Quick Start

## Starting the Proxy Server

```bash
# Start with config file
cargo run --bin louter -- --host localhost --port 8080 --config config.toml --log-file proxy.jsonl --verbose

# Health check
curl http://localhost:8080/health

# View Web UI Dashboard
open http://localhost:8080/ui

# View Prometheus metrics
curl http://localhost:8080/metrics
```

## Example Usage

### Gemini API Frontend
```bash
# Generate content
curl -X POST 'http://localhost:8080/v1beta/models/gemini-2.0-flash:generateContent' \
  -H "Content-Type: application/json" \
  -d '{
    "contents": [{
      "parts": [{"text": "Hello, world!"}],
      "role": "user"
    }]
  }'
```

### OpenAI API Frontend
```bash
# Chat completions
curl -X POST 'http://localhost:8080/v1/chat/completions' \
  -H "Content-Type: application/json" \
  -d '{
    "model": "gemma3-4b",
    "messages": [{"role": "user", "content": "Hello!"}],
    "max_tokens": 100
  }'
```

## Development Tools

```bash
# Run diagnostic tests
curl -s http://localhost:9000/api/diagnostics > diagnostics.json

# Parse logs (recommended - provides structured queries)
cargo run --bin log-parser -- --file proxy.jsonl stats
cargo run --bin log-parser -- --file proxy.jsonl functions
cargo run --bin log-parser -- --file proxy.jsonl pairs

# Or use jq directly (requires knowledge of log schema)
# See docs/design/log-format.md for schema documentation
tail -20 proxy.jsonl | jq .
cat proxy.jsonl | jq 'select(.direction == "client_request")'
```

# Documentation Organization

Project documentation is organized under `docs/` directory:

- **`docs/tutorial/`** - Getting started guides and tutorials
- **`docs/design/`** - Architecture and design decisions
- **`docs/issues/`** - Known issues and limitations
- **`docs/plans/`** - Future feature plans and roadmaps
- **`docs/resolved/`** - Resolved issues and fixes
- **`docs/debug/`** - Debugging techniques and troubleshooting

Historical development documents (test results, debug sessions) should be moved to appropriate subdirectories before release.

# Additional Resources

- **README.md** - Project overview and installation instructions
- **config-public-api.toml** - Example configuration for public APIs
- **config-mock.toml** - Example configuration for local testing
- **test-*.sh** - Test scripts for various scenarios
