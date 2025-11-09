# Louter (LLM Router) Tutorials

This document provides step-by-step tutorials for using louter with various LLM backends and debugging techniques.

## Table of Contents

1. [Quick Start Tutorial](#quick-start-tutorial)
2. [Using with OpenAI API](#using-with-openai-api)
3. [Using with llama-server + Qwen](#using-with-llama-server--qwen)
4. [Using with llama-server + GPT-OSS](#using-with-llama-server--gpt-oss)
5. [Using with gemini-cli](#using-with-gemini-cli)
6. [Complete Setup: gemini-cli + Proxy + llama-server](#complete-setup-gemini-cli--proxy--llama-server)
7. [Debugging Techniques](#debugging-techniques)
8. [Advanced Use Cases](#advanced-use-cases)

---

## Quick Start Tutorial

### Prerequisites

- Rust toolchain installed (1.70+)
- API keys (if using cloud services)
- Internet connection

### Step 1: Build the Proxy

```bash
git clone <repository-url>
cd louter
cargo build --release
```

### Step 2: Create Configuration

```bash
cp config.toml.example config.toml
```

Edit `config.toml`:

```toml
[backends.openai]
url = "https://api.openai.com"

[backends.gemini]
url = "https://generativelanguage.googleapis.com"
```

### Step 3: Set API Keys

```bash
export OPENAI_API_KEY="your-key-here"
export GEMINI_API_KEY="your-key-here"
```

### Step 4: Start the Proxy

```bash
cargo run --bin louter -- --backend openai --port 8080 --verbose
```

### Step 5: Test It

```bash
curl http://localhost:8080/health
```

---

## Using with OpenAI API

### What is OpenAI API?

The OpenAI API provides access to various language models including GPT-4, GPT-4 Turbo, GPT-3.5 Turbo, and others. These models support text generation, chat completion, function calling, and more.

**Note**: OpenAI Codex (code-davinci-002, code-cushman-001) has been deprecated since March 2023. Use GPT-4 or GPT-3.5 Turbo for code generation tasks instead.

### Tutorial: OpenAI API Through Proxy

**Goal**: Access OpenAI models through the proxy with optional format conversion.

#### 1. Get OpenAI API Key

Visit: https://platform.openai.com/account/api-keys

```bash
export OPENAI_API_KEY="your-openai-api-key"
```

#### 2. Configure Proxy

`config.toml`:

```toml
[backends.openai]
url = "https://api.openai.com"
# api_key = "your-key"  # Optional, uses OPENAI_API_KEY env var

[backends.openai.model_mapping]
"gemini-2.0-flash" = "gpt-4"
"gemini-pro" = "gpt-3.5-turbo"
"gemini-1.5-pro" = "gpt-4-turbo"
```

#### 3. Start Proxy in OpenAI Mode

```bash
cargo run --bin louter -- \
  --backend openai \
  --port 8080 \
  --config config.toml \
  --log-file openai-proxy.jsonl \
  --verbose
```

#### 4. Test with OpenAI API Format (Direct)

```bash
curl -X POST 'http://localhost:8080/v1/chat/completions' \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer $OPENAI_API_KEY" \
  -d '{
    "model": "gpt-4",
    "messages": [
      {"role": "user", "content": "Write a Python function to calculate factorial"}
    ],
    "max_tokens": 200
  }'
```

#### 5. Test Code Generation

```bash
curl -X POST 'http://localhost:8080/v1/chat/completions' \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer $OPENAI_API_KEY" \
  -d '{
    "model": "gpt-4",
    "messages": [
      {"role": "system", "content": "You are a code completion assistant."},
      {"role": "user", "content": "Complete this Python function:\ndef fibonacci(n):\n    # TODO"}
    ],
    "max_tokens": 300,
    "temperature": 0.2
  }'
```

#### 6. Test with Gemini API Format (Conversion)

The proxy can convert Gemini API requests to OpenAI Codex format:

```bash
curl -X POST 'http://localhost:8080/v1beta/models/gpt-4:generateContent' \
  -H "Content-Type: application/json" \
  -d '{
    "contents": [{
      "parts": [{"text": "Write a function to reverse a linked list in C++"}],
      "role": "user"
    }]
  }'
```

#### 7. Test Streaming

```bash
curl -X POST 'http://localhost:8080/v1/chat/completions' \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer $OPENAI_API_KEY" \
  -d '{
    "model": "gpt-4",
    "messages": [
      {"role": "user", "content": "Explain async/await in JavaScript with examples"}
    ],
    "stream": true
  }'
```

#### 8. Test Function Calling

```bash
curl -X POST 'http://localhost:8080/v1/chat/completions' \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer $OPENAI_API_KEY" \
  -d '{
    "model": "gpt-4",
    "messages": [
      {"role": "user", "content": "Execute Python code: print(2 + 2)"}
    ],
    "tools": [{
      "type": "function",
      "function": {
        "name": "execute_code",
        "description": "Execute Python code and return the result",
        "parameters": {
          "type": "object",
          "properties": {
            "code": {
              "type": "string",
              "description": "Python code to execute"
            }
          },
          "required": ["code"]
        }
      }
    }],
    "tool_choice": "auto"
  }'
```

#### 9. Analyze Logs

```bash
cargo run --bin log-parser -- --file openai-proxy.jsonl functions
cargo run --bin log-parser -- --file openai-proxy.jsonl stats
cargo run --bin log-parser -- --file openai-proxy.jsonl pairs
```

---

## Using with llama-server + Qwen

### What is llama-server?

llama-server is the HTTP server component of llama.cpp, providing an OpenAI-compatible API for running local LLMs.

### Tutorial: Proxy with Qwen3-Coder (XML Tool Format)

**Goal**: Run Qwen3-Coder locally with function calling support.

#### 1. Install llama.cpp

```bash
git clone https://github.com/ggerganov/llama.cpp
cd llama.cpp
mkdir build && cd build
cmake .. -DGGML_CUDA=ON  # Use CUDA if available
cmake --build . --config Release
```

#### 2. Download Qwen3-Coder Model

```bash
# Download from Hugging Face
huggingface-cli download \
  Qwen/Qwen3-Coder-30B-A3B-Instruct-GGUF \
  qwen3-coder-30b-a3b-instruct-q4_k_m.gguf \
  --local-dir ./models
```

#### 3. Start llama-server with Qwen

```bash
cd llama.cpp/build/bin

./llama-server \
  -m ../../models/qwen3-coder-30b-a3b-instruct-q4_k_m.gguf \
  --port 11212 \
  --ctx-size 8192 \
  --n-gpu-layers 50 \
  --threads 8 \
  --parallel 4 \
  --log-format text \
  --verbose
```

#### 4. Configure Proxy for Qwen (XML Tool Format)

Create `qwen-config.toml`:

```toml
[performance]
enable_metrics = true
log_requests = true
timeout_seconds = 120

[backends.qwen]
url = "http://localhost:11212"
tool_format = "xml"  # Qwen3-Coder uses XML format for tool calls
max_tokens = 4096
temperature = 0.7

[backends.qwen.model_mapping]
"qwen3-coder" = "Qwen3-Coder-30B-A3B-Instruct"
"qwen-coder" = "Qwen3-Coder-30B-A3B-Instruct"
```

#### 5. Start Proxy for Qwen

```bash
cd louter

cargo run --bin louter -- \
  --backend qwen \
  --port 8080 \
  --config qwen-config.toml \
  --log-file qwen-proxy.jsonl \
  --verbose
```

#### 6. Test with OpenAI API Format

```bash
curl -X POST 'http://localhost:8080/v1/chat/completions' \
  -H "Content-Type: application/json" \
  -d '{
    "model": "qwen3-coder",
    "messages": [
      {"role": "user", "content": "Write a Python function to reverse a string"}
    ],
    "max_tokens": 500,
    "temperature": 0.7
  }'
```

#### 7. Test Function Calling with Qwen (XML Format)

```bash
curl -X POST 'http://localhost:8080/v1/chat/completions' \
  -H "Content-Type: application/json" \
  -d '{
    "model": "qwen3-coder",
    "messages": [
      {"role": "user", "content": "Create a file named test.txt with content Hello World"}
    ],
    "tools": [{
      "type": "function",
      "function": {
        "name": "WriteFile",
        "description": "Write content to a file",
        "parameters": {
          "type": "object",
          "properties": {
            "file_path": {
              "type": "string",
              "description": "Path to the file"
            },
            "content": {
              "type": "string",
              "description": "Content to write"
            }
          },
          "required": ["file_path", "content"]
        }
      }
    }],
    "tool_choice": "auto"
  }' | jq .
```

**Expected Output**: The proxy will automatically convert Qwen's XML tool call format to JSON:

```json
{
  "id": "chatcmpl-...",
  "object": "chat.completion",
  "choices": [{
    "message": {
      "role": "assistant",
      "tool_calls": [{
        "id": "call_0",
        "type": "function",
        "function": {
          "name": "WriteFile",
          "arguments": "{\"file_path\":\"test.txt\",\"content\":\"Hello World\"}"
        }
      }]
    },
    "finish_reason": "tool_calls"
  }]
}
```

#### 8. Test Streaming

```bash
curl -X POST 'http://localhost:8080/v1/chat/completions' \
  -H "Content-Type: application/json" \
  -d '{
    "model": "qwen3-coder",
    "messages": [
      {"role": "user", "content": "Explain async/await in Python"}
    ],
    "stream": true
  }'
```

#### 9. Monitor Performance

```bash
# Check Prometheus metrics
curl http://localhost:8080/metrics | grep qwen

# Parse logs
cargo run --bin log-parser -- --file qwen-proxy.jsonl stats
cargo run --bin log-parser -- --file qwen-proxy.jsonl functions
```

---

## Using with llama-server + GPT-OSS

### Tutorial: Proxy with GPT-OSS (JSON Tool Format)

**Goal**: Run GPT-OSS model locally with standard JSON function calling.

#### 1. Download GPT-OSS Model

```bash
huggingface-cli download \
  your-org/gpt-oss-20b-gguf \
  gpt-oss-20b-q4_k_m.gguf \
  --local-dir ./models
```

#### 2. Start llama-server with GPT-OSS

```bash
cd llama.cpp/build/bin

./llama-server \
  -m ../../models/gpt-oss-20b-q4_k_m.gguf \
  --port 11211 \
  --ctx-size 4096 \
  --n-gpu-layers 30 \
  --threads 8 \
  --parallel 2 \
  --verbose
```

#### 3. Configure Proxy for GPT-OSS

Create `gpt-oss-config.toml`:

```toml
[performance]
enable_metrics = true
log_requests = true
timeout_seconds = 60

[backends.gpt-oss]
url = "http://localhost:11211"
tool_format = "json"  # Standard JSON tool format
max_tokens = 2048
temperature = 0.8

[backends.gpt-oss.model_mapping]
"gpt-oss-20b" = "gpt-oss:20b"
"gpt-oss" = "gpt-oss:20b"
```

#### 4. Start Proxy for GPT-OSS

```bash
cargo run --bin louter -- \
  --backend gpt-oss \
  --port 8081 \
  --config gpt-oss-config.toml \
  --log-file gpt-oss-proxy.jsonl \
  --verbose
```

#### 5. Test with OpenAI Format

```bash
curl -X POST 'http://localhost:8081/v1/chat/completions' \
  -H "Content-Type: application/json" \
  -d '{
    "model": "gpt-oss-20b",
    "messages": [
      {"role": "user", "content": "Explain Docker containers"}
    ],
    "max_tokens": 300
  }' | jq .
```

#### 6. Test Function Calling (JSON Format)

```bash
curl -X POST 'http://localhost:8081/v1/chat/completions' \
  -H "Content-Type: application/json" \
  -d '{
    "model": "gpt-oss-20b",
    "messages": [
      {"role": "user", "content": "Get the weather for New York"}
    ],
    "tools": [{
      "type": "function",
      "function": {
        "name": "getWeather",
        "description": "Get weather for a location",
        "parameters": {
          "type": "object",
          "properties": {
            "location": {"type": "string"}
          },
          "required": ["location"]
        }
      }
    }]
  }' | jq .
```

---

## Using with gemini-cli

### What is gemini-cli?

gemini-cli is a command-line tool for interacting with Gemini API, which can be configured to use the proxy as its backend.

#### 1. Install gemini-cli

```bash
# Install from npm
npm install -g @google/generative-ai-cli

# Or build from source
git clone https://github.com/google/generative-ai-cli
cd generative-ai-cli
npm install
npm link
```

#### 2. Start Proxy

```bash
cargo run --bin louter -- \
  --backend gemini \
  --port 8080 \
  --config config.toml \
  --verbose
```

#### 3. Configure gemini-cli to Use Proxy

Set environment variable to point to proxy:

```bash
export GEMINI_API_BASE_URL="http://localhost:8080"
export GEMINI_API_KEY="your-api-key"
```

#### 4. Test gemini-cli Through Proxy

```bash
# Simple query
gemini-cli "What is Rust programming language?"

# With streaming
gemini-cli --stream "Explain machine learning"

# With function calling
gemini-cli --tools weather.json "What's the weather in London?"
```

#### 5. Monitor Proxy Logs

```bash
# In another terminal, watch the proxy logs
tail -f proxy.jsonl | jq .
```

---

## Complete Setup: gemini-cli + Proxy + llama-server

### Scenario: Multi-Backend Setup

**Goal**: Use gemini-cli to interact with both Gemini API and local llama-server through a single proxy.

#### Architecture

```
gemini-cli → Proxy (port 8080) → [Backend Selection]
                                   ├─→ Gemini API (cloud)
                                   ├─→ llama-server:11211 (GPT-OSS)
                                   └─→ llama-server:11212 (Qwen3-Coder)
```

#### 1. Start llama-servers

**Terminal 1** (GPT-OSS):
```bash
cd llama.cpp/build/bin
./llama-server \
  -m ../../models/gpt-oss-20b-q4_k_m.gguf \
  --port 11211 \
  --ctx-size 4096 \
  --n-gpu-layers 30
```

**Terminal 2** (Qwen3-Coder):
```bash
cd llama.cpp/build/bin
./llama-server \
  -m ../../models/qwen3-coder-30b-a3b-instruct-q4_k_m.gguf \
  --port 11212 \
  --ctx-size 8192 \
  --n-gpu-layers 50
```

#### 2. Configure Multi-Backend Proxy

Create `multi-backend-config.toml`:

```toml
[performance]
enable_metrics = true
log_requests = true
timeout_seconds = 120
openai_token_counting_mode = "estimate"

# Gemini API backend
[backends.gemini]
url = "https://generativelanguage.googleapis.com"
weight = 1.0
tool_format = "json"

# GPT-OSS backend
[backends.gpt-oss]
url = "http://localhost:11211"
weight = 1.0
tool_format = "json"
max_tokens = 2048

[backends.gpt-oss.model_mapping]
"gpt-oss-20b" = "gpt-oss:20b"

# Qwen3-Coder backend
[backends.qwen]
url = "http://localhost:11212"
weight = 1.0
tool_format = "xml"  # Qwen uses XML format
max_tokens = 4096

[backends.qwen.model_mapping]
"qwen3-coder" = "Qwen3-Coder-30B-A3B-Instruct"
```

#### 3. Start Proxy

**Terminal 3**:
```bash
cd louter

cargo run --bin louter -- \
  --backend openai \
  --port 8080 \
  --config multi-backend-config.toml \
  --log-file multi-backend.jsonl \
  --verbose
```

#### 4. Test Each Backend Through gemini-cli

```bash
export GEMINI_API_BASE_URL="http://localhost:8080"
export GEMINI_API_KEY="dummy"  # Proxy doesn't require auth

# Test with GPT-OSS
gemini-cli --model gpt-oss-20b "Explain REST APIs"

# Test with Qwen3-Coder
gemini-cli --model qwen3-coder "Write a sorting algorithm"

# Test with cloud Gemini (if configured)
gemini-cli --model gemini-2.0-flash "What is quantum computing?"
```

#### 5. Monitor All Activity

**Terminal 4**:
```bash
# Watch live logs
cargo run --bin log-parser -- --file multi-backend.jsonl all

# Check metrics
curl http://localhost:8080/metrics | grep -E "backend|requests|tokens"

# Generate statistics
cargo run --bin log-parser -- --file multi-backend.jsonl stats
```

#### 6. Test Function Calling Across Backends

```bash
# Create tools definition
cat > tools.json <<EOF
[{
  "function_declarations": [{
    "name": "calculatePrice",
    "description": "Calculate total price with tax",
    "parameters": {
      "type": "object",
      "properties": {
        "price": {"type": "number"},
        "tax_rate": {"type": "number"}
      },
      "required": ["price", "tax_rate"]
    }
  }]
}]
EOF

# Test with each backend
gemini-cli --model gpt-oss-20b --tools tools.json \
  "Calculate price for $100 with 8% tax"

gemini-cli --model qwen3-coder --tools tools.json \
  "Calculate price for €50 with 20% tax"
```

---

## Debugging Techniques

### 1. Enable Verbose Logging

**Why**: See detailed request/response information in real-time.

```bash
cargo run --bin louter -- \
  --backend openai \
  --port 8080 \
  --verbose \
  --log-file debug.jsonl
```

**Output Example**:
```
[INFO] Starting Louter - LLM Router
[INFO] Backend: openai
[INFO] Port: 8080
[DEBUG] Request body: {"contents":[{"parts":[{"text":"Hello"}],"role":"user"}]}
[DEBUG] OpenAI request: {"model":"gpt-4","messages":[{"role":"user","content":"Hello"}]}
[DEBUG] OpenAI response: {"id":"chatcmpl-...","choices":[...]}
```

### 2. JSON Lines Logging

**Why**: Structured logs for post-mortem analysis.

```bash
# Enable logging
cargo run --bin louter -- \
  --log-file proxy.jsonl \
  --verbose

# Analyze logs
cargo run --bin log-parser -- --file proxy.jsonl all
cargo run --bin log-parser -- --file proxy.jsonl stats
cargo run --bin log-parser -- --file proxy.jsonl functions
cargo run --bin log-parser -- --file proxy.jsonl pairs
```

**Log Entry Example**:
```json
{
  "timestamp": "2024-12-19T10:30:45.123Z",
  "direction": "client_request",
  "format": "gemini",
  "endpoint": "/v1beta/models/gemini-2.0-flash:generateContent",
  "payload": {...}
}
```

### 3. Filter Logs by Direction

**Why**: Isolate specific parts of the request/response chain.

```bash
# Show only backend requests
cargo run --bin log-parser -- \
  --file proxy.jsonl \
  filter --direction backend_request

# Show only client responses
cargo run --bin log-parser -- \
  --file proxy.jsonl \
  filter --direction client_response

# Show only errors
cargo run --bin log-parser -- \
  --file proxy.jsonl \
  filter --direction error
```

### 4. Diagnose Function Calling Issues

**Why**: Function calling is complex - this helps identify conversion problems.

```bash
# Extract all function calls from logs
cargo run --bin log-parser -- \
  --file proxy.jsonl \
  functions

# Look for malformed tool definitions
cargo run --bin log-parser -- \
  --file proxy.jsonl \
  functions | jq '.tool_calls[] | select(.function.arguments == "{}")'
```

**Common Issues**:

- **Empty arguments `{}`**: Check XML parsing (Qwen) or backend response format
- **"undefined_tool_name"**: Backend didn't recognize the function
- **Streaming termination**: Check for `index` vs `id` field in tool_call deltas

### 5. Test Individual APIs Directly

**Why**: Isolate whether the issue is in the proxy or the backend.

```bash
# Test OpenAI backend directly
cargo run --bin test-runner -- \
  --log-file direct-openai.jsonl \
  openai http://localhost:11211 "" text

# Test Gemini API directly
cargo run --bin test-runner -- \
  --log-file direct-gemini.jsonl \
  gemini https://generativelanguage.googleapis.com YOUR_KEY function
```

### 6. Automated Diagnostic Testing

**Why**: Quickly verify all features work.

```bash
# Test all features with OpenAI backend
cargo run --bin diagnostic -- \
  --backend openai \
  --port 9000 \
  --config llama-cpp-config.toml

# Test only Gemini frontend
cargo run --bin diagnostic -- \
  --backend openai \
  --skip-openai-frontend

# Test only OpenAI frontend
cargo run --bin diagnostic -- \
  --backend openai \
  --skip-gemini-frontend
```

**Output Example**:
```
=== Diagnostic Test Summary ===
Gemini Frontend Tests:
  ✅ Gemini Text Generation
  ✅ Gemini Function Calling
  ✅ Gemini Streaming
  ❌ Gemini Multimedia (Error: ...)

OpenAI Frontend Tests:
  ✅ OpenAI Text Generation
  ✅ OpenAI Function Calling
  ✅ OpenAI Streaming
  ✅ OpenAI Multimedia

Overall: 7/8 tests passed
```

### 7. Debug Streaming Issues

**Why**: Streaming has special edge cases (buffering, chunking, SSE format).

```bash
# Test with verbose + streaming
curl -X POST 'http://localhost:8080/v1/chat/completions' \
  -H "Content-Type: application/json" \
  -d '{
    "model": "qwen3-coder",
    "messages": [{"role": "user", "content": "Count to 5"}],
    "stream": true
  }' -v 2>&1 | tee stream-debug.log

# Check for:
# - "data: [DONE]" at the end
# - Proper SSE format (data: prefix)
# - No malformed JSON chunks
# - Connection kept alive
```

### 8. Monitor Token Usage

**Why**: Track costs and performance.

```bash
# Check token counts in logs
cargo run --bin log-parser -- \
  --file proxy.jsonl \
  all | jq '.payload.usage // .payload.usageMetadata'

# Prometheus metrics
curl http://localhost:8080/metrics | grep tokens_total
```

### 9. Capture Network Traffic

**Why**: See raw HTTP traffic to diagnose protocol issues.

```bash
# Use tcpdump
sudo tcpdump -i lo0 -A -s 0 'tcp port 8080' > traffic.log

# Or use mitmproxy
mitmproxy --mode reverse:http://localhost:8080 -p 8081
```

### 10. Debug Configuration Issues

**Why**: Config errors can cause silent failures.

```bash
# Test config loading
cargo run --bin louter -- \
  --config test-config.toml \
  --verbose 2>&1 | grep -i "config\|error\|warning"

# Verify backend connectivity
curl -v http://localhost:11211/health
curl -v http://localhost:11212/health
```

### 11. Compare Request/Response Pairs

**Why**: Understand what transformations the proxy is doing.

```bash
# Show request/response pairs
cargo run --bin log-parser -- \
  --file proxy.jsonl \
  pairs | less

# Look for specific endpoint
cargo run --bin log-parser -- \
  --file proxy.jsonl \
  pairs | jq 'select(.request.endpoint | contains("generateContent"))'
```

### 12. Debug XML Tool Call Parsing (Qwen)

**Why**: Qwen uses custom XML format that needs parsing.

```bash
# Check if tool_format is set correctly
grep "tool_format" qwen-config.toml

# Look for XML parsing errors in logs
cargo run --bin log-parser -- \
  --file qwen-proxy.jsonl \
  all | grep -i "xml\|tool_call\|<function"

# Test XML extraction manually
echo '<tool_call><function=test><parameter=x>5</parameter></function></tool_call>' \
  | cargo run --bin test-xml-parser  # (if you create this tool)
```

**Expected XML Format**:
```xml
<tool_call>
  <function=WriteFile>
    <parameter=file_path>test.txt</parameter>
    <parameter=content>Hello</parameter>
  </function>
</tool_call>
```

### 13. Performance Profiling

**Why**: Find bottlenecks and slow requests.

```bash
# Enable Prometheus metrics
curl http://localhost:8080/metrics

# Key metrics:
# - http_request_duration_seconds_bucket
# - backend_request_duration_seconds_bucket
# - tokens_total
# - errors_total

# Analyze request latency
cargo run --bin log-parser -- \
  --file proxy.jsonl \
  stats | jq '.latency'
```

### 14. Health Check Debugging

**Why**: Verify proxy and backends are responsive.

```bash
# Check proxy health
curl http://localhost:8080/health

# Check backend health
curl http://localhost:11211/health
curl http://localhost:11212/health

# Continuous health monitoring
watch -n 5 'curl -s http://localhost:8080/health | jq .'
```

### 15. Reproduce Issues with Saved Logs

**Why**: Replay exact requests that caused errors.

```bash
# Extract failing request
cargo run --bin log-parser -- \
  --file proxy.jsonl \
  filter --direction client_request \
  | jq 'select(.payload.contents[0].parts[0].text | contains("problematic query"))' \
  > failing-request.json

# Replay the request
curl -X POST 'http://localhost:8080/v1beta/models/gemini-2.0-flash:generateContent' \
  -H "Content-Type: application/json" \
  -d @failing-request.json
```

---

## Advanced Use Cases

### Load Testing

```bash
# Use Apache Bench
ab -n 1000 -c 10 -p request.json \
  -T application/json \
  http://localhost:8080/v1/chat/completions

# Use wrk
wrk -t4 -c100 -d30s \
  -s post.lua \
  http://localhost:8080/v1/chat/completions
```

### Multi-Tenant Setup

```toml
# Different backends for different users
[backends.user1-openai]
url = "https://api.openai.com"
api_key = "user1-key"

[backends.user2-local]
url = "http://localhost:11211"
```

### Failover Configuration

```toml
# Primary backend
[backends.primary]
url = "http://localhost:11211"
weight = 2.0

# Fallback backend
[backends.fallback]
url = "http://localhost:11212"
weight = 1.0
```

### Cost Optimization

```bash
# Use local models for development
export PROXY_BACKEND=local

# Use cloud for production
export PROXY_BACKEND=gemini

# Monitor token usage
curl http://localhost:8080/metrics | grep tokens_total
```

---

## Troubleshooting Guide

### Issue: "Connection refused"

**Cause**: Backend not running or wrong port.

**Fix**:
```bash
# Check if llama-server is running
ps aux | grep llama-server
lsof -i :11211

# Restart backend
./llama-server -m model.gguf --port 11211
```

### Issue: "Empty function arguments {}"

**Cause**: Backend returned malformed tool call or streaming issue.

**Fix**:
```bash
# Check tool_format setting
grep tool_format config.toml

# For Qwen, ensure tool_format = "xml"
# Check backend logs for tool call output
```

### Issue: "Model not found"

**Cause**: Model mapping incorrect or model name mismatch.

**Fix**:
```toml
[backends.mybackend.model_mapping]
"client-model-name" = "actual-backend-model-name"
```

### Issue: Slow responses

**Cause**: Model too large, insufficient GPU, or network latency.

**Fix**:
```bash
# Increase timeout
[performance]
timeout_seconds = 180

# Use smaller model
# Increase --n-gpu-layers in llama-server
# Check network latency: ping backend-host
```

### Issue: "Failed to parse SSE chunk"

**Cause**: Streaming format mismatch or network interruption.

**Fix**:
```bash
# Check streaming logs
cargo run --bin log-parser -- --file proxy.jsonl all | grep -i sse

# Test backend directly
curl http://localhost:11211/v1/chat/completions \
  -d '{"model":"test","messages":[...],"stream":true}'
```

---

## Next Steps

1. **Explore Advanced Features**: See [FEATURE-ROADMAP.md](FEATURE-ROADMAP.md)
2. **Read Development Guide**: See [CLAUDE.md](CLAUDE.md)
3. **Check Examples**: See test scripts in repository
4. **Join Community**: Report issues and share use cases on GitHub

---

*Last Updated: 2024-12-19*
