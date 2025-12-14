# Louter Configuration Examples

This directory contains example YAML configuration files for various use cases.

## Quick Start

**Simplest setup** (local llama-server, no authentication):
```bash
stack run louter-server -- --config examples/local-llama-server.yaml --port 9000
```

## Configuration Files

### 1. **local-llama-server.yaml** - Local Development
**Use case:** Development and testing with local llama-server (no API costs)

**Features:**
- No authentication required
- Works with llama.cpp's llama-server
- Maps popular model names to your local model
- Supports all features (streaming, tools, vision)

**Setup:**
```bash
# Start llama-server
llama-server --model path/to/model.gguf --port 11211

# Start Louter
stack run louter-server -- --config examples/local-llama-server.yaml --port 9000
```

**Test:**
```bash
curl http://localhost:9000/v1/chat/completions \
  -H "Content-Type: application/json" \
  -d '{"model": "gpt-4", "messages": [{"role": "user", "content": "Hello!"}]}'
```

---

### 2. **cloud-apis.yaml** - Production Cloud APIs
**Use case:** Production deployment with OpenAI, Anthropic, and Gemini APIs

**Features:**
- Multiple cloud providers in one config
- Automatic protocol conversion
- Environment variable support for API keys
- Model-based routing

**Setup:**
```bash
# Set API keys
export OPENAI_API_KEY="sk-..."
export ANTHROPIC_API_KEY="sk-ant-..."
export GEMINI_API_KEY="..."

# Start Louter
stack run louter-server -- --config examples/cloud-apis.yaml --port 9000
```

**Example:** Use OpenAI SDK with Claude
```python
from openai import OpenAI

client = OpenAI(base_url="http://localhost:9000/v1", api_key="not-needed")
response = client.chat.completions.create(
    model="claude-3-5-sonnet-20241022",  # Routed to Anthropic
    messages=[{"role": "user", "content": "Hello!"}]
)
```

---

### 3. **multi-backend.yaml** - Hybrid Local + Cloud
**Use case:** Mix local models (fast, free) with cloud APIs (high quality)

**Features:**
- Multiple backends with different purposes
- Local models for development
- Cloud APIs for production
- Support for both JSON and XML tool formats

**Setup:**
```bash
# Start local servers
llama-server --model path/to/qwen-7b.gguf --port 11211
llama-server --model path/to/qwen-coder.gguf --port 11212

# Set cloud API keys
export OPENAI_API_KEY="sk-..."
export ANTHROPIC_API_KEY="sk-ant-..."

# Start Louter
stack run louter-server -- --config examples/multi-backend.yaml --port 9000
```

**Routing:**
- `model="gpt-3.5-turbo"` → Local llama-server (fast, free)
- `model="gpt-4"` → OpenAI cloud (high quality)
- `model="claude-3-5-sonnet"` → Anthropic cloud (Claude-specific features)
- `model="qwen3-coder"` → Local Qwen with XML tools

---

### 4. **vision-models.yaml** - Multimodal Image Support
**Use case:** Image analysis and vision tasks

**Features:**
- Local vision models (Qwen2.5-VL)
- Cloud vision APIs (GPT-4 Vision, Claude 3, Gemini Pro Vision)
- Support for base64 images and URLs
- Proper image format handling

**Setup:**
```bash
# For local vision model
llama-server --model path/to/qwen2.5-vl-7b.gguf --port 11211 --mlock

# For cloud APIs
export OPENAI_API_KEY="sk-..."
export ANTHROPIC_API_KEY="sk-ant-..."
export GEMINI_API_KEY="..."

# Start Louter
stack run louter-server -- --config examples/vision-models.yaml --port 9000
```

**Example:** Analyze image
```python
from openai import OpenAI
import base64

client = OpenAI(base_url="http://localhost:9000/v1", api_key="not-needed")

with open("image.jpg", "rb") as f:
    image_data = base64.b64encode(f.read()).decode()

response = client.chat.completions.create(
    model="gpt-4-vision",
    messages=[{
        "role": "user",
        "content": [
            {"type": "text", "text": "Describe this image"},
            {"type": "image_url", "image_url": {
                "url": f"data:image/jpeg;base64,{image_data}"
            }}
        ]
    }]
)
```

---

### 5. **xml-tools.yaml** - XML Function Calling (Qwen)
**Use case:** Function calling with Qwen models that use XML format

**Features:**
- Automatic XML ↔ JSON conversion
- Support for Qwen's XML tool format
- Streaming with proper XML buffering
- Compatible with standard OpenAI tool definitions

**Setup:**
```bash
# Start Qwen model
llama-server --model path/to/qwen2.5-7b-instruct.gguf --port 11211

# Start Louter
stack run louter-server -- --config examples/xml-tools.yaml --port 9000
```

**How it works:**
1. Client sends standard JSON tool definitions
2. Louter converts to XML format for Qwen
3. Qwen responds with XML tool calls: `<tool_call>...</tool_call>`
4. Louter buffers and parses XML during streaming
5. Client receives standard JSON tool calls

**Example:**
```python
from openai import OpenAI

client = OpenAI(base_url="http://localhost:9000/v1", api_key="not-needed")

tools = [{
    "type": "function",
    "function": {
        "name": "get_weather",
        "description": "Get current weather",
        "parameters": {
            "type": "object",
            "properties": {"location": {"type": "string"}},
            "required": ["location"]
        }
    }
}]

response = client.chat.completions.create(
    model="qwen",
    messages=[{"role": "user", "content": "Weather in Tokyo?"}],
    tools=tools
)
```

---

### 6. **claude-code-with-gemini.yaml** - Claude Code + Gemini
**Use case:** Use Claude Code interface with Gemini backend

**Features:**
- Use Claude Code's developer experience
- Pay Gemini API rates (cheaper than Claude)
- Access Gemini's large context and multimodal features
- Automatic Anthropic ↔ Gemini protocol conversion

**Setup:**
```bash
# Get Gemini API key
export GEMINI_API_KEY="your-api-key"

# Start Louter on Anthropic-compatible port
stack run louter-server -- --config examples/claude-code-with-gemini.yaml --port 8000

# Configure Claude Code:
# - API Endpoint: http://localhost:8000
# - Model: claude-3-5-sonnet-20241022
```

**What gets converted:**
- Claude Code sends Anthropic API requests
- Louter converts to Gemini API format
- Gemini processes the request
- Louter converts back to Anthropic format
- Claude Code receives standard response

---

## Configuration Reference

### Backend Types

```yaml
backends:
  backend-name:
    type: openai | anthropic | gemini
    url: http://... or https://...
    requires_auth: true | false
    api_key: "..." # Optional if using env var

    model_mapping:
      frontend-model: backend-model

    # Optional settings
    max_tokens: 4096
    temperature: 0.7
    tool_format: json | xml  # Default: json
```

### Environment Variables

Instead of hardcoding API keys in config files, use environment variables:

```yaml
# In config file - reference env var
api_key: "${OPENAI_API_KEY}"

# Or omit api_key field and Louter will check:
# - OPENAI_API_KEY (for type: openai)
# - ANTHROPIC_API_KEY (for type: anthropic)
# - GEMINI_API_KEY (for type: gemini)
```

### Model Mapping

Model mappings determine routing:

```yaml
model_mapping:
  # Client requests this -> Backend receives this
  gpt-4: qwen/qwen2.5-vl-7b
  claude-3-5-sonnet: gemini-2.0-flash-exp
```

**Client request:**
```json
{"model": "gpt-4", "messages": [...]}
```

**Sent to backend:**
```json
{"model": "qwen/qwen2.5-vl-7b", "messages": [...]}
```

---

## Testing Configurations

### Health Check
```bash
curl http://localhost:9000/health
```

**Expected response:**
```json
{"service": "louter", "status": "ok"}
```

### Basic Chat
```bash
curl http://localhost:9000/v1/chat/completions \
  -H "Content-Type: application/json" \
  -d '{
    "model": "gpt-4",
    "messages": [{"role": "user", "content": "Hello!"}]
  }'
```

### Streaming
```bash
curl -N http://localhost:9000/v1/chat/completions \
  -H "Content-Type: application/json" \
  -d '{
    "model": "gpt-4",
    "messages": [{"role": "user", "content": "Count to 5"}],
    "stream": true
  }'
```

### Function Calling
```bash
curl http://localhost:9000/v1/chat/completions \
  -H "Content-Type: application/json" \
  -d '{
    "model": "gpt-4",
    "messages": [{"role": "user", "content": "What is the weather in Tokyo?"}],
    "tools": [{
      "type": "function",
      "function": {
        "name": "get_weather",
        "description": "Get current weather",
        "parameters": {
          "type": "object",
          "properties": {
            "location": {"type": "string", "description": "City name"}
          },
          "required": ["location"]
        }
      }
    }]
  }'
```

---

## Troubleshooting

### Connection Refused
**Issue:** `curl: (7) Failed to connect to localhost port 9000`

**Solution:** Make sure Louter is running:
```bash
stack run louter-server -- --config examples/local-llama-server.yaml --port 9000
```

### Backend Connection Failed
**Issue:** Louter starts but requests fail with backend connection errors

**Solution:** Check backend is running:
```bash
# For local llama-server
curl http://localhost:11211/v1/models

# For cloud APIs
curl https://api.openai.com/v1/models -H "Authorization: Bearer $OPENAI_API_KEY"
```

### Invalid API Key
**Issue:** `401 Unauthorized` or `Invalid API key`

**Solution:** Check environment variables:
```bash
echo $OPENAI_API_KEY
echo $ANTHROPIC_API_KEY
echo $GEMINI_API_KEY
```

### Model Not Found
**Issue:** `Model 'xyz' not found`

**Solution:** Check model_mapping in config file. Make sure the frontend model name is mapped to a valid backend model.

### Debug Mode
Run with verbose logging:
```bash
stack run louter-server -- --config config.yaml --port 9000 --verbose
```

---

## Next Steps

- **Library Usage:** See [docs/haskell/LIBRARY_API.md](../docs/haskell/LIBRARY_API.md)
- **Getting Started:** See [docs/haskell/GETTING_STARTED.md](../docs/haskell/GETTING_STARTED.md)
- **Installation:** See [INSTALLATION.md](../INSTALLATION.md)
- **Contributing:** See [CONTRIBUTING.md](../CONTRIBUTING.md)

---

## Legacy TOML Configs

The `*.toml` files in this directory are legacy configurations from the previous Rust implementation. They are kept for reference but **YAML is now the recommended format**.

To convert TOML to YAML:
1. Copy the structure
2. Replace `[section.subsection]` with `section:\n  subsection:`
3. Replace `=` with `:`
4. Use proper YAML indentation (2 spaces)

YAML is simpler and more widely supported than TOML in the Haskell ecosystem.
