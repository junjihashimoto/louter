# Getting Started with Louter

This guide will help you get up and running with Louter quickly.

## Prerequisites

Make sure you have completed the [Installation](../../INSTALLATION.md) first.

## Understanding Louter

Louter serves two purposes:

1. **Client Library** - Connect your Haskell app to any LLM API
2. **Proxy Server** - Bridge between different LLM API protocols

Choose the path that fits your needs:
- **Want to call LLMs from Haskell?** → Use as a Library
- **Want to translate between API formats?** → Use as a Proxy

---

## Path 1: Using as a Library

### Basic Setup

Create a new Haskell project:

```bash
stack new my-llm-app
cd my-llm-app
```

Add louter to `package.yaml`:

```yaml
dependencies:
  - base >= 4.7 && < 5
  - louter
  - text
```

### Your First LLM Call

```haskell
-- app/Main.hs
{-# LANGUAGE OverloadedStrings #-}

import Louter.Client
import Louter.Client.OpenAI (llamaServerClient)
import Louter.Types.Request

main :: IO ()
main = do
  -- Connect to local llama-server
  client <- llamaServerClient "http://localhost:11211"

  -- Create a simple request
  let request = defaultChatRequest "gpt-oss"
        [ Message RoleUser "What is the capital of France?" ]

  -- Get response
  result <- chatCompletion client request
  case result of
    Left err -> putStrLn $ "Error: " <> show err
    Right response -> do
      let answer = choiceMessage (head $ respChoices response)
      putStrLn $ "Answer: " <> show answer
```

Run it:

```bash
# Start llama-server first (in another terminal)
# llama-server --model path/to/model.gguf --port 11211

# Run your app
stack run
```

### Streaming Responses

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Louter.Client
import Louter.Client.OpenAI (llamaServerClient)
import Louter.Types.Request
import Louter.Types.Streaming
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  client <- llamaServerClient "http://localhost:11211"

  let request = (defaultChatRequest "gpt-oss"
        [Message RoleUser "Write a haiku about Haskell"])
        { reqStream = True }

  putStrLn "Assistant: "
  streamChatWithCallback client request handleEvent

handleEvent :: StreamEvent -> IO ()
handleEvent (StreamContent txt) = do
  putStr txt
  hFlush stdout
handleEvent (StreamFinish reason) =
  putStrLn $ "\n[Finished: " <> reason <> "]"
handleEvent (StreamError err) =
  putStrLn $ "\n[Error: " <> err <> "]"
handleEvent _ = pure ()
```

### Function Calling (Tools)

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Louter.Client
import Louter.Client.OpenAI (llamaServerClient)
import Louter.Types.Request
import Data.Aeson (object, (.=))

main :: IO ()
main = do
  client <- llamaServerClient "http://localhost:11211"

  -- Define a tool
  let weatherTool = Tool
        { toolName = "get_weather"
        , toolDescription = Just "Get current weather for a location"
        , toolParameters = object
            [ "type" .= ("object" :: Text)
            , "properties" .= object
                [ "location" .= object
                    [ "type" .= ("string" :: Text)
                    , "description" .= ("City name" :: Text)
                    ]
                ]
            , "required" .= (["location"] :: [Text])
            ]
        }

  let request = (defaultChatRequest "gpt-oss"
        [Message RoleUser "What's the weather in Tokyo?"])
        { reqTools = [weatherTool]
        , reqToolChoice = ToolChoiceAuto
        }

  result <- chatCompletion client request
  case result of
    Left err -> putStrLn $ "Error: " <> show err
    Right response ->
      print response
```

---

## Path 2: Using as a Proxy Server

### Quick Start

1. **Create configuration file** (`config.yaml`):

```yaml
backends:
  llama-server:
    type: openai
    url: http://localhost:11211
    requires_auth: false
    model_mapping:
      gpt-4: qwen/qwen2.5-vl-7b
      gpt-3.5-turbo: qwen/qwen2.5-7b
```

2. **Start the proxy**:

```bash
cd haskell/louter
stack run louter-server -- --config config.yaml --port 9000
```

3. **Test it**:

```bash
# Send OpenAI-format request
curl http://localhost:9000/v1/chat/completions \
  -H "Content-Type: application/json" \
  -d '{
    "model": "gpt-4",
    "messages": [{"role": "user", "content": "Hello!"}]
  }'
```

### Use Cases

#### 1. Use OpenAI SDK with Local Models

```python
# Python script
from openai import OpenAI

# Point to your proxy
client = OpenAI(
    base_url="http://localhost:9000/v1",
    api_key="not-needed"  # Auth disabled for local models
)

response = client.chat.completions.create(
    model="gpt-4",  # Gets routed to qwen/qwen2.5-vl-7b
    messages=[{"role": "user", "content": "Hello!"}]
)
print(response.choices[0].message.content)
```

#### 2. Use Claude Code with Gemini

```yaml
# config.yaml - Anthropic frontend, Gemini backend
backends:
  gemini:
    type: gemini
    url: https://generativelanguage.googleapis.com
    requires_auth: true
    api_key: "YOUR_GEMINI_API_KEY"
    model_mapping:
      claude-3-5-sonnet-20241022: gemini-2.0-flash
```

Start proxy and configure Claude Code:

```bash
# Start on Anthropic-compatible port
stack run louter-server -- --config config.yaml --port 8000

# In Claude Code settings, set:
# API Endpoint: http://localhost:8000
```

#### 3. Multi-Backend Setup

```yaml
backends:
  local-llama:
    type: openai
    url: http://localhost:11211
    requires_auth: false
    model_mapping:
      gpt-4: qwen/qwen2.5-vl-7b

  openai-cloud:
    type: openai
    url: https://api.openai.com
    requires_auth: true
    api_key: "sk-..."
    model_mapping:
      gpt-4-turbo: gpt-4-turbo-preview

  anthropic-cloud:
    type: anthropic
    url: https://api.anthropic.com
    requires_auth: true
    api_key: "sk-ant-..."
    model_mapping:
      claude-3-opus: claude-3-opus-20240229
```

---

## Common Configuration Patterns

### Local Development (No Auth)

```yaml
backends:
  local:
    type: openai
    url: http://localhost:11211
    requires_auth: false
    tool_format: json  # or xml for Qwen
```

### Production Cloud API

```yaml
backends:
  openai:
    type: openai
    url: https://api.openai.com
    requires_auth: true
    api_key: "${OPENAI_API_KEY}"  # From environment variable
```

### Vision Support

```yaml
backends:
  qwen-vision:
    type: openai
    url: http://localhost:11211
    requires_auth: false
    model_mapping:
      gpt-4-vision: qwen/qwen2.5-vl-7b  # Vision model
```

---

## Monitoring and Debugging

### JSON-Line Logging

The proxy outputs structured JSON logs:

```bash
# Run proxy with logging
stack run louter-server -- --config config.yaml --port 9000 2>&1 | tee proxy.log

# In another terminal, watch logs
tail -f proxy.log | jq .
```

Log format:

```json
{
  "trace_id": "trace-abc123",
  "event": "request_received",
  "details": {...}
}
```

### Health Checks

```bash
curl http://localhost:9000/health
```

Response:

```json
{
  "service": "louter",
  "status": "ok"
}
```

---

## Next Steps

- **Library Users**: Read the [Library API Guide](LIBRARY_API.md)
- **Proxy Users**: Read the [Proxy Setup Guide](PROXY_SETUP.md)
- **Developers**: Check out [CONTRIBUTING.md](../../CONTRIBUTING.md)

## Troubleshooting

### "Connection refused"

Make sure your backend is running:

```bash
# For llama-server
llama-server --model path/to/model.gguf --port 11211

# Check if it's running
curl http://localhost:11211/v1/models
```

### "Invalid API key"

For cloud APIs, verify your API key:

```bash
# Test OpenAI key directly
curl https://api.openai.com/v1/models \
  -H "Authorization: Bearer YOUR_API_KEY"
```

### "Model not found"

Check your model mapping:

```yaml
model_mapping:
  frontend-model-name: backend-model-name
```

The frontend model name is what clients request. The backend model is what gets sent to the backend API.

## Examples

See the [examples/](../../examples/) directory for complete working examples:

- `library-simple.hs` - Basic library usage
- `library-streaming.hs` - Streaming responses
- `library-tools.hs` - Function calling
- `proxy-config.yaml` - Proxy configuration examples

## Support

- 📖 [Full Documentation](../)
- 🐛 [Report Issues](https://github.com/yourusername/louter/issues)
- 💬 [Ask Questions](https://github.com/yourusername/louter/discussions)
