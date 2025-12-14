# Louter - Multi-Protocol LLM Proxy & Client Library

**Louter** is a Haskell library and proxy server that enables seamless communication between different LLM APIs (OpenAI, Anthropic, Google Gemini) with automatic protocol translation.

## Key Features

### 🔄 **Protocol Translation**
- Automatic conversion between OpenAI, Anthropic, and Gemini API formats
- Bidirectional SSE streaming support with proper buffering
- Function calling (tools) translation across all protocols
- Vision support (multimodal images) across all APIs

### 📚 **Dual Usage**
1. **As a Library** - Import into your Haskell application to connect to any LLM API
2. **As a Proxy Server** - Run standalone to expose one API format while connecting to another

### 🎯 **Production Ready**
- Stateless protocol conversion
- Proper error handling and logging
- JSON-line structured logging
- Configurable authentication (optional for local models)
- YAML-based configuration

---

## Quick Start

### Installation

**Prerequisites**: GHC 9.6+ and either Stack or Cabal

```bash
git clone https://github.com/yourusername/louter.git
cd louter
cd haskell/louter

# Using Stack
stack build

# Or using Cabal
cabal build
```

### Usage

#### 1. As a Proxy Server

Create a `config.yaml`:

```yaml
backends:
  llama-server:
    type: openai
    url: http://localhost:11211
    requires_auth: false
    model_mapping:
      gpt-4: qwen/qwen2.5-vl-7b
```

Run the proxy:

```bash
# Using Stack
stack run louter-server -- --config config.yaml --port 9000

# Or using Cabal
cabal run louter-server -- --config config.yaml --port 9000
```

Now you can send OpenAI, Anthropic, or Gemini requests to `localhost:9000`.

#### 2. As a Library

```haskell
import Louter.Client
import Louter.Client.OpenAI (llamaServerClient)

main :: IO ()
main = do
  -- Connect to a local llama-server (no authentication)
  client <- llamaServerClient "http://localhost:11211"

  -- Non-streaming request
  response <- chatCompletion client $ defaultChatRequest "gpt-oss"
    [Message RoleUser "Hello!"]
  print response

  -- Streaming with callback
  streamChatWithCallback client request $ \event -> case event of
    StreamContent txt -> putStr txt >> hFlush stdout
    StreamToolCall call -> print call
    StreamFinish reason -> putStrLn $ "\n[Done: " <> reason <> "]"
    StreamError err -> putStrLn $ "[Error: " <> err <> "]"
```

---

## Supported APIs

| Frontend API | Backend API | Status |
|-------------|-------------|--------|
| OpenAI → OpenAI | Direct pass-through | ✅ |
| OpenAI → Anthropic | Full conversion | ✅ |
| OpenAI → Gemini | Full conversion | ✅ |
| Anthropic → OpenAI | Full conversion | ✅ |
| Gemini → OpenAI | Full conversion | ✅ |

### Features Support Matrix

| Feature | OpenAI | Anthropic | Gemini |
|---------|--------|-----------|--------|
| Text | ✅ | ✅ | ✅ |
| Streaming | ✅ | ✅ | ✅ (SSE & JSON) |
| Function Calling | ✅ | ✅ | ✅ |
| Vision (Images) | ✅ | ✅ | ✅ |
| XML Tools (Qwen) | ✅ | ⚠️ | ⚠️ |

---

## Configuration

### Backend Types

```yaml
backends:
  backend-name:
    type: openai | anthropic | gemini
    url: http://...
    requires_auth: true | false
    api_key: "sk-..." # Optional, required if requires_auth=true
    model_mapping:
      frontend-model: backend-model
    tool_format: json | xml  # Optional, defaults to json
```

### Authentication

**Local/OSS Models** (no authentication):
```yaml
backends:
  llama:
    type: openai
    url: http://localhost:11211
    requires_auth: false
```

**Cloud APIs** (with authentication):
```yaml
backends:
  openai:
    type: openai
    url: https://api.openai.com
    requires_auth: true
    api_key: "sk-..."
```

---

## Architecture

Louter uses a **Core IR** (Internal Representation) inspired by OpenAI's Chat Completions format:

```
Frontend Request → Core IR → Backend-Specific Format
                      ↓
Backend Response ← Core IR ← Backend-Specific Format
```

### Key Components

1. **Protocol Converters** - Bidirectional conversion between API formats
2. **SSE Parser** - Incremental streaming event parser (attoparsec-based)
3. **Tool Call Buffering** - Stateful buffering for complete JSON tool calls
4. **Vision Support** - Data URL parsing and image format conversion

See [CLAUDE.md](CLAUDE.md) for detailed architecture documentation.

---

## Testing

### Run Test Suite

```bash
# Python SDK integration tests
cd tests
python3 -m pytest test_openai_streaming.py
python3 -m pytest test_anthropic_streaming.py
python3 -m pytest test_gemini_tool_calling.py

# Haskell unit tests
cd haskell/louter
stack test
```

### Test Coverage

- ✅ 38 Python SDK integration tests
- ✅ OpenAI streaming and function calling
- ✅ Anthropic streaming and vision
- ✅ Gemini streaming (SSE & JSON array formats)
- ✅ Tool calling with XML format (Qwen)

---

## Examples

See the [examples/](examples/) directory for:
- Proxy configuration examples
- Library usage examples
- Authentication setups
- Multi-backend configurations

---

## Documentation

- [INSTALLATION.md](INSTALLATION.md) - Detailed installation guide
- [CONTRIBUTING.md](CONTRIBUTING.md) - Contribution guidelines
- [CLAUDE.md](CLAUDE.md) - Architecture and coding standards
- [docs/haskell/](docs/haskell/) - Haskell-specific guides
  - [GETTING_STARTED.md](docs/haskell/GETTING_STARTED.md)
  - [LIBRARY_API.md](docs/haskell/LIBRARY_API.md)
  - [PROXY_SETUP.md](docs/haskell/PROXY_SETUP.md)

---

## Deployment

### Docker

```bash
docker build -t louter .
docker run -p 9000:9000 -v $(pwd)/config.yaml:/app/config.yaml louter
```

### Docker Compose

```bash
docker-compose up
```

See [deployment/](deployment/) for systemd service files and production setup guides.

---

## Roadmap

- [x] OpenAI, Anthropic, Gemini protocol support
- [x] Streaming (SSE) support
- [x] Function calling / tool use
- [x] Vision (multimodal images)
- [x] XML tool format (Qwen)
- [x] Configurable authentication
- [ ] Audio streaming support
- [ ] Parallel tool execution
- [ ] Model capability auto-detection
- [ ] Backend health checks and failover
- [ ] Rate limiting and quotas
- [ ] Metrics and monitoring (Prometheus)

---

## Contributing

We welcome contributions! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

### Development

```bash
cd haskell/louter

# Build
stack build

# Run tests
stack test

# Run server in development mode
stack run louter-server -- --config test-config.yaml --port 9000
```

---

## License

MIT License - see [LICENSE](LICENSE) file for details.

---

## Acknowledgments

- Inspired by OpenAI's Chat Completions API design
- Built with Haskell, Warp, Aeson, and Attoparsec
- Tested with official Python SDKs (OpenAI, Anthropic, Google Gemini)

---

## Support

- 📖 [Documentation](docs/)
- 🐛 [Issue Tracker](https://github.com/yourusername/louter/issues)
- 💬 [Discussions](https://github.com/yourusername/louter/discussions)
