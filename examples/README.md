# Example Configurations

This directory contains example configuration files for various use cases.

## Configuration Files

### `dual-backend-config.toml`
Multi-backend configuration supporting both GPT-OSS and Qwen3-Coder backends.

**Use case**: Running multiple local LLM backends simultaneously with load balancing

**How to use**:
```bash
# Start first backend (GPT-OSS)
./llama-server -m gpt-oss-20b.gguf --port 11211

# Start second backend (Qwen3-Coder)
./llama-server -m qwen3-coder.gguf --port 11212

# Start proxy with dual backend
cargo run --bin louter -- \
  --backend openai \
  --port 8080 \
  --config examples/dual-backend-config.toml \
  --verbose
```

**Features**:
- JSON tool format for GPT-OSS
- XML tool format for Qwen3-Coder
- Model mapping for both backends
- Automatic format conversion

---

### `llama-cpp-config.toml`
Configuration for llama.cpp's llama-server backend.

**Use case**: Running local LLMs through llama.cpp

**How to use**:
```bash
# Start llama-server
./llama-server \
  -m model.gguf \
  --port 11211 \
  --ctx-size 4096 \
  --n-gpu-layers 30

# Start proxy
cargo run --bin louter -- \
  --backend openai \
  --port 8080 \
  --config examples/llama-cpp-config.toml \
  --verbose
```

**Features**:
- Optimized for llama.cpp backend
- Configurable context size and token limits
- Tool format configuration

---

### `openai-public-config.toml`
Configuration for using public OpenAI API.

**Use case**: Accessing OpenAI's cloud API through the proxy

**How to use**:
```bash
# Set API key
export OPENAI_API_KEY="your-api-key"

# Start proxy
cargo run --bin louter -- \
  --backend openai \
  --port 8080 \
  --config examples/openai-public-config.toml \
  --verbose
```

**Features**:
- Public OpenAI API endpoint
- Token counting configuration
- Model mapping for OpenAI models

---

## Creating Your Own Config

Copy an example and modify it:

```bash
cp examples/llama-cpp-config.toml my-config.toml
```

Edit `my-config.toml` to customize:

```toml
[performance]
enable_metrics = true
log_requests = true
timeout_seconds = 60

[backends.my-backend]
url = "http://localhost:11211"
tool_format = "json"  # or "xml" for Qwen
max_tokens = 2048

[backends.my-backend.model_mapping]
"my-model" = "actual-backend-model-name"
```

## Configuration Reference

For complete configuration documentation, see the main [README.md](../README.md).

Key configuration options:

- `url`: Backend API endpoint
- `tool_format`: "json" or "xml" (for Qwen3-Coder)
- `max_tokens`: Maximum tokens per request
- `model_mapping`: Map client model names to backend model names
- `api_key`: API key (or use environment variable)

## Tips

1. **Local Development**: Use `llama-cpp-config.toml` as a template
2. **Production**: Use `openai-public-config.toml` with proper API keys
3. **Multi-Backend**: Use `dual-backend-config.toml` for load balancing
4. **Debugging**: Enable `log_requests = true` and use `--log-file` flag

## See Also

- [TUTORIALS.md](../docs/TUTORIALS.md) - Detailed setup tutorials
- [FEATURE-ROADMAP.md](../docs/FEATURE-ROADMAP.md) - Upcoming configuration features
