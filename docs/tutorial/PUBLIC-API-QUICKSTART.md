# Public API Quick Start Guide

This guide shows how to use the LLM router proxy with real public APIs (Google Gemini and OpenAI).

## Setup

### 1. Get API Keys

**Google Gemini API:**
- Visit: https://makersuite.google.com/app/apikey
- Create a new API key
- Copy the key

**OpenAI API:**
- Visit: https://platform.openai.com/api-keys
- Create a new API key
- Copy the key

### 2. Set Environment Variables

```bash
export GEMINI_API_KEY="your-gemini-api-key-here"
export OPENAI_API_KEY="your-openai-api-key-here"
```

**Tip:** Add these to your `~/.bashrc` or `~/.zshrc` for persistence.

### 3. Build the Proxy

```bash
cargo build --release
```

### 4. Start the Proxy

```bash
./target/release/louter \
  --backend openai \
  --port 8080 \
  --config config-public-api.toml \
  --log-file public-api.jsonl \
  --verbose
```

The server will start on `http://localhost:8080`

## Usage Examples

### Test 1: Gemini API Format → Gemini Backend

```bash
curl -X POST "http://localhost:8080/v1beta/models/gemini-pro:generateContent?key=$GEMINI_API_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "contents": [{
      "parts": [{"text": "Explain quantum computing in one sentence"}],
      "role": "user"
    }]
  }'
```

### Test 2: OpenAI API Format → OpenAI Backend

```bash
curl -X POST "http://localhost:8080/v1/chat/completions" \
  -H "Authorization: Bearer $OPENAI_API_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "model": "gpt-3.5-turbo",
    "messages": [{"role": "user", "content": "Explain quantum computing in one sentence"}]
  }'
```

### Test 3: Cross-Format (OpenAI Format → Gemini Backend)

Use OpenAI API format to access Gemini models:

```bash
curl -X POST "http://localhost:8080/v1/chat/completions" \
  -H "Authorization: Bearer $GEMINI_API_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "model": "gemini-pro",
    "messages": [{"role": "user", "content": "Hello!"}]
  }'
```

### Test 4: Cross-Format (Gemini Format → OpenAI Backend)

Use Gemini API format to access OpenAI models:

```bash
curl -X POST "http://localhost:8080/v1beta/models/gpt-3.5-turbo:generateContent" \
  -H "Authorization: Bearer $OPENAI_API_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "contents": [{
      "parts": [{"text": "Hello!"}],
      "role": "user"
    }]
  }'
```

### Test 5: Vision (Gemini)

```bash
curl -X POST "http://localhost:8080/v1beta/models/gemini-pro:generateContent?key=$GEMINI_API_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "contents": [{
      "parts": [
        {"text": "What is in this image?"},
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

## Automated Testing

Run the test script to verify all configurations:

```bash
./test-public-api.sh
```

This will test:
- ✓ Gemini API → Gemini Backend
- ✓ OpenAI API → OpenAI Backend
- ✓ OpenAI API → Gemini Backend (cross-format)
- ✓ Gemini API → OpenAI Backend (cross-format)
- ✓ Vision capabilities (Gemini)

## View Diagnostics

Check which backends are working:

```bash
curl http://localhost:8080/api/diagnostics | jq .
```

This shows:
- Backend reachability
- Detected capabilities (text, vision, function_calling)
- Frontend API test results
- Per-model test results

## View Metrics

Real-time metrics and logs:

```bash
# Web UI
open http://localhost:8080/ui

# Prometheus metrics
curl http://localhost:8080/metrics

# Health check
curl http://localhost:8080/health
```

## Model Mapping

The configuration maps model names across backends:

**Gemini Backend:**
- `gemini-pro` → `gemini-2.0-flash-exp`
- `gemini-flash` → `gemini-2.0-flash-exp`
- `gpt-4` → `gemini-2.0-flash-exp` (allows OpenAI names)
- `gpt-3.5-turbo` → `gemini-2.0-flash-exp`

**OpenAI Backend:**
- `gpt-4` → `gpt-4-turbo-preview`
- `gpt-3.5-turbo` → `gpt-3.5-turbo`
- `gemini-pro` → `gpt-4-turbo-preview` (allows Gemini names)

## Routing

The proxy automatically routes requests based on:

1. **Model name** - Routes to the backend configured for that model
2. **Capabilities** - Selects backends that support required features (text, vision, etc.)
3. **Priority** - Uses higher priority backends first
4. **Fallback** - Falls back to alternate backends if primary fails

Example routing:
- Request with `model: "gemini-pro"` → Gemini backend (priority 1)
- Request with `model: "gpt-4"` → OpenAI backend (priority 2)
- Request with vision + `model: "gemini-pro"` → Gemini backend (supports vision)
- Request with vision + `model: "gpt-4"` → Gemini backend (OpenAI vision limited)

## Cost Optimization

Both public APIs have usage costs:

**Google Gemini:**
- Free tier: 60 requests/minute
- Paid tier: Pay-per-token pricing
- See: https://ai.google.dev/pricing

**OpenAI:**
- No free tier
- Pay-per-token pricing varies by model
- See: https://openai.com/pricing

**Tips:**
- Use `gemini-2.0-flash-exp` for cost-effective text
- Use `gpt-3.5-turbo` for cheaper OpenAI access
- Monitor usage via the web UI (`/ui`)
- Check token counts in logs (`public-api.jsonl`)

## Troubleshooting

### "Backend not configured" error

Make sure you're using correct model names. Check available models:
```bash
curl http://localhost:8080/api/diagnostics | jq '.backends | keys'
```

### "Unauthorized" or "Invalid API key"

Verify your API keys are set:
```bash
echo $GEMINI_API_KEY
echo $OPENAI_API_KEY
```

### Connection refused

Make sure the server is running:
```bash
curl http://localhost:8080/health
```

### Rate limits

Both APIs have rate limits:
- Gemini: 60 requests/minute (free tier)
- OpenAI: Varies by tier

Check rate limit headers in responses.

## Advanced Configuration

Edit `config-public-api.toml` to:
- Add custom instructions per backend
- Adjust timeout settings
- Configure token counting modes
- Set up fallback backends
- Customize model mappings

## Security Notes

**Never commit API keys to version control!**

Always use environment variables or secure secret management.

The proxy logs all requests to `public-api.jsonl` - ensure this file doesn't contain sensitive data before sharing.
