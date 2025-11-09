# Content-Based Model Routing

## Overview

Automatically route requests to the appropriate backend based on input data type:
- **Text-only input** → Text model (fast, cheap)
- **Input with images** → Vision model (capable, expensive)

## Use Case

**Problem**: You have two models:
- GPT-OSS (text-only, fast, cheap)
- Qwen3-Coder (vision-capable, slower, expensive)

**Solution**: Proxy automatically detects images in the request and routes accordingly.

## How It Works

```
Client Request
     ↓
┌────────────────┐
│ Detect Content │
│      Type      │
└────────┬───────┘
         ↓
    Has images? ─── Yes ──→ Vision Model (Qwen)
         │
         No
         ↓
    Text Model (GPT-OSS)
```

## Configuration

```toml
[routing]
strategy = "content-based"
default_backend = "text-model"  # Use text model by default

[backends.text-model]
url = "http://localhost:11211"
capabilities = ["text", "function_calling"]
tool_format = "json"

[backends.text-model.model_mapping]
"gpt-oss" = "gpt-oss:20b"

[backends.vision-model]
url = "http://localhost:11212"
capabilities = ["text", "vision", "function_calling"]
tool_format = "xml"

[backends.vision-model.model_mapping]
"qwen-vision" = "Qwen3-Coder-30B"
```

## Implementation

### Step 1: Add Capabilities to Backend Config

**File**: `src/config.rs`

```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BackendConfig {
    pub url: String,
    pub api_key: Option<String>,
    pub tool_format: String,

    // NEW: Capabilities
    #[serde(default)]
    pub capabilities: Vec<String>,  // ["text", "vision", "audio", "function_calling"]

    pub model_mapping: Option<HashMap<String, String>>,
    pub max_tokens: Option<u32>,
    // ... existing fields
}
```

### Step 2: Detect Content Type

**File**: `src/routing.rs` (new file)

```rust
use crate::models::gemini::GeminiRequest;
use crate::models::openai::OpenAIRequest;

pub fn has_vision_content(gemini_request: &GeminiRequest) -> bool {
    for content in &gemini_request.contents {
        for part in &content.parts {
            // Check for inline_data with image mime type
            if let Some(ref inline_data) = part.inline_data {
                if inline_data.mime_type.starts_with("image/") {
                    return true;
                }
            }
            // Could also check for file_data
        }
    }
    false
}

pub fn has_vision_content_openai(openai_request: &OpenAIRequest) -> bool {
    for message in &openai_request.messages {
        if let Some(ref content) = message.content {
            // OpenAI format can have array of content parts
            // Check if any part is an image_url type
            // This depends on your OpenAI model structure
            // For now, simple string check:
            if content.contains("image_url") || content.contains("data:image") {
                return true;
            }
        }
    }
    false
}
```

### Step 3: Select Backend Based on Content

**File**: `src/backends.rs` (modify)

```rust
use crate::routing::{has_vision_content, has_vision_content_openai};

impl BackendClient {
    pub fn select_backend_for_request(
        &self,
        request: &GeminiRequest,
        config: &Config,
    ) -> Result<String, ProxyError> {
        let needs_vision = has_vision_content(request);

        // Find backends that match requirements
        for (backend_name, backend_config) in &config.backends {
            if needs_vision {
                // Need vision capability
                if backend_config.capabilities.contains(&"vision".to_string()) {
                    return Ok(backend_name.clone());
                }
            } else {
                // Text-only, prefer text model (usually faster/cheaper)
                if !backend_config.capabilities.contains(&"vision".to_string()) {
                    return Ok(backend_name.clone());
                }
            }
        }

        // Fallback to first available backend
        config.backends.keys().next()
            .map(|k| k.clone())
            .ok_or(ProxyError::ConfigError("No backends configured".to_string()))
    }
}
```

### Step 4: Integrate into Request Handler

**File**: `src/main.rs` (modify)

```rust
async fn generate_content_impl(
    state: AppState,
    model: String,
    params: std::collections::HashMap<String, String>,
    headers: HeaderMap,
    request: louter::models::gemini::GeminiRequest,
) -> Result<Response, ProxyError> {
    let start_time = Instant::now();
    let endpoint = format!("/v1beta/models/{}:generateContent", model);

    // NEW: Select backend based on content
    let backend_name = state.backend_client
        .select_backend_for_request(&request, &state.config)?;

    info!("Selected backend: {} (vision: {})",
          backend_name,
          routing::has_vision_content(&request));

    let backend_config = state.config.backends.get(&backend_name)
        .ok_or(ProxyError::ConfigError(format!("Backend not found: {}", backend_name)))?;

    // Continue with selected backend...
    match backend_name.as_str() {
        // Route to appropriate backend
        // ... existing code
    }
}
```

## Example Usage

### Request 1: Text Only → GPT-OSS

```bash
curl -X POST 'http://localhost:8080/v1/chat/completions' \
  -H "Content-Type: application/json" \
  -d '{
    "model": "auto",
    "messages": [
      {"role": "user", "content": "Explain quantum computing"}
    ]
  }'
```

**Result**: Routes to `text-model` (GPT-OSS on port 11211)

### Request 2: With Image → Qwen

```bash
curl -X POST 'http://localhost:8080/v1/chat/completions' \
  -H "Content-Type: application/json" \
  -d '{
    "model": "auto",
    "messages": [
      {
        "role": "user",
        "content": "What is in this image?",
        "inline_data": {
          "mime_type": "image/jpeg",
          "data": "base64_encoded_image..."
        }
      }
    ]
  }'
```

**Result**: Routes to `vision-model` (Qwen on port 11212)

## Benefits

### Cost Savings
- Text requests: $0.002 per 1K tokens (GPT-OSS)
- Vision requests: $0.010 per 1K tokens (Qwen)
- **80% savings on text-only requests**

### Performance
- Text model: ~50 tokens/sec
- Vision model: ~20 tokens/sec
- **2.5x faster for text requests**

### Resource Usage
- Text model: 8GB VRAM
- Vision model: 24GB VRAM
- **3x better GPU utilization**

## Logging

Add logging to track routing decisions:

```rust
info!("🔀 Routing decision:");
info!("  Request ID: {}", request_id);
info!("  Has vision: {}", needs_vision);
info!("  Selected backend: {}", backend_name);
info!("  Backend URL: {}", backend_config.url);
info!("  Expected cost: ${:.4}", estimated_cost);
```

## Metrics

Track routing metrics:

```rust
// In src/metrics.rs
describe_counter!(
    "routing_decisions_total",
    "Total number of routing decisions by content type"
);

pub fn record_routing_decision(content_type: &str, backend: &str) {
    let labels = [
        ("content_type", content_type.to_string()),
        ("backend", backend.to_string()),
    ];
    counter!("routing_decisions_total", &labels).increment(1);
}
```

View metrics:
```bash
curl http://localhost:8080/metrics | grep routing_decisions_total

# routing_decisions_total{content_type="text",backend="gpt-oss"} 850
# routing_decisions_total{content_type="vision",backend="qwen"} 150
```

## Testing

### Test 1: Text Request Routes to Text Model

```bash
# Start both backends
./llama-server -m gpt-oss.gguf --port 11211 &
./llama-server -m qwen-vision.gguf --port 11212 &

# Start proxy
cargo run --bin louter -- \
  --backend auto \
  --config dual-backend-config.toml \
  --verbose

# Send text request
curl -X POST 'http://localhost:8080/v1/chat/completions' \
  -d '{"model":"auto","messages":[{"role":"user","content":"Hello"}]}'

# Check logs - should say "Selected backend: text-model"
```

### Test 2: Vision Request Routes to Vision Model

```bash
# Send vision request
curl -X POST 'http://localhost:8080/v1/chat/completions' \
  -d '{
    "model":"auto",
    "messages":[{
      "role":"user",
      "content":[
        {"type":"text","text":"What is this?"},
        {"type":"image_url","image_url":{"url":"data:image/jpeg;base64,..."}}
      ]
    }]
  }'

# Check logs - should say "Selected backend: vision-model"
```

## Edge Cases

### Case 1: No Vision Model Available
```rust
// If request has images but no vision backend configured
if needs_vision && !has_vision_backend {
    return Err(ProxyError::NoVisionBackendAvailable);
}
```

### Case 2: Vision Model Can Handle Text
```rust
// Vision models can also handle text-only
// But prefer text model for cost/performance
// Only use vision model if text model unavailable
```

### Case 3: Multiple Vision Models
```rust
// If multiple vision models available, choose by priority
let vision_backends: Vec<_> = backends.iter()
    .filter(|b| b.capabilities.contains("vision"))
    .sorted_by_key(|b| b.priority)
    .collect();
```

## Configuration Examples

### Minimal Config (2 backends)

```toml
[backends.text]
url = "http://localhost:11211"
capabilities = ["text", "function_calling"]

[backends.vision]
url = "http://localhost:11212"
capabilities = ["text", "vision", "function_calling"]
```

### Full Config (with routing preferences)

```toml
[routing]
strategy = "content-based"
prefer_text_model = true  # Use text model when possible
log_routing_decisions = true

[backends.gpt-oss]
url = "http://localhost:11211"
capabilities = ["text", "function_calling"]
tool_format = "json"
priority = 1
cost_per_1k_tokens = 0.002

[backends.qwen]
url = "http://localhost:11212"
capabilities = ["text", "vision", "function_calling"]
tool_format = "xml"
priority = 2
cost_per_1k_tokens = 0.010
```

## Implementation Checklist

- [ ] Add `capabilities` field to `BackendConfig`
- [ ] Create `src/routing.rs` with content detection
- [ ] Add `has_vision_content()` for Gemini format
- [ ] Add `has_vision_content()` for OpenAI format
- [ ] Implement `select_backend_for_request()`
- [ ] Update request handlers to use routing
- [ ] Add routing metrics
- [ ] Add routing logs
- [ ] Create example config with 2 backends
- [ ] Add tests for routing logic
- [ ] Update README.md with routing documentation
- [ ] Add to FEATURE-ROADMAP.md

## Estimated Time

- **Backend selection logic**: 2-3 hours
- **Content detection**: 2-3 hours
- **Integration**: 2-3 hours
- **Testing**: 2-3 hours
- **Documentation**: 1-2 hours
- **Total**: 1-2 days

## Next Steps

1. Implement basic content detection
2. Add capability configuration
3. Implement backend selection
4. Test with real backends
5. Add metrics and logging
6. Document and add examples

---

**This feature is HIGH PRIORITY and should be implemented soon for cost optimization.**
