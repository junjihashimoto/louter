# Hybrid Model Routing - Design Proposal

## Overview

Support intelligent routing between multiple backends based on request capabilities, allowing cost and performance optimization by using specialized models for different request types.

## Use Case

**Problem**: Vision models are expensive and slower, but text-only models can't handle images.

**Solution**: Automatically route requests to the appropriate model:
- Text-only requests → Fast, cheap text model (e.g., GPT-OSS)
- Vision requests → Capable but expensive model (e.g., GPT-4V, Qwen3-Coder with vision)
- Function calling → Models with tool support

## Benefits

1. **Cost Optimization**: 40-80% cost reduction by using cheaper models for text
2. **Performance**: Faster responses for text-only queries
3. **Resource Efficiency**: Better GPU/CPU utilization
4. **Automatic**: No client-side changes needed

## Design Options

### Option 1: Content-Based Routing (Recommended)

**How it works**: Automatically detect request capabilities and route to appropriate backend.

```toml
[routing]
strategy = "capability-based"  # or "round-robin", "weighted"

[backends.text-fast]
url = "http://localhost:11211"
capabilities = ["text", "function_calling"]
priority = 1  # Higher priority for matching requests
cost_per_token = 0.0001

[backends.vision-capable]
url = "http://localhost:11212"
capabilities = ["text", "vision", "audio", "function_calling"]
priority = 2  # Lower priority, use only when needed
cost_per_token = 0.001

[backends.text-fast.model_mapping]
"gpt-oss" = "gpt-oss:20b"

[backends.vision-capable.model_mapping]
"qwen-vision" = "Qwen3-Coder-30B"
```

**Routing Logic**:
```rust
fn select_backend(request: &Request) -> Backend {
    let required_capabilities = detect_capabilities(request);

    // Find backends that support all required capabilities
    let matching = backends.iter()
        .filter(|b| b.supports_all(required_capabilities))
        .collect();

    // Select by priority (lower cost/higher performance first)
    matching.sort_by_key(|b| (b.priority, b.cost_per_token));
    matching.first()
}

fn detect_capabilities(request: &Request) -> Vec<Capability> {
    let mut caps = vec![Capability::Text];

    // Check for images
    if has_images(request) {
        caps.push(Capability::Vision);
    }

    // Check for audio
    if has_audio(request) {
        caps.push(Capability::Audio);
    }

    // Check for tools
    if has_tools(request) {
        caps.push(Capability::FunctionCalling);
    }

    caps
}
```

**Advantages**:
- ✅ Automatic and transparent
- ✅ Optimal cost/performance
- ✅ No client changes
- ✅ Easy to configure

**Disadvantages**:
- ⚠️ Complexity in routing logic
- ⚠️ Need capability detection

---

### Option 2: Model-Prefix Routing

**How it works**: Use model name prefixes to indicate which backend to use.

```toml
[backends.text]
url = "http://localhost:11211"
model_prefix = "text-"

[backends.vision]
url = "http://localhost:11212"
model_prefix = "vision-"

[backends.text.model_mapping]
"text-gpt-oss" = "gpt-oss:20b"

[backends.vision.model_mapping]
"vision-qwen" = "Qwen3-Coder-30B"
```

**Usage**:
```bash
# Text request → routed to text backend
curl -X POST '/v1/chat/completions' \
  -d '{"model": "text-gpt-oss", "messages": [...]}'

# Vision request → routed to vision backend
curl -X POST '/v1/chat/completions' \
  -d '{"model": "vision-qwen", "messages": [...], "images": [...]}'
```

**Advantages**:
- ✅ Simple implementation
- ✅ Explicit control
- ✅ Easy debugging

**Disadvantages**:
- ❌ Requires client awareness
- ❌ No automatic optimization
- ❌ Manual model selection

---

### Option 3: Fallback Chain

**How it works**: Try backends in order, fallback on capability mismatch.

```toml
[routing]
strategy = "fallback-chain"

# Try text-only first (fast/cheap)
[[backends]]
name = "text-fast"
url = "http://localhost:11211"
capabilities = ["text", "function_calling"]
fallback_on = ["vision_required", "error"]

# Fallback to vision-capable
[[backends]]
name = "vision-fallback"
url = "http://localhost:11212"
capabilities = ["text", "vision", "function_calling"]
```

**Advantages**:
- ✅ Simple configuration
- ✅ Automatic fallback
- ✅ Good for testing

**Disadvantages**:
- ❌ Wasted requests on mismatch
- ❌ Increased latency
- ❌ Complex error handling

---

## Recommended Implementation: Option 1 (Content-Based)

### Phase 1: Basic Capability Detection (1-2 days)

1. **Add capability field to backend config**:
```toml
[backends.gpt-oss]
url = "http://localhost:11211"
capabilities = ["text", "function_calling"]
tool_format = "json"
```

2. **Implement capability detection**:
```rust
// src/routing.rs
pub enum Capability {
    Text,
    Vision,
    Audio,
    Video,
    FunctionCalling,
}

pub fn detect_required_capabilities(request: &Request) -> HashSet<Capability> {
    let mut caps = HashSet::new();
    caps.insert(Capability::Text);

    // Check for vision
    for content in &request.contents {
        for part in &content.parts {
            if part.inline_data.is_some() {
                // Check mime type
                if part.inline_data.mime_type.starts_with("image/") {
                    caps.insert(Capability::Vision);
                }
            }
        }
    }

    // Check for tools
    if request.tools.is_some() {
        caps.insert(Capability::FunctionCalling);
    }

    caps
}
```

3. **Implement backend selection**:
```rust
pub fn select_backend(
    request: &Request,
    backends: &HashMap<String, BackendConfig>
) -> Result<BackendConfig, Error> {
    let required = detect_required_capabilities(request);

    // Find compatible backends
    let compatible: Vec<_> = backends.values()
        .filter(|b| required.is_subset(&b.capabilities))
        .collect();

    if compatible.is_empty() {
        return Err(Error::NoCompatibleBackend(required));
    }

    // Select by priority/cost
    compatible.into_iter()
        .min_by_key(|b| (b.priority, b.cost_per_token))
        .cloned()
        .ok_or(Error::NoBackendAvailable)
}
```

### Phase 2: Advanced Routing (2-3 days)

1. **Load balancing within capability group**
2. **Health checks and circuit breakers**
3. **Cost tracking per backend**
4. **Routing metrics**

### Phase 3: Intelligent Routing (3-4 days)

1. **Model capability auto-detection** (query backend for capabilities)
2. **Performance-based routing** (use faster backend when both can handle)
3. **Cost optimization suggestions** (log potential savings)

---

## Configuration Examples

### Example 1: Text + Vision Hybrid

```toml
[routing]
strategy = "capability-based"
prefer_lower_cost = true

[backends.cheap-text]
name = "GPT-OSS Text"
url = "http://localhost:11211"
capabilities = ["text", "function_calling"]
priority = 1
cost_per_1k_tokens = 0.002
tool_format = "json"

[backends.cheap-text.model_mapping]
"gpt-oss" = "gpt-oss:20b"

[backends.expensive-vision]
name = "Qwen Vision"
url = "http://localhost:11212"
capabilities = ["text", "vision", "function_calling"]
priority = 2
cost_per_1k_tokens = 0.010
tool_format = "xml"

[backends.expensive-vision.model_mapping]
"qwen-vision" = "Qwen3-Coder-30B"
```

**Result**:
- Text request: Uses GPT-OSS ($0.002/1k tokens)
- Image request: Uses Qwen ($0.010/1k tokens)
- **Savings**: Up to 80% on text-only requests

### Example 2: Three-Tier Setup

```toml
[routing]
strategy = "capability-based"

# Tier 1: Ultra-fast text (no tools)
[backends.fast-text]
url = "http://localhost:11211"
capabilities = ["text"]
priority = 1
max_tokens = 2048

# Tier 2: Text + Tools
[backends.tool-capable]
url = "http://localhost:11212"
capabilities = ["text", "function_calling"]
priority = 2
max_tokens = 4096

# Tier 3: Full multimodal
[backends.multimodal]
url = "http://localhost:11213"
capabilities = ["text", "vision", "audio", "function_calling"]
priority = 3
max_tokens = 8192
```

### Example 3: Fallback to Cloud

```toml
[routing]
strategy = "capability-based"
enable_fallback = true

# Primary: Local model
[backends.local]
url = "http://localhost:11211"
capabilities = ["text", "function_calling"]
priority = 1
health_check_interval = 10

# Fallback: Cloud API
[backends.cloud-fallback]
url = "https://api.openai.com"
capabilities = ["text", "vision", "function_calling"]
priority = 10
api_key_env = "OPENAI_API_KEY"
use_only_on_failure = true
```

---

## Alternative/Complementary Features

### Feature A: Model Specialization Tags

Allow tagging backends for specific use cases:

```toml
[backends.code-specialist]
url = "http://localhost:11211"
capabilities = ["text", "function_calling"]
specializations = ["code", "api", "debugging"]

[backends.creative-writer]
url = "http://localhost:11212"
capabilities = ["text"]
specializations = ["creative", "storytelling", "translation"]
```

Request can hint specialization:
```json
{
  "model": "auto",
  "messages": [...],
  "metadata": {
    "task_type": "code"  // Routes to code-specialist
  }
}
```

### Feature B: Smart Model Selection

Use request analysis to select optimal model:

```toml
[routing]
strategy = "smart"
enable_request_analysis = true

# Analyze request to determine:
# - Complexity (simple vs complex)
# - Length (short vs long context)
# - Type (code, creative, factual)
# - Select best model automatically
```

### Feature C: Cost Budget Routing

Route based on user budget:

```toml
[routing]
strategy = "budget-aware"

[backends.cheap]
cost_per_1k_tokens = 0.001
max_budget_per_request = 0.10

[backends.expensive]
cost_per_1k_tokens = 0.010
max_budget_per_request = 1.00
```

---

## Recommendations

### For Your Use Case (Text + Vision)

**Best approach**: **Option 1 (Content-Based Routing)**

**Configuration**:
```toml
[routing]
strategy = "capability-based"

[backends.gpt-oss-text]
url = "http://localhost:11211"
capabilities = ["text", "function_calling"]
priority = 1
tool_format = "json"

[backends.qwen-vision]
url = "http://localhost:11212"
capabilities = ["text", "vision", "function_calling"]
priority = 2
tool_format = "xml"
```

**Why**:
- ✅ Automatic routing based on content
- ✅ 80% cost savings on text requests
- ✅ No client changes needed
- ✅ Clear configuration
- ✅ Easy to extend

### Implementation Priority

1. **Phase 1** (High Priority): Basic capability detection and routing
2. **Phase 2** (Medium): Health checks, metrics, load balancing
3. **Phase 3** (Low): Smart routing, cost optimization

### Related Features to Consider

1. **Response Caching**: Cache text responses to reduce backend calls
2. **Rate Limiting**: Protect expensive backends from overload
3. **Cost Tracking**: Monitor actual costs per backend
4. **A/B Testing**: Compare model performance for same requests

---

## Implementation Checklist

- [ ] Add `capabilities` field to `BackendConfig`
- [ ] Create `src/routing.rs` module
- [ ] Implement `detect_required_capabilities()`
- [ ] Implement `select_backend()` with priority logic
- [ ] Add routing metrics (which backend selected)
- [ ] Update configuration parser
- [ ] Add routing tests
- [ ] Document in README.md
- [ ] Add example configs
- [ ] Update FEATURE-ROADMAP.md

---

## Estimated Effort

- **Phase 1 (Basic)**: 1-2 days
- **Phase 2 (Advanced)**: 2-3 days
- **Phase 3 (Smart)**: 3-4 days
- **Total**: 6-9 days for complete implementation

---

## Questions for Consideration

1. Should routing be per-request or per-model?
2. How to handle partial capability matches? (e.g., model supports vision but not audio)
3. Should there be manual override capability?
4. How to handle streaming with backend switching?
5. Should cost be tracked and enforced?

---

*For implementation, this feature should be added to the FEATURE-ROADMAP.md as a high-priority feature.*
