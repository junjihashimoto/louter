# Feature Roadmap - Louter (LLM Router)

This document outlines planned features and enhancements for the louter project, organized by category and priority.

## Priority Legend
- 🔴 **High Priority**: Critical for production use or high ROI
- 🟡 **Medium Priority**: Important for production readiness
- 🟢 **Low Priority**: Nice-to-have enhancements

## Status Legend
- ✅ **Implemented**: Feature is complete and available
- 🚧 **In Progress**: Currently being developed
- 📋 **Planned**: Approved for implementation
- 💡 **Proposed**: Under consideration

---

## High Priority Features

### 1. Performance Metrics (TTFT/TPS) - 📋 Planned
**Priority**: 🔴 High | **Complexity**: Medium | **Status**: 🚧 In Progress (Prometheus)

Complete implementation of performance tracking mentioned in CLAUDE.md.

**Features**:
- Time to First Token (TTFT)
- Tokens Per Second (TPS)
- Request duration histograms
- Per-model/backend metrics
- Prometheus metrics endpoint ✅ (Implementing now)
- Grafana dashboard templates

**Justification**: Essential for production monitoring and performance optimization.

**Implementation**: Phase 1 (Prometheus) in progress, Phase 2 (TTFT/TPS) planned.

---

### 2. Content-Based Model Routing - 💡 Proposed
**Priority**: 🔴 High | **Complexity**: Low-Medium | **Estimated Time**: 1-2 days

Automatically route requests to appropriate backends based on input data type.

**Features**:
- Automatic detection of vision/audio/multimedia in requests
- Route text-only requests to fast/cheap text models
- Route vision requests to vision-capable models
- Capability-based backend selection
- No client-side changes required
- Routing decision metrics and logging

**Justification**: Massive cost savings (80%) and performance gains (2.5x) by using specialized models.

**Use Case**:
- Text request → GPT-OSS (fast, cheap: $0.002/1k tokens)
- Vision request → Qwen3-Coder (capable: $0.010/1k tokens)

**Configuration Example**:
```toml
[routing]
strategy = "content-based"
prefer_text_model = true

[backends.text-model]
url = "http://localhost:11211"
capabilities = ["text", "function_calling"]
tool_format = "json"

[backends.vision-model]
url = "http://localhost:11212"
capabilities = ["text", "vision", "function_calling"]
tool_format = "xml"
```

**How It Works**:
1. Detect content type (text, images, audio, etc.)
2. Select backend with matching capabilities
3. Route request automatically
4. Log routing decision for metrics

**ROI**: Very High - 80% cost savings on text requests, 2.5x performance improvement, minimal implementation cost.

**See**: [docs/CONTENT-BASED-ROUTING.md](CONTENT-BASED-ROUTING.md) for detailed implementation guide.

---

### 3. Response Caching - 💡 Proposed
**Priority**: 🔴 High | **Complexity**: Medium | **Estimated Time**: 3-4 days

Cache LLM responses for identical requests to reduce costs and latency.

**Features**:
- Redis or in-memory cache backend
- Configurable TTL per endpoint/model
- Cache key based on request hash (messages, tools, parameters)
- Cache bypass header support
- Cache hit/miss metrics

**Justification**: LLM API calls are expensive. Cache can reduce costs by 40-80% for common queries.

**Configuration Example**:
```toml
[cache]
enabled = true
backend = "redis"  # or "memory"
redis_url = "redis://localhost:6379"
ttl_seconds = 3600
max_memory_mb = 500
exclude_streaming = true
```

**ROI**: High - Significant cost savings with moderate implementation effort.

---

### 4. Rate Limiting - 💡 Proposed
**Priority**: 🔴 High | **Complexity**: Medium | **Estimated Time**: 3-4 days

Protect backends from overload and enforce quotas.

**Features**:
- Per-IP, per-API-key, global rate limits
- Token bucket or sliding window algorithms
- Configurable burst allowance
- Rate limit headers (X-RateLimit-*)
- Redis-backed distributed rate limiting
- Different limits per endpoint/model

**Justification**: Essential for production stability, cost control, and abuse prevention.

**Configuration Example**:
```toml
[rate_limit]
enabled = true
backend = "redis"
requests_per_minute = 60
burst_size = 10
per_api_key = true

[rate_limit.overrides]
"/v1/chat/completions" = 100
"gpt-4" = 20
```

---

### 5. Load Balancing & Failover - 💡 Proposed
**Priority**: 🔴 High | **Complexity**: Medium-High | **Estimated Time**: 4-5 days

Distribute traffic across multiple backends with automatic failover.

**Features**:
- Round-robin, weighted, least-connections strategies
- Health check probes for backends
- Circuit breaker pattern for failing backends
- Automatic retry with exponential backoff
- Backend selection metrics

**Justification**: Critical for production reliability. Foundation already exists (weight in config).

**Status**: Partially designed, needs implementation.

---

### 6. Cost Tracking & Budgets - 💡 Proposed
**Priority**: 🔴 High | **Complexity**: Medium | **Estimated Time**: 3-4 days

Track API costs and enforce budgets.

**Features**:
- Token usage × pricing calculation
- Per-API-key cost tracking
- Daily/monthly budget limits
- Cost alerts via webhook
- Cost breakdown by model/user
- Export to billing systems

**Justification**: Critical for managing expensive LLM API costs.

**Configuration Example**:
```toml
[cost_tracking]
enabled = true

[cost_tracking.pricing]
"gpt-4" = { input = 0.00003, output = 0.00006 }
"gpt-3.5-turbo" = { input = 0.0000015, output = 0.000002 }

[cost_tracking.budgets]
daily_limit_usd = 100.0
monthly_limit_usd = 2000.0
alert_webhook = "https://hooks.slack.com/..."
```

---

## Medium Priority Features

### 6. API Key Management & Authentication - 💡 Proposed
**Priority**: 🟡 Medium | **Complexity**: Medium | **Estimated Time**: 3-4 days

Multi-tenant API key support with usage tracking.

**Features**:
- Proxy-level API keys separate from backend keys
- Multiple API keys with different quotas/permissions
- Key rotation support
- Usage tracking per API key
- Rate limiting per key
- Admin API for key management

**Justification**: Essential for multi-user deployments, prevents unauthorized access.

---

### 7. Web UI Dashboard - 💡 Proposed
**Priority**: 🟡 Medium | **Complexity**: Medium-High | **Estimated Time**: 11-16 days

Interactive dashboard for monitoring and debugging.

**Features**:
- Real-time request/response metrics
- Backend health visualization
- Log viewer with advanced filtering
- Cost tracking dashboard
- Error rate graphs
- Live request log viewer

**Technology Stack**:
- Askama templates (compile-time checked)
- htmx (minimal JavaScript)
- Embedded static files (single binary)
- TailwindCSS styling
- Chart.js for visualizations

**Justification**: Developer-friendly monitoring without external tools.

**Alternative**: Use Prometheus + Grafana for similar functionality with industry-standard tools.

---

### 8. Structured Logging Enhancement - 💡 Proposed
**Priority**: 🟡 Medium | **Complexity**: Low-Medium | **Estimated Time**: 2-3 days

Improve current logging with structured fields and log levels.

**Features**:
- Structured JSON logging with contextual fields
- Log sampling for high-traffic scenarios
- Request ID tracking across the chain
- Configurable log levels per component
- Log rotation and retention policies
- Integration with log aggregation (ELK, Loki)

**Justification**: Current logging is good but could be enhanced for production observability.

---

### 9. Distributed Tracing - 💡 Proposed
**Priority**: 🟡 Medium | **Complexity**: Medium | **Estimated Time**: 3-4 days

OpenTelemetry integration for request tracing.

**Features**:
- OpenTelemetry spans for each operation
- Trace context propagation
- Jaeger/Tempo/Zipkin export
- Custom span attributes (model, tokens, etc.)
- Correlation between logs and traces

**Justification**: Essential for debugging complex issues in production.

---

### 10. Request Validation & Sanitization - 💡 Proposed
**Priority**: 🟡 Medium | **Complexity**: Medium | **Estimated Time**: 3-4 days

Validate and sanitize inputs before forwarding.

**Features**:
- JSON schema validation
- Content filtering (PII, profanity, injection attempts)
- Request size limits
- Input sanitization for SQL/command injection
- Configurable validation rules per endpoint

**Justification**: Security best practice, prevents malformed requests from reaching backends.

---

### 11. TLS/HTTPS Support - 💡 Proposed
**Priority**: 🟡 Medium | **Complexity**: Medium | **Estimated Time**: 2-3 days

Secure communication with clients.

**Features**:
- TLS 1.2+ support
- Certificate management
- Client certificate authentication (mTLS)
- Automatic certificate renewal (Let's Encrypt)

**Justification**: Required for production deployments handling sensitive data.

---

### 12. Graceful Shutdown - 💡 Proposed
**Priority**: 🟡 Medium | **Complexity**: Low | **Estimated Time**: 1-2 days

Handle in-flight requests during shutdown.

**Features**:
- Wait for active requests to complete
- Configurable shutdown timeout
- Connection draining
- Kubernetes-compatible signals

**Justification**: Prevents request failures during deployments.

---

### 13. Admin API - 💡 Proposed
**Priority**: 🟡 Medium | **Complexity**: Medium | **Estimated Time**: 3-4 days

Management endpoints for operations.

**Features**:
- Runtime config viewing/updating
- Cache invalidation
- Force backend health check
- View active connections
- Dump metrics
- Emergency circuit breaker toggle

**Justification**: Essential for production operations.

---

### 14. Docker & Kubernetes Support - 💡 Proposed
**Priority**: 🟡 Medium | **Complexity**: Low-Medium | **Estimated Time**: 2-3 days

Production deployment artifacts.

**Features**:
- Multi-stage Dockerfile
- Docker Compose example
- Kubernetes manifests (Deployment, Service, HPA)
- Helm chart
- Health check probes

**Justification**: Standard deployment method for cloud-native applications.

---

## Low Priority Features

### 15. Mock Backend Mode - 💡 Proposed
**Priority**: 🟢 Low | **Complexity**: Medium | **Estimated Time**: 3-4 days

Built-in mock responses for testing.

**Features**:
- Configurable mock responses per endpoint
- Fixture file support
- Latency simulation
- Error injection for testing
- Record/replay mode

**Justification**: Simplifies development and testing without real API keys.

---

### 16. Hot Configuration Reload - 💡 Proposed
**Priority**: 🟢 Low | **Complexity**: Medium | **Estimated Time**: 2-3 days

Reload config without restart.

**Features**:
- File watch for config changes
- Graceful config reload
- Validation before applying
- Rollback on invalid config
- Admin API for config updates

**Justification**: Reduces downtime during configuration changes.

---

### 17. Request Replay Tool - 💡 Proposed
**Priority**: 🟢 Low | **Complexity**: Low-Medium | **Estimated Time**: 2-3 days

Replay logged requests for debugging.

**Features**:
- Load requests from JSON Lines logs
- Replay against same or different backend
- Diff tool for comparing responses
- Batch replay support
- Performance regression testing

**Justification**: Extends existing log-parser tool for debugging.

---

### 18. OpenAPI/Swagger Documentation - 💡 Proposed
**Priority**: 🟢 Low | **Complexity**: Low-Medium | **Estimated Time**: 2-3 days

Auto-generated API documentation.

**Features**:
- OpenAPI 3.0 spec generation
- Interactive Swagger UI
- Example requests/responses
- Authentication documentation

**Justification**: Better documentation for API consumers.

---

### 19. WebSocket Support - 💡 Proposed
**Priority**: 🟢 Low | **Complexity**: Medium | **Estimated Time**: 3-4 days

Alternative to SSE for streaming.

**Features**:
- WebSocket endpoints for streaming
- Bidirectional communication
- Better browser compatibility
- Lower latency than SSE

**Justification**: Some clients prefer WebSockets over SSE.

---

### 20. Additional API Format Support - 💡 Proposed
**Priority**: 🟢 Low | **Complexity**: Medium-High per format | **Estimated Time**: 4-5 days each

Support more LLM API formats.

**Formats**:
- Anthropic Claude API format
- Cohere API format
- Azure OpenAI specifics
- Hugging Face Inference API
- Custom format via plugins

**Justification**: Makes proxy more universal.

---

### 21. Batch Request Support - 💡 Proposed
**Priority**: 🟢 Low | **Complexity**: Medium | **Estimated Time**: 3-4 days

Handle multiple requests in one call.

**Features**:
- OpenAI batch API support
- Parallel execution
- Partial failure handling
- Batch result aggregation

**Justification**: Reduces overhead for bulk operations.

---

### 22. Token Optimization - 💡 Proposed
**Priority**: 🟢 Low | **Complexity**: Medium-High | **Estimated Time**: 4-5 days

Automatically optimize requests to reduce token usage.

**Features**:
- Conversation history truncation
- Smart summary of old messages
- Remove redundant whitespace
- Compress system instructions
- Warn on oversized requests

**Justification**: Reduces costs without changing application logic.

**Note**: Basic version exists (`max_conversation_turns`), needs enhancement.

---

### 23. Request/Response Transformation Rules - 💡 Proposed
**Priority**: 🟢 Low | **Complexity**: High | **Estimated Time**: 5-7 days

Configurable middleware for custom transformations.

**Features**:
- Lua/WASM scripting for custom logic
- Header manipulation
- Request/response filtering
- Custom field injection
- Conditional routing based on content

**Justification**: Allows users to customize behavior without forking.

---

### 24. Database Backend for State - 💡 Proposed
**Priority**: 🟢 Low | **Complexity**: Medium | **Estimated Time**: 4-5 days

Persistent storage for logs, keys, metrics.

**Features**:
- PostgreSQL/SQLite support
- Request history storage
- API key storage
- Usage statistics
- Query interface for analytics

**Justification**: Better than file-based storage for production.

---

## Quick Wins

These features provide high value with low implementation complexity:

1. **Docker/Kubernetes Support** (2-3 days) - Makes deployment accessible
2. **Graceful Shutdown** (1-2 days) - Better reliability
3. **Configuration Validation Tool** (1-2 days) - Prevents deployment issues
4. **Health Check Enhancements** (1 day) - Better k8s integration
5. **Connection Pooling** (1-2 days) - Performance improvement

---

## Implementation Timeline (Suggested)

### Phase 1: Production Essentials (4-6 weeks)
1. ✅ Prometheus Metrics (in progress)
2. Response Caching
3. Rate Limiting
4. Load Balancing & Failover
5. Docker/Kubernetes Support

### Phase 2: Security & Monitoring (3-4 weeks)
1. API Key Management
2. TLS/HTTPS Support
3. Distributed Tracing
4. Structured Logging Enhancement
5. Request Validation

### Phase 3: Cost & Operations (3-4 weeks)
1. Cost Tracking & Budgets
2. Admin API
3. Graceful Shutdown
4. Health Check Enhancements

### Phase 4: Developer Experience (4-5 weeks)
1. Web UI Dashboard (or use Grafana)
2. Mock Backend Mode
3. Request Replay Tool
4. OpenAPI Documentation
5. Hot Configuration Reload

### Phase 5: Protocol Extensions (ongoing)
1. Additional API Formats (as needed)
2. WebSocket Support
3. Batch Request Support
4. Token Optimization

---

## Community Contributions

We welcome contributions! If you'd like to implement any of these features:

1. **Check Status**: Ensure the feature isn't already in progress
2. **Open Issue**: Discuss the approach before starting
3. **Follow Guidelines**: See CONTRIBUTING.md for coding standards
4. **Submit PR**: Include tests and documentation

High-priority features are great starting points for new contributors.

---

## Feedback & Suggestions

Have ideas for new features? Please:
- Open a GitHub issue with the "enhancement" label
- Describe the use case and expected behavior
- Provide examples if applicable

---

## Notes

- **Complexity estimates** are for a solo developer with Rust experience
- **Time estimates** assume full-time work
- **Priorities** may change based on community needs
- **Prometheus metrics** is currently being implemented (Dec 2024)

---

*Last Updated: 2024-12-19*
