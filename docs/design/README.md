# Design Documentation

**Audience:** Developers, contributors, architects

**Purpose:** Explain how louter works internally, design decisions, and architectural patterns.

## What Goes Here

✅ **Include:**
- Architecture overview and diagrams
- Design decisions and rationale
- Protocol conversion logic (Gemini ↔ OpenAI)
- API design and endpoints
- Data flow diagrams
- Token count mapping strategies
- Why we chose specific approaches

❌ **Don't Include:**
- How to use features (→ `tutorial/`)
- Current bugs (→ `issues/`)
- Future plans (→ `plans/`)
- Debugging techniques (→ `debug/`)

## Key Design Documents

### Core Architecture
- **Dual Frontend Design** - Why support both Gemini and OpenAI APIs
- **Bidirectional Conversion** - How we convert between formats
- **Centralized API Key Management** - Design decision for large-scale deployments

### Protocol Details
- **Token Count Mapping** - candidatesTokenCount + thoughtsTokenCount → completion_tokens
- **Temperature Handling** - Per-backend configuration strategy
- **max_tokens Fields** - Supporting both old and new OpenAI APIs

### Routing Logic
- **Content-Based Routing** - Capability detection and backend selection
- **Model-Based Routing** - model_mapping and priority systems
- **Fallback Chains** - How failover works

## File Naming Convention

Use descriptive names that explain what is being designed:
- `architecture-overview.md`
- `token-count-mapping.md`
- `routing-strategy.md`
- `api-conversion-logic.md`
