# Tutorial Documentation

**Audience:** New users, evaluators, first-time users

**Purpose:** Help users get started quickly with step-by-step guides and practical examples.

## Primary Use Case

**Enable gemini-cli and codex to access your local LLM:**
- Run a local LLM (llama.cpp, Ollama, vLLM, etc.)
- Use louter as a dual-API proxy
- Access via gemini-cli (Gemini API client)
- Access via codex (OpenAI coding agent)
- Both clients use the same local model through different APIs

## What Goes Here

✅ **Include:**
- Getting started with local LLM setup
- Configuring louter for gemini-cli + codex
- Step-by-step tutorials for common workflows
- Configuration examples with explanations
- Quick wins and "hello world" examples
- Common use case walkthroughs

❌ **Don't Include:**
- Architecture details (→ `design/`)
- Troubleshooting (→ `debug/`)
- Future plans (→ `plans/`)
- Bug reports (→ `issues/`)

## File Organization

- Use descriptive names: `01-local-llm-setup.md`, `02-gemini-cli-codex.md`
- Number files in recommended reading order
- Keep language simple and example-heavy
- Focus on "how to do X" not "why X works this way"

## Recommended Tutorial Sequence

1. **Local LLM Setup** - Install and run llama.cpp/Ollama
2. **Louter Configuration** - Configure for dual API access
3. **gemini-cli Setup** - Connect gemini-cli to louter
4. **codex Setup** - Connect codex (OpenAI agent) to louter
5. **Advanced Routing** - Content-based routing, multiple models
6. **Monitoring** - Web UI dashboard and metrics
