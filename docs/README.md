# Louter Documentation

Welcome to the Louter documentation! This directory contains all project documentation organized by purpose and audience.

## Documentation Structure

### 📚 [tutorial/](tutorial/) - **For New Users**
Get started quickly with step-by-step guides.

**Primary use case:** Run a local LLM and access it with both:
- `gemini-cli` (Gemini API client)
- `codex` (OpenAI coding agent)

**Start here if you are:**
- New to louter
- Setting up for the first time
- Looking for quick examples
- Learning how to configure louter

### 🏗️ [design/](design/) - **For Developers**
Understand how louter works internally.

**Start here if you are:**
- Contributing code
- Understanding architecture
- Learning protocol conversion logic
- Integrating with louter

### 🐛 [issues/](issues/) - **Current Known Issues**
Active bugs, limitations, and workarounds.

**Start here if you:**
- Hit a problem or error
- Need a workaround
- Want to check if issue is known
- Are triaging a bug report

### ✅ [resolved/](resolved/) - **Historical Fixes**
Resolved issues with solutions and lessons learned.

**Start here if you:**
- Are debugging a similar issue
- Want to learn from past problems
- Need test result references
- Are researching root causes

### 🔧 [debug/](debug/) - **Debugging Guides**
How to debug problems and use diagnostic tools.

**Start here if you:**
- Are troubleshooting an issue
- Need to analyze logs
- Want to use diagnostic tools
- Are debugging performance problems

### 🗺️ [plans/](plans/) - **Future Roadmap**
Planned features and design proposals.

**Start here if you:**
- Want to know what's coming next
- Are evaluating future features
- Want to contribute a feature
- Need to plan for upcoming changes

## Quick Navigation

### I want to...

**Get started with louter**
→ [tutorial/README.md](tutorial/README.md)

**Use louter with gemini-cli and codex**
→ tutorial/01-local-llm-setup.md *(coming soon)*

**Understand how protocol conversion works**
→ design/api-conversion-logic.md *(coming soon)*

**Debug a failed request**
→ debug/debugging-protocol-conversion.md *(coming soon)*

**Check if my issue is known**
→ [issues/README.md](issues/README.md)

**See what features are planned**
→ [plans/README.md](plans/README.md)

## User Personas

### 🆕 New User (First 15 minutes)
**Path:** tutorial/ → Try examples → Check issues/ if problems

### 👨‍💻 Developer (Contributing)
**Path:** tutorial/ → design/ → debug/ (for testing)

### 🔍 Debugging an Issue
**Path:** issues/ (check known issues) → debug/ (troubleshooting guides) → resolved/ (similar past issues)

### 📊 Operator (Production)
**Path:** tutorial/ (initial setup) → debug/ (troubleshooting) → plans/ (upcoming features)

## Documentation Standards

### Writing Style
- **Tutorials:** Simple, example-heavy, step-by-step
- **Design:** Technical, detailed, with rationale
- **Debug:** Action-oriented, checklist-style
- **Issues:** Structured, with reproduction steps
- **Plans:** Goal-oriented, with alternatives

### File Naming
- Use lowercase with hyphens: `feature-name.md`
- Number tutorials in sequence: `01-first-step.md`
- Date issues: `YYYY-MM-DD-short-description.md`
- Indicate status: `feature-name-RESOLVED.md`

### Cross-References
- Link between related documents
- Reference source code with line numbers: `src/main.rs:123`
- Link to config examples: `config-public-api.toml:45`
