# Known Issues

**Audience:** Users encountering problems, developers triaging bugs

**Purpose:** Document current known issues, limitations, and workarounds.

## What Goes Here

✅ **Include:**
- Current bugs and their status
- Known limitations
- Workarounds for common problems
- Platform-specific issues
- Compatibility issues
- Open questions needing investigation

❌ **Don't Include:**
- Fixed issues (→ `resolved/`)
- Feature requests (→ `plans/`)
- How to debug (→ `debug/`)
- Design rationale (→ `design/`)

## Issue Template

Each issue document should include:
```markdown
# [Short Title]

**Status:** Open | Under Investigation | Workaround Available
**Severity:** Critical | High | Medium | Low
**Affects:** [Version/Configuration]
**Reported:** [Date]

## Description
[What is the problem?]

## Reproduction Steps
1. Step 1
2. Step 2
3. Observe problem

## Expected Behavior
[What should happen?]

## Actual Behavior
[What actually happens?]

## Workaround
[If available, how to work around this issue]

## Investigation Notes
[What has been tried, what we learned]
```

## File Naming Convention

Use format: `YYYY-MM-DD-short-description.md`
- `2025-11-21-gemini-quota-exhaustion.md`
- `2025-11-20-temperature-unsupported.md`

## Lifecycle

- New issues → Create file here
- Issue resolved → Move to `resolved/`
- Need more info → Add "Under Investigation" status
- Workaround found → Update with workaround section
