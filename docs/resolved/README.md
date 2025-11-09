# Resolved Issues & Fixes

**Audience:** Developers debugging similar issues, historical reference, learning from past problems

**Purpose:** Document resolved issues, fixes, test results, and lessons learned.

## What Goes Here

✅ **Include:**
- Fixed bugs with solution details
- Test results from debugging sessions
- What was learned from fixing the issue
- Cross-protocol fix summaries
- Diagnostic test results
- Before/after comparisons
- Root cause analysis

❌ **Don't Include:**
- Current open issues (→ `issues/`)
- Future work (→ `plans/`)
- General debugging techniques (→ `debug/`)

## Resolved Issue Template

```markdown
# [Short Title] - RESOLVED

**Resolved:** [Date]
**Root Cause:** [Brief summary]
**Solution:** [Brief summary]

## Original Problem
[What was broken?]

## Investigation Process
[How did we debug this?]

## Root Cause
[What was actually wrong?]

## Solution
[What did we change to fix it?]

## Files Changed
- `src/file1.rs` - [what changed]
- `src/file2.rs` - [what changed]

## Test Results
[Before/after test results]

## Lessons Learned
[What did we learn? How to prevent this in future?]
```

## File Naming Convention

Use format: `YYYY-MM-DD-short-description-RESOLVED.md`
- `2025-11-21-temperature-config-RESOLVED.md`
- `2025-11-21-max-tokens-field-RESOLVED.md`
- `2025-11-21-optional-fields-RESOLVED.md`

## Types of Documents

### Bug Fix Summaries
- What was broken
- How it was fixed
- Test results proving the fix

### Test Results
- Diagnostic test outputs
- Cross-protocol test results
- Performance benchmarks
- Before/after comparisons

### Debugging Sessions
- Mock server debugging
- Public API testing
- Protocol conversion issues
