# Implementation vs Spec Analysis

## Issues Found

### ❌ Issue 1: Sending [DONE] marker
**Location:** `src/conversion.rs:1867-1873`

**Problem:** We send `data: [DONE]` after `message_stop`, but this is NOT part of the Claude API spec. This is an OpenAI convention.

**Current Code:**
```rust
// Send [DONE] marker if not sent yet
if !state.sent_done {
    state.sent_done = true;
    let event = Event::default()
        .data("[DONE]");
    return Some((Ok(event), (stream, state)));
}
```

**Spec Says:** The stream should end after `message_stop`. No `[DONE]` marker.

**Impact:** Minor - Claude Code likely ignores it, but violates spec compliance.

**Fix:** Remove the [DONE] marker logic.

---

### ⚠️ Issue 2: Preemptive text content_block_start
**Location:** `src/conversion.rs:1641-1656`

**Problem:** We send `content_block_start` for a text block immediately after `message_start`, BEFORE we know if the model will return text or tool calls.

**Current Code:**
```rust
// Send content_block_start immediately after message_start
if !state.text_block_started {
    state.text_block_started = true;
    let block_start = anthropic::StreamEvent::ContentBlockStart {
        index: 0,
        content_block: anthropic::StreamContentBlock::Text {
            text: String::new(),
        },
    };
    ...
}
```

**Spec Says:** Content blocks should only be sent when there's actual content. The model might:
- Return only text → 1 text block
- Return only tool calls → 1+ tool_use blocks
- Return both → text block + tool_use blocks

**Current Behavior:**
- Always sends empty text block first
- If model only calls tools, we send an empty text block (index 0) followed by tool blocks (index 1+)

**Spec-Compliant Behavior:**
- Wait for first content (text or tool)
- Send appropriate content_block_start when content arrives
- If model only calls tools, first block should be index 0 (tool_use), not index 1

**Impact:** Medium - Creates unnecessary empty text blocks, incorrect indexing for tool-only responses.

**Fix:** Only send content_block_start when we receive actual content (text or tool call).

---

### ✅ Correct Implementation: Tool use blocks
**Location:** `src/conversion.rs:1720-1743`

We correctly wait for both `id` and `name` before sending tool_use content_block_start. This matches the spec requirement.

---

### ✅ Correct Implementation: message_delta and message_stop
**Location:** `src/conversion.rs:1832-1863`

Correctly sends `message_delta` with stop_reason and usage, followed by `message_stop`.

---

## Recommendations

### Priority 1: Remove [DONE] marker
Simple fix, improves spec compliance.

### Priority 2: Fix preemptive content_block_start  
More complex fix, but necessary for:
- Tool-only responses (no text)
- Correct content block indexing
- Full spec compliance

### Implementation Strategy for Fix #2

Instead of sending content_block_start immediately after message_start:
1. Wait for first content from backend
2. If it's text → send text content_block_start (index 0)
3. If it's a tool call → close any text block if open, send tool_use content_block_start
4. Track if we've sent any content yet

**State Changes:**
- Remove proactive content_block_start after message_start
- Add `has_sent_any_content: bool` flag
- Check this flag when receiving first text or tool content
- Send appropriate content_block_start based on content type

