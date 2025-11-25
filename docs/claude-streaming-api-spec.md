# Claude API Streaming Specification

## Overview

The Claude Messages API supports streaming responses using Server-Sent Events (SSE).

## Event Flow

Each stream follows this event sequence:

1. **`message_start`** - Contains a Message object with empty `content`
2. **Content blocks** - A series of:
   - `content_block_start` - Start of a content block
   - `content_block_delta` (one or more) - Incremental content updates
   - `content_block_stop` - End of a content block
   
   Each content block has an `index` corresponding to its position in the final Message `content` array.

3. **`message_delta`** (one or more) - Top-level changes to the final Message object
   - **Note:** Token counts in `usage` field are **cumulative**
   
4. **`message_stop`** - Final event indicating stream completion

## Additional Events

- **`ping`** - Keep-alive events that may appear at any point
- **`error`** - Error events if something goes wrong

## Event Types

### message_start

Contains the initial message structure with empty content:

\`\`\`json
{
  "type": "message_start",
  "message": {
    "id": "msg_...",
    "type": "message",
    "role": "assistant",
    "content": [],
    "model": "claude-3-5-sonnet-20241022",
    "stop_reason": null,
    "stop_sequence": null,
    "usage": {
      "input_tokens": 25,
      "output_tokens": 1
    }
  }
}
\`\`\`

### content_block_start

Signals the start of a new content block:

#### Text Block
\`\`\`json
{
  "type": "content_block_start",
  "index": 0,
  "content_block": {
    "type": "text",
    "text": ""
  }
}
\`\`\`

#### Tool Use Block
\`\`\`json
{
  "type": "content_block_start",
  "index": 1,
  "content_block": {
    "type": "tool_use",
    "id": "toolu_...",
    "name": "get_weather",
    "input": {}
  }
}
\`\`\`

**Important:** Tool use blocks must include both `id` and `name` in the `content_block_start` event.

### content_block_delta

Provides incremental updates to content blocks:

#### Text Delta
\`\`\`json
{
  "type": "content_block_delta",
  "index": 0,
  "delta": {
    "type": "text_delta",
    "text": "Hello "
  }
}
\`\`\`

#### Input JSON Delta (for tool_use)
\`\`\`json
{
  "type": "content_block_delta",
  "index": 1,
  "delta": {
    "type": "input_json_delta",
    "partial_json": "{\"location\": \"San Fra"
  }
}
\`\`\`

**Note:** Tool input deltas are partial JSON strings. Accumulate them and parse once you receive `content_block_stop`.

### content_block_stop

Signals the end of a content block:

\`\`\`json
{
  "type": "content_block_stop",
  "index": 0
}
\`\`\`

### message_delta

Provides top-level message updates (typically stop reason and final token count):

\`\`\`json
{
  "type": "message_delta",
  "delta": {
    "stop_reason": "end_turn",
    "stop_sequence": null
  },
  "usage": {
    "output_tokens": 15
  }
}
\`\`\`

**Important:** The `output_tokens` count is **cumulative**, not incremental.

#### Stop Reasons
- `end_turn` - Natural completion
- `max_tokens` - Maximum token limit reached  
- `stop_sequence` - Stop sequence encountered
- `tool_use` - Model wants to use a tool

### message_stop

Final event indicating completion:

\`\`\`json
{
  "type": "message_stop"
}
\`\`\`

### ping

Keep-alive event with no data:

\`\`\`json
{
  "type": "ping"
}
\`\`\`

### error

Error event if something goes wrong:

\`\`\`json
{
  "type": "error",
  "error": {
    "type": "overloaded_error",
    "message": "Overloaded"
  }
}
\`\`\`

## Complete Example

\`\`\`
event: message_start
data: {"type":"message_start","message":{"id":"msg_123","type":"message","role":"assistant","content":[],"model":"claude-3-5-sonnet-20241022","stop_reason":null,"stop_sequence":null,"usage":{"input_tokens":10,"output_tokens":1}}}

event: content_block_start
data: {"type":"content_block_start","index":0,"content_block":{"type":"text","text":""}}

event: ping
data: {"type":"ping"}

event: content_block_delta
data: {"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"Hello"}}

event: content_block_delta
data: {"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"!"}}

event: content_block_stop
data: {"type":"content_block_stop","index":0}

event: message_delta
data: {"type":"message_delta","delta":{"stop_reason":"end_turn","stop_sequence":null},"usage":{"output_tokens":5}}

event: message_stop
data: {"type":"message_stop"}
\`\`\`

## Extended Thinking

When using extended thinking models, you'll receive:
- `thinking` content blocks with `thinking_delta` events
- A `signature_delta` event before `content_block_stop` to verify integrity

## Key Implementation Notes

1. **Tool Use Blocks:** Must include `id` and `name` in `content_block_start`
2. **Partial JSON:** Tool input deltas are partial JSON strings - accumulate before parsing
3. **Cumulative Tokens:** Token counts in `message_delta` are cumulative, not incremental
4. **Multiple Content Blocks:** A single message can contain multiple text and tool_use blocks
5. **Block Ordering:** Content blocks are indexed starting from 0

## References

- Official Documentation: https://docs.anthropic.com/en/api/messages-streaming
- API Reference: https://docs.anthropic.com/en/api/messages
