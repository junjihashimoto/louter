# OpenAI Streaming Function Calling Test

This test demonstrates how OpenAI streams function calls incrementally.

## Files

- `request.json` - Sample request with function definition
- `test.sh` - Script to test streaming function calls via OpenAI API
- `sample_response.txt` - Example of streamed response chunks

## Running the Test

```bash
export OPENAI_API_KEY='your-api-key-here'
./test.sh
```

## Expected Response Format

When streaming function calls, OpenAI returns Server-Sent Events (SSE) chunks:

### Chunk 1: Function name
```json
{
  "id": "chatcmpl-...",
  "choices": [{
    "index": 0,
    "delta": {
      "tool_calls": [{
        "index": 0,
        "id": "call_abc123",
        "type": "function",
        "function": {"name": "calculator", "arguments": ""}
      }]
    }
  }]
}
```

### Chunks 2-N: Incremental arguments
```json
{
  "choices": [{
    "delta": {
      "tool_calls": [{
        "index": 0,
        "function": {"arguments": "{\"expr"}
      }]
    }
  }]
}
```

```json
{
  "choices": [{
    "delta": {
      "tool_calls": [{
        "index": 0,
        "function": {"arguments": "ession\":\""}
      }]
    }
  }]
}
```

```json
{
  "choices": [{
    "delta": {
      "tool_calls": [{
        "index": 0,
        "function": {"arguments": "25*4\"}"}
      }]
    }
  }]
}
```

### Final chunk: Finish reason
```json
{
  "choices": [{
    "index": 0,
    "delta": {},
    "finish_reason": "tool_calls"
  }]
}
```

## Key Points

1. **Incremental JSON**: The function arguments are streamed as JSON fragments
2. **Assembly Required**: Clients must buffer and concatenate all argument fragments
3. **Multiple Tools**: The `index` field allows streaming multiple tool calls in parallel
4. **Validation**: Parse the complete JSON only after all chunks are received
