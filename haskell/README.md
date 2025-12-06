# Louter - Multi-Protocol LLM Client Library

> **Key Design Principle:** "Louter is a protocol converter. The API can connect to OpenAI API and Gemini API like the proxy."

Louter is a Haskell library and proxy server that provides a unified interface to multiple LLM APIs (OpenAI, Gemini, Anthropic) with automatic protocol conversion.

## Features

✅ **Protocol Conversion** - Use OpenAI-style requests with any LLM backend
✅ **SSE Streaming** - Efficient attoparsec-based SSE parsing
✅ **Automatic Buffering** - Function calls are buffered until complete JSON
✅ **Multiple Tool Calls** - Supports parallel execution of multiple tool calls
✅ **Type-Safe** - Well-defined Haskell types for all protocols
✅ **Mock Server** - Replay captured responses for testing

## Quick Start

### As a Library

```haskell
import Louter.Client
import Louter.Types

main = do
  -- Connect to Gemini API using OpenAI-style requests!
  client <- newClient $ BackendGemini "your-api-key" Nothing

  let request = defaultChatRequest "gemini-pro"
        [Message RoleUser "Hello!"]

  response <- chatCompletion client request
  print response
```

### As a Proxy Server

```bash
# Build
stack build

# Run proxy server
stack run louter-server -- --config config.toml --port 9000

# Your app connects to proxy using any protocol
curl http://localhost:9000/v1/messages \  # Anthropic format
  -H "Content-Type: application/json" \
  -d '{"model": "claude-3", "messages": [...]}'

# Proxy converts and forwards to actual backend
```

## Streaming with Automatic Buffering

Louter intelligently buffers different types of content:

| Content Type | Buffering | Why |
|--------------|-----------|-----|
| `reasoning` | **No buffer** - stream immediately | User sees thinking in real-time |
| `content` | **No buffer** - stream immediately | User sees text as it generates |
| `tool_calls` | **Buffer until complete** | Must assemble valid JSON |

```haskell
streamChatWithCallback client request $ \event -> case event of
  StreamContent txt -> putStr txt  -- Immediate
  StreamReasoning txt -> putStr $ "[thinking] " <> txt  -- Immediate
  StreamToolCall call -> executeToolCall call  -- Complete JSON, ready to use!
  StreamFinish reason -> putStrLn "Done"
```

## Multiple Parallel Tool Calls

**Memorized Feature:** "OpenAI's API supports multiple tool calls, which can be executed in parallel (concurrently) or sequentially, depending on how the developer orchestrates the workflow."

Louter tracks each tool call by index and emits complete tool calls as soon as their JSON is valid:

```haskell
-- Receive tool calls as they complete
streamChatWithCallback client request $ \event -> case event of
  StreamToolCall call -> do
    -- Execute in parallel!
    async $ do
      result <- executeToolCall call
      sendResultBack result
```

The state machine maintains `Map Int ToolCallState` to track multiple tool calls simultaneously:
- Tool call 0: `calculator("25*4")`
- Tool call 1: `search("Haskell")`
- Both stream in parallel, emit as soon as complete

## Supported Protocols

### OpenAI
- Native format (Internal Representation)
- Streaming: `data: {...}\n\n`
- Tool calls: `tool_calls[i].function.arguments`

### Gemini
- Converts Protobuf Schema (numeric types) → JSON Schema (string types)
- Streaming: SSE with `candidates[].content.parts[]`
- Tool calls: `functionCall{name, args}`

### Anthropic
- Converts Anthropic events → IR chunks
- Streaming: Event-based SSE (`content_block_delta`, `input_json_delta`)
- Tool calls: `tool_use` blocks

## Mock Server for Testing

```bash
# Start mock server with test data
stack run openai-mock -- --test-data ./test-data/openai --port 11211

# Your app connects to mock server
client <- newClient $ BackendOpenAI "test-key" (Just "http://localhost:11211")

# Replays captured responses deterministically!
```

## Architecture

```
Your Application
  ↓
Louter.Client (uses OpenAI-style requests)
  ↓
Protocol Converter (automatic)
  ↓
Actual LLM API (OpenAI/Gemini/Anthropic)
```

**Internal Representation:**
- All protocols convert TO/FROM OpenAI format
- OpenAI format is the "lingua franca"
- Protocol-specific details handled by converters

## Examples

See `louter/examples/`:
- `SimpleChat.hs` - Connect to Gemini with OpenAI requests
- `StreamingChat.hs` - Real-time streaming
- `ParallelToolCalls.hs` - Multiple concurrent tool calls

## Building

```bash
cd haskell
stack build
stack test
```

## Testing

```bash
# Run mock server
stack run openai-mock

# Run your app against mock server
OPENAI_API_KEY=test stack run example-simple-chat
```

## Documentation

See [CLAUDE.md](../CLAUDE.md#6-haskell-implementation-architecture) for detailed architecture documentation.

## License

MIT
