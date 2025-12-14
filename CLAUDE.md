# CLAUDE.md

> **Note for Claude:** This file is the **Single Source of Truth** for the `louter` project architecture, testing strategy, and coding standards. Read this before modifying any code.

## 1. Project Overview

**Louter** is a Haskell library and proxy server for multi-protocol LLM communication.

### Dual Purpose

1. **Client Library** - Import into your Haskell application to connect to any LLM API
2. **Proxy Server** - Run standalone to bridge between different LLM API protocols

### Supported APIs

**Frontends (What clients can send):**
- OpenAI API format
- Anthropic (Claude) API format
- Google Gemini API format

**Backends (What louter can connect to):**
- OpenAI API, Anthropic API, Google Gemini API
- Self-hosted/OSS models: llama-server, vLLM, text-generation-inference
- Local models via llama.cpp server

### Key Features

- **Protocol Translation**: Automatic conversion between API formats
- **Streaming Support**: Server-Sent Events (SSE) with proper buffering
- **Function Calling**: Tool/function calling across all protocols
- **Vision Support**: Multimodal image handling
- **XML Tools**: Support for Qwen's XML-based function calling format
- **Configurable Auth**: Optional authentication for local vs cloud backends

---

## 2. Haskell Implementation Architecture

### Overview

**Key Design Principle:** *"Louter is a protocol converter. The API can connect to OpenAI API and Gemini API like the proxy."*

**Single Unified Implementation: `louter`**

The Haskell `louter` package serves dual purposes:

1. **As a Library** - Import into your Haskell application to connect to any LLM API
   ```haskell
   import Louter.Client

   -- Connect to Gemini API using OpenAI-style requests
   client <- newClient GeminiBackend "gemini-api-key"
   response <- chatCompletion client openAIStyleRequest
   ```

2. **As a Proxy Server** - Run standalone to proxy requests between protocols
   ```bash
   stack run louter-server -- --config config.yaml --port 9000
   ```

**Core Components:**
- Protocol converters (OpenAI ↔ Gemini ↔ Anthropic)
- SSE parser with attoparsec
- Delta tokenizer/classifier
- Stateful function call buffering
- Mock server for testing (`openai-mock`)

### 2.1 SSE Parsing with Attoparsec

The library uses `attoparsec` for efficient incremental SSE parsing.

**SSE Format:**
```
data: {"id":"chatcmpl-...","choices":[{"delta":{...}}]}\n
\n
```

**Parser Structure:**
```haskell
data SSEChunk = SSEChunk
  { sseData :: ByteString  -- Raw JSON payload
  , sseEvent :: Maybe Text -- Optional event type
  } | SSEDone              -- [DONE] marker

parseSSE :: Parser SSEChunk
parseSSE = choice
  [ string "data: [DONE]" >> pure SSEDone
  , SSEChunk <$> (string "data: " *> takeTill isEndOfLine) <*> pure Nothing
  ]
```

### 2.2 Delta Type Classification

The tokenizer identifies delta types by examining the JSON structure:

```haskell
data DeltaType
  = ReasoningDelta Text       -- delta.reasoning: "thinking tokens"
  | ContentDelta Text         -- delta.content: "response text"
  | ToolCallDelta ToolCallFragment  -- delta.tool_calls[]: function call
  | RoleDelta Role            -- delta.role: "assistant"
  | FinishDelta FinishReason  -- finish_reason: "stop" | "tool_calls"
  | EmptyDelta                -- delta: {}

data ToolCallFragment = ToolCallFragment
  { tcfIndex :: Int
  , tcfId :: Maybe Text
  , tcfName :: Maybe Text
  , tcfArguments :: Maybe Text
  }

classifyDelta :: Value -> Either String DeltaType
classifyDelta = withObject "delta" $ \obj -> do
  case HM.lookup "delta" obj of
    Just (Object delta) ->
      case HM.lookup "reasoning" delta of
        Just (String txt) -> pure $ ReasoningDelta txt
        Nothing -> case HM.lookup "content" delta of
          Just (String txt) -> pure $ ContentDelta txt
          Nothing -> case HM.lookup "tool_calls" delta of
            Just (Array calls) -> ... -- Parse tool call fragment
            Nothing -> pure EmptyDelta
```

### 2.3 Buffering Strategy

**Key Principle:** Different delta types require different streaming behaviors.

| Delta Type | Buffering | Reason |
|------------|-----------|--------|
| `reasoning` | **No buffer** - stream immediately | User sees thinking process in real-time |
| `content` | **No buffer** - stream immediately | User sees response text as it generates |
| `tool_calls` | **Buffer until complete** | Must assemble valid JSON before emitting |
| `role` | **Pass through** | Metadata, no buffering needed |
| `finish_reason` | **Pass through** | End-of-stream marker |

**State Machine:**
```haskell
data StreamState = StreamState
  { ssToolCalls :: Map Int ToolCallState  -- Track by index
  , ssMessageId :: Text
  }

data ToolCallState = ToolCallState
  { tcsId :: Text
  , tcsName :: Text
  , tcsArguments :: Builder  -- Efficient text accumulation
  , tcsComplete :: Bool
  }

processChunk :: StreamState -> DeltaType -> (StreamState, [OutputChunk])
processChunk state (ContentDelta txt) =
  -- Immediate emission, no buffering
  (state, [OutputContentChunk txt])

processChunk state (ToolCallDelta frag) =
  -- Buffer arguments, emit only when JSON is complete
  let newState = updateToolCallBuffer state frag
      maybeComplete = checkJSONComplete newState (tcfIndex frag)
  in case maybeComplete of
       Just toolCall -> (resetBuffer newState, [OutputToolCall toolCall])
       Nothing -> (newState, [])  -- Keep buffering
```

### 2.4 JSON Completeness Detection

```haskell
isCompleteJSON :: Text -> Bool
isCompleteJSON txt =
  let trimmed = T.strip txt
  in not (T.null trimmed)
     && T.head trimmed == '{'
     && T.last trimmed == '}'
     && isRight (eitherDecode (encodeUtf8 txt) :: Either String Value)
```

### 2.5 Client Library API

**Simple Usage:**
```haskell
import Louter.Client
import Louter.Client.OpenAI (llamaServerClient)
import Louter.Client.Gemini (geminiClient)

main :: IO ()
main = do
  -- For cloud APIs (requires authentication)
  client <- geminiClient "your-api-key"

  -- For local llama-server (no authentication)
  llamaClient <- llamaServerClient "http://localhost:11211"

  -- Non-streaming request
  response <- chatCompletion client $ defaultChatRequest "gpt-4"
    [Message RoleUser "Hello!"]
  print response

  -- Streaming request with callback
  streamChatWithCallback client request $ \event -> case event of
    StreamContent txt -> putStr txt >> hFlush stdout
    StreamReasoning txt -> putStrLn ("[thinking] " <> txt)
    StreamToolCall call -> print call  -- Already complete!
    StreamFinish reason -> putStrLn ("\n[Done: " <> reason <> "]")
    StreamError err -> putStrLn ("[Error: " <> err <> "]")
```

**Backend Configuration:**
```haskell
-- Backend type includes authentication flag
data Backend
  = BackendOpenAI
      { backendApiKey :: Text
      , backendBaseUrl :: Maybe Text
      , backendRequiresAuth :: Bool  -- True for OpenAI, False for llama-server
      }
  | BackendGemini { ... , backendRequiresAuth :: Bool }
  | BackendAnthropic { ... , backendRequiresAuth :: Bool }

-- Helper functions handle authentication automatically
llamaServerClient :: Text -> IO Client  -- No auth
geminiClient :: Text -> IO Client       -- Requires auth
```

**Key Features:**
- Tool calls are **automatically buffered** - callback receives complete JSON
- Conduit integration for composable streaming
- **Configurable authentication** - supports both authenticated and non-authenticated endpoints
- Mock server support for testing

### 2.6 Proxy Server Architecture

**Protocol Conversion (OpenAI as Base):**
```
Anthropic Request  ─┐
                    ├─→ OpenAI IR ─→ Backend (via client library)
Gemini Request     ─┘

Backend Response ─→ OpenAI IR ─┬─→ Anthropic Response
                               ├─→ Gemini Response
                               └─→ OpenAI Response
```

**Converter Interface:**
```haskell
class ProtocolConverter a where
  toOpenAI :: a -> OpenAIRequest
  fromOpenAI :: OpenAIChunk -> a

instance ProtocolConverter AnthropicRequest where
  toOpenAI (AnthropicRequest msgs tools) =
    OpenAIRequest
      { messages = map convertMessage msgs
      , tools = map convertTool tools
      }

instance ProtocolConverter GeminiChunk where
  fromOpenAI (OpenAIChunk choices) =
    GeminiResponse
      { candidates = map convertChoice choices
      , usageMetadata = extractUsage choices
      }
```

**Proxy Server Usage:**
```haskell
-- Client connects to proxy
-- curl http://localhost:9000/v1/messages (Anthropic format)

-- Proxy uses client library to call backend
backend <- newClient backendConfig
response <- streamChat backend openAIRequest handleChunk

-- Proxy converts response to Anthropic format
anthropicChunks = map (fromOpenAI @AnthropicChunk) response
```

### 2.7 Mock Server for Testing

**Purpose:** Replay captured test-data responses for deterministic testing.

```haskell
-- Load test data from test-data/openai/streaming_function_calling/
loadTestResponse :: FilePath -> IO [SSEChunk]

-- Mock server endpoint
mockStreamingEndpoint :: Request -> IO Response
mockStreamingEndpoint req = do
  chunks <- loadTestResponse "test-data/openai/streaming_function_calling/sample_response.txt"
  return $ responseStream status200 headers (streamChunks chunks)
```

**Test Data Coverage:**
- ✅ `streaming_function_calling/` - Function calls with incremental JSON
- ✅ `function_calling/` - Non-streaming function calls
- ✅ `streaming/` - Text-only streaming
- ✅ `text/` - Simple text responses
- ⚠️  **Missing:** Vision, audio, errors, multiple parallel tools

### 2.8 Data Collection Process

When test data is insufficient, collect it using:

```bash
# Collect streaming function calling data
./test-data/openai/streaming_function_calling/test.sh > new_sample.txt

# Collect vision data
curl -N http://127.0.0.1:11211/v1/chat/completions \
  -H "Content-Type: application/json" \
  -d @vision_request.json > test-data/openai/vision/sample_response.txt
```

**Required Test Scenarios:**
1. **Vision streaming** - Image analysis with content deltas
2. **Audio streaming** - Audio content parts
3. **Multiple parallel tools** - Multiple `tool_calls[i]` in single response
4. **Error handling** - Rate limits, model errors, invalid requests
5. **Stop sequences** - Responses with `finish_reason: "stop"`
6. **Max tokens** - Responses with `finish_reason: "length"`

---

## 3. Development Guidelines

### 3.1 Coding Standards

1. **Strict Typing**: Use explicit types, avoid `Value` when possible
   ```haskell
   -- Good
   data CoreChatRequest = CoreChatRequest { ... }

   -- Avoid
   type LooseRequest = Value
   ```

2. **Config Driven**: Never hardcode model names or URLs
   ```yaml
   # Use config.yaml
   backends:
     backend-name:
       url: http://...
       model_mapping:
         frontend-model: backend-model
   ```

3. **Error Handling**:
   - Use `Either Text Result` for expected errors
   - Use exceptions for unexpected failures
   - Always emit detailed logs with `trace_id`

4. **Testing**:
   - Run Python SDK tests before pushing
   - Add Haskell unit tests for new features
   - Update test data when adding protocols

### 3.2 Project Structure

```
haskell/louter/
├── src/
│   └── Louter/
│       ├── Client.hs              # Client library API
│       ├── Protocol/              # Protocol converters
│       │   ├── AnthropicConverter.hs
│       │   ├── GeminiConverter.hs
│       │   └── ...
│       ├── Backend/               # Backend conversions
│       │   ├── OpenAIToAnthropic.hs
│       │   └── OpenAIToGemini.hs
│       ├── Streaming/             # Streaming parsers
│       │   └── XMLStreamProcessor.hs
│       └── Types/                 # Type definitions
│           ├── Request.hs
│           ├── Response.hs
│           └── Streaming.hs
├── app/
│   ├── Main.hs                    # Proxy server
│   └── CLI.hs                     # CLI tool
├── test/
│   └── Spec.hs                    # Haskell tests
└── examples/
    └── ...                        # Example code
```

### 3.3 Adding New Protocols

When adding support for a new LLM API:

1. Create converter in `src/Louter/Protocol/NewAPIConverter.hs`
2. Implement bidirectional conversion (to/from OpenAI IR)
3. Add streaming support
4. Write unit tests
5. Add Python SDK integration tests
6. Document in README and examples

---

## 4. Commands

### Build & Run

```bash
# Build library and executables
stack build
# Or with Cabal
cabal build

# Run Proxy Server
stack run louter-server -- --config config.yaml --port 9000

# Run Mock Server (for testing without real API)
stack run openai-mock -- --test-data ./test-data --port 11211
```

### Testing

```bash
# Haskell unit tests
cd haskell/louter
stack test

# Python SDK integration tests
cd tests
python3 -m pytest test_openai_streaming.py
python3 -m pytest test_anthropic_streaming.py
python3 -m pytest test_gemini_tool_calling.py

# Run all tests
python3 -m pytest test_*.py
```

### Development

```bash
# Watch mode (rebuild on changes)
stack build --file-watch

# Run with debug logging
stack run louter-server -- --config config.yaml --port 9000 2>&1 | jq .

# Format code
stack exec -- fourmolu -i $(find src app -name '*.hs')
```

---

## 5. Configuration

### YAML Config Format

```yaml
backends:
  backend-name:
    type: openai | anthropic | gemini
    url: http://...
    requires_auth: true | false
    api_key: "sk-..."  # Optional, required if requires_auth=true
    model_mapping:
      frontend-model: backend-model
    tool_format: json | xml  # Optional, defaults to json
```

### Examples

**Local llama-server (no auth):**
```yaml
backends:
  llama:
    type: openai
    url: http://localhost:11211
    requires_auth: false
    model_mapping:
      gpt-4: qwen/qwen2.5-vl-7b
```

**Cloud API (with auth):**
```yaml
backends:
  openai:
    type: openai
    url: https://api.openai.com
    requires_auth: true
    api_key: "${OPENAI_API_KEY}"
```

---

## 6. Troubleshooting

### Common Issues

**"Connection refused"**
- Ensure backend is running: `curl http://localhost:11211/v1/models`

**"Invalid API key"**
- Check API key in config
- Verify `requires_auth` is set correctly

**"Model not found"**
- Check `model_mapping` in config
- Frontend model name must map to backend model

**Build errors**
- Update package index: `cabal update`
- Clean build: `stack clean && stack build`

### Debug Logging

The proxy emits JSON-line logs:

```bash
stack run louter-server -- --config config.yaml --port 9000 2>&1 | tee proxy.log

# Watch logs
tail -f proxy.log | jq .
```

Log format:
```json
{
  "trace_id": "trace-abc123",
  "event": "request_received",
  "details": { ... }
}
```

---

## 7. Resources

- [README.md](README.md) - Project overview
- [INSTALLATION.md](INSTALLATION.md) - Setup guide
- [CONTRIBUTING.md](CONTRIBUTING.md) - Development guidelines
- [docs/haskell/](docs/haskell/) - Detailed guides
  - [GETTING_STARTED.md](docs/haskell/GETTING_STARTED.md)
  - [LIBRARY_API.md](docs/haskell/LIBRARY_API.md)
