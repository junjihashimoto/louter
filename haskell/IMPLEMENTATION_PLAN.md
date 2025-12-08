# Haskell Louter Implementation Plan

Based on the successful test results (13/13 OpenAI tests passed against llama-server), this document outlines what needs to be implemented in the Haskell proxy to achieve the same functionality.

## Current Status

### ✅ Completed Components

1. **Core Type System** (`Louter.Types.*`)
   - `ChatRequest` - Protocol-agnostic request IR
   - `ChatResponse` - Protocol-agnostic response IR
   - `StreamEvent` - Unified streaming events
   - `DeltaType` - Delta classification (reasoning, content, tool_calls)
   - `ToolCall` - Complete tool call representation

2. **Streaming Infrastructure**
   - `Louter.Streaming.Parser` - Attoparsec-based SSE parser
   - `Louter.Streaming.Classifier` - Delta type identification
   - `Louter.Streaming.Processor` - Stateful buffering with State monad
   - Smart buffering: reasoning/content stream immediately, tool_calls buffer until complete

3. **Protocol Converters**
   - `Louter.Protocol.OpenAI` - OpenAI ↔ IR conversion
   - `Louter.Protocol.Gemini` - Gemini ↔ IR conversion (includes Protobuf Schema)
   - `Louter.Protocol.Anthropic` - Anthropic ↔ IR conversion

4. **Client Library**
   - `Louter.Client` - High-level API for library usage
   - `Louter.Client.OpenAI` - Helper functions including `llamaServerClient`
   - `Louter.Client.Gemini` - Helper functions
   - `Louter.Client.Anthropic` - Helper functions
   - Configurable authentication (`backendRequiresAuth` flag)

5. **Test Infrastructure**
   - Mock server (`openai-mock`) for replaying test data
   - Test data in `test-data/openai/`

### ⚠️ Placeholder/Incomplete

1. **Proxy Server** (`louter/app/Main.hs`)
   - Currently just shows usage instructions
   - Does not route requests
   - Does not handle protocol conversion

## Implementation Roadmap

### Phase 1: Basic HTTP Server (Core Proxy)

**Goal:** Accept HTTP requests and forward to backend

**Tasks:**
1. Implement HTTP server using `warp` or `servant`
2. Parse incoming requests based on path/headers to determine protocol
3. Forward requests to configured backend
4. Return responses to client

**Files to modify:**
- `louter/app/Main.hs` - Main server implementation
- `louter/louter.cabal` - Add HTTP server dependencies

**Dependencies needed:**
```yaml
dependencies:
  - warp >= 3.3
  - wai >= 3.2
  - servant-server >= 0.19  # OR http-types >= 0.12
  - streaming-commons >= 0.2  # For SSE streaming
```

**Minimal Implementation:**

```haskell
-- louter/app/Main.hs
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL

app :: Application
app req respond = do
  -- 1. Determine protocol from path
  let path = rawPathInfo req
      protocol = determineProtocol path

  -- 2. Read request body
  body <- strictRequestBody req

  -- 3. Forward to backend (simplified)
  response <- forwardToBackend protocol body

  -- 4. Return response
  respond $ responseLBS status200 [("Content-Type", "application/json")] response

main :: IO ()
main = do
  putStrLn "Louter proxy starting on port 9000..."
  run 9000 app
```

### Phase 2: Protocol Detection & Routing

**Goal:** Correctly identify incoming protocol and route appropriately

**Protocol Detection Rules:**
```haskell
determineProtocol :: ByteString -> Protocol
determineProtocol path
  | "/v1/messages" `isPrefixOf` path = ProtocolAnthropic
  | "/v1beta/models/" `isPrefixOf` path = ProtocolGemini
  | "/v1/chat/completions" `isPrefixOf` path = ProtocolOpenAI
  | otherwise = ProtocolOpenAI  -- Default
```

**Endpoint Mapping:**
```
OpenAI:     POST /v1/chat/completions
Anthropic:  POST /v1/messages
Gemini:     POST /v1beta/models/{model}:generateContent
            POST /v1beta/models/{model}:streamGenerateContent
```

### Phase 3: Request/Response Conversion

**Goal:** Convert between protocols using existing converters

**Request Flow:**
```
Client Request (Protocol A)
  → Parse to Protocol-specific type
  → Convert to Internal Representation (ChatRequest)
  → Convert to Backend Protocol (Protocol B)
  → Send to Backend
```

**Response Flow:**
```
Backend Response (Protocol B)
  → Parse SSE chunks
  → Classify deltas (reasoning, content, tool_calls)
  → Process through State monad (buffer tool_calls)
  → Convert to StreamEvents
  → Convert to Client Protocol (Protocol A)
  → Send to Client
```

**Implementation:**

```haskell
-- Use existing converters
import Louter.Protocol.OpenAI (parseOpenAIChunk, openAIRequestToIR)
import Louter.Protocol.Gemini (parseGeminiChunk, geminiRequestToIR)
import Louter.Protocol.Anthropic (parseAnthropicChunk, anthropicRequestToIR)

handleRequest :: Protocol -> Protocol -> ByteString -> IO ByteString
handleRequest frontendProto backendProto reqBody = do
  -- 1. Parse frontend request to IR
  irRequest <- case frontendProto of
    ProtocolOpenAI -> openAIRequestToIR reqBody
    ProtocolGemini -> geminiRequestToIR reqBody
    ProtocolAnthropic -> anthropicRequestToIR reqBody

  -- 2. Convert IR to backend format
  backendRequest <- case backendProto of
    ProtocolOpenAI -> irToOpenAIRequest irRequest
    ProtocolGemini -> irToGeminiRequest irRequest
    ProtocolAnthropic -> irToAnthropicRequest irRequest

  -- 3. Send to backend and get response
  backendResponse <- sendToBackend backendRequest

  -- 4. Convert backend response back to frontend format
  -- (Similar conversion in reverse)

  return frontendResponse
```

### Phase 4: Streaming Support

**Goal:** Handle Server-Sent Events (SSE) streaming

**Key Requirements:**
1. Parse SSE chunks from backend (`Louter.Streaming.Parser`)
2. Classify deltas (`Louter.Streaming.Classifier`)
3. Buffer tool calls until complete (`Louter.Streaming.Processor`)
4. Emit frontend-protocol SSE chunks

**Implementation:**

```haskell
import Louter.Streaming.Parser (parseSSE)
import Louter.Streaming.Classifier (classifyDelta)
import Louter.Streaming.Processor (processChunk, StreamState, emptyStreamState)
import Control.Monad.State

handleStreamingRequest :: Protocol -> Protocol -> ChatRequest -> IO ()
handleStreamingRequest frontendProto backendProto irRequest = do
  -- 1. Connect to backend streaming endpoint
  backendStream <- connectToBackend backendProto irRequest

  -- 2. Process stream with state
  let initialState = emptyStreamState "stream-id"

  processStream backendStream initialState
  where
    processStream stream state = do
      maybeChunk <- readChunk stream
      case maybeChunk of
        Nothing -> return ()  -- Stream done
        Just chunk -> do
          -- Parse SSE
          case parseSSE chunk of
            Right sseChunk -> do
              -- Classify delta
              case classifyDelta sseChunk of
                Right deltaType -> do
                  -- Process with state monad
                  let (events, newState) = runState (processChunk deltaType) state

                  -- Convert and emit events
                  mapM_ (emitEvent frontendProto) events

                  -- Continue with new state
                  processStream stream newState
                Left err -> handleError err
            Left err -> handleError err
```

### Phase 5: Configuration & Backend Management

**Goal:** Read config, manage multiple backends, route requests

**Config Format (TOML):**
```toml
[server]
port = 9000
host = "0.0.0.0"

[backends.openai]
url = "https://api.openai.com"
protocol = "openai"
requires_auth = true
api_key = "${OPENAI_API_KEY}"

[backends.llama]
url = "http://localhost:11211"
protocol = "openai"
requires_auth = false

[backends.gemini]
url = "https://generativelanguage.googleapis.com"
protocol = "gemini"
requires_auth = true
api_key = "${GOOGLE_API_KEY}"

[routing]
# Map frontend protocol → backend
openai = "llama"      # OpenAI requests → llama-server
anthropic = "llama"   # Anthropic requests → llama-server
gemini = "gemini"     # Gemini requests → Gemini API
```

**Implementation:**
```haskell
import Data.Aeson (FromJSON)
import qualified Data.Yaml as Yaml

data Config = Config
  { configServer :: ServerConfig
  , configBackends :: Map Text BackendConfig
  , configRouting :: Map Protocol Text  -- protocol → backend name
  }

data BackendConfig = BackendConfig
  { backendUrl :: Text
  , backendProtocol :: Protocol
  , backendRequiresAuth :: Bool
  , backendApiKey :: Maybe Text
  }

loadConfig :: FilePath -> IO Config
readBackend :: Config -> Protocol -> Maybe BackendConfig
```

### Phase 6: Error Handling & Logging

**Goal:** Proper error handling and request/response logging

**Tasks:**
1. Handle backend connection errors
2. Handle protocol conversion errors
3. Log requests and responses (with `trace_id`)
4. Implement `/health` endpoint
5. Implement `/diagnostics` endpoint (show config, backends, stats)

### Phase 7: Testing & Validation

**Goal:** Pass all Python SDK test suites

**Testing Strategy:**

1. **Unit Tests** (Haskell)
   - Test SSE parser with various inputs
   - Test delta classifier
   - Test state monad processor
   - Test protocol converters

2. **Integration Tests** (Python SDK)
   ```bash
   # Start Haskell proxy
   cabal run louter-server -- -c config.toml

   # Run OpenAI tests
   OPENAI_BASE_URL="http://localhost:9000" python3 tests/test_openai_streaming.py

   # Run Anthropic tests
   ANTHROPIC_BASE_URL="http://localhost:9000" python3 tests/test_anthropic_streaming.py

   # Run Gemini tests
   GOOGLE_GEMINI_BASE_URL="http://localhost:9000" python3 tests/test_gemini_streaming.py
   ```

3. **Success Criteria:**
   - All 13 OpenAI tests pass
   - All 15 Anthropic tests pass
   - All 10 Gemini tests pass
   - **Total: 38/38 tests passing**

## Development Priority

### Minimum Viable Proxy (MVP)

**Goal:** OpenAI → OpenAI passthrough (simplest case)

**Scope:**
- Accept OpenAI requests on port 9000
- Forward to llama-server on port 11211
- Handle streaming responses
- Pass all 13 OpenAI tests

**Timeline:** ~1-2 days

**Files to implement:**
1. `louter/app/Main.hs` - HTTP server + routing
2. Add streaming support using existing parsers

### Full Protocol Converter

**Goal:** All three protocols with cross-protocol conversion

**Scope:**
- Accept OpenAI/Anthropic/Gemini requests
- Convert between any protocols
- Handle all streaming cases
- Pass all 38 tests

**Timeline:** ~3-5 days

**Files to implement:**
1. Complete proxy server
2. Config file loader
3. Backend connection pool
4. Error handling
5. Logging/diagnostics

## Testing Checklist

After each phase, verify:

- [ ] Phase 1: Can accept HTTP requests and return responses
- [ ] Phase 2: Correctly routes based on URL path
- [ ] Phase 3: Converts requests between protocols
- [ ] Phase 4: Handles streaming correctly (buffers tool calls)
- [ ] Phase 5: Reads config, manages multiple backends
- [ ] Phase 6: Errors are handled gracefully, logs are useful
- [ ] Phase 7: All 38 Python SDK tests pass

## Dependencies to Add

```yaml
# louter.cabal
dependencies:
  # HTTP Server
  - warp >= 3.3
  - wai >= 3.2
  - http-types >= 0.12
  - streaming-commons >= 0.2

  # Config
  - yaml >= 0.11
  - toml-reader >= 0.2  # OR aeson >= 2.0

  # Logging
  - fast-logger >= 3.0

  # Already have:
  - http-client >= 0.7
  - http-client-tls >= 0.3
  - conduit >= 1.3
  - attoparsec >= 0.14
  - aeson >= 2.0
```

## Next Steps

1. **Immediate:** Implement Phase 1 (Basic HTTP Server)
   - Get a simple proxy accepting and forwarding requests
   - Test with curl

2. **Short-term:** Implement Phase 2-4 (MVP)
   - OpenAI passthrough working
   - Pass all 13 OpenAI tests

3. **Medium-term:** Implement Phase 5-7 (Full Proxy)
   - All protocols working
   - Pass all 38 tests

4. **Polish:** Performance, error handling, documentation

## Success Metrics

| Metric | Target |
|--------|--------|
| OpenAI Tests | 13/13 passing |
| Anthropic Tests | 15/15 passing |
| Gemini Tests | 10/10 passing |
| Latency Overhead | < 10ms |
| Memory Usage | < 100MB |
| Uptime | 99.9% |

## References

- Test suites: `tests/test_*.py`
- Existing library code: `louter/src/Louter/`
- CLAUDE.md Section 6 for architecture details
- Python SDK test results showing what to implement
