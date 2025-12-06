# CLAUDE.md

> **Note for Claude:** This file is the **Single Source of Truth** for the `louter` project architecture, testing strategy, and coding standards. Read this before modifying any code.

## 1. Project Overview

**Louter** is a multi-protocol coding agent proxy designed to unify communication between various frontend agents and LLM backends.

*   **Supported Coding Agents (Frontends):**
    *   Claude Code (via Anthropic API)
    *   Gemini CLI (via Gemini API)
    *   Codex / Cursor / Generic Tools (via OpenAI API)
*   **Supported LLM Backends:**
    *   OpenAI API, Gemini API
    *   Self-hosted/OSS models: gpt-oss, Qwen (XML tools), Gemma3
*   **Key Feature:** Protocol translation (Requests, SSE Streams, Tool Calling, Vision) via a strictly typed Internal Representation (IR).

---

## 2. Architecture & Core IR

We avoid "intermediate protocol models" that are too loose. Instead, we use a **Rust-native Core IR** heavily inspired by OpenAI's Chat Completions structure but owned by this project.

### 2.1 Core Data Structures (The Contract)

All Frontend and Backend adapters must convert TO/FROM these structures. Do not bypass them.

```rust
// Core Request (Input)
pub struct CoreChatRequest {
    pub model: String, // Abstract model name (resolved via config)
    pub messages: Vec<CoreMessage>,
    pub tools: Vec<CoreTool>,
    pub tool_choice: CoreToolChoice,
    pub temperature: Option<f32>,
    pub max_tokens: Option<u32>,
    pub top_p: Option<f32>,
    pub stream: bool,
    pub response_format: Option<CoreResponseFormat>, // json_object, text
    pub vision: Option<CoreVisionOptions>,
    // Escape hatch for protocol-specific metadata (e.g., Anthropic beta headers)
    pub metadata: serde_json::Value,
}

pub struct CoreMessage {
    pub role: CoreRole, // System, User, Assistant, Tool
    pub content: Vec<CoreContentPart>,
}

pub enum CoreContentPart {
    Text(String),
    Image {
        media_type: String,
        data: String, // base64
    },
    ToolResult {
        tool_call_id: String,
        content: String,
    },
    // ... potentially others
}

// Core Streaming Events (Output)
// Must handle both text and tool calls uniformly across protocols.
pub enum CoreStreamEvent {
    TextDelta {
        index: u32,
        content: String,
    },
    ToolCallDelta {
        index: u32,
        delta: CoreToolCallDelta,
    },
    Usage(CoreUsage),
    FinalResponse(CoreChatResponse), // For non-streaming fallback
    Error(CoreError),
}

pub enum CoreToolCallDelta {
    Start { id: String, name: String },
    ArgumentsText { id: String, fragment: String }, // Raw accumulation
    ArgumentsJson { id: String, arguments: serde_json::Value }, // Parsed (if available)
    Done { id: String },
}
```

### 2.2 Adapter Traits

Frontends and Backends are decoupled.

```rust
#[async_trait::async_trait]
pub trait FrontendAdapter: Send + Sync {
    fn protocol_name(&self) -> &'static str;
    
    /// Parse incoming HTTP request to Core IR
    async fn parse_request(&self, req: &HttpRequest) -> Result<CoreChatRequest, CoreError>;
    
    /// Encode Core IR event to protocol-specific SSE event or JSON
    fn encode_stream_event(&self, event: CoreStreamEvent) -> Option<FrontendEncodedChunk>;
}

#[async_trait::async_trait]
pub trait BackendAdapter: Send + Sync {
    fn backend_name(&self) -> &'static str;
    
    /// Build backend-specific HTTP request from Core IR
    fn build_http_request(&self, core: &CoreChatRequest, target_model: &str) -> Result<BackendHttpRequest, CoreError>;
    
    /// Decode backend SSE chunk to Core IR events (Stateful)
    fn decode_stream_chunk(&mut self, chunk: BackendSseChunk) -> Result<Vec<CoreStreamEvent>, CoreError>;
}
```

---

## 3. Implementation Details

### 3.1 Function Calling & XML Support (Qwen)

Some backends (e.g., Qwen) use XML for function calling. The Backend Adapter must handle the conversion to OpenAI-style JSON `tools`.

*   **Requirement:** The Proxy must be stateful (buffer tokens) to parse XML cleanly during streaming.
*   **Qwen State Machine Logic:**

```rust
enum QwenMode {
    NormalText,
    InToolCall, // Buffering <tool_call>...</tool_call>
}

struct QwenXmlState {
    mode: QwenMode,
    xml_buffer: String,
}

// Logic:
// 1. Detect "<tool_call>" -> Switch to InToolCall.
// 2. Buffer content. Do NOT emit text to frontend.
// 3. Detect "</tool_call>" -> Parse XML -> Emit CoreToolCallDelta (Start -> Args -> Done).
// 4. Reset to NormalText.
```

### 3.2 Parallel Tool Execution

*   **Protocol Support:** OpenAI, Anthropic, and Gemini all support defining multiple tools in a single response turn.
*   **Constraint:** `CoreChatRequest` / `CoreStreamEvent` must always support `Vec<ToolCall>`.
*   **Execution:** The proxy does not execute tools; it forwards the request. The *Coding Agent* (client) is responsible for executing tools (potentially in parallel) and returning results.

---

## 4. Testing Strategy

Since testing with real agents (UI) is flaky and expensive, we use a 3-layer approach.

### 4.1 Layer 1: Protocol Contract (SDK-based)

Use official SDKs (Anthropic Python SDK, OpenAI Node SDK, etc.) to simulate agents.

*   **Goal:** Validate protocol compliance.
*   **Method:** Send standard requests (Text, Tools, Vision) via SDK -> Proxy -> Mock/Real Backend.
*   **Success:** SDK receives correct response without error.

### 4.2 Layer 2: Record & Replay (Baseline vs. Proxy)

To isolate bugs, we use a "Recording" mechanism with two modes:

1.  **Baseline Mode (Pass-through):**
    *   Client -> Louter (Pass-through) -> Real API.
    *   Record: Raw Request/Response & Normalized Core IR.
2.  **Proxy Mode (Transformation):**
    *   Client -> Louter (Transform) -> Real Backend.
    *   Record: Raw Request/Response & Normalized Core IR.

**Analysis:**
Compare the `CoreChatRequest` and `final_core_response` traces.
*   If Baseline works but Proxy fails -> **Proxy Bug**.
*   If both fail -> **Agent/Backend Issue**.

**Trace JSON Format:**
```json
{
  "trace_id": "uuid",
  "frontend": {
    "protocol": "anthropic",
    "core_request": { ... } // Normalized
  },
  "backend": {
    "provider": "qwen",
    "core_events": [ ... ],
    "final_response": { ... }
  },
  "status": "ok"
}
```

### 4.3 Layer 3: E2E Smoke Tests
Only occasionally run real Claude Code / Gemini CLI to verify end-to-end integration.

---

## 5. Development Guidelines

1.  **Strict Typing:** Do not use `serde_json::Value` loosely. Use the `CoreChatRequest` structs.
2.  **Config Driven:** Never hardcode model names (e.g., "claude-3-5-sonnet"). Use `config.toml` mappings.
3.  **Error Handling:**
    *   UI errors in agents are hard to debug.
    *   Always emit detailed logs with `trace_id`.
    *   Use the Diagnostics Endpoint: `curl http://localhost:8080/api/diagnostics`.
4.  **Testing:**
    *   Before pushing, run the SDK simulation suite.
    *   If adding a new protocol, add a corresponding fixture in `tests/fixtures`.

## 6. Haskell Implementation Architecture

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
   stack run louter-server -- --config config.toml --port 9000
   ```

**Core Components:**
- Protocol converters (OpenAI ↔ Gemini ↔ Anthropic)
- SSE parser with attoparsec
- Delta tokenizer/classifier
- Stateful function call buffering
- Mock server for testing (`openai-mock`)

### 6.1 SSE Parsing with Attoparsec

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

### 6.2 Delta Type Classification

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

### 6.3 Buffering Strategy

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

### 6.4 JSON Completeness Detection

```haskell
isCompleteJSON :: Text -> Bool
isCompleteJSON txt =
  let trimmed = T.strip txt
  in not (T.null trimmed)
     && T.head trimmed == '{'
     && T.last trimmed == '}'
     && isRight (eitherDecode (encodeUtf8 txt) :: Either String Value)
```

### 6.5 Client Library API

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

### 6.6 Proxy Server Architecture

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

### 6.7 Mock Server for Testing

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

### 6.8 Data Collection Process

When test data is insufficient, collect it using:

```bash
# Collect streaming function calling data
./test-data/openai/streaming_function_calling/test.sh > new_sample.txt

# Collect vision data (when implemented)
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

## 7. Commands

```bash
# === Rust Implementation ===
# Run Rust Proxy Server
cargo run --bin louter -- --config config-dev.toml

# Run Diagnostics (Built-in self-test)
curl -s http://localhost:8080/api/diagnostics | jq .

# Run Protocol Integration Tests
TEST_INTEGRATION=1 cargo test --test integration_protocol

# === Haskell Implementation ===
# Build both library and proxy
stack build

# Run Haskell Mock Server (for testing without API)
stack run openai-mock -- --test-data ./test-data --port 11211

# Run Haskell Proxy Server
stack run louter-hs -- --config config-dev.toml --port 9000

# Use Haskell Client Library (in your app)
import OpenAI.Client
client <- newClient "sk-..."
response <- streamChat client request handleChunk
