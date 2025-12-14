# Louter Library API Reference

Complete API reference for using Louter as a Haskell library.

## Table of Contents

- [Client Creation](#client-creation)
- [Request Types](#request-types)
- [Response Types](#response-types)
- [Streaming](#streaming)
- [Error Handling](#error-handling)
- [Advanced Usage](#advanced-usage)

---

## Client Creation

### Module: `Louter.Client`

The main module for client operations.

#### Creating Clients

**For local models (no authentication):**

```haskell
import Louter.Client.OpenAI (llamaServerClient)

client <- llamaServerClient "http://localhost:11211"
```

**For cloud APIs (with authentication):**

```haskell
import Louter.Client.OpenAI (openAIClient, openAIClientWithUrl)
import Louter.Client.Anthropic (anthropicClient, anthropicClientWithUrl)
import Louter.Client.Gemini (geminiClient, geminiClientWithUrl)

-- OpenAI
openAIClient <- openAIClient "sk-..."
customOpenAI <- openAIClientWithUrl "sk-..." "https://custom-endpoint.com"

-- Anthropic
anthropicClient <- anthropicClient "sk-ant-..."
customAnthropic <- anthropicClientWithUrl "sk-ant-..." "https://custom-endpoint.com"

-- Gemini
geminiClient <- geminiClient "your-api-key"
customGemini <- geminiClientWithUrl "your-api-key" "https://custom-endpoint.com"
```

#### Type Signatures

```haskell
-- | Client type
data Client

-- | Backend configuration
data Backend
  = BackendOpenAI
      { backendApiKey :: Text
      , backendBaseUrl :: Maybe Text
      , backendRequiresAuth :: Bool
      }
  | BackendAnthropic { ... }
  | BackendGemini { ... }

-- | Create a new client
newClient :: Backend -> IO Client

-- Helper functions
llamaServerClient :: Text -> IO Client
openAIClient :: Text -> IO Client
anthropicClient :: Text -> IO Client
geminiClient :: Text -> IO Client
```

---

## Request Types

### Module: `Louter.Types.Request`

#### ChatRequest

Main request type for chat completions:

```haskell
data ChatRequest = ChatRequest
  { reqModel :: Text
  , reqMessages :: [Message]
  , reqTools :: [Tool]
  , reqToolChoice :: ToolChoice
  , reqTemperature :: Maybe Float
  , reqMaxTokens :: Maybe Int
  , reqTopP :: Maybe Float
  , reqStream :: Bool
  }
```

**Create with defaults:**

```haskell
import Louter.Types.Request (defaultChatRequest, Message(..), MessageRole(..))

request = defaultChatRequest "gpt-4"
  [ Message RoleUser "Hello, world!" ]
```

**With options:**

```haskell
request = (defaultChatRequest "gpt-4" messages)
  { reqTemperature = Just 0.7
  , reqMaxTokens = Just 1000
  , reqStream = True
  }
```

#### Message

```haskell
data Message = Message
  { msgRole :: MessageRole
  , msgContent :: Text
  }

data MessageRole
  = RoleSystem
  | RoleUser
  | RoleAssistant
  | RoleTool

-- Examples
systemMsg = Message RoleSystem "You are a helpful assistant."
userMsg = Message RoleUser "What is 2+2?"
assistantMsg = Message RoleAssistant "2+2 equals 4."
```

#### Tool (Function Calling)

```haskell
data Tool = Tool
  { toolName :: Text
  , toolDescription :: Maybe Text
  , toolParameters :: Value  -- JSON schema
  }

data ToolChoice
  = ToolChoiceAuto
  | ToolChoiceNone
  | ToolChoiceRequired
  | ToolChoiceFunction Text  -- Specific function name

-- Example
weatherTool = Tool
  { toolName = "get_weather"
  , toolDescription = Just "Get current weather"
  , toolParameters = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "location" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("City name" :: Text)
              ]
          ]
      , "required" .= (["location"] :: [Text])
      ]
  }
```

#### Content Parts (Multimodal)

```haskell
data ContentPart
  = TextPart Text
  | ImagePart
      { imageMediaType :: Text
      , imageData :: Text  -- Base64 encoded
      }
  | ToolResultPart
      { toolCallId :: Text
      , toolResult :: Text
      }

-- Example: Message with image
imageMessage = Message
  { msgRole = RoleUser
  , msgContent = ...  -- Use ContentPart for multimodal
  }
```

---

## Response Types

### Module: `Louter.Types.Response`

#### ChatResponse

```haskell
data ChatResponse = ChatResponse
  { respId :: Text
  , respModel :: Text
  , respChoices :: [Choice]
  , respUsage :: Maybe Usage
  }

data Choice = Choice
  { choiceIndex :: Int
  , choiceMessage :: Text
  , choiceFinishReason :: Maybe FinishReason
  }

data FinishReason
  = FinishStop
  | FinishLength
  | FinishToolCalls
  | FinishContentFilter

data Usage = Usage
  { usagePromptTokens :: Int
  , usageCompletionTokens :: Int
  , usageTotalTokens :: Int
  }
```

**Usage:**

```haskell
result <- chatCompletion client request
case result of
  Left err -> handleError err
  Right response -> do
    let firstChoice = head (respChoices response)
    let answer = choiceMessage firstChoice
    putStrLn answer
```

---

## Streaming

### Module: `Louter.Types.Streaming`

#### StreamEvent

```haskell
data StreamEvent
  = StreamContent Text           -- Text delta
  | StreamReasoning Text         -- Thinking/reasoning tokens
  | StreamToolCall ToolCall      -- Complete tool call (already buffered!)
  | StreamFinish FinishReason    -- Stream end
  | StreamError Text             -- Error occurred

data ToolCall = ToolCall
  { tcId :: Text
  , tcName :: Text
  , tcArguments :: Value  -- Parsed JSON
  }
```

#### Streaming API

**With callback:**

```haskell
import Louter.Types.Streaming (StreamEvent(..), streamChatWithCallback)

streamChatWithCallback :: Client -> ChatRequest -> StreamCallback -> IO ()
type StreamCallback = StreamEvent -> IO ()

-- Usage
main = do
  client <- llamaServerClient "http://localhost:11211"
  let request = (defaultChatRequest "gpt-4" messages) { reqStream = True }

  streamChatWithCallback client request $ \event -> case event of
    StreamContent txt -> putStr txt >> hFlush stdout
    StreamReasoning txt -> putStrLn $ "[Thinking: " <> txt <> "]"
    StreamToolCall call -> do
      putStrLn $ "\n[Tool: " <> tcName call <> "]"
      print (tcArguments call)
    StreamFinish reason -> putStrLn "\n[Done]"
    StreamError err -> putStrLn $ "\n[Error: " <> err <> "]"
```

**With Conduit:**

```haskell
import Conduit
import Louter.Types.Streaming (streamChat)

streamChat :: Client -> ChatRequest -> ConduitT () StreamEvent IO ()

-- Usage
main = do
  client <- llamaServerClient "http://localhost:11211"
  let request = ...

  runConduit $
    streamChat client request
    .| filterC isContent
    .| mapC extractText
    .| stdoutC

  where
    isContent (StreamContent _) = True
    isContent _ = False

    extractText (StreamContent txt) = txt
    extractText _ = ""
```

---

## Error Handling

All API calls return `Either Text Result`:

```haskell
chatCompletion :: Client -> ChatRequest -> IO (Either Text ChatResponse)
```

**Best practices:**

```haskell
-- Pattern match on Either
result <- chatCompletion client request
case result of
  Left err -> do
    -- Handle error (connection failed, invalid request, etc.)
    logError err
    returnDefaultValue

  Right response -> do
    -- Process successful response
    processResponse response

-- Or use do-notation with ExceptT
import Control.Monad.Except

makeRequest :: ExceptT Text IO ChatResponse
makeRequest = ExceptT $ chatCompletion client request

-- Chain multiple requests
do
  response1 <- makeRequest
  response2 <- makeOtherRequest
  pure (response1, response2)
```

**Common errors:**

- `"Connection refused"` - Backend not running
- `"Invalid API key"` - Authentication failed
- `"Model not found"` - Model doesn't exist on backend
- `"Rate limit exceeded"` - API quota exceeded

---

## Advanced Usage

### Custom Backend

```haskell
import Louter.Client (Backend(..), newClient)

customBackend = BackendOpenAI
  { backendApiKey = "your-key"
  , backendBaseUrl = Just "https://custom-api.com"
  , backendRequiresAuth = True
  }

client <- newClient customBackend
```

### Multimodal Requests (Vision)

```haskell
-- Note: Current API uses Text for content
-- For vision, encode image as data URL

let imageDataUrl = "data:image/png;base64,iVBORw0KGgo..."
let message = Message RoleUser "What's in this image?"

-- Implementation depends on ContentPart support
-- Check current API version
```

### Parallel Requests

```haskell
import Control.Concurrent.Async

-- Make multiple requests concurrently
results <- concurrently
  (chatCompletion client request1)
  (chatCompletion client request2)

case results of
  (Right resp1, Right resp2) -> ...
  _ -> handleError
```

### Request Timeouts

```haskell
import System.Timeout

-- Timeout after 30 seconds
result <- timeout (30 * 1000000) (chatCompletion client request)
case result of
  Nothing -> putStrLn "Request timed out"
  Just (Right response) -> processResponse response
  Just (Left err) -> handleError err
```

### Retries with Exponential Backoff

```haskell
import Control.Retry

retryPolicy = exponentialBackoff 1000000 <> limitRetries 3

result <- retrying retryPolicy shouldRetry $ \_ ->
  chatCompletion client request

  where
    shouldRetry _ (Left err)
      | "rate limit" `isInfixOf` err = return True
      | "timeout" `isInfixOf` err = return True
      | otherwise = return False
    shouldRetry _ (Right _) = return False
```

---

## Complete Example

Putting it all together:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Louter.Client
import Louter.Client.OpenAI (llamaServerClient)
import Louter.Types.Request
import Louter.Types.Response
import Louter.Types.Streaming
import Control.Monad (when)
import System.IO (hFlush, stdout)
import Data.Aeson (object, (.=))

main :: IO ()
main = do
  -- Create client
  client <- llamaServerClient "http://localhost:11211"

  -- Define a tool
  let weatherTool = Tool
        { toolName = "get_weather"
        , toolDescription = Just "Get current weather"
        , toolParameters = object
            [ "type" .= ("object" :: Text)
            , "properties" .= object
                [ "location" .= object ["type" .= ("string" :: Text)] ]
            , "required" .= (["location"] :: [Text])
            ]
        }

  -- Create request
  let request = (defaultChatRequest "gpt-4"
        [ Message RoleSystem "You are a helpful assistant."
        , Message RoleUser "What's the weather in Tokyo?"
        ])
        { reqTools = [weatherTool]
        , reqToolChoice = ToolChoiceAuto
        , reqTemperature = Just 0.7
        , reqMaxTokens = Just 500
        , reqStream = True
        }

  -- Stream response
  putStrLn "Assistant: "
  streamChatWithCallback client request handleEvent
  putStrLn ""

handleEvent :: StreamEvent -> IO ()
handleEvent (StreamContent txt) = putStr txt >> hFlush stdout
handleEvent (StreamReasoning txt) = putStrLn $ "\n[Thinking: " <> txt <> "]"
handleEvent (StreamToolCall call) = do
  putStrLn $ "\n[Calling: " <> tcName call <> "]"
  putStrLn $ "Arguments: " <> show (tcArguments call)
handleEvent (StreamFinish reason) = putStrLn $ "\n[Finished: " <> show reason <> "]"
handleEvent (StreamError err) = putStrLn $ "\n[Error: " <> err <> "]"
```

---

## API Stability

**Current Version:** 0.5.0

**Stability Guarantees:**
- Core API (`Louter.Client`, `Louter.Types.*`) - Stable
- Protocol converters (`Louter.Protocol.*`) - Internal, may change
- Streaming API - Stable

**Breaking Changes:**
Will be announced with major version bumps (1.0.0 → 2.0.0)

---

## See Also

- [Getting Started Guide](GETTING_STARTED.md) - Quickstart tutorial
- [Proxy Setup Guide](PROXY_SETUP.md) - Running as a proxy server
- [CLAUDE.md](../../CLAUDE.md) - Architecture documentation
- [Examples](../../examples/) - Complete code examples

## Support

- 📖 [Documentation](../)
- 🐛 [Report Issues](https://github.com/yourusername/louter/issues)
- 💬 [Ask Questions](https://github.com/yourusername/louter/discussions)
