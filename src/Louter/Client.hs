{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | High-level client API for Louter
-- This module uses the same proven converters as the proxy server
--
-- Key Design: The client library reuses server-side protocol converters
-- for maximum reliability (no code duplication).
--
-- Example usage:
-- @
--   import Louter.Client
--   import Louter.Client.OpenAI (llamaServerClient)
--
--   main = do
--     client <- llamaServerClient "http://localhost:11211"
--     response <- chatCompletion client $ defaultChatRequest "gpt-oss"
--       [Message RoleUser "Hello!"]
--     print response
-- @
module Louter.Client
  ( -- * Client Configuration
    Client
  , Backend(..)
  , newClient
    -- * Simple API
  , chatCompletion
  , streamChat
    -- * Streaming with Callbacks
  , StreamCallback
  , streamChatWithCallback
    -- * Re-exports from Types
  , module Louter.Types.Request
  , module Louter.Types.Response
  , module Louter.Types.Streaming
  ) where

import Control.Monad (foldM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value(..), encode, eitherDecode, object, (.=))
import qualified Data.Aeson.KeyMap as HM
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.Conduit ((.|), runConduit, ConduitT, yield, await)
import qualified Data.Conduit.List as CL
import qualified Data.HashMap.Strict as HMS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Debug.Trace (trace)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (hContentType, hAuthorization)
import Network.HTTP.Types.Header (RequestHeaders)
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

-- Import server-side converters (proven, tested code)
import Louter.Protocol.AnthropicConverter
import Louter.Protocol.GeminiConverter
import Louter.Types.Request
import Louter.Types.Response
import Louter.Types.Streaming

-- | Check if debug mode is enabled via LOUTER_DEBUG environment variable
-- Set LOUTER_DEBUG=1 or LOUTER_DEBUG=true to enable debug logging
{-# NOINLINE isDebugEnabled #-}
isDebugEnabled :: Bool
isDebugEnabled = unsafePerformIO $ do
  maybeDebug <- lookupEnv "LOUTER_DEBUG"
  pure $ case maybeDebug of
    Just "1" -> True
    Just "true" -> True
    Just "TRUE" -> True
    Just "yes" -> True
    Just "YES" -> True
    _ -> False

-- | Conditional debug trace - only traces if LOUTER_DEBUG is set
debugTrace :: String -> a -> a
debugTrace msg x = if isDebugEnabled then trace msg x else x

-- | Client configuration
data Client = Client
  { clientManager :: Manager
  , clientBackend :: Backend
  }

-- | Backend configuration
data Backend
  = BackendOpenAI
      { backendApiKey :: Text
      , backendBaseUrl :: Maybe Text
      , backendRequiresAuth :: Bool
      }
  | BackendGemini
      { backendApiKey :: Text
      , backendBaseUrl :: Maybe Text
      , backendRequiresAuth :: Bool
      }
  | BackendAnthropic
      { backendApiKey :: Text
      , backendBaseUrl :: Maybe Text
      , backendRequiresAuth :: Bool
      }

-- | Create a new client
newClient :: Backend -> IO Client
newClient backend = do
  manager <- newManager tlsManagerSettings
  pure $ Client manager backend

-- | Non-streaming chat completion
chatCompletion :: Client -> ChatRequest -> IO (Either Text ChatResponse)
chatCompletion client req = do
  let req' = req { reqStream = False }
  result <- makeRequest client req'
  case result of
    Left err -> pure $ Left err
    Right respBody ->
      case parseBackendResponse (clientBackend client) respBody of
        Left err -> pure $ Left $ "Failed to parse response: " <> T.pack err
        Right resp -> pure $ Right resp

-- | Parse SSE stream from HTTP response
parseSSEStream :: Manager -> Request -> ConduitT () StreamEvent IO ()
parseSSEStream manager httpReq = do
  -- We need to lift the withResponse into the Conduit monad
  -- The trick is to use bracket-style resource management
  response <- liftIO $ responseOpen httpReq manager
  parseSSEChunks (responseBody response)
  liftIO $ responseClose response

-- | Parse SSE chunks from body reader
parseSSEChunks :: BodyReader -> ConduitT () StreamEvent IO ()
parseSSEChunks bodyReader = loop BS.empty HMS.empty
  where
    loop acc toolCallState = do
      chunk <- liftIO $ brRead bodyReader
      if BS.null chunk
        then do
          -- End of stream - emit any buffered tool calls
          mapM_ emitToolCall (HMS.toList toolCallState)
        else do
          let combined = acc <> chunk
              lines' = BS8.split '\n' combined
          case lines' of
            [] -> loop BS.empty toolCallState
            [incomplete] -> loop incomplete toolCallState
            _ -> do
              let (completeLines, rest) = (init lines', last lines')
              newState <- foldM processSSELine toolCallState completeLines
              loop rest newState

    processSSELine state line
      | BS.isPrefixOf "data: " line = do
          let jsonText = TE.decodeUtf8 $ BS.drop 6 line
          if jsonText == "[DONE]"
            then do
              -- Emit all buffered tool calls and finish
              mapM_ emitToolCall (HMS.toList state)
              yield (StreamFinish "stop")
              pure HMS.empty
            else case eitherDecode (BL.fromStrict $ TE.encodeUtf8 jsonText) of
              Right (Object chunk) -> processChunk state chunk
              Left err -> do
                yield (StreamError $ "Failed to parse JSON: " <> T.pack err)
                pure state
              _ -> pure state
      | otherwise = pure state

    processChunk state chunk = do
      case HM.lookup "choices" chunk of
        Just (Array choices) | not (V.null choices) -> do
          case V.head choices of
            Object choice -> processChoice state choice
            _ -> pure state
        _ -> pure state

    processChoice state choice = do
      case HM.lookup "delta" choice of
        Just (Object delta) -> do
          -- Handle content
          newState1 <- case HM.lookup "content" delta of
            Just (String content) -> do
              yield (StreamContent content)
              pure state
            _ -> pure state

          -- Handle reasoning (o1 models)
          newState2 <- case HM.lookup "reasoning" delta of
            Just (String reasoning) -> do
              yield (StreamReasoning reasoning)
              pure newState1
            _ -> pure newState1

          -- Handle tool calls (need buffering)
          case HM.lookup "tool_calls" delta of
            Just (Array toolCalls) -> processToolCalls newState2 toolCalls
            _ -> pure newState2
        _ -> pure state

    processToolCalls state toolCalls = do
      V.foldM processToolCallDelta state toolCalls

    processToolCallDelta state (Object tcDelta) = do
      case HM.lookup "index" tcDelta of
        Just (Number idx) -> do
          let index = floor idx :: Int
          let existingTC = HMS.lookupDefault emptyToolCallState index state

          -- Update tool call state
          let updatedTC = existingTC
                { tcId = case HM.lookup "id" tcDelta of
                    Just (String id') -> Just id'
                    _ -> tcId existingTC
                , tcName = case HM.lookup "function" tcDelta >>= getFunctionName of
                    Just name -> Just name
                    _ -> tcName existingTC
                , tcArgs = tcArgs existingTC <> case HM.lookup "function" tcDelta >>= getFunctionArgs of
                    Just args -> args
                    _ -> ""
                }

          -- Check if JSON is complete
          if isCompleteJSON (tcArgs updatedTC) && isJust (tcId updatedTC) && isJust (tcName updatedTC)
            then do
              -- Emit complete tool call
              emitToolCall (index, updatedTC)
              pure $ HMS.delete index state
            else
              pure $ HMS.insert index updatedTC state
        _ -> pure state
    processToolCallDelta state _ = pure state

    getFunctionName (Object func) = case HM.lookup "name" func of
      Just (String name) -> Just name
      _ -> Nothing
    getFunctionName _ = Nothing

    getFunctionArgs (Object func) = case HM.lookup "arguments" func of
      Just (String args) -> Just args
      _ -> Nothing
    getFunctionArgs _ = Nothing

    emptyToolCallState = ToolCallBufferState Nothing Nothing ""

    emitToolCall (_, ToolCallBufferState (Just id') (Just name) args) = do
      case eitherDecode (BL.fromStrict $ TE.encodeUtf8 args) of
        Right argsValue -> yield (StreamToolCall $ ToolCall id' name argsValue)
        Left _ -> pure ()  -- Malformed JSON, skip
    emitToolCall _ = pure ()

    isCompleteJSON txt =
      let trimmed = T.strip txt
      in not (T.null trimmed)
         && T.head trimmed == '{'
         && T.last trimmed == '}'
         && case eitherDecode (BL.fromStrict $ TE.encodeUtf8 txt) of
              Right (_ :: Value) -> True
              Left _ -> False

-- | Tool call buffer state
data ToolCallBufferState = ToolCallBufferState
  { tcId :: Maybe Text
  , tcName :: Maybe Text
  , tcArgs :: Text
  } deriving (Show)

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

-- | Streaming chat with conduit
streamChat :: Client -> ChatRequest -> ConduitT () StreamEvent IO ()
streamChat client req = do
  let req' = req { reqStream = True }
  let backend = clientBackend client

  -- Convert ChatRequest to backend-specific format
  case convertRequestToBackend backend req' of
    Left err -> yield (StreamError err)
    Right (url, body, headers) -> do
      httpReq <- liftIO $ parseRequest (T.unpack url)
      let httpReq' = httpReq
            { method = "POST"
            , requestBody = RequestBodyLBS body
            , requestHeaders = headers
            }

      -- Make streaming request and pipe to parseSSEStream
      parseSSEStream (clientManager client) httpReq'

-- | Type alias for streaming callbacks
type StreamCallback = StreamEvent -> IO ()

-- | Streaming chat with callback
streamChatWithCallback :: Client -> ChatRequest -> StreamCallback -> IO ()
streamChatWithCallback client req callback = do
  runConduit $ streamChat client req .| CL.mapM_ (liftIO . callback)

-- | Make HTTP request to backend
makeRequest :: Client -> ChatRequest -> IO (Either Text BL.ByteString)
makeRequest Client{..} chatReq = do
  let backend = clientBackend

  -- Convert ChatRequest to backend-specific format using server converters
  case convertRequestToBackend backend chatReq of
    Left err -> pure $ Left err
    Right (url, body, headers) -> do
      -- Debug logging (only if LOUTER_DEBUG is set)
      debugTrace ("DEBUG: Request URL: " <> T.unpack url) $ return ()
      debugTrace ("DEBUG: Request headers: " <> show headers) $ return ()
      debugTrace ("DEBUG: Request body (first 500 bytes): " <> show (BL.take 500 body)) $ return ()

      req <- parseRequest (T.unpack url)
      let req' = req
            { method = "POST"
            , requestBody = RequestBodyLBS body
            , requestHeaders = headers
            }

      response <- httpLbs req' clientManager
      debugTrace ("DEBUG: Response status: " <> show (responseStatus response)) $ return ()
      debugTrace ("DEBUG: Response headers: " <> show (responseHeaders response)) $ return ()
      pure $ Right $ responseBody response

-- | Convert ChatRequest to backend-specific format
-- This reuses the server-side converters
convertRequestToBackend :: Backend -> ChatRequest -> Either Text (Text, BL.ByteString, RequestHeaders)
convertRequestToBackend backend chatReq =
  case backend of
    BackendOpenAI{..} -> do
      let url = case backendBaseUrl of
            Just u -> u <> "/v1/chat/completions"
            Nothing -> "https://api.openai.com/v1/chat/completions"

          -- Build OpenAI request format
          messagesJson = map (\msg -> object
            [ "role" .= msgRole msg
            , "content" .= msgContent msg
            ]) (reqMessages chatReq)

          requestBody = encode $ object
            [ "model" .= reqModel chatReq
            , "messages" .= messagesJson
            , "tools" .= if null (reqTools chatReq) then Nothing else Just (reqTools chatReq)
            , "temperature" .= reqTemperature chatReq
            , "max_tokens" .= reqMaxTokens chatReq
            , "stream" .= reqStream chatReq
            ]

          headers = [(hContentType, "application/json")]
                 ++ if backendRequiresAuth
                    then [(hAuthorization, TE.encodeUtf8 $ "Bearer " <> backendApiKey)]
                    else []

      Right (url, requestBody, headers)

    BackendAnthropic{..} -> do
      let url = case backendBaseUrl of
            Just u -> u <> "/v1/messages"
            Nothing -> "https://api.anthropic.com/v1/messages"

      -- Convert to Anthropic format (reverse of what anthropicToOpenAI does)
      let anthropicMessages = map chatMessageToAnthropic (reqMessages chatReq)
          anthropicTools = map chatToolToAnthropic (reqTools chatReq)

          requestBody = encode $ object $
            [ "model" .= reqModel chatReq
            , "messages" .= anthropicMessages
            , "max_tokens" .= reqMaxTokens chatReq
            , "stream" .= reqStream chatReq
            ] ++ (if null anthropicTools then [] else ["tools" .= anthropicTools])
              ++ (case reqTemperature chatReq of Just t -> ["temperature" .= t]; Nothing -> [])

          headers = [(hContentType, "application/json")]
                 ++ if backendRequiresAuth
                    then [(hAuthorization, TE.encodeUtf8 $ "Bearer " <> backendApiKey)]
                    else []

      Right (url, requestBody, headers)

    BackendGemini{..} -> do
      let baseUrl = case backendBaseUrl of
            Just u -> u
            Nothing -> "https://generativelanguage.googleapis.com"

          -- Construct URL path based on endpoint type
          -- Different endpoints use different URL structures:
          -- - generativelanguage.googleapis.com: /v1beta/models/{model}:generateContent
          -- - aiplatform.googleapis.com: /v1/publishers/google/models/{model}:generateContent
          -- - {region}-aiplatform.googleapis.com: /v1/publishers/google/models/{model}:generateContent
          baseUrlWithPath = if T.isInfixOf "generativelanguage.googleapis.com" baseUrl
                           then baseUrl <> "/v1beta/models/" <> reqModel chatReq <> ":generateContent"
                           else if T.isInfixOf "aiplatform.googleapis.com" baseUrl
                           then baseUrl <> "/v1/publishers/google/models/" <> reqModel chatReq <> ":generateContent"
                           else baseUrl <> "/v1beta/models/" <> reqModel chatReq <> ":generateContent"  -- default

          -- Determine authentication method based on endpoint
          -- Three methods:
          -- 1. Query parameter: aiplatform.googleapis.com?key=API_KEY
          -- 2. Bearer token: ${LOCATION}-aiplatform.googleapis.com with Authorization header
          -- 3. API key header: generativelanguage.googleapis.com with x-goog-api-key header
          (finalUrl, authHeaders) = if backendRequiresAuth
            then
              if T.isInfixOf "generativelanguage.googleapis.com" baseUrl
              then
                -- Method 3: x-goog-api-key header for generativelanguage.googleapis.com
                (baseUrlWithPath, [("x-goog-api-key", TE.encodeUtf8 backendApiKey)])
              else if T.isInfixOf "-aiplatform.googleapis.com" baseUrl
              then
                -- Method 2: Authorization Bearer for region-specific endpoints (e.g., us-central1-aiplatform.googleapis.com)
                (baseUrlWithPath, [(hAuthorization, TE.encodeUtf8 $ "Bearer " <> backendApiKey)])
              else if T.isInfixOf "aiplatform.googleapis.com" baseUrl
              then
                -- Method 1: Query parameter for aiplatform.googleapis.com
                (baseUrlWithPath <> "?key=" <> backendApiKey, [])
              else
                -- Default to x-goog-api-key for unknown endpoints
                (baseUrlWithPath, [("x-goog-api-key", TE.encodeUtf8 backendApiKey)])
            else
              (baseUrlWithPath, [])

      -- Convert to Gemini format (reverse of what geminiToOpenAI does)
      let geminiContents = map chatMessageToGemini (reqMessages chatReq)
          geminiTools = if null (reqTools chatReq)
                       then []
                       else [object ["functionDeclarations" .= map chatToolToGemini (reqTools chatReq)]]

          requestBody = encode $ object $
            [ "contents" .= geminiContents
            ] ++ (if null geminiTools then [] else ["tools" .= geminiTools])
              ++ (case reqTemperature chatReq of
                   Just t -> ["generationConfig" .= object ["temperature" .= t]]
                   Nothing -> [])
              ++ (case reqMaxTokens chatReq of
                   Just m -> ["generationConfig" .= object ["maxOutputTokens" .= m]]
                   Nothing -> [])

          headers = [(hContentType, "application/json")] ++ authHeaders

      Right (finalUrl, requestBody, headers)

-- | Parse backend response into ChatResponse
parseBackendResponse :: Backend -> BL.ByteString -> Either String ChatResponse
parseBackendResponse backend respBody =
  case backend of
    BackendOpenAI{..} -> parseOpenAIResponse respBody
    BackendAnthropic{..} -> parseAnthropicResponse respBody
    BackendGemini{..} -> parseGeminiResponse respBody

-- | Parse OpenAI format response
parseOpenAIResponse :: BL.ByteString -> Either String ChatResponse
parseOpenAIResponse body = do
  obj <- eitherDecode body
  case obj of
    Object o -> do
      respId <- case HM.lookup "id" o of
        Just (String i) -> Right i
        _ -> Right "unknown"

      respModel <- case HM.lookup "model" o of
        Just (String m) -> Right m
        _ -> Right "unknown"

      choices <- case HM.lookup "choices" o of
        Just (Array cs) -> Right $ V.toList cs
        _ -> Left "Missing choices"

      parsedChoices <- mapM parseOpenAIChoice choices

      pure $ ChatResponse respId respModel parsedChoices Nothing

    _ -> Left "Expected object"

parseOpenAIChoice :: Value -> Either String Choice
parseOpenAIChoice (Object choice) = do
  index <- case HM.lookup "index" choice of
    Just (Number n) -> Right (floor n)
    _ -> Right 0

  (message, toolCalls) <- case HM.lookup "message" choice of
    Just (Object msg) -> do
      let content = case HM.lookup "content" msg of
            Just (String txt) -> txt
            Just Null -> ""
            _ -> ""

      tools <- case HM.lookup "tool_calls" msg of
        Just (Array arr) -> mapM parseToolCall (V.toList arr)
        _ -> Right []

      Right (content, tools)
    _ -> Right ("", [])

  let finishReason = case HM.lookup "finish_reason" choice of
        Just (String "stop") -> Just FinishStop
        Just (String "length") -> Just FinishLength
        Just (String "tool_calls") -> Just FinishToolCalls
        _ -> Nothing

  pure $ Choice index message toolCalls finishReason

parseOpenAIChoice _ = Left "Expected choice object"

-- | Parse a tool call from OpenAI format
parseToolCall :: Value -> Either String ResponseToolCall
parseToolCall (Object obj) = do
  tcId <- case HM.lookup "id" obj of
    Just (String i) -> Right i
    _ -> Left "Missing tool call id"

  tcType <- case HM.lookup "type" obj of
    Just (String t) -> Right t
    _ -> Right "function"

  tcFunction <- case HM.lookup "function" obj of
    Just (Object func) -> do
      name <- case HM.lookup "name" func of
        Just (String n) -> Right n
        _ -> Left "Missing function name"

      args <- case HM.lookup "arguments" func of
        Just (String a) -> Right a
        _ -> Right ""

      Right $ FunctionCall name args
    _ -> Left "Missing function object"

  pure $ ResponseToolCall tcId tcType tcFunction

parseToolCall _ = Left "Expected tool call object"

-- | Parse Anthropic format response (uses converter)
parseAnthropicResponse :: BL.ByteString -> Either String ChatResponse
parseAnthropicResponse body = do
  obj <- eitherDecode body
  -- Use anthropicToOpenAI converter, then parse as OpenAI
  case anthropicToOpenAI obj of
    Left err -> Left (T.unpack err)
    Right openAIFormat -> parseOpenAIResponse (encode openAIFormat)

-- | Parse Gemini format response
-- Gemini response format: {"candidates": [{"content": {"role": "model", "parts": [{"text": "..."}]}}]}
parseGeminiResponse :: BL.ByteString -> Either String ChatResponse
parseGeminiResponse body = do
  -- Debug logging (only if LOUTER_DEBUG is set)
  let bodyPreview = BL.take 1000 body
  debugTrace ("DEBUG: Gemini response body (first 1000 bytes): " <> show bodyPreview) $ return ()
  debugTrace ("DEBUG: Gemini response body length: " <> show (BL.length body)) $ return ()

  obj <- eitherDecode body
  case obj of
    Object o -> do
      -- Extract model (optional)
      let model = case HM.lookup "modelVersion" o of
            Just (String m) -> m
            _ -> "unknown"

      -- Extract candidates array
      candidates <- case HM.lookup "candidates" o of
        Just (Array cs) -> Right $ V.toList cs
        _ -> Left $ "Missing 'candidates' field in Gemini response. Available fields: "
                 <> show (HM.keys o) <> ". Response body: " <> show (BL.take 500 body)

      -- Parse each candidate as a choice
      choices <- mapM (parseGeminiCandidate model) (zip [0..] candidates)

      -- Extract usage metadata if present
      let usage = case HM.lookup "usageMetadata" o of
            Just (Object u) -> Just $ Usage
              { usagePromptTokens = case HM.lookup "promptTokenCount" u of
                  Just (Number n) -> floor n
                  _ -> 0
              , usageCompletionTokens = case HM.lookup "candidatesTokenCount" u of
                  Just (Number n) -> floor n
                  _ -> 0
              , usageTotalTokens = case HM.lookup "totalTokenCount" u of
                  Just (Number n) -> floor n
                  _ -> 0
              }
            _ -> Nothing

      -- Extract response ID (optional)
      let respId = case HM.lookup "responseId" o of
            Just (String i) -> i
            _ -> "unknown"

      Right $ ChatResponse respId model choices usage

    _ -> Left "Expected JSON object for Gemini response"

-- Parse a single Gemini candidate into a Choice
parseGeminiCandidate :: Text -> (Int, Value) -> Either String Choice
parseGeminiCandidate model (index, Object candidate) = do
  -- Extract content object
  content <- case HM.lookup "content" candidate of
    Just (Object c) -> Right c
    _ -> Left "Missing 'content' in candidate"

  -- Extract parts array
  parts <- case HM.lookup "parts" content of
    Just (Array ps) -> Right $ V.toList ps
    _ -> Left "Missing 'parts' in content"

  -- Extract text from parts and function calls
  let (texts, functionCalls) = extractGeminiParts parts

  -- Combine all text parts
  let messageText = T.intercalate " " texts

  -- Parse finish reason
  finishReason <- case HM.lookup "finishReason" candidate of
    Just (String "STOP") -> Right $ Just FinishStop
    Just (String "MAX_TOKENS") -> Right $ Just FinishLength
    Just (String "SAFETY") -> Right $ Just FinishContentFilter
    _ -> Right Nothing

  Right $ Choice
    { choiceIndex = index
    , choiceMessage = messageText
    , choiceToolCalls = functionCalls
    , choiceFinishReason = finishReason
    }
parseGeminiCandidate _ (_, _) = Left "Expected object for candidate"

-- Extract text and function calls from Gemini parts
extractGeminiParts :: [Value] -> ([Text], [ResponseToolCall])
extractGeminiParts parts =
  let texts = [txt | Object part <- parts
                    , Just (String txt) <- [HM.lookup "text" part]]

      functionCalls = [call | Object part <- parts
                             , Just call <- [parseGeminiFunctionCall part]]
  in (texts, functionCalls)

-- Parse a Gemini function call part
parseGeminiFunctionCall :: HM.KeyMap Value -> Maybe ResponseToolCall
parseGeminiFunctionCall part = do
  Object funcCall <- HM.lookup "functionCall" part
  String name <- HM.lookup "name" funcCall
  args <- HM.lookup "args" funcCall

  -- Generate an ID (Gemini doesn't provide one)
  let callId = "call_" <> name

  -- Encode args as proper JSON string
  let argsJson = TE.decodeUtf8 $ BL.toStrict $ encode args

  Just $ ResponseToolCall
    { rtcId = callId
    , rtcType = "function"
    , rtcFunction = FunctionCall name argsJson
    }

-- Helper conversions for Anthropic
chatMessageToAnthropic :: Message -> Value
chatMessageToAnthropic msg = object
  [ "role" .= msgRole msg
  , "content" .= msgContent msg
  ]

chatToolToAnthropic :: Tool -> Value
chatToolToAnthropic tool = object $
  [ "name" .= toolName tool
  ] ++ (case toolDescription tool of Just d -> ["description" .= d]; Nothing -> [])
    ++ ["input_schema" .= toolParameters tool]

-- Helper conversions for Gemini
chatMessageToGemini :: Message -> Value
chatMessageToGemini msg =
  let role = case msgRole msg of
        RoleAssistant -> "model"
        RoleUser -> "user"
        _ -> "user"  -- Default for system/tool
      -- Convert each ContentPart to Gemini part format
      parts = map contentPartToGemini (msgContent msg)
  in object
      [ "role" .= (role :: Text)
      , "parts" .= parts
      ]

-- Convert ContentPart to Gemini part format
contentPartToGemini :: ContentPart -> Value
contentPartToGemini (TextPart txt) = object ["text" .= txt]
contentPartToGemini (ImagePart mediaType imageData) = object
  [ "inline_data" .= object
      [ "mime_type" .= mediaType
      , "data" .= imageData
      ]
  ]

chatToolToGemini :: Tool -> Value
chatToolToGemini tool = object $
  [ "name" .= toolName tool
  ] ++ (case toolDescription tool of Just d -> ["description" .= d]; Nothing -> [])
    ++ ["parametersJsonSchema" .= toolParameters tool]
