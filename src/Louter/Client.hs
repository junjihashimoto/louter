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
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (hContentType, hAuthorization)
import Network.HTTP.Types.Header (RequestHeaders)

-- Import server-side converters (proven, tested code)
import Louter.Protocol.AnthropicConverter
import Louter.Protocol.GeminiConverter
import Louter.Types.Request
import Louter.Types.Response
import Louter.Types.Streaming

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
      req <- parseRequest (T.unpack url)
      let req' = req
            { method = "POST"
            , requestBody = RequestBodyLBS body
            , requestHeaders = headers
            }

      response <- httpLbs req' clientManager
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
      let url = case backendBaseUrl of
            Just u -> u <> "/v1beta/models/" <> reqModel chatReq <> ":generateContent"
            Nothing -> "https://generativelanguage.googleapis.com/v1beta/models/"
                      <> reqModel chatReq <> ":generateContent"

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

          headers = [(hContentType, "application/json")]
                 ++ if backendRequiresAuth
                    then [(hAuthorization, TE.encodeUtf8 $ "Bearer " <> backendApiKey)]
                    else []

      Right (url, requestBody, headers)

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

-- | Parse Gemini format response (uses converter)
parseGeminiResponse :: BL.ByteString -> Either String ChatResponse
parseGeminiResponse body = do
  obj <- eitherDecode body
  -- Use geminiToOpenAI converter, then parse as OpenAI
  case geminiToOpenAI "unknown" False obj of  -- False = non-streaming
    Left err -> Left (T.unpack err)
    Right openAIFormat -> parseOpenAIResponse (encode openAIFormat)

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
      parts = [object ["text" .= msgContent msg]]
  in object
      [ "role" .= (role :: Text)
      , "parts" .= parts
      ]

chatToolToGemini :: Tool -> Value
chatToolToGemini tool = object $
  [ "name" .= toolName tool
  ] ++ (case toolDescription tool of Just d -> ["description" .= d]; Nothing -> [])
    ++ ["parametersJsonSchema" .= toolParameters tool]
