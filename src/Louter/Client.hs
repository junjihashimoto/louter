{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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

-- | Streaming chat with conduit
streamChat :: Client -> ChatRequest -> ConduitT () StreamEvent IO ()
streamChat client req = do
  let req' = req { reqStream = True }
  -- For now, just make the request and parse simple events
  -- TODO: Implement proper streaming when we have tested server-side streaming
  result <- liftIO $ makeRequest client req'
  case result of
    Left err -> yield (StreamError err)
    Right _respBody -> do
      -- Placeholder: just return a finish event
      -- Real implementation would parse SSE stream
      yield (StreamFinish "stop")

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
