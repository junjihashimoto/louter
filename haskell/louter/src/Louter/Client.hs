{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | High-level client API for Louter
-- This is the main interface for applications using Louter as a library
--
-- Key Design: "Louter is a protocol converter. The API can connect to
-- OpenAI API and Gemini API like the proxy."
--
-- Example usage:
-- @
--   import Louter.Client
--
--   main = do
--     -- Connect to Gemini API using OpenAI-style requests
--     client <- newClient (BackendGemini "your-api-key")
--     response <- chatCompletion client $ defaultChatRequest "gemini-pro"
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
    -- * Re-exports
  , module Louter.Types
  , module Louter.Protocol
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, encode, eitherDecode, toJSON)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Conduit ((.|), runConduit, ConduitT, yield)
import qualified Data.Conduit.List as CL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)
import Network.HTTP.Client (Manager, newManager, httpLbs, parseRequest, requestBody, requestHeaders, RequestBody(..), responseBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (hContentType, hAuthorization)

import Louter.Protocol
import Louter.Streaming.Processor (processStream)
import Louter.Types

-- | Client configuration
data Client = Client
  { clientManager :: Manager
  , clientBackend :: Backend
  }

-- | Backend configuration
data Backend
  = BackendOpenAI
      { backendApiKey :: Text
      , backendBaseUrl :: Maybe Text  -- ^ Optional custom base URL
      , backendRequiresAuth :: Bool    -- ^ Whether API key authentication is required
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
      case eitherDecode respBody of
        Left err -> pure $ Left $ "Failed to parse response: " <> T.pack err
        Right resp -> pure $ Right resp

-- | Streaming chat with conduit
streamChat :: Client -> ChatRequest -> ConduitT () StreamEvent IO ()
streamChat client req = do
  let req' = req { reqStream = True }
  result <- liftIO $ makeRequest client req'
  case result of
    Left err -> yield (StreamError err)
    Right respBody -> do
      let chunks = parseStreamingResponse respBody
      processStreamingChunks (getProtocol $ clientBackend client) chunks

-- | Type alias for streaming callbacks
type StreamCallback = StreamEvent -> IO ()

-- | Streaming chat with callback
streamChatWithCallback :: Client -> ChatRequest -> StreamCallback -> IO ()
streamChatWithCallback client req callback = do
  runConduit $ streamChat client req .| CL.mapM_ (liftIO . callback)

-- | Make HTTP request to backend
makeRequest :: Client -> ChatRequest -> IO (Either Text ByteString)
makeRequest Client{..} chatReq = do
  let backend = clientBackend
      protocol = getProtocol backend
      baseUrl = getBaseUrl backend
      apiKey = getApiKey backend
      requiresAuth = getRequiresAuth backend

  -- Convert IR request to protocol-specific format
  case requestToIR protocol (toJSON chatReq) of
    Left err -> pure $ Left err
    Right irReq -> do
      let endpoint = getEndpoint protocol (reqModel chatReq)
          url = T.unpack $ baseUrl <> endpoint
          body = encode (toJSON irReq)

      req <- parseRequest url
      let authHeader = if requiresAuth
                       then [(hAuthorization, TE.encodeUtf8 $ "Bearer " <> apiKey)]
                       else []
          req' = req
            { requestBody = RequestBodyLBS body
            , requestHeaders = (hContentType, "application/json") : authHeader
            }

      response <- httpLbs req' clientManager
      pure $ Right $ responseBody response

-- | Get protocol from backend
getProtocol :: Backend -> Protocol
getProtocol (BackendOpenAI _ _ _) = ProtocolOpenAI
getProtocol (BackendGemini _ _ _) = ProtocolGemini
getProtocol (BackendAnthropic _ _ _) = ProtocolAnthropic

-- | Get base URL from backend
getBaseUrl :: Backend -> Text
getBaseUrl (BackendOpenAI _ (Just url) _) = url
getBaseUrl (BackendOpenAI _ Nothing _) = "https://api.openai.com"
getBaseUrl (BackendGemini _ (Just url) _) = url
getBaseUrl (BackendGemini _ Nothing _) = "https://generativelanguage.googleapis.com"
getBaseUrl (BackendAnthropic _ (Just url) _) = url
getBaseUrl (BackendAnthropic _ Nothing _) = "https://api.anthropic.com"

-- | Get API key from backend
getApiKey :: Backend -> Text
getApiKey (BackendOpenAI key _ _) = key
getApiKey (BackendGemini key _ _) = key
getApiKey (BackendAnthropic key _ _) = key

-- | Check if backend requires authentication
getRequiresAuth :: Backend -> Bool
getRequiresAuth (BackendOpenAI _ _ auth) = auth
getRequiresAuth (BackendGemini _ _ auth) = auth
getRequiresAuth (BackendAnthropic _ _ auth) = auth

-- | Get API endpoint for protocol
getEndpoint :: Protocol -> Text -> Text
getEndpoint ProtocolOpenAI _ = "/v1/chat/completions"
getEndpoint ProtocolGemini model = "/v1beta/models/" <> model <> ":streamGenerateContent"
getEndpoint ProtocolAnthropic _ = "/v1/messages"

-- | Parse streaming response body into SSE chunks
parseStreamingResponse :: ByteString -> [Either Text Text]
parseStreamingResponse body =
  let lines = BL.split (fromIntegral (fromEnum '\n' :: Int) :: Word8) body
      textLines = map (TE.decodeUtf8 . BL.toStrict) lines
      sseLines = filter (T.isPrefixOf "data: ") textLines
  in map parseSSEData sseLines
  where
    parseSSEData line = Right $ T.drop 6 line  -- Remove "data: " prefix

-- | Process streaming chunks through the protocol converter and processor
processStreamingChunks :: Protocol -> [Either Text Text] -> ConduitT () StreamEvent IO ()
processStreamingChunks protocol chunks = do
  let messageId = "stream-001"  -- TODO: Extract from response
  mapM_ (processChunk protocol messageId) chunks

-- | Process a single chunk
processChunk :: Protocol -> Text -> Either Text Text -> ConduitT () StreamEvent IO ()
processChunk _ _ (Left err) = yield (StreamError err)
processChunk _ _ (Right "[DONE]") = yield (StreamFinish "stop")
processChunk protocol messageId (Right jsonText) =
  case parseChunk protocol jsonText of
    Left err -> yield (StreamError err)
    Right deltaType -> do
      let events = processStream messageId [deltaType]
      mapM_ yield events
