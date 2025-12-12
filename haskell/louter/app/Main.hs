{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Louter proxy server executable
-- Routes requests between different LLM protocols using raw WAI
module Main where

import Control.Applicative ((<|>))
import Control.Monad (foldM, forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value(..), Object, encode, eitherDecode, object, (.=))
import qualified Data.Aeson.KeyMap as HM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Builder (Builder, byteString)
import Data.Conduit ((.|), runConduit, await)
import qualified Data.Conduit.List as CL
import qualified Data.HashMap.Strict as HMS
import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client (Manager, withResponse, brRead)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Options.Applicative
import System.IO (hFlush, stdout)
import System.Random (randomIO)
import Data.Word (Word64)
import Text.Printf (printf)

import Louter.Client (Client, Backend(..), newClient, chatCompletion, streamChat)
import Louter.Client.OpenAI (llamaServerClient)
import Louter.Types.Request (ChatRequest(..), Message(..), MessageRole(..), Tool(..), ToolChoice(..))
import Louter.Types.Streaming (StreamEvent(..))
import Louter.Protocol.AnthropicConverter
import Louter.Protocol.AnthropicStreaming
import Louter.Protocol.GeminiConverter
import Louter.Protocol.GeminiStreaming
import Louter.Backend.OpenAIToAnthropic
import Louter.Backend.OpenAIToGemini

-- | Server configuration from CLI
data ServerConfig = ServerConfig
  { serverPort :: Int
  , serverConfigFile :: FilePath
  } deriving (Show)

-- | Application state
data AppState = AppState
  { appClients :: Map Text Client  -- backend name -> client
  , appConfig :: Config             -- loaded from TOML
  , appPort :: Int
  , appManager :: Manager           -- HTTP client manager for direct proxying
  }

-- | Backend type enumeration
data BackendType
  = BackendTypeOpenAI    -- OpenAI-compatible (including llama-server)
  | BackendTypeAnthropic -- Anthropic Claude API
  | BackendTypeGemini    -- Google Gemini API
  deriving (Show, Eq)

-- | Simplified config (will expand later)
data Config = Config
  { configBackends :: Map Text BackendConfig
  } deriving (Show)

data BackendConfig = BackendConfig
  { backendType :: BackendType
  , backendUrl :: Text
  , backendApiKey :: Maybe Text
  , backendRequiresAuth :: Bool
  , backendModelMapping :: Map Text Text  -- frontend model -> backend model
  } deriving (Show)

-- | CLI parser
serverConfigParser :: Parser ServerConfig
serverConfigParser = ServerConfig
  <$> option auto
      ( long "port"
     <> short 'p'
     <> metavar "PORT"
     <> value 9000
     <> help "Port to listen on" )
  <*> strOption
      ( long "config"
     <> short 'c'
     <> metavar "FILE"
     <> value "config.toml"
     <> help "Configuration file" )

-- | Main entry point
main :: IO ()
main = do
  ServerConfig{..} <- execParser opts
  putStrLn $ "Louter proxy server starting on port " <> show serverPort
  putStrLn $ "Using config file: " <> serverConfigFile

  -- For now, create a default config with llama-server
  let defaultConfig = Config
        { configBackends = Map.fromList
            [("llama", BackendConfig
                { backendType = BackendTypeOpenAI
                , backendUrl = "http://localhost:11211"
                , backendApiKey = Nothing
                , backendRequiresAuth = False
                , backendModelMapping = Map.empty
                })]
        }

  -- Initialize clients and HTTP manager
  clients <- initClients defaultConfig
  manager <- HTTP.newManager tlsManagerSettings

  let appState = AppState
        { appClients = clients
        , appConfig = defaultConfig
        , appPort = serverPort
        , appManager = manager
        }

  putStrLn $ "Initialized " <> show (Map.size clients) <> " backend client(s)"
  putStrLn $ "Server ready on http://localhost:" <> show serverPort
  putStrLn ""

  -- Start server
  run serverPort (application appState)
  where
    opts = info (serverConfigParser <**> helper)
      ( fullDesc
     <> progDesc "Multi-protocol LLM proxy server"
     <> header "louter-server - protocol converter for LLM APIs" )

-- | Initialize backend clients from config
initClients :: Config -> IO (Map Text Client)
initClients Config{..} = do
  let backends = Map.toList configBackends
  clients <- mapM initBackend backends
  return $ Map.fromList clients
  where
    initBackend :: (Text, BackendConfig) -> IO (Text, Client)
    initBackend (name, BackendConfig{..}) = do
      client <- llamaServerClient backendUrl
      return (name, client)

-- | Generate a trace ID for request tracking
generateTraceId :: IO Text
generateTraceId = do
  rnd <- randomIO :: IO Word64
  return $ T.pack $ printf "trace-%016x" rnd

-- | Log event in JSON line format
logEvent :: Text -> Text -> Value -> IO ()
logEvent traceId eventType details = do
  let logLine = object
        [ "trace_id" .= traceId
        , "event" .= eventType
        , "details" .= details
        ]
  BS8.putStrLn (BL.toStrict $ encode logLine)
  hFlush stdout

-- | Main WAI application with manual routing
application :: AppState -> Application
application state req respond = do
  -- Generate trace ID for this request
  traceId <- generateTraceId

  let path = pathInfo req
      method = requestMethod req
      pathStr = T.intercalate "/" path

  -- Log incoming request in JSON line format
  logEvent traceId "request_received" $ object
    [ "method" .= TE.decodeUtf8 method
    , "path" .= pathStr
    , "query" .= TE.decodeUtf8 (rawQueryString req)
    ]

  case (method, path) of
    -- Health check
    ("GET", ["health"]) ->
      healthHandler state req respond

    -- OpenAI API
    ("POST", ["v1", "chat", "completions"]) ->
      openAIChatHandler state req respond

    -- Anthropic API
    ("POST", ["v1", "messages"]) ->
      anthropicMessagesHandler state req respond

    -- Gemini API - list models
    ("GET", ["v1beta", "models"]) ->
      geminiListModelsHandler state req respond

    -- Gemini API - model actions (generate/stream)
    ("POST", "v1beta" : "models" : modelPath) ->
      geminiModelActionHandler traceId state modelPath req respond

    -- Diagnostics
    ("GET", ["api", "diagnostics"]) ->
      diagnosticsHandler state req respond

    -- Default 404
    _ -> do
      logEvent traceId "not_found" $ object
        [ "method" .= TE.decodeUtf8 method
        , "path" .= pathStr
        , "available_endpoints" .=
            [ "/health" :: Text
            , "/v1/chat/completions"
            , "/v1/messages"
            , "/v1beta/models"
            , "/v1beta/models/:model:streamGenerateContent"
            , "/v1beta/models/:model:generateContent"
            ]
        ]
      respond $ responseLBS status404
        [("Content-Type", "application/json")]
        (encode $ object
          [ "error" .= object
              [ "code" .= (404 :: Int)
              , "message" .= ("Requested entity was not found." :: Text)
              , "status" .= ("NOT_FOUND" :: Text)
              ]
          ])

-- | /health endpoint
healthHandler :: AppState -> Application
healthHandler _state _req respond =
  respond $ responseLBS status200
    [("Content-Type", "application/json")]
    (encode $ object
      [ "status" .= ("ok" :: Text)
      , "service" .= ("louter" :: Text)
      ])

-- | /v1/chat/completions - OpenAI endpoint
-- Routes to appropriate backend based on configuration
openAIChatHandler :: AppState -> Application
openAIChatHandler state req respond = do
  -- Read request body
  body <- strictRequestBody req

  case eitherDecode body of
    Left err -> respond $ responseLBS status400
      [("Content-Type", "application/json")]
      (encode $ object ["error" .= ("Invalid request: " <> T.pack err :: Text)])

    Right openAIReq -> do
      -- Get first backend (for now - TODO: support backend selection via model name)
      case Map.toList (configBackends $ appConfig state) of
        [] -> respond $ responseLBS status500
          [("Content-Type", "application/json")]
          (encode $ object ["error" .= ("No backend configured" :: Text)])

        ((backendName, backendCfg):_) -> do
          -- Check if streaming is requested
          let isStreaming = getStreamFlag openAIReq

          -- Route based on backend type
          case backendType backendCfg of
            BackendTypeOpenAI ->
              -- Direct OpenAI-compatible backend (no conversion needed)
              if isStreaming
                then handleOpenAIStreamingToOpenAI state backendCfg openAIReq respond
                else handleOpenAINonStreamingToOpenAI state backendCfg openAIReq respond

            BackendTypeAnthropic ->
              -- OpenAI frontend → Anthropic backend (needs conversion)
              if isStreaming
                then handleOpenAIStreamingToAnthropic state backendCfg openAIReq respond
                else handleOpenAINonStreamingToAnthropic state backendCfg openAIReq respond

            BackendTypeGemini ->
              -- OpenAI frontend → Gemini backend (needs conversion)
              if isStreaming
                then handleOpenAIStreamingToGemini state backendCfg openAIReq respond
                else handleOpenAINonStreamingToGemini state backendCfg openAIReq respond

-- ==============================================================================
-- OpenAI Frontend → OpenAI Backend (Direct, No Conversion)
-- ==============================================================================

-- | OpenAI → OpenAI non-streaming
handleOpenAINonStreamingToOpenAI :: AppState -> BackendConfig -> Value -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleOpenAINonStreamingToOpenAI state backendCfg openAIReq respond = do
  let url = T.unpack $ backendUrl backendCfg <> "/v1/chat/completions"

  -- Create backend request (pass-through, no conversion)
  req <- HTTP.parseRequest ("POST " <> url)
  let req' = req
        { HTTP.requestBody = HTTP.RequestBodyLBS (encode openAIReq)
        , HTTP.requestHeaders = [("Content-Type", "application/json")]
        }

  -- Make synchronous request
  response <- HTTP.httpLbs req' (appManager state)
  respond $ responseLBS (HTTP.responseStatus response)
    [("Content-Type", "application/json")]
    (HTTP.responseBody response)

-- | OpenAI → OpenAI streaming
handleOpenAIStreamingToOpenAI :: AppState -> BackendConfig -> Value -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleOpenAIStreamingToOpenAI state backendCfg openAIReq respond = do
  let url = T.unpack $ backendUrl backendCfg <> "/v1/chat/completions"

  req <- HTTP.parseRequest ("POST " <> url)
  let req' = req
        { HTTP.requestBody = HTTP.RequestBodyLBS (encode openAIReq)
        , HTTP.requestHeaders = [("Content-Type", "application/json")]
        }

  let sseResponse = responseStream status200
        [ ("Content-Type", "text/event-stream")
        , ("Cache-Control", "no-cache")
        , ("Connection", "keep-alive")
        ] $ \write flush -> do
          withResponse req' (appManager state) $ \backendResp -> do
            let body = HTTP.responseBody backendResp
            let loop = do
                  chunk <- brRead body
                  if BS.null chunk
                    then pure ()
                    else do
                      write (byteString chunk)
                      flush
                      loop
            loop

  respond sseResponse

-- ==============================================================================
-- OpenAI Frontend → Anthropic Backend (Requires Conversion)
-- ==============================================================================

-- | OpenAI → Anthropic non-streaming
handleOpenAINonStreamingToAnthropic :: AppState -> BackendConfig -> Value -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleOpenAINonStreamingToAnthropic state backendCfg openAIReq respond = do
  -- Convert OpenAI request to Anthropic format (reverse of anthropicToOpenAI)
  case openAIToAnthropic openAIReq of
    Left err -> respond $ responseLBS status400
      [("Content-Type", "application/json")]
      (encode $ object ["error" .= err])

    Right anthropicReq -> do
      let url = T.unpack $ backendUrl backendCfg <> "/v1/messages"

      req <- HTTP.parseRequest ("POST " <> url)
      let req' = req
            { HTTP.requestBody = HTTP.RequestBodyLBS (encode anthropicReq)
            , HTTP.requestHeaders = [("Content-Type", "application/json")]
            }

      response <- HTTP.httpLbs req' (appManager state)

      -- Convert Anthropic response back to OpenAI format
      case eitherDecode (HTTP.responseBody response) of
        Left err -> respond $ responseLBS status500
          [("Content-Type", "application/json")]
          (encode $ object ["error" .= ("Failed to parse Anthropic response: " <> T.pack err :: Text)])

        Right anthropicResp -> do
          let openAIResp = anthropicToOpenAIResponse anthropicResp
          respond $ responseLBS status200
            [("Content-Type", "application/json")]
            (encode openAIResp)

-- | OpenAI → Anthropic streaming
handleOpenAIStreamingToAnthropic :: AppState -> BackendConfig -> Value -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleOpenAIStreamingToAnthropic state backendCfg openAIReq respond = do
  case openAIToAnthropic openAIReq of
    Left err -> respond $ responseLBS status400
      [("Content-Type", "application/json")]
      (encode $ object ["error" .= err])

    Right anthropicReq -> do
      let url = T.unpack $ backendUrl backendCfg <> "/v1/messages"

      req <- HTTP.parseRequest ("POST " <> url)
      let req' = req
            { HTTP.requestBody = HTTP.RequestBodyLBS (encode anthropicReq)
            , HTTP.requestHeaders = [("Content-Type", "application/json")]
            }

      -- Stream and convert Anthropic SSE → OpenAI SSE
      let sseResponse = responseStream status200
            [ ("Content-Type", "text/event-stream")
            , ("Cache-Control", "no-cache")
            , ("Connection", "keep-alive")
            ] $ \write flush -> do
              withResponse req' (appManager state) $ \backendResp -> do
                let body = HTTP.responseBody backendResp
                -- TODO: Convert Anthropic SSE events to OpenAI SSE format
                -- For now, pass through (will need proper conversion)
                convertAnthropicToOpenAIStream write flush body

      respond sseResponse

-- ==============================================================================
-- OpenAI Frontend → Gemini Backend (Requires Conversion)
-- ==============================================================================

-- | OpenAI → Gemini non-streaming
handleOpenAINonStreamingToGemini :: AppState -> BackendConfig -> Value -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleOpenAINonStreamingToGemini state backendCfg openAIReq respond = do
  -- Convert OpenAI request to Gemini format (reverse of geminiToOpenAI)
  case openAIToGemini openAIReq of
    Left err -> respond $ responseLBS status400
      [("Content-Type", "application/json")]
      (encode $ object ["error" .= err])

    Right (modelName, geminiReq) -> do
      let url = T.unpack $ backendUrl backendCfg <> "/v1beta/models/" <> modelName <> ":generateContent"

      req <- HTTP.parseRequest ("POST " <> url)
      let req' = req
            { HTTP.requestBody = HTTP.RequestBodyLBS (encode geminiReq)
            , HTTP.requestHeaders = [("Content-Type", "application/json")]
            }

      response <- HTTP.httpLbs req' (appManager state)

      -- Convert Gemini response back to OpenAI format
      case eitherDecode (HTTP.responseBody response) of
        Left err -> respond $ responseLBS status500
          [("Content-Type", "application/json")]
          (encode $ object ["error" .= ("Failed to parse Gemini response: " <> T.pack err :: Text)])

        Right geminiResp -> do
          let openAIResp = geminiToOpenAIResponse geminiResp
          respond $ responseLBS status200
            [("Content-Type", "application/json")]
            (encode openAIResp)

-- | OpenAI → Gemini streaming
handleOpenAIStreamingToGemini :: AppState -> BackendConfig -> Value -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleOpenAIStreamingToGemini state backendCfg openAIReq respond = do
  case openAIToGemini openAIReq of
    Left err -> respond $ responseLBS status400
      [("Content-Type", "application/json")]
      (encode $ object ["error" .= err])

    Right (modelName, geminiReq) -> do
      let url = T.unpack $ backendUrl backendCfg <> "/v1beta/models/" <> modelName <> ":streamGenerateContent"

      req <- HTTP.parseRequest ("POST " <> url)
      let req' = req
            { HTTP.requestBody = HTTP.RequestBodyLBS (encode geminiReq)
            , HTTP.requestHeaders = [("Content-Type", "application/json")]
            }

      -- Stream and convert Gemini SSE → OpenAI SSE
      let sseResponse = responseStream status200
            [ ("Content-Type", "text/event-stream")
            , ("Cache-Control", "no-cache")
            , ("Connection", "keep-alive")
            ] $ \write flush -> do
              withResponse req' (appManager state) $ \backendResp -> do
                let body = HTTP.responseBody backendResp
                -- TODO: Convert Gemini SSE events to OpenAI SSE format
                -- For now, pass through (will need proper conversion)
                convertGeminiToOpenAIStream write flush body

      respond sseResponse

-- | Convert StreamEvent to SSE format
eventToSSE :: StreamEvent -> BS.ByteString
eventToSSE event = case event of
  StreamContent txt ->
    "data: " <> BL.toStrict (encode $ object
      [ "choices" .= [object
          [ "delta" .= object ["content" .= txt]
          , "index" .= (0 :: Int)
          ]]
      ]) <> "\n\n"

  StreamReasoning txt ->
    "data: " <> BL.toStrict (encode $ object
      [ "choices" .= [object
          [ "delta" .= object ["reasoning" .= txt]
          , "index" .= (0 :: Int)
          ]]
      ]) <> "\n\n"

  StreamToolCall toolCall ->
    "data: " <> BL.toStrict (encode $ object
      [ "choices" .= [object
          [ "delta" .= object ["tool_calls" .= [toolCall]]
          , "index" .= (0 :: Int)
          ]]
      ]) <> "\n\n"

  StreamFinish reason ->
    "data: " <> BL.toStrict (encode $ object
      [ "choices" .= [object
          [ "finish_reason" .= reason
          , "index" .= (0 :: Int)
          ]]
      ]) <> "\n\n"

  StreamError err ->
    "data: " <> BL.toStrict (encode $ object ["error" .= err]) <> "\n\n"

-- | Extract stream flag from OpenAI request
getStreamFlag :: Value -> Bool
getStreamFlag (Object obj) = case HM.lookup "stream" obj of
  Just (Bool b) -> b
  _ -> False
getStreamFlag _ = False

-- | Parse OpenAI request to ChatRequest
parseOpenAIRequest :: Value -> Either Text ChatRequest
parseOpenAIRequest (Object obj) = do
  -- Extract model
  model <- case HM.lookup "model" obj of
    Just (String m) -> Right m
    _ -> Left "Missing or invalid 'model' field"

  -- Extract messages
  messages <- case HM.lookup "messages" obj of
    Just (Array msgs) -> parseMessages (V.toList msgs)
    _ -> Left "Missing or invalid 'messages' field"

  -- Extract optional fields
  let temperature = case HM.lookup "temperature" obj of
        Just (Number n) -> Just (realToFrac n)
        _ -> Nothing

  let maxTokens = case HM.lookup "max_tokens" obj of
        Just (Number n) -> Just (floor n)
        _ -> Nothing

  let stream = case HM.lookup "stream" obj of
        Just (Bool b) -> b
        _ -> False

  -- Extract tools (if any)
  tools <- case HM.lookup "tools" obj of
    Just (Array ts) -> parseTools (V.toList ts)
    _ -> Right []

  Right ChatRequest
    { reqModel = model
    , reqMessages = messages
    , reqTools = tools
    , reqToolChoice = ToolChoiceAuto
    , reqTemperature = temperature
    , reqMaxTokens = maxTokens
    , reqStream = stream
    }
parseOpenAIRequest _ = Left "Request must be a JSON object"

-- | Parse messages array
parseMessages :: [Value] -> Either Text [Message]
parseMessages = mapM parseMessage
  where
    parseMessage :: Value -> Either Text Message
    parseMessage (Object obj) = do
      role <- case HM.lookup "role" obj of
        Just (String "system") -> Right RoleSystem
        Just (String "user") -> Right RoleUser
        Just (String "assistant") -> Right RoleAssistant
        Just (String "tool") -> Right RoleTool
        _ -> Left "Invalid or missing 'role' field"

      content <- case HM.lookup "content" obj of
        Just (String c) -> Right c
        _ -> Left "Invalid or missing 'content' field"

      Right Message { msgRole = role, msgContent = content }
    parseMessage _ = Left "Message must be a JSON object"

-- | Parse tools array
parseTools :: [Value] -> Either Text [Tool]
parseTools = mapM parseTool
  where
    parseTool :: Value -> Either Text Tool
    parseTool (Object obj) = do
      -- OpenAI tools format: { "type": "function", "function": { ... } }
      function <- case HM.lookup "function" obj of
        Just (Object fn) -> Right fn
        _ -> Left "Missing or invalid 'function' field"

      name <- case HM.lookup "name" function of
        Just (String n) -> Right n
        _ -> Left "Missing or invalid function 'name'"

      let description = case HM.lookup "description" function of
            Just (String d) -> Just d
            _ -> Nothing

      parameters <- case HM.lookup "parameters" function of
        Just params -> Right params
        _ -> Left "Missing 'parameters' field"

      Right Tool
        { toolName = name
        , toolDescription = description
        , toolParameters = parameters
        }
    parseTool _ = Left "Tool must be a JSON object"

-- | /v1/messages - Anthropic endpoint
anthropicMessagesHandler :: AppState -> Application
anthropicMessagesHandler state req respond = do
  -- Read request body
  body <- strictRequestBody req

  case eitherDecode body of
    Left err -> respond $ responseLBS status400
      [("Content-Type", "application/json")]
      (encode $ object ["error" .= ("Invalid Anthropic request: " <> T.pack err :: Text)])

    Right anthropicReq -> do
      -- Check if streaming
      let isStreaming = getAnthropicStreamFlag anthropicReq

      if isStreaming
        then handleAnthropicStreaming state anthropicReq respond
        else handleAnthropicNonStreaming state anthropicReq respond

-- | Get Anthropic stream flag
getAnthropicStreamFlag :: Value -> Bool
getAnthropicStreamFlag (Object obj) = case HM.lookup "stream" obj of
  Just (Bool b) -> b
  _ -> False
getAnthropicStreamFlag _ = False

-- | Handle Anthropic streaming request
handleAnthropicStreaming :: AppState -> Value -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleAnthropicStreaming state anthropicReq respond = do
  -- Convert Anthropic request to OpenAI format
  case anthropicToOpenAI anthropicReq of
    Left err -> respond $ responseLBS status400
      [("Content-Type", "application/json")]
      (encode $ object ["error" .= err])

    Right openAIReq -> do
      -- Make request to backend
      let backendUrl = "http://localhost:11211/v1/chat/completions"

      req <- HTTP.parseRequest ("POST " <> backendUrl)
      let req' = req
            { HTTP.requestBody = HTTP.RequestBodyLBS (encode openAIReq)
            , HTTP.requestHeaders = [("Content-Type", "application/json")]
            }

      -- Stream response and convert OpenAI SSE → Anthropic SSE
      let sseResponse = responseStream status200
            [ ("Content-Type", "text/event-stream")
            , ("Cache-Control", "no-cache")
            , ("Connection", "keep-alive")
            ] $ \write flush -> do
              withResponse req' (appManager state) $ \backendResp -> do
                let body = HTTP.responseBody backendResp
                -- Convert OpenAI chunks to Anthropic events
                convertOpenAIToAnthropic write flush body

      respond sseResponse

-- | Handle Anthropic non-streaming request
handleAnthropicNonStreaming :: AppState -> Value -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleAnthropicNonStreaming state anthropicReq respond = do
  case anthropicToOpenAI anthropicReq of
    Left err -> respond $ responseLBS status400
      [("Content-Type", "application/json")]
      (encode $ object ["error" .= err])

    Right openAIReq -> do
      let backendUrl = "http://localhost:11211/v1/chat/completions"

      req <- HTTP.parseRequest ("POST " <> backendUrl)
      let req' = req
            { HTTP.requestBody = HTTP.RequestBodyLBS (encode openAIReq)
            , HTTP.requestHeaders = [("Content-Type", "application/json")]
            }

      response <- HTTP.httpLbs req' (appManager state)

      -- Convert OpenAI response to Anthropic format
      case eitherDecode (HTTP.responseBody response) of
        Left err -> respond $ responseLBS status500
          [("Content-Type", "application/json")]
          (encode $ object ["error" .= ("Failed to parse backend response: " <> T.pack err :: Text)])

        Right openAIResp -> do
          let anthropicResp = openAIResponseToAnthropic openAIResp
          respond $ responseLBS status200
            [("Content-Type", "application/json")]
            (encode anthropicResp)

-- Anthropic conversion functions and streaming now imported from library modules

-- ==============================================================================
-- Gemini API Handlers
-- ==============================================================================

-- | Handle Gemini streaming request
handleGeminiStreaming :: Text -> AppState -> Text -> Value -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleGeminiStreaming traceId state modelName geminiReq respond = do
  case geminiToOpenAI modelName True geminiReq of
    Left err -> do
      logEvent traceId "gemini_to_openai_error" $ object
        [ "error" .= err
        , "model" .= modelName
        ]
      respond $ responseLBS status400
        [("Content-Type", "application/json")]
        (encode $ object ["error" .= err])

    Right openAIReq -> do
      -- Log converted OpenAI request
      logEvent traceId "openai_request" $ object
        [ "backend_url" .= ("http://localhost:11211/v1/chat/completions" :: Text)
        , "request" .= openAIReq
        , "streaming" .= True
        ]

      -- Create backend HTTP request
      let backendUrl = "http://localhost:11211/v1/chat/completions"
      req <- HTTP.parseRequest ("POST " <> backendUrl)
      let req' = req
            { HTTP.requestBody = HTTP.RequestBodyLBS (encode openAIReq)
            , HTTP.requestHeaders = [("Content-Type", "application/json")]
            }

      -- Stream response and convert OpenAI SSE → Gemini SSE
      -- Note: Gemini uses SSE format with "data: {...}\n\n" (NOT arrays)
      let streamResponse = responseStream status200
            [ ("Content-Type", "text/event-stream; charset=utf-8")
            , ("Cache-Control", "no-cache")
            , ("Connection", "keep-alive")
            ] $ \write flush -> do
              withResponse req' (appManager state) $ \backendResp -> do
                let body = HTTP.responseBody backendResp
                    statusCode = HTTP.responseStatus backendResp

                -- Log backend response status
                logEvent traceId "backend_response" $ object
                  [ "status" .= show statusCode
                  , "streaming" .= True
                  ]

                -- Convert OpenAI SSE to Gemini SSE
                convertOpenAIToGeminiStream write flush body

      logEvent traceId "streaming_started" $ object ["status" .= ("ok" :: Text)]
      respond streamResponse

-- | Handle Gemini non-streaming request
handleGeminiNonStreaming :: Text -> AppState -> Text -> Value -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleGeminiNonStreaming traceId state modelName geminiReq respond = do
  case geminiToOpenAI modelName False geminiReq of
    Left err -> do
      logEvent traceId "gemini_to_openai_error" $ object
        [ "error" .= err
        , "model" .= modelName
        ]
      respond $ responseLBS status400
        [("Content-Type", "application/json")]
        (encode $ object ["error" .= err])

    Right openAIReq -> do
      -- Log converted OpenAI request
      logEvent traceId "openai_request" $ object
        [ "backend_url" .= ("http://localhost:11211/v1/chat/completions" :: Text)
        , "request" .= openAIReq
        , "streaming" .= False
        ]

      -- Create backend HTTP request
      let backendUrl = "http://localhost:11211/v1/chat/completions"
      req <- HTTP.parseRequest ("POST " <> backendUrl)
      let req' = req
            { HTTP.requestBody = HTTP.RequestBodyLBS (encode openAIReq)
            , HTTP.requestHeaders = [("Content-Type", "application/json")]
            }

      -- Make synchronous request
      httpResp <- HTTP.httpLbs req' (appManager state)
      let responseBody' = HTTP.responseBody httpResp
          statusCode = HTTP.responseStatus httpResp

      -- Log backend response status
      logEvent traceId "backend_response" $ object
        [ "status" .= show statusCode
        , "body_length" .= BL.length responseBody'
        ]

      -- Convert OpenAI response to Gemini format
      case eitherDecode responseBody' of
        Left err -> do
          logEvent traceId "backend_parse_error" $ object
            [ "error" .= T.pack err
            , "body_preview" .= T.take 200 (TE.decodeUtf8 (BL.toStrict responseBody'))
            ]
          respond $ responseLBS status500
            [("Content-Type", "application/json")]
            (encode $ object ["error" .= ("Failed to parse backend response: " <> T.pack err :: Text)])

        Right openAIResp -> do
          logEvent traceId "response_success" $ object
            [ "status" .= ("ok" :: Text)
            ]
          respond $ responseLBS status200
            [("Content-Type", "application/json")]
            (encode $ openAIResponseToGemini openAIResp)

-- | Handle Gemini countTokens request
handleCountTokens :: Text -> AppState -> Text -> Value -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleCountTokens traceId state modelName geminiReq respond = do
  -- Log countTokens request
  logEvent traceId "count_tokens_request" $ object
    [ "model" .= modelName
    , "request" .= geminiReq
    ]

  case geminiToOpenAI modelName False geminiReq of
    Left err -> do
      logEvent traceId "gemini_to_openai_error" $ object ["error" .= err]
      respond $ responseLBS status400
        [("Content-Type", "application/json")]
        (encode $ object ["error" .= err])

    Right openAIReq -> do
      -- Estimate tokens from the OpenAI request
      let totalTokens = estimateTokensFromRequest openAIReq

      -- Log token count result
      logEvent traceId "count_tokens_result" $ object
        [ "model" .= modelName
        , "total_tokens" .= totalTokens
        ]

      -- Return Gemini countTokens response format
      respond $ responseLBS status200
        [("Content-Type", "application/json")]
        (encode $ object ["totalTokens" .= totalTokens])

-- | Estimate tokens from an OpenAI request
-- Simple heuristic: ~4 characters per token
estimateTokensFromRequest :: Value -> Int
estimateTokensFromRequest (Object obj) =
  let messagesTokens = case HM.lookup "messages" obj of
        Just (Array msgs) -> sum $ map estimateTokensFromMessage (V.toList msgs)
        _ -> 0
      toolsTokens = case HM.lookup "tools" obj of
        Just (Array tools) -> sum $ map estimateTokensFromValue (V.toList tools)
        _ -> 0
  in max 1 (messagesTokens + toolsTokens)
estimateTokensFromRequest _ = 1

-- | Estimate tokens from a single message
estimateTokensFromMessage :: Value -> Int
estimateTokensFromMessage (Object msg) =
  case HM.lookup "content" msg of
    Just (String txt) -> estimateTokensFromText txt
    Just val -> estimateTokensFromValue val
    Nothing -> 0
estimateTokensFromMessage _ = 0

-- | Estimate tokens from text
estimateTokensFromText :: Text -> Int
estimateTokensFromText txt = max 1 ((T.length txt + 3) `div` 4)

-- | Estimate tokens from any JSON value
estimateTokensFromValue :: Value -> Int
estimateTokensFromValue (String txt) = estimateTokensFromText txt
estimateTokensFromValue (Array arr) = sum $ map estimateTokensFromValue (V.toList arr)
estimateTokensFromValue (Object obj) = sum $ map estimateTokensFromValue (HM.elems obj)
estimateTokensFromValue _ = 1

-- Gemini conversion functions and streaming now imported from library modules

-- | /v1beta/models - Gemini list models
geminiListModelsHandler :: AppState -> Application
geminiListModelsHandler _state _req respond =
  respond $ responseLBS status501
    [("Content-Type", "application/json")]
    (encode $ object ["error" .= ("Gemini list models not yet implemented" :: Text)])

-- | /v1beta/models/:model:action - Gemini model actions
geminiModelActionHandler :: Text -> AppState -> [Text] -> Application
geminiModelActionHandler traceId state modelPath req respond = do
  -- Parse model and action from path
  -- Path format: ["model-name:streamGenerateContent"] or ["model-name:generateContent"]
  case modelPath of
    [] -> do
      logEvent traceId "error" $ object ["message" .= ("Missing model path" :: Text)]
      respond $ responseLBS status400
        [("Content-Type", "application/json")]
        (encode $ object ["error" .= ("Missing model path" :: Text)])

    (fullPath:_) -> do
      -- Split on ':' to get model and action
      let parts = T.splitOn ":" fullPath
      case parts of
        [modelName, action] -> do
          -- Read request body
          body <- strictRequestBody req

          -- Log incoming Gemini request
          case eitherDecode body of
            Left err -> do
              logEvent traceId "request_parse_error" $ object
                [ "error" .= T.pack err
                , "body_size" .= BL.length body
                ]
              respond $ responseLBS status400
                [("Content-Type", "application/json")]
                (encode $ object ["error" .= ("Invalid Gemini request: " <> T.pack err :: Text)])

            Right geminiReq -> do
              -- Log parsed Gemini request
              logEvent traceId "gemini_request_parsed" $ object
                [ "model" .= modelName
                , "action" .= action
                , "request" .= geminiReq
                ]

              case action of
                "streamGenerateContent" -> handleGeminiStreaming traceId state modelName geminiReq respond
                "generateContent" -> handleGeminiNonStreaming traceId state modelName geminiReq respond
                "countTokens" -> handleCountTokens traceId state modelName geminiReq respond
                _ -> do
                  logEvent traceId "error" $ object
                    [ "message" .= ("Unsupported action" :: Text)
                    , "action" .= action
                    ]
                  respond $ responseLBS status400
                    [("Content-Type", "application/json")]
                    (encode $ object ["error" .= ("Unsupported action: " <> action :: Text)])

        _ -> do
          logEvent traceId "error" $ object
            [ "message" .= ("Invalid model path format" :: Text)
            , "path" .= fullPath
            ]
          respond $ responseLBS status400
            [("Content-Type", "application/json")]
            (encode $ object ["error" .= ("Invalid model path format" :: Text)])

-- | /api/diagnostics endpoint
diagnosticsHandler :: AppState -> Application
diagnosticsHandler state _req respond =
  respond $ responseLBS status200
    [("Content-Type", "application/json")]
    (encode $ object
      [ "status" .= ("ok" :: Text)
      , "backends" .= Map.keys (appClients state)
      , "port" .= appPort state
      ])
