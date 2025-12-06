{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Louter proxy server executable
-- Routes requests between different LLM protocols using raw WAI
module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value(..), encode, eitherDecode, object, (.=))
import qualified Data.Aeson.KeyMap as HM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Builder (byteString)
import Data.Conduit ((.|), runConduit, await)
import qualified Data.Conduit.List as CL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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

import Louter.Client (Client, Backend(..), newClient, chatCompletion, streamChat)
import Louter.Client.OpenAI (llamaServerClient)
import Louter.Types.Request (ChatRequest(..), Message(..), MessageRole(..), Tool(..), ToolChoice(..))
import Louter.Types.Streaming (StreamEvent(..))

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

-- | Simplified config (will expand later)
data Config = Config
  { configBackends :: Map Text BackendConfig
  } deriving (Show)

data BackendConfig = BackendConfig
  { backendUrl :: Text
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
                { backendUrl = "http://localhost:11211"
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

-- | Main WAI application with manual routing
application :: AppState -> Application
application state req respond = do
  let path = pathInfo req
      method = requestMethod req

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
      geminiModelActionHandler state modelPath req respond

    -- Diagnostics
    ("GET", ["api", "diagnostics"]) ->
      diagnosticsHandler state req respond

    -- Default 404
    _ -> respond $ responseLBS status404
           [("Content-Type", "application/json")]
           (encode $ object ["error" .= ("Not found" :: Text)])

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
openAIChatHandler :: AppState -> Application
openAIChatHandler state req respond = do
  -- Read request body
  body <- strictRequestBody req

  case eitherDecode body of
    Left err -> respond $ responseLBS status400
      [("Content-Type", "application/json")]
      (encode $ object ["error" .= ("Invalid request: " <> T.pack err :: Text)])

    Right openAIReq -> do
      -- For now, just use the first available client (llama)
      case Map.lookup "llama" (appClients state) of
        Nothing -> respond $ responseLBS status500
          [("Content-Type", "application/json")]
          (encode $ object ["error" .= ("No backend configured" :: Text)])

        Just client -> do
          -- Check if streaming is requested
          let isStreaming = getStreamFlag openAIReq

          if isStreaming
            then handleStreamingRequest state openAIReq respond
            else handleNonStreamingRequest state openAIReq respond

-- | Handle non-streaming OpenAI request
handleNonStreamingRequest :: AppState -> Value -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleNonStreamingRequest state openAIReq respond = do
  -- Parse OpenAI request
  case parseOpenAIRequest openAIReq of
    Left err -> respond $ responseLBS status400
      [("Content-Type", "application/json")]
      (encode $ object ["error" .= err])

    Right chatReq -> nonStreamingResponse state chatReq respond

-- | Non-streaming response from backend
nonStreamingResponse :: AppState -> ChatRequest -> (Response -> IO ResponseReceived) -> IO ResponseReceived
nonStreamingResponse state chatReq respond = do
  -- Get backend URL from config
  let backendUrl = "http://localhost:11211/v1/chat/completions"  -- TODO: Get from config
      -- Convert messages to proper JSON format
      messagesJson = map (\msg -> object
        [ "role" .= msgRole msg
        , "content" .= msgContent msg
        ]) (reqMessages chatReq)
      requestBody' = encode $ object
        [ "model" .= reqModel chatReq
        , "messages" .= messagesJson
        , "tools" .= if null (reqTools chatReq) then Nothing else Just (reqTools chatReq)
        , "temperature" .= reqTemperature chatReq
        , "max_tokens" .= reqMaxTokens chatReq
        , "stream" .= False  -- Non-streaming
        ]

  -- Create backend request
  req <- HTTP.parseRequest ("POST " <> backendUrl)
  let req' = req
        { HTTP.requestBody = HTTP.RequestBodyLBS requestBody'
        , HTTP.requestHeaders = [("Content-Type", "application/json")]
        }

  -- Make synchronous request
  response <- HTTP.httpLbs req' (appManager state)
  let responseBody' = HTTP.responseBody response
      status' = HTTP.responseStatus response

  -- Return the response as-is
  respond $ responseLBS status'
    [("Content-Type", "application/json")]
    responseBody'

-- | Handle streaming OpenAI request
handleStreamingRequest :: AppState -> Value -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleStreamingRequest state openAIReq respond = do
  -- Parse OpenAI request
  case parseOpenAIRequest openAIReq of
    Left err -> respond $ responseLBS status400
      [("Content-Type", "application/json")]
      (encode $ object ["error" .= err])

    Right chatReq -> streamResponse state chatReq respond

-- | Stream response from backend by directly proxying HTTP stream
streamResponse :: AppState -> ChatRequest -> (Response -> IO ResponseReceived) -> IO ResponseReceived
streamResponse state chatReq respond = do
  -- Get backend URL from config
  let backendUrl = "http://localhost:11211/v1/chat/completions"  -- TODO: Get from config
      -- Convert messages to proper JSON format
      messagesJson = map (\msg -> object
        [ "role" .= msgRole msg
        , "content" .= msgContent msg
        ]) (reqMessages chatReq)
      requestBody' = encode $ object
        [ "model" .= reqModel chatReq
        , "messages" .= messagesJson
        , "tools" .= if null (reqTools chatReq) then Nothing else Just (reqTools chatReq)
        , "temperature" .= reqTemperature chatReq
        , "max_tokens" .= reqMaxTokens chatReq
        , "stream" .= True
        ]

  -- Create backend request
  req <- HTTP.parseRequest ("POST " <> backendUrl)  -- Explicitly set POST method
  let req' = req
        { HTTP.requestBody = HTTP.RequestBodyLBS requestBody'
        , HTTP.requestHeaders = [("Content-Type", "application/json")]
        }

  -- Create SSE response that directly streams from backend
  let sseResponse = responseStream status200
        [ ("Content-Type", "text/event-stream")
        , ("Cache-Control", "no-cache")
        , ("Connection", "keep-alive")
        ] $ \write flush -> do
          -- Make request with streaming response handler
          withResponse req' (appManager state) $ \backendResp -> do
            let body = HTTP.responseBody backendResp
            -- Stream chunks directly from backend
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
anthropicMessagesHandler _state _req respond =
  respond $ responseLBS status501
    [("Content-Type", "application/json")]
    (encode $ object ["error" .= ("Anthropic endpoint not yet implemented" :: Text)])

-- | /v1beta/models - Gemini list models
geminiListModelsHandler :: AppState -> Application
geminiListModelsHandler _state _req respond =
  respond $ responseLBS status501
    [("Content-Type", "application/json")]
    (encode $ object ["error" .= ("Gemini list models not yet implemented" :: Text)])

-- | /v1beta/models/:model:action - Gemini model actions
geminiModelActionHandler :: AppState -> [Text] -> Application
geminiModelActionHandler _state _modelPath _req respond =
  respond $ responseLBS status501
    [("Content-Type", "application/json")]
    (encode $ object ["error" .= ("Gemini model actions not yet implemented" :: Text)])

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
