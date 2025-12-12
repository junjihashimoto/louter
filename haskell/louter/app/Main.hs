{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Louter proxy server executable
-- Routes requests between different LLM protocols using raw WAI
module Main where

import Control.Monad (foldM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value(..), encode, eitherDecode, object, (.=))
import qualified Data.Aeson.KeyMap as HM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Builder (Builder, byteString)
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
import System.Random (randomIO)
import Data.Word (Word64)
import Text.Printf (printf)

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

-- | Convert Anthropic request to OpenAI format
anthropicToOpenAI :: Value -> Either Text Value
anthropicToOpenAI (Object obj) = do
  -- Extract messages
  messages <- case HM.lookup "messages" obj of
    Just (Array msgs) -> Right $ V.toList msgs
    _ -> Left "Missing 'messages' field"

  -- Extract max_tokens
  let maxTokens = case HM.lookup "max_tokens" obj of
        Just (Number n) -> Just (floor n :: Int)
        _ -> Nothing

  -- Extract optional fields
  let temperature = case HM.lookup "temperature" obj of
        Just (Number n) -> Just (realToFrac n :: Double)
        _ -> Nothing

  let model = case HM.lookup "model" obj of
        Just (String m) -> m
        _ -> "gpt-4"

  -- Extract system message if present and prepend to messages
  let systemMsg = case HM.lookup "system" obj of
        Just (String sys) -> [object ["role" .= ("system" :: Text), "content" .= sys]]
        _ -> []

  -- Extract tools if present
  let tools = case HM.lookup "tools" obj of
        Just (Array ts) -> Just (Array ts)
        _ -> Nothing

  Right $ object $
    [ "model" .= model
    , "messages" .= (systemMsg ++ messages)
    , "max_tokens" .= maxTokens
    , "temperature" .= temperature
    , "stream" .= True
    ] ++ case tools of
          Just t -> ["tools" .= t]
          Nothing -> []

anthropicToOpenAI _ = Left "Request must be a JSON object"

-- | Convert OpenAI SSE stream to Anthropic SSE format
convertOpenAIToAnthropic :: (Builder -> IO ()) -> IO () -> HTTP.BodyReader -> IO ()
convertOpenAIToAnthropic write flush bodyReader = do
  -- Send message_start event
  write (byteString $ BS8.pack "event: message_start\ndata: {\"type\":\"message_start\",\"message\":{\"id\":\"msg_1\",\"type\":\"message\",\"role\":\"assistant\",\"content\":[],\"model\":\"claude-3-haiku-20240307\",\"stop_reason\":null,\"usage\":{\"input_tokens\":10,\"output_tokens\":0}}}\n\n")
  flush

  -- Send content_block_start
  write (byteString $ BS8.pack "event: content_block_start\ndata: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}\n\n")
  flush

  -- Stream content deltas
  streamContentDeltas write flush bodyReader 0

  -- Send content_block_stop
  write (byteString $ BS8.pack "event: content_block_stop\ndata: {\"type\":\"content_block_stop\",\"index\":0}\n\n")
  flush

  -- Send message_delta with stop_reason
  write (byteString $ BS8.pack "event: message_delta\ndata: {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\",\"stop_sequence\":null},\"usage\":{\"output_tokens\":10}}\n\n")
  flush

  -- Send message_stop
  write (byteString $ BS8.pack "event: message_stop\ndata: {\"type\":\"message_stop\"}\n\n")
  flush

-- | Stream content deltas from OpenAI response
streamContentDeltas :: (Builder -> IO ()) -> IO () -> HTTP.BodyReader -> Int -> IO ()
streamContentDeltas write flush bodyReader contentIndex = loop BS.empty
  where
    loop acc = do
      chunk <- brRead bodyReader
      if BS.null chunk
        then pure ()
        else do
          let combined = acc <> chunk
              lines' = BS.split (fromIntegral $ fromEnum '\n') combined
          case lines' of
            [] -> loop BS.empty
            [incomplete] -> loop incomplete
            _ -> do
              let (completeLines, rest) = (init lines', last lines')
              mapM_ (processOpenAILine write flush contentIndex) completeLines
              loop rest

-- | Process a single OpenAI SSE line and convert to Anthropic format
processOpenAILine :: (Builder -> IO ()) -> IO () -> Int -> BS.ByteString -> IO ()
processOpenAILine write flush contentIndex line
  | BS.isPrefixOf "data: " line = do
      let jsonText = TE.decodeUtf8 $ BS.drop 6 line
      if jsonText == "[DONE]"
        then pure ()  -- Don't send [DONE] in Anthropic format
        else case eitherDecode (BL.fromStrict $ TE.encodeUtf8 jsonText) of
          Right (Object openAIChunk) -> do
            -- Extract content delta
            case HM.lookup "choices" openAIChunk of
              Just (Array choices) | not (V.null choices) -> do
                case V.head choices of
                  Object choice -> do
                    case HM.lookup "delta" choice of
                      Just (Object delta) -> do
                        -- Check for content
                        case HM.lookup "content" delta of
                          Just (String content) -> do
                            let anthropicEvent = object
                                  [ "type" .= ("content_block_delta" :: Text)
                                  , "index" .= contentIndex
                                  , "delta" .= object
                                      [ "type" .= ("text_delta" :: Text)
                                      , "text" .= content
                                      ]
                                  ]
                            write (byteString $ BS8.pack "event: content_block_delta\ndata: " <> BL.toStrict (encode anthropicEvent) <> BS8.pack "\n\n")
                            flush
                          _ -> pure ()

                        -- Check for reasoning (treat as content for now)
                        case HM.lookup "reasoning" delta of
                          Just (String reasoning) -> do
                            let anthropicEvent = object
                                  [ "type" .= ("content_block_delta" :: Text)
                                  , "index" .= contentIndex
                                  , "delta" .= object
                                      [ "type" .= ("text_delta" :: Text)
                                      , "text" .= reasoning
                                      ]
                                  ]
                            write (byteString $ BS8.pack "event: content_block_delta\ndata: " <> BL.toStrict (encode anthropicEvent) <> BS8.pack "\n\n")
                            flush
                          _ -> pure ()
                      _ -> pure ()
                  _ -> pure ()
              _ -> pure ()
          _ -> pure ()
  | otherwise = pure ()

-- | Convert OpenAI non-streaming response to Anthropic format
openAIResponseToAnthropic :: Value -> Value
openAIResponseToAnthropic (Object openAIResp) =
  let content = case HM.lookup "choices" openAIResp of
        Just (Array choices) | not (V.null choices) ->
          case V.head choices of
            Object choice -> case HM.lookup "message" choice of
              Just (Object msg) -> case HM.lookup "content" msg of
                Just (String txt) -> [object ["type" .= ("text" :: Text), "text" .= txt]]
                _ -> []
              _ -> []
            _ -> []
        _ -> []
  in object
      [ "id" .= ("msg_1" :: Text)
      , "type" .= ("message" :: Text)
      , "role" .= ("assistant" :: Text)
      , "content" .= content
      , "model" .= ("claude-3-haiku-20240307" :: Text)
      , "stop_reason" .= ("end_turn" :: Text)
      , "usage" .= object
          [ "input_tokens" .= (10 :: Int)
          , "output_tokens" .= (10 :: Int)
          ]
      ]
openAIResponseToAnthropic _ = object []

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

-- | Convert Gemini request to OpenAI format
geminiToOpenAI :: Text -> Bool -> Value -> Either Text Value
geminiToOpenAI modelName streaming (Object obj) = do
  -- Extract contents array
  contents <- case HM.lookup "contents" obj of
    Just (Array cs) -> Right $ V.toList cs
    _ -> Left "Missing 'contents' field"

  -- Convert contents to OpenAI messages
  messages <- mapM convertGeminiContentToMessage contents

  -- Extract system instruction if present
  let systemMsg = case HM.lookup "systemInstruction" obj of
        Just (Object sysInst) -> case HM.lookup "parts" sysInst of
          Just (Array parts) | not (V.null parts) ->
            case V.head parts of
              Object part -> case HM.lookup "text" part of
                Just (String txt) -> [object ["role" .= ("system" :: Text), "content" .= txt]]
                _ -> []
              _ -> []
          _ -> []
        _ -> []

  -- Extract generation config
  let temperature = case HM.lookup "generationConfig" obj of
        Just (Object cfg) -> HM.lookup "temperature" cfg
        _ -> Nothing

  let maxTokens = case HM.lookup "generationConfig" obj of
        Just (Object cfg) -> HM.lookup "maxOutputTokens" cfg
        _ -> Nothing

  -- Extract tools
  let tools = case HM.lookup "tools" obj of
        Just (Array ts) -> Just $ V.toList ts
        _ -> Nothing

  Right $ object $
    [ "model" .= modelName
    , "messages" .= (systemMsg ++ messages)
    , "stream" .= streaming
    ] ++ (case temperature of Just t -> ["temperature" .= t]; Nothing -> [])
      ++ (case maxTokens of Just m -> ["max_tokens" .= m]; Nothing -> [])
      ++ (case tools of Just t -> ["tools" .= convertGeminiToolsToOpenAI t]; Nothing -> [])

geminiToOpenAI _ _ _ = Left "Request must be a JSON object"

-- | Convert Gemini content to OpenAI message
convertGeminiContentToMessage :: Value -> Either Text Value
convertGeminiContentToMessage (Object content) = do
  role <- case HM.lookup "role" content of
    Just (String r) -> Right r
    _ -> Right "user"  -- Default to user

  -- Convert Gemini roles to OpenAI roles
  -- Gemini uses "model", OpenAI uses "assistant"
  let openAIRole = if role == "model" then "assistant" else role

  parts <- case HM.lookup "parts" content of
    Just (Array ps) -> Right $ V.toList ps
    _ -> Left "Missing 'parts' in content"

  -- Check if any part is a functionResponse (tool result)
  let hasFunctionResponse = any isFunctionResponse parts

  if hasFunctionResponse && not (null parts)
    then do
      -- Convert function response to OpenAI tool message format
      -- Gemini can have multiple function responses in one message
      let toolMessages = map convertFunctionResponsePart (filter isFunctionResponse parts)
      -- For now, return the first tool message (OpenAI expects one tool result per message)
      case toolMessages of
        (msg:_) -> Right msg
        [] -> Left "Function response part missing required fields"
    else do
      -- Regular text content
      let textContent = case parts of
            (Object part : _) -> case HM.lookup "text" part of
              Just (String txt) -> txt
              _ -> ""
            _ -> ""

      Right $ object
        [ "role" .= openAIRole
        , "content" .= textContent
        ]
  where
    isFunctionResponse (Object part) = HM.member "functionResponse" part
    isFunctionResponse _ = False

    convertFunctionResponsePart (Object part) =
      case HM.lookup "functionResponse" part of
        Just (Object funcResp) ->
          let funcName = case HM.lookup "name" funcResp of
                Just (String n) -> n
                _ -> "unknown"
              funcResult = case HM.lookup "response" funcResp of
                Just resp -> encode resp
                _ -> "{}"
              -- Generate a tool_call_id (in real Gemini API, this would come from the original call)
              -- For now, use the function name as ID
              toolCallId = funcName <> "_result"
          in object
              [ "role" .= ("tool" :: Text)
              , "content" .= TE.decodeUtf8 (BL.toStrict funcResult)
              , "tool_call_id" .= toolCallId
              ]
        _ -> object []
    convertFunctionResponsePart _ = object []

convertGeminiContentToMessage _ = Left "Content must be a JSON object"

-- | Convert Gemini tools to OpenAI format
convertGeminiToolsToOpenAI :: [Value] -> [Value]
convertGeminiToolsToOpenAI = concatMap convertTool
  where
    convertTool (Object tool) =
      case HM.lookup "functionDeclarations" tool of
        Just (Array funcs) -> map (\func -> object
          [ "type" .= ("function" :: Text)
          , "function" .= convertFunctionDeclaration func
          ]) (V.toList funcs)
        _ -> []
    convertTool _ = []

    -- Convert Gemini function declaration to OpenAI format
    -- Rename "parametersJsonSchema" to "parameters"
    convertFunctionDeclaration (Object funcObj) =
      let renamedObj = case HM.lookup "parametersJsonSchema" funcObj of
            Just params -> HM.insert "parameters" params (HM.delete "parametersJsonSchema" funcObj)
            Nothing -> funcObj
      in Object renamedObj
    convertFunctionDeclaration other = other

-- | State for tracking tool call arguments during streaming
data ToolCallState = ToolCallState
  { toolCallId :: Maybe Text
  , toolCallName :: Maybe Text
  , toolCallArgs :: Text  -- Accumulated arguments string
  } deriving (Show)

-- | Convert OpenAI SSE stream to Gemini newline-delimited JSON format
convertOpenAIToGeminiStream :: (Builder -> IO ()) -> IO () -> HTTP.BodyReader -> IO ()
convertOpenAIToGeminiStream write flush bodyReader = do
  streamGeminiDeltas write flush bodyReader

-- | Stream Gemini deltas from OpenAI response
streamGeminiDeltas :: (Builder -> IO ()) -> IO () -> HTTP.BodyReader -> IO ()
streamGeminiDeltas write flush bodyReader = loop BS.empty (ToolCallState Nothing Nothing "")
  where
    loop acc toolState = do
      chunk <- brRead bodyReader
      if BS.null chunk
        then pure ()
        else do
          let combined = acc <> chunk
              lines' = BS.split (fromIntegral $ fromEnum '\n') combined
          case lines' of
            [] -> loop BS.empty toolState
            [incomplete] -> loop incomplete toolState
            _ -> do
              let (completeLines, rest) = (init lines', last lines')
              newToolState <- foldM (processOpenAILineToGeminiStateful write flush) toolState completeLines
              loop rest newToolState

-- | Process a single OpenAI SSE line with state tracking for tool calls
processOpenAILineToGeminiStateful :: (Builder -> IO ()) -> IO () -> ToolCallState -> BS.ByteString -> IO ToolCallState
processOpenAILineToGeminiStateful write flush toolState line
  | BS.isPrefixOf "data: " line = do
      let jsonText = TE.decodeUtf8 $ BS.drop 6 line
      if jsonText == "[DONE]"
        then pure toolState  -- Gemini doesn't send [DONE]
        else case eitherDecode (BL.fromStrict $ TE.encodeUtf8 jsonText) of
          Right (Object openAIChunk) -> do
            -- Check if this chunk contains tool_calls
            let hasToolCalls = case HM.lookup "choices" openAIChunk of
                  Just (Array choices) | not (V.null choices) ->
                    case V.head choices of
                      Object choice -> case HM.lookup "delta" choice of
                        Just (Object delta) -> HM.member "tool_calls" delta
                        _ -> False
                      _ -> False
                  _ -> False

            -- Check if this is a finish_reason = "tool_calls" chunk with buffered state
            let finishReason = case HM.lookup "choices" openAIChunk of
                  Just (Array choices) | not (V.null choices) ->
                    case V.head choices of
                      Object choice -> HM.lookup "finish_reason" choice
                      _ -> Nothing
                  _ -> Nothing
                hasBufferedToolCall = toolCallName toolState /= Nothing

            if hasToolCalls
              then do
                -- Process tool call and update state
                (newState, maybeGeminiChunk) <- processToolCallChunk toolState openAIChunk
                case maybeGeminiChunk of
                  Just geminiChunk -> do
                    write (byteString $ BS8.pack "data: " <> BL.toStrict (encode geminiChunk) <> BS8.pack "\n\n")
                    flush
                  Nothing -> pure ()
                pure newState
              else if finishReason == Just (String "tool_calls") && hasBufferedToolCall
                then do
                  -- Emit buffered tool call
                  (newState, maybeGeminiChunk) <- processToolCallChunk toolState openAIChunk
                  case maybeGeminiChunk of
                    Just geminiChunk -> do
                      write (byteString $ BS8.pack "data: " <> BL.toStrict (encode geminiChunk) <> BS8.pack "\n\n")
                      flush
                    Nothing -> pure ()
                  pure newState
                else do
                  -- Regular text/reasoning chunk
                  let geminiChunk = openAIChunkToGemini openAIChunk
                  write (byteString $ BS8.pack "data: " <> BL.toStrict (encode geminiChunk) <> BS8.pack "\n\n")
                  flush
                  pure toolState
          _ -> pure toolState
  | otherwise = pure toolState

-- | Process tool call chunk, accumulating arguments until complete
processToolCallChunk :: ToolCallState -> HM.KeyMap Value -> IO (ToolCallState, Maybe Value)
processToolCallChunk state openAIChunk = do
  let choices = case HM.lookup "choices" openAIChunk of
        Just (Array cs) | not (V.null cs) -> V.head cs
        _ -> Object HM.empty
      delta = case choices of
        Object choice -> case HM.lookup "delta" choice of
          Just (Object d) -> d
          _ -> HM.empty
        _ -> HM.empty
      toolCalls = case HM.lookup "tool_calls" delta of
        Just (Array tcs) | not (V.null tcs) -> Just $ V.head tcs
        _ -> Nothing
      finishReason = case choices of
        Object choice -> HM.lookup "finish_reason" choice
        _ -> Nothing

  -- Check if we should emit based on finish_reason, even without new tool_calls
  case finishReason of
    Just (String "tool_calls") | toolCallName state /= Nothing -> do
      -- Arguments are complete, emit the buffered function call
      let parsedArgs = case eitherDecode (BL.fromStrict $ TE.encodeUtf8 (toolCallArgs state)) of
            Right val -> val
            Left _ -> object []
          geminiChunk = object
            [ "candidates" .= [object
                [ "content" .= object
                    [ "parts" .= [object $
                        [ "functionCall" .= object
                            ([ "name" .= n | Just n <- [toolCallName state] ] ++
                             [ "args" .= parsedArgs ])
                        ] ++ [ "id" .= i | Just i <- [toolCallId state] ]]
                    , "role" .= ("model" :: Text)
                    ]
                , "finishReason" .= ("tool_calls" :: Text)
                ]]
            , "usageMetadata" .= object
                [ "promptTokenCount" .= (0 :: Int)
                , "candidatesTokenCount" .= (0 :: Int)
                , "totalTokenCount" .= (0 :: Int)
                ]
            ]
      -- Reset state for next tool call
      pure (ToolCallState Nothing Nothing "", Just geminiChunk)
    _ -> do
      -- Process new tool_calls delta if present
      case toolCalls of
        Just (Object tc) -> do
          let tcId = case HM.lookup "id" tc of
                Just (String i) -> Just i
                _ -> Nothing
              tcFunc = case HM.lookup "function" tc of
                Just (Object f) -> f
                _ -> HM.empty
              funcName = case HM.lookup "name" tcFunc of
                Just (String n) -> Just n
                _ -> Nothing
              funcArgs = case HM.lookup "arguments" tcFunc of
                Just (String args) -> args
                _ -> ""

          -- Update state with new information
          let newId = case tcId of Just i -> Just i; Nothing -> toolCallId state
              newName = case funcName of Just n -> Just n; Nothing -> toolCallName state
              newArgs = toolCallArgs state <> funcArgs

          -- Still accumulating, don't emit yet
          pure (ToolCallState newId newName newArgs, Nothing)
        _ -> pure (state, Nothing)

-- | Process a single OpenAI SSE line and convert to Gemini SSE (stateless version, kept for compatibility)
processOpenAILineToGemini :: (Builder -> IO ()) -> IO () -> BS.ByteString -> IO ()
processOpenAILineToGemini write flush line = do
  _ <- processOpenAILineToGeminiStateful write flush (ToolCallState Nothing Nothing "") line
  pure ()

-- | Convert OpenAI chunk to Gemini chunk
openAIChunkToGemini :: HM.KeyMap Value -> Value
openAIChunkToGemini openAIChunk =
  let candidates = case HM.lookup "choices" openAIChunk of
        Just (Array choices) | not (V.null choices) ->
          V.toList $ V.map convertChoice choices
        _ -> []
  in object
      [ "candidates" .= candidates
      , "usageMetadata" .= object
          [ "promptTokenCount" .= (0 :: Int)
          , "candidatesTokenCount" .= (0 :: Int)
          , "totalTokenCount" .= (0 :: Int)
          ]
      ]
  where
    convertChoice (Object choice) =
      let delta = case HM.lookup "delta" choice of
            Just (Object d) -> d
            _ -> HM.empty
          finishReason = HM.lookup "finish_reason" choice
          -- Extract text from either content or reasoning
          textParts = case (HM.lookup "content" delta, HM.lookup "reasoning" delta) of
            (Just (String txt), _) -> [object ["text" .= txt]]
            (_, Just (String txt)) -> [object ["text" .= txt]]
            _ -> []
          -- Extract tool calls and convert to Gemini functionCall format
          toolCallParts = case HM.lookup "tool_calls" delta of
            Just (Array toolCalls) -> V.toList $ V.map convertToolCall toolCalls
            _ -> []
          parts = textParts ++ toolCallParts
      in object $
          [ "content" .= object
              [ "parts" .= parts
              , "role" .= ("model" :: Text)
              ]
          ] ++ (case finishReason of
                  Just r -> ["finishReason" .= r]
                  Nothing -> [])
    convertChoice _ = object []

    -- Convert OpenAI tool_call to Gemini functionCall part
    convertToolCall (Object tc) =
      let tcId = HM.lookup "id" tc
          tcFunc = case HM.lookup "function" tc of
            Just (Object f) -> f
            _ -> HM.empty
          funcName = HM.lookup "name" tcFunc
          funcArgs = HM.lookup "arguments" tcFunc
      in object $
          [ "functionCall" .= object
              ([ "name" .= fname | Just fname <- [funcName] ] ++
               [ "args" .= parseArgs args | Just args <- [funcArgs] ])
          ] ++ [ "id" .= tid | Just tid <- [tcId] ]
    convertToolCall _ = object []

    -- Parse function arguments string to JSON object
    parseArgs (String argsStr) = case eitherDecode (BL.fromStrict $ TE.encodeUtf8 argsStr) of
      Right val -> val
      Left _ -> object []
    parseArgs other = other

-- | Convert OpenAI non-streaming response to Gemini format
openAIResponseToGemini :: Value -> Value
openAIResponseToGemini (Object openAIResp) =
  let candidates = case HM.lookup "choices" openAIResp of
        Just (Array choices) | not (V.null choices) ->
          V.toList $ V.map convertChoice choices
        _ -> []
  in object
      [ "candidates" .= candidates
      , "usageMetadata" .= object
          [ "promptTokenCount" .= (0 :: Int)
          , "candidatesTokenCount" .= (0 :: Int)
          , "totalTokenCount" .= (0 :: Int)
          ]
      ]
  where
    convertChoice (Object choice) =
      let message = case HM.lookup "message" choice of
            Just (Object m) -> m
            _ -> HM.empty
          finishReason = HM.lookup "finish_reason" choice
          content = HM.lookup "content" message
          parts = case content of
            Just (String txt) -> [object ["text" .= txt]]
            _ -> []
      in object $
          [ "content" .= object
              [ "parts" .= parts
              , "role" .= ("model" :: Text)
              ]
          ] ++ (case finishReason of
                  Just r -> ["finishReason" .= r]
                  Nothing -> [])
    convertChoice _ = object []
openAIResponseToGemini _ = object []

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
