{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Interactive CLI for chatting with LLMs via Louter client API
-- Supports MCP (Model Context Protocol) for external tools and resources
module Main where

import Control.Monad (forever, when, unless)
import Data.Aeson (Value(..), Object, encode, eitherDecode, object, (.=))
import qualified Data.Aeson.KeyMap as HM
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative
import System.IO (hFlush, stdout, hSetBuffering, BufferMode(..))
import System.Exit (exitSuccess)

import Louter.Client
import Louter.Client.OpenAI
import Louter.Types.Request
import Louter.Types.Streaming

-- | CLI configuration
data CLIConfig = CLIConfig
  { cliBackendUrl :: Text
  , cliModel :: Text
  , cliApiKey :: Maybe Text
  , cliMcpServers :: [Text]  -- MCP server URLs/commands
  , cliTemperature :: Double
  , cliMaxTokens :: Int
  , cliStreaming :: Bool
  } deriving (Show)

-- | MCP Tool definition
data MCPTool = MCPTool
  { mcpToolName :: Text
  , mcpToolDescription :: Text
  , mcpToolServer :: Text
  , mcpToolSchema :: Value
  } deriving (Show)

-- | CLI state
data CLIState = CLIState
  { stateClient :: Client
  , stateConfig :: CLIConfig
  , stateHistory :: [Message]  -- Conversation history
  , stateMCPTools :: [MCPTool]  -- Available MCP tools
  }

-- | Parse CLI arguments
cliParser :: Parser CLIConfig
cliParser = CLIConfig
  <$> strOption
      ( long "backend"
     <> short 'b'
     <> metavar "URL"
     <> value "http://localhost:11211"
     <> help "Backend LLM server URL (default: llama-server on 11211)" )
  <*> strOption
      ( long "model"
     <> short 'm'
     <> metavar "MODEL"
     <> value "gpt-oss"
     <> help "Model name (default: gpt-oss)" )
  <*> optional (strOption
      ( long "api-key"
     <> short 'k'
     <> metavar "KEY"
     <> help "API key (if required)" ))
  <*> many (strOption
      ( long "mcp-server"
     <> metavar "SERVER"
     <> help "MCP server to connect (can specify multiple)" ))
  <*> option auto
      ( long "temperature"
     <> short 't'
     <> metavar "TEMP"
     <> value 0.7
     <> help "Temperature (default: 0.7)" )
  <*> option auto
      ( long "max-tokens"
     <> metavar "TOKENS"
     <> value 2000
     <> help "Max tokens (default: 2000)" )
  <*> switch
      ( long "stream"
     <> short 's'
     <> help "Enable streaming responses" )

main :: IO ()
main = do
  config <- execParser opts
  runCLI config
  where
    opts = info (cliParser <**> helper)
      ( fullDesc
     <> progDesc "Interactive CLI for LLMs with MCP support"
     <> header "louter-cli - Chat with local/remote LLMs" )

-- | Run the interactive CLI
runCLI :: CLIConfig -> IO ()
runCLI config@CLIConfig{..} = do
  -- Initialize client
  client <- llamaServerClient cliBackendUrl

  -- Initialize MCP tools
  mcpTools <- initMCPServers cliMcpServers

  let initialState = CLIState
        { stateClient = client
        , stateConfig = config
        , stateHistory = []
        , stateMCPTools = mcpTools
        }

  -- Set unbuffered input for interactive experience
  hSetBuffering stdout NoBuffering

  -- Print welcome message
  printWelcome config mcpTools

  -- Start REPL
  repl initialState

-- | Print welcome message
printWelcome :: CLIConfig -> [MCPTool] -> IO ()
printWelcome CLIConfig{..} mcpTools = do
  TIO.putStrLn "╔═══════════════════════════════════════════════════════════╗"
  TIO.putStrLn "║           Louter CLI - LLM Chat with MCP Support         ║"
  TIO.putStrLn "╚═══════════════════════════════════════════════════════════╝"
  TIO.putStrLn ""
  TIO.putStrLn $ "Backend: " <> cliBackendUrl
  TIO.putStrLn $ "Model:   " <> cliModel
  TIO.putStrLn $ "Streaming: " <> (if cliStreaming then "enabled" else "disabled")

  unless (null mcpTools) $ do
    TIO.putStrLn ""
    TIO.putStrLn "MCP Tools available:"
    mapM_ (\tool -> TIO.putStrLn $ "  • " <> mcpToolName tool <> " - " <> mcpToolDescription tool) mcpTools

  TIO.putStrLn ""
  TIO.putStrLn "Commands:"
  TIO.putStrLn "  /help     - Show this help"
  TIO.putStrLn "  /clear    - Clear conversation history"
  TIO.putStrLn "  /history  - Show conversation history"
  TIO.putStrLn "  /tools    - List available MCP tools"
  TIO.putStrLn "  /exit     - Exit the CLI"
  TIO.putStrLn ""

-- | REPL loop
repl :: CLIState -> IO ()
repl state = do
  TIO.putStr "You: "
  hFlush stdout
  input <- TIO.getLine

  let inputText = T.strip input

  -- Handle commands
  if T.null inputText
    then repl state
    else if "/" `T.isPrefixOf` inputText
      then do
        newState <- handleCommand inputText state
        if T.toLower inputText == "/exit"
          then pure ()
          else repl newState
      else do
        -- Handle regular chat message
        newState <- handleMessage inputText state
        repl newState

-- | Handle special commands
handleCommand :: Text -> CLIState -> IO CLIState
handleCommand cmd state@CLIState{..}
  | cmd == "/help" = do
      printWelcome stateConfig stateMCPTools
      pure state

  | cmd == "/clear" = do
      TIO.putStrLn "Conversation history cleared."
      pure state { stateHistory = [] }

  | cmd == "/history" = do
      TIO.putStrLn "\nConversation History:"
      mapM_ printMessage stateHistory
      pure state

  | cmd == "/tools" = do
      TIO.putStrLn "\nAvailable MCP Tools:"
      if null stateMCPTools
        then TIO.putStrLn "  (none)"
        else mapM_ (\tool -> TIO.putStrLn $ "  • " <> mcpToolName tool <> " - " <> mcpToolDescription tool) stateMCPTools
      pure state

  | cmd == "/exit" = do
      TIO.putStrLn "Goodbye!"
      pure state

  | otherwise = do
      TIO.putStrLn $ "Unknown command: " <> cmd
      TIO.putStrLn "Type /help for available commands"
      pure state

-- | Print a message
printMessage :: Message -> IO ()
printMessage msg = do
  let roleStr = case msgRole msg of
        RoleUser -> "You"
        RoleAssistant -> "Assistant"
        RoleSystem -> "System"
        RoleTool -> "Tool"
  TIO.putStrLn $ roleStr <> ": " <> msgContent msg

-- | Handle a chat message
handleMessage :: Text -> CLIState -> IO CLIState
handleMessage userInput state@CLIState{..} = do
  let CLIConfig{..} = stateConfig
      userMessage = Message RoleUser userInput
      newHistory = stateHistory ++ [userMessage]

      -- Build tools list from MCP tools
      tools = map mcpToolToTool stateMCPTools

      request = ChatRequest
        { reqModel = cliModel
        , reqMessages = newHistory
        , reqTools = tools
        , reqToolChoice = ToolChoiceAuto
        , reqTemperature = Just cliTemperature
        , reqMaxTokens = Just cliMaxTokens
        , reqStream = cliStreaming
        }

  TIO.putStr "Assistant: "
  hFlush stdout

  -- Make request
  if cliStreaming
    then do
      -- Streaming response
      assistantContent <- handleStreamingResponse stateClient request
      TIO.putStrLn ""  -- Newline after streaming
      let assistantMessage = Message RoleAssistant assistantContent
      pure state { stateHistory = newHistory ++ [assistantMessage] }
    else do
      -- Non-streaming response
      result <- chatCompletion stateClient request
      case result of
        Left err -> do
          TIO.putStrLn $ "Error: " <> err
          pure state
        Right response -> do
          let content = case respChoices response of
                (choice:_) -> choiceMessage choice
                [] -> ""
          TIO.putStrLn content
          let assistantMessage = Message RoleAssistant content
          pure state { stateHistory = newHistory ++ [assistantMessage] }

-- | Handle streaming response
handleStreamingResponse :: Client -> ChatRequest -> IO Text
handleStreamingResponse client request = do
  contentRef <- newIORef ""

  streamChatWithCallback client request $ \event -> do
    case event of
      StreamContent txt -> do
        TIO.putStr txt
        hFlush stdout
        modifyIORef' contentRef (<> txt)

      StreamReasoning txt -> do
        -- Show reasoning in different color if terminal supports it
        TIO.putStr $ "[thinking: " <> txt <> "] "
        hFlush stdout

      StreamToolCall toolCall -> do
        TIO.putStrLn $ "\n[Tool call: " <> T.pack (show toolCall) <> "]"
        -- TODO: Execute MCP tool call here

      StreamFinish reason -> do
        pure ()

      StreamError err -> do
        TIO.putStrLn $ "\nError: " <> err

  readIORef contentRef

-- | Convert MCP tool to Louter Tool
mcpToolToTool :: MCPTool -> Tool
mcpToolToTool MCPTool{..} = Tool
  { toolName = mcpToolName
  , toolDescription = Just mcpToolDescription
  , toolParameters = mcpToolSchema
  }

-- | Initialize MCP servers
initMCPServers :: [Text] -> IO [MCPTool]
initMCPServers serverSpecs = do
  -- TODO: Implement actual MCP protocol connection
  -- For now, return empty list
  -- In full implementation:
  -- 1. Connect to each MCP server via stdio or HTTP+SSE
  -- 2. Send 'tools/list' request
  -- 3. Parse tool definitions
  -- 4. Return as MCPTool list

  unless (null serverSpecs) $ do
    TIO.putStrLn "Note: MCP support is placeholder - not yet fully implemented"
    TIO.putStrLn "Will connect to:"
    mapM_ (\spec -> TIO.putStrLn $ "  - " <> spec) serverSpecs

  pure []
