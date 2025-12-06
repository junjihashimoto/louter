{-# LANGUAGE OverloadedStrings #-}

-- | Louter proxy server executable
-- Routes requests between different LLM protocols
module Main where

import Options.Applicative

data ServerConfig = ServerConfig
  { serverPort :: Int
  , serverConfigFile :: FilePath
  } deriving (Show)

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

main :: IO ()
main = do
  config <- execParser opts
  putStrLn $ "Louter proxy server starting on port " <> show (serverPort config)
  putStrLn $ "Using config file: " <> serverConfigFile config
  putStrLn ""
  putStrLn "Note: Full proxy server implementation is in progress."
  putStrLn "For now, use the library directly in your Haskell applications."
  putStrLn ""
  putStrLn "Example usage as library:"
  putStrLn "  import Louter.Client"
  putStrLn "  import Louter.Client.OpenAI (llamaServerClient)"
  putStrLn "  import Louter.Client.Gemini (geminiClient)"
  putStrLn ""
  putStrLn "  -- For cloud APIs (with API key):"
  putStrLn "  client <- geminiClient \"your-api-key\""
  putStrLn "  response <- chatCompletion client request"
  putStrLn ""
  putStrLn "  -- For local llama-server (no API key needed):"
  putStrLn "  client <- llamaServerClient \"http://localhost:11211\""
  putStrLn "  response <- chatCompletion client request"
  where
    opts = info (serverConfigParser <**> helper)
      ( fullDesc
     <> progDesc "Multi-protocol LLM proxy server"
     <> header "louter-server - protocol converter for LLM APIs" )
