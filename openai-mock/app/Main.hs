{-# LANGUAGE OverloadedStrings #-}

-- | Mock OpenAI server that replays captured test data
-- Usage: openai-mock --test-data ./test-data --port 11211
module Main where

import Control.Monad (forM_)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network.HTTP.Types (status200, status404)
import Network.Wai (Application, Response, responseLBS, responseFile, pathInfo, rawPathInfo)
import Network.Wai.Handler.Warp (run)
import Options.Applicative
import System.Directory (doesFileExist, listDirectory)
import System.FilePath ((</>), takeExtension)

data Config = Config
  { configTestDataDir :: FilePath
  , configPort :: Int
  } deriving (Show)

configParser :: Parser Config
configParser = Config
  <$> strOption
      ( long "test-data"
     <> metavar "DIR"
     <> value "./test-data/openai"
     <> help "Test data directory" )
  <*> option auto
      ( long "port"
     <> metavar "PORT"
     <> value 11211
     <> help "Port to listen on" )

main :: IO ()
main = do
  config <- execParser opts
  putStrLn $ "Starting OpenAI mock server on port " <> show (configPort config)
  putStrLn $ "Serving test data from: " <> configTestDataDir config
  putStrLn ""

  -- List available test scenarios
  scenarios <- listDirectory (configTestDataDir config)
  putStrLn "Available test scenarios:"
  forM_ scenarios $ \scenario -> do
    let path = configTestDataDir config </> scenario
    hasRequest <- doesFileExist (path </> "request.json")
    hasResponse <- doesFileExist (path </> "sample_response.txt")
    when (hasRequest || hasResponse) $
      putStrLn $ "  - " <> scenario

  putStrLn ""
  putStrLn "Ready to accept requests..."
  run (configPort config) (app config)
  where
    opts = info (configParser <**> helper)
      ( fullDesc
     <> progDesc "Mock OpenAI server that replays test data"
     <> header "openai-mock - deterministic LLM testing" )

-- | WAI Application
app :: Config -> Application
app config req respond = do
  let path = pathInfo req
  case path of
    ["v1", "chat", "completions"] -> serveChatCompletion config req respond
    _ -> respond $ responseLBS status404 [] "Not Found"

-- | Serve chat completion endpoint
serveChatCompletion :: Config -> Application
serveChatCompletion config req respond = do
  -- Determine which test scenario to use
  -- For now, use streaming_function_calling if available
  let testDir = configTestDataDir config </> "streaming_function_calling"
      responseFile = testDir </> "sample_response.txt"

  exists <- doesFileExist responseFile
  if exists
    then do
      -- Read and stream the response
      content <- BL.readFile responseFile
      let lines = BL.split (fromIntegral $ fromEnum '\n') content
          sseLines = filter (BL.isPrefixOf "data: ") lines

      -- For simplicity, return all at once
      -- In a real mock, we'd stream line by line
      respond $ responseLBS status200
        [ ("Content-Type", "text/event-stream")
        , ("Cache-Control", "no-cache")
        , ("Connection", "keep-alive")
        ]
        (BL.intercalate "\n" sseLines <> "\n")
    else
      respond $ responseLBS status404 [] "Test data not found"

when :: Bool -> IO () -> IO ()
when True action = action
when False _ = pure ()
