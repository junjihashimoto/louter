{-# LANGUAGE OverloadedStrings #-}

-- | Streaming example: Real-time text generation with automatic buffering
module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Conduit ((.|), runConduit)
import qualified Data.Conduit.List as CL
import Louter.Client
import Louter.Types
import System.Environment (getEnv)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  apiKey <- getEnv "OPENAI_API_KEY"
  client <- newClient $ BackendOpenAI apiKey Nothing

  let request = (defaultChatRequest "gpt-4"
        [ Message RoleUser "Count from 1 to 10, explaining each number." ])
        { reqStream = True }

  putStrLn "Streaming response (watch it appear in real-time):\n"

  -- Stream with conduit
  runConduit $
    streamChat client request
    .| CL.mapM_ (liftIO . handleEvent)

  putStrLn "\n\n✓ Streaming complete!"

handleEvent :: StreamEvent -> IO ()
handleEvent (StreamContent txt) = do
  putStr txt
  hFlush stdout
handleEvent (StreamReasoning txt) = do
  putStr $ "[thinking: " <> txt <> "] "
  hFlush stdout
handleEvent (StreamToolCall call) = do
  putStrLn $ "\n[Tool Call: " <> toolCallName call <> "]"
handleEvent (StreamFinish reason) = do
  putStrLn $ "\n[Finished: " <> reason <> "]"
handleEvent (StreamError err) = do
  putStrLn $ "\n[Error: " <> err <> "]"
