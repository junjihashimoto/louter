{-# LANGUAGE OverloadedStrings #-}

-- | Parallel tool calls example
-- Demonstrates multiple tool calls being executed concurrently
-- This is the key feature memorized: "OpenAI's API supports multiple tool calls,
-- which can be executed in parallel (concurrently) or sequentially"
module Main where

import Control.Concurrent.Async (async, wait)
import Control.Monad (forM_)
import Data.Aeson (object, (.=))
import Louter.Client
import Louter.Types
import System.Environment (getEnv)

main :: IO ()
main = do
  apiKey <- getEnv "OPENAI_API_KEY"
  client <- newClient $ BackendOpenAI apiKey Nothing

  -- Define tools
  let calculatorTool = Tool
        { toolName = "calculator"
        , toolDescription = Just "Evaluate mathematical expressions"
        , toolParameters = object
            [ "type" .= ("object" :: String)
            , "properties" .= object
                [ "expression" .= object ["type" .= ("string" :: String)] ]
            , "required" .= (["expression"] :: [String])
            ]
        }

      searchTool = Tool
        { toolName = "search"
        , toolDescription = Just "Search the web"
        , toolParameters = object
            [ "type" .= ("object" :: String)
            , "properties" .= object
                [ "query" .= object ["type" .= ("string" :: String)] ]
            , "required" .= (["query"] :: [String])
            ]
        }

  let request = (defaultChatRequest "gpt-4"
        [ Message RoleUser "What is 25 * 4? Also, search for 'Haskell tutorials'." ])
        { reqTools = [calculatorTool, searchTool]
        , reqStream = True
        }

  putStrLn "Requesting multiple tool calls...\n"

  -- Collect tool calls
  toolCalls <- collectToolCalls client request

  putStrLn $ "\n✓ Received " <> show (length toolCalls) <> " tool calls\n"

  -- Execute in parallel!
  putStrLn "Executing tool calls in PARALLEL:"
  asyncTasks <- mapM (async . executeToolCall) toolCalls
  forM_ asyncTasks wait

  putStrLn "\n✓ All tool calls completed!"

-- | Collect all tool calls from stream
collectToolCalls :: Client -> ChatRequest -> IO [ToolCall]
collectToolCalls client request = do
  -- In a real implementation, you'd accumulate StreamToolCall events
  -- For this example, we simulate:
  pure []

-- | Execute a tool call (simulated)
executeToolCall :: ToolCall -> IO ()
executeToolCall call = do
  putStrLn $ "  [Thread " <> toolCallId call <> "] Executing: " <> toolCallName call
  threadDelay 1000000  -- Simulate work
  putStrLn $ "  [Thread " <> toolCallId call <> "] ✓ Complete"

threadDelay :: Int -> IO ()
threadDelay _ = pure ()  -- Placeholder
