{-# LANGUAGE OverloadedStrings #-}

-- | Simple example: Connect to Gemini API using OpenAI-style requests
-- This demonstrates the key feature: "Louter is a protocol converter"
module Main where

import Louter.Client
import Louter.Types
import System.Environment (getEnv)

main :: IO ()
main = do
  -- Get API key from environment
  apiKey <- getEnv "GEMINI_API_KEY"

  -- Create client that connects to Gemini API
  client <- newClient $ BackendGemini apiKey Nothing

  -- Make OpenAI-style request to Gemini!
  let request = defaultChatRequest "gemini-pro"
        [ Message RoleUser "Hello! Tell me a short joke." ]

  putStrLn "Making request to Gemini API using OpenAI-style request..."

  -- Non-streaming response
  result <- chatCompletion client request
  case result of
    Left err -> putStrLn $ "Error: " <> show err
    Right response -> do
      putStrLn "\nResponse:"
      case respChoices response of
        (choice:_) -> putStrLn $ choiceMessage choice
        [] -> putStrLn "(no response)"

  putStrLn "\n✓ Success! We called Gemini API with an OpenAI-style request!"
