{-# LANGUAGE OverloadedStrings #-}

-- | OpenAI-specific client helpers
module Louter.Client.OpenAI
  ( openAIClient
  , openAIClientWithUrl
  , llamaServerClient
  ) where

import Data.Text (Text)
import Louter.Client (Backend(..), Client, newClient)

-- | Create an OpenAI client with API key (requires authentication)
openAIClient :: Text -> IO Client
openAIClient apiKey = newClient $ BackendOpenAI apiKey Nothing True

-- | Create an OpenAI client with custom base URL (requires authentication)
openAIClientWithUrl :: Text -> Text -> IO Client
openAIClientWithUrl apiKey baseUrl = newClient $ BackendOpenAI apiKey (Just baseUrl) True

-- | Create a client for llama-server (no authentication required)
-- Example: llamaServerClient "http://localhost:11211"
llamaServerClient :: Text -> IO Client
llamaServerClient baseUrl = newClient $ BackendOpenAI "" (Just baseUrl) False
