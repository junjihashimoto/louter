{-# LANGUAGE OverloadedStrings #-}

-- | Anthropic-specific client helpers
module Louter.Client.Anthropic
  ( anthropicClient
  , anthropicClientWithUrl
  ) where

import Data.Text (Text)
import Louter.Client (Backend(..), Client, newClient)

-- | Create an Anthropic client with API key (requires authentication)
anthropicClient :: Text -> IO Client
anthropicClient apiKey = newClient $ BackendAnthropic apiKey Nothing True

-- | Create an Anthropic client with custom base URL (requires authentication)
anthropicClientWithUrl :: Text -> Text -> IO Client
anthropicClientWithUrl apiKey baseUrl = newClient $ BackendAnthropic apiKey (Just baseUrl) True
