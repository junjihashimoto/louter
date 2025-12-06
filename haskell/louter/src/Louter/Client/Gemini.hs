{-# LANGUAGE OverloadedStrings #-}

-- | Gemini-specific client helpers
module Louter.Client.Gemini
  ( geminiClient
  , geminiClientWithUrl
  ) where

import Data.Text (Text)
import Louter.Client (Backend(..), Client, newClient)

-- | Create a Gemini client with API key (requires authentication)
geminiClient :: Text -> IO Client
geminiClient apiKey = newClient $ BackendGemini apiKey Nothing True

-- | Create a Gemini client with custom base URL (requires authentication)
geminiClientWithUrl :: Text -> Text -> IO Client
geminiClientWithUrl apiKey baseUrl = newClient $ BackendGemini apiKey (Just baseUrl) True
