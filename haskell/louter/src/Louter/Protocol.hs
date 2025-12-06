{-# LANGUAGE OverloadedStrings #-}

-- | Protocol conversion module
-- Provides unified interface for converting between all supported protocols
module Louter.Protocol
  ( -- * Protocol Types
    Protocol(..)
  , parseChunk
  , requestToIR
  , irToResponse
    -- * Re-exports
  , module Louter.Protocol.OpenAI
  , module Louter.Protocol.Gemini
  , module Louter.Protocol.Anthropic
  ) where

import Data.Aeson (Value)
import Data.Text (Text)
import Louter.Protocol.Anthropic
import Louter.Protocol.Gemini
import Louter.Protocol.OpenAI
import Louter.Types
import Louter.Types.Streaming

-- | Supported protocols
data Protocol
  = ProtocolOpenAI
  | ProtocolGemini
  | ProtocolAnthropic
  deriving (Show, Eq)

-- | Parse a streaming chunk based on protocol
parseChunk :: Protocol -> Text -> Either Text DeltaType
parseChunk ProtocolOpenAI = parseOpenAIChunk
parseChunk ProtocolGemini = parseGeminiChunk
parseChunk ProtocolAnthropic = parseAnthropicChunk

-- | Convert protocol-specific request to Internal Representation
requestToIR :: Protocol -> Value -> Either Text ChatRequest
requestToIR ProtocolOpenAI = openAIRequestToIR
requestToIR ProtocolGemini = geminiRequestToIR
requestToIR ProtocolAnthropic = anthropicRequestToIR

-- | Convert Internal Representation to protocol-specific response
irToResponse :: Protocol -> ChatResponse -> Value
irToResponse ProtocolOpenAI = irToOpenAIResponse
irToResponse ProtocolGemini = irToGeminiResponse
irToResponse ProtocolAnthropic = irToAnthropicResponse
