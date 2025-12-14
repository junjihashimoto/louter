{-# LANGUAGE OverloadedStrings #-}

-- | Tool format types for backend configuration
-- Supports both JSON (OpenAI/Anthropic/Gemini) and XML (Qwen) tool calling formats
module Louter.Types.ToolFormat
  ( ToolFormat(..)
  , XMLMode(..)
  , XMLToolCallState(..)
  , initialXMLState
  ) where

import Data.Text (Text)
import qualified Data.HashMap.Strict as HM
import Data.Aeson (Value)

-- | Tool call format supported by backend
data ToolFormat
  = ToolFormatJSON  -- ^ Standard OpenAI JSON format (default)
  | ToolFormatXML   -- ^ Qwen3-Coder XML format
  deriving (Show, Eq)

-- | XML parsing state machine mode
data XMLMode
  = NormalText    -- ^ Emitting regular text chunks
  | InToolCall    -- ^ Buffering XML content inside <tool_call> tags
  deriving (Show, Eq)

-- | State for XML tool call streaming parser
data XMLToolCallState = XMLToolCallState
  { xmlMode :: XMLMode
    -- ^ Current parsing mode (NormalText or InToolCall)
  , xmlBuffer :: Text
    -- ^ Accumulated XML content (only used in InToolCall mode)
  , xmlAccumulatedText :: Text
    -- ^ Regular text content accumulated (outside tool calls)
  , xmlExtractedCalls :: [(Text, HM.HashMap Text Value)]
    -- ^ Completed tool calls: (function_name, parameters)
  } deriving (Show, Eq)

-- | Initial XML parsing state
initialXMLState :: XMLToolCallState
initialXMLState = XMLToolCallState
  { xmlMode = NormalText
  , xmlBuffer = ""
  , xmlAccumulatedText = ""
  , xmlExtractedCalls = []
  }
