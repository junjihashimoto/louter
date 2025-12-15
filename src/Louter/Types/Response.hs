{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Response types (Protocol-Agnostic Internal Representation)
-- All protocol-specific responses convert FROM this format
module Louter.Types.Response
  ( ChatResponse(..)
  , Choice(..)
  , FinishReason(..)
  , Usage(..)
  , ResponseToolCall(..)
  , FunctionCall(..)
  ) where

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Protocol-agnostic chat response
data ChatResponse = ChatResponse
  { respId :: !Text           -- ^ Response ID
  , respModel :: !Text        -- ^ Model used
  , respChoices :: ![Choice]  -- ^ Response choices
  , respUsage :: !(Maybe Usage) -- ^ Token usage
  } deriving (Show, Eq, Generic)

instance FromJSON ChatResponse
instance ToJSON ChatResponse

-- | A single response choice
data Choice = Choice
  { choiceIndex :: !Int
  , choiceMessage :: !Text          -- ^ Response text (or empty if tool call)
  , choiceToolCalls :: ![ResponseToolCall]  -- ^ Tool/function calls
  , choiceFinishReason :: !(Maybe FinishReason)
  } deriving (Show, Eq, Generic)

instance FromJSON Choice
instance ToJSON Choice

-- | Tool/function call from the model (for non-streaming responses)
data ResponseToolCall = ResponseToolCall
  { rtcId :: !Text
  , rtcType :: !Text          -- ^ Usually "function"
  , rtcFunction :: !FunctionCall
  } deriving (Show, Eq, Generic)

instance FromJSON ResponseToolCall
instance ToJSON ResponseToolCall

-- | Function call details
data FunctionCall = FunctionCall
  { functionName :: !Text
  , functionArguments :: !Text     -- ^ JSON string of arguments
  } deriving (Show, Eq, Generic)

instance FromJSON FunctionCall
instance ToJSON FunctionCall

-- | Why the model stopped generating
data FinishReason
  = FinishStop          -- ^ Natural stop
  | FinishLength        -- ^ Hit max tokens
  | FinishToolCalls     -- ^ Called a tool
  | FinishContentFilter -- ^ Content filtered
  deriving (Show, Eq, Generic)

instance FromJSON FinishReason
instance ToJSON FinishReason

-- | Token usage statistics
data Usage = Usage
  { usagePromptTokens :: !Int
  , usageCompletionTokens :: !Int
  , usageTotalTokens :: !Int
  } deriving (Show, Eq, Generic)

instance FromJSON Usage
instance ToJSON Usage
