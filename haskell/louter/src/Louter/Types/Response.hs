{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Response types (Protocol-Agnostic Internal Representation)
-- All protocol-specific responses convert FROM this format
module Louter.Types.Response
  ( ChatResponse(..)
  , Choice(..)
  , FinishReason(..)
  , Usage(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
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
  , choiceFinishReason :: !(Maybe FinishReason)
  } deriving (Show, Eq, Generic)

instance FromJSON Choice
instance ToJSON Choice

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
