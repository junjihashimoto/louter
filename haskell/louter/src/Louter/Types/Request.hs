{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Request types (Protocol-Agnostic Internal Representation)
-- All protocol-specific requests convert TO this format
module Louter.Types.Request
  ( ChatRequest(..)
  , Message(..)
  , MessageRole(..)
  , Tool(..)
  , ToolChoice(..)
  , defaultChatRequest
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), (.=), object)
import Data.Aeson.KeyMap (lookup)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (lookup)

-- | Protocol-agnostic chat request (Internal Representation)
-- Inspired by OpenAI's format but owned by Louter
data ChatRequest = ChatRequest
  { reqModel :: !Text                 -- ^ Model name (e.g., "gpt-4", "gemini-pro")
  , reqMessages :: ![Message]         -- ^ Conversation messages
  , reqTools :: ![Tool]               -- ^ Available tools/functions
  , reqToolChoice :: !ToolChoice      -- ^ How to choose tools
  , reqTemperature :: !(Maybe Double) -- ^ Sampling temperature
  , reqMaxTokens :: !(Maybe Int)      -- ^ Maximum tokens to generate
  , reqStream :: !Bool                -- ^ Whether to stream response
  } deriving (Show, Eq, Generic)

instance FromJSON ChatRequest
instance ToJSON ChatRequest

-- | Default request with sensible defaults
defaultChatRequest :: Text -> [Message] -> ChatRequest
defaultChatRequest model msgs = ChatRequest
  { reqModel = model
  , reqMessages = msgs
  , reqTools = []
  , reqToolChoice = ToolChoiceAuto
  , reqTemperature = Nothing
  , reqMaxTokens = Nothing
  , reqStream = False
  }

-- | Message in a conversation
data Message = Message
  { msgRole :: !MessageRole
  , msgContent :: !Text
  } deriving (Show, Eq, Generic)

instance FromJSON Message
instance ToJSON Message

-- | Message role
data MessageRole
  = RoleSystem
  | RoleUser
  | RoleAssistant
  | RoleTool
  deriving (Show, Eq)

instance FromJSON MessageRole where
  parseJSON (String "system") = pure RoleSystem
  parseJSON (String "user") = pure RoleUser
  parseJSON (String "assistant") = pure RoleAssistant
  parseJSON (String "tool") = pure RoleTool
  parseJSON _ = fail "Invalid role"

instance ToJSON MessageRole where
  toJSON role = case role of
    RoleSystem -> String "system"
    RoleUser -> String "user"
    RoleAssistant -> String "assistant"
    RoleTool -> String "tool"

-- | Tool/Function definition
data Tool = Tool
  { toolName :: !Text              -- ^ Function name
  , toolDescription :: !(Maybe Text) -- ^ Description
  , toolParameters :: !Value       -- ^ JSON Schema for parameters
  } deriving (Show, Eq, Generic)

instance FromJSON Tool

instance ToJSON Tool where
  toJSON t = object
    [ "type" .= ("function" :: Text)
    , "function" .= object
        [ "name" .= toolName t
        , "description" .= toolDescription t
        , "parameters" .= toolParameters t
        ]
    ]

-- | How to choose which tool to call
data ToolChoice
  = ToolChoiceAuto     -- ^ Let model decide
  | ToolChoiceNone     -- ^ Don't call any tools
  | ToolChoiceRequired -- ^ Must call at least one tool
  | ToolChoiceSpecific !Text -- ^ Call specific tool
  deriving (Show, Eq)

instance FromJSON ToolChoice where
  parseJSON (String "auto") = pure ToolChoiceAuto
  parseJSON (String "none") = pure ToolChoiceNone
  parseJSON (String "required") = pure ToolChoiceRequired
  parseJSON (Object obj) = case lookup "type" obj of
    Just (String "function") -> case lookup "function" obj of
      Just (Object fn) -> case lookup "name" fn of
        Just (String name) -> pure $ ToolChoiceSpecific name
        _ -> fail "Missing name in function"
      _ -> fail "Missing function object"
    _ -> fail "Unknown tool choice type"
  parseJSON _ = fail "Invalid tool choice"

instance ToJSON ToolChoice where
  toJSON choice = case choice of
    ToolChoiceAuto -> String "auto"
    ToolChoiceNone -> String "none"
    ToolChoiceRequired -> String "required"
    ToolChoiceSpecific name -> object ["type" .= ("function" :: Text), "function" .= object ["name" .= name]]
