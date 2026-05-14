{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE LambdaCase #-}

-- | Request types (Protocol-Agnostic Internal Representation)
-- All protocol-specific requests convert TO this format
module Louter.Types.Request
  ( ChatRequest(..)
  , Message(..)
  , MessageRole(..)
  , ContentPart(..)
  , Tool(..)
  , ToolChoice(..)
  , defaultChatRequest
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), (.=), (.:), (.:?), object)
import Data.Aeson.KeyMap (lookup)
import Data.Text (Text)
import qualified Data.Vector as V (toList)
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

-- | Content part (text, image, etc.)
data ContentPart
  = TextPart !Text
  | ImagePart
      { imageMediaType :: !Text  -- ^ MIME type (e.g., "image/png")
      , imageData :: !Text       -- ^ Base64-encoded image data
      }
  deriving (Show, Eq, Generic)

instance ToJSON ContentPart where
  toJSON (TextPart txt) = object
    [ "type" .= ("text" :: Text)
    , "text" .= txt
    ]
  toJSON (ImagePart mediaType dataB64) = object
    [ "type" .= ("image_url" :: Text)
    , "image_url" .= object
        [ "url" .= ("data:" <> mediaType <> ";base64," <> dataB64)
        ]
    ]

instance FromJSON ContentPart where
  parseJSON (Object obj) = case lookup "type" obj of
    Just (String "text") -> case lookup "text" obj of
      Just (String txt) -> pure $ TextPart txt
      _ -> fail "Missing text field"
    Just (String "image_url") -> case lookup "image_url" obj of
      Just (Object imgObj) -> case lookup "url" imgObj of
        Just (String url) -> pure $ TextPart url  -- Simplified for now
        _ -> fail "Missing url in image_url"
      _ -> fail "Missing image_url object"
    _ -> fail "Unknown content part type"
  parseJSON _ = fail "Expected object for ContentPart"

-- | Message in a conversation
data Message = Message
  { msgRole :: !MessageRole
  , msgContent :: ![ContentPart]  -- ^ Changed from Text to [ContentPart]
  , msgToolCalls :: ![MessageToolCall]
  , msgToolCallId :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

data MessageToolCall = MessageToolCall
  {
    mtcId        :: !Text
  , mtcName      :: !Text
  , mtcArguments :: !Text
  } deriving (Show, Eq, Generic)

instance ToJSON MessageToolCall
instance FromJSON MessageToolCall

instance FromJSON Message where
  parseJSON (Object obj) = do
    role <- obj .: "role"
    content <- obj .:? "content" >>= \case
      Nothing -> pure []
      Just Null -> pure []
      Just (String text) -> pure [TextPart text]
      Just (Array arr) -> mapM parseJSON $ V.toList arr
      Just other -> fail $ "Expected Array or String, got: " <> show other
    toolCalls <- obj .:? "tool_calls" >>= \case
      Nothing -> pure []
      Just Null -> pure []
      Just (Array arr) -> mapM parseJSON $ V.toList arr
      Just other -> fail $ "Expected Array or String, got: " <> show other
    toolCallId <- obj .:? "tool_call_id"
    pure $ Message role content toolCalls toolCallId
  parseJSON _ = fail "Expected object for Message"

instance ToJSON Message where
  toJSON msg@(Message {})
    | not (null $ msgToolCalls msg) =
      object [ "role"       .= msgRole msg
             , "content"    .= Null
             , "tool_calls" .= map toToolCallJson (msgToolCalls msg)
             ]
    | Just toolCallID <- msgToolCallId msg =
      object [ "role"         .= msgRole msg
             , "content"      .= stringOrArray (msgContent msg)
             , "tool_call_id" .= toolCallID
             ]
    | otherwise =
      object [ "role"    .= msgRole msg
             , "content" .= stringOrArray (msgContent msg)
             ]
    where
      stringOrArray [TextPart text] = String text -- Simplify single text to string
      stringOrArray parts = toJSON parts          -- Multiple parts as array
      toToolCallJson toolCall =
        object [ "id"       .= mtcId toolCall
               , "type"     .= ("function" :: Text)
               , "function" .= object [ "name"      .= mtcName toolCall
                                      , "arguments" .= mtcArguments toolCall
                                      ]
               ]

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
