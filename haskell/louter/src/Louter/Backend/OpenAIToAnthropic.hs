{-# LANGUAGE OverloadedStrings #-}

-- | OpenAI ↔ Anthropic protocol conversion
-- Used when OpenAI frontend needs to communicate with Anthropic backend
module Louter.Backend.OpenAIToAnthropic
  ( -- * Request Conversion
    openAIToAnthropic
  , openAIMessageToAnthropic
    -- * Response Conversion
  , anthropicToOpenAIResponse
  , convertAnthropicToOpenAIStream
  ) where

import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson.KeyMap as HM
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder, byteString)
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client (brRead)

-- | Convert OpenAI request to Anthropic request format
openAIToAnthropic :: Value -> Either Text Value
openAIToAnthropic (Object obj) = do
  -- Extract required fields from OpenAI format
  model <- case HM.lookup "model" obj of
    Just (String m) -> Right m
    _ -> Left "Missing 'model' field"

  messages <- case HM.lookup "messages" obj of
    Just (Array msgs) -> Right $ V.toList msgs
    _ -> Left "Missing 'messages' field"

  -- Convert to Anthropic format (inverse of anthropicToOpenAI)
  let anthropicMessages = map openAIMessageToAnthropic messages
      maxTokens = case HM.lookup "max_tokens" obj of
        Just (Number n) -> Just (floor n :: Int)
        _ -> Just 1024  -- Default

      temperature = HM.lookup "temperature" obj
      stream = case HM.lookup "stream" obj of
        Just (Bool b) -> b
        _ -> False

  Right $ object $
    [ "model" .= model
    , "messages" .= anthropicMessages
    , "max_tokens" .= maxTokens
    , "stream" .= stream
    ] ++ (case temperature of Just t -> ["temperature" .= t]; Nothing -> [])

openAIToAnthropic _ = Left "Request must be a JSON object"

-- | Convert OpenAI message to Anthropic message format
openAIMessageToAnthropic :: Value -> Value
openAIMessageToAnthropic (Object msg) =
  let role = case HM.lookup "role" msg of
        Just (String r) -> r
        _ -> "user"
      content = case HM.lookup "content" msg of
        Just (String c) -> c
        _ -> ""
  in object ["role" .= role, "content" .= content]
openAIMessageToAnthropic other = other

-- | Convert Anthropic response to OpenAI response format
anthropicToOpenAIResponse :: Value -> Value
anthropicToOpenAIResponse (Object anthropicResp) =
  let content = case HM.lookup "content" anthropicResp of
        Just (Array contentBlocks) | not (V.null contentBlocks) ->
          case V.head contentBlocks of
            Object block -> case HM.lookup "text" block of
              Just (String txt) -> txt
              _ -> ""
            _ -> ""
        _ -> ""

      finishReason :: Text
      finishReason = case HM.lookup "stop_reason" anthropicResp of
        Just (String "end_turn") -> "stop"
        Just (String "max_tokens") -> "length"
        Just (String "tool_use") -> "tool_calls"
        _ -> "stop"

  in object
      [ "id" .= ("chatcmpl-" <> "123" :: Text)
      , "object" .= ("chat.completion" :: Text)
      , "created" .= (1234567890 :: Int)
      , "model" .= ("gpt-4" :: Text)
      , "choices" .= [object
          [ "index" .= (0 :: Int)
          , "message" .= object
              [ "role" .= ("assistant" :: Text)
              , "content" .= content
              ]
          , "finish_reason" .= finishReason
          ]]
      ]
anthropicToOpenAIResponse _ = object []

-- | Convert Anthropic SSE stream to OpenAI SSE format
convertAnthropicToOpenAIStream :: (Builder -> IO ()) -> IO () -> HTTP.BodyReader -> IO ()
convertAnthropicToOpenAIStream write flush bodyReader = do
  -- TODO: Implement proper Anthropic → OpenAI SSE conversion
  -- For now, just pass through (will need to parse Anthropic events and convert)
  let loop = do
        chunk <- brRead bodyReader
        if BS.null chunk
          then pure ()
          else do
            -- Simple pass-through for now
            write (byteString chunk)
            flush
            loop
  loop
