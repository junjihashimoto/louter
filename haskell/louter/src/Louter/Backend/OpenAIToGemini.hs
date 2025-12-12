{-# LANGUAGE OverloadedStrings #-}

-- | OpenAI ↔ Gemini protocol conversion
-- Used when OpenAI frontend needs to communicate with Gemini backend
module Louter.Backend.OpenAIToGemini
  ( -- * Request Conversion
    openAIToGemini
  , openAIMessageToGemini
    -- * Response Conversion
  , geminiToOpenAIResponse
  , convertGeminiToOpenAIStream
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

-- | Convert OpenAI request to Gemini request format
-- Returns (model_name, gemini_request_body)
openAIToGemini :: Value -> Either Text (Text, Value)
openAIToGemini (Object obj) = do
  -- Extract model name
  model <- case HM.lookup "model" obj of
    Just (String m) -> Right m
    _ -> Left "Missing 'model' field"

  messages <- case HM.lookup "messages" obj of
    Just (Array msgs) -> Right $ V.toList msgs
    _ -> Left "Missing 'messages' field"

  -- Convert to Gemini format (inverse of geminiToOpenAI)
  let geminiContents = map openAIMessageToGemini messages
      temperature = HM.lookup "temperature" obj
      maxTokens = HM.lookup "max_tokens" obj

      geminiReq = object $
        [ "contents" .= geminiContents
        ] ++ (case temperature of
               Just t -> ["generationConfig" .= object ["temperature" .= t]]
               Nothing -> [])
          ++ (case maxTokens of
               Just m -> ["generationConfig" .= object ["maxOutputTokens" .= m]]
               Nothing -> [])

  Right (model, geminiReq)

openAIToGemini _ = Left "Request must be a JSON object"

-- | Convert OpenAI message to Gemini message format
openAIMessageToGemini :: Value -> Value
openAIMessageToGemini (Object msg) =
  let role = case HM.lookup "role" msg of
        Just (String "assistant") -> "model"
        Just (String r) -> r
        _ -> "user"
      content = case HM.lookup "content" msg of
        Just (String c) -> c
        _ -> ""
      parts = [object ["text" .= content]]
  in object ["role" .= role, "parts" .= parts]
openAIMessageToGemini other = other

-- | Convert Gemini response to OpenAI response format
geminiToOpenAIResponse :: Value -> Value
geminiToOpenAIResponse (Object geminiResp) =
  let content = case HM.lookup "candidates" geminiResp of
        Just (Array candidates) | not (V.null candidates) ->
          case V.head candidates of
            Object candidate -> case HM.lookup "content" candidate of
              Just (Object contentObj) -> case HM.lookup "parts" contentObj of
                Just (Array parts) | not (V.null parts) ->
                  case V.head parts of
                    Object part -> case HM.lookup "text" part of
                      Just (String txt) -> txt
                      _ -> ""
                    _ -> ""
                _ -> ""
              _ -> ""
            _ -> ""
        _ -> ""

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
          , "finish_reason" .= ("stop" :: Text)
          ]]
      ]
geminiToOpenAIResponse _ = object []

-- | Convert Gemini SSE stream to OpenAI SSE format
convertGeminiToOpenAIStream :: (Builder -> IO ()) -> IO () -> HTTP.BodyReader -> IO ()
convertGeminiToOpenAIStream write flush bodyReader = do
  -- TODO: Implement proper Gemini → OpenAI SSE conversion
  -- For now, just pass through (will need to parse Gemini events and convert)
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
