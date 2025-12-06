{-# LANGUAGE OverloadedStrings #-}

-- | OpenAI protocol converter
-- OpenAI format is the "base" Internal Representation
-- This module handles the SSE streaming format specific to OpenAI
module Louter.Protocol.OpenAI
  ( parseOpenAIChunk
  , openAIRequestToIR
  , irToOpenAIResponse
  ) where

import Data.Aeson (Value(..), eitherDecode)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Louter.Streaming.Classifier (classifyDelta)
import Louter.Types
import Louter.Types.Streaming

-- | Parse an OpenAI SSE chunk to extract delta type
parseOpenAIChunk :: Text -> Either Text DeltaType
parseOpenAIChunk jsonText =
  case eitherDecode (BL.fromStrict $ TE.encodeUtf8 jsonText) of
    Left err -> Left $ "JSON parse error: " <> T.pack err
    Right (Object obj) ->
      case KM.lookup "choices" obj of
        Just (Array choices) ->
          if V.null choices
            then pure EmptyDelta
            else case choices V.! 0 of
              Object choice -> classifyDelta (Object choice)
              _ -> pure EmptyDelta
        _ -> pure EmptyDelta
    Right _ -> Left "Expected JSON object"

-- | Convert OpenAI request to Internal Representation
-- Since OpenAI IS the IR format, this is mostly identity
openAIRequestToIR :: Value -> Either Text ChatRequest
openAIRequestToIR val = case Aeson.fromJSON val of
  Aeson.Success req -> Right req
  Aeson.Error err -> Left $ "Failed to parse OpenAI request: " <> T.pack err

-- | Convert IR response to OpenAI format
-- Again, mostly identity since OpenAI IS the IR
irToOpenAIResponse :: ChatResponse -> Value
irToOpenAIResponse = Aeson.toJSON
