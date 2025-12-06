{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Anthropic protocol converter
-- Converts between Anthropic API format and Internal Representation (OpenAI-based)
module Louter.Protocol.Anthropic
  ( anthropicRequestToIR
  , irToAnthropicResponse
  , parseAnthropicChunk
  ) where

import Data.Aeson (Value(..), Object, (.=), object, eitherDecode)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Louter.Types
import Louter.Types.Streaming

-- | Convert Anthropic request to Internal Representation
-- Anthropic format: {model, messages: [{role, content}], tools, max_tokens}
-- IR format: {model, messages: [{role, content}], tools}
anthropicRequestToIR :: Value -> Either Text ChatRequest
anthropicRequestToIR (Object obj) = do
  -- Extract model
  model <- case KM.lookup "model" obj of
    Just (String m) -> Right m
    _ -> Left "Missing 'model' field"

  -- Extract messages
  messages <- case KM.lookup "messages" obj of
    Just (Array msgs) -> mapM parseAnthropicMessage (V.toList msgs)
    _ -> Left "Missing 'messages' field"

  -- Extract tools
  tools <- case KM.lookup "tools" obj of
    Just (Array toolsArray) -> mapM anthropicToolToIR (V.toList toolsArray)
    Nothing -> Right []
    _ -> Left "Invalid 'tools' field"

  -- Extract max_tokens (required in Anthropic)
  let maxTokens = case KM.lookup "max_tokens" obj of
        Just (Number n) -> Just (floor n)
        _ -> Nothing

  let temperature = case KM.lookup "temperature" obj of
        Just (Number n) -> Just (realToFrac n)
        _ -> Nothing

  pure $ ChatRequest
    { reqModel = model
    , reqMessages = messages
    , reqTools = tools
    , reqToolChoice = ToolChoiceAuto
    , reqTemperature = temperature
    , reqMaxTokens = maxTokens
    , reqStream = case KM.lookup "stream" obj of
        Just (Bool b) -> b
        _ -> False
    }
anthropicRequestToIR _ = Left "Expected JSON object"

-- | Parse an Anthropic message to IR Message
parseAnthropicMessage :: Value -> Either Text Message
parseAnthropicMessage (Object obj) = do
  role <- case KM.lookup "role" obj of
    Just (String "user") -> Right RoleUser
    Just (String "assistant") -> Right RoleAssistant
    Just (String r) -> Left $ "Unknown role: " <> r
    _ -> Left "Missing 'role' field"

  content <- case KM.lookup "content" obj of
    Just (String txt) -> Right txt
    Just (Array parts) -> extractTextFromAnthropicContent (V.toList parts)
    _ -> Left "Missing or invalid 'content' field"

  pure $ Message role content
parseAnthropicMessage _ = Left "Expected JSON object for message"

-- | Extract text from Anthropic content array
extractTextFromAnthropicContent :: [Value] -> Either Text Text
extractTextFromAnthropicContent parts =
  let texts = [txt | Object part <- parts
                    , Just (String "text") <- [KM.lookup "type" part]
                    , Just (String txt) <- [KM.lookup "text" part]]
  in if null texts
       then Left "No text found in content"
       else Right $ T.intercalate " " texts

-- | Convert Anthropic tool to IR Tool
anthropicToolToIR :: Value -> Either Text Tool
anthropicToolToIR (Object obj) = do
  name <- case KM.lookup "name" obj of
    Just (String n) -> Right n
    _ -> Left "Missing 'name' in tool"

  let description = case KM.lookup "description" obj of
        Just (String d) -> Just d
        _ -> Nothing

  parameters <- case KM.lookup "input_schema" obj of
    Just schema -> Right schema
    _ -> Left "Missing 'input_schema' in tool"

  pure $ Tool
    { toolName = name
    , toolDescription = description
    , toolParameters = parameters
    }
anthropicToolToIR _ = Left "Expected JSON object for tool"

-- | Convert IR response to Anthropic format
-- IR: {choices: [{message, finish_reason}]}
-- Anthropic: {content: [{type: "text", text}], stop_reason}
irToAnthropicResponse :: ChatResponse -> Value
irToAnthropicResponse resp = object
  [ "id" .= respId resp
  , "type" .= ("message" :: Text)
  , "role" .= ("assistant" :: Text)
  , "model" .= respModel resp
  , "content" .= case respChoices resp of
      (choice:_) -> [object ["type" .= ("text" :: Text), "text" .= choiceMessage choice]]
      [] -> ([] :: [Value])
  , "stop_reason" .= case respChoices resp of
      (choice:_) -> case choiceFinishReason choice of
        Just FinishStop -> String "end_turn"
        Just FinishLength -> String "max_tokens"
        Just FinishToolCalls -> String "tool_use"
        Just FinishContentFilter -> String "stop_sequence"
        Nothing -> Null
      [] -> Null
  , "usage" .= case respUsage resp of
      Just usage -> object
        [ "input_tokens" .= usagePromptTokens usage
        , "output_tokens" .= usageCompletionTokens usage
        ]
      Nothing -> Null
  ]

-- | Parse Anthropic SSE chunk to extract delta type
-- Anthropic uses event types: message_start, content_block_start, content_block_delta, content_block_stop, message_stop
parseAnthropicChunk :: Text -> Either Text DeltaType
parseAnthropicChunk jsonText =
  case eitherDecode (BL.fromStrict $ TE.encodeUtf8 jsonText) of
    Left err -> Left $ "JSON parse error: " <> T.pack err
    Right (Object obj) ->
      case KM.lookup "type" obj of
        Just (String "content_block_delta") ->
          case KM.lookup "delta" obj of
            Just (Object delta) ->
              case KM.lookup "type" delta of
                Just (String "text_delta") ->
                  case KM.lookup "text" delta of
                    Just (String txt) -> pure $ ContentDelta txt
                    _ -> pure EmptyDelta
                Just (String "input_json_delta") ->
                  case KM.lookup "partial_json" delta of
                    Just (String json) ->
                      pure $ ToolCallDelta $ ToolCallFragment 0 Nothing Nothing (Just json)
                    _ -> pure EmptyDelta
                _ -> pure EmptyDelta
            _ -> pure EmptyDelta

        Just (String "content_block_start") ->
          case KM.lookup "content_block" obj of
            Just (Object block) ->
              case KM.lookup "type" block of
                Just (String "tool_use") ->
                  let name = case KM.lookup "name" block of
                        Just (String n) -> Just n
                        _ -> Nothing
                      callId = case KM.lookup "id" block of
                        Just (String i) -> Just i
                        _ -> Nothing
                  in pure $ ToolCallDelta $ ToolCallFragment 0 callId name Nothing
                _ -> pure EmptyDelta
            _ -> pure EmptyDelta

        Just (String "message_delta") ->
          case KM.lookup "delta" obj of
            Just (Object delta) ->
              case KM.lookup "stop_reason" delta of
                Just (String reason) -> pure $ FinishDelta reason
                _ -> pure EmptyDelta
            _ -> pure EmptyDelta

        _ -> pure EmptyDelta
    Right _ -> Left "Expected JSON object"
