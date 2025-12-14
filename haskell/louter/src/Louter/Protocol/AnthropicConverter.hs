{-# LANGUAGE OverloadedStrings #-}

-- | Anthropic <-> OpenAI Protocol Converter
-- This module handles bidirectional conversion between Anthropic and OpenAI formats
module Louter.Protocol.AnthropicConverter
  ( -- * Request Conversion (Anthropic -> OpenAI)
    anthropicToOpenAI
  , convertAnthropicMessageToOpenAI
  , convertAnthropicToolsToOpenAI
  , isAnthropicToolResult
  , isAnthropicToolUse
  , convertAnthropicToolResultToOpenAI
  , convertAnthropicToolUseToOpenAI

    -- * Response Conversion (OpenAI -> Anthropic)
  , openAIResponseToAnthropic
  , convertOpenAIToolCallToAnthropic
  ) where

import Control.Applicative ((<|>))
import Data.Aeson (Value(..), Object, encode, eitherDecode, object, (.=))
import qualified Data.Aeson.KeyMap as HM
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V

-- ============================================================================
-- Request Conversion: Anthropic -> OpenAI
-- ============================================================================

-- | Convert Anthropic request to OpenAI format
anthropicToOpenAI :: Value -> Either Text Value
anthropicToOpenAI (Object obj) = do
  -- Extract messages
  messages <- case HM.lookup "messages" obj of
    Just (Array msgs) -> Right $ V.toList msgs
    _ -> Left "Missing 'messages' field"

  -- Convert messages to OpenAI format
  let convertedMessages = map convertAnthropicMessageToOpenAI messages

  -- Extract max_tokens
  let maxTokens = case HM.lookup "max_tokens" obj of
        Just (Number n) -> Just (floor n :: Int)
        _ -> Nothing

  -- Extract optional fields
  let temperature = case HM.lookup "temperature" obj of
        Just (Number n) -> Just (realToFrac n :: Double)
        _ -> Nothing

  let model = case HM.lookup "model" obj of
        Just (String m) -> m
        _ -> "gpt-4"

  -- Extract system message if present and prepend to messages
  let systemMsg = case HM.lookup "system" obj of
        Just (String sys) -> [object ["role" .= ("system" :: Text), "content" .= sys]]
        _ -> []

  -- Extract tools if present and convert to OpenAI format
  let tools = case HM.lookup "tools" obj of
        Just (Array ts) -> Just (convertAnthropicToolsToOpenAI (V.toList ts))
        _ -> Nothing

  -- Extract stream flag (default to False for Anthropic)
  let streamFlag = case HM.lookup "stream" obj of
        Just (Bool b) -> b
        _ -> False

  Right $ object $
    [ "model" .= model
    , "messages" .= (systemMsg ++ convertedMessages)
    , "max_tokens" .= maxTokens
    , "temperature" .= temperature
    , "stream" .= streamFlag
    ] ++ case tools of
          Just t -> ["tools" .= t]
          Nothing -> []

anthropicToOpenAI _ = Left "Request must be a JSON object"

-- | Convert Anthropic message to OpenAI format
convertAnthropicMessageToOpenAI :: Value -> Value
convertAnthropicMessageToOpenAI (Object msg) =
  let role = case HM.lookup "role" msg of
        Just (String r) -> r
        _ -> "user"

      content = case HM.lookup "content" msg of
        Just c -> c
        _ -> String ""

  in case content of
    -- Simple text content
    String text -> object ["role" .= role, "content" .= text]

    -- Array of content blocks (may include tool_result, tool_use, or text)
    Array blocks ->
      let contentBlocks = V.toList blocks
          hasToolResult = any isAnthropicToolResult contentBlocks
          hasToolUse = any isAnthropicToolUse contentBlocks
      in
        if hasToolResult
          then convertAnthropicToolResultToOpenAI contentBlocks
        else if hasToolUse
          then convertAnthropicToolUseToOpenAI role contentBlocks
        else
          -- Regular text/image blocks - convert to OpenAI format
          let convertedContent = map convertContentBlock contentBlocks
              convertContentBlock (Object block) =
                case HM.lookup "type" block of
                  Just (String "text") -> case HM.lookup "text" block of
                    Just (String txt) -> object ["type" .= ("text" :: Text), "text" .= txt]
                    _ -> object []
                  Just (String "image") -> case HM.lookup "source" block of
                    Just (Object source) ->
                      let mediaType = case HM.lookup "media_type" source of
                            Just (String mt) -> mt
                            _ -> "image/png"
                          imageData = case HM.lookup "data" source of
                            Just (String dat) -> dat
                            _ -> ""
                          dataUrl = "data:" <> mediaType <> ";base64," <> imageData
                      in object
                          [ "type" .= ("image_url" :: Text)
                          , "image_url" .= object ["url" .= dataUrl]
                          ]
                    _ -> object []
                  _ -> object []
              convertContentBlock _ = object []
          in if length convertedContent == 1 && isSimpleText (head convertedContent)
               then object ["role" .= role, "content" .= extractText (head convertedContent)]
               else object ["role" .= role, "content" .= convertedContent]
          where
            isSimpleText (Object obj) = HM.lookup "type" obj == Just (String "text")
            isSimpleText _ = False
            extractText (Object obj) = case HM.lookup "text" obj of
              Just (String txt) -> txt
              _ -> ""
            extractText _ = ""

    _ -> object ["role" .= role, "content" .= content]

convertAnthropicMessageToOpenAI other = other

-- | Check if content block is a tool_result
isAnthropicToolResult :: Value -> Bool
isAnthropicToolResult (Object block) =
  case HM.lookup "type" block of
    Just (String "tool_result") -> True
    _ -> False
isAnthropicToolResult _ = False

-- | Check if content block is a tool_use
isAnthropicToolUse :: Value -> Bool
isAnthropicToolUse (Object block) =
  case HM.lookup "type" block of
    Just (String "tool_use") -> True
    _ -> False
isAnthropicToolUse _ = False

-- | Convert Anthropic tool_result to OpenAI tool message
convertAnthropicToolResultToOpenAI :: [Value] -> Value
convertAnthropicToolResultToOpenAI blocks =
  case [block | block@(Object _) <- blocks, isAnthropicToolResult block] of
    (Object toolResult:_) ->
      let toolUseId = case HM.lookup "tool_use_id" toolResult of
            Just (String tid) -> tid
            _ -> "unknown"
          resultContent = case HM.lookup "content" toolResult of
            Just (String c) -> c
            Just other -> TE.decodeUtf8 (BL.toStrict $ encode other)
            _ -> ""
      in object
          [ "role" .= ("tool" :: Text)
          , "content" .= resultContent
          , "tool_call_id" .= toolUseId
          ]
    _ -> object ["role" .= ("tool" :: Text), "content" .= ("" :: Text)]

-- | Convert Anthropic tool_use to OpenAI assistant message with tool_calls
convertAnthropicToolUseToOpenAI :: Text -> [Value] -> Value
convertAnthropicToolUseToOpenAI role blocks =
  let textParts = [txt | Object block <- blocks,
                         HM.lookup "type" block == Just (String "text"),
                         Just (String txt) <- [HM.lookup "text" block]]
      textContent = if null textParts then Nothing else Just (T.concat textParts)

      toolCalls = [convertToolUseBlock block | block@(Object _) <- blocks, isAnthropicToolUse block]

  in object $
      [ "role" .= role ] ++
      [ "content" .= tc | Just tc <- [textContent] ] ++
      [ "tool_calls" .= toolCalls | not (null toolCalls) ]
  where
    convertToolUseBlock (Object block) =
      let toolId = case HM.lookup "id" block of
            Just (String tid) -> tid
            _ -> "call_unknown"
          toolName = case HM.lookup "name" block of
            Just (String n) -> n
            _ -> "unknown"
          toolInput = case HM.lookup "input" block of
            Just inp -> encode inp
            _ -> "{}"
      in object
          [ "id" .= toolId
          , "type" .= ("function" :: Text)
          , "function" .= object
              [ "name" .= toolName
              , "arguments" .= TE.decodeUtf8 (BL.toStrict toolInput)
              ]
          ]
    convertToolUseBlock _ = object []

-- | Convert Anthropic tools to OpenAI format
convertAnthropicToolsToOpenAI :: [Value] -> [Value]
convertAnthropicToolsToOpenAI = map convertTool
  where
    convertTool (Object tool) =
      let name = HM.lookup "name" tool
          description = HM.lookup "description" tool
          inputSchema = HM.lookup "input_schema" tool
      in object
          [ "type" .= ("function" :: Text)
          , "function" .= object
              ([ "name" .= n | Just n <- [name] ] ++
               [ "description" .= d | Just d <- [description] ] ++
               [ "parameters" .= s | Just s <- [inputSchema] ])
          ]
    convertTool other = other

-- ============================================================================
-- Response Conversion: OpenAI -> Anthropic (Non-Streaming)
-- ============================================================================

-- | Convert OpenAI non-streaming response to Anthropic format
openAIResponseToAnthropic :: Value -> Value
openAIResponseToAnthropic (Object openAIResp) =
  let (content, stopReason) = case HM.lookup "choices" openAIResp of
        Just (Array choices) | not (V.null choices) ->
          case V.head choices of
            Object choice ->
              let finishReason = case HM.lookup "finish_reason" choice of
                    Just (String "tool_calls") -> "tool_use"
                    Just (String "stop") -> "end_turn"
                    Just (String "length") -> "max_tokens"
                    _ -> "end_turn"
              in case HM.lookup "message" choice of
                Just (Object msg) ->
                  let textContent = case HM.lookup "content" msg of
                        Just (String txt) | not (T.null txt) ->
                          [object ["type" .= ("text" :: Text), "text" .= txt]]
                        _ -> []

                      toolContent = case HM.lookup "tool_calls" msg of
                        Just (Array tcs) -> map convertOpenAIToolCallToAnthropic (V.toList tcs)
                        _ -> []

                      allContent = textContent ++ toolContent
                  in (allContent, finishReason)
                _ -> ([], "end_turn")
            _ -> ([], "end_turn")
        _ -> ([], "end_turn")
  in object
      [ "id" .= ("msg_1" :: Text)
      , "type" .= ("message" :: Text)
      , "role" .= ("assistant" :: Text)
      , "content" .= content
      , "model" .= ("claude-3-haiku-20240307" :: Text)
      , "stop_reason" .= (stopReason :: Text)
      , "usage" .= object
          [ "input_tokens" .= (10 :: Int)
          , "output_tokens" .= (10 :: Int)
          ]
      ]
openAIResponseToAnthropic _ = object []

-- | Convert OpenAI tool_call to Anthropic tool_use content block
convertOpenAIToolCallToAnthropic :: Value -> Value
convertOpenAIToolCallToAnthropic (Object tc) =
  let toolId = case HM.lookup "id" tc of
        Just (String tid) -> tid
        _ -> "tool_unknown"

      (toolName, toolArgs) = case HM.lookup "function" tc of
        Just (Object func) ->
          let name = case HM.lookup "name" func of
                Just (String n) -> n
                _ -> "unknown"
              args = case HM.lookup "arguments" func of
                Just (String a) -> case eitherDecode (BL.fromStrict $ TE.encodeUtf8 a) of
                  Right val -> val
                  Left _ -> object []
                _ -> object []
          in (name, args)
        _ -> ("unknown", object [])
  in object
      [ "type" .= ("tool_use" :: Text)
      , "id" .= toolId
      , "name" .= toolName
      , "input" .= toolArgs
      ]
convertOpenAIToolCallToAnthropic _ = object []
