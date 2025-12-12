{-# LANGUAGE OverloadedStrings #-}

-- | Gemini <-> OpenAI Protocol Converter
-- This module handles bidirectional conversion between Gemini and OpenAI formats
module Louter.Protocol.GeminiConverter
  ( -- * Request Conversion (Gemini -> OpenAI)
    geminiToOpenAI
  , convertGeminiContentToMessage
  , convertGeminiToolsToOpenAI

    -- * Response Conversion (OpenAI -> Gemini)
  , openAIResponseToGemini
  ) where

import Data.Aeson (Value(..), Object, encode, object, (.=))
import qualified Data.Aeson.KeyMap as HM
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V

-- ============================================================================
-- Request Conversion: Gemini -> OpenAI
-- ============================================================================

-- | Convert Gemini request to OpenAI format
geminiToOpenAI :: Text -> Bool -> Value -> Either Text Value
geminiToOpenAI modelName streaming (Object obj) = do
  -- Extract contents array
  contents <- case HM.lookup "contents" obj of
    Just (Array cs) -> Right $ V.toList cs
    _ -> Left "Missing 'contents' field"

  -- Convert contents to OpenAI messages
  messages <- mapM convertGeminiContentToMessage contents

  -- Extract system instruction if present
  let systemMsg = case HM.lookup "systemInstruction" obj of
        Just (Object sysInst) -> case HM.lookup "parts" sysInst of
          Just (Array parts) | not (V.null parts) ->
            case V.head parts of
              Object part -> case HM.lookup "text" part of
                Just (String txt) -> [object ["role" .= ("system" :: Text), "content" .= txt]]
                _ -> []
              _ -> []
          _ -> []
        _ -> []

  -- Extract generation config
  let temperature = case HM.lookup "generationConfig" obj of
        Just (Object cfg) -> HM.lookup "temperature" cfg
        _ -> Nothing

  let maxTokens = case HM.lookup "generationConfig" obj of
        Just (Object cfg) -> HM.lookup "maxOutputTokens" cfg
        _ -> Nothing

  -- Extract tools
  let tools = case HM.lookup "tools" obj of
        Just (Array ts) -> Just $ V.toList ts
        _ -> Nothing

  Right $ object $
    [ "model" .= modelName
    , "messages" .= (systemMsg ++ messages)
    , "stream" .= streaming
    ] ++ (case temperature of Just t -> ["temperature" .= t]; Nothing -> [])
      ++ (case maxTokens of Just m -> ["max_tokens" .= m]; Nothing -> [])
      ++ (case tools of Just t -> ["tools" .= convertGeminiToolsToOpenAI t]; Nothing -> [])

geminiToOpenAI _ _ _ = Left "Request must be a JSON object"

-- | Convert Gemini content to OpenAI message
convertGeminiContentToMessage :: Value -> Either Text Value
convertGeminiContentToMessage (Object content) = do
  role <- case HM.lookup "role" content of
    Just (String r) -> Right r
    _ -> Right "user"  -- Default to user

  -- Convert Gemini roles to OpenAI roles
  -- Gemini uses "model", OpenAI uses "assistant"
  let openAIRole = if role == "model" then "assistant" else role

  parts <- case HM.lookup "parts" content of
    Just (Array ps) -> Right $ V.toList ps
    _ -> Left "Missing 'parts' in content"

  -- Check if any part is a functionResponse (tool result)
  let hasFunctionResponse = any isFunctionResponse parts

  if hasFunctionResponse && not (null parts)
    then do
      -- Convert function response to OpenAI tool message format
      -- Gemini can have multiple function responses in one message
      let toolMessages = map convertFunctionResponsePart (filter isFunctionResponse parts)
      -- For now, return the first tool message (OpenAI expects one tool result per message)
      case toolMessages of
        (msg:_) -> Right msg
        [] -> Left "Function response part missing required fields"
    else do
      -- Regular text content
      let textContent = case parts of
            (Object part : _) -> case HM.lookup "text" part of
              Just (String txt) -> txt
              _ -> ""
            _ -> ""

      Right $ object
        [ "role" .= openAIRole
        , "content" .= textContent
        ]
  where
    isFunctionResponse (Object part) = HM.member "functionResponse" part
    isFunctionResponse _ = False

    convertFunctionResponsePart (Object part) =
      case HM.lookup "functionResponse" part of
        Just (Object funcResp) ->
          let funcName = case HM.lookup "name" funcResp of
                Just (String n) -> n
                _ -> "unknown"
              funcResult = case HM.lookup "response" funcResp of
                Just resp -> encode resp
                _ -> "{}"
              -- Generate a tool_call_id (in real Gemini API, this would come from the original call)
              -- For now, use the function name as ID
              toolCallId = funcName <> "_result"
          in object
              [ "role" .= ("tool" :: Text)
              , "content" .= TE.decodeUtf8 (BL.toStrict funcResult)
              , "tool_call_id" .= toolCallId
              ]
        _ -> object []
    convertFunctionResponsePart _ = object []

convertGeminiContentToMessage _ = Left "Content must be a JSON object"

-- | Convert Gemini tools to OpenAI format
convertGeminiToolsToOpenAI :: [Value] -> [Value]
convertGeminiToolsToOpenAI = concatMap convertTool
  where
    convertTool (Object tool) =
      case HM.lookup "functionDeclarations" tool of
        Just (Array funcs) -> map (\func -> object
          [ "type" .= ("function" :: Text)
          , "function" .= convertFunctionDeclaration func
          ]) (V.toList funcs)
        _ -> []
    convertTool _ = []

    -- Convert Gemini function declaration to OpenAI format
    -- Rename "parametersJsonSchema" to "parameters"
    convertFunctionDeclaration (Object funcObj) =
      let renamedObj = case HM.lookup "parametersJsonSchema" funcObj of
            Just params -> HM.insert "parameters" params (HM.delete "parametersJsonSchema" funcObj)
            Nothing -> funcObj
      in Object renamedObj
    convertFunctionDeclaration other = other

-- ============================================================================
-- Response Conversion: OpenAI -> Gemini (Non-Streaming)
-- ============================================================================

-- | Convert OpenAI non-streaming response to Gemini format
openAIResponseToGemini :: Value -> Value
openAIResponseToGemini (Object openAIResp) =
  let candidates = case HM.lookup "choices" openAIResp of
        Just (Array choices) | not (V.null choices) ->
          V.toList $ V.map convertChoice choices
        _ -> []
  in object
      [ "candidates" .= candidates
      , "usageMetadata" .= object
          [ "promptTokenCount" .= (0 :: Int)
          , "candidatesTokenCount" .= (0 :: Int)
          , "totalTokenCount" .= (0 :: Int)
          ]
      ]
  where
    convertChoice (Object choice) =
      let message = case HM.lookup "message" choice of
            Just (Object m) -> m
            _ -> HM.empty
          finishReason = HM.lookup "finish_reason" choice
          content = HM.lookup "content" message
          parts = case content of
            Just (String txt) -> [object ["text" .= txt]]
            _ -> []
      in object $
          [ "content" .= object
              [ "parts" .= parts
              , "role" .= ("model" :: Text)
              ]
          ] ++ (case finishReason of
                  Just r -> ["finishReason" .= r]
                  Nothing -> [])
    convertChoice _ = object []
openAIResponseToGemini _ = object []
