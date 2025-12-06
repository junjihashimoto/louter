{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Gemini protocol converter
-- Converts between Gemini API format and Internal Representation (OpenAI-based)
module Louter.Protocol.Gemini
  ( geminiRequestToIR
  , irToGeminiResponse
  , parseGeminiChunk
  , geminiToolToIR
  , irToolToGemini
  ) where

import Data.Aeson (Value(..), Object, (.=), object, eitherDecode, encode)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Louter.Types
import Louter.Types.Streaming

-- | Convert Gemini request to Internal Representation
-- Gemini format: {contents: [{role, parts: [{text}]}], tools: [{functionDeclarations}]}
-- IR format: {messages: [{role, content}], tools: [{name, parameters}]}
geminiRequestToIR :: Value -> Either Text ChatRequest
geminiRequestToIR (Object obj) = do
  -- Extract contents (messages)
  messages <- case KM.lookup "contents" obj of
    Just (Array contents) -> mapM parseGeminiMessage (V.toList contents)
    _ -> Left "Missing 'contents' field"

  -- Extract tools
  tools <- case KM.lookup "tools" obj of
    Just (Array toolsArray) -> Right $ concatMap extractFunctionDeclarations (V.toList toolsArray)
    Nothing -> Right []
    _ -> Left "Invalid 'tools' field"

  -- Extract model (may be in URL path, use default for now)
  let model = case KM.lookup "model" obj of
        Just (String m) -> m
        _ -> "gemini-pro"

  -- Extract generation config
  let temperature = case KM.lookup "generationConfig" obj of
        Just (Object config) -> case KM.lookup "temperature" config of
          Just (Number n) -> Just (realToFrac n)
          _ -> Nothing
        _ -> Nothing

  let maxTokens = case KM.lookup "generationConfig" obj of
        Just (Object config) -> case KM.lookup "maxOutputTokens" config of
          Just (Number n) -> Just (floor n)
          _ -> Nothing
        _ -> Nothing

  pure $ ChatRequest
    { reqModel = model
    , reqMessages = messages
    , reqTools = tools
    , reqToolChoice = ToolChoiceAuto
    , reqTemperature = temperature
    , reqMaxTokens = maxTokens
    , reqStream = True  -- Gemini streaming endpoint implies streaming
    }
geminiRequestToIR _ = Left "Expected JSON object"

-- | Parse a Gemini message to IR Message
parseGeminiMessage :: Value -> Either Text Message
parseGeminiMessage (Object obj) = do
  role <- case KM.lookup "role" obj of
    Just (String "user") -> Right RoleUser
    Just (String "model") -> Right RoleAssistant
    Just (String r) -> Left $ "Unknown role: " <> r
    _ -> Left "Missing 'role' field"

  content <- case KM.lookup "parts" obj of
    Just (Array parts) -> extractTextFromParts (V.toList parts)
    _ -> Left "Missing 'parts' field"

  pure $ Message role content
parseGeminiMessage _ = Left "Expected JSON object for message"

-- | Extract text from Gemini parts array
extractTextFromParts :: [Value] -> Either Text Text
extractTextFromParts parts =
  let texts = [txt | Object part <- parts
                    , Just (String txt) <- [KM.lookup "text" part]]
  in if null texts
       then Left "No text found in parts"
       else Right $ T.intercalate " " texts

-- | Extract function declarations from Gemini tools
extractFunctionDeclarations :: Value -> [Tool]
extractFunctionDeclarations (Object obj) =
  case KM.lookup "functionDeclarations" obj of
    Just (Array decls) -> mapMaybe geminiToolToIR (V.toList decls)
    _ -> []
extractFunctionDeclarations _ = []

-- | Convert Gemini function declaration to IR Tool
geminiToolToIR :: Value -> Maybe Tool
geminiToolToIR (Object obj) = do
  name <- case KM.lookup "name" obj of
    Just (String n) -> Just n
    _ -> Nothing

  let description = case KM.lookup "description" obj of
        Just (String d) -> Just d
        _ -> Nothing

  -- Convert Protobuf Schema to JSON Schema
  parameters <- case KM.lookup "parameters" obj of
    Just params -> Just $ convertProtobufSchemaToJSON params
    Nothing -> case KM.lookup "parametersJsonSchema" obj of
      Just params -> Just params
      Nothing -> Just $ object []

  Just $ Tool
    { toolName = name
    , toolDescription = description
    , toolParameters = parameters
    }
geminiToolToIR _ = Nothing

-- | Convert Protobuf Schema (numeric types) to JSON Schema (string types)
-- Gemini uses: 1=STRING, 2=NUMBER, 3=INTEGER, 4=BOOLEAN, 5=ARRAY, 6=OBJECT
convertProtobufSchemaToJSON :: Value -> Value
convertProtobufSchemaToJSON (Object obj) =
  let converted = KM.mapMaybe convertField obj
  in Object converted
  where
    convertField (Number 1) = Just (String "string")
    convertField (Number 2) = Just (String "number")
    convertField (Number 3) = Just (String "integer")
    convertField (Number 4) = Just (String "boolean")
    convertField (Number 5) = Just (String "array")
    convertField (Number 6) = Just (String "object")
    convertField (Object nested) = Just $ convertProtobufSchemaToJSON (Object nested)
    convertField (Array arr) = Just $ Array (V.map convertProtobufSchemaToJSON arr)
    convertField v = Just v
convertProtobufSchemaToJSON v = v

-- | Convert IR Tool to Gemini function declaration
irToolToGemini :: Tool -> Value
irToolToGemini Tool{..} = object
  [ "name" .= toolName
  , "description" .= toolDescription
  , "parameters" .= toolParameters
  ]

-- | Convert IR response to Gemini format
-- IR: {choices: [{message: {content, role}}]}
-- Gemini: {candidates: [{content: {role, parts: [{text}]}}]}
irToGeminiResponse :: ChatResponse -> Value
irToGeminiResponse resp = object
  [ "candidates" .= map choiceToCandidate (respChoices resp)
  , "usageMetadata" .= case respUsage resp of
      Just usage -> object
        [ "promptTokenCount" .= usagePromptTokens usage
        , "candidatesTokenCount" .= usageCompletionTokens usage
        , "totalTokenCount" .= usageTotalTokens usage
        ]
      Nothing -> Null
  , "modelVersion" .= respModel resp
  , "responseId" .= respId resp
  ]
  where
    choiceToCandidate choice = object
      [ "content" .= object
          [ "role" .= ("model" :: Text)
          , "parts" .= [object ["text" .= choiceMessage choice]]
          ]
      , "index" .= choiceIndex choice
      , "finishReason" .= case choiceFinishReason choice of
          Just FinishStop -> String "STOP"
          Just FinishLength -> String "MAX_TOKENS"
          Just FinishToolCalls -> String "STOP"
          Just FinishContentFilter -> String "SAFETY"
          Nothing -> Null
      ]

-- | Parse Gemini SSE chunk to extract delta type
parseGeminiChunk :: Text -> Either Text DeltaType
parseGeminiChunk jsonText =
  case eitherDecode (BL.fromStrict $ TE.encodeUtf8 jsonText) of
    Left err -> Left $ "JSON parse error: " <> T.pack err
    Right (Object obj) ->
      case KM.lookup "candidates" obj of
        Just (Array candidates) ->
          if V.null candidates
            then pure EmptyDelta
            else parseGeminiCandidate (candidates V.! 0)
        _ -> pure EmptyDelta
    Right _ -> Left "Expected JSON object"

-- | Parse a Gemini candidate to extract delta type
parseGeminiCandidate :: Value -> Either Text DeltaType
parseGeminiCandidate (Object obj) =
  case KM.lookup "content" obj of
    Just (Object content) ->
      case KM.lookup "parts" obj of
        Just (Array parts) ->
          if V.null parts
            then pure EmptyDelta
            else parseGeminiPart (parts V.! 0)
        _ -> checkFinishReason obj
    _ -> checkFinishReason obj
  where
    checkFinishReason o = case KM.lookup "finishReason" o of
      Just (String reason) -> pure $ FinishDelta reason
      _ -> pure EmptyDelta
parseGeminiCandidate _ = pure EmptyDelta

-- | Parse a Gemini part to extract content type
parseGeminiPart :: Value -> Either Text DeltaType
parseGeminiPart (Object part) =
  case KM.lookup "text" part of
    Just (String txt) -> pure $ ContentDelta txt
    Nothing ->
      case KM.lookup "functionCall" part of
        Just funcCall -> parseGeminiFunctionCall funcCall
        Nothing -> pure EmptyDelta
    _ -> pure EmptyDelta
parseGeminiPart _ = pure EmptyDelta

-- | Parse Gemini function call
parseGeminiFunctionCall :: Value -> Either Text DeltaType
parseGeminiFunctionCall (Object obj) = do
  name <- case KM.lookup "name" obj of
    Just (String n) -> Right n
    _ -> Left "Missing function name"

  let args = case KM.lookup "args" obj of
        Just v -> Aeson.encode v
        Nothing -> "{}"

  pure $ ToolCallDelta $ ToolCallFragment
    { tcfIndex = 0
    , tcfId = Just "gemini-call"
    , tcfName = Just name
    , tcfArguments = Just (TE.decodeUtf8 $ BL.toStrict args)
    }
parseGeminiFunctionCall _ = Left "Expected object for function call"

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x:xs) = case f x of
  Just y -> y : mapMaybe f xs
  Nothing -> mapMaybe f xs
