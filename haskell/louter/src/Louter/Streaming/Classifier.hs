{-# LANGUAGE OverloadedStrings #-}

-- | Delta classification - determines the type of streaming chunk
-- This is where we analyze the JSON structure to identify:
-- - reasoning tokens
-- - content text
-- - function calls
-- - finish reasons
module Louter.Streaming.Classifier
  ( classifyDelta
  , extractDeltaType
  , parseToolCallFragment
  ) where

import Data.Aeson (Value(..), Object)
import Data.Aeson.KeyMap (lookup)
import Data.Text (Text)
import Data.Vector ((!?))
import qualified Data.Vector as V
import Louter.Types.Streaming (DeltaType(..), ToolCallFragment(..))
import Prelude hiding (lookup)

-- | Classify a delta object to determine its type
-- This determines the buffering strategy
classifyDelta :: Value -> Either Text DeltaType
classifyDelta (Object obj) = extractDeltaType obj
classifyDelta _ = Left "Expected JSON object"

-- | Extract delta type from a choice object
-- Looks at the "delta" field and classifies its contents
extractDeltaType :: Object -> Either Text DeltaType
extractDeltaType obj = do
  case lookup "delta" obj of
    Just (Object delta) -> classifyDeltaContent delta
    Just (String txt) -> pure $ ContentDelta txt  -- Some APIs send string directly
    Nothing ->
      -- Check for finish_reason without delta
      case lookup "finish_reason" obj of
        Just (String reason) -> pure $ FinishDelta reason
        _ -> pure EmptyDelta
    _ -> pure EmptyDelta

-- | Classify the contents of a delta object
classifyDeltaContent :: Object -> Either Text DeltaType
classifyDeltaContent delta =
  -- Check in order of priority
  case lookup "reasoning" delta of
    Just (String txt) -> pure $ ReasoningDelta txt
    Just _ -> pure EmptyDelta  -- Invalid type for reasoning
    Nothing ->
      case lookup "content" delta of
        Just (String txt) -> pure $ ContentDelta txt
        Just _ -> pure EmptyDelta  -- Invalid type for content
        Nothing ->
          case lookup "tool_calls" delta of
            Just (Array toolCalls) ->
              if V.null toolCalls
                then pure EmptyDelta
                else case toolCalls !? 0 of
                  Just tc -> ToolCallDelta <$> parseToolCallFragment tc
                  Nothing -> pure EmptyDelta
            Just _ -> pure EmptyDelta  -- Invalid type for tool_calls
            Nothing ->
              case lookup "role" delta of
                Just (String role) -> pure $ RoleDelta role
                Just _ -> pure EmptyDelta  -- Invalid type for role
                Nothing -> pure EmptyDelta

-- | Parse a tool call fragment from JSON
-- Format: {"index": 0, "id": "call_xyz", "type": "function", "function": {"name": "calc", "arguments": "{\"x\""}}
parseToolCallFragment :: Value -> Either Text ToolCallFragment
parseToolCallFragment (Object obj) = do
  index <- case lookup "index" obj of
    Just (Number n) -> pure $ floor n
    _ -> Left "Missing or invalid 'index' in tool_call"

  let tcId = case lookup "id" obj of
        Just (String i) -> Just i
        _ -> Nothing

  let tcName = case lookup "function" obj of
        Just (Object fn) -> case lookup "name" fn of
          Just (String n) -> Just n
          _ -> Nothing
        _ -> Nothing

  let tcArgs = case lookup "function" obj of
        Just (Object fn) -> case lookup "arguments" fn of
          Just (String a) -> Just a
          _ -> Nothing
        _ -> Nothing

  pure $ ToolCallFragment
    { tcfIndex = index
    , tcfId = tcId
    , tcfName = tcName
    , tcfArguments = tcArgs
    }
parseToolCallFragment _ = Left "Expected object for tool_call"
