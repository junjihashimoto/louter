{-# LANGUAGE OverloadedStrings #-}

-- | XML Tool Call Parser for Qwen3-Coder format
--
-- Parses XML-formatted tool calls like:
-- @
-- <tool_call>
--   <function=WriteFile>
--     <parameter=file_path>test.txt</parameter>
--     <parameter=content>Hello World!</parameter>
--   </function>
-- </tool_call>
-- @
--
-- Converts to OpenAI ToolCall format for uniform handling
module Louter.Streaming.XMLToolCallParser
  ( parseXMLToolCalls
  , extractFunctionName
  , extractParameters
  , stripXMLToolCallTags
  , convertToToolCall
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import Data.Aeson (Value(..), decode, object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Text.Regex.TDFA ((=~))
import Data.Maybe (mapMaybe, fromMaybe)
import Louter.Types.Streaming (ToolCall(..))

-- | Parse all XML tool calls from text content
-- Returns list of (function_name, parameters_map)
parseXMLToolCalls :: Text -> [(Text, HM.HashMap Text Value)]
parseXMLToolCalls content =
  let toolCallBlocks = extractToolCallBlocks content
  in mapMaybe parseToolCallBlock toolCallBlocks

-- | Extract all <tool_call>...</tool_call> blocks from text
-- Uses manual splitting to avoid regex complexity with nested tags
extractToolCallBlocks :: Text -> [Text]
extractToolCallBlocks content = extractBlocks content []
  where
    extractBlocks :: Text -> [Text] -> [Text]
    extractBlocks text acc
      | T.null text = reverse acc
      | otherwise =
          case T.breakOn "<tool_call>" text of
            (_, rest) | T.null rest -> reverse acc
            (_, rest) ->
              let afterOpen = T.drop (T.length "<tool_call>") rest
              in case T.breakOn "</tool_call>" afterOpen of
                   (block, afterClose) | T.null afterClose -> reverse acc
                   (block, afterClose) ->
                     let remaining = T.drop (T.length "</tool_call>") afterClose
                     in extractBlocks remaining (block : acc)

-- | Parse a single tool call block
parseToolCallBlock :: Text -> Maybe (Text, HM.HashMap Text Value)
parseToolCallBlock block = do
  functionName <- extractFunctionName block
  let parameters = extractParameters block
  return (functionName, parameters)

-- | Extract function name from <function=NAME> tag
extractFunctionName :: Text -> Maybe Text
extractFunctionName block =
  let pattern = "<function=([^>]+)>" :: String
      matches = T.unpack block =~ pattern :: [[String]]
  in case matches of
       ((_ : name : _) : _) -> Just (T.pack name)
       _ -> Nothing

-- | Extract all parameters from <parameter=key>value</parameter> tags
extractParameters :: Text -> HM.HashMap Text Value
extractParameters block =
  let pattern = "<parameter=([^>]+)>([^<]*)</parameter>" :: String
      matches = T.unpack block =~ pattern :: [[String]]
      pairs = [(T.pack key, parseValue (T.pack value)) | (_ : key : value : _) <- matches]
  in HM.fromList pairs

-- | Parse parameter value with type detection
-- Attempts to parse as JSON first (for numbers, booleans, objects)
-- Falls back to string if JSON parsing fails
parseValue :: Text -> Value
parseValue text =
  let trimmed = T.strip text
      -- Try parsing as JSON
      jsonResult = decode (BL.fromStrict $ TE.encodeUtf8 trimmed) :: Maybe Value
  in case jsonResult of
       Just val -> val
       Nothing  -> String trimmed  -- Fallback to string

-- | Remove all <tool_call>...</tool_call> tags from text
-- Keeps surrounding text content intact
stripXMLToolCallTags :: Text -> Text
stripXMLToolCallTags content = T.strip $ removeBlocks content
  where
    removeBlocks :: Text -> Text
    removeBlocks text =
      case T.breakOn "<tool_call>" text of
        (before, rest) | T.null rest -> before
        (before, rest) ->
          case T.breakOn "</tool_call>" (T.drop (T.length "<tool_call>") rest) of
            (_, afterClose) | T.null afterClose -> before
            (_, afterClose) ->
              let remaining = T.drop (T.length "</tool_call>") afterClose
              in before <> " " <> removeBlocks remaining

-- | Convert parsed XML tool call to ToolCall format
-- Uses the Louter.Types.Streaming.ToolCall structure
convertToToolCall :: Int -> (Text, HM.HashMap Text Value) -> ToolCall
convertToToolCall index (functionName, parameters) =
  ToolCall
    { toolCallId = "call_" <> T.pack (show index)
    , toolCallName = functionName
    , toolCallArguments = Object $ KM.fromHashMapText parameters
    }
