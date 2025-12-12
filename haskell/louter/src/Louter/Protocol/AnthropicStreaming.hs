{-# LANGUAGE OverloadedStrings #-}

-- | Anthropic Streaming Protocol Handler
-- Converts OpenAI SSE stream to Anthropic SSE format with stateful tool call buffering
module Louter.Protocol.AnthropicStreaming
  ( -- * State Types
    AnthropicStreamState(..)
  , AnthropicToolCallState(..)

    -- * Main Streaming Function
  , convertOpenAIToAnthropic

    -- * Internal Functions (exported for testing)
  , streamAnthropicDeltas
  , processOpenAILineToAnthropicStateful
  , processAnthropicChoice
  , processAnthropicToolCalls
  , processAnthropicSingleToolCall
  , emitAnthropicToolCalls
  ) where

import Control.Applicative ((<|>))
import Control.Monad (foldM, forM_, unless, when)
import Data.Aeson (Value(..), Object, encode, eitherDecode, object, (.=))
import qualified Data.Aeson.KeyMap as HM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Builder (Builder, byteString)
import qualified Data.HashMap.Strict as HMS
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Network.HTTP.Client as HTTP

-- ============================================================================
-- State Types
-- ============================================================================

-- | Tool call state for Anthropic streaming
data AnthropicToolCallState = AnthropicToolCallState
  { anthropicToolCallId :: Maybe Text
  , anthropicToolCallName :: Maybe Text
  , anthropicToolCallArgs :: Text  -- Accumulated arguments string
  } deriving (Show)

-- | Anthropic streaming state
data AnthropicStreamState = AnthropicStreamState
  { anthropicToolCalls :: HMS.HashMap Int AnthropicToolCallState
  , anthropicContentBlockStarted :: Bool
  , anthropicCurrentIndex :: Int
  } deriving (Show)

-- ============================================================================
-- Main Streaming Function
-- ============================================================================

-- | Convert OpenAI SSE stream to Anthropic SSE format
convertOpenAIToAnthropic :: (Builder -> IO ()) -> IO () -> HTTP.BodyReader -> IO ()
convertOpenAIToAnthropic write flush bodyReader = do
  -- Send message_start event
  write (byteString $ BS8.pack "event: message_start\ndata: {\"type\":\"message_start\",\"message\":{\"id\":\"msg_1\",\"type\":\"message\",\"role\":\"assistant\",\"content\":[],\"model\":\"claude-3-haiku-20240307\",\"stop_reason\":null,\"usage\":{\"input_tokens\":10,\"output_tokens\":0}}}\n\n")
  flush

  -- Stream with stateful tool call tracking
  let initialState = AnthropicStreamState HMS.empty False 0
  streamAnthropicDeltas write flush bodyReader initialState

-- ============================================================================
-- Streaming Loop
-- ============================================================================

-- | Stream Anthropic deltas with tool call state management
streamAnthropicDeltas :: (Builder -> IO ()) -> IO () -> HTTP.BodyReader -> AnthropicStreamState -> IO ()
streamAnthropicDeltas write flush bodyReader initialState = loop BS.empty initialState
  where
    loop acc state = do
      chunk <- HTTP.brRead bodyReader
      if BS.null chunk
        then finalize state
        else do
          let combined = acc <> chunk
              lines' = BS.split (fromIntegral $ fromEnum '\n') combined
          case lines' of
            [] -> loop BS.empty state
            [incomplete] -> loop incomplete state
            _ -> do
              let (completeLines, rest) = (init lines', last lines')
              newState <- foldM (processOpenAILineToAnthropicStateful write flush) state completeLines
              loop rest newState

    finalize state = do
      -- Close any open content blocks
      when (anthropicContentBlockStarted state) $ do
        write (byteString $ BS8.pack $ "event: content_block_stop\ndata: {\"type\":\"content_block_stop\",\"index\":" ++ show (anthropicCurrentIndex state) ++ "}\n\n")
        flush

      -- Send message_delta with stop_reason
      write (byteString $ BS8.pack "event: message_delta\ndata: {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\",\"stop_sequence\":null},\"usage\":{\"output_tokens\":10}}\n\n")
      flush

      -- Send message_stop
      write (byteString $ BS8.pack "event: message_stop\ndata: {\"type\":\"message_stop\"}\n\n")
      flush

-- ============================================================================
-- SSE Line Processing
-- ============================================================================

-- | Process a single OpenAI SSE line and convert to Anthropic format (stateful)
processOpenAILineToAnthropicStateful :: (Builder -> IO ()) -> IO () -> AnthropicStreamState -> BS.ByteString -> IO AnthropicStreamState
processOpenAILineToAnthropicStateful write flush state line
  | BS.isPrefixOf "data: " line = do
      let jsonText = TE.decodeUtf8 $ BS.drop 6 line
      if jsonText == "[DONE]"
        then pure state  -- Don't send [DONE] in Anthropic format
        else case eitherDecode (BL.fromStrict $ TE.encodeUtf8 jsonText) of
          Right (Object openAIChunk) -> do
            -- Extract choices
            case HM.lookup "choices" openAIChunk of
              Just (Array choices) | not (V.null choices) -> do
                case V.head choices of
                  Object choice -> processAnthropicChoice write flush state choice openAIChunk
                  _ -> pure state
              _ -> pure state
          _ -> pure state
  | otherwise = pure state

-- ============================================================================
-- Choice Processing
-- ============================================================================

-- | Process a single choice and update Anthropic state
processAnthropicChoice :: (Builder -> IO ()) -> IO () -> AnthropicStreamState -> Object -> Object -> IO AnthropicStreamState
processAnthropicChoice write flush state choice _openAIChunk = do
  let finishReason = case HM.lookup "finish_reason" choice of
        Just (String reason) -> Just reason
        _ -> Nothing

  case HM.lookup "delta" choice of
    Just (Object delta) -> do
      -- Check for text content
      let hasContent = HM.member "content" delta
      let hasToolCalls = HM.member "tool_calls" delta

      newState <- if hasContent
        then do
          -- Start text content block if not started
          unless (anthropicContentBlockStarted state) $ do
            let idx = anthropicCurrentIndex state
            let startEvent = object
                  [ "type" .= ("content_block_start" :: Text)
                  , "index" .= idx
                  , "content_block" .= object
                      [ "type" .= ("text" :: Text)
                      , "text" .= ("" :: Text)
                      ]
                  ]
            write (byteString $ BS8.pack "event: content_block_start\ndata: " <> BL.toStrict (encode startEvent) <> BS8.pack "\n\n")
            flush

          -- Send content delta
          case HM.lookup "content" delta of
            Just (String content) -> do
              let idx = anthropicCurrentIndex state
              let deltaEvent = object
                    [ "type" .= ("content_block_delta" :: Text)
                    , "index" .= idx
                    , "delta" .= object
                        [ "type" .= ("text_delta" :: Text)
                        , "text" .= content
                        ]
                    ]
              write (byteString $ BS8.pack "event: content_block_delta\ndata: " <> BL.toStrict (encode deltaEvent) <> BS8.pack "\n\n")
              flush
            _ -> pure ()

          pure $ state { anthropicContentBlockStarted = True }

        else if hasToolCalls
          then processAnthropicToolCalls write flush state delta finishReason
          else pure state

      -- Handle finish_reason
      finalState <- case finishReason of
        Just "tool_calls" ->
          -- Emit buffered tool calls
          emitAnthropicToolCalls write flush newState

        Just _ ->
          -- Close text content block if open
          if anthropicContentBlockStarted newState
            then do
              let idx = anthropicCurrentIndex newState
              write (byteString $ BS8.pack $ "event: content_block_stop\ndata: {\"type\":\"content_block_stop\",\"index\":" ++ show idx ++ "}\n\n")
              flush
              pure newState { anthropicContentBlockStarted = False }
            else pure newState

        Nothing -> pure newState

      pure finalState

    _ -> pure state

-- ============================================================================
-- Tool Call Processing
-- ============================================================================

-- | Process tool calls delta and buffer them
processAnthropicToolCalls :: (Builder -> IO ()) -> IO () -> AnthropicStreamState -> Object -> Maybe Text -> IO AnthropicStreamState
processAnthropicToolCalls _write _flush state delta _finishReason = do
  case HM.lookup "tool_calls" delta of
    Just (Array toolCallsArray) -> do
      foldM (processAnthropicSingleToolCall _write _flush) state (V.toList toolCallsArray)
    _ -> pure state

-- | Process a single tool call fragment
processAnthropicSingleToolCall :: (Builder -> IO ()) -> IO () -> AnthropicStreamState -> Value -> IO AnthropicStreamState
processAnthropicSingleToolCall _write _flush state (Object tcDelta) = do
  let tcIndex = case HM.lookup "index" tcDelta of
        Just (Number n) -> floor n :: Int
        _ -> 0

  let tcId = case HM.lookup "id" tcDelta of
        Just (String i) -> Just i
        _ -> Nothing

  let tcFunc = HM.lookup "function" tcDelta

  let tcName = case tcFunc of
        Just (Object f) -> case HM.lookup "name" f of
          Just (String n) -> Just n
          _ -> Nothing
        _ -> Nothing

  let tcArgs = case tcFunc of
        Just (Object f) -> case HM.lookup "arguments" f of
          Just (String a) -> a
          _ -> ""
        _ -> ""

  -- Update state
  let currentTc = HMS.lookupDefault (AnthropicToolCallState Nothing Nothing "") tcIndex (anthropicToolCalls state)
  let updatedTc = AnthropicToolCallState
        { anthropicToolCallId = tcId <|> anthropicToolCallId currentTc
        , anthropicToolCallName = tcName <|> anthropicToolCallName currentTc
        , anthropicToolCallArgs = anthropicToolCallArgs currentTc <> tcArgs
        }
  let newToolCalls = HMS.insert tcIndex updatedTc (anthropicToolCalls state)

  pure $ state { anthropicToolCalls = newToolCalls }

processAnthropicSingleToolCall _ _ state _ = pure state

-- | Emit buffered tool calls as Anthropic tool_use blocks
emitAnthropicToolCalls :: (Builder -> IO ()) -> IO () -> AnthropicStreamState -> IO AnthropicStreamState
emitAnthropicToolCalls write flush state = do
  let toolCallsList = HMS.toList (anthropicToolCalls state)
  let sortedToolCalls = sortBy (comparing fst) toolCallsList

  forM_ sortedToolCalls $ \(tcIndex, tcState) -> do
    case (anthropicToolCallId tcState, anthropicToolCallName tcState) of
      (Just toolId, Just toolName) -> do
        -- Parse arguments as JSON
        let argsJson = case eitherDecode (BL.fromStrict $ TE.encodeUtf8 (anthropicToolCallArgs tcState)) of
              Right val -> val
              Left _ -> object []

        -- Calculate content block index (after text blocks)
        let blockIndex = anthropicCurrentIndex state + tcIndex

        -- Send content_block_start
        let startEvent = object
              [ "type" .= ("content_block_start" :: Text)
              , "index" .= blockIndex
              , "content_block" .= object
                  [ "type" .= ("tool_use" :: Text)
                  , "id" .= toolId
                  , "name" .= toolName
                  ]
              ]
        write (byteString $ BS8.pack "event: content_block_start\ndata: " <> BL.toStrict (encode startEvent) <> BS8.pack "\n\n")
        flush

        -- Send input_json_delta
        let deltaEvent = object
              [ "type" .= ("content_block_delta" :: Text)
              , "index" .= blockIndex
              , "delta" .= object
                  [ "type" .= ("input_json_delta" :: Text)
                  , "partial_json" .= TE.decodeUtf8 (BL.toStrict $ encode argsJson)
                  ]
              ]
        write (byteString $ BS8.pack "event: content_block_delta\ndata: " <> BL.toStrict (encode deltaEvent) <> BS8.pack "\n\n")
        flush

        -- Send content_block_stop
        let stopEvent = object
              [ "type" .= ("content_block_stop" :: Text)
              , "index" .= blockIndex
              ]
        write (byteString $ BS8.pack "event: content_block_stop\ndata: " <> BL.toStrict (encode stopEvent) <> BS8.pack "\n\n")
        flush

      _ -> pure ()

  pure $ state { anthropicCurrentIndex = anthropicCurrentIndex state + length sortedToolCalls }
