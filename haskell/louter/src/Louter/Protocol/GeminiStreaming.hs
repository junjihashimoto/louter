{-# LANGUAGE OverloadedStrings #-}

-- | Gemini Streaming Protocol Handler
-- Converts OpenAI SSE stream to Gemini newline-delimited JSON format
module Louter.Protocol.GeminiStreaming
  ( -- * State Types
    ToolCallState(..)

    -- * Main Streaming Functions
  , convertOpenAIToGeminiStream
  , streamGeminiDeltas

    -- * Internal Functions (exported for testing)
  , processOpenAILineToGeminiStateful
  , processToolCallChunk
  , openAIChunkToGemini
  ) where

import Control.Monad (foldM)
import Data.Aeson (Value(..), Object, encode, eitherDecode, object, (.=))
import qualified Data.Aeson.KeyMap as HM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Builder (Builder, byteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Network.HTTP.Client as HTTP

-- ============================================================================
-- State Types
-- ============================================================================

-- | State for tracking tool call arguments during streaming
data ToolCallState = ToolCallState
  { toolCallId :: Maybe Text
  , toolCallName :: Maybe Text
  , toolCallArgs :: Text  -- Accumulated arguments string
  } deriving (Show)

-- ============================================================================
-- Main Streaming Functions
-- ============================================================================

-- | Convert OpenAI SSE stream to Gemini newline-delimited JSON format
convertOpenAIToGeminiStream :: (Builder -> IO ()) -> IO () -> HTTP.BodyReader -> IO ()
convertOpenAIToGeminiStream write flush bodyReader = do
  streamGeminiDeltas write flush bodyReader

-- | Stream Gemini deltas from OpenAI response
streamGeminiDeltas :: (Builder -> IO ()) -> IO () -> HTTP.BodyReader -> IO ()
streamGeminiDeltas write flush bodyReader = loop BS.empty (ToolCallState Nothing Nothing "")
  where
    loop acc toolState = do
      chunk <- HTTP.brRead bodyReader
      if BS.null chunk
        then pure ()
        else do
          let combined = acc <> chunk
              lines' = BS.split (fromIntegral $ fromEnum '\n') combined
          case lines' of
            [] -> loop BS.empty toolState
            [incomplete] -> loop incomplete toolState
            _ -> do
              let (completeLines, rest) = (init lines', last lines')
              newToolState <- foldM (processOpenAILineToGeminiStateful write flush) toolState completeLines
              loop rest newToolState

-- ============================================================================
-- SSE Line Processing
-- ============================================================================

-- | Process a single OpenAI SSE line with state tracking for tool calls
processOpenAILineToGeminiStateful :: (Builder -> IO ()) -> IO () -> ToolCallState -> BS.ByteString -> IO ToolCallState
processOpenAILineToGeminiStateful write flush toolState line
  | BS.isPrefixOf "data: " line = do
      let jsonText = TE.decodeUtf8 $ BS.drop 6 line
      if jsonText == "[DONE]"
        then pure toolState  -- Gemini doesn't send [DONE]
        else case eitherDecode (BL.fromStrict $ TE.encodeUtf8 jsonText) of
          Right (Object openAIChunk) -> do
            -- Check if this chunk contains tool_calls
            let hasToolCalls = case HM.lookup "choices" openAIChunk of
                  Just (Array choices) | not (V.null choices) ->
                    case V.head choices of
                      Object choice -> case HM.lookup "delta" choice of
                        Just (Object delta) -> HM.member "tool_calls" delta
                        _ -> False
                      _ -> False
                  _ -> False

            -- Check if this is a finish_reason = "tool_calls" chunk with buffered state
            let finishReason = case HM.lookup "choices" openAIChunk of
                  Just (Array choices) | not (V.null choices) ->
                    case V.head choices of
                      Object choice -> HM.lookup "finish_reason" choice
                      _ -> Nothing
                  _ -> Nothing
                hasBufferedToolCall = toolCallName toolState /= Nothing

            if hasToolCalls
              then do
                -- Process tool call and update state
                (newState, maybeGeminiChunk) <- processToolCallChunk toolState openAIChunk
                case maybeGeminiChunk of
                  Just geminiChunk -> do
                    write (byteString $ BS8.pack "data: " <> BL.toStrict (encode geminiChunk) <> BS8.pack "\n\n")
                    flush
                  Nothing -> pure ()
                pure newState
              else if finishReason == Just (String "tool_calls") && hasBufferedToolCall
                then do
                  -- Emit buffered tool call
                  (newState, maybeGeminiChunk) <- processToolCallChunk toolState openAIChunk
                  case maybeGeminiChunk of
                    Just geminiChunk -> do
                      write (byteString $ BS8.pack "data: " <> BL.toStrict (encode geminiChunk) <> BS8.pack "\n\n")
                      flush
                    Nothing -> pure ()
                  pure newState
                else do
                  -- Regular text/reasoning chunk
                  let geminiChunk = openAIChunkToGemini openAIChunk
                  write (byteString $ BS8.pack "data: " <> BL.toStrict (encode geminiChunk) <> BS8.pack "\n\n")
                  flush
                  pure toolState
          _ -> pure toolState
  | otherwise = pure toolState

-- ============================================================================
-- Tool Call Processing
-- ============================================================================

-- | Process tool call chunk, accumulating arguments until complete
processToolCallChunk :: ToolCallState -> HM.KeyMap Value -> IO (ToolCallState, Maybe Value)
processToolCallChunk state openAIChunk = do
  let choices = case HM.lookup "choices" openAIChunk of
        Just (Array cs) | not (V.null cs) -> V.head cs
        _ -> Object HM.empty
      delta = case choices of
        Object choice -> case HM.lookup "delta" choice of
          Just (Object d) -> d
          _ -> HM.empty
        _ -> HM.empty
      toolCalls = case HM.lookup "tool_calls" delta of
        Just (Array tcs) | not (V.null tcs) -> Just $ V.head tcs
        _ -> Nothing
      finishReason = case choices of
        Object choice -> HM.lookup "finish_reason" choice
        _ -> Nothing

  -- Check if we should emit based on finish_reason, even without new tool_calls
  case finishReason of
    Just (String "tool_calls") | toolCallName state /= Nothing -> do
      -- Arguments are complete, emit the buffered function call
      let parsedArgs = case eitherDecode (BL.fromStrict $ TE.encodeUtf8 (toolCallArgs state)) of
            Right val -> val
            Left _ -> object []
          geminiChunk = object
            [ "candidates" .= [object
                [ "content" .= object
                    [ "parts" .= [object $
                        [ "functionCall" .= object
                            ([ "name" .= n | Just n <- [toolCallName state] ] ++
                             [ "args" .= parsedArgs ])
                        ] ++ [ "id" .= i | Just i <- [toolCallId state] ]]
                    , "role" .= ("model" :: Text)
                    ]
                , "finishReason" .= ("tool_calls" :: Text)
                ]]
            , "usageMetadata" .= object
                [ "promptTokenCount" .= (0 :: Int)
                , "candidatesTokenCount" .= (0 :: Int)
                , "totalTokenCount" .= (0 :: Int)
                ]
            ]
      -- Reset state for next tool call
      pure (ToolCallState Nothing Nothing "", Just geminiChunk)
    _ -> do
      -- Process new tool_calls delta if present
      case toolCalls of
        Just (Object tc) -> do
          let tcId = case HM.lookup "id" tc of
                Just (String i) -> Just i
                _ -> Nothing
              tcFunc = case HM.lookup "function" tc of
                Just (Object f) -> f
                _ -> HM.empty
              funcName = case HM.lookup "name" tcFunc of
                Just (String n) -> Just n
                _ -> Nothing
              funcArgs = case HM.lookup "arguments" tcFunc of
                Just (String args) -> args
                _ -> ""

          -- Update state with new information
          let newId = case tcId of Just i -> Just i; Nothing -> toolCallId state
              newName = case funcName of Just n -> Just n; Nothing -> toolCallName state
              newArgs = toolCallArgs state <> funcArgs

          -- Still accumulating, don't emit yet
          pure (ToolCallState newId newName newArgs, Nothing)
        _ -> pure (state, Nothing)

-- ============================================================================
-- Chunk Conversion
-- ============================================================================

-- | Convert OpenAI chunk to Gemini chunk
openAIChunkToGemini :: HM.KeyMap Value -> Value
openAIChunkToGemini openAIChunk =
  let candidates = case HM.lookup "choices" openAIChunk of
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
      let delta = case HM.lookup "delta" choice of
            Just (Object d) -> d
            _ -> HM.empty
          finishReason = HM.lookup "finish_reason" choice
          -- Extract text from either content or reasoning
          textParts = case (HM.lookup "content" delta, HM.lookup "reasoning" delta) of
            (Just (String txt), _) -> [object ["text" .= txt]]
            (_, Just (String txt)) -> [object ["text" .= txt]]
            _ -> []
          -- Extract tool calls and convert to Gemini functionCall format
          toolCallParts = case HM.lookup "tool_calls" delta of
            Just (Array toolCalls) -> V.toList $ V.map convertToolCall toolCalls
            _ -> []
          parts = textParts ++ toolCallParts
      in object $
          [ "content" .= object
              [ "parts" .= parts
              , "role" .= ("model" :: Text)
              ]
          ] ++ (case finishReason of
                  Just r -> ["finishReason" .= r]
                  Nothing -> [])
    convertChoice _ = object []

    -- Convert OpenAI tool_call to Gemini functionCall part
    convertToolCall (Object tc) =
      let tcId = HM.lookup "id" tc
          tcFunc = case HM.lookup "function" tc of
            Just (Object f) -> f
            _ -> HM.empty
          funcName = HM.lookup "name" tcFunc
          funcArgs = HM.lookup "arguments" tcFunc
      in object $
          [ "functionCall" .= object
              ([ "name" .= fname | Just fname <- [funcName] ] ++
               [ "args" .= parseArgs args | Just args <- [funcArgs] ])
          ] ++ [ "id" .= tid | Just tid <- [tcId] ]
    convertToolCall _ = object []

    -- Parse function arguments string to JSON object
    parseArgs (String argsStr) = case eitherDecode (BL.fromStrict $ TE.encodeUtf8 argsStr) of
      Right val -> val
      Left _ -> object []
    parseArgs other = other
