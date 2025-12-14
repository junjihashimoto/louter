{-# LANGUAGE OverloadedStrings #-}

-- | Gemini Streaming Protocol Handler - JSON Array Format
-- Converts OpenAI SSE stream to Gemini JSON array format (alt=json)
--
-- Gemini supports two streaming formats:
-- 1. SSE (alt=sse): "data: {...}\n\ndata: {...}\n\n" - Handled by GeminiStreaming
-- 2. JSON Array (alt=json): "[{...}, {...}]" - Handled by this module
--
-- JSON Array Format Semantics:
-- - Stream begins with "["
-- - Each response chunk is emitted as a JSON object
-- - Elements are separated by ","
-- - Stream ends with "]"
-- - Example: [{"candidates":[...]},{"candidates":[...]},{"candidates":[...]}]
--
-- The stream is INCREMENTAL - elements appear one at a time, not as a complete array.
module Louter.Protocol.GeminiStreamingJsonArray
  ( convertOpenAIToGeminiJsonArray
  ) where

import Control.Monad (foldM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value(..), Object, encode, eitherDecode, object, (.=))
import qualified Data.Aeson.KeyMap as HM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Builder (Builder, byteString, lazyByteString, char8)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Network.HTTP.Client as HTTP

import Louter.Protocol.GeminiStreaming (ToolCallState(..), processOpenAILineToGeminiStateful, openAIChunkToGemini)

-- | Convert OpenAI SSE stream to Gemini JSON array format
-- Format: [
--   {"candidates":[...]},
--   {"candidates":[...]},
--   ...
-- ]
convertOpenAIToGeminiJsonArray :: (Builder -> IO ()) -> IO () -> HTTP.BodyReader -> IO ()
convertOpenAIToGeminiJsonArray write flush bodyReader = do
  -- Track if we've emitted the opening "["
  isFirstRef <- newIORef True
  toolStateRef <- newIORef (ToolCallState Nothing Nothing "")

  -- Emit opening bracket
  write (char8 '[')
  flush

  -- Process all SSE events and emit incrementally
  let loop acc = do
        chunk <- HTTP.brRead bodyReader
        if BS.null chunk
          then do
            -- End of stream - emit closing bracket
            write (char8 ']')
            flush
          else do
            let combined = acc <> chunk
                lines' = BS.split (fromIntegral $ fromEnum '\n') combined
            case lines' of
              [] -> loop BS.empty
              [incomplete] -> loop incomplete
              _ -> do
                let (completeLines, rest) = (init lines', last lines')
                -- Process each line and emit chunks incrementally
                mapM_ (processLineAndEmit isFirstRef toolStateRef write flush) completeLines
                loop rest

  loop BS.empty

-- | Process a single SSE line and emit Gemini chunk incrementally
-- Emits: "," separator (if not first) followed by the JSON object and newline
processLineAndEmit :: IORef Bool -> IORef ToolCallState -> (Builder -> IO ()) -> IO () -> BS.ByteString -> IO ()
processLineAndEmit isFirstRef toolStateRef write flush line
  | BS.isPrefixOf "data: " line = do
      let jsonText = TE.decodeUtf8 $ BS.drop 6 line
      if jsonText == "[DONE]"
        then pure ()  -- Skip [DONE] marker
        else case eitherDecode (BL.fromStrict $ TE.encodeUtf8 jsonText) of
          Right (Object openAIChunk) -> do
            -- Convert OpenAI chunk to Gemini format
            let geminiChunk = openAIChunkToGemini openAIChunk

            -- Emit comma separator if not first element
            isFirst <- readIORef isFirstRef
            if isFirst
              then writeIORef isFirstRef False
              else do
                write (char8 ',')
                write (char8 '\n')

            -- Emit the Gemini chunk as JSON followed by newline
            write (lazyByteString $ encode geminiChunk)
            write (char8 '\n')
            flush
          _ -> pure ()
  | otherwise = pure ()
