{-# LANGUAGE OverloadedStrings #-}

-- | SSE (Server-Sent Events) parser using attoparsec
-- Parses the streaming format used by OpenAI, Gemini, and other LLM APIs
module Louter.Streaming.Parser
  ( parseSSE
  , parseSSEChunk
  , parseSSELine
  ) where

import Data.Attoparsec.Text hiding (takeWhile)
import qualified Data.Attoparsec.Text as A
import Data.Text (Text)
import qualified Data.Text as T
import Louter.Types.Streaming (SSEChunk(..))

-- | Parse a complete SSE chunk
-- Format: "data: {json}\n\n" or "data: [DONE]\n\n"
parseSSE :: Parser SSEChunk
parseSSE = parseSSEChunk <* endOfLine <* option () endOfLine

-- | Parse SSE chunk without trailing newlines
parseSSEChunk :: Parser SSEChunk
parseSSEChunk = choice
  [ parseDone
  , parseData
  ]

-- | Parse "data: [DONE]"
parseDone :: Parser SSEChunk
parseDone = string "data: [DONE]" >> pure SSEDone

-- | Parse "data: {json payload}"
parseData :: Parser SSEChunk
parseData = do
  _ <- string "data: "
  payload <- parsePayload
  pure $ SSEData payload Nothing

-- | Parse the JSON payload until end of line
-- We don't parse the JSON here - that's done by the classifier
parsePayload :: Parser Text
parsePayload = takeWhile1 (/= '\n')

-- | Parse a single SSE line (for incremental parsing)
-- Handles both "data:" lines and other SSE fields like "event:", "id:", etc.
parseSSELine :: Parser (Text, Text)
parseSSELine = do
  field <- takeWhile1 (/= ':')
  _ <- char ':'
  skipSpace
  value <- A.takeWhile (/= '\n')
  pure (field, value)
