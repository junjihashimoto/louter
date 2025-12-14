{-# LANGUAGE OverloadedStrings #-}

-- | XML Stream Processor for Qwen3-Coder streaming responses
--
-- Implements state machine for parsing XML tool calls during streaming:
-- 1. NormalText mode: Emit text chunks normally
-- 2. Detect <tool_call> → Switch to InToolCall mode
-- 3. InToolCall mode: Buffer all content (do NOT emit)
-- 4. Detect </tool_call> → Parse XML → Emit ToolCall event → Back to NormalText
--
-- This ensures XML tags are not sent to frontend and tool calls are complete.
module Louter.Streaming.XMLStreamProcessor
  ( processXMLStream
  , processXMLChunk
  , finalizeXMLState
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Data.Aeson (Value)
import Louter.Types.ToolFormat
  ( XMLMode(..)
  , XMLToolCallState(..)
  )
import Louter.Streaming.XMLToolCallParser
  ( parseXMLToolCalls
  , convertToToolCall
  )
import Louter.Types.Streaming
  ( StreamEvent(..)
  , DeltaType(..)
  , ToolCall(..)
  )

-- | Process a single text chunk from SSE stream
-- Returns updated state and list of events to emit to frontend
processXMLStream :: XMLToolCallState -> Text -> (XMLToolCallState, [StreamEvent])
processXMLStream state chunk =
  let
    -- Detect mode transitions based on XML tags in chunk
    (newMode, bufferUpdate, events) = case xmlMode state of
      NormalText ->
        if "<tool_call>" `T.isInfixOf` chunk
          then handleToolCallStart state chunk
          else handleNormalText state chunk

      InToolCall ->
        if "</tool_call>" `T.isInfixOf` chunk
          then handleToolCallEnd state chunk
          else handleToolCallBuffer state chunk

    newState = state
      { xmlMode = newMode
      , xmlBuffer = bufferUpdate
      }
  in (newState, events)

-- | Handle text chunk in NormalText mode
-- Check if <tool_call> appears, switch mode if found
handleNormalText :: XMLToolCallState -> Text -> (XMLMode, Text, [StreamEvent])
handleNormalText state chunk =
  case T.breakOn "<tool_call>" chunk of
    (before, rest) | T.null rest ->
      -- No <tool_call> found, emit entire chunk as text
      ( NormalText
      , ""
      , if T.null before then [] else [StreamContent before]
      )
    (before, rest) ->
      -- Found <tool_call>, emit text before it, start buffering
      let afterTag = T.drop (T.length "<tool_call>") rest
      in ( InToolCall
         , afterTag  -- Start buffering from after opening tag
         , if T.null before then [] else [StreamContent before]
         )

-- | Handle text chunk while in InToolCall mode (buffering)
-- Accumulate all content until closing tag
handleToolCallBuffer :: XMLToolCallState -> Text -> (XMLMode, Text, [StreamEvent])
handleToolCallBuffer state chunk =
  let newBuffer = xmlBuffer state <> chunk
  in ( InToolCall
     , newBuffer
     , []  -- Do NOT emit anything while buffering
     )

-- | Handle text chunk containing </tool_call> (end of tool call)
-- Parse accumulated XML and emit ToolCall event
handleToolCallEnd :: XMLToolCallState -> Text -> (XMLMode, Text, [StreamEvent])
handleToolCallEnd state chunk =
  case T.breakOn "</tool_call>" chunk of
    (before, rest) | T.null rest ->
      -- Should not happen (we checked </tool_call> exists)
      ( InToolCall
      , xmlBuffer state <> chunk
      , []
      )
    (before, rest) ->
      -- Complete the buffer with content before closing tag
      let completeXML = xmlBuffer state <> before
          afterTag = T.drop (T.length "</tool_call>") rest

          -- Parse the complete XML block
          parsedCalls = parseXMLToolCalls ("<tool_call>" <> completeXML <> "</tool_call>")

          -- Convert to ToolCall events
          toolCallEvents = case parsedCalls of
            [] -> []  -- No valid tool calls found
            calls ->
              let existingCount = length (xmlExtractedCalls state)
                  toolCalls = zipWith convertToToolCall [existingCount..] calls
              in map StreamToolCall toolCalls

          -- Emit any text after closing tag (if in NormalText now)
          textEvent = if T.null afterTag then [] else [StreamContent afterTag]

          -- Update extracted calls list
          newExtractedCalls = xmlExtractedCalls state ++ parsedCalls

      in ( NormalText  -- Back to normal mode
         , ""  -- Clear buffer
         , toolCallEvents ++ textEvent
         )

-- | Handle transition when <tool_call> starts
handleToolCallStart :: XMLToolCallState -> Text -> (XMLMode, Text, [StreamEvent])
handleToolCallStart state chunk =
  case T.breakOn "<tool_call>" chunk of
    (before, rest) ->
      let afterTag = T.drop (T.length "<tool_call>") rest
          textEvent = if T.null before then [] else [StreamContent before]
      in ( InToolCall
         , afterTag
         , textEvent
         )

-- | Process a DeltaType from OpenAI backend
-- This integrates with existing streaming pipeline
processXMLChunk :: XMLToolCallState -> DeltaType -> (XMLToolCallState, [StreamEvent])
processXMLChunk state deltaType =
  case deltaType of
    -- Only process ContentDelta - other types pass through
    ContentDelta text ->
      processXMLStream state text

    -- Reasoning content also needs XML processing (Qwen may put XML here)
    ReasoningDelta text ->
      let (newState, events) = processXMLStream state text
          -- Convert StreamContent back to StreamReasoning
          reasoningEvents = map convertToReasoning events
      in (newState, reasoningEvents)

    -- Pass through all other delta types unchanged
    RoleDelta _role ->
      -- Role deltas don't emit events in current StreamEvent type
      (state, [])

    ToolCallDelta _fragment ->
      -- This shouldn't happen with XML backends, but handle it
      (state, [])  -- Tool calls come from XML parsing, not deltas

    FinishDelta reason ->
      (state, [StreamFinish reason])

    EmptyDelta ->
      (state, [])

-- | Convert StreamContent to StreamReasoning
convertToReasoning :: StreamEvent -> StreamEvent
convertToReasoning (StreamContent text) = StreamReasoning text
convertToReasoning other = other

-- | Finalize XML state at end of stream
-- Emit any remaining buffered content as incomplete tool call warning
finalizeXMLState :: XMLToolCallState -> [StreamEvent]
finalizeXMLState state =
  case xmlMode state of
    NormalText -> []  -- Clean finish
    InToolCall ->
      -- Incomplete tool call - emit warning or error
      if T.null (xmlBuffer state)
        then []
        else [StreamError $ "Incomplete XML tool call: " <> xmlBuffer state]
