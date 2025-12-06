{-# LANGUAGE OverloadedStrings #-}

-- | Stream processor with stateful function call buffering
-- This implements the core logic:
-- - reasoning/content: stream immediately
-- - tool_calls: buffer until complete JSON, then emit
module Louter.Streaming.Processor
  ( processChunk
  , processStream
  , isCompleteJSON
  , updateToolCallBuffer
  , checkAndEmitToolCall
  ) where

import Control.Monad.State.Strict
import Data.Aeson (eitherDecode, Value)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Louter.Types.Streaming

-- | Process a single chunk with state transformation
-- Returns the new state and optional output events
processChunk :: DeltaType -> State StreamState [StreamEvent]
processChunk deltaType = case deltaType of
  -- Stream immediately - no buffering
  ReasoningDelta txt -> pure [StreamReasoning txt]
  ContentDelta txt -> pure [StreamContent txt]
  RoleDelta _ -> pure []  -- Metadata, don't emit
  EmptyDelta -> pure []

  -- Finish event - emit any pending tool calls first, then finish
  FinishDelta reason -> do
    pending <- flushAllToolCalls
    pure $ pending ++ [StreamFinish reason]

  -- Tool calls - buffer until complete
  ToolCallDelta frag -> processToolCallFragment frag

-- | Process a tool call fragment (may buffer or emit)
processToolCallFragment :: ToolCallFragment -> State StreamState [StreamEvent]
processToolCallFragment frag = do
  updateToolCallBuffer frag
  checkAndEmitToolCall (tcfIndex frag)

-- | Update the tool call buffer with a new fragment
updateToolCallBuffer :: ToolCallFragment -> State StreamState ()
updateToolCallBuffer frag = modify $ \s ->
  let idx = tcfIndex frag
      currentState = Map.lookup idx (ssToolCalls s)
  in case currentState of
    Nothing ->
      -- First fragment - create new state
      case (tcfId frag, tcfName frag) of
        (Just callId, Just name) ->
          let newState = ToolCallState
                { tcsId = callId
                , tcsName = name
                , tcsArguments = maybe mempty TB.fromText (tcfArguments frag)
                , tcsComplete = False
                }
          in s { ssToolCalls = Map.insert idx newState (ssToolCalls s) }
        _ -> s  -- Invalid fragment, ignore

    Just existingState ->
      -- Append arguments
      case tcfArguments frag of
        Just args ->
          let newArgs = tcsArguments existingState <> TB.fromText args
          in s { ssToolCalls = Map.insert idx
                  (existingState { tcsArguments = newArgs })
                  (ssToolCalls s) }
        Nothing -> s

-- | Check if tool call is complete and emit if so
checkAndEmitToolCall :: Int -> State StreamState [StreamEvent]
checkAndEmitToolCall idx = do
  s <- get
  case Map.lookup idx (ssToolCalls s) of
    Nothing -> pure []
    Just tcState ->
      let argsText = TL.toStrict $ TB.toLazyText (tcsArguments tcState)
      in if isCompleteJSON argsText
         then do
           -- Remove from buffer
           modify $ \st -> st { ssToolCalls = Map.delete idx (ssToolCalls st) }
           -- Try to parse JSON
           case eitherDecode (BL.fromStrict $ TE.encodeUtf8 argsText) of
             Right argsValue ->
               let toolCall = ToolCall
                     { toolCallId = tcsId tcState
                     , toolCallName = tcsName tcState
                     , toolCallArguments = argsValue
                     }
               in pure [StreamToolCall toolCall]
             Left err ->
               pure [StreamError $ "Failed to parse tool arguments: " <> T.pack err]
         else
           pure []  -- Keep buffering

-- | Flush all pending tool calls (called on finish)
flushAllToolCalls :: State StreamState [StreamEvent]
flushAllToolCalls = do
  s <- get
  let allCalls = Map.elems (ssToolCalls s)
  put $ s { ssToolCalls = Map.empty }

  pure $ map emitIncomplete allCalls
  where
    emitIncomplete tcState =
      let argsText = TL.toStrict $ TB.toLazyText (tcsArguments tcState)
      in case eitherDecode (BL.fromStrict $ TE.encodeUtf8 argsText) of
           Right argsValue ->
             StreamToolCall $ ToolCall
               { toolCallId = tcsId tcState
               , toolCallName = tcsName tcState
               , toolCallArguments = argsValue
               }
           Left _ ->
             StreamError $ "Incomplete tool call: " <> tcsName tcState

-- | Check if a JSON string is complete and valid
-- Must start with '{', end with '}', and parse successfully
isCompleteJSON :: Text -> Bool
isCompleteJSON txt =
  let trimmed = T.strip txt
  in not (T.null trimmed)
     && T.head trimmed == '{'
     && T.last trimmed == '}'
     && case eitherDecode (BL.fromStrict $ TE.encodeUtf8 trimmed) :: Either String Value of
          Right _ -> True
          Left _ -> False

-- | Process a stream of delta types, maintaining state
-- This is the high-level interface for processing a complete stream
processStream :: Text -> [DeltaType] -> [StreamEvent]
processStream messageId deltas =
  let initialState = emptyStreamState messageId
      (events, _) = runState (concat <$> mapM processChunk deltas) initialState
  in events
