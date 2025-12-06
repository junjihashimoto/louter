{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Streaming-specific types for SSE parsing and delta classification
-- These types are protocol-agnostic and form the Internal Representation (IR)
module Louter.Types.Streaming
  ( -- * SSE Chunk Types
    SSEChunk(..)
    -- * Delta Types (Protocol-Agnostic IR)
  , DeltaType(..)
  , ToolCallFragment(..)
    -- * Stream State
  , StreamState(..)
  , ToolCallState(..)
  , emptyStreamState
    -- * Output Events
  , StreamEvent(..)
  , ToolCall(..)
  ) where

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text.Lazy.Builder as TB
import GHC.Generics (Generic)

-- | Server-Sent Event chunk (protocol-agnostic)
-- All protocols (OpenAI, Gemini, Anthropic) parse to this
data SSEChunk
  = SSEData
      { sseData :: !Text      -- ^ JSON payload after "data: "
      , sseEvent :: Maybe Text -- ^ Optional event type
      }
  | SSEDone                   -- ^ [DONE] marker or end-of-stream
  deriving (Show, Eq, Generic)

-- | Classification of delta content types (Internal Representation)
-- Different types require different buffering strategies
data DeltaType
  = ReasoningDelta !Text       -- ^ Thinking tokens (stream immediately)
  | ContentDelta !Text         -- ^ Response text (stream immediately)
  | ToolCallDelta !ToolCallFragment  -- ^ Function call (buffer until complete)
  | RoleDelta !Text            -- ^ Role assignment (pass through)
  | FinishDelta !Text          -- ^ finish_reason (pass through)
  | EmptyDelta                 -- ^ Empty delta {}
  deriving (Show, Eq, Generic)

-- | Incremental fragment of a tool call
-- Represents a piece of a streaming function call
data ToolCallFragment = ToolCallFragment
  { tcfIndex :: !Int           -- ^ Tool call index (for parallel calls)
  , tcfId :: !(Maybe Text)     -- ^ Tool call ID (only in first chunk)
  , tcfName :: !(Maybe Text)   -- ^ Function name (only in first chunk)
  , tcfArguments :: !(Maybe Text) -- ^ JSON fragment
  } deriving (Show, Eq, Generic)

instance FromJSON ToolCallFragment
instance ToJSON ToolCallFragment

-- | State machine for streaming processing
-- Tracks ongoing tool calls and buffers their arguments
data StreamState = StreamState
  { ssToolCalls :: !(Map Int ToolCallState) -- ^ Active tool calls by index
  , ssMessageId :: !Text                     -- ^ Current message ID
  } deriving (Show, Eq, Generic)

-- | State of a single tool call being assembled
data ToolCallState = ToolCallState
  { tcsId :: !Text              -- ^ Tool call ID
  , tcsName :: !Text            -- ^ Function name
  , tcsArguments :: !TB.Builder -- ^ Accumulated JSON arguments
  , tcsComplete :: !Bool        -- ^ Whether JSON is complete
  } deriving (Show, Generic)

-- Manual Eq instance since Builder doesn't have Eq
instance Eq ToolCallState where
  a == b = tcsId a == tcsId b
        && tcsName a == tcsName b
        && tcsComplete a == tcsComplete b

-- | Create empty initial state
emptyStreamState :: Text -> StreamState
emptyStreamState msgId = StreamState
  { ssToolCalls = Map.empty
  , ssMessageId = msgId
  }

-- | High-level events emitted to the user (Protocol-Agnostic)
-- These are what applications receive, regardless of backend protocol
data StreamEvent
  = StreamContent !Text        -- ^ Text content chunk
  | StreamReasoning !Text      -- ^ Reasoning/thinking chunk
  | StreamToolCall !ToolCall   -- ^ Complete tool call (buffered)
  | StreamFinish !Text         -- ^ Stream finished with reason
  | StreamError !Text          -- ^ Error occurred
  deriving (Show, Eq, Generic)

-- | Complete tool call with validated JSON arguments
data ToolCall = ToolCall
  { toolCallId :: !Text
  , toolCallName :: !Text
  , toolCallArguments :: !Value -- ^ Parsed JSON arguments
  } deriving (Show, Eq, Generic)

instance FromJSON ToolCall
instance ToJSON ToolCall
