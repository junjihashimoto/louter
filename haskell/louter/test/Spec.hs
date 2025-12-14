{-# LANGUAGE OverloadedStrings #-}

-- | Test suite for louter
module Main (main) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.HashMap.Strict as HM
import Data.Aeson (Value(..))
import Data.Text (Text)
import qualified Data.Text as T

import Louter.Streaming.XMLToolCallParser
import Louter.Types.Streaming (ToolCall(..))

main :: IO ()
main = hspec $ do
  describe "XML Tool Call Parser" $ do
    describe "parseXMLToolCalls" $ do
      it "parses a simple XML tool call" $ do
        let xmlInput = "<tool_call><function=WriteFile><parameter=file_path>test.txt</parameter><parameter=content>Hello World!</parameter></function></tool_call>"
        let result = parseXMLToolCalls xmlInput
        length result `shouldBe` 1
        case result of
          [(name, params)] -> do
            name `shouldBe` "WriteFile"
            HM.lookup "file_path" params `shouldBe` Just (String "test.txt")
            HM.lookup "content" params `shouldBe` Just (String "Hello World!")
          _ -> expectationFailure "Should parse exactly one tool call"

      it "parses multiple tool calls" $ do
        let xmlInput = "<tool_call><function=CreateDirectory><parameter=path>/tmp/test</parameter></function></tool_call>\
                       \<tool_call><function=WriteFile><parameter=file_path>/tmp/test/file.txt</parameter></function></tool_call>"
        let result = parseXMLToolCalls xmlInput
        length result `shouldBe` 2

      it "handles empty input" $ do
        let result = parseXMLToolCalls ""
        result `shouldBe` []

      it "handles text without tool calls" $ do
        let result = parseXMLToolCalls "Just some regular text"
        result `shouldBe` []

      it "preserves number types" $ do
        let xmlInput = "<tool_call><function=SetCount><parameter=count>42</parameter></function></tool_call>"
        let result = parseXMLToolCalls xmlInput
        case result of
          [(_, params)] ->
            HM.lookup "count" params `shouldBe` Just (Number 42)
          _ -> expectationFailure "Should parse tool call with number"

      it "preserves boolean types" $ do
        let xmlInput = "<tool_call><function=SetFlag><parameter=enabled>true</parameter></function></tool_call>"
        let result = parseXMLToolCalls xmlInput
        case result of
          [(_, params)] ->
            HM.lookup "enabled" params `shouldBe` Just (Bool True)
          _ -> expectationFailure "Should parse tool call with boolean"

    describe "extractFunctionName" $ do
      it "extracts function name from XML" $ do
        extractFunctionName "<function=WriteFile>" `shouldBe` Just "WriteFile"

      it "returns Nothing for invalid format" $ do
        extractFunctionName "no function here" `shouldBe` Nothing

    describe "stripXMLToolCallTags" $ do
      it "removes XML tool call tags from text" $ do
        let input = "Before <tool_call><function=Test></function></tool_call> After"
        let result = stripXMLToolCallTags input
        -- Should contain "Before" and "After" with XML removed
        T.isInfixOf "Before" result `shouldBe` True
        T.isInfixOf "After" result `shouldBe` True
        T.isInfixOf "<tool_call>" result `shouldBe` False

      it "keeps text without tool calls unchanged" $ do
        let input = "No tool calls here"
        stripXMLToolCallTags input `shouldBe` input

      it "handles multiple tool calls" $ do
        let input = "A <tool_call>...</tool_call> B <tool_call>...</tool_call> C"
        let result = stripXMLToolCallTags input
        -- Should contain A, B, C with XML removed
        T.isInfixOf "A" result `shouldBe` True
        T.isInfixOf "B" result `shouldBe` True
        T.isInfixOf "C" result `shouldBe` True
        T.isInfixOf "<tool_call>" result `shouldBe` False

    describe "convertToToolCall" $ do
      it "converts parsed XML to ToolCall structure" $ do
        let params = HM.fromList [("file_path", String "test.txt"), ("content", String "data")]
        let toolCall = convertToToolCall 0 ("WriteFile", params)
        toolCallId toolCall `shouldBe` "call_0"
        toolCallName toolCall `shouldBe` "WriteFile"

      it "generates unique IDs for multiple tool calls" $ do
        let params1 = HM.fromList [("path", String "/tmp")]
        let params2 = HM.fromList [("name", String "file.txt")]
        let tc1 = convertToToolCall 0 ("Func1", params1)
        let tc2 = convertToToolCall 1 ("Func2", params2)
        toolCallId tc1 `shouldBe` "call_0"
        toolCallId tc2 `shouldBe` "call_1"
