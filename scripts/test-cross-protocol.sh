#!/bin/bash
# Test cross-protocol conversion scenarios

echo "========================================"
echo "Cross-Protocol Conversion Tests"
echo "========================================"
echo ""

echo "1. Gemini API → OpenAI Backend (Text)"
curl -s -X POST 'http://localhost:9001/v1beta/models/gpt-5-nano:generateContent' \
  -H "Content-Type: application/json" \
  -d '{"contents":[{"parts":[{"text":"Hi"}],"role":"user"}]}' \
  | jq -r '.candidates[0].content.parts[0].text' | head -1
echo "---"
echo ""

echo "2. Gemini API → OpenAI Backend (Function Calling)"
curl -s -X POST 'http://localhost:9001/v1beta/models/gpt-5-nano:generateContent' \
  -H "Content-Type: application/json" \
  -d '{"contents":[{"parts":[{"text":"Search"}],"role":"user"}],"tools":[{"functionDeclarations":[{"name":"search"}]}]}' \
  | jq -r '.candidates[0].content.parts[0].functionCall.name'
echo "---"
echo ""

echo "3. OpenAI API → Gemini Backend (Text)"
curl -s -X POST 'http://localhost:9001/v1/chat/completions' \
  -H "Content-Type: application/json" \
  -d '{"model":"gemini-2.5-flash","messages":[{"role":"user","content":"Hi"}]}' \
  | jq -r '.choices[0].message.content'
echo "---"
echo ""

echo "4. OpenAI API → Gemini Backend (Function Calling)"
curl -s -X POST 'http://localhost:9001/v1/chat/completions' \
  -H "Content-Type: application/json" \
  -d '{"model":"gemini-2.5-flash","messages":[{"role":"user","content":"Search"}],"tools":[{"type":"function","function":{"name":"search"}}]}' \
  | jq -r '.choices[0].message.tool_calls[0].function.name'
echo "---"
echo ""

echo "========================================"
echo "✅ All Tests Complete!"
echo "========================================"
