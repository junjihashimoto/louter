#!/bin/bash
# Test Cross-Protocol Diagnostics

PORT=9000
BASE_URL="http://localhost:$PORT"

echo "======================================"
echo "Cross-Protocol Diagnostics Test"
echo "======================================"
echo

# Test 1: OpenAI API → OpenAI backend (native)
echo "Test 1: OpenAI API → OpenAI backend (gpt-5-nano) - NATIVE MODE"
curl -s -X POST "$BASE_URL/v1/chat/completions" \
  -H "Content-Type: application/json" \
  -d '{"model":"gpt-5-nano","messages":[{"role":"user","content":"Hi"}],"max_completion_tokens":20}' \
  | jq -r '.choices[0].message.content' | head -1
echo
echo "---"
echo

# Test 2: OpenAI API → gpt-oss backend (native)
echo "Test 2: OpenAI API → gpt-oss backend - NATIVE MODE"
curl -s -X POST "$BASE_URL/v1/chat/completions" \
  -H "Content-Type: application/json" \
  -d '{"model":"gpt-oss:20b","messages":[{"role":"user","content":"Hi"}],"max_completion_tokens":20}' \
  | jq -r '.choices[0].message.content // .error // "Error"' | head -1
echo
echo "---"
echo

# Test 3: Gemini API → gemini backend (if available) - NATIVE MODE
echo "Test 3: Gemini API → gemini-2.5-flash backend - NATIVE/CONVERSION MODE"
curl -s -X POST "$BASE_URL/v1beta/models/gemini-2.5-flash:generateContent" \
  -H "Content-Type: application/json" \
  -d '{"contents":[{"parts":[{"text":"Hi"}],"role":"user"}],"generationConfig":{"maxOutputTokens":20}}' \
  | jq -r '.candidates[0].content.parts[0].text // .error.message // "Error"' | head -1
echo
echo "---"
echo

# Test 4: Gemini API → OpenAI backend (conversion)
echo "Test 4: Gemini API → gpt-5-nano backend - CONVERSION MODE"
curl -s -X POST "$BASE_URL/v1beta/models/gpt-5-nano:generateContent" \
  -H "Content-Type: application/json" \
  -d '{"contents":[{"parts":[{"text":"Hi"}],"role":"user"}],"generationConfig":{"maxOutputTokens":20}}' \
  | jq -r '.candidates[0].content.parts[0].text // .error.message // "Error"' | head -1
echo
echo "---"
echo

# Test 5: Function calling via OpenAI API
echo "Test 5: OpenAI API → Function Calling Test"
curl -s -X POST "$BASE_URL/v1/chat/completions" \
  -H "Content-Type: application/json" \
  -d '{"model":"gpt-5-nano","messages":[{"role":"user","content":"Search for hello"}],"tools":[{"type":"function","function":{"name":"search","description":"Search","parameters":{"type":"object","properties":{"query":{"type":"string"}},"required":["query"]}}}],"tool_choice":"auto","max_completion_tokens":50}' \
  | jq -r '.choices[0].message.tool_calls[0].function.name // "No function call"'
echo
echo "---"
echo

# Test 6: Function calling via Gemini API
echo "Test 6: Gemini API → Function Calling Test"
curl -s -X POST "$BASE_URL/v1beta/models/gpt-5-nano:generateContent" \
  -H "Content-Type: application/json" \
  -d '{"contents":[{"parts":[{"text":"Search for hello"}],"role":"user"}],"tools":[{"functionDeclarations":[{"name":"search","description":"Search","parameters":{"type":"object","properties":{"query":{"type":"string"}},"required":["query"]}}]}],"generationConfig":{"maxOutputTokens":50}}' \
  | jq -r '.candidates[0].content.parts[0].functionCall.name // "No function call"'
echo
echo "---"
echo

echo
echo "======================================"
echo "Diagnostic Tests Complete!"
echo "======================================"
echo
echo "Check the proxy logs for detailed routing information:"
echo "tail -f /tmp/proxy-diagnostic.jsonl | jq '.'"
