#!/bin/bash
# Comprehensive Mock Server Test

echo "========================================"
echo "Mock Server Comprehensive Test"
echo "========================================"
echo

# Test 1: Health Check
echo "1. Health Check"
curl -s http://localhost:8888/health
echo -e "\n---\n"

# Test 2: OpenAI Text
echo "2. OpenAI - Text Generation"
curl -s -X POST http://localhost:8888/v1/chat/completions \
  -H "Content-Type: application/json" \
  -d '{"model":"gpt-5-nano","messages":[{"role":"user","content":"Hi"}]}' \
  | jq -r '.choices[0].message.content' | head -3
echo -e "\n---\n"

# Test 3: OpenAI Function Calling
echo "3. OpenAI - Function Calling"
curl -s -X POST http://localhost:8888/v1/chat/completions \
  -H "Content-Type: application/json" \
  -d '{"model":"gpt-5-nano","messages":[{"role":"user","content":"Search"}],"tools":[{"type":"function","function":{"name":"search"}}]}' \
  | jq -r '.choices[0].message.tool_calls[0].function.name // "No function call"'
echo -e "\n---\n"

# Test 4: OpenAI Vision
echo "4. OpenAI - Vision"
curl -s -X POST http://localhost:8888/v1/chat/completions \
  -H "Content-Type: application/json" \
  -d '{"model":"gpt-5-nano","messages":[{"role":"user","content":[{"type":"text","text":"What?"},{"type":"image_url","image_url":{"url":"data:image/jpeg;base64,ABC"}}]}]}' \
  | jq -r '.choices[0].message.content' | head -3
echo -e "\n---\n"

# Test 5: Gemini Text
echo "5. Gemini - Text Generation"
curl -s -X POST http://localhost:8888/v1beta/models/gemini-2.5-flash:generateContent \
  -H "Content-Type: application/json" \
  -d '{"contents":[{"parts":[{"text":"Hi"}],"role":"user"}]}' \
  | jq -r '.candidates[0].content.parts[0].text' | head -3
echo -e "\n---\n"

# Test 6: Gemini Function Calling
echo "6. Gemini - Function Calling"
curl -s -X POST http://localhost:8888/v1beta/models/gemini-2.5-flash:generateContent \
  -H "Content-Type: application/json" \
  -d '{"contents":[{"parts":[{"text":"Search"}],"role":"user"}],"tools":[{"functionDeclarations":[{"name":"search"}]}]}' \
  | jq -r '.candidates[0].content.parts[0].functionCall.name // "No function call"'
echo -e "\n---\n"

# Test 7: Gemini Vision
echo "7. Gemini - Vision"
curl -s -X POST http://localhost:8888/v1beta/models/gemini-2.5-flash:generateContent \
  -H "Content-Type: application/json" \
  -d '{"contents":[{"parts":[{"text":"What?"},{"inlineData":{"mimeType":"image/jpeg","data":"ABC"}}],"role":"user"}]}' \
  | jq -r '.candidates[0].content.parts[0].text' | head -3
echo -e "\n---\n"

# Test 8: Local OpenAI Text
echo "8. Local OpenAI - Text Generation"
curl -s -X POST http://localhost:8888/v1/chat/completions \
  -H "Content-Type: application/json" \
  -d '{"model":"gpt-oss:20b","messages":[{"role":"user","content":"Hi"}]}' \
  | jq -r '.choices[0].message.content' | head -3
echo -e "\n---\n"

# Test 9: Local OpenAI Function Calling
echo "9. Local OpenAI - Function Calling"
curl -s -X POST http://localhost:8888/v1/chat/completions \
  -H "Content-Type: application/json" \
  -d '{"model":"gpt-oss:20b","messages":[{"role":"user","content":"Search"}],"tools":[{"type":"function","function":{"name":"search"}}]}' \
  | jq -r '.choices[0].message.tool_calls[0].function.name // "No function call"'
echo -e "\n---\n"

echo "========================================"
echo "✅ Mock Server Test Complete!"
echo "========================================"
