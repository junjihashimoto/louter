#!/bin/bash
# Script to capture real API responses from all 3 backends
# This creates test data for mock server

set -e

PORT=9000
BASE_URL="http://localhost:$PORT"
TEST_DATA_DIR="test-data"

echo "======================================"
echo "Capturing API Responses"
echo "======================================"
echo

# Ensure test data directories exist
mkdir -p "$TEST_DATA_DIR"/{openai,gemini,local-openai}/{text,function_calling,vision}

echo "📸 Capturing OpenAI API responses..."
echo

# OpenAI - Text
echo "1. OpenAI: Text generation"
REQ='{"model":"gpt-5-nano","messages":[{"role":"user","content":"Say hello"}],"max_completion_tokens":50}'
echo "$REQ" | jq . > "$TEST_DATA_DIR/openai/text/request.json"
curl -s -X POST "$BASE_URL/v1/chat/completions" \
  -H "Content-Type: application/json" \
  -d "$REQ" | jq . > "$TEST_DATA_DIR/openai/text/response.json"
echo "  ✓ Saved to $TEST_DATA_DIR/openai/text/"

# OpenAI - Function Calling
echo "2. OpenAI: Function calling"
REQ='{"model":"gpt-5-nano","messages":[{"role":"user","content":"Search for hello"}],"tools":[{"type":"function","function":{"name":"search","description":"Search function","parameters":{"type":"object","properties":{"query":{"type":"string"}},"required":["query"]}}}],"tool_choice":"auto","max_completion_tokens":50}'
echo "$REQ" | jq . > "$TEST_DATA_DIR/openai/function_calling/request.json"
curl -s -X POST "$BASE_URL/v1/chat/completions" \
  -H "Content-Type: application/json" \
  -d "$REQ" | jq . > "$TEST_DATA_DIR/openai/function_calling/response.json"
echo "  ✓ Saved to $TEST_DATA_DIR/openai/function_calling/"

# OpenAI - Vision
echo "3. OpenAI: Vision"
REQ='{"model":"gpt-5-nano","messages":[{"role":"user","content":[{"type":"text","text":"What is in this image?"},{"type":"image_url","image_url":{"url":"data:image/jpeg;base64,/9j/4AAQSkZJRgABAQEAYABgAAD/2wBDAAEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQH/2wBDAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQH/wAARCAAIAAgDAREAAhEBAxEB/8QAFQABAQAAAAAAAAAAAAAAAAAAAAv/xAAUEAEAAAAAAAAAAAAAAAAAAAAA/8QAFQEBAQAAAAAAAAAAAAAAAAAAAAX/xAAUEQEAAAAAAAAAAAAAAAAAAAAA/9oADAMBAAIRAxEAPwCwAA8A/9k="}}]}],"max_completion_tokens":50}'
echo "$REQ" | jq . > "$TEST_DATA_DIR/openai/vision/request.json"
curl -s -X POST "$BASE_URL/v1/chat/completions" \
  -H "Content-Type: application/json" \
  -d "$REQ" | jq . > "$TEST_DATA_DIR/openai/vision/response.json"
echo "  ✓ Saved to $TEST_DATA_DIR/openai/vision/"

echo
echo "📸 Capturing Gemini API responses..."
echo

# Gemini - Text
echo "4. Gemini: Text generation"
REQ='{"contents":[{"parts":[{"text":"Say hello"}],"role":"user"}],"generationConfig":{"maxOutputTokens":50}}'
echo "$REQ" | jq . > "$TEST_DATA_DIR/gemini/text/request.json"
curl -s -X POST "$BASE_URL/v1beta/models/gemini-2.5-flash:generateContent" \
  -H "Content-Type: application/json" \
  -d "$REQ" | jq . > "$TEST_DATA_DIR/gemini/text/response.json"
echo "  ✓ Saved to $TEST_DATA_DIR/gemini/text/"

# Gemini - Function Calling
echo "5. Gemini: Function calling"
REQ='{"contents":[{"parts":[{"text":"Search for hello"}],"role":"user"}],"tools":[{"functionDeclarations":[{"name":"search","description":"Search function","parameters":{"type":"object","properties":{"query":{"type":"string"}},"required":["query"]}}]}],"generationConfig":{"maxOutputTokens":50}}'
echo "$REQ" | jq . > "$TEST_DATA_DIR/gemini/function_calling/request.json"
curl -s -X POST "$BASE_URL/v1beta/models/gemini-2.5-flash:generateContent" \
  -H "Content-Type: application/json" \
  -d "$REQ" | jq . > "$TEST_DATA_DIR/gemini/function_calling/response.json"
echo "  ✓ Saved to $TEST_DATA_DIR/gemini/function_calling/"

# Gemini - Vision
echo "6. Gemini: Vision"
REQ='{"contents":[{"parts":[{"text":"What is in this image?"},{"inlineData":{"mimeType":"image/jpeg","data":"/9j/4AAQSkZJRgABAQEAYABgAAD/2wBDAAEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQH/2wBDAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQH/wAARCAAIAAgDAREAAhEBAxEB/8QAFQABAQAAAAAAAAAAAAAAAAAAAAv/xAAUEAEAAAAAAAAAAAAAAAAAAAAA/8QAFQEBAQAAAAAAAAAAAAAAAAAAAAX/xAAUEQEAAAAAAAAAAAAAAAAAAAAA/9oADAMBAAIRAxEAPwCwAA8A/9k="}}],"role":"user"}],"generationConfig":{"maxOutputTokens":50}}'
echo "$REQ" | jq . > "$TEST_DATA_DIR/gemini/vision/request.json"
curl -s -X POST "$BASE_URL/v1beta/models/gemini-2.5-flash:generateContent" \
  -H "Content-Type: application/json" \
  -d "$REQ" | jq . > "$TEST_DATA_DIR/gemini/vision/response.json"
echo "  ✓ Saved to $TEST_DATA_DIR/gemini/vision/"

echo
echo "📸 Capturing Local OpenAI (llama-server) responses..."
echo

# Local OpenAI - Text
echo "7. Local OpenAI: Text generation"
REQ='{"model":"gpt-oss:20b","messages":[{"role":"user","content":"Say hello"}],"max_completion_tokens":50}'
echo "$REQ" | jq . > "$TEST_DATA_DIR/local-openai/text/request.json"
curl -s -X POST "$BASE_URL/v1/chat/completions" \
  -H "Content-Type: application/json" \
  -d "$REQ" | jq . > "$TEST_DATA_DIR/local-openai/text/response.json"
echo "  ✓ Saved to $TEST_DATA_DIR/local-openai/text/"

# Local OpenAI - Function Calling
echo "8. Local OpenAI: Function calling"
REQ='{"model":"gpt-oss:20b","messages":[{"role":"user","content":"Search for hello"}],"tools":[{"type":"function","function":{"name":"search","description":"Search function","parameters":{"type":"object","properties":{"query":{"type":"string"}},"required":["query"]}}}],"tool_choice":"auto","max_completion_tokens":50}'
echo "$REQ" | jq . > "$TEST_DATA_DIR/local-openai/function_calling/request.json"
curl -s -X POST "$BASE_URL/v1/chat/completions" \
  -H "Content-Type: application/json" \
  -d "$REQ" | jq . > "$TEST_DATA_DIR/local-openai/function_calling/response.json"
echo "  ✓ Saved to $TEST_DATA_DIR/local-openai/function_calling/"

echo
echo "======================================"
echo "✅ API Response Capture Complete!"
echo "======================================"
echo
echo "Test data saved to: $TEST_DATA_DIR/"
echo
echo "Next steps:"
echo "1. Review captured responses in test-data/"
echo "2. Create mock server using this data"
echo "3. Test conversion logic with mock server"
