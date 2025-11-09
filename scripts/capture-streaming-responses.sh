#!/bin/bash
# Capture streaming API responses for test data

set -e

echo "📡 Capturing Streaming API Responses"
echo "===================================="
echo

# Check for API keys
if [ -z "$OPENAI_API_KEY" ]; then
    echo "❌ OPENAI_API_KEY not set"
    exit 1
fi

if [ -z "$GEMINI_API_KEY" ]; then
    echo "❌ GEMINI_API_KEY not set"
    exit 1
fi

# Create directories
mkdir -p test-data/openai/streaming
mkdir -p test-data/gemini/streaming

# OpenAI Streaming Request
echo "1️⃣  Capturing OpenAI streaming response..."
cat > test-data/openai/streaming/request.json << 'EOF'
{
  "model": "gpt-4o-mini",
  "messages": [
    {
      "role": "user",
      "content": "Count from 1 to 3"
    }
  ],
  "stream": true,
  "max_tokens": 50
}
EOF

echo "   Sending request to OpenAI..."
curl -s https://api.openai.com/v1/chat/completions \
  -H "Authorization: Bearer $OPENAI_API_KEY" \
  -H "Content-Type: application/json" \
  -d @test-data/openai/streaming/request.json \
  > test-data/openai/streaming/response.txt

echo "   ✓ Saved to test-data/openai/streaming/response.txt"
echo

# Gemini Streaming Request
echo "2️⃣  Capturing Gemini streaming response..."
cat > test-data/gemini/streaming/request.json << 'EOF'
{
  "contents": [
    {
      "parts": [
        {
          "text": "Count from 1 to 3"
        }
      ],
      "role": "user"
    }
  ]
}
EOF

echo "   Sending request to Gemini..."
curl -s "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash-exp:streamGenerateContent?key=$GEMINI_API_KEY" \
  -H "Content-Type: application/json" \
  -d @test-data/gemini/streaming/request.json \
  > test-data/gemini/streaming/response.txt

echo "   ✓ Saved to test-data/gemini/streaming/response.txt"
echo

echo "✅ All streaming responses captured!"
echo
echo "📁 Files created:"
echo "   test-data/openai/streaming/request.json"
echo "   test-data/openai/streaming/response.txt"
echo "   test-data/gemini/streaming/request.json"
echo "   test-data/gemini/streaming/response.txt"
