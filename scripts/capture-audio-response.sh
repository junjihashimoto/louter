#!/bin/bash
# Capture audio response from Gemini API

API_KEY="${GEMINI_API_KEY}"
MODEL="gemini-2.5-flash"

# Short audio sample (base64-encoded WAV "hello")
# This is a minimal WAV file saying "hello" - just for testing
AUDIO_BASE64="UklGRiQAAABXQVZFZm10IBAAAAABAAEARKwAAIhYAQACABAAZGF0YQAAAAA="

echo "Capturing Gemini audio response..."

# Create test-data directory if it doesn't exist
mkdir -p test-data/gemini/audio

# Request with audio input
curl -X POST "https://generativelanguage.googleapis.com/v1beta/models/${MODEL}:generateContent?key=${API_KEY}" \
  -H "Content-Type: application/json" \
  -d '{
    "contents": [{
      "parts": [
        {
          "text": "What do you hear in this audio?"
        },
        {
          "inlineData": {
            "mimeType": "audio/wav",
            "data": "'"${AUDIO_BASE64}"'"
          }
        }
      ],
      "role": "user"
    }],
    "generationConfig": {
      "maxOutputTokens": 500
    }
  }' | gzip -9 > test-data/gemini/audio/response.json.gz

echo ""
echo "Response saved to test-data/gemini/audio/response.json.gz"

# Display decompressed response for viewing
echo "Response preview:"
zcat test-data/gemini/audio/response.json.gz | jq .
