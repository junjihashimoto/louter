#!/bin/bash
# Test Gemini streaming with JSON array format (alt=json)

set -e

GEMINI_API_KEY="${GEMINI_API_KEY:-test-key}"
BASE_URL="${GOOGLE_GEMINI_BASE_URL:-https://generativelanguage.googleapis.com}"
MODEL="${MODEL:-gemini-2.5-flash}"

echo "Testing Gemini Streaming - JSON Array Format (alt=json)"
echo "========================================================"
echo "Base URL: $BASE_URL"
echo "Model: $MODEL"
echo ""

curl -N "${BASE_URL}/v1beta/models/${MODEL}:streamGenerateContent?alt=json" \
  -H "x-goog-api-key: $GEMINI_API_KEY" \
  -H 'Content-Type: application/json' \
  -X POST \
  -d @sample_request.json

echo ""
echo "========================================================"
echo "Expected format: JSON array with multiple objects"
echo "[{...}, {...}]"
