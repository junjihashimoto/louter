#!/bin/bash
# Test OpenAI streaming function calling using llama-server
# This demonstrates how function calls are streamed incrementally

API_URL="${API_URL:-http://127.0.0.1:11211/v1/chat/completions}"

echo "Testing OpenAI Streaming Function Calling"
echo "API URL: $API_URL"
echo "=========================================="
echo

curl -N "$API_URL" \
  -H "Content-Type: application/json" \
  -d @request.json

echo
echo "=========================================="
echo "Note: Streaming function calls return incremental chunks:"
echo "  1. First chunk: tool_calls[].function.name (function name)"
echo "  2. Middle chunks: tool_calls[].function.arguments (JSON fragments)"
echo "  3. Last chunk: finish_reason='tool_calls'"
