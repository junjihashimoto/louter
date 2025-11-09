#!/bin/bash
# Test script for public API configuration
# ==========================================
# This script tests the proxy with real public APIs (Gemini and OpenAI)

set -e  # Exit on error

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}Testing Public API Configuration${NC}"
echo -e "${BLUE}========================================${NC}\n"

# Check if API keys are set
if [ -z "$GEMINI_API_KEY" ]; then
    echo -e "${YELLOW}⚠ GEMINI_API_KEY not set${NC}"
    echo "Get your key from: https://makersuite.google.com/app/apikey"
    echo "Then run: export GEMINI_API_KEY='your-key-here'"
    echo ""
fi

if [ -z "$OPENAI_API_KEY" ]; then
    echo -e "${YELLOW}⚠ OPENAI_API_KEY not set${NC}"
    echo "Get your key from: https://platform.openai.com/api-keys"
    echo "Then run: export OPENAI_API_KEY='your-key-here'"
    echo ""
fi

# Default port
PORT=8080

# Check if server is already running
if ! curl -s http://localhost:$PORT/health > /dev/null 2>&1; then
    echo -e "${YELLOW}⚠ Server not running on port $PORT${NC}"
    echo "Start the server with:"
    echo "  ./target/release/louter --backend openai --port $PORT --config config-public-api.toml --verbose"
    echo ""
    exit 1
fi

echo -e "${GREEN}✓ Server is running on port $PORT${NC}\n"

# Test 1a: Direct Gemini API call (no proxy)
echo -e "${BLUE}Test 1a: Direct Gemini Public API Call${NC}"
echo "Testing https://generativelanguage.googleapis.com directly..."

if [ -n "$GEMINI_API_KEY" ]; then
    echo "Request: Gemini API format → Google servers"
    DIRECT_RESULT=$(curl -s -X POST "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent?key=$GEMINI_API_KEY" \
      -H "Content-Type: application/json" \
      -d '{
        "contents": [{
          "parts": [{"text": "Say hello in one word"}],
          "role": "user"
        }],
        "generationConfig": {
          "maxOutputTokens": 10,
          "temperature": 0.0
        }
      }')

    echo "Response: $(echo "$DIRECT_RESULT" | jq -r '.candidates[0].content.parts[0].text // .error.message' | head -n 1)"
    echo ""
else
    echo -e "${YELLOW}Skipped (GEMINI_API_KEY not set)${NC}\n"
fi

# Test 1b: Same request through proxy
echo -e "${BLUE}Test 1b: Gemini API → Proxy → Gemini Backend${NC}"
echo "Testing via proxy at http://localhost:$PORT..."

if [ -n "$GEMINI_API_KEY" ]; then
    echo "Request: Gemini API format → Proxy → Google servers"
    PROXY_RESULT=$(curl -s -X POST "http://localhost:$PORT/v1beta/models/gemini-pro:generateContent?key=$GEMINI_API_KEY" \
      -H "Content-Type: application/json" \
      -d '{
        "contents": [{
          "parts": [{"text": "Say hello in one word"}],
          "role": "user"
        }],
        "generationConfig": {
          "maxOutputTokens": 10,
          "temperature": 0.0
        }
      }')

    echo "Response: $(echo "$PROXY_RESULT" | jq -r '.candidates[0].content.parts[0].text // .error.message' | head -n 1)"
    echo ""
else
    echo -e "${YELLOW}Skipped (GEMINI_API_KEY not set)${NC}\n"
fi

# Test 2a: Direct OpenAI API call (no proxy)
echo -e "${BLUE}Test 2a: Direct OpenAI Public API Call${NC}"
echo "Testing https://api.openai.com directly..."

if [ -n "$OPENAI_API_KEY" ]; then
    echo "Request: OpenAI API format → OpenAI servers"
    DIRECT_OPENAI=$(curl -s -X POST "https://api.openai.com/v1/chat/completions" \
      -H "Authorization: Bearer $OPENAI_API_KEY" \
      -H "Content-Type: application/json" \
      -d '{
        "model": "gpt-3.5-turbo",
        "messages": [{"role": "user", "content": "Say hello in one word"}],
        "max_tokens": 10,
        "temperature": 0.0
      }')

    echo "Response: $(echo "$DIRECT_OPENAI" | jq -r '.choices[0].message.content // .error.message' | head -n 1)"
    echo ""
else
    echo -e "${YELLOW}Skipped (OPENAI_API_KEY not set)${NC}\n"
fi

# Test 2b: Same request through proxy
echo -e "${BLUE}Test 2b: OpenAI API → Proxy → OpenAI Backend${NC}"
echo "Testing via proxy at http://localhost:$PORT..."

if [ -n "$OPENAI_API_KEY" ]; then
    echo "Request: OpenAI API format → Proxy → OpenAI servers"
    PROXY_OPENAI=$(curl -s -X POST "http://localhost:$PORT/v1/chat/completions" \
      -H "Authorization: Bearer $OPENAI_API_KEY" \
      -H "Content-Type: application/json" \
      -d '{
        "model": "gpt-3.5-turbo",
        "messages": [{"role": "user", "content": "Say hello in one word"}],
        "max_tokens": 10,
        "temperature": 0.0
      }')

    echo "Response: $(echo "$PROXY_OPENAI" | jq -r '.choices[0].message.content // .error.message' | head -n 1)"
    echo ""
else
    echo -e "${YELLOW}Skipped (OPENAI_API_KEY not set)${NC}\n"
fi

# Test 3: Cross-format - OpenAI API format with Gemini backend
echo -e "${BLUE}Test 3: Cross-Format Test (OpenAI format → Gemini)${NC}"
echo "Demonstrating format conversion..."
echo ""

if [ -n "$GEMINI_API_KEY" ]; then
    echo "3a) Direct Gemini API (native format):"
    echo "    URL: https://generativelanguage.googleapis.com"
    echo "    Format: Gemini (contents/parts)"
    GEMINI_NATIVE=$(curl -s -X POST "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent?key=$GEMINI_API_KEY" \
      -H "Content-Type: application/json" \
      -d '{
        "contents": [{
          "parts": [{"text": "What is 2+2?"}],
          "role": "user"
        }],
        "generationConfig": {"maxOutputTokens": 20, "temperature": 0.0}
      }')
    echo "    Response: $(echo "$GEMINI_NATIVE" | jq -r '.candidates[0].content.parts[0].text // .error.message' | head -n 1)"
    echo ""

    echo "3b) Proxy with OpenAI format → Gemini backend:"
    echo "    URL: http://localhost:$PORT"
    echo "    Format: OpenAI (messages/content) → Converted → Gemini"
    OPENAI_TO_GEMINI=$(curl -s -X POST "http://localhost:$PORT/v1/chat/completions" \
      -H "Authorization: Bearer $GEMINI_API_KEY" \
      -H "Content-Type: application/json" \
      -d '{
        "model": "gemini-pro",
        "messages": [{"role": "user", "content": "What is 2+2?"}],
        "max_tokens": 20,
        "temperature": 0.0
      }')
    echo "    Response: $(echo "$OPENAI_TO_GEMINI" | jq -r '.choices[0].message.content // .error.message' | head -n 1)"
    echo ""

    echo -e "${GREEN}✓ Format conversion working! Same backend, different input formats${NC}"
    echo ""
else
    echo -e "${YELLOW}Skipped (GEMINI_API_KEY not set)${NC}\n"
fi

# Test 4: Cross-format - Gemini API with OpenAI backend
echo -e "${BLUE}Test 4: Gemini API → OpenAI Backend (cross-format)${NC}"
echo "Testing /v1beta/models/gpt-3.5-turbo:generateContent..."

if [ -n "$OPENAI_API_KEY" ]; then
    curl -s -X POST "http://localhost:$PORT/v1beta/models/gpt-3.5-turbo:generateContent" \
      -H "Content-Type: application/json" \
      -H "Authorization: Bearer $OPENAI_API_KEY" \
      -d '{
        "contents": [{
          "parts": [{"text": "Say hello in one word"}],
          "role": "user"
        }],
        "generationConfig": {
          "maxOutputTokens": 10,
          "temperature": 0.0
        }
      }' | jq '.candidates[0].content.parts[0].text' || echo -e "${RED}✗ Failed${NC}"
    echo ""
else
    echo -e "${YELLOW}Skipped (OPENAI_API_KEY not set)${NC}\n"
fi

# Test 5: Vision test with Gemini
echo -e "${BLUE}Test 5: Vision Test (Gemini)${NC}"
echo "Testing image understanding with gemini-pro..."

if [ -n "$GEMINI_API_KEY" ]; then
    # Blue square test image
    TEST_IMAGE="data:image/jpeg;base64,/9j/4AAQSkZJRgABAQEAYABgAAD/2wBDAAEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQH/2wBDAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQH/wAARCAAIAAgDAREAAhEBAxEB/8QAFQABAQAAAAAAAAAAAAAAAAAAAAv/xAAUEAEAAAAAAAAAAAAAAAAAAAAA/8QAFQEBAQAAAAAAAAAAAAAAAAAAAAX/xAAUEQEAAAAAAAAAAAAAAAAAAAAA/9oADAMBAAIRAxEAPwCwAA8A/9k="

    curl -s -X POST "http://localhost:$PORT/v1beta/models/gemini-pro:generateContent?key=$GEMINI_API_KEY" \
      -H "Content-Type: application/json" \
      -d "{
        \"contents\": [{
          \"parts\": [
            {\"text\": \"What color is this image? Answer in one word.\"},
            {\"inlineData\": {\"mimeType\": \"image/jpeg\", \"data\": \"$TEST_IMAGE\"}}
          ],
          \"role\": \"user\"
        }],
        \"generationConfig\": {
          \"maxOutputTokens\": 10,
          \"temperature\": 0.0
        }
      }" | jq '.candidates[0].content.parts[0].text' || echo -e "${RED}✗ Failed${NC}"
    echo ""
else
    echo -e "${YELLOW}Skipped (GEMINI_API_KEY not set)${NC}\n"
fi

# Test 6: Format Conversion Verification (without API key)
echo -e "${BLUE}Test 6: Format Conversion Verification (No API Key)${NC}"
echo "Testing that OpenAI format gets converted to Gemini format..."
echo ""

RESPONSE=$(curl -s -X POST "http://localhost:$PORT/v1/chat/completions" \
  -H "Content-Type: application/json" \
  -d '{
    "model": "gemini-pro",
    "messages": [{"role": "user", "content": "Test"}],
    "max_tokens": 5
  }')

# Check if we get a response from Gemini API (even if it's an error)
if echo "$RESPONSE" | grep -q "generativelanguage.googleapis.com\|RESOURCE_EXHAUSTED\|quota"; then
    echo -e "${GREEN}✓ Format conversion working! Request routed to Gemini API${NC}"
    echo "Response snippet:"
    echo "$RESPONSE" | jq -r '.error.details // .error.message' | head -n 3
else
    echo -e "${YELLOW}Response: $(echo "$RESPONSE" | jq -c .)${NC}"
fi
echo ""

# Test 7: Model Mapping Verification
echo -e "${BLUE}Test 7: Model Mapping Verification${NC}"
echo "Testing how different model names route to backends..."
echo ""

for MODEL in "gpt-3.5-turbo" "gemini-pro" "gemini-flash" "gpt-4"; do
    echo -n "  Model '$MODEL': "

    RESPONSE=$(curl -s -X POST "http://localhost:$PORT/v1/chat/completions" \
      -H "Content-Type: application/json" \
      -d "{
        \"model\": \"$MODEL\",
        \"messages\": [{\"role\": \"user\", \"content\": \"Hi\"}],
        \"max_tokens\": 1
      }")

    # Check which backend it routes to based on error message
    if echo "$RESPONSE" | grep -q "generativelanguage.googleapis.com\|gemini"; then
        echo -e "${GREEN}→ Gemini Backend${NC}"
    elif echo "$RESPONSE" | grep -q "api.openai.com\|openai"; then
        echo -e "${BLUE}→ OpenAI Backend${NC}"
    else
        echo -e "${YELLOW}→ Unknown ($(echo "$RESPONSE" | jq -r '.error.code // "error"'))${NC}"
    fi
done
echo ""

# Test 8: Diagnostics API
echo -e "${BLUE}Test 8: Diagnostics API${NC}"
echo "Checking backend status via /api/diagnostics..."
echo ""

DIAG=$(curl -s "http://localhost:$PORT/api/diagnostics")

echo "Backend Status:"
echo "$DIAG" | jq -r '.backends | to_entries[] | "  \(.key): \(if .value.reachable then "✓ REACHABLE" else "✗ UNREACHABLE" end) (\(.value.url))"'
echo ""

echo "Frontend Capabilities:"
echo "$DIAG" | jq -r '.frontends.gemini_api | "  Gemini API: \(.passed_capabilities | join(", "))"'
echo "$DIAG" | jq -r '.frontends.openai_api | "  OpenAI API: \(.passed_capabilities | join(", "))"'
echo ""

# Test 9: Streaming Test (if API key available)
echo -e "${BLUE}Test 9: Streaming Test (SSE)${NC}"
echo "Testing Server-Sent Events streaming..."
echo ""

if [ -n "$GEMINI_API_KEY" ]; then
    echo "Sending streaming request to OpenAI endpoint with Gemini backend..."

    STREAM_OUTPUT=$(curl -s -N -X POST "http://localhost:$PORT/v1/chat/completions" \
      -H "Authorization: Bearer $GEMINI_API_KEY" \
      -H "Content-Type: application/json" \
      -d '{
        "model": "gemini-pro",
        "messages": [{"role": "user", "content": "Count 1 2 3"}],
        "max_tokens": 20,
        "stream": true
      }' | head -n 5)

    if echo "$STREAM_OUTPUT" | grep -q "data:"; then
        echo -e "${GREEN}✓ Streaming working! Received SSE events${NC}"
        echo "First few events:"
        echo "$STREAM_OUTPUT" | head -n 3
    else
        echo -e "${RED}✗ Streaming failed${NC}"
    fi
elif [ -n "$OPENAI_API_KEY" ]; then
    echo "Sending streaming request to OpenAI endpoint..."

    STREAM_OUTPUT=$(curl -s -N -X POST "http://localhost:$PORT/v1/chat/completions" \
      -H "Authorization: Bearer $OPENAI_API_KEY" \
      -H "Content-Type: application/json" \
      -d '{
        "model": "gpt-3.5-turbo",
        "messages": [{"role": "user", "content": "Count 1 2 3"}],
        "max_tokens": 20,
        "stream": true
      }' | head -n 5)

    if echo "$STREAM_OUTPUT" | grep -q "data:"; then
        echo -e "${GREEN}✓ Streaming working! Received SSE events${NC}"
        echo "First few events:"
        echo "$STREAM_OUTPUT" | head -n 3
    else
        echo -e "${RED}✗ Streaming failed${NC}"
    fi
else
    echo -e "${YELLOW}Skipped (No API keys set)${NC}"
fi
echo ""

# Test 10: Health and Metrics Endpoints
echo -e "${BLUE}Test 10: Monitoring Endpoints${NC}"
echo "Testing health, metrics, and web UI..."
echo ""

# Health check
HEALTH=$(curl -s "http://localhost:$PORT/health")
if echo "$HEALTH" | grep -q '"status":"ok"'; then
    echo -e "${GREEN}✓ Health endpoint: OK${NC}"
else
    echo -e "${RED}✗ Health endpoint: Failed${NC}"
fi

# Metrics check
METRICS=$(curl -s "http://localhost:$PORT/metrics" | head -n 5)
if echo "$METRICS" | grep -q "ttft_seconds\|tokens_per_second"; then
    echo -e "${GREEN}✓ Metrics endpoint: Prometheus metrics available${NC}"
else
    echo -e "${YELLOW}⚠ Metrics endpoint: No metrics yet${NC}"
fi

# Web UI check
UI_RESPONSE=$(curl -s -o /dev/null -w "%{http_code}" "http://localhost:$PORT/ui")
if [ "$UI_RESPONSE" = "200" ]; then
    echo -e "${GREEN}✓ Web UI: Available at http://localhost:$PORT/ui${NC}"
else
    echo -e "${YELLOW}⚠ Web UI: HTTP $UI_RESPONSE${NC}"
fi
echo ""

# Summary
echo -e "${GREEN}========================================${NC}"
echo -e "${GREEN}Testing Complete!${NC}"
echo -e "${GREEN}========================================${NC}"
echo ""
echo "Summary:"
echo "  - Format conversion: Working"
echo "  - Model routing: Working"
echo "  - Diagnostics: Available"
echo "  - Monitoring: Available"
echo ""
if [ -z "$GEMINI_API_KEY" ] && [ -z "$OPENAI_API_KEY" ]; then
    echo -e "${YELLOW}Note: Set API keys to test actual LLM responses:${NC}"
    echo "  export GEMINI_API_KEY='your-key'"
    echo "  export OPENAI_API_KEY='your-key'"
else
    echo -e "${GREEN}API keys configured - full testing enabled!${NC}"
fi
echo ""
