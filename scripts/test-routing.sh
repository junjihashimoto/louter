#!/bin/bash

# Test script for content-based routing with capability detection
# This script demonstrates:
# 1. Auto mode backend selection
# 2. Explicit capability-based routing
# 3. Fallback behavior
# 4. Routing metrics

set -e

PROXY_URL="http://localhost:8080"
GEMINI_ENDPOINT="/v1beta/models/gemini-2.0-flash:generateContent"

echo "╔════════════════════════════════════════════════════════════╗"
echo "║         Content-Based Routing Test Suite                  ║"
echo "╚════════════════════════════════════════════════════════════╝"
echo ""

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${BLUE}Prerequisites:${NC}"
echo "1. Start proxy: cargo run -- --backend openai --port 8080 --config examples/llama-cpp-config.toml --verbose"
echo "2. Ensure backend(s) are running (configured in config.toml)"
echo ""
read -p "Press Enter when proxy is ready..."
echo ""

# Test 1: Text-only request (should route to text-capable backend)
echo -e "${GREEN}Test 1: Text-only request${NC}"
echo "Expected: Routes to backend with 'text' capability (priority-based selection)"
echo ""

curl -X POST "${PROXY_URL}${GEMINI_ENDPOINT}" \
  -H "Content-Type: application/json" \
  -d '{
    "contents": [{
      "parts": [{"text": "Hello, can you help me?"}],
      "role": "user"
    }]
  }' 2>/dev/null | jq -C '.'

echo ""
echo -e "${YELLOW}Check proxy logs for:${NC}"
echo "  📋 Request requires capabilities: [text]"
echo "  ✓ Routing selected: 'openai' (mode: EXPLICIT, ...)"
echo ""
read -p "Press Enter to continue..."
echo ""

# Test 2: Vision request (should route to vision-capable backend)
echo -e "${GREEN}Test 2: Vision request (with image)${NC}"
echo "Expected: Routes to backend with 'vision' capability"
echo ""

# Small 1x1 transparent PNG
TEST_IMAGE="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNkYPhfDwAChwGA60e6kgAAAABJRU5ErkJggg=="

curl -X POST "${PROXY_URL}${GEMINI_ENDPOINT}" \
  -H "Content-Type: application/json" \
  -d "{
    \"contents\": [{
      \"parts\": [
        {\"text\": \"What's in this image?\"},
        {\"inlineData\": {\"mimeType\": \"image/png\", \"data\": \"${TEST_IMAGE#data:image/png;base64,}\"}}
      ],
      \"role\": \"user\"
    }]
  }" 2>/dev/null | jq -C '.'

echo ""
echo -e "${YELLOW}Check proxy logs for:${NC}"
echo "  📋 Request requires capabilities: [text, vision]"
echo "  ✓ Routing selected: 'gemini' (mode: EXPLICIT, ...)"
echo ""
read -p "Press Enter to continue..."
echo ""

# Test 3: Function calling request
echo -e "${GREEN}Test 3: Function calling request${NC}"
echo "Expected: Routes to backend with 'function_calling' capability"
echo ""

curl -X POST "${PROXY_URL}${GEMINI_ENDPOINT}" \
  -H "Content-Type: application/json" \
  -d '{
    "contents": [{
      "parts": [{"text": "What is the weather in Tokyo?"}],
      "role": "user"
    }],
    "tools": [{
      "functionDeclarations": [{
        "name": "getWeather",
        "description": "Get current weather for a location",
        "parameters": {
          "type": "object",
          "properties": {
            "location": {
              "type": "string",
              "description": "City name"
            }
          },
          "required": ["location"]
        }
      }]
    }]
  }' 2>/dev/null | jq -C '.'

echo ""
echo -e "${YELLOW}Check proxy logs for:${NC}"
echo "  📋 Request requires capabilities: [text, function_calling]"
echo "  ✓ Routing selected: 'openai' (mode: EXPLICIT, priority: 1)"
echo ""
read -p "Press Enter to continue..."
echo ""

# Test 4: Check Prometheus metrics
echo -e "${GREEN}Test 4: Check routing metrics${NC}"
echo "Fetching Prometheus metrics..."
echo ""

curl -s "${PROXY_URL}/metrics" | grep -A 5 "routing_decisions_total" || echo "Metrics endpoint not available"

echo ""
echo -e "${YELLOW}Expected metrics:${NC}"
echo "  routing_decisions_total{backend=\"openai\",mode=\"explicit\",capabilities=\"text\",success=\"true\"}"
echo "  routing_decisions_total{backend=\"gemini\",mode=\"explicit\",capabilities=\"text,vision\",success=\"true\"}"
echo ""
read -p "Press Enter to continue..."
echo ""

# Test 5: Backend diagnostics
echo -e "${GREEN}Test 5: Check backend diagnostics (startup logs)${NC}"
echo ""
echo -e "${YELLOW}When proxy starts, you should see:${NC}"
echo ""
echo "╔════════════════════════════════════════════════════════════╗"
echo "║          Backend Capability Diagnostics                   ║"
echo "╚════════════════════════════════════════════════════════════╝"
echo ""
echo "Backend: openai (http://localhost:11211)"
echo "  Status: ✓ REACHABLE"
echo "  Mode: AUTO (accepts all request types)     ← or EXPLICIT"
echo "  Detected Capabilities:"
echo "    ✓ text"
echo "    ✓ function_calling"
echo "    ○ vision (not supported)"
echo ""

echo -e "${GREEN}✓ Routing test suite complete!${NC}"
echo ""
echo "Summary:"
echo "  • Text requests route to text-capable backends (priority-based)"
echo "  • Vision requests route to vision-capable backends"
echo "  • Function calls route to function-capable backends"
echo "  • Auto mode backends accept all request types"
echo "  • Metrics track all routing decisions"
echo ""
echo "Next steps:"
echo "  1. Check proxy logs for detailed routing information"
echo "  2. View metrics at ${PROXY_URL}/metrics"
echo "  3. Access UI dashboard at ${PROXY_URL}/ui"
echo ""
