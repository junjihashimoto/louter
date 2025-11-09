#!/bin/bash
# Test script for public OpenAI API through Gemini proxy

echo "Testing Gemini Proxy with Public OpenAI API"
echo "============================================"
echo ""

# Check for API key
if [ -z "$OPENAI_API_KEY" ]; then
    echo "❌ Error: OPENAI_API_KEY environment variable not set"
    echo "Set it with: export OPENAI_API_KEY='sk-...'"
    exit 1
fi

echo "✓ API key found"
echo ""

# Start proxy
echo "Starting proxy with OpenAI public API backend..."
cargo run --bin gemini-proxy --quiet -- \
    --backend openai \
    --port 8080 \
    --config openai-public-config.toml \
    > /tmp/proxy-openai-public.log 2>&1 &

PROXY_PID=$!
echo "Proxy started (PID: $PROXY_PID)"

# Wait for proxy to be ready
echo "Waiting for proxy to start..."
for i in {1..10}; do
    if curl -s http://localhost:8080/health > /dev/null 2>&1; then
        echo "✓ Proxy is ready!"
        break
    fi
    sleep 1
done

echo ""
echo "Testing function calling with OpenAI API..."
echo ""

# Test function calling
curl -s 'http://localhost:8080/v1beta/models/gemini-flash:generateContent' \
  -H "Content-Type: application/json" \
  -d '{
    "contents": [{
      "parts": [{"text": "What is 2+2? Use the calculator function."}],
      "role": "user"
    }],
    "tools": [{
      "functionDeclarations": [{
        "name": "calculate",
        "description": "Perform a calculation",
        "parameters": {
          "type": "object",
          "properties": {
            "expression": {"type": "string", "description": "The math expression"}
          },
          "required": ["expression"]
        }
      }]
    }]
  }' | python3 -m json.tool

echo ""
echo ""
echo "Check logs at: /tmp/proxy-openai-public.log"
echo "Stop proxy with: kill $PROXY_PID"
