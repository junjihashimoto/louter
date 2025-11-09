#!/bin/bash

# Test how gemini-cli style tools are converted

# Start proxy in background
cargo run --bin gemini-proxy -- --backend openai --port 9999 --config llama-cpp-config.toml --log-file test-gemini-cli-tools.jsonl 2>/dev/null &
PROXY_PID=$!

# Wait for proxy to start
sleep 3

# Send a test request with a tool similar to write_file
curl -X POST 'http://localhost:9999/v1beta/models/gemma3-4b:generateContent' \
  -H "Content-Type: application/json" \
  -d '{
    "contents": [{
      "parts": [{"text": "Write hello.txt with content: hello world"}],
      "role": "user"
    }],
    "tools": [{
      "functionDeclarations": [{
        "name": "write_file",
        "description": "Write content to a file",
        "parameters": {
          "type": "object",
          "properties": {
            "file_path": {
              "type": "string",
              "description": "Absolute path to the file"
            },
            "content": {
              "type": "string",
              "description": "Content to write"
            }
          },
          "required": ["file_path", "content"]
        }
      }]
    }]
  }'

# Kill proxy
kill $PROXY_PID 2>/dev/null

echo ""
echo "=== Check test-gemini-cli-tools.jsonl for results ==="
