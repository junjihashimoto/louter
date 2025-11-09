#!/bin/bash

# Test with increased context window and multiple tools like gemini-cli

# Start proxy in background
cargo run --bin gemini-proxy -- --backend openai --port 9999 --config llama-cpp-config.toml --log-file test-64k.jsonl 2>/dev/null &
PROXY_PID=$!

# Wait for proxy to start
sleep 3

# Send a test request with multiple tools like gemini-cli uses
curl -s -X POST 'http://localhost:9999/v1beta/models/gemma3-4b:generateContent' \
  -H "Content-Type: application/json" \
  -d '{
    "contents": [{
      "parts": [{"text": "Write hello.txt file with content: Hello World"}],
      "role": "user"
    }],
    "systemInstruction": {
      "parts": [{
        "text": "You are a helpful CLI assistant. When the user asks you to write a file, use the write_file function. When they ask to run a command, use run_shell_command."
      }]
    },
    "tools": [{
      "functionDeclarations": [
        {
          "name": "write_file",
          "description": "Write content to a file at the specified path",
          "parameters": {
            "type": "object",
            "properties": {
              "file_path": {
                "type": "string",
                "description": "The absolute path where the file should be written"
              },
              "content": {
                "type": "string",
                "description": "The content to write to the file"
              }
            },
            "required": ["file_path", "content"]
          }
        },
        {
          "name": "run_shell_command",
          "description": "Execute a shell command",
          "parameters": {
            "type": "object",
            "properties": {
              "command": {
                "type": "string",
                "description": "The shell command to execute"
              }
            },
            "required": ["command"]
          }
        },
        {
          "name": "read_file",
          "description": "Read the contents of a file",
          "parameters": {
            "type": "object",
            "properties": {
              "file_path": {
                "type": "string",
                "description": "The absolute path to the file to read"
              }
            },
            "required": ["file_path"]
          }
        }
      ]
    }]
  }' | python3 -m json.tool

# Kill proxy
kill $PROXY_PID 2>/dev/null
wait $PROXY_PID 2>/dev/null

echo ""
echo "=== Checking test-64k.jsonl for conversion details ==="
cat test-64k.jsonl | python3 -m json.tool | grep -A 20 "backend_response" | head -30
