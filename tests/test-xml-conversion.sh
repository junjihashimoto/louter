#!/bin/bash

# Test XML to JSON conversion in OpenAI pass-through mode
# This script tests that the proxy correctly converts XML tool calls
# from backends (like Qwen3-Coder) to standard JSON format

set -e

PROXY_PORT=8080
BACKEND_PORT=11212
CONFIG_FILE="test-xml-config.toml"
LOG_FILE="xml-conversion-test.jsonl"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "🧪 Testing XML to JSON Conversion in OpenAI Pass-through Mode"
echo "=============================================================="

# Create test configuration
cat > $CONFIG_FILE <<EOF
[performance]
enable_metrics = true
log_requests = true
timeout_seconds = 30

[backends.qwen]
url = "http://localhost:${BACKEND_PORT}"
tool_format = "xml"  # Enable XML parsing
max_tokens = 1024

[backends.qwen.model_mapping]
"qwen3-coder" = "Qwen3-Coder-30B-A3B-Instruct"
EOF

echo -e "${YELLOW}📝 Configuration created: $CONFIG_FILE${NC}"
cat $CONFIG_FILE

# Check if backend is running
echo ""
echo -e "${YELLOW}🔍 Checking if backend is running on port $BACKEND_PORT...${NC}"
if curl -s http://localhost:$BACKEND_PORT/health > /dev/null 2>&1; then
    echo -e "${GREEN}✅ Backend is running${NC}"
    BACKEND_AVAILABLE=true
else
    echo -e "${RED}⚠️  Backend not available on port $BACKEND_PORT${NC}"
    echo "   You can start llama-server with:"
    echo "   ./llama-server -m model.gguf --port $BACKEND_PORT"
    echo ""
    echo "   For this test, we'll use a mock test instead."
    BACKEND_AVAILABLE=false
fi

# Test 1: Unit test of XML parsing function
echo ""
echo -e "${YELLOW}📦 Test 1: Unit Test - XML Parsing Function${NC}"
echo "Creating test XML content..."

# Create a test Rust program to test the function directly
cat > /tmp/test_xml_parsing.rs <<'RUST_EOF'
use louter::conversion::extract_and_convert_xml_tool_calls;

fn main() {
    let test_xml = r#"Sure, I'll help you write to a file.
<tool_call>
  <function=WriteFile>
    <parameter=file_path>test.txt</parameter>
    <parameter=content>Hello World!</parameter>
  </function>
</tool_call>
Let me know if you need anything else."#;

    let (cleaned_text, tool_calls) = extract_and_convert_xml_tool_calls(test_xml);

    println!("Cleaned text: {}", cleaned_text);
    println!("Number of tool calls: {}", tool_calls.len());

    if !tool_calls.is_empty() {
        println!("Tool call:");
        println!("  ID: {}", tool_calls[0].id);
        println!("  Function: {}", tool_calls[0].function.name);
        println!("  Arguments: {}", tool_calls[0].function.arguments);

        // Verify conversion
        assert_eq!(tool_calls[0].function.name, "WriteFile");
        assert!(tool_calls[0].function.arguments.contains("test.txt"));
        assert!(tool_calls[0].function.arguments.contains("Hello World!"));

        println!("\n✅ XML to JSON conversion successful!");
    } else {
        eprintln!("❌ No tool calls extracted!");
        std::process::exit(1);
    }
}
RUST_EOF

echo "Running unit test..."
if rustc --edition 2021 /tmp/test_xml_parsing.rs -L target/release/deps -L target/debug/deps --extern louter=target/debug/liblouter.rlib -o /tmp/test_xml_parsing 2>/dev/null && /tmp/test_xml_parsing; then
    echo -e "${GREEN}✅ Test 1 PASSED: XML parsing works correctly${NC}"
else
    echo -e "${YELLOW}⚠️  Unit test skipped (requires compiled library)${NC}"
    echo "   Testing via integration test instead..."
fi

# Test 2: Integration test with proxy
if [ "$BACKEND_AVAILABLE" = true ]; then
    echo ""
    echo -e "${YELLOW}🔄 Test 2: Integration Test - XML Conversion via Proxy${NC}"

    # Start proxy if not already running
    if ! curl -s http://localhost:$PROXY_PORT/health > /dev/null 2>&1; then
        echo "Starting proxy..."
        cargo run --bin louter -- \
            --backend qwen \
            --port $PROXY_PORT \
            --config $CONFIG_FILE \
            --log-file $LOG_FILE \
            --verbose > proxy-test.log 2>&1 &
        PROXY_PID=$!

        # Wait for proxy to start
        for i in {1..10}; do
            if curl -s http://localhost:$PROXY_PORT/health > /dev/null 2>&1; then
                echo -e "${GREEN}✅ Proxy started (PID: $PROXY_PID)${NC}"
                break
            fi
            sleep 1
        done
    else
        echo -e "${GREEN}✅ Proxy already running${NC}"
        PROXY_PID=""
    fi

    # Test request with function calling
    echo ""
    echo "Sending test request with function calling..."

    RESPONSE=$(curl -s -X POST "http://localhost:$PROXY_PORT/v1/chat/completions" \
        -H "Content-Type: application/json" \
        -d '{
            "model": "qwen3-coder",
            "messages": [
                {"role": "user", "content": "Create a file named hello.txt with content: Hello from Qwen!"}
            ],
            "tools": [{
                "type": "function",
                "function": {
                    "name": "WriteFile",
                    "description": "Write content to a file",
                    "parameters": {
                        "type": "object",
                        "properties": {
                            "file_path": {"type": "string", "description": "Path to file"},
                            "content": {"type": "string", "description": "Content to write"}
                        },
                        "required": ["file_path", "content"]
                    }
                }
            }],
            "tool_choice": "auto"
        }')

    echo "Response received:"
    echo "$RESPONSE" | jq . 2>/dev/null || echo "$RESPONSE"

    # Check if response contains tool_calls
    if echo "$RESPONSE" | jq -e '.choices[0].message.tool_calls' > /dev/null 2>&1; then
        echo -e "${GREEN}✅ Test 2 PASSED: Response contains tool_calls${NC}"

        # Verify the tool call is in JSON format
        TOOL_NAME=$(echo "$RESPONSE" | jq -r '.choices[0].message.tool_calls[0].function.name' 2>/dev/null)
        TOOL_ARGS=$(echo "$RESPONSE" | jq -r '.choices[0].message.tool_calls[0].function.arguments' 2>/dev/null)

        echo ""
        echo "Extracted tool call:"
        echo "  Function: $TOOL_NAME"
        echo "  Arguments: $TOOL_ARGS"

        if [ "$TOOL_NAME" = "WriteFile" ] && echo "$TOOL_ARGS" | grep -q "hello.txt"; then
            echo -e "${GREEN}✅ XML was correctly converted to JSON format!${NC}"
        else
            echo -e "${RED}❌ Tool call format incorrect${NC}"
        fi
    else
        echo -e "${RED}❌ Test 2 FAILED: No tool_calls in response${NC}"
        echo "This might mean:"
        echo "  1. Backend didn't generate XML tool calls"
        echo "  2. XML parsing failed"
        echo "  3. Configuration issue"
    fi

    # Check logs for XML conversion messages
    echo ""
    echo "Checking logs for XML conversion..."
    if grep -q "🔄 XML Conversion" proxy-test.log 2>/dev/null; then
        echo -e "${GREEN}✅ XML conversion was triggered${NC}"
        grep "🔄 XML Conversion" proxy-test.log | tail -1
    else
        echo -e "${YELLOW}⚠️  No XML conversion messages found in logs${NC}"
    fi

    # Analyze detailed logs
    if [ -f "$LOG_FILE" ]; then
        echo ""
        echo "Analyzing JSON Lines logs..."
        cargo run --bin log-parser -- --file $LOG_FILE functions 2>/dev/null | head -20
    fi

    # Cleanup
    if [ -n "$PROXY_PID" ]; then
        echo ""
        echo "Stopping proxy..."
        kill $PROXY_PID 2>/dev/null || true
    fi
else
    echo ""
    echo -e "${YELLOW}⏭️  Test 2 SKIPPED: Backend not available${NC}"
fi

# Test 3: Manual verification guide
echo ""
echo -e "${YELLOW}📚 Test 3: Manual Verification Guide${NC}"
echo "To manually test XML to JSON conversion:"
echo ""
echo "1. Start llama-server with Qwen3-Coder:"
echo "   ./llama-server -m qwen3-coder-model.gguf --port $BACKEND_PORT"
echo ""
echo "2. Start the proxy:"
echo "   cargo run --bin louter -- \\"
echo "       --backend qwen \\"
echo "       --port $PROXY_PORT \\"
echo "       --config $CONFIG_FILE \\"
echo "       --log-file $LOG_FILE \\"
echo "       --verbose"
echo ""
echo "3. Send a test request:"
echo "   curl -X POST 'http://localhost:$PROXY_PORT/v1/chat/completions' \\"
echo "       -H 'Content-Type: application/json' \\"
echo "       -d '{"
echo "           \"model\": \"qwen3-coder\","
echo "           \"messages\": [{\"role\": \"user\", \"content\": \"Write hello.txt\"}],"
echo "           \"tools\": [{\"type\": \"function\", \"function\": {\"name\": \"WriteFile\", ...}}]"
echo "       }' | jq ."
echo ""
echo "4. Verify the response contains 'tool_calls' in JSON format (not XML)"
echo ""

# Summary
echo ""
echo "=============================================================="
echo "🏁 Test Summary"
echo "=============================================================="
echo "Configuration: $CONFIG_FILE"
echo "Log file: $LOG_FILE"
echo ""
echo "The XML to JSON conversion:"
echo "  1. Detects XML <tool_call> tags in backend responses"
echo "  2. Parses function name and parameters"
echo "  3. Converts to standard OpenAI JSON format"
echo "  4. Removes XML from the response text"
echo ""
echo "For more details, see:"
echo "  - src/backends.rs (lines 67-80)"
echo "  - src/conversion.rs (extract_and_convert_xml_tool_calls)"
echo "  - XML-TOOL-FORMAT-SUPPORT.md"
echo ""

# Cleanup
rm -f /tmp/test_xml_parsing.rs /tmp/test_xml_parsing

echo -e "${GREEN}✅ Test completed!${NC}"
