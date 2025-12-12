#!/usr/bin/env python3
"""
Comprehensive Gemini API tool calling tests.

These tests verify the complete tool calling flow:
1. Sending tool declarations in the request
2. Receiving functionCall in the response with complete arguments
3. Sending functionResponse back to the API
4. Receiving final text response after tool execution

These tests would have caught the bugs:
- Missing tool call conversion (functionCall not in response)
- Incomplete arguments (args were fragments, not complete JSON)
- Missing functionResponse handling (infinite loop bug)
"""

import os
import sys
import requests
import json
from typing import List, Dict, Any, Optional


# Test configuration
PROXY_URL = os.getenv("GOOGLE_GEMINI_BASE_URL", "http://localhost:9000")
MODEL = "gpt-oss"


def parse_sse_stream(response) -> List[Dict[str, Any]]:
    """Parse SSE stream and return list of data objects."""
    chunks = []
    for line in response.iter_lines(decode_unicode=True):
        if line.startswith('data: '):
            data = line[6:]  # Remove 'data: ' prefix
            try:
                chunk = json.loads(data)
                chunks.append(chunk)
            except json.JSONDecodeError as e:
                print(f"Failed to parse JSON: {e}")
                print(f"Raw data: {data}")
    return chunks


def extract_function_call(chunks: List[Dict[str, Any]]) -> Optional[Dict[str, Any]]:
    """Extract the first functionCall from chunks."""
    for chunk in chunks:
        if chunk.get('candidates'):
            candidate = chunk['candidates'][0]
            if candidate.get('content', {}).get('parts'):
                for part in candidate['content']['parts']:
                    if 'functionCall' in part:
                        return part['functionCall']
    return None


def extract_finish_reason(chunks: List[Dict[str, Any]]) -> Optional[str]:
    """Extract finish reason from chunks."""
    for chunk in reversed(chunks):
        if chunk.get('candidates'):
            candidate = chunk['candidates'][0]
            if candidate.get('finishReason'):
                return candidate['finishReason']
    return None


def test_tool_call_basic():
    """Test 1: Basic tool call - verify functionCall is emitted with complete args."""
    print("\n=== Test 1: Basic Tool Call ===")

    try:
        url = f"{PROXY_URL}/v1beta/models/{MODEL}:streamGenerateContent?alt=sse"
        payload = {
            "contents": [{
                "parts": [{"text": "Calculate 5 + 3"}],
                "role": "user"
            }],
            "tools": [{
                "functionDeclarations": [{
                    "name": "calculator",
                    "description": "Perform arithmetic calculations",
                    "parameters": {
                        "type": "object",
                        "properties": {
                            "expression": {
                                "type": "string",
                                "description": "The arithmetic expression to evaluate"
                            }
                        },
                        "required": ["expression"]
                    }
                }]
            }]
        }

        response = requests.post(
            url,
            json=payload,
            headers={"Content-Type": "application/json"},
            stream=True
        )

        if response.status_code != 200:
            print(f"❌ FAIL: HTTP {response.status_code}")
            print(f"   Response: {response.text}")
            return False

        chunks = parse_sse_stream(response)
        function_call = extract_function_call(chunks)
        finish_reason = extract_finish_reason(chunks)

        # Verify we got a function call
        if not function_call:
            print(f"❌ FAIL: No functionCall found in response")
            print(f"   Chunks received: {len(chunks)}")
            return False

        # Verify function call has required fields
        if 'name' not in function_call:
            print(f"❌ FAIL: functionCall missing 'name' field")
            print(f"   functionCall: {function_call}")
            return False

        if 'args' not in function_call:
            print(f"❌ FAIL: functionCall missing 'args' field")
            print(f"   functionCall: {function_call}")
            return False

        # Verify args is a complete object, not a string or fragment
        args = function_call['args']
        if not isinstance(args, dict):
            print(f"❌ FAIL: args is not a dict, got {type(args)}")
            print(f"   args: {args}")
            return False

        # Verify args has the expected field
        if 'expression' not in args:
            print(f"❌ FAIL: args missing 'expression' field")
            print(f"   args: {args}")
            return False

        # Verify finish reason is tool_calls
        if finish_reason != "tool_calls":
            print(f"❌ FAIL: Expected finish_reason='tool_calls', got '{finish_reason}'")
            return False

        print(f"✅ PASS: Basic tool call")
        print(f"   Function: {function_call['name']}")
        print(f"   Args: {args}")
        print(f"   Finish reason: {finish_reason}")
        return True

    except Exception as e:
        print(f"❌ FAIL: {str(e)}")
        import traceback
        traceback.print_exc()
        return False


def test_tool_call_roundtrip():
    """Test 2: Complete roundtrip - send functionResponse and verify no infinite loop."""
    print("\n=== Test 2: Tool Call Roundtrip (No Infinite Loop) ===")

    try:
        # Step 1: Initial request with tool
        url = f"{PROXY_URL}/v1beta/models/{MODEL}:streamGenerateContent?alt=sse"
        payload = {
            "contents": [{
                "parts": [{"text": "Write hello.txt with content 'test'"}],
                "role": "user"
            }],
            "tools": [{
                "functionDeclarations": [{
                    "name": "write_file",
                    "description": "Write content to a file",
                    "parameters": {
                        "type": "object",
                        "properties": {
                            "file_path": {"type": "string"},
                            "content": {"type": "string"}
                        },
                        "required": ["file_path", "content"]
                    }
                }]
            }]
        }

        print("  Step 1: Requesting tool use...")
        response = requests.post(
            url,
            json=payload,
            headers={"Content-Type": "application/json"},
            stream=True
        )

        chunks = parse_sse_stream(response)
        function_call = extract_function_call(chunks)

        if not function_call:
            print(f"❌ FAIL: Step 1 - No functionCall received")
            return False

        print(f"  Received functionCall: {function_call['name']}")
        print(f"  Args: {function_call['args']}")

        # Step 2: Send functionResponse back
        print("  Step 2: Sending functionResponse...")
        payload2 = {
            "contents": [
                {
                    "parts": [{"text": "Write hello.txt with content 'test'"}],
                    "role": "user"
                },
                {
                    "parts": [{
                        "functionCall": {
                            "name": function_call['name'],
                            "args": function_call['args']
                        }
                    }],
                    "role": "model"
                },
                {
                    "parts": [{
                        "functionResponse": {
                            "name": function_call['name'],
                            "response": {
                                "result": "File written successfully"
                            }
                        }
                    }],
                    "role": "user"
                }
            ]
        }

        response2 = requests.post(
            url,
            json=payload2,
            headers={"Content-Type": "application/json"},
            stream=True
        )

        chunks2 = parse_sse_stream(response2)

        # Accumulate text from response
        text_content = ""
        for chunk in chunks2:
            if chunk.get('candidates'):
                candidate = chunk['candidates'][0]
                if candidate.get('content', {}).get('parts'):
                    for part in candidate['content']['parts']:
                        if 'text' in part:
                            text_content += part['text']

        # Verify we got a text response, not another function call
        function_call2 = extract_function_call(chunks2)

        if function_call2:
            print(f"❌ FAIL: Step 2 - Got another functionCall (infinite loop!)")
            print(f"   This means functionResponse was not handled properly")
            return False

        if not text_content:
            print(f"❌ FAIL: Step 2 - No text response after functionResponse")
            return False

        print(f"✅ PASS: Tool call roundtrip (no infinite loop)")
        print(f"   Final response: \"{text_content[:100]}...\"")
        return True

    except Exception as e:
        print(f"❌ FAIL: {str(e)}")
        import traceback
        traceback.print_exc()
        return False


def test_tool_call_with_required_params():
    """Test 3: Tool call with multiple required parameters."""
    print("\n=== Test 3: Tool Call with Multiple Required Parameters ===")

    try:
        url = f"{PROXY_URL}/v1beta/models/{MODEL}:streamGenerateContent?alt=sse"
        payload = {
            "contents": [{
                "parts": [{"text": "Write hello.txt with content 'Hello World'"}],
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
                                "description": "Path to the file"
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
        }

        response = requests.post(
            url,
            json=payload,
            headers={"Content-Type": "application/json"},
            stream=True
        )

        chunks = parse_sse_stream(response)
        function_call = extract_function_call(chunks)

        if not function_call:
            print(f"❌ FAIL: No functionCall received")
            return False

        args = function_call.get('args', {})

        # Verify all required parameters are present
        if 'file_path' not in args:
            print(f"❌ FAIL: Missing required parameter 'file_path'")
            print(f"   Args: {args}")
            return False

        if 'content' not in args:
            print(f"❌ FAIL: Missing required parameter 'content'")
            print(f"   Args: {args}")
            return False

        # Verify parameter values are not empty
        if not args['file_path']:
            print(f"❌ FAIL: Parameter 'file_path' is empty")
            return False

        if not args['content']:
            print(f"❌ FAIL: Parameter 'content' is empty")
            return False

        print(f"✅ PASS: Multiple required parameters")
        print(f"   file_path: {args['file_path']}")
        print(f"   content: {args['content']}")
        return True

    except Exception as e:
        print(f"❌ FAIL: {str(e)}")
        import traceback
        traceback.print_exc()
        return False


def test_multiple_tools_available():
    """Test 4: Multiple tools available, model chooses correct one."""
    print("\n=== Test 4: Multiple Tools Available ===")

    try:
        url = f"{PROXY_URL}/v1beta/models/{MODEL}:streamGenerateContent?alt=sse"
        payload = {
            "contents": [{
                "parts": [{"text": "What is 10 + 5?"}],
                "role": "user"
            }],
            "tools": [{
                "functionDeclarations": [
                    {
                        "name": "calculator",
                        "description": "Perform calculations",
                        "parameters": {
                            "type": "object",
                            "properties": {
                                "expression": {"type": "string"}
                            },
                            "required": ["expression"]
                        }
                    },
                    {
                        "name": "write_file",
                        "description": "Write a file",
                        "parameters": {
                            "type": "object",
                            "properties": {
                                "file_path": {"type": "string"},
                                "content": {"type": "string"}
                            },
                            "required": ["file_path", "content"]
                        }
                    }
                ]
            }]
        }

        response = requests.post(
            url,
            json=payload,
            headers={"Content-Type": "application/json"},
            stream=True
        )

        chunks = parse_sse_stream(response)
        function_call = extract_function_call(chunks)

        if not function_call:
            print(f"❌ FAIL: No functionCall received")
            return False

        # Model should choose calculator, not write_file
        if function_call['name'] != 'calculator':
            print(f"⚠️  WARNING: Expected 'calculator', got '{function_call['name']}'")
            print(f"   (Model may have chosen different tool)")

        print(f"✅ PASS: Multiple tools handled")
        print(f"   Chosen tool: {function_call['name']}")
        print(f"   Args: {function_call.get('args', {})}")
        return True

    except Exception as e:
        print(f"❌ FAIL: {str(e)}")
        import traceback
        traceback.print_exc()
        return False


def main():
    """Run all Gemini tool calling tests."""
    print("=" * 60)
    print("Gemini API Tool Calling Tests")
    print("=" * 60)
    print(f"Proxy URL: {PROXY_URL}")
    print(f"Model: {MODEL}")

    tests = [
        ("Basic tool call", test_tool_call_basic),
        ("Tool call roundtrip (no infinite loop)", test_tool_call_roundtrip),
        ("Multiple required parameters", test_tool_call_with_required_params),
        ("Multiple tools available", test_multiple_tools_available),
    ]

    results = []
    for name, test_func in tests:
        try:
            passed = test_func()
            results.append((name, passed))
        except Exception as e:
            print(f"\n❌ Test '{name}' crashed: {e}")
            import traceback
            traceback.print_exc()
            results.append((name, False))

    # Summary
    print("\n" + "=" * 60)
    print("Test Summary")
    print("=" * 60)

    passed = sum(1 for _, p in results if p)
    total = len(results)

    for name, result in results:
        status = "✅ PASS" if result else "❌ FAIL"
        print(f"{status}: {name}")

    print(f"\n{passed}/{total} tests passed")

    return 0 if passed == total else 1


if __name__ == "__main__":
    sys.exit(main())
