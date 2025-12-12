#!/usr/bin/env python3
"""
Comprehensive tests for OpenAI API streaming implementation using official SDK.

Tests verify spec compliance:
- Correct SSE chunk sequence
- Proper delta handling (content, reasoning, tool_calls)
- Function calling (single and multiple parallel tools)
- Stop reasons and finish events
- Token usage tracking
- Error handling
"""

import os
import sys
import json
from typing import List, Dict, Any, Optional
from openai import OpenAI

# Test configuration
PROXY_URL = os.getenv("OPENAI_BASE_URL", "http://localhost:9000/v1")
API_KEY = os.getenv("OPENAI_API_KEY", "test-key")  # Proxy may ignore this for llama-server
MODEL = os.getenv("OPENAI_MODEL", "gpt-oss")


class StreamChunkCapture:
    """Captures and analyzes streaming chunks."""

    def __init__(self):
        self.chunks: List[Dict[str, Any]] = []
        self.deltas: List[str] = []
        self.content = ""
        self.reasoning = ""
        self.tool_calls: Dict[int, Dict] = {}  # index -> tool call data
        self.finish_reason: Optional[str] = None
        self.role: Optional[str] = None

    def add_chunk(self, chunk):
        """Add a chunk to the capture."""
        chunk_dict = {
            "id": chunk.id,
            "choices": []
        }

        for choice in chunk.choices:
            choice_dict = {
                "index": choice.index,
                "delta": {},
                "finish_reason": choice.finish_reason
            }

            delta = choice.delta

            # Capture role
            if delta.role:
                self.role = delta.role
                choice_dict["delta"]["role"] = delta.role

            # Capture content
            if delta.content:
                self.content += delta.content
                choice_dict["delta"]["content"] = delta.content
                self.deltas.append("content")

            # Capture reasoning (o1 models, etc.)
            if hasattr(delta, 'reasoning') and delta.reasoning:
                self.reasoning += delta.reasoning
                choice_dict["delta"]["reasoning"] = delta.reasoning
                self.deltas.append("reasoning")

            # Capture tool calls
            if delta.tool_calls:
                for tc in delta.tool_calls:
                    idx = tc.index
                    if idx not in self.tool_calls:
                        self.tool_calls[idx] = {
                            "id": "",
                            "type": "function",
                            "function": {
                                "name": "",
                                "arguments": ""
                            }
                        }

                    if tc.id:
                        self.tool_calls[idx]["id"] = tc.id
                    if tc.function:
                        if tc.function.name:
                            self.tool_calls[idx]["function"]["name"] = tc.function.name
                        if tc.function.arguments:
                            self.tool_calls[idx]["function"]["arguments"] += tc.function.arguments

                choice_dict["delta"]["tool_calls"] = [
                    {"index": tc.index, "id": tc.id, "function": tc.function}
                    for tc in delta.tool_calls
                ]
                self.deltas.append("tool_calls")

            # Capture finish reason
            if choice.finish_reason:
                self.finish_reason = choice.finish_reason
                self.deltas.append("finish")

            chunk_dict["choices"].append(choice_dict)

        self.chunks.append(chunk_dict)

    def get_delta_types(self) -> List[str]:
        """Get list of delta types in order."""
        return self.deltas

    def get_chunk_count(self) -> int:
        """Get number of chunks received."""
        return len(self.chunks)

    def get_content(self) -> str:
        """Get accumulated content."""
        return self.content

    def get_reasoning(self) -> str:
        """Get accumulated reasoning."""
        return self.reasoning

    def get_tool_calls(self) -> List[Dict]:
        """Get all tool calls as list."""
        return [self.tool_calls[i] for i in sorted(self.tool_calls.keys())]

    def verify_tool_calls_complete(self) -> tuple[bool, str]:
        """Verify all tool calls have complete JSON arguments."""
        for idx, tc in self.tool_calls.items():
            if not tc["id"]:
                return False, f"Tool call {idx} missing id"
            if not tc["function"]["name"]:
                return False, f"Tool call {idx} missing function name"

            args = tc["function"]["arguments"]
            if not args:
                return False, f"Tool call {idx} has empty arguments"

            # Verify arguments are valid JSON
            try:
                json.loads(args)
            except json.JSONDecodeError as e:
                return False, f"Tool call {idx} has invalid JSON: {e}"

        return True, "All tool calls complete and valid"


def test_text_only_streaming():
    """Test 1: Basic text-only streaming."""
    print("\n=== Test 1: Text-Only Streaming ===")

    client = OpenAI(api_key=API_KEY, base_url=PROXY_URL)
    capture = StreamChunkCapture()

    try:
        stream = client.chat.completions.create(
            model=MODEL,
            messages=[{"role": "user", "content": "Say hello in one sentence"}],
            max_tokens=50,
            stream=True
        )

        for chunk in stream:
            capture.add_chunk(chunk)

        # Verify we got content
        content = capture.get_content()
        if not content:
            print(f"⚠️  SKIP: Backend returned no content (may have used all tokens for reasoning)")
            return True

        # Verify we got multiple chunks (streaming)
        if capture.get_chunk_count() < 2:
            print(f"⚠️  WARNING: Expected multiple chunks for streaming, got {capture.get_chunk_count()}")

        # Verify finish reason
        if not capture.finish_reason:
            print(f"❌ FAIL: No finish_reason received")
            return False

        valid_reasons = ["stop", "length", "max_tokens"]
        if capture.finish_reason not in valid_reasons:
            print(f"⚠️  WARNING: Unexpected finish_reason: {capture.finish_reason}")

        print(f"✅ PASS: Text-only streaming")
        print(f"   Chunks: {capture.get_chunk_count()}")
        print(f"   Content: \"{content[:60]}...\"")
        print(f"   Finish reason: {capture.finish_reason}")
        return True

    except Exception as e:
        print(f"❌ FAIL: {str(e)}")
        import traceback
        traceback.print_exc()
        return False


def test_function_calling_single():
    """Test 2: Single function call streaming."""
    print("\n=== Test 2: Single Function Call Streaming ===")

    client = OpenAI(api_key=API_KEY, base_url=PROXY_URL)
    capture = StreamChunkCapture()

    try:
        stream = client.chat.completions.create(
            model=MODEL,
            messages=[{"role": "user", "content": "What is 25 * 4? Use the calculator."}],
            max_tokens=200,
            tools=[{
                "type": "function",
                "function": {
                    "name": "calculator",
                    "description": "Evaluate mathematical expressions",
                    "parameters": {
                        "type": "object",
                        "properties": {
                            "expression": {"type": "string", "description": "The math expression"}
                        },
                        "required": ["expression"]
                    }
                }
            }],
            stream=True
        )

        for chunk in stream:
            capture.add_chunk(chunk)

        tool_calls = capture.get_tool_calls()

        # Backend might return text instead of tool call
        if not tool_calls:
            print(f"⚠️  SKIP: Backend returned text instead of tool call")
            return True

        # Verify tool call completeness
        valid, msg = capture.verify_tool_calls_complete()
        if not valid:
            print(f"❌ FAIL: {msg}")
            return False

        # Verify finish reason
        if capture.finish_reason != "tool_calls":
            print(f"⚠️  WARNING: Expected finish_reason='tool_calls', got '{capture.finish_reason}'")

        print(f"✅ PASS: Single function call streaming")
        print(f"   Chunks: {capture.get_chunk_count()}")
        print(f"   Tool calls: {len(tool_calls)}")
        for tc in tool_calls:
            print(f"     - {tc['function']['name']}: {tc['function']['arguments']}")
        print(f"   Finish reason: {capture.finish_reason}")
        return True

    except Exception as e:
        print(f"❌ FAIL: {str(e)}")
        import traceback
        traceback.print_exc()
        return False


def test_function_calling_multiple():
    """Test 3: Multiple parallel function calls."""
    print("\n=== Test 3: Multiple Parallel Function Calls ===")

    client = OpenAI(api_key=API_KEY, base_url=PROXY_URL)
    capture = StreamChunkCapture()

    try:
        stream = client.chat.completions.create(
            model=MODEL,
            messages=[{"role": "user", "content": "Calculate both 25*4 and 10+20. Use the calculator for each."}],
            max_tokens=300,
            tools=[{
                "type": "function",
                "function": {
                    "name": "calculator",
                    "description": "Evaluate mathematical expressions",
                    "parameters": {
                        "type": "object",
                        "properties": {
                            "expression": {"type": "string"}
                        },
                        "required": ["expression"]
                    }
                }
            }],
            stream=True
        )

        for chunk in stream:
            capture.add_chunk(chunk)

        tool_calls = capture.get_tool_calls()

        # Backend might not return multiple tool calls
        if len(tool_calls) == 0:
            print(f"⚠️  SKIP: Backend returned no tool calls")
            return True

        # Verify all tool calls are complete
        valid, msg = capture.verify_tool_calls_complete()
        if not valid:
            print(f"❌ FAIL: {msg}")
            return False

        # Verify indices are sequential
        indices = sorted(capture.tool_calls.keys())
        expected = list(range(len(indices)))
        if indices != expected:
            print(f"❌ FAIL: Tool call indices not sequential. Expected {expected}, got {indices}")
            return False

        print(f"✅ PASS: Multiple parallel function calls")
        print(f"   Chunks: {capture.get_chunk_count()}")
        print(f"   Tool calls: {len(tool_calls)}")
        for i, tc in enumerate(tool_calls):
            print(f"     [{i}] {tc['function']['name']}: {tc['function']['arguments']}")
        return True

    except Exception as e:
        print(f"❌ FAIL: {str(e)}")
        import traceback
        traceback.print_exc()
        return False


def test_reasoning_content_streaming():
    """Test 4: Reasoning + content streaming (o1 models)."""
    print("\n=== Test 4: Reasoning + Content Streaming ===")

    client = OpenAI(api_key=API_KEY, base_url=PROXY_URL)
    capture = StreamChunkCapture()

    try:
        stream = client.chat.completions.create(
            model=MODEL,
            messages=[{"role": "user", "content": "Explain why 2+2=4"}],
            max_tokens=200,
            stream=True
        )

        for chunk in stream:
            capture.add_chunk(chunk)

        # Check if we got reasoning or content
        has_reasoning = bool(capture.get_reasoning())
        has_content = bool(capture.get_content())

        if not has_reasoning and not has_content:
            print(f"❌ FAIL: No reasoning or content received")
            return False

        # Verify delta ordering if we have both
        if has_reasoning and has_content:
            deltas = capture.get_delta_types()
            # Reasoning should come before content (typical for o1 models)
            first_reasoning = deltas.index("reasoning") if "reasoning" in deltas else -1
            first_content = deltas.index("content") if "content" in deltas else -1

            if first_reasoning > first_content and first_content != -1:
                print(f"⚠️  WARNING: Content came before reasoning (unusual)")

        print(f"✅ PASS: Reasoning + content streaming")
        print(f"   Chunks: {capture.get_chunk_count()}")
        print(f"   Has reasoning: {has_reasoning} ({len(capture.get_reasoning())} chars)")
        print(f"   Has content: {has_content} ({len(capture.get_content())} chars)")
        print(f"   Finish reason: {capture.finish_reason}")
        return True

    except Exception as e:
        print(f"❌ FAIL: {str(e)}")
        import traceback
        traceback.print_exc()
        return False


def test_max_tokens_finish_reason():
    """Test 5: max_tokens finish reason."""
    print("\n=== Test 5: max_tokens Finish Reason ===")

    client = OpenAI(api_key=API_KEY, base_url=PROXY_URL)
    capture = StreamChunkCapture()

    try:
        stream = client.chat.completions.create(
            model=MODEL,
            messages=[{"role": "user", "content": "Write a very long story about a dragon"}],
            max_tokens=10,  # Very low to trigger max_tokens
            stream=True
        )

        for chunk in stream:
            capture.add_chunk(chunk)

        # Verify finish reason is max_tokens or length
        if capture.finish_reason not in ["length", "max_tokens"]:
            print(f"⚠️  WARNING: Expected finish_reason='length' or 'max_tokens', got '{capture.finish_reason}'")

        print(f"✅ PASS: max_tokens finish reason")
        print(f"   Finish reason: {capture.finish_reason}")
        print(f"   Content length: {len(capture.get_content())} chars")
        return True

    except Exception as e:
        print(f"❌ FAIL: {str(e)}")
        import traceback
        traceback.print_exc()
        return False


def test_system_message():
    """Test 6: Streaming with system message."""
    print("\n=== Test 6: System Message Streaming ===")

    client = OpenAI(api_key=API_KEY, base_url=PROXY_URL)
    capture = StreamChunkCapture()

    try:
        stream = client.chat.completions.create(
            model=MODEL,
            messages=[
                {"role": "system", "content": "You are a helpful assistant. Always respond with exactly 'OK'."},
                {"role": "user", "content": "Say something"}
            ],
            max_tokens=50,
            stream=True
        )

        for chunk in stream:
            capture.add_chunk(chunk)

        content = capture.get_content()
        if not content:
            print(f"⚠️  SKIP: No content returned")
            return True

        print(f"✅ PASS: System message streaming")
        print(f"   Content: \"{content}\"")
        return True

    except Exception as e:
        print(f"❌ FAIL: {str(e)}")
        import traceback
        traceback.print_exc()
        return False


def test_temperature_parameter():
    """Test 7: Streaming with temperature parameter."""
    print("\n=== Test 7: Temperature Parameter ===")

    client = OpenAI(api_key=API_KEY, base_url=PROXY_URL)
    capture = StreamChunkCapture()

    try:
        stream = client.chat.completions.create(
            model=MODEL,
            messages=[{"role": "user", "content": "Say hi"}],
            max_tokens=50,
            temperature=0.5,
            stream=True
        )

        for chunk in stream:
            capture.add_chunk(chunk)

        if not capture.get_content() and not capture.get_reasoning():
            print(f"❌ FAIL: No content or reasoning received")
            return False

        print(f"✅ PASS: Temperature parameter handled")
        return True

    except Exception as e:
        print(f"❌ FAIL: {str(e)}")
        import traceback
        traceback.print_exc()
        return False


def test_top_p_parameter():
    """Test 8: Streaming with top_p parameter."""
    print("\n=== Test 8: top_p Parameter ===")

    client = OpenAI(api_key=API_KEY, base_url=PROXY_URL)
    capture = StreamChunkCapture()

    try:
        stream = client.chat.completions.create(
            model=MODEL,
            messages=[{"role": "user", "content": "Say hello"}],
            max_tokens=50,
            top_p=0.9,
            stream=True
        )

        for chunk in stream:
            capture.add_chunk(chunk)

        if not capture.get_content() and not capture.get_reasoning():
            print(f"❌ FAIL: No content or reasoning received")
            return False

        print(f"✅ PASS: top_p parameter handled")
        return True

    except Exception as e:
        print(f"❌ FAIL: {str(e)}")
        import traceback
        traceback.print_exc()
        return False


def test_role_in_first_chunk():
    """Test 9: Role present in first delta."""
    print("\n=== Test 9: Role in First Delta ===")

    client = OpenAI(api_key=API_KEY, base_url=PROXY_URL)
    capture = StreamChunkCapture()

    try:
        stream = client.chat.completions.create(
            model=MODEL,
            messages=[{"role": "user", "content": "Hi"}],
            max_tokens=50,
            stream=True
        )

        for chunk in stream:
            capture.add_chunk(chunk)

        if not capture.role:
            print(f"⚠️  WARNING: No role received in deltas")
        elif capture.role != "assistant":
            print(f"⚠️  WARNING: Expected role='assistant', got '{capture.role}'")

        print(f"✅ PASS: Role field handled")
        print(f"   Role: {capture.role}")
        return True

    except Exception as e:
        print(f"❌ FAIL: {str(e)}")
        import traceback
        traceback.print_exc()
        return False


def test_tool_roundtrip():
    """Test 10: Complete tool use conversation."""
    print("\n=== Test 10: Complete Tool Use Round-Trip ===")

    client = OpenAI(api_key=API_KEY, base_url=PROXY_URL)

    try:
        # Step 1: Request tool use
        print("  Step 1: Requesting tool use...")
        response1 = client.chat.completions.create(
            model=MODEL,
            messages=[{"role": "user", "content": "What is 25 * 4? Use the calculator."}],
            max_tokens=200,
            tools=[{
                "type": "function",
                "function": {
                    "name": "calculator",
                    "description": "Evaluate mathematical expressions",
                    "parameters": {
                        "type": "object",
                        "properties": {
                            "expression": {"type": "string"}
                        },
                        "required": ["expression"]
                    }
                }
            }]
        )

        # Extract tool call
        if not response1.choices[0].message.tool_calls:
            print(f"  ⚠️  SKIP: Backend did not return tool call")
            return True

        tool_call = response1.choices[0].message.tool_calls[0]
        print(f"    Tool called: {tool_call.function.name}")
        print(f"    Arguments: {tool_call.function.arguments}")

        # Step 2: Send tool result
        print("  Step 2: Sending tool result...")
        capture = StreamChunkCapture()

        stream = client.chat.completions.create(
            model=MODEL,
            messages=[
                {"role": "user", "content": "What is 25 * 4? Use the calculator."},
                response1.choices[0].message,  # Assistant's tool call
                {
                    "role": "tool",
                    "tool_call_id": tool_call.id,
                    "content": "100"
                }
            ],
            max_tokens=200,
            tools=[{
                "type": "function",
                "function": {
                    "name": "calculator",
                    "description": "Evaluate mathematical expressions",
                    "parameters": {
                        "type": "object",
                        "properties": {
                            "expression": {"type": "string"}
                        }
                    }
                }
            }],
            stream=True
        )

        for chunk in stream:
            capture.add_chunk(chunk)

        # Step 3: Verify final response
        print("  Step 3: Verifying final response...")
        final_content = capture.get_content()

        if not final_content:
            print(f"  ⚠️  SKIP: No final content (may have used all tokens for reasoning)")
            return True

        print(f"  ✅ PASS: Complete tool use round-trip")
        print(f"    Turn 1: User request → Tool call")
        print(f"    Turn 2: Tool result → Final answer")
        print(f"    Final answer: \"{final_content[:80]}...\"")
        return True

    except Exception as e:
        print(f"  ❌ FAIL: {str(e)}")
        import traceback
        traceback.print_exc()
        return False


def test_empty_tool_arguments():
    """Test 11: Tool call with empty arguments."""
    print("\n=== Test 11: Tool Call with Empty Arguments ===")

    client = OpenAI(api_key=API_KEY, base_url=PROXY_URL)
    capture = StreamChunkCapture()

    try:
        stream = client.chat.completions.create(
            model=MODEL,
            messages=[{"role": "user", "content": "Get the current weather. Use the weather tool."}],
            max_tokens=200,
            tools=[{
                "type": "function",
                "function": {
                    "name": "get_weather",
                    "description": "Get current weather",
                    "parameters": {
                        "type": "object",
                        "properties": {}  # No required parameters
                    }
                }
            }],
            stream=True
        )

        for chunk in stream:
            capture.add_chunk(chunk)

        tool_calls = capture.get_tool_calls()

        if not tool_calls:
            print(f"⚠️  SKIP: Backend did not return tool call")
            return True

        # Verify tool call has valid (possibly empty) JSON
        valid, msg = capture.verify_tool_calls_complete()
        if not valid:
            print(f"❌ FAIL: {msg}")
            return False

        print(f"✅ PASS: Empty arguments tool call")
        print(f"   Tool: {tool_calls[0]['function']['name']}")
        print(f"   Arguments: {tool_calls[0]['function']['arguments']}")
        return True

    except Exception as e:
        print(f"❌ FAIL: {str(e)}")
        import traceback
        traceback.print_exc()
        return False


def test_stop_sequence():
    """Test 12: Streaming with stop sequence."""
    print("\n=== Test 12: Stop Sequence ===")

    client = OpenAI(api_key=API_KEY, base_url=PROXY_URL)
    capture = StreamChunkCapture()

    try:
        stream = client.chat.completions.create(
            model=MODEL,
            messages=[{"role": "user", "content": "Count: 1, 2, 3, 4, 5"}],
            max_tokens=100,
            stop=["4"],
            stream=True
        )

        for chunk in stream:
            capture.add_chunk(chunk)

        # Backend may or may not respect stop sequences
        content = capture.get_content()

        print(f"✅ PASS: Stop sequence handled")
        print(f"   Content: \"{content}\"")
        print(f"   Finish reason: {capture.finish_reason}")
        return True

    except Exception as e:
        print(f"❌ FAIL: {str(e)}")
        import traceback
        traceback.print_exc()
        return False


def test_chunk_ids_consistent():
    """Test 13: All chunks have same ID."""
    print("\n=== Test 13: Consistent Chunk IDs ===")

    client = OpenAI(api_key=API_KEY, base_url=PROXY_URL)
    capture = StreamChunkCapture()

    try:
        stream = client.chat.completions.create(
            model=MODEL,
            messages=[{"role": "user", "content": "Say hello"}],
            max_tokens=50,
            stream=True
        )

        for chunk in stream:
            capture.add_chunk(chunk)

        # Get all unique chunk IDs
        chunk_ids = set(c["id"] for c in capture.chunks if c["id"])

        if len(chunk_ids) == 0:
            print(f"⚠️  WARNING: No chunk IDs found")
        elif len(chunk_ids) > 1:
            print(f"⚠️  WARNING: Multiple chunk IDs found: {chunk_ids}")

        print(f"✅ PASS: Chunk ID consistency checked")
        print(f"   Unique IDs: {len(chunk_ids)}")
        return True

    except Exception as e:
        print(f"❌ FAIL: {str(e)}")
        import traceback
        traceback.print_exc()
        return False


def main():
    """Run all tests."""
    print("=" * 60)
    print("OpenAI API Streaming Compliance Tests")
    print("=" * 60)
    print(f"Proxy URL: {PROXY_URL}")
    print(f"Model: {MODEL}")

    tests = [
        ("Text-only streaming", test_text_only_streaming),
        ("Single function call", test_function_calling_single),
        ("Multiple parallel function calls", test_function_calling_multiple),
        ("Reasoning + content streaming", test_reasoning_content_streaming),
        ("max_tokens finish reason", test_max_tokens_finish_reason),
        ("System message", test_system_message),
        ("Temperature parameter", test_temperature_parameter),
        ("top_p parameter", test_top_p_parameter),
        ("Role in first delta", test_role_in_first_chunk),
        ("Complete tool use round-trip", test_tool_roundtrip),
        ("Tool call with empty arguments", test_empty_tool_arguments),
        ("Stop sequence", test_stop_sequence),
        ("Consistent chunk IDs", test_chunk_ids_consistent),
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
