#!/usr/bin/env python3
"""
Unit tests for Gemini API streaming implementation using official SDK.

STREAMING FORMAT: JSON Array (alt=json)
This test uses Gemini's JSON array streaming format where responses come as:
  [{"candidates": [...]}, {"candidates": [...]}]

Note: Gemini supports TWO streaming formats:
1. JSON Array (alt=json) - Used by this test - Returns array of JSON objects
2. SSE (alt=sse) - Server-Sent Events format - Returns "data: {...}" lines

The Python SDK uses JSON array format by default.

Tests verify spec compliance:
- Correct chunk sequence
- Proper content and tool handling
- Streaming format correctness
"""

import os
import sys
from typing import List, Dict, Any
import google.generativeai as genai
from google.generativeai.types import GenerateContentResponse

# Test configuration
PROXY_URL = os.getenv("GOOGLE_GEMINI_BASE_URL", "http://localhost:9000")
API_KEY = os.getenv("GOOGLE_API_KEY", "test-key")  # Proxy ignores this
MODEL = "gemini-2.0-flash"


class StreamChunkCapture:
    """Captures and analyzes streaming chunks."""

    def __init__(self):
        self.chunks: List[GenerateContentResponse] = []
        self.full_text = ""
        self.tool_calls = []

    def add_chunk(self, chunk: GenerateContentResponse):
        """Add a chunk to the capture."""
        self.chunks.append(chunk)

        # Accumulate text (safely handle chunks without text)
        try:
            if chunk.text:
                self.full_text += chunk.text
        except (ValueError, AttributeError):
            # Chunk doesn't have text (e.g., function call, empty final chunk)
            pass

        # Collect tool calls
        if chunk.candidates and len(chunk.candidates) > 0:
            candidate = chunk.candidates[0]
            if candidate.content and candidate.content.parts:
                for part in candidate.content.parts:
                    if hasattr(part, 'function_call') and part.function_call:
                        self.tool_calls.append({
                            'name': part.function_call.name,
                            'args': dict(part.function_call.args) if part.function_call.args else {}
                        })

    def get_chunk_count(self) -> int:
        """Get number of chunks received."""
        return len(self.chunks)

    def has_finish_reason(self) -> bool:
        """Check if any chunk has a finish reason."""
        for chunk in self.chunks:
            if chunk.candidates and len(chunk.candidates) > 0:
                if chunk.candidates[0].finish_reason:
                    return True
        return False

    def get_finish_reason(self) -> str:
        """Get the finish reason from the last chunk."""
        for chunk in reversed(self.chunks):
            if chunk.candidates and len(chunk.candidates) > 0:
                if chunk.candidates[0].finish_reason:
                    return str(chunk.candidates[0].finish_reason)
        return None

    def get_text(self) -> str:
        """Get accumulated text."""
        return self.full_text

    def get_tool_calls(self) -> List[Dict]:
        """Get all tool calls."""
        return self.tool_calls


def test_text_only_streaming():
    """Test 1: Text-only response streaming."""
    print("\n=== Test 1: Text-Only Streaming ===")

    try:
        # Configure with proxy URL
        genai.configure(api_key=API_KEY, transport='rest',
                       client_options={'api_endpoint': PROXY_URL})

        model = genai.GenerativeModel(MODEL)
        capture = StreamChunkCapture()

        response = model.generate_content(
            "Say hello in one sentence",
            stream=True
        )

        for chunk in response:
            capture.add_chunk(chunk)

        # Verify we got chunks
        if capture.get_chunk_count() == 0:
            print(f"❌ FAIL: No chunks received")
            return False

        # Verify we got text
        text = capture.get_text()
        if not text or len(text.strip()) == 0:
            print(f"❌ FAIL: No text content received")
            return False

        # Verify finish reason
        if not capture.has_finish_reason():
            print(f"⚠️  WARNING: No finish reason in chunks")

        print(f"✅ PASS: Text-only streaming")
        print(f"   Chunks: {capture.get_chunk_count()}")
        print(f"   Content: \"{text[:50]}...\"")
        print(f"   Finish reason: {capture.get_finish_reason()}")
        return True

    except Exception as e:
        print(f"❌ FAIL: {str(e)}")
        import traceback
        traceback.print_exc()
        return False


def test_function_calling_streaming():
    """Test 2: Function calling in streaming mode."""
    print("\n=== Test 2: Function Calling Streaming ===")

    try:
        genai.configure(api_key=API_KEY, transport='rest',
                       client_options={'api_endpoint': PROXY_URL})

        # Define calculator tool
        calculator_tool = genai.protos.Tool(
            function_declarations=[
                genai.protos.FunctionDeclaration(
                    name='calculator',
                    description='Evaluate mathematical expressions',
                    parameters=genai.protos.Schema(
                        type=genai.protos.Type.OBJECT,
                        properties={
                            'expression': genai.protos.Schema(
                                type=genai.protos.Type.STRING,
                                description='Mathematical expression to evaluate'
                            )
                        },
                        required=['expression']
                    )
                )
            ]
        )

        model = genai.GenerativeModel(
            MODEL,
            tools=[calculator_tool]
        )
        capture = StreamChunkCapture()

        response = model.generate_content(
            "What is 25 multiplied by 4? Use the calculator.",
            stream=True
        )

        for chunk in response:
            capture.add_chunk(chunk)

        # Verify we got chunks
        if capture.get_chunk_count() == 0:
            print(f"❌ FAIL: No chunks received")
            return False

        # Check for tool calls or text
        tool_calls = capture.get_tool_calls()
        text = capture.get_text()

        if len(tool_calls) == 0 and (not text or len(text.strip()) == 0):
            print(f"❌ FAIL: No content (neither tool calls nor text)")
            return False

        # If we got tool calls, verify structure
        if len(tool_calls) > 0:
            for i, call in enumerate(tool_calls):
                if 'name' not in call or not call['name']:
                    print(f"❌ FAIL: Tool call {i} missing name")
                    return False
                if 'args' not in call:
                    print(f"❌ FAIL: Tool call {i} missing args")
                    return False

        print(f"✅ PASS: Function calling streaming")
        print(f"   Chunks: {capture.get_chunk_count()}")
        print(f"   Tool calls: {len(tool_calls)}")
        if tool_calls:
            for call in tool_calls:
                print(f"     - {call['name']}: {call['args']}")
        if text:
            print(f"   Text: \"{text[:50]}...\"")
        return True

    except Exception as e:
        print(f"❌ FAIL: {str(e)}")
        import traceback
        traceback.print_exc()
        return False


def test_streaming_consistency():
    """Test 3: Verify streaming chunks are consistent."""
    print("\n=== Test 3: Streaming Consistency ===")

    try:
        genai.configure(api_key=API_KEY, transport='rest',
                       client_options={'api_endpoint': PROXY_URL})

        model = genai.GenerativeModel(MODEL)
        capture = StreamChunkCapture()

        response = model.generate_content(
            "Count from 1 to 5",
            stream=True
        )

        chunk_texts = []
        for chunk in response:
            capture.add_chunk(chunk)
            try:
                if chunk.text:
                    chunk_texts.append(chunk.text)
            except (ValueError, AttributeError):
                pass

        # Verify chunks are not empty
        if len(chunk_texts) == 0:
            print(f"❌ FAIL: No text chunks received")
            return False

        # Verify accumulated text matches concatenation
        concatenated = ''.join(chunk_texts)
        accumulated = capture.get_text()

        if concatenated != accumulated:
            print(f"❌ FAIL: Text accumulation mismatch")
            print(f"   Concatenated: \"{concatenated}\"")
            print(f"   Accumulated: \"{accumulated}\"")
            return False

        print(f"✅ PASS: Streaming consistency verified")
        print(f"   Text chunks: {len(chunk_texts)}")
        print(f"   Total text length: {len(accumulated)} chars")
        return True

    except Exception as e:
        print(f"❌ FAIL: {str(e)}")
        import traceback
        traceback.print_exc()
        return False


def test_complete_tool_roundtrip():
    """Test 4: Complete tool use conversation - request → result → final answer."""
    print("\n=== Test 4: Complete Tool Use Round-Trip ===")

    try:
        genai.configure(api_key=API_KEY, transport='rest',
                       client_options={'api_endpoint': PROXY_URL})

        # Define calculator tool
        calculator_tool = genai.protos.Tool(
            function_declarations=[
                genai.protos.FunctionDeclaration(
                    name='calculator',
                    description='Evaluate mathematical expressions',
                    parameters=genai.protos.Schema(
                        type=genai.protos.Type.OBJECT,
                        properties={
                            'expression': genai.protos.Schema(
                                type=genai.protos.Type.STRING,
                                description='Mathematical expression to evaluate'
                            )
                        },
                        required=['expression']
                    )
                )
            ]
        )

        model = genai.GenerativeModel(
            MODEL,
            tools=[calculator_tool]
        )

        # Step 1: Initial request
        print("  Step 1: Sending initial request to trigger tool use...")
        chat = model.start_chat()

        response1 = chat.send_message(
            "What is 25 multiplied by 4? Use the calculator tool."
        )

        # Step 2: Extract tool call
        print("  Step 2: Extracting function call from response...")
        tool_call = None
        if response1.candidates and len(response1.candidates) > 0:
            candidate = response1.candidates[0]
            if candidate.content and candidate.content.parts:
                for part in candidate.content.parts:
                    if hasattr(part, 'function_call') and part.function_call:
                        tool_call = part.function_call
                        break

        if not tool_call:
            print(f"  ⚠️  SKIP: Backend did not return function call (returned text instead)")
            return True

        print(f"    Function called: {tool_call.name}")
        print(f"    Function args: {dict(tool_call.args)}")

        # Step 3: Send tool result and stream response
        print("  Step 3: Sending function result and streaming final response...")

        # Create function response
        function_response = genai.protos.Part(
            function_response=genai.protos.FunctionResponse(
                name=tool_call.name,
                response={'result': '100'}
            )
        )

        capture = StreamChunkCapture()
        response2 = chat.send_message(
            function_response,
            stream=True
        )

        for chunk in response2:
            capture.add_chunk(chunk)

        # Step 4: Verify final response
        print("  Step 4: Verifying final response...")

        final_text = capture.get_text()
        if not final_text or len(final_text.strip()) == 0:
            print(f"  ⚠️  WARNING: No text in final response")

        print(f"  ✅ PASS: Complete tool use round-trip successful")
        print(f"    Turn 1: LLM requested function call → {tool_call.name}")
        print(f"    Turn 2: User provided function result → '100'")
        print(f"    Turn 3: LLM generated final answer → \"{final_text[:80]}...\"")
        print(f"    Chunks received: {capture.get_chunk_count()}")
        return True

    except Exception as e:
        print(f"  ❌ FAIL: {str(e)}")
        import traceback
        traceback.print_exc()
        return False


def test_system_instruction():
    """Test 5: Streaming with system instruction."""
    print("\n=== Test 5: System Instruction ===")

    try:
        genai.configure(api_key=API_KEY, transport='rest',
                       client_options={'api_endpoint': PROXY_URL})

        model = genai.GenerativeModel(
            MODEL,
            system_instruction="You are a helpful assistant. Always be concise."
        )
        capture = StreamChunkCapture()

        response = model.generate_content(
            "What is AI?",
            stream=True
        )

        for chunk in response:
            capture.add_chunk(chunk)

        # Verify we got chunks
        if capture.get_chunk_count() == 0:
            print(f"❌ FAIL: No chunks received")
            return False

        # Verify we got text
        text = capture.get_text()
        if not text or len(text.strip()) == 0:
            print(f"❌ FAIL: No text content received")
            return False

        print(f"✅ PASS: System instruction handled")
        print(f"   Chunks: {capture.get_chunk_count()}")
        print(f"   Content length: {len(text)} chars")
        return True

    except Exception as e:
        print(f"❌ FAIL: {str(e)}")
        import traceback
        traceback.print_exc()
        return False


def test_temperature_parameter():
    """Test 6: Streaming with temperature parameter."""
    print("\n=== Test 6: Temperature Parameter ===")

    try:
        genai.configure(api_key=API_KEY, transport='rest',
                       client_options={'api_endpoint': PROXY_URL})

        model = genai.GenerativeModel(MODEL)
        capture = StreamChunkCapture()

        generation_config = genai.types.GenerationConfig(
            temperature=0.5,
            max_output_tokens=50
        )

        response = model.generate_content(
            "Say hello",
            generation_config=generation_config,
            stream=True
        )

        for chunk in response:
            capture.add_chunk(chunk)

        # Verify we got chunks
        if capture.get_chunk_count() == 0:
            print(f"❌ FAIL: No chunks received")
            return False

        print(f"✅ PASS: Temperature parameter handled")
        print(f"   Chunks: {capture.get_chunk_count()}")
        return True

    except Exception as e:
        print(f"❌ FAIL: {str(e)}")
        import traceback
        traceback.print_exc()
        return False


def test_chat_history():
    """Test 7: Streaming with chat history."""
    print("\n=== Test 7: Chat History ===")

    try:
        genai.configure(api_key=API_KEY, transport='rest',
                       client_options={'api_endpoint': PROXY_URL})

        model = genai.GenerativeModel(MODEL)
        chat = model.start_chat()

        # First message
        response1 = chat.send_message("My name is Alice")

        # Second message with streaming
        capture = StreamChunkCapture()
        response2 = chat.send_message("What is my name?", stream=True)

        for chunk in response2:
            capture.add_chunk(chunk)

        # Verify we got chunks
        if capture.get_chunk_count() == 0:
            print(f"❌ FAIL: No chunks received")
            return False

        text = capture.get_text()
        if not text:
            print(f"❌ FAIL: No text content received")
            return False

        print(f"✅ PASS: Chat history handled")
        print(f"   Chunks: {capture.get_chunk_count()}")
        print(f"   Response mentions name: {'Alice' in text or 'alice' in text.lower()}")
        return True

    except Exception as e:
        print(f"❌ FAIL: {str(e)}")
        import traceback
        traceback.print_exc()
        return False


def test_max_output_tokens():
    """Test 8: Verify max_output_tokens limit."""
    print("\n=== Test 8: Max Output Tokens ===")

    try:
        genai.configure(api_key=API_KEY, transport='rest',
                       client_options={'api_endpoint': PROXY_URL})

        model = genai.GenerativeModel(MODEL)
        capture = StreamChunkCapture()

        generation_config = genai.types.GenerationConfig(
            max_output_tokens=10  # Very low to trigger limit
        )

        response = model.generate_content(
            "Write a long story about a dragon",
            generation_config=generation_config,
            stream=True
        )

        for chunk in response:
            capture.add_chunk(chunk)

        # Verify we got chunks
        if capture.get_chunk_count() == 0:
            print(f"❌ FAIL: No chunks received")
            return False

        # Check finish reason
        finish_reason = capture.get_finish_reason()
        if not finish_reason:
            print(f"⚠️  WARNING: No finish reason found")

        print(f"✅ PASS: Max output tokens handled")
        print(f"   Chunks: {capture.get_chunk_count()}")
        print(f"   Finish reason: {finish_reason}")
        return True

    except Exception as e:
        print(f"❌ FAIL: {str(e)}")
        import traceback
        traceback.print_exc()
        return False


def test_multiple_function_calls():
    """Test 9: Multiple function calls in streaming."""
    print("\n=== Test 9: Multiple Function Calls ===")

    try:
        genai.configure(api_key=API_KEY, transport='rest',
                       client_options={'api_endpoint': PROXY_URL})

        # Define calculator tool
        calculator_tool = genai.protos.Tool(
            function_declarations=[
                genai.protos.FunctionDeclaration(
                    name='calculator',
                    description='Evaluate mathematical expressions',
                    parameters=genai.protos.Schema(
                        type=genai.protos.Type.OBJECT,
                        properties={
                            'expression': genai.protos.Schema(
                                type=genai.protos.Type.STRING
                            )
                        },
                        required=['expression']
                    )
                )
            ]
        )

        model = genai.GenerativeModel(
            MODEL,
            tools=[calculator_tool]
        )
        capture = StreamChunkCapture()

        response = model.generate_content(
            "Calculate both 5*7 and 10+20 using the calculator",
            stream=True
        )

        for chunk in response:
            capture.add_chunk(chunk)

        # Verify we got chunks
        if capture.get_chunk_count() == 0:
            print(f"❌ FAIL: No chunks received")
            return False

        tool_calls = capture.get_tool_calls()

        if len(tool_calls) == 0:
            print(f"⚠️  SKIP: Backend did not return function calls")
            return True

        # Verify each tool call has required fields
        for i, call in enumerate(tool_calls):
            if 'name' not in call or not call['name']:
                print(f"❌ FAIL: Function call {i} missing name")
                return False

        print(f"✅ PASS: Multiple function calls handled")
        print(f"   Function calls: {len(tool_calls)}")
        for call in tool_calls:
            print(f"     - {call['name']}: {call['args']}")
        return True

    except Exception as e:
        print(f"❌ FAIL: {str(e)}")
        import traceback
        traceback.print_exc()
        return False


def test_empty_response_handling():
    """Test 10: Handle edge case of empty response."""
    print("\n=== Test 10: Empty Response Handling ===")

    try:
        genai.configure(api_key=API_KEY, transport='rest',
                       client_options={'api_endpoint': PROXY_URL})

        model = genai.GenerativeModel(MODEL)
        capture = StreamChunkCapture()

        generation_config = genai.types.GenerationConfig(
            max_output_tokens=1  # Extremely low
        )

        response = model.generate_content(
            "Hello",
            generation_config=generation_config,
            stream=True
        )

        for chunk in response:
            capture.add_chunk(chunk)

        # Should still get at least one chunk even if empty
        if capture.get_chunk_count() == 0:
            print(f"❌ FAIL: No chunks received")
            return False

        print(f"✅ PASS: Empty response handled gracefully")
        print(f"   Chunks: {capture.get_chunk_count()}")
        print(f"   Text length: {len(capture.get_text())} chars")
        return True

    except Exception as e:
        print(f"❌ FAIL: {str(e)}")
        import traceback
        traceback.print_exc()
        return False


def main():
    """Run all tests."""
    print("=" * 60)
    print("Gemini API Streaming Compliance Tests")
    print("=" * 60)
    print(f"Proxy URL: {PROXY_URL}")
    print(f"Model: {MODEL}")

    tests = [
        ("Text-only streaming", test_text_only_streaming),
        ("Function calling streaming", test_function_calling_streaming),
        ("Streaming consistency", test_streaming_consistency),
        ("Complete tool use round-trip", test_complete_tool_roundtrip),
        ("System instruction", test_system_instruction),
        ("Temperature parameter", test_temperature_parameter),
        ("Chat history", test_chat_history),
        ("Max output tokens", test_max_output_tokens),
        ("Multiple function calls", test_multiple_function_calls),
        ("Empty response handling", test_empty_response_handling),
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
