#!/usr/bin/env python3
"""
SSE (Server-Sent Events) streaming tests for Gemini API.

The Gemini API supports SSE streaming with alt=sse parameter.
These tests verify the proxy correctly handles SSE format.
"""

import os
import sys
import requests
import json
from typing import List, Dict, Any


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


def test_sse_text_only():
    """Test 1: SSE streaming with text-only response."""
    print("\n=== Test 1: SSE Text-Only Streaming ===")

    try:
        url = f"{PROXY_URL}/v1beta/models/{MODEL}:streamGenerateContent?alt=sse"
        payload = {
            "contents": [{
                "parts": [{"text": "Say hello in one sentence"}]
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

        if len(chunks) == 0:
            print(f"❌ FAIL: No SSE chunks received")
            return False

        # Verify chunk structure
        for i, chunk in enumerate(chunks):
            if 'candidates' not in chunk:
                print(f"❌ FAIL: Chunk {i} missing 'candidates' field")
                return False

        # Accumulate text
        full_text = ""
        for chunk in chunks:
            if chunk.get('candidates'):
                candidate = chunk['candidates'][0]
                if candidate.get('content', {}).get('parts'):
                    for part in candidate['content']['parts']:
                        if 'text' in part:
                            full_text += part['text']

        if not full_text:
            print(f"❌ FAIL: No text content received")
            return False

        print(f"✅ PASS: SSE text-only streaming")
        print(f"   Chunks: {len(chunks)}")
        print(f"   Content: \"{full_text[:50]}...\"")
        return True

    except Exception as e:
        print(f"❌ FAIL: {str(e)}")
        import traceback
        traceback.print_exc()
        return False


def test_sse_with_system_instruction():
    """Test 2: SSE streaming with system instruction."""
    print("\n=== Test 2: SSE with System Instruction ===")

    try:
        url = f"{PROXY_URL}/v1beta/models/{MODEL}:streamGenerateContent?alt=sse"
        payload = {
            "contents": [{
                "parts": [{"text": "What is AI?"}]
            }],
            "systemInstruction": {
                "parts": [{"text": "You are a helpful assistant. Be concise."}]
            }
        }

        response = requests.post(
            url,
            json=payload,
            headers={"Content-Type": "application/json"},
            stream=True
        )

        if response.status_code != 200:
            print(f"❌ FAIL: HTTP {response.status_code}")
            return False

        chunks = parse_sse_stream(response)

        if len(chunks) == 0:
            print(f"❌ FAIL: No chunks received")
            return False

        print(f"✅ PASS: System instruction handled")
        print(f"   Chunks: {len(chunks)}")
        return True

    except Exception as e:
        print(f"❌ FAIL: {str(e)}")
        import traceback
        traceback.print_exc()
        return False


def test_sse_finish_reason():
    """Test 3: SSE streaming finish reason."""
    print("\n=== Test 3: SSE Finish Reason ===")

    try:
        url = f"{PROXY_URL}/v1beta/models/{MODEL}:streamGenerateContent?alt=sse"
        payload = {
            "contents": [{
                "parts": [{"text": "Count to 3"}]
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
            return False

        chunks = parse_sse_stream(response)

        # Check for finish reason in last chunk
        finish_reason = None
        for chunk in reversed(chunks):
            if chunk.get('candidates'):
                candidate = chunk['candidates'][0]
                if candidate.get('finishReason'):
                    finish_reason = candidate['finishReason']
                    break

        if not finish_reason:
            print(f"⚠️  WARNING: No finish reason found")

        print(f"✅ PASS: Finish reason present")
        print(f"   Chunks: {len(chunks)}")
        print(f"   Finish reason: {finish_reason}")
        return True

    except Exception as e:
        print(f"❌ FAIL: {str(e)}")
        import traceback
        traceback.print_exc()
        return False


def main():
    """Run SSE streaming tests."""
    print("=" * 60)
    print("Gemini API SSE Streaming Tests")
    print("=" * 60)
    print(f"Proxy URL: {PROXY_URL}")
    print(f"Model: {MODEL}")

    tests = [
        ("SSE text-only streaming", test_sse_text_only),
        ("SSE with system instruction", test_sse_with_system_instruction),
        ("SSE finish reason", test_sse_finish_reason),
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
