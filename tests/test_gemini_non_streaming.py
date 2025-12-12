#!/usr/bin/env python3
"""
Non-streaming tests for Gemini API.

These tests verify the proxy correctly handles non-streaming requests
using the generateContent endpoint (without streaming).
"""

import os
import sys
import requests
import json
from typing import Dict, Any


# Test configuration
PROXY_URL = os.getenv("GOOGLE_GEMINI_BASE_URL", "http://localhost:9000")
MODEL = "gpt-oss"


def test_non_streaming_text():
    """Test 1: Non-streaming text generation."""
    print("\n=== Test 1: Non-Streaming Text Generation ===")

    try:
        url = f"{PROXY_URL}/v1beta/models/{MODEL}:generateContent"
        payload = {
            "contents": [{
                "parts": [{"text": "Say hello in one sentence"}]
            }]
        }

        response = requests.post(
            url,
            json=payload,
            headers={"Content-Type": "application/json"}
        )

        if response.status_code != 200:
            print(f"❌ FAIL: HTTP {response.status_code}")
            print(f"   Response: {response.text}")
            return False

        data = response.json()

        # Verify structure
        if 'candidates' not in data:
            print(f"❌ FAIL: Missing 'candidates' field")
            print(f"   Response: {json.dumps(data, indent=2)}")
            return False

        if len(data['candidates']) == 0:
            print(f"❌ FAIL: No candidates in response")
            return False

        candidate = data['candidates'][0]
        if 'content' not in candidate:
            print(f"❌ FAIL: No content in candidate")
            return False

        # Extract text
        text = ""
        for part in candidate['content'].get('parts', []):
            if 'text' in part:
                text += part['text']

        if not text:
            print(f"❌ FAIL: No text content")
            return False

        print(f"✅ PASS: Non-streaming text generation")
        print(f"   Content: \"{text[:50]}...\"")
        return True

    except Exception as e:
        print(f"❌ FAIL: {str(e)}")
        import traceback
        traceback.print_exc()
        return False


def test_non_streaming_with_system_instruction():
    """Test 2: Non-streaming with system instruction."""
    print("\n=== Test 2: Non-Streaming with System Instruction ===")

    try:
        url = f"{PROXY_URL}/v1beta/models/{MODEL}:generateContent"
        payload = {
            "contents": [{
                "parts": [{"text": "What is AI?"}]
            }],
            "systemInstruction": {
                "parts": [{"text": "You are a helpful assistant. Be very concise."}]
            }
        }

        response = requests.post(
            url,
            json=payload,
            headers={"Content-Type": "application/json"}
        )

        if response.status_code != 200:
            print(f"❌ FAIL: HTTP {response.status_code}")
            return False

        data = response.json()

        if 'candidates' not in data or len(data['candidates']) == 0:
            print(f"❌ FAIL: No candidates")
            return False

        print(f"✅ PASS: System instruction handled")
        return True

    except Exception as e:
        print(f"❌ FAIL: {str(e)}")
        import traceback
        traceback.print_exc()
        return False


def test_non_streaming_temperature():
    """Test 3: Non-streaming with temperature parameter."""
    print("\n=== Test 3: Non-Streaming with Temperature ===")

    try:
        url = f"{PROXY_URL}/v1beta/models/{MODEL}:generateContent"
        payload = {
            "contents": [{
                "parts": [{"text": "Say hi"}]
            }],
            "generationConfig": {
                "temperature": 0.5,
                "maxOutputTokens": 50
            }
        }

        response = requests.post(
            url,
            json=payload,
            headers={"Content-Type": "application/json"}
        )

        if response.status_code != 200:
            print(f"❌ FAIL: HTTP {response.status_code}")
            return False

        data = response.json()

        if 'candidates' not in data:
            print(f"❌ FAIL: No candidates")
            return False

        print(f"✅ PASS: Temperature parameter handled")
        return True

    except Exception as e:
        print(f"❌ FAIL: {str(e)}")
        import traceback
        traceback.print_exc()
        return False


def test_count_tokens():
    """Test 4: countTokens endpoint."""
    print("\n=== Test 4: Count Tokens ===")

    try:
        url = f"{PROXY_URL}/v1beta/models/{MODEL}:countTokens"
        payload = {
            "contents": [{
                "parts": [{"text": "Hello, how are you? This is a test message."}]
            }]
        }

        response = requests.post(
            url,
            json=payload,
            headers={"Content-Type": "application/json"}
        )

        if response.status_code != 200:
            print(f"❌ FAIL: HTTP {response.status_code}")
            print(f"   Response: {response.text}")
            return False

        data = response.json()

        if 'totalTokens' not in data:
            print(f"❌ FAIL: Missing 'totalTokens' field")
            print(f"   Response: {json.dumps(data, indent=2)}")
            return False

        total_tokens = data['totalTokens']
        if not isinstance(total_tokens, int) or total_tokens <= 0:
            print(f"❌ FAIL: Invalid token count: {total_tokens}")
            return False

        print(f"✅ PASS: Count tokens")
        print(f"   Total tokens: {total_tokens}")
        return True

    except Exception as e:
        print(f"❌ FAIL: {str(e)}")
        import traceback
        traceback.print_exc()
        return False


def main():
    """Run non-streaming tests."""
    print("=" * 60)
    print("Gemini API Non-Streaming Tests")
    print("=" * 60)
    print(f"Proxy URL: {PROXY_URL}")
    print(f"Model: {MODEL}")

    tests = [
        ("Non-streaming text generation", test_non_streaming_text),
        ("Non-streaming with system instruction", test_non_streaming_with_system_instruction),
        ("Non-streaming with temperature", test_non_streaming_temperature),
        ("Count tokens", test_count_tokens),
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
