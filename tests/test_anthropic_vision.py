#!/usr/bin/env python3
"""
Vision tests for Anthropic API using base64-encoded images.

Tests verify:
- Image content blocks with base64 data
- Mixed text + image messages
- Multiple images in one message
- Vision + tool use combined
"""

import os
import sys
import base64
from pathlib import Path
from anthropic import Anthropic

# Test configuration
PROXY_URL = os.getenv("ANTHROPIC_BASE_URL", "http://localhost:9000")
API_KEY = os.getenv("ANTHROPIC_API_KEY", "test-key")
MODEL = "claude-3-haiku-20240307"


def create_test_image_base64():
    """
    Create a small test image (1x1 red pixel PNG) as base64.

    PNG structure:
    - Signature: 89 50 4E 47 0D 0A 1A 0A
    - IHDR chunk: width=1, height=1, bit_depth=8, color_type=2 (RGB)
    - IDAT chunk: compressed image data
    - IEND chunk
    """
    # 1x1 red pixel PNG (69 bytes)
    png_bytes = bytes([
        0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A,  # PNG signature
        0x00, 0x00, 0x00, 0x0D, 0x49, 0x48, 0x44, 0x52,  # IHDR chunk
        0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01,  # width=1, height=1
        0x08, 0x02, 0x00, 0x00, 0x00, 0x90, 0x77, 0x53,  # bit_depth=8, color=RGB
        0xDE, 0x00, 0x00, 0x00, 0x0C, 0x49, 0x44, 0x41,  # IDAT chunk
        0x54, 0x08, 0xD7, 0x63, 0xF8, 0xCF, 0xC0, 0x00,  # compressed data (red)
        0x00, 0x03, 0x01, 0x01, 0x00, 0x18, 0xDD, 0x8D,
        0xB4, 0x00, 0x00, 0x00, 0x00, 0x49, 0x45, 0x4E,  # IEND chunk
        0x44, 0xAE, 0x42, 0x60, 0x82
    ])
    return base64.b64encode(png_bytes).decode('utf-8')


def test_vision_single_image():
    """Test 1: Single image with text prompt."""
    print("\n=== Test 1: Single Image Vision ===")

    client = Anthropic(api_key=API_KEY, base_url=PROXY_URL)
    image_data = create_test_image_base64()

    try:
        response = client.messages.create(
            model=MODEL,
            max_tokens=100,
            messages=[{
                "role": "user",
                "content": [
                    {
                        "type": "text",
                        "text": "What color is this image?"
                    },
                    {
                        "type": "image",
                        "source": {
                            "type": "base64",
                            "media_type": "image/png",
                            "data": image_data
                        }
                    }
                ]
            }]
        )

        print(f"✅ PASS: Single image vision")
        print(f"   Response: {response.content[0].text[:100]}...")
        return True

    except Exception as e:
        print(f"❌ FAIL: {str(e)}")
        return False


def test_vision_streaming():
    """Test 2: Streaming response with image."""
    print("\n=== Test 2: Vision Streaming ===")

    client = Anthropic(api_key=API_KEY, base_url=PROXY_URL)
    image_data = create_test_image_base64()

    try:
        event_count = 0
        text_content = ""

        with client.messages.stream(
            model=MODEL,
            max_tokens=100,
            messages=[{
                "role": "user",
                "content": [
                    {
                        "type": "text",
                        "text": "Describe this image briefly."
                    },
                    {
                        "type": "image",
                        "source": {
                            "type": "base64",
                            "media_type": "image/png",
                            "data": image_data
                        }
                    }
                ]
            }]
        ) as stream:
            for event in stream:
                event_count += 1
                if event.type == "content_block_delta":
                    if hasattr(event.delta, 'text'):
                        text_content += event.delta.text

        if event_count == 0:
            print(f"❌ FAIL: No events received")
            return False

        print(f"✅ PASS: Vision streaming")
        print(f"   Events: {event_count}")
        print(f"   Response: {text_content[:80]}...")
        return True

    except Exception as e:
        print(f"❌ FAIL: {str(e)}")
        return False


def test_vision_multiple_images():
    """Test 3: Multiple images in one message."""
    print("\n=== Test 3: Multiple Images ===")

    client = Anthropic(api_key=API_KEY, base_url=PROXY_URL)
    red_image = create_test_image_base64()

    # Create a blue pixel PNG (modify the red pixel data)
    blue_png_bytes = bytes([
        0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A,
        0x00, 0x00, 0x00, 0x0D, 0x49, 0x48, 0x44, 0x52,
        0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01,
        0x08, 0x02, 0x00, 0x00, 0x00, 0x90, 0x77, 0x53,
        0xDE, 0x00, 0x00, 0x00, 0x0C, 0x49, 0x44, 0x41,
        0x54, 0x08, 0xD7, 0x63, 0xF8, 0x0F, 0x04, 0x00,  # blue instead of red
        0x00, 0x03, 0x01, 0x01, 0x00, 0x1B, 0x4B, 0x8F,
        0x54, 0x00, 0x00, 0x00, 0x00, 0x49, 0x45, 0x4E,
        0x44, 0xAE, 0x42, 0x60, 0x82
    ])
    blue_image = base64.b64encode(blue_png_bytes).decode('utf-8')

    try:
        response = client.messages.create(
            model=MODEL,
            max_tokens=150,
            messages=[{
                "role": "user",
                "content": [
                    {"type": "text", "text": "Compare these two images."},
                    {
                        "type": "image",
                        "source": {
                            "type": "base64",
                            "media_type": "image/png",
                            "data": red_image
                        }
                    },
                    {
                        "type": "image",
                        "source": {
                            "type": "base64",
                            "media_type": "image/png",
                            "data": blue_image
                        }
                    }
                ]
            }]
        )

        print(f"✅ PASS: Multiple images")
        print(f"   Response: {response.content[0].text[:100]}...")
        return True

    except Exception as e:
        print(f"❌ FAIL: {str(e)}")
        return False


def test_vision_with_tools():
    """Test 4: Vision combined with tool use."""
    print("\n=== Test 4: Vision + Tool Use ===")

    client = Anthropic(api_key=API_KEY, base_url=PROXY_URL)
    image_data = create_test_image_base64()

    try:
        response = client.messages.create(
            model=MODEL,
            max_tokens=200,
            messages=[{
                "role": "user",
                "content": [
                    {"type": "text", "text": "Analyze this image and save the color name to a file."},
                    {
                        "type": "image",
                        "source": {
                            "type": "base64",
                            "media_type": "image/png",
                            "data": image_data
                        }
                    }
                ]
            }],
            tools=[{
                "name": "save_analysis",
                "description": "Save analysis results to a file",
                "input_schema": {
                    "type": "object",
                    "properties": {
                        "filename": {"type": "string"},
                        "content": {"type": "string"}
                    },
                    "required": ["filename", "content"]
                }
            }]
        )

        # Check if tool was used or text response
        has_tool_use = any(block.type == "tool_use" for block in response.content)
        has_text = any(block.type == "text" for block in response.content)

        if not has_tool_use and not has_text:
            print(f"❌ FAIL: No tool_use or text in response")
            return False

        print(f"✅ PASS: Vision + tool use")
        print(f"   Content blocks: {len(response.content)}")
        print(f"   Has tool_use: {has_tool_use}, Has text: {has_text}")
        return True

    except Exception as e:
        print(f"❌ FAIL: {str(e)}")
        return False


def test_vision_jpeg():
    """Test 5: JPEG image support."""
    print("\n=== Test 5: JPEG Image ===")

    client = Anthropic(api_key=API_KEY, base_url=PROXY_URL)

    # Minimal JPEG (1x1 red pixel, ~134 bytes)
    jpeg_bytes = bytes([
        0xFF, 0xD8, 0xFF, 0xE0, 0x00, 0x10, 0x4A, 0x46,  # JPEG header
        0x49, 0x46, 0x00, 0x01, 0x01, 0x01, 0x00, 0x48,
        0x00, 0x48, 0x00, 0x00, 0xFF, 0xDB, 0x00, 0x43,
        0x00, 0x08, 0x06, 0x06, 0x07, 0x06, 0x05, 0x08,
        0x07, 0x07, 0x07, 0x09, 0x09, 0x08, 0x0A, 0x0C,
        0x14, 0x0D, 0x0C, 0x0B, 0x0B, 0x0C, 0x19, 0x12,
        0x13, 0x0F, 0x14, 0x1D, 0x1A, 0x1F, 0x1E, 0x1D,
        0x1A, 0x1C, 0x1C, 0x20, 0x24, 0x2E, 0x27, 0x20,
        0x22, 0x2C, 0x23, 0x1C, 0x1C, 0x28, 0x37, 0x29,
        0x2C, 0x30, 0x31, 0x34, 0x34, 0x34, 0x1F, 0x27,
        0x39, 0x3D, 0x38, 0x32, 0x3C, 0x2E, 0x33, 0x34,
        0x32, 0xFF, 0xC0, 0x00, 0x0B, 0x08, 0x00, 0x01,
        0x00, 0x01, 0x01, 0x01, 0x11, 0x00, 0xFF, 0xC4,
        0x00, 0x14, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0xFF, 0xC4, 0x00, 0x14,
        0x10, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0xFF, 0xDA, 0x00, 0x08, 0x01, 0x01,
        0x00, 0x00, 0x3F, 0x00, 0x7F, 0xFF, 0xD9
    ])
    jpeg_data = base64.b64encode(jpeg_bytes).decode('utf-8')

    try:
        response = client.messages.create(
            model=MODEL,
            max_tokens=100,
            messages=[{
                "role": "user",
                "content": [
                    {"type": "text", "text": "What's in this image?"},
                    {
                        "type": "image",
                        "source": {
                            "type": "base64",
                            "media_type": "image/jpeg",
                            "data": jpeg_data
                        }
                    }
                ]
            }]
        )

        print(f"✅ PASS: JPEG image")
        print(f"   Response: {response.content[0].text[:100]}...")
        return True

    except Exception as e:
        print(f"❌ FAIL: {str(e)}")
        return False


def main():
    """Run all vision tests."""
    print("=" * 60)
    print("Anthropic Vision API Tests (Base64)")
    print("=" * 60)
    print(f"Proxy URL: {PROXY_URL}")
    print(f"Model: {MODEL}")

    tests = [
        ("Single image vision", test_vision_single_image),
        ("Vision streaming", test_vision_streaming),
        ("Multiple images", test_vision_multiple_images),
        ("Vision + tool use", test_vision_with_tools),
        ("JPEG image", test_vision_jpeg),
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
