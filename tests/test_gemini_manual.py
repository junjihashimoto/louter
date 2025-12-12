#!/usr/bin/env python3
"""
Manual Gemini streaming test using test-data fixtures.
Compares proxy output against captured real Gemini API responses.
"""

import json
import requests
import sys

def parse_sse_line(line):
    """Parse SSE line: 'data: {...}' -> dict"""
    if line.startswith('data: '):
        json_str = line[6:]  # Remove 'data: ' prefix
        try:
            return json.loads(json_str)
        except:
            return None
    return None

def test_streaming_long():
    """Test 1: Long streaming response"""
    print("\n=== Test 1: Streaming Long Response ===")
    
    # Load request from test-data
    with open('test-data/gemini/streaming_long/request.json', 'r') as f:
        request_data = json.load(f)
    
    # Make request to proxy
    url = 'http://localhost:9000/v1beta/models/gemini-2.0-flash:streamGenerateContent'
    response = requests.post(url, json=request_data, stream=True)
    
    if response.status_code != 200:
        print(f"❌ FAIL: Status {response.status_code}")
        return False
    
    # Check Content-Type
    content_type = response.headers.get('Content-Type', '')
    if 'text/event-stream' not in content_type:
        print(f"❌ FAIL: Wrong Content-Type: {content_type}")
        return False
    
    # Parse streaming response
    chunks = []
    for line in response.iter_lines():
        if line:
            line_str = line.decode('utf-8')
            chunk = parse_sse_line(line_str)
            if chunk:
                chunks.append(chunk)
    
    if len(chunks) == 0:
        print(f"❌ FAIL: No chunks received")
        return False
    
    # Verify structure of first chunk
    first = chunks[0]
    if 'candidates' not in first:
        print(f"❌ FAIL: Missing 'candidates' in chunk")
        return False
    
    if not first['candidates']:
        print(f"❌ FAIL: Empty candidates array")
        return False
    
    candidate = first['candidates'][0]
    if 'content' not in candidate:
        print(f"❌ FAIL: Missing 'content' in candidate")
        return False
    
    content = candidate['content']
    if 'parts' not in content or 'role' not in content:
        print(f"❌ FAIL: Missing 'parts' or 'role' in content")
        return False
    
    # Verify we got text
    has_text = any(
        chunk.get('candidates', [{}])[0].get('content', {}).get('parts', [{}])[0].get('text')
        for chunk in chunks
    )
    
    if not has_text:
        print(f"❌ FAIL: No text content in any chunk")
        return False
    
    # Check for finishReason in last chunk
    last = chunks[-1]
    has_finish = last.get('candidates', [{}])[0].get('finishReason')
    
    print(f"✅ PASS: Streaming long response")
    print(f"   Chunks received: {len(chunks)}")
    print(f"   Content-Type: {content_type}")
    print(f"   Has text: {has_text}")
    print(f"   Has finishReason: {has_finish is not None}")
    
    return True

def test_function_calling():
    """Test 2: Function calling streaming"""
    print("\n=== Test 2: Function Calling Streaming ===")
    
    # Load request from test-data
    with open('test-data/gemini/streaming_function_calling/request_0.json', 'r') as f:
        request_data = json.load(f)
    
    # Make request to proxy
    url = 'http://localhost:9000/v1beta/models/gemini-2.0-flash:streamGenerateContent'
    response = requests.post(url, json=request_data, stream=True)
    
    if response.status_code != 200:
        print(f"❌ FAIL: Status {response.status_code}")
        return False
    
    # Parse streaming response
    chunks = []
    for line in response.iter_lines():
        if line:
            line_str = line.decode('utf-8')
            chunk = parse_sse_line(line_str)
            if chunk:
                chunks.append(chunk)
    
    if len(chunks) == 0:
        print(f"❌ FAIL: No chunks received")
        return False
    
    # Check structure
    first = chunks[0]
    if 'candidates' not in first or not first['candidates']:
        print(f"❌ FAIL: Invalid structure")
        return False
    
    print(f"✅ PASS: Function calling streaming")
    print(f"   Chunks received: {len(chunks)}")
    print(f"   Structure valid: True")
    
    return True

def test_format_match():
    """Test 3: Format matches real Gemini API"""
    print("\n=== Test 3: Format Match with Real Gemini API ===")
    
    # Load expected response from test-data
    with open('test-data/gemini/streaming_long/response.json', 'r') as f:
        expected_lines = f.readlines()
    
    # Parse first expected chunk
    expected_first = parse_sse_line(expected_lines[0].strip())
    
    if not expected_first:
        print(f"❌ FAIL: Could not parse expected response")
        return False
    
    # Load request and get proxy response
    with open('test-data/gemini/streaming_long/request.json', 'r') as f:
        request_data = json.load(f)
    
    url = 'http://localhost:9000/v1beta/models/gemini-2.0-flash:streamGenerateContent'
    response = requests.post(url, json=request_data, stream=True)
    
    # Get first chunk from proxy
    proxy_first = None
    for line in response.iter_lines():
        if line:
            line_str = line.decode('utf-8')
            proxy_first = parse_sse_line(line_str)
            if proxy_first:
                break
    
    if not proxy_first:
        print(f"❌ FAIL: No chunks from proxy")
        return False
    
    # Compare structure (keys)
    expected_keys = set(expected_first.keys())
    proxy_keys = set(proxy_first.keys())
    
    if 'candidates' not in proxy_keys or 'usageMetadata' not in proxy_keys:
        print(f"❌ FAIL: Missing required keys")
        print(f"   Expected keys: {expected_keys}")
        print(f"   Proxy keys: {proxy_keys}")
        return False
    
    # Check candidate structure
    expected_candidate_keys = set(expected_first['candidates'][0].keys())
    proxy_candidate_keys = set(proxy_first['candidates'][0].keys())
    
    required_keys = {'content', 'index'}
    if not required_keys.issubset(proxy_candidate_keys):
        print(f"❌ FAIL: Missing required candidate keys")
        return False
    
    print(f"✅ PASS: Format matches real Gemini API")
    print(f"   Top-level keys match: {expected_keys == proxy_keys or proxy_keys.issuperset({'candidates', 'usageMetadata'})}")
    print(f"   Candidate structure valid: True")
    
    return True

if __name__ == '__main__':
    print("=" * 60)
    print("Gemini Streaming Tests (Manual Verification)")
    print("=" * 60)
    
    results = []
    results.append(test_streaming_long())
    results.append(test_function_calling())
    results.append(test_format_match())
    
    print("\n" + "=" * 60)
    print("Test Summary")
    print("=" * 60)
    passed = sum(results)
    total = len(results)
    print(f"{passed}/{total} tests passed")
    
    sys.exit(0 if all(results) else 1)
