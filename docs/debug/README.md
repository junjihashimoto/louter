# Debugging & Troubleshooting

**Audience:** Developers debugging issues, operators troubleshooting production problems

**Purpose:** Teach debugging techniques, tool usage, and systematic troubleshooting approaches.

## What Goes Here

✅ **Include:**
- How to debug common problems
- Diagnostic tool usage guides
- Log analysis techniques
- Performance troubleshooting
- Network debugging
- Protocol conversion debugging
- Test script usage
- How to interpret error messages

❌ **Don't Include:**
- Specific bug reports (→ `issues/` or `resolved/`)
- Design rationale (→ `design/`)
- Tutorial content (→ `tutorial/`)

## Debugging Guide Structure

### By Problem Type
- **Protocol Conversion Issues** - How to debug Gemini ↔ OpenAI conversion
- **Performance Problems** - TTFT, TPS, latency debugging
- **Token Count Mismatches** - Debugging candidatesTokenCount vs completion_tokens
- **Routing Failures** - Why request went to wrong backend
- **Configuration Errors** - Invalid TOML, missing fields

### By Tool
- **Web UI Dashboard** - Using /ui for real-time debugging
- **Diagnostic API** - Using /api/diagnostics for testing
- **Log Parser** - Analyzing JSON Lines logs
- **Test Runner** - Direct backend testing
- **Mock Server** - Local testing without API keys

### By Technique
- **Enable Verbose Logging** - `--verbose` flag and what it shows
- **Reading JSON Logs** - Structure and important fields
- **Prometheus Metrics** - Querying for anomalies
- **Network Tracing** - tcpdump, curl -v, etc.

## Essential Debugging Workflows

### "Request Failed" Debugging Checklist
1. Check proxy logs (`--log-file`)
2. Check backend reachability (`/api/diagnostics`)
3. Verify model_mapping in config
4. Check API key configuration
5. Test backend directly (bypass proxy)
6. Compare request/response formats

### "Wrong Response" Debugging Checklist
1. Check which backend handled request (logs)
2. Verify protocol conversion (native vs conversion)
3. Compare input/output tokens
4. Check temperature and max_tokens settings
5. Look for missing/optional fields

### "Performance Issues" Debugging Checklist
1. Check TTFT metrics (Time to First Token)
2. Check TPS metrics (Tokens Per Second)
3. Compare backend vs proxy latency
4. Check for rate limiting (429 errors)
5. Monitor with Prometheus

## File Naming Convention

Use descriptive action-oriented names:
- `debugging-protocol-conversion.md`
- `using-diagnostic-api.md`
- `analyzing-json-logs.md`
- `troubleshooting-performance.md`
- `common-error-messages.md`
