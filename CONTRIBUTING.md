# Contributing to Louter

Thank you for your interest in contributing to Louter! This document provides guidelines and instructions for contributing.

## Code of Conduct

- Be respectful and inclusive
- Focus on constructive feedback
- Help maintain a welcoming environment

## How to Contribute

### Reporting Bugs

Before creating a bug report:
1. Check the [existing issues](https://github.com/yourusername/louter/issues)
2. Verify you're using the latest version
3. Check the [troubleshooting guide](INSTALLATION.md#troubleshooting)

When creating a bug report, include:
- Clear title and description
- Steps to reproduce
- Expected vs actual behavior
- Version information (`ghc --version`, `stack --version`)
- Relevant configuration files (sanitized)
- Error messages and stack traces

### Suggesting Features

Feature requests are welcome! Please:
1. Check if it's already been suggested
2. Explain the use case clearly
3. Describe the expected behavior
4. Consider implementation complexity

### Pull Requests

1. **Fork and Clone**
   ```bash
   git clone https://github.com/yourusername/louter.git
   cd louter
   git checkout -b feature/your-feature-name
   ```

2. **Set Up Development Environment**
   ```bash
   cd haskell/louter
   stack build
   stack test
   ```

3. **Make Your Changes**
   - Follow the [coding standards](#coding-standards)
   - Add tests for new functionality
   - Update documentation as needed
   - Keep commits atomic and well-described

4. **Test Your Changes**
   ```bash
   # Run Haskell tests
   stack test

   # Run Python integration tests
   cd ../../tests
   python3 -m pytest test_*.py

   # Check code formatting
   stack exec -- fourmolu -i $(find src app -name '*.hs')
   ```

5. **Submit PR**
   - Push to your fork
   - Create a Pull Request with clear description
   - Link any related issues
   - Wait for review

## Development Workflow

### Project Structure

```
louter/
├── haskell/louter/          # Main Haskell implementation
│   ├── src/                 # Library source code
│   │   ├── Louter/
│   │   │   ├── Client.hs              # Client library API
│   │   │   ├── Protocol/              # Protocol converters
│   │   │   ├── Backend/               # Backend conversions
│   │   │   ├── Streaming/             # Streaming parsers
│   │   │   └── Types/                 # Type definitions
│   ├── app/                 # Executables
│   │   ├── Main.hs                    # Proxy server
│   │   └── CLI.hs                     # CLI tool
│   ├── test/                # Tests
│   └── examples/            # Example code
├── tests/                   # Python SDK integration tests
├── docs/                    # Documentation
└── examples/                # Configuration examples
```

### Coding Standards

#### Haskell Style

Follow [CLAUDE.md](CLAUDE.md) for architecture guidelines. Key points:

1. **Use Strict Types**
   ```haskell
   -- Good
   data CoreChatRequest = CoreChatRequest
     { model :: Text
     , messages :: [CoreMessage]
     }

   -- Avoid
   type LooseRequest = Value  -- Too permissive
   ```

2. **Explicit Exports**
   ```haskell
   module Louter.Protocol.OpenAI
     ( openAIToCore
     , coreToOpenAI
     ) where
   ```

3. **Comprehensive Type Signatures**
   ```haskell
   -- Always include type signatures
   convertMessage :: OpenAIMessage -> Either Text CoreMessage
   convertMessage msg = ...
   ```

4. **Error Handling**
   ```haskell
   -- Use Either for expected errors
   parseRequest :: ByteString -> Either Text Request

   -- Use exceptions for unexpected failures
   readConfigFile :: FilePath -> IO Config
   ```

5. **Documentation**
   ```haskell
   -- | Convert OpenAI message format to Core IR.
   --
   -- Handles both simple string content and multimodal arrays.
   -- Returns Left with error message if conversion fails.
   openAIMessageToCore :: Value -> Either Text CoreMessage
   ```

#### Formatting

Use `fourmolu` for consistent formatting:

```bash
# Format all Haskell files
stack exec -- fourmolu -i $(find src app test -name '*.hs')

# Check formatting without changes
stack exec -- fourmolu --mode check $(find src app -name '*.hs')
```

#### Naming Conventions

- **Functions**: `camelCase`
- **Types**: `PascalCase`
- **Modules**: `Hierarchical.PascalCase`
- **Constants**: `camelCase`

### Testing Guidelines

#### Unit Tests

```haskell
-- test/Spec.hs
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "OpenAI Converter" $ do
    it "converts text messages correctly" $ do
      let input = object ["role" .= "user", "content" .= "Hello"]
      openAIMessageToCore input `shouldBe` Right expectedMessage

    it "handles multimodal content" $ do
      let input = object [...]
      ...
```

#### Integration Tests

Python tests use official SDKs:

```python
# tests/test_openai_streaming.py
def test_streaming_completion(proxy_server):
    """Test OpenAI streaming through proxy"""
    client = OpenAI(base_url=f"http://localhost:{proxy_server.port}")
    stream = client.chat.completions.create(
        model="gpt-4",
        messages=[{"role": "user", "content": "Hello"}],
        stream=True
    )
    ...
```

#### Test Coverage Goals

- Unit tests: > 80% coverage
- Integration tests: All major workflows
- Edge cases: Error handling, malformed input

### Documentation

#### Code Documentation

- Use Haddock comments for all exported functions
- Include examples in documentation
- Document preconditions and invariants

```haskell
-- | Parse a Gemini streaming chunk.
--
-- ==== __Examples__
--
-- >>> parseGeminiChunk "data: {\"candidates\":[...]}"
-- Right (GeminiChunk {...})
--
-- >>> parseGeminiChunk "data: [DONE]"
-- Right GeminiDone
parseGeminiChunk :: ByteString -> Either Text GeminiEvent
```

#### User Documentation

When adding features, update:
- README.md (if user-facing)
- Relevant docs/haskell/ guides
- CLAUDE.md (if architectural)
- Example code

### Commit Messages

Follow conventional commits:

```
<type>(<scope>): <subject>

<body>

<footer>
```

**Types:**
- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation only
- `refactor`: Code restructuring
- `test`: Adding tests
- `chore`: Maintenance

**Examples:**
```
feat(streaming): add JSON array streaming for Gemini

Implements JSON array format alongside SSE streaming.
Fixes issue with certain Gemini API versions.

Closes #123
```

```
fix(vision): handle empty image data URLs

Previously crashed on malformed data URLs.
Now returns proper error message.
```

### Release Process

1. Update CHANGELOG.md
2. Bump version in .cabal file
3. Create git tag: `git tag -a v0.5.0 -m "Release v0.5.0"`
4. Push tag: `git push origin v0.5.0`
5. Create GitHub release with notes

## Architecture Guidelines

### Adding Protocol Support

When adding a new protocol (e.g., Claude, Cohere):

1. **Create Converter Module**
   ```haskell
   -- src/Louter/Protocol/ClaudeConverter.hs
   module Louter.Protocol.ClaudeConverter
     ( claudeToOpenAI
     , openAIToClaude
     ) where
   ```

2. **Implement Conversion**
   - Request conversion
   - Response conversion
   - Streaming support
   - Error handling

3. **Add Tests**
   - Unit tests for converter
   - Integration tests with SDK
   - Test data fixtures

4. **Update Documentation**
   - Add to README feature matrix
   - Document configuration
   - Add examples

### Adding Features

Follow the principle: **Library First, Proxy Second**

1. Implement in library (src/Louter/)
2. Expose through proxy (app/Main.hs)
3. Add tests for both
4. Document library API

## Community

- **Questions?** Open a [Discussion](https://github.com/yourusername/louter/discussions)
- **Chat:** [Discord/Slack link if available]
- **Email:** [maintainer email]

## Recognition

Contributors will be:
- Listed in CONTRIBUTORS.md
- Mentioned in release notes
- Appreciated in the community!

## License

By contributing, you agree that your contributions will be licensed under the MIT License.

---

Thank you for contributing to Louter! 🎉
