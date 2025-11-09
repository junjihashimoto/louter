# Louter (LLM Router) Implementation TODO

## ✅ Completed
- [x] Initialize Rust project with Cargo.toml
- [x] Define API request/response models (Gemini & OpenAI)
- [x] Create streaming response models
- [x] Design performance metrics structures
- [x] Unit tests for protocol compatibility
- [x] Integration tests with real APIs
- [x] Test multimedia support (images, audio, video, PDF)
- [x] Create configurable test endpoints

## ✅ Completed: Core Proxy Implementation

### 1. Project Structure 📁
- [x] Create main server binary (`src/main.rs`)
- [x] Implement configuration module (`src/config.rs`)
- [x] Create conversion logic (`src/conversion.rs`)
- [x] Add backend management (`src/backends.rs`)

### 2. Core Server 🚀
- [x] Axum HTTP server setup
- [x] Gemini API endpoint routing
- [x] Health check endpoint
- [x] Error handling middleware
- [x] Request logging

### 3. Request/Response Conversion 🔄
- [x] Gemini → OpenAI request conversion
  - [x] Text completions
  - [x] Vision requests (image handling)
  - [x] Function calling
  - [x] Streaming requests
- [x] OpenAI → Gemini response conversion
  - [x] Text responses
  - [x] Streaming SSE format
  - [x] Function call responses
  - [x] Token usage mapping

### 4. Backend Management 🌐
- [x] OpenAI API client implementation
- [ ] Multiple backend support
- [ ] Load balancing logic
- [ ] Backend health monitoring
- [ ] Fallback mechanisms

### 5. Streaming Support 🌊
- [x] SSE response handling
- [x] Real-time Gemini → OpenAI conversion
- [x] Stream buffering and chunking
- [x] Connection management
- [x] Error recovery in streams

### 6. Configuration 📝
- [x] TOML configuration parsing
- [x] Backend configuration
- [x] Custom instruction injection
- [x] Performance settings
- [x] Example config files

### 7. Performance Metrics 📊
- [ ] TTFT (Time to First Token) tracking
- [ ] TPS (Tokens Per Second) calculation
- [ ] Request/response timing
- [ ] Verbose mode implementation
- [ ] Metrics export

## 🎯 Testing Milestones
- [ ] Phase 2 tests pass: Gemini → OpenAI conversion
- [ ] Phase 3 tests pass: Gemini → Gemini passthrough
- [ ] Performance benchmarks
- [ ] Load testing

## 📚 Documentation
- [ ] API endpoint documentation
- [ ] Configuration guide
- [ ] Deployment instructions
- [ ] Performance tuning guide

## 🔮 Future Enhancements
- [ ] Multiple model support
- [ ] Rate limiting
- [ ] Authentication/authorization
- [ ] Caching layer
- [ ] Monitoring dashboard