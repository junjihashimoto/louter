# Louter Dockerfile - Multi-stage build for production
# ======================================================
# This Dockerfile creates a minimal production image for the Louter proxy server.
# Uses multi-stage build to keep final image small (~100MB).

# Stage 1: Build
# --------------
FROM haskell:9.6-slim AS builder

# Install system dependencies for building
RUN apt-get update && apt-get install -y \
    git \
    build-essential \
    libgmp-dev \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /build

# Copy only cabal/stack files first for better caching
COPY louter.cabal stack.yaml* ./

# Pre-build dependencies (cached layer)
RUN stack build --only-dependencies --system-ghc 2>/dev/null || \
    stack setup && stack build --only-dependencies

# Copy source code
COPY src ./src
COPY app ./app
COPY test ./test
COPY docs ./docs
COPY examples ./examples

# Build application
RUN stack build --system-ghc --copy-bins --local-bin-path /build/bin

# Stage 2: Runtime
# ----------------
FROM debian:bookworm-slim

# Install runtime dependencies only
RUN apt-get update && apt-get install -y \
    ca-certificates \
    libgmp10 \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Create non-root user
RUN useradd -m -u 1000 louter

# Copy binary from builder
COPY --from=builder /build/bin/louter-server /usr/local/bin/

# Copy default configuration examples
COPY --from=builder /build/examples /app/examples
COPY --from=builder /build/README.md /app/

# Set working directory
WORKDIR /app

# Change ownership
RUN chown -R louter:louter /app

# Switch to non-root user
USER louter

# Expose default port
EXPOSE 9000

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
    CMD curl -f http://localhost:9000/health || exit 1

# Default command
# Users can override config via: docker run -v $(pwd)/config.yaml:/app/config.yaml
CMD ["louter-server", "--config", "examples/local-llama-server.yaml", "--port", "9000"]

# Usage Examples:
# ===============
#
# Build:
# ------
# docker build -t louter:latest .
# docker build -t louter:0.1.0 .
#
# Run with default config (local llama-server):
# ----------------------------------------------
# docker run -p 9000:9000 louter:latest
#
# Run with custom config:
# -----------------------
# docker run -p 9000:9000 -v $(pwd)/my-config.yaml:/app/config.yaml \
#   louter:latest louter-server --config /app/config.yaml --port 9000
#
# Run with environment variables:
# --------------------------------
# docker run -p 9000:9000 \
#   -e OPENAI_API_KEY="sk-..." \
#   -e ANTHROPIC_API_KEY="sk-ant-..." \
#   -v $(pwd)/cloud-apis.yaml:/app/config.yaml \
#   louter:latest louter-server --config /app/config.yaml --port 9000
#
# Run in background:
# ------------------
# docker run -d -p 9000:9000 --name louter-proxy \
#   -v $(pwd)/config.yaml:/app/config.yaml \
#   louter:latest
#
# Check logs:
# -----------
# docker logs louter-proxy
# docker logs -f louter-proxy  # Follow logs
#
# Stop:
# -----
# docker stop louter-proxy
# docker rm louter-proxy
#
# Networking (connect to local llama-server):
# --------------------------------------------
# # Use host network on Linux
# docker run --network host louter:latest
#
# # On macOS/Windows, use host.docker.internal
# # In config.yaml: url: http://host.docker.internal:11211
#
# Multi-platform build:
# ---------------------
# docker buildx build --platform linux/amd64,linux/arm64 -t louter:latest .
