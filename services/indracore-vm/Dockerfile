# IndraCore VM Dockerfile for Railway Deployment
FROM ghcr.io/haskell/haskell:9.4-slim as builder

# Install system dependencies
RUN apt-get update && apt-get install -y \
    git \
    pkg-config \
    libssl-dev \
    libgmp-dev \
    zlib1g-dev \
    libpq-dev \
    curl \
    build-essential \
    && rm -rf /var/lib/apt/lists/*

# Set up Haskell environment
WORKDIR /build

# Copy project configuration
COPY cabal.project cabal.project.freeze* ./
COPY indracore-vm.cabal ./

# Update cabal and install dependencies
RUN cabal update && \
    cabal configure \
        --enable-optimization=2 \
        --enable-library-for-ghci \
        --enable-executable-stripping \
        --enable-library-stripping

# Copy source code
COPY src/ ./src/
COPY app/ ./app/

# Build the application
RUN cabal build --ghc-options="-O2 -funbox-strict-fields -fllvm" && \
    cabal install --install-method=copy --installdir=/opt/indracore

# Runtime stage
FROM ubuntu:22.04 as runtime

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    libssl3 \
    libgmp10 \
    zlib1g \
    libpq5 \
    curl \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Create indra user
RUN useradd -r -s /bin/false indra && \
    mkdir -p /app/data /app/keys /app/logs && \
    chown -R indra:indra /app

# Copy built application
COPY --from=builder /opt/indracore/indracore-vm /usr/local/bin/
COPY --chown=indra:indra config/ /app/config/
COPY --chown=indra:indra scripts/entrypoint.sh /app/

# Set up working directory
WORKDIR /app
USER indra

# Expose ports
EXPOSE 9933 9944 30333

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=120s --retries=3 \
    CMD curl -f http://localhost:9933/health || exit 1

# Set runtime configuration
ENV RUST_LOG=info
ENV INDRA_BASE_PATH=/app/data
ENV INDRA_CONFIG_PATH=/app/config

# Entry point
ENTRYPOINT ["/app/entrypoint.sh"]
CMD ["indracore-vm"]