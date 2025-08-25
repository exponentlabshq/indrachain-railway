# Account Abstraction Processor Dockerfile for Railway
FROM ghcr.io/haskell/haskell:9.4-slim as builder

# Install build dependencies
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

WORKDIR /build

# Copy project files
COPY cabal.project cabal.project.freeze* ./
COPY aa-processor.cabal ./

# Install dependencies
RUN cabal update && \
    cabal configure --enable-optimization=2

# Copy source
COPY src/ ./src/
COPY app/ ./app/

# Build
RUN cabal build --ghc-options="-O2 -funbox-strict-fields" && \
    cabal install --install-method=copy --installdir=/opt/aa-processor

# Runtime stage
FROM ubuntu:22.04 as runtime

RUN apt-get update && apt-get install -y \
    libssl3 \
    libgmp10 \
    zlib1g \
    libpq5 \
    curl \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Create user
RUN useradd -r -s /bin/false indra && \
    mkdir -p /app/data /app/config /app/logs && \
    chown -R indra:indra /app

# Copy application
COPY --from=builder /opt/aa-processor/aa-processor /usr/local/bin/
COPY --chown=indra:indra config/ /app/config/
COPY --chown=indra:indra scripts/entrypoint-aa.sh /app/entrypoint.sh

WORKDIR /app
USER indra

# Expose port for AA processing
EXPOSE 8080

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=60s --retries=3 \
    CMD curl -f http://localhost:8080/health || exit 1

# Environment
ENV AA_CONFIG_PATH=/app/config
ENV AA_DATA_PATH=/app/data
ENV LOG_LEVEL=INFO

ENTRYPOINT ["/app/entrypoint.sh"]
CMD ["aa-processor"]