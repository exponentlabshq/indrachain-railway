# UMPF Pattern Analyzer Service Dockerfile
FROM python:3.11-slim as builder

# Install system dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Set up Python environment
WORKDIR /build
COPY requirements.txt ./
RUN pip install --no-cache-dir -r requirements.txt

# Runtime stage
FROM python:3.11-slim as runtime

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Copy Python environment from builder
COPY --from=builder /usr/local/lib/python3.11/site-packages /usr/local/lib/python3.11/site-packages
COPY --from=builder /usr/local/bin /usr/local/bin

# Create application user
RUN useradd -r -s /bin/false umpf && \
    mkdir -p /app/data /app/models /app/config /app/logs && \
    chown -R umpf:umpf /app

# Copy application
COPY --chown=umpf:umpf src/ /app/src/
COPY --chown=umpf:umpf config/ /app/config/
COPY --chown=umpf:umpf models/ /app/models/
COPY --chown=umpf:umpf data/ /app/data/

WORKDIR /app
USER umpf

# Expose port for UMPF analysis API
EXPOSE 5000

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=60s --retries=3 \
    CMD curl -f http://localhost:5000/health || exit 1

# Environment variables
ENV PYTHONPATH=/app/src
ENV UMPF_CONFIG_PATH=/app/config
ENV UMPF_MODEL_PATH=/app/models
ENV FLASK_APP=src/app.py
ENV FLASK_ENV=production

# Start the UMPF analyzer
CMD ["python", "src/app.py"]