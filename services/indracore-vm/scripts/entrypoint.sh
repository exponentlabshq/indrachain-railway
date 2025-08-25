#!/bin/bash

# IndraCore VM Entry Point for Railway Deployment
set -e

echo "IndraCore VM starting on Railway..."
echo "Environment: $(env | grep -E '^(NODE_ID|CONSENSUS_ROLE|NETWORK_PORT|RPC_PORT)' | sort)"

# Wait for dependencies
if [ -n "$DATABASE_URL" ]; then
    echo "Waiting for database connection..."
    until curl -s "$DATABASE_URL" >/dev/null 2>&1 || sleep 2; do
        echo "Database not ready, retrying..."
    done
    echo "Database connection confirmed"
fi

# Set GHC runtime options for Railway containers
export GHCRTS="-N -A32m -qg -I0"

# Create necessary directories
mkdir -p /app/data /app/logs

# Set permissions
chown -R indra:indra /app/data /app/logs 2>/dev/null || true

# Health check endpoint test
health_check() {
    local max_attempts=30
    local attempt=1
    
    while [ $attempt -le $max_attempts ]; do
        if curl -f http://localhost:${RPC_PORT:-9933}/health >/dev/null 2>&1; then
            echo "Health check passed"
            return 0
        fi
        echo "Health check attempt $attempt/$max_attempts failed, retrying..."
        sleep 2
        ((attempt++))
    done
    
    echo "Health check failed after $max_attempts attempts"
    return 1
}

# Start the IndraCore VM
echo "Starting IndraCore Virtual Machine..."
exec indracore-vm "$@"