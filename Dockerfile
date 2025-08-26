# IndraChain Node for Railway Deployment
FROM node:18-alpine

# Install dependencies
RUN apk add --no-cache curl bash

# Create app directory
WORKDIR /app

# Copy server file
COPY server.js .

# Expose port
EXPOSE 9933

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=30s \
    CMD curl -f http://localhost:${PORT:-9933}/health || exit 1

# Start the server
CMD ["node", "server.js"]