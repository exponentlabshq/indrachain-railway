# Monitoring Service Dockerfile for Railway
FROM prom/prometheus:v2.45.0 as prometheus-base

FROM grafana/grafana:10.0.0 as grafana-base

# Multi-stage build for monitoring stack
FROM ubuntu:22.04 as monitoring

# Install system dependencies
RUN apt-get update && apt-get install -y \
    curl \
    wget \
    supervisor \
    nginx \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Create directories
RUN mkdir -p /app/prometheus /app/grafana /app/config /app/data /var/log/supervisor

# Copy Prometheus
COPY --from=prometheus-base /bin/prometheus /usr/local/bin/
COPY --from=prometheus-base /usr/share/prometheus /usr/share/prometheus

# Copy Grafana  
COPY --from=grafana-base /usr/share/grafana /usr/share/grafana
COPY --from=grafana-base /usr/sbin/grafana-server /usr/local/bin/

# Configuration files
COPY config/prometheus.yml /app/config/
COPY config/grafana.ini /app/config/
COPY config/supervisord.conf /etc/supervisor/conf.d/
COPY dashboards/ /app/grafana/dashboards/

# Create grafana user
RUN useradd -r -s /bin/false grafana && \
    chown -R grafana:grafana /app/grafana /usr/share/grafana

# Expose ports
EXPOSE 3000 9090

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=60s \
    CMD curl -f http://localhost:3000/api/health && curl -f http://localhost:9090/-/healthy || exit 1

# Supervisor configuration
COPY config/supervisord.conf /etc/supervisor/conf.d/supervisord.conf

# Environment variables
ENV GF_SECURITY_ADMIN_PASSWORD=admin
ENV GF_INSTALL_PLUGINS=grafana-piechart-panel,grafana-worldmap-panel

# Start supervisor
CMD ["/usr/bin/supervisord", "-c", "/etc/supervisor/conf.d/supervisord.conf"]