#!/bin/bash

# IndraChain Railway Deployment Script
# Usage: ./deploy.sh [environment] [service]
# Environment: development, staging, production
# Service: all, indracore, aa-processor, monitoring, umpf-analyzer

set -e

ENVIRONMENT=${1:-staging}
SERVICE=${2:-all}
PROJECT_ROOT=$(dirname "$(dirname "$(realpath "$0")")")

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check dependencies
check_dependencies() {
    log_info "Checking dependencies..."
    
    if ! command -v railway &> /dev/null; then
        log_error "Railway CLI not found. Please install: npm install -g @railway/cli"
        exit 1
    fi
    
    if ! command -v docker &> /dev/null; then
        log_error "Docker not found. Please install Docker."
        exit 1
    fi
    
    if ! command -v git &> /dev/null; then
        log_error "Git not found. Please install Git."
        exit 1
    fi
    
    log_success "Dependencies check passed"
}

# Validate environment
validate_environment() {
    case $ENVIRONMENT in
        development|staging|production)
            log_info "Deploying to: $ENVIRONMENT"
            ;;
        *)
            log_error "Invalid environment: $ENVIRONMENT. Use: development, staging, or production"
            exit 1
            ;;
    esac
}

# Set environment variables
set_environment_vars() {
    log_info "Setting environment variables for $ENVIRONMENT..."
    
    case $ENVIRONMENT in
        development)
            export RAILWAY_ENVIRONMENT=development
            export REPLICAS=1
            export CPU_LIMIT="1"
            export MEMORY_LIMIT="2Gi"
            ;;
        staging)
            export RAILWAY_ENVIRONMENT=staging
            export REPLICAS=3
            export CPU_LIMIT="2"
            export MEMORY_LIMIT="4Gi"
            ;;
        production)
            export RAILWAY_ENVIRONMENT=production
            export REPLICAS=5
            export CPU_LIMIT="4"
            export MEMORY_LIMIT="8Gi"
            ;;
    esac
}

# Login to Railway
railway_login() {
    log_info "Checking Railway authentication..."
    
    if ! railway whoami &> /dev/null; then
        log_warning "Not logged in to Railway. Please login:"
        railway login
    fi
    
    log_success "Railway authentication verified"
}

# Create or select Railway project
setup_railway_project() {
    log_info "Setting up Railway project..."
    
    PROJECT_NAME="indrachain-${ENVIRONMENT}"
    
    # Check if project exists
    if railway projects list | grep -q "$PROJECT_NAME"; then
        log_info "Using existing project: $PROJECT_NAME"
        railway project use "$PROJECT_NAME"
    else
        log_info "Creating new project: $PROJECT_NAME"
        railway project create "$PROJECT_NAME"
    fi
    
    # Set environment
    if ! railway environments list | grep -q "$ENVIRONMENT"; then
        log_info "Creating environment: $ENVIRONMENT"
        railway environment create "$ENVIRONMENT"
    fi
    
    railway environment use "$ENVIRONMENT"
    log_success "Railway project setup complete"
}

# Deploy database services
deploy_databases() {
    log_info "Deploying database services..."
    
    # PostgreSQL
    log_info "Deploying PostgreSQL..."
    railway service create postgres --image postgres:15-alpine
    railway variables set POSTGRES_DB=indrachain POSTGRES_USER=indra --service postgres
    railway variables set "POSTGRES_PASSWORD=$(openssl rand -base64 32)" --service postgres
    
    # Redis
    log_info "Deploying Redis..."
    railway service create redis --image redis:7-alpine
    railway variables set REDIS_MAXMEMORY=256mb REDIS_MAXMEMORY_POLICY=allkeys-lru --service redis
    
    log_success "Database services deployed"
}

# Deploy IndraCore VM services
deploy_indracore() {
    log_info "Deploying IndraCore VM services..."
    
    for i in $(seq 1 $REPLICAS); do
        SERVICE_NAME="indracore-vm-$i"
        log_info "Deploying $SERVICE_NAME..."
        
        railway service create "$SERVICE_NAME" --dockerfile services/indracore-vm/Dockerfile
        
        # Set environment variables
        railway variables set \
            NODE_ID="validator-$i" \
            CONSENSUS_ROLE="validator" \
            NETWORK_PORT=$((9943 + i)) \
            RPC_PORT=$((9932 + i)) \
            --service "$SERVICE_NAME"
            
        if [ "$i" -gt 1 ]; then
            railway variables set BOOTNODE_ADDRESS="indracore-vm-1:9944" --service "$SERVICE_NAME"
        fi
    done
    
    log_success "IndraCore VM services deployed"
}

# Deploy Account Abstraction Processor
deploy_aa_processor() {
    log_info "Deploying Account Abstraction Processor..."
    
    railway service create aa-processor --dockerfile services/aa-processor/Dockerfile
    
    railway variables set \
        MAX_AA_COMPLEXITY=1000 \
        COLLATERAL_THRESHOLD=10000 \
        PROCESSING_THREADS=4 \
        --service aa-processor
        
    # Set validator endpoints
    ENDPOINTS=""
    for i in $(seq 1 $REPLICAS); do
        if [ -n "$ENDPOINTS" ]; then
            ENDPOINTS="${ENDPOINTS},"
        fi
        ENDPOINTS="${ENDPOINTS}indracore-vm-$i:$((9932 + i))"
    done
    railway variables set "VALIDATOR_ENDPOINTS=$ENDPOINTS" --service aa-processor
    
    log_success "Account Abstraction Processor deployed"
}

# Deploy API Gateway
deploy_api_gateway() {
    log_info "Deploying API Gateway..."
    
    railway service create api-gateway --dockerfile services/api-gateway/Dockerfile
    
    railway variables set \
        RATE_LIMIT_PER_MINUTE=1000 \
        CORS_ORIGINS="*" \
        AA_PROCESSOR_ENDPOINT="aa-processor:8080" \
        LOG_LEVEL=INFO \
        --service api-gateway
        
    # Generate JWT secret
    railway variables set "JWT_SECRET=$(openssl rand -base64 64)" --service api-gateway
    
    log_success "API Gateway deployed"
}

# Deploy Monitoring
deploy_monitoring() {
    log_info "Deploying Monitoring services..."
    
    railway service create monitoring --dockerfile services/monitoring/Dockerfile
    
    railway variables set \
        PROMETHEUS_PORT=9090 \
        GRAFANA_PORT=3000 \
        --service monitoring
        
    # Generate Grafana admin password
    railway variables set "GRAFANA_ADMIN_PASSWORD=$(openssl rand -base64 32)" --service monitoring
    
    log_success "Monitoring services deployed"
}

# Deploy UMPF Analyzer
deploy_umpf_analyzer() {
    log_info "Deploying UMPF Pattern Analyzer..."
    
    railway service create umpf-analyzer --dockerfile services/umpf-analyzer/Dockerfile
    
    railway variables set \
        ANALYSIS_INTERVAL=60 \
        PATTERN_DISCOVERY_ENABLED=true \
        EQUIVALENCY_THRESHOLD=0.85 \
        CROSS_DOMAIN_ANALYSIS=true \
        --service umpf-analyzer
        
    log_success "UMPF Analyzer deployed"
}

# Wait for deployment
wait_for_deployment() {
    log_info "Waiting for deployment to complete..."
    
    # Wait for services to be healthy
    local max_attempts=30
    local attempt=1
    
    while [ $attempt -le $max_attempts ]; do
        log_info "Deployment check attempt $attempt/$max_attempts"
        
        if railway status | grep -q "All services are running"; then
            log_success "All services are running!"
            break
        fi
        
        if [ $attempt -eq $max_attempts ]; then
            log_error "Deployment verification timed out"
            exit 1
        fi
        
        sleep 30
        ((attempt++))
    done
}

# Run health checks
health_check() {
    log_info "Running health checks..."
    
    # Get service URLs
    GATEWAY_URL=$(railway service url api-gateway 2>/dev/null || echo "")
    MONITORING_URL=$(railway service url monitoring 2>/dev/null || echo "")
    
    if [ -n "$GATEWAY_URL" ]; then
        if curl -f "$GATEWAY_URL/health" > /dev/null 2>&1; then
            log_success "API Gateway health check passed"
        else
            log_warning "API Gateway health check failed"
        fi
    fi
    
    if [ -n "$MONITORING_URL" ]; then
        if curl -f "$MONITORING_URL/api/health" > /dev/null 2>&1; then
            log_success "Monitoring health check passed"
        else
            log_warning "Monitoring health check failed"
        fi
    fi
}

# Display deployment info
display_info() {
    log_success "Deployment complete!"
    echo
    echo "Environment: $ENVIRONMENT"
    echo "Services deployed:"
    railway services list
    echo
    echo "Service URLs:"
    railway service url api-gateway 2>/dev/null | sed 's/^/  API Gateway: /' || true
    railway service url monitoring 2>/dev/null | sed 's/^/  Monitoring: /' || true
    railway service url umpf-analyzer 2>/dev/null | sed 's/^/  UMPF Analyzer: /' || true
    echo
    log_info "Check service status: railway status"
    log_info "View logs: railway logs"
    log_info "Open dashboard: railway dashboard"
}

# Main deployment function
deploy() {
    log_info "Starting IndraChain deployment to Railway..."
    
    cd "$PROJECT_ROOT"
    
    case $SERVICE in
        all)
            deploy_databases
            deploy_indracore
            deploy_aa_processor
            deploy_api_gateway
            deploy_monitoring
            deploy_umpf_analyzer
            ;;
        indracore)
            deploy_indracore
            ;;
        aa-processor)
            deploy_aa_processor
            ;;
        api-gateway)
            deploy_api_gateway
            ;;
        monitoring)
            deploy_monitoring
            ;;
        umpf-analyzer)
            deploy_umpf_analyzer
            ;;
        databases)
            deploy_databases
            ;;
        *)
            log_error "Invalid service: $SERVICE"
            echo "Available services: all, indracore, aa-processor, api-gateway, monitoring, umpf-analyzer, databases"
            exit 1
            ;;
    esac
    
    wait_for_deployment
    health_check
    display_info
}

# Main script execution
main() {
    log_info "IndraChain Railway Deployment Script"
    log_info "Environment: $ENVIRONMENT"
    log_info "Service: $SERVICE"
    echo
    
    check_dependencies
    validate_environment
    set_environment_vars
    railway_login
    setup_railway_project
    deploy
}

# Run main function
main "$@"