#!/bin/bash

# IndraChain Health Check Script
# Usage: ./health-check.sh [environment]

set -e

ENVIRONMENT=${1:-staging}

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

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

# Get Railway service URLs
get_service_url() {
    local service_name=$1
    railway service url "$service_name" 2>/dev/null || echo ""
}

# Check HTTP endpoint
check_endpoint() {
    local name=$1
    local url=$2
    local expected_status=${3:-200}
    
    if [ -z "$url" ]; then
        log_warning "$name: No URL available (service might not expose public endpoint)"
        return 1
    fi
    
    log_info "Checking $name: $url"
    
    local response=$(curl -s -w "HTTPSTATUS:%{http_code}" "$url/health" 2>/dev/null || echo "HTTPSTATUS:000")
    local status=$(echo "$response" | tr -d '\n' | sed -e 's/.*HTTPSTATUS://')
    
    if [ "$status" -eq "$expected_status" ]; then
        log_success "$name is healthy (HTTP $status)"
        return 0
    else
        log_error "$name health check failed (HTTP $status)"
        return 1
    fi
}

# Check service with custom path
check_endpoint_custom() {
    local name=$1
    local url=$2
    local path=$3
    local expected_status=${4:-200}
    
    if [ -z "$url" ]; then
        log_warning "$name: No URL available"
        return 1
    fi
    
    local full_url="$url$path"
    log_info "Checking $name: $full_url"
    
    local response=$(curl -s -w "HTTPSTATUS:%{http_code}" "$full_url" 2>/dev/null || echo "HTTPSTATUS:000")
    local status=$(echo "$response" | tr -d '\n' | sed -e 's/.*HTTPSTATUS://')
    
    if [ "$status" -eq "$expected_status" ]; then
        log_success "$name is healthy (HTTP $status)"
        return 0
    else
        log_error "$name health check failed (HTTP $status)"
        return 1
    fi
}

# Test API functionality
test_api_functionality() {
    local api_url=$1
    
    if [ -z "$api_url" ]; then
        log_warning "API Gateway URL not available - skipping functionality tests"
        return
    fi
    
    log_info "Testing API functionality..."
    
    # Test node status endpoint
    local status_response=$(curl -s "$api_url/api/status" 2>/dev/null || echo "")
    if echo "$status_response" | jq . >/dev/null 2>&1; then
        local node_count=$(echo "$status_response" | jq '.connected_peers // 0')
        log_success "Node status API working - Connected peers: $node_count"
    else
        log_warning "Node status API not responding with valid JSON"
    fi
    
    # Test health endpoint
    if check_endpoint_custom "API Health" "$api_url" "/health"; then
        log_success "API Gateway health endpoint working"
    fi
}

# Test UMPF analyzer
test_umpf_analyzer() {
    local umpf_url=$1
    
    if [ -z "$umpf_url" ]; then
        log_warning "UMPF Analyzer URL not available - skipping tests"
        return
    fi
    
    log_info "Testing UMPF Pattern Analyzer..."
    
    # Test patterns endpoint
    local patterns_response=$(curl -s "$umpf_url/patterns" 2>/dev/null || echo "")
    if echo "$patterns_response" | jq . >/dev/null 2>&1; then
        local pattern_count=$(echo "$patterns_response" | jq '.patterns | length // 0')
        log_success "UMPF patterns endpoint working - Discovered patterns: $pattern_count"
    else
        log_warning "UMPF patterns endpoint not responding with valid JSON"
    fi
}

# Check database connectivity
check_database_health() {
    log_info "Checking database connectivity through API..."
    
    # Get API Gateway URL
    local api_url=$(get_service_url "api-gateway")
    
    if [ -n "$api_url" ]; then
        local db_status=$(curl -s "$api_url/api/database/health" 2>/dev/null || echo "")
        if echo "$db_status" | grep -q "healthy\|ok\|connected"; then
            log_success "Database connectivity confirmed through API"
        else
            log_warning "Database health check inconclusive"
        fi
    fi
}

# Main health check function
main() {
    log_info "IndraChain Health Check - Environment: $ENVIRONMENT"
    echo
    
    # Set Railway environment
    if command -v railway &> /dev/null; then
        railway environment use "$ENVIRONMENT" 2>/dev/null || log_warning "Could not set Railway environment"
    fi
    
    local overall_status=0
    
    # Get service URLs
    log_info "Fetching service URLs from Railway..."
    local api_gateway_url=$(get_service_url "api-gateway")
    local monitoring_url=$(get_service_url "monitoring")
    local umpf_analyzer_url=$(get_service_url "umpf-analyzer")
    
    # Core service health checks
    log_info "Checking core services..."
    
    if ! check_endpoint "API Gateway" "$api_gateway_url"; then
        overall_status=1
    fi
    
    if ! check_endpoint_custom "Monitoring (Grafana)" "$monitoring_url" "/api/health"; then
        overall_status=1
    fi
    
    if ! check_endpoint "UMPF Analyzer" "$umpf_analyzer_url"; then
        overall_status=1
    fi
    
    echo
    
    # Functional tests
    log_info "Running functional tests..."
    test_api_functionality "$api_gateway_url"
    test_umpf_analyzer "$umpf_analyzer_url"
    check_database_health
    
    echo
    
    # Summary
    if [ $overall_status -eq 0 ]; then
        log_success "All critical services are healthy!"
        echo
        echo "Service URLs:"
        [ -n "$api_gateway_url" ] && echo "  API Gateway: $api_gateway_url"
        [ -n "$monitoring_url" ] && echo "  Monitoring: $monitoring_url"
        [ -n "$umpf_analyzer_url" ] && echo "  UMPF Analyzer: $umpf_analyzer_url"
        echo
        log_info "Next steps:"
        echo "  - View logs: railway logs"
        echo "  - Monitor metrics: Open monitoring URL above"
        echo "  - Test API: curl $api_gateway_url/api/status"
        echo "  - Explore patterns: curl $umpf_analyzer_url/patterns"
    else
        log_error "Some services are not healthy. Check Railway dashboard for details."
        exit 1
    fi
}

# Run main function
main "$@"