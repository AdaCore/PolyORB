#!/usr/bin/env bash
# Build script for PolyORB Ada microservices with Docker layer caching
# Usage: ./build-polyorb.sh [service-name|all] [--no-cache] [--push]

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0;no Color'

# Configuration
REGISTRY="${DOCKER_REGISTRY:-localhost:5000}"
VERSION="${VERSION:-latest}"
BUILD_PARALLEL="${BUILD_PARALLEL:-4}"

# Service list
SERVICES=(
    "orb-core"
    "giop-protocol"
    "poa-manager"
    "naming-service"
    "event-service"
    "notification-service"
    "interface-repository"
    "soap-gateway"
    "security-service"
)

# Function to print colored messages
log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Function to build a single service
build_service() {
    local service=$1
    local no_cache=$2
    local push=$3

    log_info "Building $service..."

    local cache_from=""
    if [ "$no_cache" != "true" ]; then
        cache_from="--cache-from ${REGISTRY}/${service}:${VERSION}"
    fi

    local build_args=""
    if [ "$no_cache" == "true" ]; then
        build_args="--no-cache"
    fi

    # Build with BuildKit for better caching
    DOCKER_BUILDKIT=1 docker build \
        ${build_args} \
        ${cache_from} \
        -t ${service}:${VERSION} \
        -t ${service}:latest \
        -t ${REGISTRY}/${service}:${VERSION} \
        -t ${REGISTRY}/${service}:latest \
        --build-arg BUILDKIT_INLINE_CACHE=1 \
        -f polyorb-services/${service}/Dockerfile \
        polyorb-services/${service}/ || {
            log_error "Failed to build $service"
            return 1
        }

    log_info "✓ Successfully built $service"

    # Push if requested
    if [ "$push" == "true" ]; then
        log_info "Pushing $service to registry..."
        docker push ${REGISTRY}/${service}:${VERSION}
        docker push ${REGISTRY}/${service}:${VERSION}
        log_info "✓ Pushed $service"
    fi

    return 0
}

# Function to build all services
build_all() {
    local no_cache=$1
    local push=$2
    local failed_services=()

    log_info "Building all ${#SERVICES[@]} PolyORB services..."

    for service in "${SERVICES[@]}"; do
        if ! build_service "$service" "$no_cache" "$push"; then
            failed_services+=("$service")
        fi
    done

    # Summary
    echo ""
    log_info "=========================================="
    log_info "Build Summary"
    log_info "=========================================="
    log_info "Total services: ${#SERVICES[@]}"
    log_info "Successful: $((${#SERVICES[@]} - ${#failed_services[@]}))"

    if [ ${#failed_services[@]} -gt 0 ]; then
        log_error "Failed: ${#failed_services[@]}"
        log_error "Failed services: ${failed_services[*]}"
        return 1
    else
        log_info "All services built successfully! ✓"
    fi

    return 0
}

# Function to show image sizes
show_sizes() {
    log_info "=========================================="
    log_info "Image Sizes"
    log_info "=========================================="
    for service in "${SERVICES[@]}"; do
        if docker image inspect ${service}:latest &>/dev/null; then
            local size=$(docker image inspect ${service}:latest --format='{{.Size}}' | numfmt --to=iec-i --suffix=B)
            printf "%-25s %10s\n" "$service" "$size"
        fi
    done
}

# Function to run Trivy security scans
security_scan() {
    local service=$1

    log_info "Running security scan on $service..."

    if ! command -v trivy &> /dev/null; then
        log_warn "Trivy not installed. Install: https://aquasecurity.github.io/trivy/"
        return 0
    fi

    trivy image --severity HIGH,CRITICAL ${service}:latest
}

# Function to scan all services
security_scan_all() {
    log_info "Running security scans on all services..."

    for service in "${SERVICES[@]}"; do
        echo ""
        security_scan "$service"
    done
}

# Parse arguments
SERVICE="${1:-all}"
NO_CACHE="false"
PUSH="false"
SCAN="false"

shift || true
while [[ $# -gt 0 ]]; do
    case $1 in
        --no-cache)
            NO_CACHE="true"
            shift
            ;;
        --push)
            PUSH="true"
            shift
            ;;
        --scan)
            SCAN="true"
            shift
            ;;
        *)
            log_error "Unknown option: $1"
            exit 1
            ;;
    esac
done

# Main execution
log_info "=========================================="
log_info "PolyORB Ada Microservices Build Script"
log_info "=========================================="
log_info "Registry: $REGISTRY"
log_info "Version: $VERSION"
log_info "No Cache: $NO_CACHE"
log_info "Push: $PUSH"
log_info "=========================================="
echo ""

# Build
if [ "$SERVICE" == "all" ]; then
    build_all "$NO_CACHE" "$PUSH"
    show_sizes
else
    # Check if service exists
    if [[ ! " ${SERVICES[@]} " =~ " ${SERVICE} " ]]; then
        log_error "Unknown service: $SERVICE"
        log_info "Available services: ${SERVICES[*]}"
        exit 1
    fi
    build_service "$SERVICE" "$NO_CACHE" "$PUSH"
fi

# Security scan if requested
if [ "$SCAN" == "true" ]; then
    echo ""
    if [ "$SERVICE" == "all" ]; then
        security_scan_all
    else
        security_scan "$SERVICE"
    fi
fi

log_info "Done! ✓"
