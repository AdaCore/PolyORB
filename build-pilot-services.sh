#!/bin/bash
# Build script for 3 pilot services
# Purpose: Build Docker images for widget-core, orb-core, and xrc-service
# Usage: ./build-pilot-services.sh [--push] [--scan]

set -euo pipefail

REGISTRY="${DOCKER_REGISTRY:-localhost:5000}"
VERSION="v1.0.0"
PUSH=false
SCAN=false

# Parse arguments
for arg in "$@"; do
    case $arg in
        --push)
            PUSH=true
            shift
            ;;
        --scan)
            SCAN=true
            shift
            ;;
        *)
            ;;
    esac
done

echo "=========================================="
echo "Building Pilot Service Images"
echo "=========================================="
echo "Registry: $REGISTRY"
echo "Version: $VERSION"
echo "Push: $PUSH"
echo "Security Scan: $SCAN"
echo "=========================================="

# Function to build a service
build_service() {
    local service_name=$1
    local service_path=$2
    local dockerfile=$3

    echo ""
    echo "Building $service_name..."
    echo "----------------------------------------"

    cd "$service_path"

    # Build image
    docker build \
        -f "$dockerfile" \
        -t "${service_name}:latest" \
        -t "${service_name}:${VERSION}" \
        -t "${REGISTRY}/${service_name}:latest" \
        -t "${REGISTRY}/${service_name}:${VERSION}" \
        --build-arg BUILD_DATE="$(date -u +'%Y-%m-%dT%H:%M:%SZ')" \
        --build-arg VCS_REF="$(git rev-parse --short HEAD 2>/dev/null || echo 'unknown')" \
        .

    echo "✓ Built $service_name"

    # Security scan with Trivy
    if [ "$SCAN" = true ]; then
        echo "Running security scan on $service_name..."
        if command -v trivy &> /dev/null; then
            trivy image --severity HIGH,CRITICAL "${service_name}:latest"
        else
            echo "⚠ Trivy not installed, skipping scan"
        fi
    fi

    # Push to registry
    if [ "$PUSH" = true ]; then
        echo "Pushing $service_name to registry..."
        docker push "${REGISTRY}/${service_name}:latest"
        docker push "${REGISTRY}/${service_name}:${VERSION}"
        echo "✓ Pushed $service_name"
    fi

    cd - > /dev/null
}

# Build widget-core (C++ wxWidgets service)
build_service "widget-core" "services/widget-core" "Dockerfile.minimal"

# Build orb-core (Ada PolyORB service)
build_service "orb-core" "services/orb-core" "Dockerfile.minimal"

# Build xrc-service (C++ HTTP service)
build_service "xrc-service" "services/xrc-service" "Dockerfile.minimal"

echo ""
echo "=========================================="
echo "Build Complete!"
echo "=========================================="
echo "Images built:"
echo "  - widget-core:${VERSION}"
echo "  - orb-core:${VERSION}"
echo "  - xrc-service:${VERSION}"
echo ""
echo "Next steps:"
echo "  1. Run: ./build-pilot-services.sh --scan (security scan)"
echo "  2. Run: ./build-pilot-services.sh --push (push to registry)"
echo "  3. Deploy: kubectl apply -k k8s/overlays/dev"
echo "=========================================="
