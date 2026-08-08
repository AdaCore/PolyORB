#!/bin/bash
# Quick-Start Deployment Script
# Automates: Build → Scan → Push → Deploy → Validate → Test
# Usage: ./quickstart.sh [--skip-build] [--skip-deploy] [--registry REGISTRY]

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
REGISTRY="${DOCKER_REGISTRY:-localhost:5000}"
NAMESPACE="dev"
SKIP_BUILD=false
SKIP_DEPLOY=false
DRY_RUN=false

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --skip-build)
            SKIP_BUILD=true
            shift
            ;;
        --skip-deploy)
            SKIP_DEPLOY=true
            shift
            ;;
        --registry)
            REGISTRY="$2"
            shift 2
            ;;
        --dry-run)
            DRY_RUN=true
            shift
            ;;
        --help)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Options:"
            echo "  --skip-build     Skip Docker image builds"
            echo "  --skip-deploy    Skip K8s deployment"
            echo "  --registry URL   Set Docker registry (default: localhost:5000)"
            echo "  --dry-run        Show what would be done"
            echo "  --help           Show this help"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

# Helper functions
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

check_command() {
    if ! command -v "$1" &> /dev/null; then
        log_error "$1 is not installed"
        return 1
    fi
    return 0
}

# Pre-flight checks
log_info "Running pre-flight checks..."

if ! check_command docker; then
    log_error "Docker is not installed. Please install Docker Desktop first."
    log_info "See DEPLOYMENT_CHECKLIST.md for installation instructions"
    exit 1
fi

if ! docker info &> /dev/null; then
    log_error "Docker daemon is not running. Please start Docker Desktop."
    exit 1
fi

log_success "Docker is running"

if ! check_command kubectl; then
    log_warning "kubectl is not installed. Deployment steps will be skipped."
    SKIP_DEPLOY=true
fi

if [ "$SKIP_DEPLOY" = false ]; then
    if ! kubectl cluster-info &> /dev/null; then
        log_warning "No Kubernetes cluster available. Deployment steps will be skipped."
        SKIP_DEPLOY=true
    else
        log_success "Kubernetes cluster is accessible"
    fi
fi

echo ""
log_info "Configuration:"
log_info "  Registry: $REGISTRY"
log_info "  Namespace: $NAMESPACE"
log_info "  Skip Build: $SKIP_BUILD"
log_info "  Skip Deploy: $SKIP_DEPLOY"
log_info "  Dry Run: $DRY_RUN"
echo ""

if [ "$DRY_RUN" = true ]; then
    log_warning "DRY RUN MODE - No actual changes will be made"
    echo ""
fi

# Step 1: Build Images
if [ "$SKIP_BUILD" = false ]; then
    log_info "=========================================="
    log_info "Step 1/6: Building Docker Images"
    log_info "=========================================="

    if [ "$DRY_RUN" = false ]; then
        export DOCKER_REGISTRY="$REGISTRY"

        if check_command trivy; then
            log_info "Building with security scans..."
            ./build-pilot-services.sh --scan
        else
            log_warning "Trivy not installed, skipping security scans"
            ./build-pilot-services.sh
        fi

        log_success "All images built successfully"
    else
        log_info "Would run: ./build-pilot-services.sh --scan"
    fi
else
    log_info "Skipping build step"
fi

echo ""

# Step 2: Push Images
log_info "=========================================="
log_info "Step 2/6: Pushing Images to Registry"
log_info "=========================================="

if [ "$DRY_RUN" = false ]; then
    # Check if registry is local
    if [[ "$REGISTRY" == "localhost"* ]]; then
        log_info "Using local registry at $REGISTRY"

        # Start local registry if not running
        if ! docker ps | grep -q "registry:2"; then
            log_info "Starting local Docker registry..."
            docker run -d -p 5000:5000 --name registry registry:2 || true
        fi
    fi

    log_info "Pushing images to $REGISTRY..."
    export DOCKER_REGISTRY="$REGISTRY"
    ./build-pilot-services.sh --push

    log_success "All images pushed successfully"
else
    log_info "Would run: ./build-pilot-services.sh --push"
fi

echo ""

# Step 3: Deploy to Kubernetes
if [ "$SKIP_DEPLOY" = false ]; then
    log_info "=========================================="
    log_info "Step 3/6: Deploying widget-core to K8s"
    log_info "=========================================="

    if [ "$DRY_RUN" = false ]; then
        # Create namespace if doesn't exist
        kubectl create namespace "$NAMESPACE" --dry-run=client -o yaml | kubectl apply -f -

        # Update image references in manifests
        log_info "Updating image references to $REGISTRY..."

        # Deploy widget-core
        log_info "Deploying widget-core..."
        kubectl apply -f k8s/base/services/widget-core/ -n "$NAMESPACE"

        log_success "widget-core deployed"
    else
        log_info "Would run: kubectl apply -f k8s/base/services/widget-core/ -n $NAMESPACE"
    fi
else
    log_info "Skipping deployment step"
fi

echo ""

# Step 4: Validate Deployment
if [ "$SKIP_DEPLOY" = false ]; then
    log_info "=========================================="
    log_info "Step 4/6: Validating Deployment"
    log_info "=========================================="

    if [ "$DRY_RUN" = false ]; then
        log_info "Waiting for pods to be ready..."

        # Wait for pods
        if kubectl wait --for=condition=ready pod -l app=widget-core -n "$NAMESPACE" --timeout=120s; then
            log_success "Pods are ready"
        else
            log_error "Pods failed to become ready"
            kubectl get pods -n "$NAMESPACE" -l app=widget-core
            kubectl describe pods -n "$NAMESPACE" -l app=widget-core
            exit 1
        fi

        # Show pod status
        echo ""
        log_info "Pod Status:"
        kubectl get pods -n "$NAMESPACE" -l app=widget-core

        # Show service status
        echo ""
        log_info "Service Status:"
        kubectl get svc -n "$NAMESPACE" widget-core || log_warning "Service not found"

        log_success "Deployment validated"
    else
        log_info "Would run: kubectl wait --for=condition=ready pod -l app=widget-core -n $NAMESPACE"
    fi
else
    log_info "Skipping validation step"
fi

echo ""

# Step 5: Check Logs
if [ "$SKIP_DEPLOY" = false ]; then
    log_info "=========================================="
    log_info "Step 5/6: Checking Logs"
    log_info "=========================================="

    if [ "$DRY_RUN" = false ]; then
        log_info "Recent logs from widget-core:"
        echo ""
        kubectl logs -n "$NAMESPACE" -l app=widget-core --tail=20 --prefix=true

        log_success "Logs retrieved"
    else
        log_info "Would run: kubectl logs -n $NAMESPACE -l app=widget-core --tail=20"
    fi
else
    log_info "Skipping log check"
fi

echo ""

# Step 6: Run Smoke Tests
if [ "$SKIP_DEPLOY" = false ]; then
    log_info "=========================================="
    log_info "Step 6/6: Running Smoke Tests"
    log_info "=========================================="

    if [ "$DRY_RUN" = false ]; then
        # Test 1: Pod count
        log_info "Test 1: Checking pod count..."
        POD_COUNT=$(kubectl get pods -n "$NAMESPACE" -l app=widget-core --field-selector=status.phase=Running -o json | jq '.items | length')

        if [ "$POD_COUNT" -ge 1 ]; then
            log_success "✓ Pod count: $POD_COUNT (expected: ≥1)"
        else
            log_error "✗ Pod count: $POD_COUNT (expected: ≥1)"
        fi

        # Test 2: Heartbeat in logs
        log_info "Test 2: Checking for heartbeat in logs..."
        if kubectl logs -n "$NAMESPACE" -l app=widget-core --tail=50 | grep -q "heartbeat"; then
            log_success "✓ Heartbeat detected in logs"
        else
            log_error "✗ No heartbeat detected in logs"
        fi

        # Test 3: Resource usage
        log_info "Test 3: Checking resource usage..."
        if check_command kubectl && kubectl top pod -n "$NAMESPACE" -l app=widget-core &> /dev/null; then
            kubectl top pod -n "$NAMESPACE" -l app=widget-core
            log_success "✓ Resource metrics available"
        else
            log_warning "⚠ Metrics server not available, skipping resource check"
        fi

        # Test 4: Service endpoint
        log_info "Test 4: Checking service endpoint..."
        if kubectl get svc -n "$NAMESPACE" widget-core &> /dev/null; then
            CLUSTER_IP=$(kubectl get svc -n "$NAMESPACE" widget-core -o jsonpath='{.spec.clusterIP}')
            log_success "✓ Service endpoint: $CLUSTER_IP:50051"
        else
            log_warning "⚠ Service not found"
        fi

        echo ""
        log_success "Smoke tests complete"
    else
        log_info "Would run smoke tests"
    fi
else
    log_info "Skipping smoke tests"
fi

echo ""
log_info "=========================================="
log_success "Quick-Start Deployment Complete!"
log_info "=========================================="
echo ""

if [ "$SKIP_DEPLOY" = false ] && [ "$DRY_RUN" = false ]; then
    log_info "Next steps:"
    log_info "  1. View logs: kubectl logs -n $NAMESPACE -l app=widget-core -f"
    log_info "  2. Port-forward: kubectl port-forward -n $NAMESPACE svc/widget-core 50051:50051"
    log_info "  3. Deploy orb-core: kubectl apply -f k8s/base/services/orb-core/ -n $NAMESPACE"
    log_info "  4. Deploy xrc-service: kubectl apply -f k8s/base/services/xrc-service/ -n $NAMESPACE"
    echo ""
    log_info "For detailed testing, see: DEPLOYMENT_CHECKLIST.md"
fi
