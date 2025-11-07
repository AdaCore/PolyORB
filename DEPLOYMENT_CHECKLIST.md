# Deployment Checklist - Pilot Services

## Current Status

**Environment Analysis** (2025-11-06):
- ✅ macOS Darwin 24.6.0
- ✅ Homebrew installed: `/opt/homebrew/bin/brew`
- ❌ Docker not installed (requires sudo/manual installation)
- ❌ kubectl not installed (included with Docker Desktop)
- ✅ All source code ready (widget-core, orb-core, xrc-service)
- ✅ Build scripts created and executable
- ✅ Dockerfiles ready
- ✅ K8s manifests available (138 files in k8s/)

**Blocker**: Docker Desktop installation requires:
1. Manual download OR sudo password for Homebrew installation
2. User interaction to launch Docker Desktop app
3. Docker daemon must be running

## Step 1: Install Docker Desktop

### Option A: Manual Installation (Recommended)

```bash
# Download Docker Desktop for Apple Silicon
open "https://desktop.docker.com/mac/main/arm64/Docker.dmg"

# Or for Intel Mac:
# open "https://desktop.docker.com/mac/main/amd64/Docker.dmg"

# Steps:
# 1. Mount the DMG
# 2. Drag Docker.app to Applications
# 3. Open Docker.app from Applications
# 4. Accept license agreement
# 5. Wait for Docker to start (whale icon in menu bar)
```

### Option B: Homebrew with Manual Sudo

```bash
# Install Docker Desktop
brew install --cask docker

# When prompted for password, enter it

# Launch Docker Desktop
open -a Docker

# Wait for Docker to fully start
```

### Verify Installation

```bash
# Check Docker is running
docker --version
docker info

# Expected output:
# Docker version 27.x.x
# Server: Docker Desktop (running)
```

## Step 2: Run Builds with Security Scan

### Install Trivy (Security Scanner)

```bash
# Install Trivy via Homebrew
brew install aquasecurity/trivy/trivy

# Verify
trivy --version
```

### Execute Builds

```bash
# Navigate to project root
cd "/Users/heathdorn/Documents/Playground/New Folder With Items/Agents/RefactorTeam/code_architect"

# Option A: Build all services with scan
./build-pilot-services.sh --scan

# Option B: Build individually
cd services/widget-core && ./build.sh && cd ../..
cd services/orb-core && ./build.sh && cd ../..
cd services/xrc-service && ./build.sh && cd ../..

# Verify images
docker images | grep -E 'widget-core|orb-core|xrc-service'
```

### Expected Build Times

- widget-core: ~45-90 seconds
- orb-core: ~90-120 seconds (GNAT compiler)
- xrc-service: ~45-90 seconds

### Expected Image Sizes

- widget-core: ~45-55 MB
- orb-core: ~75-85 MB (Ada runtime)
- xrc-service: ~45-55 MB

## Step 3: Push to Registry

### Setup Registry

**Option A: Local Registry (Testing)**

```bash
# Start local registry
docker run -d -p 5000:5000 --name registry registry:2

# Set environment variable
export DOCKER_REGISTRY=localhost:5000

# Push images
./build-pilot-services.sh --push
```

**Option B: Docker Hub**

```bash
# Login
docker login

# Tag and push
export DOCKER_REGISTRY=yourusername
docker tag widget-core:v1.0.0 $DOCKER_REGISTRY/widget-core:v1.0.0
docker push $DOCKER_REGISTRY/widget-core:v1.0.0

docker tag orb-core:v1.0.0 $DOCKER_REGISTRY/orb-core:v1.0.0
docker push $DOCKER_REGISTRY/orb-core:v1.0.0

docker tag xrc-service:v1.0.0 $DOCKER_REGISTRY/xrc-service:v1.0.0
docker push $DOCKER_REGISTRY/xrc-service:v1.0.0
```

**Option C: Private Registry (Production)**

```bash
# Login to private registry
docker login your-registry.io

# Set registry
export DOCKER_REGISTRY=your-registry.io/refactor-team

# Push
./build-pilot-services.sh --push
```

### Verify Push

```bash
# Check registry
docker search $DOCKER_REGISTRY/widget-core
# or
curl -X GET http://localhost:5000/v2/_catalog
```

## Step 4: Setup Kubernetes Cluster

### Option A: Docker Desktop Kubernetes

```bash
# Enable Kubernetes in Docker Desktop:
# 1. Open Docker Desktop
# 2. Settings → Kubernetes
# 3. Enable Kubernetes
# 4. Click "Apply & Restart"
# 5. Wait for "Kubernetes is running" status

# Verify
kubectl version --client
kubectl cluster-info
kubectl get nodes
```

### Option B: Minikube

```bash
# Install minikube
brew install minikube

# Start cluster
minikube start --driver=docker --cpus=4 --memory=8192

# Verify
kubectl get nodes
```

### Option C: Kind (Kubernetes in Docker)

```bash
# Install kind
brew install kind

# Create cluster
kind create cluster --name refactor-dev

# Verify
kubectl cluster-info --context kind-refactor-dev
```

## Step 4: Deploy widget-core to K8s

### Update Image References

```bash
# Set your registry
export DOCKER_REGISTRY=localhost:5000  # or your actual registry

# Update widget-core deployment manifest
cat > k8s/base/services/widget-core/deployment.yaml <<EOF
apiVersion: apps/v1
kind: Deployment
metadata:
  name: widget-core
  labels:
    app: widget-core
    tier: core
spec:
  replicas: 2
  selector:
    matchLabels:
      app: widget-core
  template:
    metadata:
      labels:
        app: widget-core
        tier: core
    spec:
      containers:
      - name: widget-core
        image: ${DOCKER_REGISTRY}/widget-core:v1.0.0
        imagePullPolicy: Always
        ports:
        - containerPort: 50051
          name: grpc
        resources:
          requests:
            memory: "64Mi"
            cpu: "100m"
          limits:
            memory: "128Mi"
            cpu: "500m"
        livenessProbe:
          exec:
            command:
            - /bin/sh
            - -c
            - "pgrep widget-core-service"
          initialDelaySeconds: 10
          periodSeconds: 30
        readinessProbe:
          exec:
            command:
            - /bin/sh
            - -c
            - "pgrep widget-core-service"
          initialDelaySeconds: 5
          periodSeconds: 10
EOF
```

### Deploy

```bash
# Create namespace
kubectl create namespace dev

# Deploy using Kustomize
kubectl apply -k k8s/overlays/dev

# Or deploy base manifests directly
kubectl apply -f k8s/base/services/widget-core/ -n dev
```

## Step 5: Validate Deployment

### Check Pod Status

```bash
# Watch pods come up
kubectl get pods -n dev -w

# Expected output:
# NAME                           READY   STATUS    RESTARTS   AGE
# widget-core-xxxxxxxxxx-xxxxx   1/1     Running   0          30s
# widget-core-xxxxxxxxxx-xxxxx   1/1     Running   0          30s

# Get detailed info
kubectl describe pod -n dev -l app=widget-core

# Check events
kubectl get events -n dev --sort-by=.metadata.creationTimestamp
```

### Check Logs

```bash
# Tail logs from all widget-core pods
kubectl logs -n dev -l app=widget-core --tail=50 -f

# Expected output:
# =====================================
# Widget Core Service v1.0.0
# =====================================
# Port: 50051
# Workers: 4
# Status: RUNNING
# =====================================
# [1] Service heartbeat - healthy
# [2] Service heartbeat - healthy
# [3] Service heartbeat - healthy

# Check specific pod
POD=$(kubectl get pod -n dev -l app=widget-core -o jsonpath='{.items[0].metadata.name}')
kubectl logs -n dev $POD --tail=100
```

### Check Service

```bash
# Get service details
kubectl get svc -n dev widget-core
kubectl describe svc -n dev widget-core

# Check endpoints
kubectl get endpoints -n dev widget-core
```

### Check Resource Usage

```bash
# Pod metrics (requires metrics-server)
kubectl top pod -n dev -l app=widget-core

# Node metrics
kubectl top nodes
```

## Step 6: Run Smoke Tests

### Test 1: Port-Forward and Connectivity

```bash
# Port-forward to local machine
kubectl port-forward -n dev svc/widget-core 50051:50051 &

# Test with netcat
nc -zv localhost 50051

# Expected: Connection successful

# Kill port-forward
pkill -f "kubectl port-forward"
```

### Test 2: Verify Heartbeat

```bash
# Watch logs for heartbeat messages
kubectl logs -n dev -l app=widget-core --tail=20 -f | grep "heartbeat"

# Should see:
# [N] Service heartbeat - healthy (every 5 seconds)

# Count heartbeats over 30 seconds (should be ~6)
timeout 30s kubectl logs -n dev -l app=widget-core -f | grep -c "heartbeat"
```

### Test 3: Pod Restart Test

```bash
# Get initial pod name
POD=$(kubectl get pod -n dev -l app=widget-core -o jsonpath='{.items[0].metadata.name}')
echo "Testing pod: $POD"

# Delete pod (should auto-restart)
kubectl delete pod -n dev $POD

# Watch new pod come up
kubectl get pods -n dev -l app=widget-core -w

# Check new pod logs
sleep 10
NEW_POD=$(kubectl get pod -n dev -l app=widget-core -o jsonpath='{.items[0].metadata.name}')
kubectl logs -n dev $NEW_POD --tail=20

# Expected: New pod starts, heartbeat begins
```

### Test 4: Load Test (Multiple Pods)

```bash
# Scale up
kubectl scale deployment widget-core -n dev --replicas=5

# Watch pods scale
kubectl get pods -n dev -l app=widget-core -w

# Check all pods are running
kubectl get pods -n dev -l app=widget-core

# Check logs from all pods
kubectl logs -n dev -l app=widget-core --tail=10 --prefix=true

# Scale back down
kubectl scale deployment widget-core -n dev --replicas=2
```

### Test 5: Resource Limits

```bash
# Check actual resource usage
kubectl top pod -n dev -l app=widget-core

# Expected:
# NAME                          CPU(cores)   MEMORY(bytes)
# widget-core-xxx-xxx          <100m        <64Mi

# Verify limits are respected
kubectl get pod -n dev -l app=widget-core -o json | jq '.items[].spec.containers[].resources'
```

## Test Results Template

```markdown
### Smoke Test Results - widget-core

**Date**: YYYY-MM-DD
**Tester**: @CodeArchitect
**Environment**: Docker Desktop K8s / Minikube / Kind

| Test | Status | Notes |
|------|--------|-------|
| Pod Deployment | ✅/❌ | 2 pods running in X seconds |
| Heartbeat Logging | ✅/❌ | Heartbeat every 5s as expected |
| Pod Restart | ✅/❌ | Auto-restart working |
| Service Endpoint | ✅/❌ | Port 50051 accessible |
| Resource Limits | ✅/❌ | <64Mi memory, <100m CPU |
| Scale Test | ✅/❌ | Scaled to 5 pods successfully |

**Issues Found**: None / List issues

**Next Steps**:
- Deploy orb-core
- Deploy xrc-service
- Run integration tests
```

## Troubleshooting

### Docker Build Fails

```bash
# Clean Docker cache
docker system prune -a

# Rebuild with no cache
cd services/widget-core
docker build --no-cache -f Dockerfile.minimal -t widget-core:v1.0.0 .

# Check build logs
docker build --progress=plain -f Dockerfile.minimal -t widget-core:v1.0.0 .
```

### Image Pull Errors

```bash
# Check image exists
docker images | grep widget-core

# If using local registry, ensure it's accessible from K8s
kubectl run test --image=localhost:5000/widget-core:v1.0.0 --dry-run=client -o yaml

# For minikube, use minikube docker-env
eval $(minikube docker-env)
# Then rebuild images in minikube's Docker daemon
```

### Pod CrashLoopBackOff

```bash
# Check logs
kubectl logs -n dev <pod-name> --previous

# Check events
kubectl describe pod -n dev <pod-name>

# Common issues:
# - Wrong image tag
# - Missing resources
# - Port conflicts
# - Container cannot start
```

### Service Not Accessible

```bash
# Check service exists
kubectl get svc -n dev widget-core

# Check endpoints
kubectl get endpoints -n dev widget-core

# If empty, check pod labels match service selector
kubectl get pod -n dev -l app=widget-core --show-labels
```

## Success Criteria

**All steps complete when**:
- ✅ Docker installed and running
- ✅ All 3 images built (<2min each)
- ✅ Security scans show no HIGH/CRITICAL vulnerabilities
- ✅ Images pushed to registry
- ✅ K8s cluster running
- ✅ widget-core deployed (2 pods)
- ✅ Pods show "Running" status
- ✅ Heartbeat logs appearing every 5s
- ✅ Service endpoint accessible
- ✅ Resource usage within limits

## Next Steps After Validation

1. Deploy orb-core (Ada service)
2. Deploy xrc-service (HTTP service)
3. Run integration tests between services
4. Set up Prometheus/Grafana monitoring
5. Configure service mesh (Istio)
6. Deploy remaining 13 services

## Files Reference

- Build script: `build-pilot-services.sh`
- Service source: `services/{widget-core,orb-core,xrc-service}/`
- K8s manifests: `k8s/base/services/`
- Deployment guide: `PILOT_SERVICES_BUILD_GUIDE.md`
- K8s guide: `K8S_DEPLOYMENT_README.md`
