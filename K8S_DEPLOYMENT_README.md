# Kubernetes Deployment Guide

Comprehensive deployment guide for wxWidgets and PolyORB microservices on Kubernetes with Helm, Kustomize, and Istio service mesh.

## Table of Contents

1. [Overview](#overview)
2. [Prerequisites](#prerequisites)
3. [Directory Structure](#directory-structure)
4. [Deployment Methods](#deployment-methods)
5. [Configuration](#configuration)
6. [Validation](#validation)
7. [Monitoring](#monitoring)
8. [Troubleshooting](#troubleshooting)

## Overview

This repository contains Kubernetes manifests for deploying 16 microservices:

**wxWidgets Services (7)**:
- widget-core (port 50051, 3 replicas)
- render-manager (port 50052, 3 replicas)
- event-manager (port 50053, 2 replicas)
- windows-adapter (port 50054, 2 replicas)
- macos-adapter (port 50055, 2 replicas)
- linux-adapter (port 50056, 3 replicas)
- xrc-service (port 8080, 2 replicas)

**PolyORB Services (9)**:
- orb-core (port 50060, 3 replicas)
- giop-protocol (port 50061, 2 replicas)
- poa-manager (port 50062, 2 replicas)
- naming-service (port 50063, 2 replicas)
- event-service (port 50064, 2 replicas)
- notification-service (port 50065, 3 replicas)
- interface-repository (port 50066, 2 replicas)
- soap-gateway (port 8081, 2 replicas)
- security-service (ports 50067/8443, 3 replicas)

## Prerequisites

### Required Tools

```bash
# kubectl (v1.28+)
kubectl version --client

# Helm (v3.12+)
helm version

# Kustomize (v5.0+)
kustomize version

# Optional: Istio (v1.20+)
istioctl version
```

### Cluster Requirements

- Kubernetes cluster v1.28+
- Minimum 8 CPU cores, 16GB RAM
- Storage class with dynamic provisioning
- Istio service mesh (optional but recommended)

## Directory Structure

```
.
├── k8s/
│   ├── namespaces/                  # Namespace definitions
│   ├── base/                        # Base manifests
│   │   ├── wxwidgets/              # wxWidgets services
│   │   │   ├── widget-core/
│   │   │   ├── render-manager/
│   │   │   └── ...
│   │   ├── polyorb/                # PolyORB services
│   │   │   ├── orb-core/
│   │   │   ├── security-service/
│   │   │   └── ...
│   │   └── kustomization.yaml      # Base kustomization
│   └── overlays/                    # Environment overlays
│       ├── dev/
│       ├── staging/
│       └── prod/
├── helm/
│   ├── wxwidgets/                   # wxWidgets Helm chart
│   │   ├── Chart.yaml
│   │   ├── values.yaml
│   │   └── templates/
│   └── polyorb/                     # PolyORB Helm chart
│       ├── Chart.yaml
│       ├── values.yaml
│       └── templates/
└── istio/
    ├── wxwidgets-virtualservices.yaml
    ├── wxwidgets-destinationrules.yaml
    ├── polyorb-virtualservices.yaml
    └── polyorb-destinationrules.yaml
```

## Deployment Methods

### Method 1: Using kubectl with Base Manifests

Deploy base manifests directly:

```bash
# Create namespaces
kubectl apply -f k8s/namespaces/

# Deploy wxWidgets services
kubectl apply -f k8s/base/wxwidgets/widget-core/
kubectl apply -f k8s/base/wxwidgets/render-manager/
# ... repeat for all wxWidgets services

# Deploy PolyORB services
kubectl apply -f k8s/base/polyorb/orb-core/
kubectl apply -f k8s/base/polyorb/security-service/
# ... repeat for all PolyORB services
```

### Method 2: Using Kustomize

Deploy with environment-specific overlays:

#### Development Environment

```bash
# Lower resource limits, 1 replica per service
kubectl apply -k k8s/overlays/dev/

# Verify deployment
kubectl get pods -n wxwidgets -l environment=dev
kubectl get pods -n polyorb -l environment=dev
```

#### Staging Environment

```bash
# Moderate resources, 1-2 replicas
kubectl apply -k k8s/overlays/staging/

# Verify deployment
kubectl get pods -n wxwidgets -l environment=staging
kubectl get pods -n polyorb -l environment=staging
```

#### Production Environment

```bash
# Full resources, production replica counts
kubectl apply -k k8s/overlays/prod/

# Verify deployment
kubectl get pods -n wxwidgets -l environment=production
kubectl get pods -n polyorb -l environment=production
```

### Method 3: Using Helm

Deploy using Helm charts:

#### wxWidgets Services

```bash
# Install wxWidgets chart
helm install wxwidgets ./helm/wxwidgets \
  --namespace wxwidgets \
  --create-namespace

# Upgrade existing installation
helm upgrade wxwidgets ./helm/wxwidgets \
  --namespace wxwidgets

# Uninstall
helm uninstall wxwidgets --namespace wxwidgets
```

#### PolyORB Services

```bash
# Install PolyORB chart
helm install polyorb ./helm/polyorb \
  --namespace polyorb \
  --create-namespace

# Upgrade existing installation
helm upgrade polyorb ./helm/polyorb \
  --namespace polyorb

# Uninstall
helm uninstall polyorb --namespace polyorb
```

#### Custom Values

```bash
# Create custom values file
cat > custom-values.yaml <<EOF
global:
  imageRegistry: "myregistry.io/"
  imagePullPolicy: Always

services:
  widget-core:
    replicas: 5
    resources:
      requests:
        memory: "1Gi"
        cpu: "1000m"
EOF

# Deploy with custom values
helm install wxwidgets ./helm/wxwidgets \
  --namespace wxwidgets \
  --create-namespace \
  -f custom-values.yaml
```

## Configuration

### Secrets Management

Before deployment, update secrets in:

1. **API Keys** (all services):
```bash
kubectl create secret generic <service-name>-secrets \
  --from-literal=api_key=YOUR_API_KEY \
  --namespace=<namespace>
```

2. **TLS Certificates** (security-service):
```bash
# Generate self-signed cert for testing
openssl req -x509 -nodes -days 365 -newkey rsa:2048 \
  -keyout tls.key -out tls.crt \
  -subj "/CN=security-service.polyorb.svc.cluster.local"

# Create secrets
kubectl create secret tls security-service-tls-certs \
  --cert=tls.crt \
  --key=tls.key \
  --namespace=polyorb

kubectl create secret generic security-service-tls-private \
  --from-file=tls.key=tls.key \
  --namespace=polyorb
```

### ConfigMap Updates

Update service configurations:

```bash
# Edit ConfigMap
kubectl edit configmap <service-name>-config -n <namespace>

# Or apply changes
kubectl apply -f k8s/base/<namespace>/<service>/configmap.yaml
```

### Resource Limits

Default resource allocations:

| Service | Memory Request | Memory Limit | CPU Request | CPU Limit |
|---------|----------------|--------------|-------------|-----------|
| widget-core | 256Mi | 512Mi | 250m | 500m |
| render-manager | 512Mi | 1Gi | 500m | 1000m |
| orb-core | 512Mi | 1Gi | 500m | 1000m |
| security-service | 256Mi | 512Mi | 250m | 500m |

Adjust in Helm values or Kustomize patches as needed.

## Istio Service Mesh

### Installation

```bash
# Install Istio
istioctl install --set profile=default -y

# Label namespaces for automatic sidecar injection
kubectl label namespace wxwidgets istio-injection=enabled
kubectl label namespace polyorb istio-injection=enabled

# Deploy Istio configurations
kubectl apply -f istio/wxwidgets-virtualservices.yaml
kubectl apply -f istio/wxwidgets-destinationrules.yaml
kubectl apply -f istio/polyorb-virtualservices.yaml
kubectl apply -f istio/polyorb-destinationrules.yaml
```

### Traffic Management

Istio provides:
- **Load balancing**: Round-robin, least-request, consistent hashing
- **Retries**: 3 attempts with 2s per-try timeout
- **Circuit breaking**: 5 consecutive 5xx errors trigger ejection
- **TLS**: mTLS for security-service

### Monitoring Traffic

```bash
# View Istio dashboard
istioctl dashboard kiali

# Check VirtualServices
kubectl get virtualservices -A

# Check DestinationRules
kubectl get destinationrules -A
```

## Validation

### Manifest Validation

Validate manifests before applying:

```bash
# Validate with kubectl dry-run
kubectl apply -f k8s/base/wxwidgets/widget-core/ --dry-run=client

# Validate Kustomize build
kustomize build k8s/overlays/prod/ | kubectl apply --dry-run=server -f -

# Validate Helm charts
helm lint ./helm/wxwidgets
helm lint ./helm/polyorb

# Helm template dry-run
helm template wxwidgets ./helm/wxwidgets | kubectl apply --dry-run=server -f -
```

### Health Checks

All services include:

- **Liveness probe**: Ensures pod is alive (30s delay, 15s period)
- **Readiness probe**: Ensures pod is ready (15s delay, 10s period)
- **Startup probe**: Allows slow start (0s delay, 5s period, 15 failures)

Check health:

```bash
# Check pod status
kubectl get pods -n wxwidgets
kubectl get pods -n polyorb

# Check probe status
kubectl describe pod <pod-name> -n <namespace>

# View logs
kubectl logs <pod-name> -n <namespace>
```

### Horizontal Pod Autoscaling

HPA configured for CPU (70%) and Memory (80%) thresholds:

```bash
# Check HPA status
kubectl get hpa -n wxwidgets
kubectl get hpa -n polyorb

# View HPA details
kubectl describe hpa <service-name>-hpa -n <namespace>
```

## Monitoring

### Prometheus Integration

All services expose Prometheus metrics:

```bash
# Check Prometheus annotations
kubectl get service -n wxwidgets -o yaml | grep prometheus

# Port-forward to service
kubectl port-forward -n wxwidgets svc/widget-core 50051:50051

# Scrape metrics
curl http://localhost:50051/metrics
```

### Logging

View aggregated logs:

```bash
# All pods in namespace
kubectl logs -n wxwidgets -l tier=backend --tail=100 -f

# Specific service
kubectl logs -n polyorb -l app=security-service --tail=100 -f
```

## Troubleshooting

### Common Issues

#### 1. ImagePullBackOff

```bash
# Check image pull secrets
kubectl get pods -n wxwidgets <pod-name> -o yaml | grep imagePullSecrets

# Check image exists
docker pull <image-name>:<tag>

# Add image pull secrets
kubectl create secret docker-registry regcred \
  --docker-server=<registry> \
  --docker-username=<user> \
  --docker-password=<password> \
  --namespace=wxwidgets
```

#### 2. CrashLoopBackOff

```bash
# Check logs
kubectl logs -n wxwidgets <pod-name> --previous

# Check events
kubectl describe pod -n wxwidgets <pod-name>

# Check liveness/readiness probes
kubectl get pod -n wxwidgets <pod-name> -o yaml | grep -A 10 livenessProbe
```

#### 3. Pending Pods

```bash
# Check node resources
kubectl describe nodes

# Check PVC status
kubectl get pvc -n wxwidgets

# Check pod events
kubectl describe pod -n wxwidgets <pod-name>
```

#### 4. Service Not Accessible

```bash
# Check service endpoints
kubectl get endpoints -n wxwidgets <service-name>

# Check network policy
kubectl get networkpolicies -n wxwidgets

# Test connectivity
kubectl run -it --rm debug --image=curlimages/curl --restart=Never -- sh
# Inside pod: curl http://<service-name>.<namespace>.svc.cluster.local:<port>
```

### Rollback

```bash
# Helm rollback
helm rollback wxwidgets 1 --namespace wxwidgets

# kubectl rollout undo
kubectl rollout undo deployment/<service-name> -n wxwidgets

# Kustomize - reapply previous overlay
kubectl apply -k k8s/overlays/prod/
```

### Cleanup

```bash
# Uninstall Helm releases
helm uninstall wxwidgets --namespace wxwidgets
helm uninstall polyorb --namespace polyorb

# Delete Kustomize deployments
kubectl delete -k k8s/overlays/prod/

# Delete namespaces (removes all resources)
kubectl delete namespace wxwidgets
kubectl delete namespace polyorb

# Remove Istio configs
kubectl delete -f istio/
```

## Best Practices

1. **Always validate** manifests with `--dry-run` before applying
2. **Use Kustomize overlays** for environment-specific configurations
3. **Enable Istio** for advanced traffic management and observability
4. **Monitor HPA** to ensure appropriate scaling
5. **Secure secrets** using external secret management (Vault, Sealed Secrets)
6. **Set resource limits** to prevent resource exhaustion
7. **Use network policies** to enforce least-privilege access
8. **Enable audit logging** for security compliance
9. **Implement backup strategies** for stateful services
10. **Test rollbacks** in staging before production deployments

## Support

For issues or questions:
- Review logs: `kubectl logs -n <namespace> <pod-name>`
- Check events: `kubectl get events -n <namespace> --sort-by='.lastTimestamp'`
- Describe resources: `kubectl describe <resource> -n <namespace> <name>`

## References

- [Kubernetes Documentation](https://kubernetes.io/docs/)
- [Helm Documentation](https://helm.sh/docs/)
- [Kustomize Documentation](https://kustomize.io/)
- [Istio Documentation](https://istio.io/latest/docs/)
