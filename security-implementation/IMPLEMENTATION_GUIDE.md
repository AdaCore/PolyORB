# Security Implementation Guide - Phase 1

**Author**: @CodeArchitect
**Date**: 2025-11-04
**Addresses**: Critical/High Security Findings (#3, #8, #11, #15, #16)
**Status**: READY FOR EXECUTION

## Executive Summary

This guide provides step-by-step instructions for implementing the security controls documented in ADR-002 and RDB-001-Security-Addendum. All artifacts in this directory are production-ready and address the CRITICAL/HIGH security findings from @security_verification.

## Table of Contents

1. [Prerequisites](#prerequisites)
2. [Phase 1A: Security Blockers (Week 1-2)](#phase-1a-security-blockers)
3. [Phase 1B: Infrastructure Security (Week 3-4)](#phase-1b-infrastructure-security)
4. [Phase 1C: CI/CD Security (Week 5-6)](#phase-1c-cicd-security)
5. [Verification & Testing](#verification--testing)
6. [Troubleshooting](#troubleshooting)

---

## Prerequisites

### Tools Required

```bash
# Kubernetes CLI
kubectl version  # ≥1.28

# Istio CLI
istioctl version  # ≥1.20

# Docker with BuildKit
docker buildx version  # ≥0.12

# Security scanning tools
trivy --version  # ≥0.48
grype version  # ≥0.74
syft version  # ≥0.100

# Secret scanning
docker run --rm trufflesecurity/trufflehog:latest --version
```

### Access Required

- Kubernetes cluster with admin permissions
- Container registry (DockerHub, GCR, ECR)
- HashiCorp Vault instance
- GitHub repository with Actions enabled

### Infrastructure

- Kubernetes cluster: ≥3 nodes, 8 CPU, 16GB RAM each
- Istio installed: `istioctl install --set profile=production`
- Vault deployed: `helm install vault hashicorp/vault`

---

## Phase 1A: Security Blockers (Week 1-2)

### Finding #15: Istio mTLS STRICT Mode

**Objective**: Enforce mutual TLS for all service-to-service communication

**Implementation**:

```bash
# Step 1: Apply global mTLS STRICT
kubectl apply -f istio-security-policies.yaml

# Step 2: Verify mTLS status
istioctl x authz check widget-core.wxwidgets

# Expected output:
# LISTENER     CERTIFICATE           mTLS      AUTHZ
# 0.0.0.0:50051  default              STRICT    ALLOW (with policies)

# Step 3: Test plaintext connection (should fail)
kubectl exec -it deploy/widget-core -n wxwidgets -c istio-proxy -- \
  curl http://rendering:50053 -v

# Expected: Connection refused or 503 (mTLS required)
```

**Acceptance Criteria**:
- [ ] Global PeerAuthentication applied
- [ ] Per-namespace PeerAuthentication applied
- [ ] `istioctl x authz check` shows STRICT for all services
- [ ] tcpdump shows zero plaintext gRPC traffic

---

### Finding #16: Zero-Trust Authorization

**Objective**: Implement default-deny with explicit allow-lists for all 256 service connections

**Implementation**:

```bash
# Step 1: Apply default-deny policies
kubectl apply -f istio-security-policies.yaml

# Step 2: Verify policies in place
kubectl get authorizationpolicy -A

# Expected: See deny-all + individual allow policies

# Step 3: Test unauthorized access (should fail)
kubectl exec -it deploy/layout-engine -n wxwidgets -- \
  grpcurl -plaintext widget-core:50051 wxwidgets.WidgetCore/CreateButton

# Expected: Code = PermissionDenied

# Step 4: Test authorized access (should succeed)
kubectl exec -it deploy/event-processing -n wxwidgets -- \
  grpcurl -plaintext widget-core:50051 wxwidgets.WidgetCore/CreateButton

# Expected: Success
```

**Service Connection Matrix** (complete all 256 connections):
- [ ] Event Processing → Widget Core
- [ ] Layout Engine → Widget Core
- [ ] Widget Core → Rendering
- [ ] Rendering → MSW/GTK/Cocoa Adapters
- [ ] GIOP → ORB Core
- [ ] Security → ORB Core
- [ ] Naming → ORB Core
- [ ] Transaction → ORB Core
- [ ] ... (continue for all 16×16 connections)

**Acceptance Criteria**:
- [ ] Default-deny policy applied to all namespaces
- [ ] All 256 service connections documented with AuthorizationPolicy
- [ ] Unauthorized service calls return 403 PERMISSION_DENIED
- [ ] Authorized service calls succeed

---

### Finding #3: API Authentication

**Objective**: Implement JWT authentication for all external APIs

**Implementation**:

```bash
# Step 1: Deploy Kong API Gateway with JWT plugin
helm repo add kong https://charts.konghq.com
helm install kong kong/kong \
  --set ingressController.enabled=true \
  --set proxy.type=LoadBalancer \
  -n api-gateway --create-namespace

# Step 2: Configure JWT plugin
kubectl apply -f - <<EOF
apiVersion: configuration.konghq.com/v1
kind: KongPlugin
metadata:
  name: jwt-auth
  namespace: api-gateway
config:
  key_claim_name: iss
  claims_to_verify:
    - exp
    - aud
  maximum_expiration: 3600
EOF

# Step 3: Build Widget Core with auth interceptor
cd /path/to/wxWidgets
cp /code_architect/security-implementation/auth_interceptor.cc src/security/
cp /code_architect/security-implementation/Dockerfile.widget-core.hardened Dockerfile

# Step 4: Build image
docker buildx build \
  --secret id=conan_token,src=$HOME/.conan/token \
  -t wxwidgets/widget-core:2.0-secure \
  -f Dockerfile \
  .

# Step 5: Deploy with auth
kubectl apply -f k8s-widget-core.yaml

# Step 6: Test unauthenticated request (should fail)
grpcurl -plaintext -d '{"label":"Test"}' \
  widget-core.wxwidgets:50051 \
  wxwidgets.WidgetCore/CreateButton

# Expected: Code = Unauthenticated

# Step 7: Generate test JWT
export TEST_JWT=$(./scripts/generate-test-jwt.sh)

# Step 8: Test authenticated request (should succeed)
grpcurl -H "Authorization: Bearer $TEST_JWT" \
  -d '{"label":"Test"}' \
  widget-core.wxwidgets:50051 \
  wxwidgets.WidgetCore/CreateButton

# Expected: Success with widget_id
```

**JWT Public Key Setup**:

```bash
# Step 1: Generate RSA key pair (do this ONCE)
openssl genrsa -out jwt-private-key.pem 2048
openssl rsa -in jwt-private-key.pem -pubout -out jwt-public-key.pem

# Step 2: Store in Vault
vault kv put secret/wxwidgets/jwt \
  public_key=@jwt-public-key.pem

# Step 3: Configure External Secrets Operator
kubectl apply -f k8s-widget-core.yaml  # Includes SecretProviderClass

# Step 4: Verify secret mounted
kubectl exec -it deploy/widget-core -n wxwidgets -- \
  ls -la /mnt/secrets/
# Expected: jwt-public-key.pem present
```

**Acceptance Criteria**:
- [ ] Kong API Gateway deployed with JWT plugin
- [ ] gRPC interceptor code integrated into all services
- [ ] JWT public key stored in Vault
- [ ] External Secrets Operator syncing secrets
- [ ] Unauthenticated requests return 401
- [ ] Authenticated requests with valid JWT succeed
- [ ] Expired/invalid JWTs rejected

---

### Finding #11: Pod Security Standards

**Objective**: Apply "restricted" Pod Security Standards to all namespaces

**Implementation**:

```bash
# Step 1: Label namespaces
kubectl label namespace wxwidgets \
  pod-security.kubernetes.io/enforce=restricted \
  pod-security.kubernetes.io/enforce-version=latest \
  pod-security.kubernetes.io/audit=restricted \
  pod-security.kubernetes.io/warn=restricted

kubectl label namespace polyorb \
  pod-security.kubernetes.io/enforce=restricted \
  pod-security.kubernetes.io/enforce-version=latest \
  pod-security.kubernetes.io/audit=restricted \
  pod-security.kubernetes.io/warn=restricted

# Step 2: Deploy hardened manifests
kubectl apply -f k8s-widget-core.yaml

# Step 3: Verify pods running as non-root
kubectl exec -it deploy/widget-core -n wxwidgets -- id
# Expected: uid=10001 gid=10001

# Step 4: Verify read-only root filesystem
kubectl exec -it deploy/widget-core -n wxwidgets -- touch /test
# Expected: Read-only file system error

# Step 5: Verify no capabilities
kubectl exec -it deploy/widget-core -n wxwidgets -- capsh --print
# Expected: Current: = (empty set)

# Step 6: Attempt to deploy privileged pod (should fail)
kubectl apply -f - <<EOF
apiVersion: v1
kind: Pod
metadata:
  name: test-privileged
  namespace: wxwidgets
spec:
  containers:
  - name: test
    image: nginx
    securityContext:
      privileged: true
EOF
# Expected: Error - violates PodSecurity "restricted:latest"
```

**Acceptance Criteria**:
- [ ] All namespaces labeled with pod-security.kubernetes.io/enforce=restricted
- [ ] All pods run as UID 10001 (non-root)
- [ ] All pods have readOnlyRootFilesystem: true
- [ ] All pods drop ALL Linux capabilities
- [ ] Privileged pod creation fails
- [ ] No violations in `kubectl get events`

---

### Finding #8: Build-Time Secret Scanning

**Objective**: Prevent secrets from entering Docker images or git history

**Implementation**:

```bash
# Step 1: Add GitHub Actions workflow
mkdir -p .github/workflows
cp /code_architect/security-implementation/.github-workflows-security.yaml \
   .github/workflows/security.yaml

# Step 2: Configure GitLeaks
cat > .gitleaks.toml <<EOF
[extend]
useDefault = true

[[rules]]
id = "generic-api-key"
description = "Generic API Key"
regex = '''(?i)(api[_-]?key|apikey)['":\s]*[=:]\s*['"][0-9a-zA-Z]{32,}['"]'''

[allowlist]
paths = [
  '''\.git/''',
  '''node_modules/''',
]
EOF

# Step 3: Run local scan before committing
docker run --rm -v $(pwd):/repo \
  trufflesecurity/trufflehog:latest \
  filesystem /repo --fail

docker run --rm -v $(pwd):/repo \
  zricethezav/gitleaks:latest \
  detect --source /repo --verbose

# Step 4: Scan Docker image history
docker history wxwidgets/widget-core:2.0-secure --no-trunc | \
  grep -iE "password|secret|token|key|credential"
# Expected: No matches (exit code 1)

# Step 5: Push code (triggers GitHub Actions)
git add .
git commit -m "Add security scanning"
git push

# Step 6: Verify GitHub Actions runs all security scans
# Visit: https://github.com/heathdorn00/wxWidgets/actions
# Expected: All security jobs pass (green checkmarks)
```

**Acceptance Criteria**:
- [ ] TruffleHog scan passes (exit code 0)
- [ ] GitLeaks scan passes (exit code 0)
- [ ] Docker history contains no secrets
- [ ] GitHub Actions security workflow runs on every push
- [ ] CI fails if secrets detected

---

## Phase 1B: Infrastructure Security (Week 3-4)

### Container Image Hardening

**Objective**: Build minimal, hardened container images

```bash
# Step 1: Build with hardened Dockerfile
docker buildx build \
  --secret id=conan_token,src=$HOME/.conan/token \
  --platform linux/amd64,linux/arm64 \
  -t wxwidgets/widget-core:2.0-secure \
  -f Dockerfile.widget-core.hardened \
  --push \
  .

# Step 2: Generate SBOM
syft wxwidgets/widget-core:2.0-secure -o cyclonedx-json > sbom.json

# Step 3: Scan for CVEs
trivy image --severity HIGH,CRITICAL \
  --exit-code 1 \
  wxwidgets/widget-core:2.0-secure

grype sbom:sbom.json --fail-on high

# Step 4: Verify image size
docker images wxwidgets/widget-core:2.0-secure
# Expected: <200MB (distroless base)

# Step 5: Verify non-root user
docker run --rm wxwidgets/widget-core:2.0-secure id
# Expected: uid=10001 gid=10001
```

**Acceptance Criteria**:
- [ ] Multi-stage Dockerfile with distroless runtime
- [ ] Image size <200MB
- [ ] SBOM generated (CycloneDX format)
- [ ] Zero HIGH/CRITICAL CVEs
- [ ] Runs as UID 10001
- [ ] No secrets in image layers

---

## Phase 1C: CI/CD Security (Week 5-6)

### GitHub Actions Security Pipeline

**Objective**: Automated security scanning on every commit

```bash
# Step 1: Verify workflow file in place
cat .github/workflows/security.yaml

# Step 2: Set GitHub secrets
gh secret set CONAN_TOKEN --body "your-conan-token"
gh secret set FOSSA_API_KEY --body "your-fossa-key"

# Step 3: Trigger workflow
git commit --allow-empty -m "Test security workflow"
git push

# Step 4: Monitor workflow
gh run watch

# Step 5: Review results
gh run view --log

# Expected jobs:
# ✓ secret-scan (TruffleHog, GitLeaks)
# ✓ sast-scan (Cppcheck, Clang-Tidy, Semgrep)
# ✓ container-scan (Trivy, Grype, Dockle)
# ✓ sbom-scan (Syft, Grype)
# ✓ dependency-scan (Conan, GitHub Dependency Review)
# ✓ policy-check (Conftest, OPA)
# ✓ scorecard (OpenSSF Scorecard)
```

**Acceptance Criteria**:
- [ ] All 9 security jobs defined in workflow
- [ ] Workflow runs on push/PR
- [ ] HIGH/CRITICAL findings fail the build
- [ ] Security report posted to PR
- [ ] SARIF uploaded to GitHub Security tab

---

## Verification & Testing

### End-to-End Security Validation

Run this comprehensive test suite to verify all security controls:

```bash
#!/bin/bash
# security-validation.sh

set -e

echo "=== Security Validation Suite ==="

# Test 1: mTLS STRICT
echo "[1/10] Testing mTLS STRICT enforcement..."
istioctl x authz check widget-core.wxwidgets | grep "STRICT"

# Test 2: Zero-Trust AuthZ
echo "[2/10] Testing zero-trust authorization..."
kubectl exec -it deploy/layout-engine -n wxwidgets -- \
  grpcurl -plaintext widget-core:50051 list 2>&1 | grep "PermissionDenied"

# Test 3: JWT Authentication
echo "[3/10] Testing JWT authentication..."
grpcurl -plaintext widget-core.wxwidgets:50051 \
  wxwidgets.WidgetCore/CreateButton 2>&1 | grep "Unauthenticated"

# Test 4: Pod Security Standards
echo "[4/10] Testing Pod Security Standards..."
kubectl get ns wxwidgets -o jsonpath='{.metadata.labels}' | grep "pod-security.*restricted"

# Test 5: Non-root execution
echo "[5/10] Testing non-root user..."
kubectl exec -it deploy/widget-core -n wxwidgets -- id | grep "uid=10001"

# Test 6: Read-only filesystem
echo "[6/10] Testing read-only root filesystem..."
kubectl exec -it deploy/widget-core -n wxwidgets -- touch /test 2>&1 | grep "Read-only"

# Test 7: No capabilities
echo "[7/10] Testing dropped capabilities..."
kubectl exec -it deploy/widget-core -n wxwidgets -- capsh --print | grep "Current: ="

# Test 8: Secret scanning
echo "[8/10] Testing secret scanning..."
docker run --rm -v $(pwd):/repo trufflesecurity/trufflehog:latest filesystem /repo --fail

# Test 9: CVE scanning
echo "[9/10] Testing CVE scanning..."
trivy image --severity HIGH,CRITICAL --exit-code 1 wxwidgets/widget-core:2.0-secure

# Test 10: SBOM validation
echo "[10/10] Testing SBOM completeness..."
grype sbom:sbom.json --fail-on high

echo "=== ✓ All security validations passed ==="
```

**Run validation**:
```bash
chmod +x security-validation.sh
./security-validation.sh
```

---

## Troubleshooting

### Common Issues

#### Issue 1: mTLS connection failures

**Symptom**: Services cannot communicate, seeing "503 upstream connect error"

**Diagnosis**:
```bash
# Check mTLS status
istioctl proxy-status

# View Envoy config
istioctl proxy-config cluster deploy/widget-core -n wxwidgets

# Check for mTLS conflicts
kubectl get peerauthentication -A
```

**Solution**:
```bash
# Ensure consistent mTLS mode
kubectl delete peerauthentication --all -n wxwidgets
kubectl apply -f istio-security-policies.yaml
```

---

#### Issue 2: JWT validation failing

**Symptom**: Valid JWTs rejected with "Invalid signature"

**Diagnosis**:
```bash
# Check public key mounted
kubectl exec -it deploy/widget-core -n wxwidgets -- \
  cat /mnt/secrets/jwt-public-key.pem

# Verify JWT structure
echo $JWT | jwt decode -
```

**Solution**:
```bash
# Ensure RS256 algorithm (not HS256)
# Verify Vault secret synced
kubectl get secret widget-core-secrets -n wxwidgets -o yaml
```

---

#### Issue 3: Pod Security violations

**Symptom**: Pods stuck in Pending state

**Diagnosis**:
```bash
kubectl get events -n wxwidgets | grep "violates PodSecurity"
kubectl describe pod <pod-name> -n wxwidgets
```

**Solution**:
```bash
# Fix securityContext in deployment
# Ensure all fields match k8s-widget-core.yaml template
kubectl apply -f k8s-widget-core.yaml
```

---

#### Issue 4: Secret scanning false positives

**Symptom**: CI failing on test fixtures

**Diagnosis**:
```bash
# Check what triggered the failure
cat gitleaks-report.json
```

**Solution**:
```bash
# Add to .gitleaks.toml allowlist
[allowlist]
paths = [
  '''test/fixtures/''',
  '''test/testdata/''',
]
```

---

## Rollback Procedures

If security controls cause service disruptions:

### Emergency Rollback

```bash
# 1. Revert to permissive mTLS
kubectl apply -f - <<EOF
apiVersion: security.istio.io/v1beta1
kind: PeerAuthentication
metadata:
  name: default-mtls-permissive
  namespace: istio-system
spec:
  mtls:
    mode: PERMISSIVE  # Allow both mTLS and plaintext
EOF

# 2. Remove default-deny policies
kubectl delete authorizationpolicy deny-all -n wxwidgets
kubectl delete authorizationpolicy deny-all -n polyorb

# 3. Rollback deployment
kubectl rollout undo deployment/widget-core -n wxwidgets

# 4. Verify service restored
kubectl rollout status deployment/widget-core -n wxwidgets
```

### Gradual Re-Enable

```bash
# After rollback, re-enable security controls one at a time:
# Week 1: mTLS PERMISSIVE → STRICT (test for 48 hours)
# Week 2: Add default-deny + basic AuthZ policies (test)
# Week 3: Enable JWT auth for 10% traffic (canary)
# Week 4: Enable JWT auth for 100% traffic
```

---

## Success Criteria - Checklist

Phase 1 security implementation is complete when:

**Finding #15 (mTLS STRICT)**:
- [ ] Global PeerAuthentication with STRICT mode
- [ ] Per-namespace PeerAuthentication applied
- [ ] Zero plaintext gRPC traffic (tcpdump verification)
- [ ] `istioctl x authz check` shows STRICT for all services

**Finding #16 (Zero-Trust)**:
- [ ] Default-deny AuthorizationPolicy in all namespaces
- [ ] All 256 service connections documented
- [ ] Unauthorized calls return 403 PERMISSION_DENIED
- [ ] Trust zones enforced (no backwards transitions)

**Finding #3 (API Authentication)**:
- [ ] Kong API Gateway with JWT plugin deployed
- [ ] gRPC interceptors integrated in all services
- [ ] JWT public keys in Vault
- [ ] Unauthenticated requests return 401
- [ ] RBAC roles enforced

**Finding #11 (Pod Security Standards)**:
- [ ] "restricted" PSS on all namespaces
- [ ] All pods run as UID 10001
- [ ] All pods have readOnlyRootFilesystem
- [ ] All pods drop ALL capabilities
- [ ] Privileged pod creation blocked

**Finding #8 (Secret Scanning)**:
- [ ] TruffleHog + GitLeaks in CI/CD
- [ ] Docker history contains no secrets
- [ ] GitHub Actions security workflow green
- [ ] Vault integrated with External Secrets Operator

**Overall**:
- [ ] security-validation.sh passes 10/10 tests
- [ ] @security_verification sign-off received
- [ ] Zero HIGH/CRITICAL CVEs in production
- [ ] P95 latency ≤505ms (auth overhead <5%)

---

## Contacts & Support

**Security Questions**: @security_verification
**Architecture Questions**: @CodeArchitect
**Implementation Support**: @code_refactor
**Testing Support**: @test_stabilize

**Documentation**:
- ADR-002: API Security and Authentication
- RDB-001-Security-Addendum
- Istio Security: https://istio.io/latest/docs/concepts/security/

---

**Version**: 1.0
**Last Updated**: 2025-11-04
**Next Review**: After Phase 1 completion
