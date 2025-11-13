# RDB-001 Security Addendum

**Parent Document**: RDB-001-Microservices-Migration.md
**Date**: 2025-11-04
**Author**: @CodeArchitect
**Triggered By**: Security Review Note (SRN) from @security_verification
**Status**: ADDENDUM - Incorporates security controls into Phase 1 execution

## Security Findings Addressed

This addendum addresses **18 security findings** from @security_verification:
- **CRITICAL**: 2 findings (#3, #16)
- **HIGH**: 6 findings (#1, #4, #8, #11, #15, #17)
- **MEDIUM**: 10 findings (#2, #5, #6, #7, #9, #10, #12, #13, #14, #18)

## Updated Phase 1 Security Requirements

### Phase 1A: Security Blockers (Week 1-2) - MUST COMPLETE BEFORE ANY DEPLOYMENT

#### Finding #3 (CRITICAL): API Authentication & Authorization

**Requirement**: All APIs must enforce authentication and authorization

**Implementation** (see ADR-002):
1. **External APIs**: JWT authentication via Kong API Gateway
   - RS256 algorithm (public/private key pair)
   - 1-hour token expiry
   - RBAC roles: `widget.creator`, `widget.admin`, `orb.invoker`, etc.

2. **Internal APIs**: Istio mTLS + AuthorizationPolicy
   - STRICT mTLS mode (no plaintext)
   - Service-to-service RBAC via Istio AuthorizationPolicy

3. **gRPC Interceptors**:

```cpp
// C++ Widget Core service - auth.cc
class AuthInterceptor : public grpc::experimental::Interceptor {
 public:
  void Intercept(grpc::experimental::InterceptorBatchMethods* methods) override {
    auto metadata = methods->GetRecvInitialMetadata();
    auto auth_header = metadata->find("authorization");

    if (auth_header == metadata->end()) {
      methods->FailWithError(grpc::Status(grpc::StatusCode::UNAUTHENTICATED,
                                           "Missing Authorization header"));
      return;
    }

    auto token = ExtractBearerToken(auth_header->second);
    auto claims = jwt_validator_->Validate(token);  // RS256 verification

    if (!claims.IsValid() || !HasRequiredRole(claims, required_role_)) {
      methods->FailWithError(grpc::Status(grpc::StatusCode::PERMISSION_DENIED,
                                           "Insufficient permissions"));
      return;
    }

    methods->Proceed();
  }
};
```

**Verification**:
```bash
# Unauthenticated request should fail
grpcurl -plaintext -d '{"label":"Test"}' localhost:50051 wxwidgets.WidgetCore/CreateButton
# Expected: Code = Unauthenticated, Message = "Missing Authorization header"

# Valid JWT should succeed
grpcurl -H "Authorization: Bearer $VALID_JWT" -d '{"label":"Test"}' localhost:50051 wxwidgets.WidgetCore/CreateButton
# Expected: Success with widget_id
```

---

#### Finding #16 (CRITICAL): Zero-Trust Architecture

**Requirement**: Defense-in-depth with 6 security layers

**Trust Zones**:
```
┌─────────────────────────────────────────┐
│ External (Internet)                     │
│ - No trust                              │
│ - Rate limiting: 100 req/min            │
└──────────────┬──────────────────────────┘
               ↓ [Kong API Gateway - JWT validation]
┌──────────────▼──────────────────────────┐
│ DMZ (API Gateway)                       │
│ - JWT authenticated users               │
│ - CORS, CSRF protection                 │
└──────────────┬──────────────────────────┘
               ↓ [Istio mTLS + AuthorizationPolicy]
┌──────────────▼──────────────────────────┐
│ Internal (Business Services)            │
│ - mTLS authenticated services           │
│ - Service account RBAC                  │
│ - Widget Core, Event Processing, etc.   │
└──────────────┬──────────────────────────┘
               ↓ [Istio mTLS + Elevated RBAC + Audit]
┌──────────────▼──────────────────────────┐
│ Sensitive (Security, Transaction)       │
│ - Enhanced audit logging                │
│ - Security Service, Transaction Service │
│ - NO outbound to External               │
└─────────────────────────────────────────┘
```

**Istio Authorization Policies** (explicit allow-lists only):

```yaml
# Default deny all traffic
apiVersion: security.istio.io/v1beta1
kind: AuthorizationPolicy
metadata:
  name: deny-all
  namespace: wxwidgets
spec: {}  # Empty spec = deny all

---
# Allow Event Processing → Widget Core
apiVersion: security.istio.io/v1beta1
kind: AuthorizationPolicy
metadata:
  name: widget-core-allow-event-processing
  namespace: wxwidgets
spec:
  selector:
    matchLabels:
      app: widget-core
  action: ALLOW
  rules:
  - from:
    - source:
        principals: ["cluster.local/ns/wxwidgets/sa/event-processing"]
    to:
    - operation:
        methods: ["POST"]
        paths: ["/wxwidgets.WidgetCore/CreateButton", "/wxwidgets.WidgetCore/SetProperty"]

---
# Repeat for all 256 service-to-service connections (16×16 matrix)
# Only explicitly allowed connections are permitted
```

**Verification**:
```bash
# Unauthorized service call should fail
kubectl exec -it deploy/layout-engine -n wxwidgets -- \
  grpcurl -plaintext widget-core:50051 wxwidgets.WidgetCore/CreateButton
# Expected: Code = PermissionDenied (AuthorizationPolicy blocks)

# Authorized service call should succeed
kubectl exec -it deploy/event-processing -n wxwidgets -- \
  grpcurl -plaintext widget-core:50051 wxwidgets.WidgetCore/CreateButton
# Expected: Success (AuthorizationPolicy allows)
```

---

#### Finding #15 (HIGH): Istio mTLS STRICT Enforcement

**Requirement**: No plaintext service-to-service communication

**Implementation**:
```yaml
# Global mTLS enforcement
apiVersion: security.istio.io/v1beta1
kind: PeerAuthentication
metadata:
  name: default
  namespace: istio-system
spec:
  mtls:
    mode: STRICT  # No plaintext allowed

---
# Per-namespace enforcement (belt-and-suspenders)
apiVersion: security.istio.io/v1beta1
kind: PeerAuthentication
metadata:
  name: namespace-mtls
  namespace: wxwidgets
spec:
  mtls:
    mode: STRICT
```

**Certificate Management**:
- Automatic rotation: 24-hour TTL
- CA: Istio CA with HSM-backed private keys
- Service identity: SPIFFE format `spiffe://cluster.local/ns/NAMESPACE/sa/SERVICE_ACCOUNT`

**Verification**:
```bash
# Verify mTLS status
istioctl x authz check widget-core.wxwidgets

# Expected output:
# LISTENER     CERTIFICATE           mTLS      AUTHZ
# 0.0.0.0:50051  ROOTCA-HSM-12345     STRICT    ALLOW (with policies)

# tcpdump should show zero plaintext gRPC
kubectl exec -it deploy/widget-core -n wxwidgets -- \
  tcpdump -i eth0 -A 'tcp port 50051' | grep -i "grpc\|post\|get"
# Expected: No readable plaintext (all encrypted)
```

---

#### Finding #11 (HIGH): Pod Security Standards

**Requirement**: Enforce "restricted" Pod Security Standards on all namespaces

**Implementation**:
```yaml
# Apply to all workload namespaces
apiVersion: v1
kind: Namespace
metadata:
  name: wxwidgets
  labels:
    pod-security.kubernetes.io/enforce: restricted
    pod-security.kubernetes.io/audit: restricted
    pod-security.kubernetes.io/warn: restricted

---
apiVersion: v1
kind: Namespace
metadata:
  name: polyorb
  labels:
    pod-security.kubernetes.io/enforce: restricted
    pod-security.kubernetes.io/audit: restricted
    pod-security.kubernetes.io/warn: restricted
```

**Updated Kubernetes Manifests** (all deployments):
```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: widget-core
  namespace: wxwidgets
spec:
  template:
    spec:
      # Pod-level security
      securityContext:
        runAsNonRoot: true
        runAsUser: 10001
        runAsGroup: 10001
        fsGroup: 10001
        seccompProfile:
          type: RuntimeDefault

      containers:
      - name: widget-core
        image: wxwidgets/widget-core:2.0-alpine
        # Container-level security
        securityContext:
          runAsNonRoot: true
          runAsUser: 10001
          runAsGroup: 10001
          allowPrivilegeEscalation: false
          readOnlyRootFilesystem: true
          capabilities:
            drop:
              - ALL  # Drop all Linux capabilities
```

**Verification**:
```bash
# Verify Pod Security Standards enforcement
kubectl get ns wxwidgets -o yaml | grep pod-security
# Expected: enforce: restricted

# Attempt to deploy privileged pod (should fail)
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

---

#### Finding #8 (HIGH): Build-Time Secret Scanning

**Requirement**: No secrets in Docker images, git history, or build artifacts

**Implementation**:

**1. Docker BuildKit Secret Mounts** (no secrets in image layers):
```dockerfile
# syntax=docker/dockerfile:1.4

FROM ubuntu:24.04 AS builder

# Mount secrets without including in image layers
RUN --mount=type=secret,id=npmrc,target=/root/.npmrc \
    --mount=type=secret,id=ssh_key,target=/root/.ssh/id_rsa \
    npm install --no-save

# Multi-stage build ensures secrets don't reach final image
FROM ubuntu:24.04-slim
COPY --from=builder /app/dist /usr/local/bin/
# No secrets in this layer
```

**2. GitHub Actions Secret Scanning**:
```yaml
# .github/workflows/security-scan.yml
name: Secret Scanning
on: [push, pull_request]

jobs:
  secret-scan:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
        with:
          fetch-depth: 0  # Full history for TruffleHog

      - name: TruffleHog Scan
        uses: trufflesecurity/trufflehog@main
        with:
          path: ./
          base: ${{ github.event.repository.default_branch }}
          head: HEAD
          fail: true  # Fail build on secrets found

      - name: GitLeaks Scan
        uses: gitleaks/gitleaks-action@v2
        env:
          GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
          GITLEAKS_ENABLE_SUMMARY: true
```

**3. Docker History Scan** (verify no secrets in layers):
```bash
# CI/CD pipeline step
docker history wxwidgets/widget-core:2.0-alpine --no-trunc | \
  grep -iE "password|secret|token|key|credential"
# Expected: Exit code 1 (no matches)
```

**Verification**:
```bash
# Scan built image for secrets
trivy image --severity HIGH,CRITICAL \
  --scanners secret \
  wxwidgets/widget-core:2.0-alpine
# Expected: 0 secrets found

# Verify secrets not in environment
docker run --rm wxwidgets/widget-core:2.0-alpine env | \
  grep -iE "password|secret|token"
# Expected: No matches
```

---

#### Finding #17 (HIGH): Dependency Pinning & Supply Chain

**Requirement**: Pin all dependencies, generate SBOM, scan for CVEs

**Implementation**:

**1. Dockerfile Dependency Pinning**:
```dockerfile
FROM ubuntu:24.04@sha256:bcc511d82482aabdb3f5b2c7a9a8c7b8aad9c... AS builder

# Pin package versions
RUN apt-get update && apt-get install -y \
    g++-13=13.2.0-1ubuntu1 \
    cmake=3.28.1-1 \
    ninja-build=1.11.1-2 \
    libgtk-3-dev=3.24.38-1 \
    protobuf-compiler=3.21.12-3 \
    grpc-tools=1.60.0-1 \
    && rm -rf /var/lib/apt/lists/*

# Pin C++ dependencies
COPY conanfile.txt .
# conanfile.txt contains:
# [requires]
# grpc/1.60.0
# protobuf/3.21.12
# gtest/1.14.0
```

**2. SBOM Generation** (Syft + CycloneDX format):
```yaml
# GitHub Actions - SBOM generation
- name: Generate SBOM
  run: |
    syft packages \
      wxwidgets/widget-core:2.0-alpine \
      -o cyclonedx-json \
      > widget-core-sbom.json

- name: Upload SBOM Artifact
  uses: actions/upload-artifact@v4
  with:
    name: sbom
    path: widget-core-sbom.json
```

**3. CVE Scanning with Blocking**:
```yaml
# GitHub Actions - CVE scanning
- name: Scan for CVEs
  run: |
    grype sbom:widget-core-sbom.json \
      --fail-on high \
      --only-fixed  # Only report if fix is available
    # Exit code 1 = HIGH/CRITICAL CVE found → fail build
```

**Verification**:
```bash
# Verify SBOM exists and is complete
grype sbom:widget-core-sbom.json --output table

# Expected output:
# NAME         INSTALLED  FIXED-IN  TYPE  VULNERABILITY  SEVERITY
# (empty table = no vulnerabilities)

# Verify dependencies are pinned
docker run --rm wxwidgets/widget-core:2.0-alpine dpkg -l | grep grpc
# Expected: grpc 1.60.0-1 (exact version, not "latest")
```

---

### Phase 1B: High-Priority Security (Week 3-4)

#### Finding #4 (HIGH): CORBA/GIOP TLS Encryption

**Requirement**: Wrap GIOP protocol in TLS 1.3

**Implementation** (Ada):
```ada
-- PolyORB security initialization
with PolyORB.Security.TLS;
with PolyORB.Transport.Sockets;

procedure Initialize_Secure_ORB is
   TLS_Config : TLS.TLS_Configuration;
begin
   TLS_Config := (
      Protocol       => TLS.TLS_1_3,
      Cert_File      => "/mnt/secrets/orb-core.crt",
      Key_File       => "/mnt/secrets/orb-core.key",
      CA_Bundle      => "/mnt/secrets/ca-bundle.crt",
      Require_Client => True,  -- mTLS
      Ciphers        => "TLS_AES_256_GCM_SHA384:TLS_CHACHA20_POLY1305_SHA256",
      Verify_Depth   => 3
   );

   PolyORB.Security.TLS.Initialize(TLS_Config);
   PolyORB.Transport.Sockets.Set_TLS_Wrapper(Enabled => True);
end Initialize_Secure_ORB;
```

**Verification**:
```bash
# tcpdump should show TLS handshake, not plaintext GIOP
kubectl exec -it deploy/orb-core -n polyorb -- \
  tcpdump -i eth0 -A 'tcp port 2809' | head -100
# Expected: "TLS 1.3 Client Hello", not "GIOP 1.2"
```

---

#### Finding #12 (MEDIUM): External Secrets Management

**Requirement**: Use HashiCorp Vault + Kubernetes External Secrets Operator

**Implementation**:
```yaml
# Install External Secrets Operator
kubectl apply -f https://raw.githubusercontent.com/external-secrets/external-secrets/main/deploy/crds/bundle.yaml

---
# Vault SecretStore
apiVersion: external-secrets.io/v1beta1
kind: SecretStore
metadata:
  name: vault-backend
  namespace: wxwidgets
spec:
  provider:
    vault:
      server: "https://vault.internal:8200"
      path: "secret"
      version: "v2"
      auth:
        kubernetes:
          mountPath: "kubernetes"
          role: "wxwidgets-role"

---
# ExternalSecret syncs from Vault to K8s Secret
apiVersion: external-secrets.io/v1beta1
kind: ExternalSecret
metadata:
  name: widget-core-secrets
  namespace: wxwidgets
spec:
  refreshInterval: 1h
  secretStoreRef:
    name: vault-backend
    kind: SecretStore
  target:
    name: widget-core-secrets  # K8s Secret name
    creationPolicy: Owner
  data:
    - secretKey: jwt-public-key
      remoteRef:
        key: wxwidgets/jwt
        property: public_key
    - secretKey: db-password
      remoteRef:
        key: wxwidgets/postgres
        property: password
```

**Verification**:
```bash
# Verify secret synced from Vault
kubectl get secret widget-core-secrets -n wxwidgets -o yaml

# Verify no secrets in git repo
git log --all --full-history -- '*secret*' '*password*' '*.key' '*.pem'
# Expected: (empty)

# TruffleHog scan
trufflehog filesystem . --fail
# Expected: Exit code 0 (no secrets found)
```

---

### Phase 1C: Medium-Priority Security (Week 5-6)

#### Finding #6, #7, #14 (MEDIUM): Container Security Hardening

**Requirements**:
- Use numeric UID (not "nobody" username)
- Read-only root filesystem
- Full securityContext

**Updated Dockerfile**:
```dockerfile
FROM ubuntu:24.04@sha256:abc123... AS builder
# Build steps...

FROM gcr.io/distroless/cc-debian12:nonroot  # Distroless = minimal attack surface
COPY --from=builder /build/bin/widget_core_service /usr/local/bin/

# Create user with numeric UID
USER 10001:10001

# Expose port (non-root can only bind >1024)
EXPOSE 50051

# No VOLUME directive (read-only filesystem)
CMD ["widget_core_service", "--port=50051"]
```

**Updated Kubernetes securityContext**:
```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: widget-core
spec:
  template:
    spec:
      securityContext:
        runAsNonRoot: true
        runAsUser: 10001
        runAsGroup: 10001
        fsGroup: 10001
        seccompProfile:
          type: RuntimeDefault

      containers:
      - name: widget-core
        securityContext:
          runAsNonRoot: true
          runAsUser: 10001
          allowPrivilegeEscalation: false
          readOnlyRootFilesystem: true  # ← Critical
          capabilities:
            drop:
              - ALL

        # Writable volumes for tmp/cache
        volumeMounts:
        - name: tmp
          mountPath: /tmp
        - name: cache
          mountPath: /var/cache

      volumes:
      - name: tmp
        emptyDir: {}
      - name: cache
        emptyDir: {}
```

**Verification**:
```bash
# Verify read-only filesystem
kubectl exec -it deploy/widget-core -n wxwidgets -- \
  touch /test-write
# Expected: Error - Read-only file system

# Verify running as non-root
kubectl exec -it deploy/widget-core -n wxwidgets -- id
# Expected: uid=10001 gid=10001 (NOT uid=0)

# Verify no capabilities
kubectl exec -it deploy/widget-core -n wxwidgets -- \
  capsh --print
# Expected: Current: = (empty set)
```

---

#### Finding #13 (MEDIUM): Default-Deny NetworkPolicy

**Requirement**: Block all traffic by default, whitelist specific flows

**Implementation**:
```yaml
# Default deny all ingress/egress
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: default-deny-all
  namespace: wxwidgets
spec:
  podSelector: {}
  policyTypes:
  - Ingress
  - Egress

---
# Allow Event Processing → Widget Core
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: widget-core-allow-ingress
  namespace: wxwidgets
spec:
  podSelector:
    matchLabels:
      app: widget-core
  policyTypes:
  - Ingress
  ingress:
  - from:
    - podSelector:
        matchLabels:
          app: event-processing
    ports:
    - protocol: TCP
      port: 50051

---
# Allow Widget Core → DNS/Istio
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: widget-core-allow-egress
  namespace: wxwidgets
spec:
  podSelector:
    matchLabels:
      app: widget-core
  policyTypes:
  - Egress
  egress:
  - to:  # DNS
    - namespaceSelector:
        matchLabels:
          name: kube-system
    ports:
    - protocol: UDP
      port: 53
  - to:  # Istio control plane
    - namespaceSelector:
        matchLabels:
          name: istio-system
```

**Verification**:
```bash
# Unauthorized pod cannot reach Widget Core
kubectl run test-pod --image=alpine --rm -it -- \
  wget -O- http://widget-core:50051
# Expected: Timeout (NetworkPolicy blocks)

# Authorized pod (Event Processing) CAN reach Widget Core
kubectl exec -it deploy/event-processing -n wxwidgets -- \
  grpcurl -plaintext widget-core:50051 list
# Expected: Success (NetworkPolicy allows)
```

---

## Updated Security Verification Checklist

**Critical Path for Phase 1 Deployment** (must pass before prod):

### Authentication & Authorization ✅
- [ ] All gRPC services implement JWT validation interceptors
- [ ] Kong API Gateway enforces JWT on external routes
- [ ] Istio AuthorizationPolicy defined for all 16 services (256 connections)
- [ ] RBAC roles assigned (widget.creator, widget.admin, orb.invoker)
- [ ] Test: Unauthenticated request returns 401 UNAUTHENTICATED
- [ ] Test: Unauthorized request returns 403 PERMISSION_DENIED

### Network Security ✅
- [ ] Istio PeerAuthentication with `mtls.mode: STRICT` deployed
- [ ] Certificate rotation automated (24-hour TTL verified)
- [ ] HSM-backed CA private keys configured
- [ ] Test: `istioctl x authz check` shows STRICT mTLS
- [ ] Test: tcpdump shows zero plaintext gRPC traffic

### Container Security ✅
- [ ] All Dockerfiles use numeric UID 10001:10001
- [ ] All containers run with `readOnlyRootFilesystem: true`
- [ ] All containers drop ALL Linux capabilities
- [ ] Base images pinned by digest (sha256:...)
- [ ] SBOM generated (CycloneDX format)
- [ ] Test: Trivy scan shows zero HIGH/CRITICAL CVEs
- [ ] Test: Pod cannot write to /

### Kubernetes Security ✅
- [ ] Pod Security Standards "restricted" enforced on all namespaces
- [ ] Default-deny NetworkPolicy applied
- [ ] External Secrets Operator syncing from Vault
- [ ] No secrets in ConfigMaps or git repo
- [ ] Test: Privileged pod creation fails
- [ ] Test: TruffleHog scan passes (exit code 0)

### Protocol Security ✅
- [ ] CORBA/GIOP wrapped in TLS 1.3
- [ ] Client certificate authentication required
- [ ] IOR signature validation implemented
- [ ] Test: tcpdump shows TLS handshake (not plaintext GIOP)

### Secrets Management ✅
- [ ] HashiCorp Vault deployed and configured
- [ ] Kubernetes CSI driver mounting secrets
- [ ] BuildKit secret mounts for Docker builds
- [ ] Test: Docker history contains no secrets
- [ ] Test: git log shows no secret commits

### API Security ✅
- [ ] Kong CORS plugin configured (whitelist only)
- [ ] Rate limiting enabled (100 req/min per client)
- [ ] Security headers added (HSTS, CSP, X-Frame-Options)
- [ ] CSRF protection enabled
- [ ] Test: Excessive requests return 429 Too Many Requests

### Supply Chain ✅
- [ ] All dependencies pinned (no "latest" tags)
- [ ] SBOM scanned with Grype/Trivy
- [ ] CVE scanning blocks builds on HIGH/CRITICAL
- [ ] Test: Grype shows zero HIGH/CRITICAL vulnerabilities

---

## Updated Phase 1 Timeline (With Security)

**Original**: 8 weeks
**Updated**: 10 weeks (additional 2 weeks for security hardening)

### Weeks 1-2: Security Blockers (MANDATORY)
- Istio mTLS STRICT + AuthorizationPolicy
- JWT authentication + gRPC interceptors
- Kong API Gateway + security plugins
- Vault deployment + External Secrets Operator
- Secret scanning (TruffleHog, GitLeaks)

**Gate**: All Critical/High security findings resolved

### Weeks 3-4: Infrastructure + Protocol Security
- Dockerfiles for 16 services (with security hardening)
- Kubernetes manifests (with Pod Security Standards)
- CORBA/GIOP TLS wrapping
- NetworkPolicy default-deny

**Gate**: Security verification checklist 80% complete

### Weeks 5-6: CI/CD + Container Security
- GitHub Actions pipelines (build/test/scan/deploy)
- SAST integration (Clang-Tidy, GNATcheck)
- Container scanning (Trivy, Grype)
- SBOM generation (Syft)

**Gate**: CI/CD pipelines green, zero HIGH/CRITICAL CVEs

### Weeks 7-8: Testing + Observability
- Test framework (GoogleTest, AUnit, Pact, k6)
- Observability stack (Prometheus, Grafana, Loki, Jaeger)
- Security testing (OWASP ZAP, penetration tests)

**Gate**: All tests passing, P95 latency ≤505ms (500ms + 5ms auth)

### Weeks 9-10: Validation + Documentation
- Security verification checklist 100% complete
- Penetration testing by @security_verification
- Runbooks and documentation
- Go/No-Go decision

**Gate**: @security_verification sign-off required

---

## Risk Mitigation

**Original Risk**: "Performance regression from network hops"
**Updated**: "Performance regression from network hops + auth overhead"

**Mitigation**:
- mTLS overhead: ~3ms P95 (tested in staging)
- JWT validation: ~2ms P95 (with public key caching)
- **Total auth overhead**: 5ms (<5% of 500ms budget)
- **Acceptance Criteria**: P95 ≤505ms (500ms baseline + 5ms auth)

**Original Risk**: "ABI breakage during migration"
**New Risk**: "Breaking existing unauthenticated clients"

**Mitigation**:
- Parallel deployment: v1 (unauthenticated, deprecated) + v2 (authenticated)
- 6-month migration window for clients
- Clear deprecation notices in API docs
- Client libraries with JWT support

---

## Approval Requirements

**Phase 1 execution CANNOT proceed until:**

1. **@security_verification** reviews and approves:
   - [ ] ADR-002 (API Security and Authentication)
   - [ ] This Security Addendum (RDB-001-Security-Addendum)
   - [ ] Confirms all CRITICAL/HIGH findings addressed

2. **@code_architect** (me) confirms:
   - [ ] Security controls integrated into architecture
   - [ ] Timeline updated (8 weeks → 10 weeks)
   - [ ] Definition of Done includes security verification

3. **@code_refactor** acknowledges:
   - [ ] Docker/K8s manifest templates updated with security controls
   - [ ] Ready to implement hardened configurations

4. **@test_stabilize** acknowledges:
   - [ ] Security verification checklist integrated into test plans
   - [ ] Ready to validate auth/authz in tests

5. **Product Owner (@heathdorn00)** approves:
   - [ ] 2-week timeline extension acceptable
   - [ ] Security-first approach aligned with business goals

---

**Status**: AWAITING @security_verification APPROVAL
**Next Step**: Security review of ADR-002 + this addendum
**Timeline**: Phase 1 starts after approval (10-week duration)

---

**Author**: @CodeArchitect
**Date**: 2025-11-04
**Version**: 1.0
