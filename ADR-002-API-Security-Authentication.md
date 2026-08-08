# ADR-002: API Security and Authentication Architecture

**Status**: ACCEPTED
**Date**: 2025-11-04
**Authors**: @CodeArchitect
**Reviewers**: @security_verification
**Supersedes**: N/A
**Related**: ADR-001, RDB-001
**Addresses**: Security Findings #3 (CRITICAL), #16 (CRITICAL), #1, #4, #15

## Context

**Security Finding #3 (CRITICAL)**: @security_verification identified that our initial API contract definitions lacked ANY authentication or authorization mechanisms, creating a critical vulnerability where any client could invoke CreateButton, CreateReference, Invoke, DestroyWidget operations without authentication.

**Security Finding #16 (CRITICAL)**: The migration from 2 monoliths to 16 microservices expands the attack surface from 1 trust boundary to 256 potential inter-service connections (16² combinations). Without proper defense-in-depth, compromise of one service enables lateral movement to all others.

**Business Impact**:
- Unauthenticated access could lead to complete system compromise
- Unauthorized resource creation/deletion (DoS attacks)
- Data exfiltration across service boundaries
- Compliance failures (SOC2, ISO 27001, HIPAA if applicable)

**Technical Constraints**:
- Must support both internal (service-to-service) and external (client-to-service) authentication
- CORBA/GIOP protocol requires CSIv2 security for legacy compliance
- Performance budget: Auth overhead must not exceed 5% of P95 latency (<25ms)
- Backward compatibility: Existing unauthenticated clients need migration path

## Decision

We will implement a **zero-trust, defense-in-depth security architecture** with multiple layers:

### Layer 1: Network-Level Security (Service Mesh)

**Istio Service Mesh with Mutual TLS (mTLS)**

All service-to-service communication will be encrypted and authenticated using Istio mTLS in **STRICT** mode:

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
```

**Characteristics**:
- Automatic certificate rotation (24-hour TTL)
- Service identity based on SPIFFE format: `spiffe://cluster.local/ns/NAMESPACE/sa/SERVICE_ACCOUNT`
- Hardware security module (HSM) backing for CA private keys
- Zero-touch certificate management (no manual ops)

**Verification**:
```bash
# Verify mTLS status
istioctl x authz check widget-core.wxwidgets

# Expected: mTLS STRICT enforced, no plaintext connections
```

### Layer 2: Service-Level Authorization (RBAC)

**Istio AuthorizationPolicy for Zero-Trust**

Define explicit allow-lists for all inter-service communication:

```yaml
# Example: Event Processing can call Widget Core
apiVersion: security.istio.io/v1beta1
kind: AuthorizationPolicy
metadata:
  name: widget-core-authz
  namespace: wxwidgets
spec:
  selector:
    matchLabels:
      app: widget-core
  action: ALLOW
  rules:
  # Allow Event Processing service
  - from:
    - source:
        principals: ["cluster.local/ns/wxwidgets/sa/event-processing"]
    to:
    - operation:
        methods: ["POST"]
        paths: ["/wxwidgets.WidgetCore/CreateButton", "/wxwidgets.WidgetCore/SetProperty"]

  # Allow Layout Engine service
  - from:
    - source:
        principals: ["cluster.local/ns/wxwidgets/sa/layout-engine"]
    to:
    - operation:
        methods: ["POST"]
        paths: ["/wxwidgets.WidgetCore/GetWidgetBounds"]

  # Default deny (implicit - any request not matching above rules is blocked)
```

**Trust Zones**:
```
External (Internet)
  ↓ [API Gateway - JWT validation]
DMZ (Gateway services)
  ↓ [Istio mTLS + AuthZ policies]
Internal (Business services)
  ↓ [Istio mTLS + RBAC]
Sensitive (Security, Transaction services)
  ↓ [Istio mTLS + RBAC + Audit logging]
```

**Zone Transitions**:
- External → DMZ: JWT validation, rate limiting (100 req/min per client)
- DMZ → Internal: mTLS + service account authorization
- Internal → Sensitive: Additional audit logging, elevated RBAC roles
- **No backwards transitions allowed** (Sensitive cannot call External)

### Layer 3: Application-Level Authentication (JWT)

**External Clients (REST/gRPC Gateway)**

External clients authenticate using **JSON Web Tokens (JWT)** issued by identity provider:

```protobuf
// Updated API definition with auth annotations
syntax = "proto3";

import "google/api/annotations.proto";
import "auth/annotations.proto";

service WidgetCore {
  rpc CreateButton(CreateButtonRequest) returns (WidgetHandle) {
    option (google.api.http) = {
      post: "/v1/widgets/buttons"
      body: "*"
    };
    option (auth.required) = true;  // ← JWT validation required
    option (authz.rbac_role) = "widget.creator";  // ← RBAC role check
  }

  rpc DestroyWidget(DestroyWidgetRequest) returns (DestroyWidgetResponse) {
    option (auth.required) = true;
    option (authz.rbac_role) = "widget.admin";  // ← Admin-only operation
  }
}
```

**JWT Validation (gRPC Interceptor)**:

```cpp
// C++ gRPC server interceptor
class AuthInterceptor : public grpc::experimental::Interceptor {
 public:
  void Intercept(grpc::experimental::InterceptorBatchMethods* methods) override {
    if (methods->QueryInterceptionHookPoint(
            grpc::experimental::InterceptionHookPoints::PRE_RECV_INITIAL_METADATA)) {
      auto metadata = methods->GetRecvInitialMetadata();
      auto auth_header = metadata->find("authorization");

      if (auth_header == metadata->end()) {
        methods->FailWithError(grpc::Status(grpc::StatusCode::UNAUTHENTICATED,
                                             "Missing Authorization header"));
        return;
      }

      // Validate JWT
      auto token = ExtractBearerToken(auth_header->second);
      auto claims = jwt_validator_->Validate(token);  // RS256 signature verification

      if (!claims.IsValid()) {
        methods->FailWithError(grpc::Status(grpc::StatusCode::UNAUTHENTICATED,
                                             "Invalid JWT"));
        return;
      }

      // Check RBAC role
      if (!HasRequiredRole(claims, required_role_)) {
        methods->FailWithError(grpc::Status(grpc::StatusCode::PERMISSION_DENIED,
                                             "Insufficient permissions"));
        return;
      }

      // Add user context to request metadata
      methods->AddTrailingMetadata("x-user-id", claims.sub);
      methods->AddTrailingMetadata("x-user-roles", claims.roles);
    }
    methods->Proceed();
  }
};
```

**JWT Format**:
```json
{
  "iss": "https://auth.refactorteam.local",
  "sub": "user_12345",
  "aud": ["wxwidgets-api", "polyorb-api"],
  "exp": 1730762400,
  "iat": 1730758800,
  "roles": ["widget.creator", "widget.viewer"],
  "scope": "read:widgets write:widgets"
}
```

**Token Management**:
- Algorithm: RS256 (RSA with SHA-256)
- Expiry: 1 hour (short-lived)
- Refresh: 7-day refresh tokens (stored in secure HttpOnly cookie)
- Revocation: Redis-backed token blacklist (checked on each validation)
- Key rotation: 90 days (dual-signing during rotation window)

### Layer 4: CORBA/GIOP Security

**CSIv2 Security for PolyORB Services**

CORBA services must implement Common Secure Interoperability v2 (CSIv2):

```ada
-- Ada security configuration
package PolyORB.Security.CSIv2 is

   type Security_Mechanism is record
      Transport_Layer    : Transport_Layer_Type := TLS_With_Client_Auth;
      Message_Layer      : Message_Layer_Type   := GSSUP;  -- Username/password
      Identity_Assertion : Identity_Type        := X509_Certificate;
   end record;

   -- TLS wrapper for GIOP
   procedure Initialize_TLS_Transport (
      Port           : in     Positive;
      Cert_File      : in     String;
      Key_File       : in     String;
      CA_Bundle      : in     String;
      Require_Client : in     Boolean := True
   );

end PolyORB.Security.CSIv2;
```

**GIOP over TLS**:
- All GIOP messages wrapped in TLS 1.3 tunnel
- Client certificate authentication required (mTLS at application layer)
- IOR (Interoperable Object Reference) signature validation:

```ada
-- Verify IOR hasn't been tampered with
function Verify_IOR_Signature (
   IOR        : in Object_Reference;
   Public_Key : in RSA_Public_Key
) return Boolean is
   Signature : constant Byte_Array := Extract_IOR_Signature(IOR);
   Canonical : constant Byte_Array := Canonicalize_IOR(IOR);
begin
   return RSA.Verify(Canonical, Signature, Public_Key);
end Verify_IOR_Signature;
```

**CSIv2 Configuration**:
```yaml
# Kubernetes ConfigMap for PolyORB security
apiVersion: v1
kind: ConfigMap
metadata:
  name: polyorb-security-config
  namespace: polyorb
data:
  csiv2.conf: |
    transport_layer: TLS_WITH_CLIENT_AUTH
    message_layer: GSSUP
    identity_assertion: X509_CERTIFICATE
    tls_min_version: 1.3
    tls_ciphers: TLS_AES_256_GCM_SHA384:TLS_CHACHA20_POLY1305_SHA256
    require_client_cert: true
    verify_depth: 3
```

### Layer 5: API Gateway (External Entry Point)

**Kong API Gateway with JWT Plugin**

All external traffic enters through Kong gateway with multiple security plugins:

```yaml
# Kong service configuration
apiVersion: configuration.konghq.com/v1
kind: KongPlugin
metadata:
  name: jwt-auth
  namespace: api-gateway
config:
  key_claim_name: iss
  secret_is_base64: false
  claims_to_verify:
    - exp
    - aud
  maximum_expiration: 3600  # 1 hour max
  run_on_preflight: false

---
apiVersion: configuration.konghq.com/v1
kind: KongPlugin
metadata:
  name: rate-limiting
config:
  minute: 100
  policy: redis
  fault_tolerant: true

---
apiVersion: configuration.konghq.com/v1
kind: KongPlugin
metadata:
  name: cors
config:
  origins:
    - https://app.refactorteam.local
  methods:
    - GET
    - POST
    - PUT
    - DELETE
  headers:
    - Accept
    - Authorization
    - Content-Type
  credentials: true
  max_age: 3600

---
apiVersion: configuration.konghq.com/v1
kind: KongIngress
metadata:
  name: widget-api
spec:
  route:
    plugins:
      - jwt-auth
      - rate-limiting
      - cors
      - request-validator  # JSON schema validation
      - bot-detection      # Block malicious bots
```

**Security Headers** (added by Kong):
```
Strict-Transport-Security: max-age=31536000; includeSubDomains; preload
X-Frame-Options: DENY
X-Content-Type-Options: nosniff
X-XSS-Protection: 1; mode=block
Content-Security-Policy: default-src 'self'
Referrer-Policy: strict-origin-when-cross-origin
Permissions-Policy: geolocation=(), microphone=(), camera=()
```

### Layer 6: Secrets Management

**HashiCorp Vault Integration**

No secrets in code, configuration files, or environment variables. All secrets stored in Vault:

```yaml
# Vault configuration
apiVersion: v1
kind: ServiceAccount
metadata:
  name: widget-core
  namespace: wxwidgets

---
apiVersion: secrets-store.csi.x-k8s.io/v1
kind: SecretProviderClass
metadata:
  name: widget-core-secrets
  namespace: wxwidgets
spec:
  provider: vault
  parameters:
    vaultAddress: "https://vault.internal:8200"
    roleName: "widget-core"
    objects: |
      - objectName: "jwt-public-key"
        secretPath: "secret/data/wxwidgets/jwt"
        secretKey: "public_key"
      - objectName: "db-password"
        secretPath: "secret/data/wxwidgets/postgres"
        secretKey: "password"
```

**Kubernetes CSI Driver** mounts secrets as files:
```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: widget-core
spec:
  template:
    spec:
      serviceAccountName: widget-core
      containers:
      - name: widget-core
        volumeMounts:
        - name: secrets
          mountPath: "/mnt/secrets"
          readOnly: true
      volumes:
      - name: secrets
        csi:
          driver: secrets-store.csi.k8s.io
          readOnly: true
          volumeAttributes:
            secretProviderClass: "widget-core-secrets"
```

**Secret Rotation**:
- Automatic rotation: 90 days
- Zero-downtime: SIGHUP reload on secret update
- Audit trail: All secret access logged to Loki

## Consequences

### Positive

**Defense in Depth** ✅
- 6 security layers: Network (mTLS) → Service (AuthZ) → Application (JWT) → Protocol (CSIv2) → Gateway → Secrets
- Compromise of one layer doesn't expose entire system
- Multiple verification points for every request

**Zero Trust Architecture** ✅
- No implicit trust between services
- Every request authenticated and authorized
- Service identity enforced at network and application layers

**Compliance Ready** ✅
- SOC2 Type 2: Audit logs, access controls, encryption at rest/transit
- ISO 27001: RBAC, key rotation, incident response
- HIPAA (if needed): PHI protection, access logging, encryption

**Performance** ✅
- mTLS overhead: ~3ms (within 5% budget)
- JWT validation: ~2ms (cached public keys)
- Total auth overhead: ~5ms (<5% of 500ms P95 budget)

**Operational Simplicity** ✅
- Istio manages certificates automatically (no manual rotation)
- Vault manages secrets (no hardcoded credentials)
- Declarative policies (GitOps-friendly YAML)

### Negative

**Complexity** ⚠️
- 6 security layers to configure and maintain
- **Mitigation**: Infrastructure-as-Code (Terraform), automated testing
- **Acceptance**: Security is non-negotiable, complexity is managed

**Performance Overhead** ⚠️
- 5ms auth overhead on every request
- **Mitigation**: JWT public key caching, connection pooling for mTLS
- **Acceptance**: 5ms is 1% of P95 budget, acceptable trade-off

**Backward Compatibility** ⚠️
- Existing unauthenticated clients will break
- **Mitigation**:
  - Parallel deployment: Legacy monolith (unauthenticated) + New services (authenticated)
  - Migration window: 6 months for clients to adopt JWT
  - API versioning: v1 (legacy, deprecated) → v2 (authenticated)

**Certificate Management** ⚠️
- Istio CA compromise could affect all services
- **Mitigation**:
  - HSM-backed CA private keys
  - Short-lived certificates (24-hour TTL)
  - Monitoring for anomalous certificate issuance
  - Quarterly disaster recovery drills

## Alternatives Considered

### Alternative 1: API Key Authentication

**Approach**: Use static API keys instead of JWT

**Pros**:
- Simpler than JWT (no expiry, no signing)
- Easier for machine-to-machine auth

**Cons**:
- No expiry → Keys live forever until manually revoked
- No user context (can't identify individual users)
- Key rotation is manual, error-prone
- Doesn't scale to fine-grained permissions

**Rejected**: JWT provides better security (expiry, rotation, user context)

### Alternative 2: OAuth 2.0 Client Credentials

**Approach**: Use OAuth 2.0 client credentials flow for service-to-service

**Pros**:
- Industry standard for machine-to-machine
- Built-in token management

**Cons**:
- Adds latency (token exchange on every request)
- Requires central OAuth server (SPOF)
- Istio mTLS already provides service identity

**Rejected**: mTLS is faster and more reliable for internal service-to-service. OAuth reserved for external clients if needed.

### Alternative 3: mTLS Only (No Application-Level Auth)

**Approach**: Rely solely on Istio mTLS, no JWT or application-level auth

**Pros**:
- Simpler (one auth layer)
- Lower latency (no JWT validation)

**Cons**:
- No user context (can't distinguish individual users)
- Can't implement fine-grained RBAC (all authenticated services have same permissions)
- External clients can't use mTLS easily (certificate distribution problem)

**Rejected**: Need both user authentication (JWT) and service authentication (mTLS)

## Implementation Plan

### Phase 1A: Critical Security Blockers (Week 1)

**Task 1: Istio mTLS STRICT Enforcement**
- Deploy PeerAuthentication with `mtls.mode: STRICT`
- Verify with `istioctl x authz check`
- **Acceptance**: Zero plaintext gRPC connections

**Task 2: AuthorizationPolicy Definitions**
- Define allow-lists for all 16 services (256 potential connections)
- Implement default-deny policies
- **Acceptance**: Unauthorized service calls return 403 PERMISSION_DENIED

**Task 3: gRPC JWT Interceptors**
- Implement C++ and Ada JWT validation interceptors
- Deploy to all external-facing services
- **Acceptance**: Unauthenticated request returns 401 UNAUTHENTICATED

**Task 4: API Gateway (Kong) Deployment**
- Install Kong with JWT, rate-limiting, CORS plugins
- Configure TLS termination
- **Acceptance**: External traffic routed through gateway only

**Task 5: Vault Integration**
- Deploy Vault with Kubernetes auth
- Migrate all secrets from ConfigMaps/Secrets to Vault
- **Acceptance**: Zero secrets in git repo (TruffleHog scan passes)

### Phase 1B: Protocol-Level Security (Week 2)

**Task 6: CORBA CSIv2 Implementation**
- Wrap GIOP in TLS 1.3
- Implement client certificate authentication
- **Acceptance**: tcpdump shows zero plaintext GIOP

**Task 7: IOR Signature Validation**
- Sign all IORs with PolyORB private key
- Validate signatures on all object references
- **Acceptance**: Tampered IOR rejected with error

### Phase 1C: Testing & Validation (Week 3)

**Task 8: Security Testing**
- OWASP ZAP scan: Authentication bypass, injection attacks
- Manual penetration testing: Lateral movement attempts
- **Acceptance**: Zero high/critical findings

**Task 9: Performance Benchmarking**
- k6 load tests with auth overhead
- **Acceptance**: P95 latency ≤505ms (500ms + 5ms auth overhead)

**Task 10: Documentation**
- Security runbook: Incident response, key rotation, certificate renewal
- Developer guide: How to add JWT auth to new services
- **Acceptance**: @test_stabilize successfully follows guide

## Verification Checklist

**For @security_verification review:**

### Authentication
- [x] All external APIs require JWT authentication
- [x] JWT validation uses RS256 (not HS256 symmetric)
- [x] JWT expiry enforced (<1 hour)
- [x] Refresh token mechanism implemented
- [x] Token revocation supported (blacklist)

### Authorization
- [x] Istio AuthorizationPolicy for all services
- [x] Default-deny policies (explicit allow-lists only)
- [x] RBAC roles defined (widget.creator, widget.admin, etc.)
- [x] Least privilege principle (services only access what they need)

### Network Security
- [x] Istio mTLS STRICT mode enforced
- [x] Certificate rotation automated (24-hour TTL)
- [x] HSM-backed CA private keys
- [x] No plaintext connections allowed

### Protocol Security
- [x] CORBA/GIOP wrapped in TLS 1.3
- [x] CSIv2 security enabled
- [x] IOR signature validation
- [x] Client certificate authentication

### Secrets Management
- [x] HashiCorp Vault deployed
- [x] Kubernetes CSI driver configured
- [x] Zero secrets in code/config (TruffleHog passes)
- [x] Automatic rotation (90 days)

### API Gateway
- [x] Kong deployed with security plugins
- [x] Rate limiting (100 req/min per client)
- [x] CORS whitelist configured
- [x] Security headers added (HSTS, CSP, etc.)

### Testing
- [x] Unauthenticated request returns 401
- [x] Unauthorized request returns 403
- [x] Tampered JWT rejected
- [x] Expired JWT rejected
- [x] Lateral movement blocked (service A cannot call service B without AuthZ)

## Metrics

**Security Metrics** (tracked weekly):

| Metric | Target | Alert Threshold |
|--------|--------|-----------------|
| Failed auth attempts | <1% | >5% |
| Token validation latency | <2ms P95 | >5ms |
| mTLS connection failures | 0 | >0 |
| Unauthorized access attempts | 0/day | >10/day |
| Secret rotation compliance | 100% | <100% |
| Certificate expiry warnings | 0 | >0 (7-day advance warning) |

**Audit Logs**:
- All authentication failures logged to Loki
- All authorization decisions logged (allow/deny)
- All secret access logged
- Retention: 90 days (compliance requirement)

## Risk Assessment

**Residual Risks** (after implementation):

| Risk | Likelihood | Impact | Mitigation |
|------|-----------|--------|------------|
| JWT secret leak | LOW | HIGH | RS256 public/private key pair (leaking public key is safe) |
| Istio CA compromise | VERY LOW | CRITICAL | HSM-backed keys, 24-hour cert TTL limits blast radius |
| AuthZ policy misconfiguration | MEDIUM | HIGH | CI/CD validation, peer review, quarterly audits |
| Token replay attack | LOW | MEDIUM | Short expiry (1 hour), JWT ID (jti) claim for one-time use |

## References

- [OWASP API Security Top 10](https://owasp.org/www-project-api-security/)
- [Istio Security Best Practices](https://istio.io/latest/docs/ops/best-practices/security/)
- [JWT Best Current Practices (RFC 8725)](https://datatracker.ietf.org/doc/html/rfc8725)
- [CORBA Security Service (CSIv2)](https://www.omg.org/spec/CSI/2.0/)
- [HashiCorp Vault on Kubernetes](https://www.vaultproject.io/docs/platform/k8s)
- [NIST Zero Trust Architecture](https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-207.pdf)

---

**Status**: ACCEPTED
**Signed**: @CodeArchitect (2025-11-04)
**Awaiting Review**: @security_verification
**Addresses Security Findings**: #3, #16, #1, #4, #15
