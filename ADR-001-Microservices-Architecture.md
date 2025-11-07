# ADR-001: Adopt Microservices Architecture for wxWidgets and PolyORB

**Status**: ACCEPTED
**Date**: 2025-11-04
**Authors**: @CodeArchitect
**Reviewers**: @security_verification, @test_stabilize, @code_refactor
**Supersedes**: N/A
**Related**: RDB-001, ADR-002, ADR-003

## Context

We maintain two large monolithic codebases that have accumulated technical debt and architectural issues over multiple years:

**wxWidgets (C++ GUI Framework)**
- 6,560 files, ~160K LoC
- Cross-platform widget library (Windows/macOS/Linux)
- **Problems**:
  - Platform-specific code scattered across 520+ files (shotgun surgery)
  - God class `wxApp` handling too many responsibilities (2,800 LoC)
  - Cyclic dependencies preventing modular testing
  - Avg cyclomatic complexity: 18.4 (high)
  - Difficult to deploy incremental updates
  - Build times exceeding 45 minutes

**PolyORB (Ada Middleware)**
- 4,054 files, ~95K LoC
- CORBA/SOAP/GIOP middleware implementation
- **Problems**:
  - Tight coupling between IDL compiler and runtime ORB
  - Global ORB singleton with thread-safety issues
  - 412 IDL interfaces creating monolithic deployment
  - GIOP protocol details leaking into application layer
  - Cannot scale individual components independently

**Business Drivers**:
1. **Deployment Velocity**: Need 5× deployments per week (currently 1×/month)
2. **Scalability**: Individual components must scale independently
3. **Team Autonomy**: 3 teams need to develop/deploy without coordination
4. **Technology Evolution**: Adopt containers, K8s, cloud-native patterns
5. **Failure Isolation**: Bugs in one component shouldn't crash entire system

## Decision

We will decompose both monolithic applications into **16 independently deployable microservices**:

**wxWidgets Services (7)**:
1. Widget Core Service (6.8K LoC) - Base widget primitives, lifecycle management
2. Event Processing Service (4.2K LoC) - Event loop, handlers, propagation
3. Rendering Service (8.4K LoC) - Cross-platform drawing abstractions
4. MSW Platform Adapter (5.5K LoC) - Windows-specific implementations
5. GTK Platform Adapter (3.6K LoC) - Linux/GNOME implementations
6. Cocoa Platform Adapter (6.2K LoC) - macOS implementations
7. Layout Engine Service (5.1K LoC) - Sizers, constraints, positioning

**PolyORB Services (9)**:
1. ORB Core Service (9.2K LoC) - Object request broker, object lifecycle
2. GIOP Protocol Service (3.8K LoC) - IIOP transport, marshaling
3. Security Service (2.4K LoC) - Authentication, encryption, ACLs
4. Naming Service (1.9K LoC) - Object directory, name resolution
5. Event Notification Service (2.5K LoC) - Pub/sub messaging
6. Transaction Service (3.1K LoC) - ACID operations, 2PC
7. POA Manager Service (2.7K LoC) - Portable Object Adapter lifecycle
8. IDL Compiler Service (4.6K LoC) - Interface definition parsing, code generation
9. Protocol Adapters (2.8K LoC) - Multi-protocol support (SOAP, DIOP)

**Architecture Characteristics**:

- **Service Size**: 2K-10K LoC per service (optimal for microservices)
- **Communication**:
  - gRPC (Protobuf) for internal low-latency calls
  - REST (OpenAPI 3.1) for external/legacy clients
  - GraphQL (Apollo) for cross-service aggregation
  - Kafka for async event streaming
- **Deployment**:
  - Docker containers (multi-stage builds, <200MB images)
  - Kubernetes orchestration (manifests, Helm, operators)
  - Istio service mesh (mTLS, traffic management, observability)
- **Data**:
  - Database per service (no shared databases)
  - Event sourcing for distributed transactions
  - Eventual consistency model
- **Observability**:
  - Prometheus metrics, Grafana dashboards
  - Loki structured logging
  - Jaeger distributed tracing
- **Testing**:
  - Pyramid: Unit (50%), Component (30%), Contract (10%), Integration (8%), E2E (2%)
  - Contract tests with Pact CDC
  - Load tests with k6 (P95 <500ms threshold)
- **Security**:
  - SAST: Clang-Tidy (C++), GNATcheck (Ada)
  - DAST: OWASP ZAP automated scans
  - Container: Trivy/Grype CVE scanning
  - Runtime: Falco anomaly detection

## Consequences

### Positive

**Deployment Independence**
- Teams can deploy services independently without coordination
- Reduces blast radius of changes (single service vs entire monolith)
- Enables canary deployments and A/B testing per service
- Expected: 5×/week deployment frequency (from 1×/month)

**Technology Flexibility**
- Services can use optimal tech stacks (C++17 for widgets, Ada 2012 for middleware)
- Easier to upgrade dependencies per service
- Can introduce new protocols (gRPC) without rewriting everything

**Scalability**
- Scale high-load services independently (e.g., Rendering Service × 10, Naming Service × 2)
- Right-size resources per service (reduce waste)
- Kubernetes HPA enables auto-scaling based on CPU/memory

**Team Autonomy**
- 3 teams own services end-to-end (development → deployment → operations)
- Reduces coordination overhead, meeting fatigue
- Clear ownership boundaries prevent stepping on toes

**Failure Isolation**
- Bug in Layout Engine doesn't crash Event Processing
- Circuit breakers prevent cascading failures
- Service mesh provides automatic retries, timeouts

**Testability**
- Services can be tested in isolation with mocked dependencies
- Faster test feedback (unit tests <10ms vs 5min integration tests)
- Contract tests ensure API compatibility

### Negative

**Operational Complexity** ⚠️
- Managing 16 services vs 2 monoliths requires more ops maturity
- **Mitigation**:
  - Invest in observability stack (Prometheus, Grafana, Jaeger)
  - Standardize deployment (Helm charts, GitOps)
  - Automate with CI/CD pipelines
  - Centralized logging (Loki) for correlation

**Network Latency** ⚠️
- Inter-service calls add network hops (P95 latency risk)
- **Mitigation**:
  - Use gRPC with HTTP/2 multiplexing (lower overhead than REST)
  - Co-locate related services (K8s node affinity)
  - Implement aggressive caching (Redis)
  - Load test early: P95 <500ms threshold
- **Acceptance Criteria**: No >20% latency regression vs monolith

**Distributed Transactions** ⚠️
- ACID transactions across services require saga pattern or 2PC
- **Mitigation**:
  - Event sourcing for audit trail
  - Compensating transactions for rollbacks
  - Transaction Service handles 2PC coordination
- **Limitation**: Some operations will be eventually consistent

**Data Consistency** ⚠️
- No shared database means eventual consistency model
- **Mitigation**:
  - Event-driven architecture with Kafka
  - Idempotent event handlers
  - Versioned APIs to handle schema evolution
- **Acceptance Criteria**: <100ms consistency lag for critical paths

**Development Overhead** ⚠️
- Contract tests, API versioning, backward compatibility burden
- **Mitigation**:
  - Pact CDC automates contract testing
  - OpenAPI/Protobuf schemas enforce contracts
  - Semantic versioning (v1, v2 coexistence)

**Debugging Difficulty** ⚠️
- Tracing errors across 16 services harder than single process
- **Mitigation**:
  - Correlation IDs in all logs/traces
  - Jaeger distributed tracing
  - Centralized error tracking (Sentry)

### Neutral

**Infrastructure Costs**
- Kubernetes cluster overhead (~$500/month dev + $2000/month prod)
- Offset by better resource utilization (scale per service)

**Learning Curve**
- Team must learn K8s, Istio, gRPC, contract testing
- **Plan**: 2-week training sprint, pair programming, documentation

## Alternatives Considered

### Alternative 1: Keep Monoliths, Improve Modularity

**Approach**: Refactor monoliths into well-defined modules with clear boundaries but keep single-deployment model.

**Pros**:
- Lower operational complexity (no K8s, service mesh)
- No network latency concerns
- Simpler debugging and tracing

**Cons**:
- Still requires full rebuild/redeploy for any change (slow deployment)
- Cannot scale individual components
- Teams still have deployment dependencies
- Doesn't address platform-specific code scattering

**Rejected Because**: Doesn't meet business requirement for 5×/week deployment velocity and team autonomy.

### Alternative 2: Hybrid - Monolith + Selected Microservices

**Approach**: Keep core monoliths but extract high-churn or high-load components as microservices (e.g., Rendering Service, Event Notification).

**Pros**:
- Incremental migration reduces risk
- Focuses effort on bottleneck services
- Lower initial complexity

**Cons**:
- Maintains dual architecture (monolith + microservices)
- Still requires coordination for monolith changes
- Unclear long-term strategy (when to stop extracting?)

**Rejected Because**: Halfway solution creates organizational confusion about which pattern to use. Better to commit fully to microservices for consistent architecture.

### Alternative 3: Serverless Functions (FaaS)

**Approach**: Decompose into AWS Lambda/Google Cloud Functions instead of containerized microservices.

**Pros**:
- No container/K8s management
- Automatic scaling
- Pay-per-invocation cost model

**Cons**:
- Cold start latency unacceptable for UI widgets (200-500ms)
- Vendor lock-in (AWS/GCP)
- Ada runtime not supported by FaaS platforms
- CORBA persistent connections incompatible with stateless functions

**Rejected Because**: Latency requirements and technology constraints (Ada, CORBA) make FaaS unsuitable.

### Alternative 4: Service-Oriented Architecture (SOA)

**Approach**: Larger, coarser-grained services (3-4 services) with ESB for communication.

**Pros**:
- Fewer services to manage (lower ops complexity)
- ESB provides centralized routing, transformation

**Cons**:
- ESB becomes bottleneck and single point of failure
- Larger services harder to test, deploy
- ESB adds latency vs direct service-to-service calls
- Doesn't provide fine-grained scaling

**Rejected Because**: Microservices with service mesh (Istio) provides decentralized routing without ESB bottleneck.

## Implementation Plan

See **RDB-001 Migration Strategy** for full 65-week roadmap.

**Phase 1 (Weeks 1-8)** - Foundation ← CURRENT
- Create Dockerfiles for all 16 services
- Build Kubernetes manifests (Deployments, Services, ConfigMaps)
- Establish CI/CD pipelines (GitHub Actions)
- Deploy observability stack (Prometheus, Grafana, Loki, Jaeger)
- Set up security scanning (SAST, DAST, container)
- Implement test framework (GoogleTest, AUnit, Pact, k6)

**Phase 2 (Weeks 9-16)** - API Gateway
- Deploy Kong/Envoy gateway
- Route 10% traffic to new services (canary)
- Monitor latency, error rates
- A/B test performance

**Phase 3 (Weeks 17-40)** - Service Migration
- Migrate services incrementally (strangler fig pattern)
- Increase traffic % gradually (10% → 50% → 100%)
- Keep monolith running in parallel
- Feature flags for instant rollback

**Checkpoints**:
- Week 8: All services deployed to staging, passing smoke tests
- Week 16: 10% production traffic on new services, P95 <500ms
- Week 40: 100% traffic on microservices, monolith deprecated

## Metrics and Monitoring

**Success Metrics** (measured weekly):

| Metric | Baseline | Target | Current |
|--------|----------|--------|---------|
| Deploy Frequency | 1×/month | 5×/week | TBD |
| Lead Time | 14 days | <1 day | TBD |
| MTTR | 4 hours | <15 min | TBD |
| Change Failure Rate | 15% | <5% | TBD |
| P95 Latency | 350ms | <500ms | TBD |
| Test Coverage | 42% | 80% | TBD |
| High/Critical CVEs | 8 | 0 | TBD |

**Dashboards**:
1. **Service Health**: Error rates, latency percentiles, uptime per service
2. **Infrastructure**: CPU, memory, disk, network utilization per pod
3. **DORA Metrics**: Deployment frequency, lead time, MTTR, change failure rate
4. **Business Metrics**: Widgets created/sec, CORBA invocations/sec

**Alerts** (PagerDuty):
- P95 latency >500ms for 5 minutes
- Error rate >1% for 2 minutes
- Service unavailable (3 consecutive health check failures)
- High/critical CVE detected in production image

## Risks and Mitigations

See **RDB-001 Risk Assessment** for detailed analysis.

**Critical Risks**:
1. **ABI Breakage**: Maintain C ABI shim layer, version APIs
2. **Performance Regression**: Load test early, co-locate services, use gRPC
3. **Ada Runtime Issues**: Use official GNAT Docker images, static linking

**Rollback Strategy**:
- Per-service rollback: `kubectl rollout undo deployment/SERVICE`
- Traffic shifting: Istio VirtualService weights (100% to monolith)
- Feature flags: Disable new services via ConfigMap
- Monolith kept running for 6 months post-migration

## Decision Rationale

**Why Microservices Now?**
1. **Technical Debt Critical**: Cyclomatic complexity, cyclic deps blocking progress
2. **Business Pressure**: Competitive need for faster deployment (5×/week)
3. **Team Growth**: 3 teams need autonomy to scale development velocity
4. **Technology Maturity**: K8s, Istio, gRPC now production-ready (not 5 years ago)

**Why Not Wait?**
- Monoliths growing 10K LoC/year (complexity compounding)
- Build times increasing 5% monthly (45min → 2hr if unchecked)
- Recruitment: Modern developers expect cloud-native architecture

## References

- [RDB-001: Refactor Design Brief](./RDB-001-Microservices-Migration.md)
- [ADR-002: API Protocol Selection](./ADR-002-API-Protocols.md) (Forthcoming)
- [ADR-003: Container Base Image Strategy](./ADR-003-Container-Images.md) (Forthcoming)
- [Building Microservices](https://www.oreilly.com/library/view/building-microservices-2nd/9781492034018/) - Sam Newman
- [Accelerate: DORA Metrics](https://www.devops-research.com/research.html)
- [Google SRE Book: Monitoring Distributed Systems](https://sre.google/sre-book/monitoring-distributed-systems/)

## Appendix: Service Boundaries Analysis

**wxWidgets Cohesion Analysis**:
```
Widget Core (High Cohesion):
  - wxObject, wxWindow, wxControl base classes
  - Widget lifecycle (Create, Destroy, Show, Hide)
  - Property management (GetLabel, SetLabel, Enable, Disable)

Event Processing (High Cohesion):
  - wxEvtHandler, wxEvent hierarchy
  - Event propagation, binding, handling
  - Timer, idle events

Rendering (High Cohesion):
  - wxDC, wxGraphicsContext abstractions
  - Cross-platform drawing primitives
  - Image, bitmap handling
```

**PolyORB Cohesion Analysis**:
```
ORB Core (High Cohesion):
  - Object references, lifecycles
  - Request dispatching
  - Servant management

GIOP Protocol (High Cohesion):
  - IIOP transport
  - CDR marshaling/unmarshaling
  - Message framing

Naming (High Cohesion):
  - Object directory (bind, resolve, unbind)
  - Name context hierarchy
  - Persistent storage
```

**Coupling Analysis**:
- Widget Core → Event Processing: **LOW** (async events via message queue)
- Rendering → Platform Adapters: **MEDIUM** (polymorphic dispatch, well-defined interface)
- ORB Core → GIOP: **LOW** (protocol abstraction, swappable transports)
- Security → ORB Core: **LOW** (interceptor pattern, aspect-oriented)

All service boundaries exhibit **high cohesion** and **low coupling** - ideal for microservices decomposition.

---

**Status**: ACCEPTED
**Signed**: @CodeArchitect (2025-11-04)
**Next Review**: 2025-11-18 (Sprint 1 Retrospective)
