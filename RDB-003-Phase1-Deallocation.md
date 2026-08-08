# Refactor Design Brief (RDB) - Phase 1 Deallocation

**RDB ID**: RDB-003
**Title**: Centralized Memory Deallocation with Security-Critical Zeroization
**Author**: @code_architect
**Date**: 2025-11-05
**Status**: DRAFT

---

## Executive Summary

Centralize 73 scattered instances of memory deallocation into a single, auditable utility package with automatic memory zeroization for security-critical types (crypto keys, credentials, session tokens). Phase 1A focuses on 13 highest-risk instances (3 CRITICAL + 10 HIGH priority) to validate approach before scaling to remaining 60 MEDIUM/LOW instances.

**Key Points**:
- **Goal**: Eliminate memory disclosure vulnerabilities through consistent zeroization
- **Scope**: 13 security-critical instances in Phase 1A (73 total across all phases)
- **Timeline**: 4 weeks (Design 1w, Implementation 1w, Validation 1w, Deployment 1w)
- **Risk Level**: HIGH (touching crypto subsystem, session management, credential handling)
- **Security Impact**: SECURITY-CRITICAL (prevents credential leakage in memory dumps)

---

## 1. Context & Motivation

### Current State (Problems)

**Key Issues**:
- **Issue 1**: Shotgun surgery - 73 instances of duplicated `Ada.Unchecked_Deallocation` logic scattered across codebase
- **Issue 2**: Security vulnerability - Inconsistent memory zeroization (only 15% of security-critical types currently zeroized)
- **Issue 3**: Audit nightmare - No centralized logging of deallocation failures for security-sensitive types
- **Issue 4**: Maintainability - Every new type requiring secure deallocation needs custom implementation

**Impact of NOT Refactoring**:
- **Business impact**: Potential credential disclosure in crash dumps violates SOC2/GDPR requirements
- **Technical debt accumulation**: 73 instances grow to 100+, impossible to audit comprehensively
- **Security risks**:
  - Crypto keys persist in memory after use (P1 finding in last security audit)
  - Session tokens vulnerable to memory scanning attacks
  - Credentials in error logs during OOM conditions

### Desired State (Goals)

**Measurable Outcomes**:
- **Goal 1**: Reduce 73 deallocation instances to 1 centralized utility package (95% reduction)
- **Goal 2**: Achieve 100% memory zeroization for CRITICAL/HIGH security types (from 15%)
- **Goal 3**: Enable audit trail for all security-critical deallocations (0% → 100% coverage)
- **Goal 4**: Zero performance regression (deallocation time ≤baseline ±5%)

**Success Criteria**:
- ✅ All 13 Phase 1A instances migrated to centralized utility
- ✅ Memory zeroization verified via Valgrind + custom tests (100% success rate)
- ✅ Zero new HIGH/CRITICAL SAST findings
- ✅ Audit logging operational for all CRITICAL types
- ✅ Security Review Note (SRN) approval from @security_verification

---

## 2. Scope & Non-Goals

### In Scope

**Phase 1A (This RDB) - 13 Security-Critical Instances**:

**CRITICAL Priority (3 instances)**:
- `/src/crypto/key_manager.adb:245` - AES-256 encryption keys
- `/src/auth/credential_store.adb:187` - User credentials (passwords, API keys)
- `/src/session/session_handler.adb:412` - Session tokens (JWT, OAuth)

**HIGH Priority (10 instances)**:
- `/src/auth/acl_manager.adb:89` - Access control lists
- `/src/crypto/buffer_manager.adb:156` - Temporary crypto buffers
- `/src/auth/oauth_client.adb:234` - OAuth client secrets
- `/src/session/session_cache.adb:301` - Cached session data
- `/src/crypto/hash_context.adb:78` - Hash computation contexts
- `/src/auth/token_validator.adb:412` - Token validation state
- `/src/crypto/cipher_context.adb:198` - Cipher operation contexts
- `/src/session/refresh_token.adb:267` - Refresh token storage
- `/src/auth/api_key_store.adb:345` - API key database
- `/src/crypto/random_pool.adb:123` - Cryptographic random number pool

**Modules/Services Affected**:
- `/src/crypto/` - Cryptographic subsystem (5 instances)
- `/src/auth/` - Authentication & authorization (5 instances)
- `/src/session/` - Session management (3 instances)
- `/lib/deallocation/` - NEW utility package (created by this refactor)

**Change Types**:
- [x] Code structure (new centralized utility package)
- [ ] API contracts (NO changes - backward compatible)
- [x] Data models (generic types for type-safe wrappers)
- [ ] Infrastructure (NO deployment changes)
- [ ] Dependencies (NO new external dependencies)

### Out of Scope (Non-Goals)

**Explicitly Excluded (Deferred to Phase 1B)**:
- **Non-goal 1**: MEDIUM/LOW priority instances (60 instances in UI, logging, debugging code)
- **Non-goal 2**: Performance optimization beyond "no regression" requirement
- **Non-goal 3**: Comprehensive crypto subsystem redesign (separate effort, RDB-005)
- **Non-goal 4**: Automated memory scanning detection system (future work)

**Rationale for Exclusions**:
- **Phase 1B deferral**: Validate approach on 13 highest-risk instances before scaling to 60 lower-risk instances (reduces blast radius)
- **Performance optimization**: Current deallocation performance is acceptable; premature optimization adds risk
- **Crypto redesign**: Out of scope for memory management refactor; separate security initiative
- **Detection system**: Requires production monitoring infrastructure not yet available

---

## 3. Technical Design

### Current Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│  Current State: 73 Scattered Deallocation Instances             │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  key_manager.adb:          crypto_buffer.adb:                   │
│  ┌──────────────────┐     ┌──────────────────┐                 │
│  │ type Key_Ptr is  │     │ type Buf_Ptr is  │                 │
│  │   access Key;    │     │   access Buffer; │                 │
│  │                  │     │                  │                 │
│  │ procedure Free is│     │ procedure Free is│                 │
│  │  new Unchecked_  │     │  new Unchecked_  │                 │
│  │  Deallocation... │     │  Deallocation... │                 │
│  │                  │     │                  │                 │
│  │ Zeroize(K);      │     │ -- NO ZEROIZATION│ ⚠️ VULNERABILITY│
│  │ Free(K);         │     │ Free(B);         │                 │
│  └──────────────────┘     └──────────────────┘                 │
│                                                                  │
│  session_handler.adb:      ... (70 more instances)              │
│  ┌──────────────────┐                                           │
│  │ Manual zeroize   │     ❌ 85% missing zeroization            │
│  │ inconsistent     │     ❌ No audit trail                     │
│  └──────────────────┘     ❌ No centralized error handling      │
└─────────────────────────────────────────────────────────────────┘
```

**Anti-Patterns Identified**:
- **Anti-pattern 1**: Shotgun surgery - 73 duplicated deallocation instances requiring identical security changes
- **Anti-pattern 2**: Leaky abstraction - Memory security concerns scattered throughout business logic
- **Anti-pattern 3**: Inconsistent implementation - 15% zeroization coverage due to manual, ad-hoc approach

### Target Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│  Target State: Centralized Deallocation Utility Package         │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  /lib/deallocation/secure_deallocation.ads (NEW PACKAGE)        │
│  ┌───────────────────────────────────────────────────────────┐ │
│  │ generic                                                    │ │
│  │   type T is private;                                      │ │
│  │   type T_Ptr is access T;                                 │ │
│  │   with procedure Zeroize(Item : in out T) is <>;         │ │
│  │ procedure Secure_Free(Ptr : in out T_Ptr;                │ │
│  │                       Log_Failure : Boolean := True);     │ │
│  │                                                            │ │
│  │ -- Automatic zeroization + deallocation + audit logging   │ │
│  │ -- Type-safe, generic, reusable                           │ │
│  └───────────────────────────────────────────────────────────┘ │
│                                                                  │
│  Client Code (key_manager.adb):                                 │
│  ┌───────────────────────────────────────────────────────────┐ │
│  │ procedure Free_Key is new Secure_Free(                    │ │
│  │   T => AES_Key,                                           │ │
│  │   T_Ptr => Key_Ptr,                                       │ │
│  │   Zeroize => Zeroize_Key);                                │ │
│  │                                                            │ │
│  │ Free_Key(K);  -- Automatic zeroization + logging          │ │
│  └───────────────────────────────────────────────────────────┘ │
│                                                                  │
│  ✅ 100% zeroization coverage (enforced by type system)         │
│  ✅ Centralized audit trail                                     │
│  ✅ Consistent error handling                                   │
│  ✅ Backward compatible (drop-in replacement)                   │
└─────────────────────────────────────────────────────────────────┘
```

**Design Principles Applied**:
- **Principle 1**: Don't Repeat Yourself - 73 instances → 1 generic utility
- **Principle 2**: Separation of Concerns - Security logic isolated from business logic
- **Principle 3**: Fail-Safe Defaults - Zeroization is mandatory (type system enforced)

### Migration Path

**Approach**: Strangler Fig (incremental migration with feature flag)

**Steps**:

**Step 1: Utility Package Creation + Unit Testing**
- Duration: 3 days
- Validation:
  - Unit tests pass (95%+ coverage, 90%+ mutation score)
  - Valgrind confirms zero memory leaks
  - Zeroization verification tests pass (100% success rate)
- Rollback: Delete new package, no client changes made yet

**Step 2: Migrate CRITICAL Instances (3)**
- Duration: 2 days
- Validation:
  - Compilation successful
  - Unit tests pass for migrated modules
  - Memory zeroization verified via custom tests
  - Feature flag OFF by default (legacy code active)
- Rollback: Feature flag toggle to legacy code (<2min)

**Step 3: Migrate HIGH Instances (10)**
- Duration: 3 days
- Validation:
  - Integration tests pass
  - Security regression tests (SAST comparison, memory safety)
  - Canary deployment 1% traffic (24-48h bake)
- Rollback: Feature flag toggle + canary rollback

**Step 4: Full Deployment**
- Duration: 7 days
- Validation:
  - Canary progression: 1% → 10% → 50% → 100%
  - Monitoring: Error rates, latency, memory usage
  - Final security review + SRN
- Rollback: Multi-layer (feature flag, canary, Kubernetes)

---

## 4. Security Analysis

### Security Invariants

**What MUST NOT Break**:

**Authentication & Authorization**:
- **Invariant 1**: Credential types (passwords, API keys, OAuth secrets) MUST be zeroized before deallocation
- **Invariant 2**: ACL checks MUST occur before deallocation (no use-after-free in authz logic)

**Cryptography**:
- **Invariant 3**: ALL crypto keys (AES, RSA, HMAC) MUST be zeroized before deallocation
- **Invariant 4**: Crypto buffers holding plaintext/ciphertext MUST be zeroized
- **Invariant 5**: Hash/cipher contexts MUST be zeroized (prevent state leakage)
- **Invariant 6**: Random pool state MUST be zeroized (prevent RNG prediction)

**Memory Safety**:
- **Invariant 7**: Zeroization MUST occur BEFORE deallocation (not after)
- **Invariant 8**: Zeroization MUST be compiler-barrier protected (prevent optimization removal)
- **Invariant 9**: No credential data in crash dumps or logs (scrubbed before logging)
- **Invariant 10**: Deallocation failures MUST be audited for security-critical types

**Audit & Compliance**:
- **Invariant 11**: All CRITICAL type deallocations MUST be logged (timestamp, type, success/failure)
- **Invariant 12**: Audit logs MUST NOT contain sensitive data (type name only, no values)

**Data Handling**:
- **Invariant 13**: Session tokens MUST be invalidated before deallocation (check "in-use" flag)
- **Invariant 14**: Multi-threaded access MUST be protected (no concurrent deallocation)

### Hidden Security Properties

**⚠️ CRITICAL: Undocumented Security Assumptions That Need Investigation**

**Potential Hidden Invariants**:
- **Property 1**: Session token deallocation - Does session_handler.adb:412 require checking "in-use" flag before deallocation? (Similar to production bug mentioned by @security_verification)
- **Property 2**: Crypto buffer caching - Are any crypto buffers in buffer_manager.adb:156 cached/reused between operations? If yes, timing of zeroization is critical
- **Property 3**: ACL reference counting - Do ACLs in acl_manager.adb:89 have shared references? Must verify all references released before deallocation
- **Property 4**: OAuth token refresh - Does oauth_client.adb:234 have implicit dependency on token still being valid during deallocation?
- **Property 5**: Random pool state - Does random_pool.adb:123 require special deinitialization beyond zeroization?

**Domain Experts to Consult**:
- **@crypto_team_lead** - Crypto subsystem (5 instances), original implementer, knows all edge cases
- **@auth_architect** - Authentication & authorization (5 instances), 7 years on this codebase
- **@session_expert** - Session management (3 instances), designed current session lifecycle

**"Magic" Code Requiring Investigation**:
- `/src/session/session_handler.adb:412` - Session token deallocation has complex lifecycle logic
- `/src/crypto/buffer_manager.adb:156` - Buffer pooling behavior unclear from code
- `/src/auth/acl_manager.adb:89` - ACL parent-child relationships may have hidden dependencies

**Pre-Refactor Actions Required**:
- [ ] Consult @crypto_team_lead on crypto buffer lifecycle (est. 1 hour)
- [ ] Consult @auth_architect on ACL reference counting (est. 1 hour)
- [ ] Consult @session_expert on session token "in-use" flag requirement (est. 30 min)
- [ ] @security_verification runs Task 5 baseline scan (4 hours) - **BLOCKING**
- [ ] Document findings in Security Invariants section above

### Security Enhancements

**Opportunistic Hardening (Clearly Marked as New Behavior)**:

**In-Scope Enhancements** (Low regression risk, aligns with refactor):
- **Enhancement 1**: Add audit logging for CRITICAL type deallocation failures (0% → 100% coverage)
- **Enhancement 2**: Implement compiler barrier to prevent zeroization optimization removal
- **Enhancement 3**: Add memory zeroization for 2 additional HIGH-priority types discovered during Task 5 scan

**Out-of-Scope Enhancements** (Defer to separate refactor):
- **Enhancement 4**: Comprehensive memory encryption at rest - requires kernel support, RDB-005
- **Enhancement 5**: Automated memory scanning detection - requires production monitoring infra, separate project
- **Enhancement 6**: Key rotation automation - separate security sprint, independent of deallocation

**Rationale for In-Scope Enhancements**:
- **Enhancement 1**: Audit logging is natural extension of centralized utility; negligible performance cost (<1μs per deallocation)
- **Enhancement 2**: Compiler barrier is 1-line change per zeroization function; prevents critical security regression
- **Enhancement 3**: If Task 5 scan identifies 2 additional types needing zeroization, adding them now avoids future refactor

### Security Test Requirements

**Mandatory Security Testing**:

**Prevention (Before Deployment)**:
- [ ] SAST baseline comparison (0 new CRITICAL, ≤5 new HIGH findings)
- [ ] Dependency CVE scan (0 CRITICAL, ≤3 HIGH)
- [ ] Secret scanning (0 secrets in code/logs)
- [ ] Security test coverage: ≥95% crypto/auth/session, ≥85% deallocation utility

**Detection (During Testing)**:
- [ ] Memory safety testing (Valgrind: 0 leaks, 0 invalid reads/writes)
- [ ] Zeroization verification (custom test: dump memory after deallocation, verify all-zeros)
- [ ] Security integration tests (auth/authz/session lifecycle validation)
- [ ] Mutation testing (≥90% auth/crypto, ≥80% general code)

**Response (Post-Deployment)**:
- [ ] Security monitoring (MTTD <1h CRITICAL, <24h HIGH)
- [ ] Incident response readiness (MTTR <1h CRITICAL, <4h HIGH)
- [ ] Rollback capability validated (100% success rate, <2min via feature flag)

**Compliance Testing**:
- [ ] Audit log coverage (100% CRITICAL type deallocations)
- [ ] Memory dump analysis (0 credential leakage in simulated crash dumps)
- [ ] SOC2 evidence (centralized audit trail, access controls)

### Security Review Checkpoints

**@security_verification Review Schedule**:

**Checkpoint 1: Draft RDB Review** (48h SLA - SECURITY-CRITICAL)
- **Timing**: NOW (this draft RDB at 90% complete)
- **Artifacts**: This RDB, affected modules list (13 instances), architecture diagram
- **Expected Output**: BLOCKING findings on hidden properties OR APPROVED with advisory suggestions
- **Turnaround**: 48 hours (security-critical refactor)

**Checkpoint 2: Pre-Implementation Baseline** (4h - BLOCKING)
- **Timing**: After domain expert consultations, before implementation starts
- **Artifacts**: Task 5 baseline scan (SAST, security invariants, hidden properties analysis)
- **Expected Output**: Security baseline report, memory zeroization test scripts, Security Watch List
- **Turnaround**: 4 hours

**Checkpoint 3: Mid-Implementation Review** (2h)
- **Timing**: After Step 2 (CRITICAL instances migrated, unit tests pass)
- **Artifacts**: Unit test results, mutation testing, initial coverage reports
- **Expected Output**: Early feedback on test gaps, zeroization verification
- **Turnaround**: 2 hours

**Checkpoint 4: Pre-Deployment Review** (2h)
- **Timing**: After Step 3 (integration tests complete, canary 1% ready)
- **Artifacts**: Integration test results, SAST comparison, Valgrind reports, memory tests
- **Expected Output**: Validation of security boundaries, no regressions, canary deployment approval
- **Turnaround**: 2 hours

**Checkpoint 5: Final Security Sign-Off** (4h)
- **Timing**: After canary 50% (before 100% rollout)
- **Artifacts**: Complete test results, SAST/Valgrind, audit log validation, production metrics
- **Expected Output**: Security Review Note (SRN) - formal approval for 100% deployment
- **Turnaround**: 4 hours (2h if zero issues in canary)

**Total Security Review Time**: ~16 hours spread across 4-week refactor lifecycle

---

## 5. Risk Assessment & Mitigation

### Risk Matrix

| Risk | Likelihood | Impact | Severity | Mitigation |
|------|------------|--------|----------|------------|
| Hidden session token invariant breaks (in-use flag) | MEDIUM (3) | HIGH (4) | P1 (48h fix) | @session_expert consultation + Task 5 scan + feature flag rollback |
| Crypto buffer caching assumption violated | LOW (2) | HIGH (4) | P2 (1 week) | @crypto_team_lead consultation + memory analysis tests + canary deployment |
| Performance regression >50% | LOW (2) | MEDIUM (3) | P2 (1 week) | Benchmark suite + canary monitoring + rollback triggers |
| Zeroization optimized away by compiler | MEDIUM (3) | CRITICAL (5) | P1 (48h fix) | Compiler barrier implementation + Valgrind verification + regression tests |
| ACL reference counting bug (use-after-free) | LOW (2) | HIGH (4) | P2 (1 week) | @auth_architect consultation + integration tests + memory sanitizer |
| Audit logging failure exposes security gap | LOW (1) | MEDIUM (3) | P3 (1 month) | Comprehensive audit log tests + monitoring alerts |

**Risk Scoring**: Risk = Likelihood (1-5) × Impact (1-5) × Exposure (1-5)
- 100-125: P0 (drop everything, fix now)
- 75-99: P1 (fix within 48h)
- 50-74: P2 (fix within 1 week)
- 25-49: P3 (fix within 1 month)
- 1-24: P4 (backlog)

### Security-Specific Risks

**P0/P1 Security Risks** (Must fix before refactor):
- **NONE CURRENTLY IDENTIFIED** - Task 5 baseline scan may identify P0/P1 blockers

**P2/P3 Security Risks** (Accept with mitigation OR fix opportunistically):
- **Medium Risk 1**: Minor info disclosure if audit logs exposed (ACCEPTED - logs already protected by RBAC)
- **Medium Risk 2**: Timing side-channel in zeroization (ACCEPTED - out of scope for this refactor, compensating control: constant-time zeroization in crypto library)

**Risk Acceptance**:
- **Medium Risk 1**: Accepted with mitigation (RBAC on audit logs, scrubbed sensitive data). Approval: @security_verification (P3 acceptable)
- **Medium Risk 2**: Accepted - timing attacks on zeroization are theoretical; crypto library already uses constant-time operations. Time-boxed acceptance: 90 days, revisit in security sprint.

### Blast Radius

**Affected Components**:
- **Crypto subsystem** (`/src/crypto/`): HIGH impact (5 instances, critical security boundary)
- **Auth subsystem** (`/src/auth/`): HIGH impact (5 instances, authentication/authorization logic)
- **Session subsystem** (`/src/session/`): MEDIUM impact (3 instances, session lifecycle management)
- **Deallocation utility** (`/lib/deallocation/`): NEW component (zero existing users, gradual rollout)

**User Impact**:
- **Number of users affected**: 0 direct impact (behavior-preserving refactor)
- **Affected user workflows**: NONE (internal memory management, transparent to users)
- **Downtime required**: NONE (canary deployment with feature flag, zero downtime)

**Rollback Complexity**:
- **LOW**: Feature flag toggle (<2min instant rollback to legacy code)
- **MEDIUM**: Canary deployment rollback (gradual, 5-10min if needed)
- **LOW**: Kubernetes rollback (5-10min if catastrophic failure)

---

## 6. Rollback Strategy

**Multi-Layer Rollback Defense**:

### Layer 1: Feature Flags (Instant - <2min)

```yaml
# Feature flag configuration
feature_flags:
  secure_deallocation_utility:
    enabled: true
    rollout: 100  # 0-100% (gradual rollout during canary)
    rollback_trigger:
      memory_zeroization_loss: true  # Immediate rollback
      error_rate: 0.1%  # Auto-rollback after 2min
      timeout: 5min
    per_type_granularity:
      CRITICAL_types: 100  # Always use new utility for CRITICAL
      HIGH_types: 50       # 50/50 split during testing
```

**Capabilities**:
- Instant toggle (new utility → legacy deallocation code)
- Per-type granularity (CRITICAL vs HIGH types can have different rollout %)
- Automatic rollback on error thresholds

### Layer 2: Canary Deployment (Gradual)

**Rollout Schedule**:
- **Week 4, Day 1-2**: 1% traffic (monitor 24-48h)
  - Rollback trigger: Memory zeroization loss detected
  - Metrics: Deallocation error rate, memory usage, latency
- **Week 4, Day 3-4**: 10% traffic (monitor 48h)
  - Rollback trigger: Error rate >0.1% OR P95 latency >+20% baseline
  - Security: Compare audit logs (old vs new utility)
- **Week 4, Day 5-6**: 50% traffic (monitor 48-72h)
  - Rollback trigger: Any CRITICAL security event OR SAST findings
  - Testing: Chaos engineering (kill crypto service, verify graceful degradation)
- **Week 4, Day 7+**: 100% traffic (monitor 2 weeks)
  - Bake time: Full observability, comprehensive logging
  - Final: Security Review Note (SRN) after 2-week bake

### Layer 3: Kubernetes Rollback (5-10min)

```bash
# Instant rollback to previous deployment (if feature flag fails)
kubectl rollout undo deployment/crypto-service -n production
kubectl rollout undo deployment/auth-service -n production
kubectl rollout undo deployment/session-service -n production

# Verify rollback status
kubectl rollout status deployment/crypto-service -n production
```

### Layer 4: Database Migration Rollback (Not Applicable)

**N/A** - This refactor does NOT involve database schema changes. No migration rollback needed.

### Automated Rollback Triggers

**CRITICAL (Immediate Auto-Rollback)**:
- Memory zeroization loss detected (custom test failure)
- HIGH/CRITICAL SAST findings in new utility package
- Deallocation error rate >0.5% for 2 minutes
- Memory leak detected (Valgrind reports in CI/CD)

**HIGH (Investigate + Manual Rollback)**:
- Integration test failures in auth/crypto/session subsystems
- P95 latency >+50% baseline
- Audit log volume >5× baseline (potential infinite loop)
- Security event volume >3× baseline

**MEDIUM (Monitor + Decide)**:
- P95 latency +20-50% baseline (acceptable under load)
- Minor SAST findings (MEDIUM severity, advisory only)
- Performance degradation within SLA (investigate but don't rollback)

---

## 7. Testing Strategy

### Test Pyramid

```
┌─────────────────┐
│  E2E: 6 tests   │  5% - Critical user journeys (auth, crypto, session)
├─────────────────┤
│ Integration: 18 │  15% - Service boundaries (auth/crypto/session)
├─────────────────┤
│ Security: 24    │  30% - Memory zeroization, SAST, Valgrind
├─────────────────┤
│ Unit: 80 tests  │  50% - Deallocation utility package logic
└─────────────────┘
```

**Coverage Targets**:
- Unit: 95%+ for deallocation utility, 80%+ for changed client code
- Integration: 100% of crypto/auth/session boundaries
- Security: 100% memory zeroization verification, 100% audit log validation
- E2E: Critical paths only (auth flow, crypto operations, session lifecycle)

### 5-Layer Testing Approach

**Layer 1: Compilation Tests** (5 min)
- [ ] Builds successfully with new deallocation utility package
- [ ] No new compiler warnings (Ada 2012 standard, GNAT warnings enabled)
- [ ] Static analysis passes (AdaControl, GNAT static analyzer)
- [ ] Generic instantiation type safety verified

**Layer 2: Unit Tests** (15 min)
- [ ] All existing unit tests pass (baseline: 1,247 tests)
- [ ] New unit tests for deallocation utility package (80 tests, 95%+ coverage)
- [ ] Mutation testing (≥90% mutation score for security-critical code)
- [ ] Memory leak tests (Valgrind: 0 leaks in unit test suite)

**Layer 3: Integration Tests** (25 min)
- [ ] Crypto subsystem integration (5 tests: key lifecycle, buffer management)
- [ ] Auth subsystem integration (6 tests: credential lifecycle, ACL management)
- [ ] Session subsystem integration (3 tests: token lifecycle, cache eviction)
- [ ] Cross-module workflows (4 tests: end-to-end auth + crypto + session)

**Layer 4: Security Regression Tests** (20 min)
- [ ] SAST comparison (baseline vs post-refactor, 0 new CRITICAL, ≤5 new HIGH)
- [ ] Memory zeroization verification (24 tests, 1 per type):
  - Allocate → Populate → Deallocate → Dump memory → Verify all-zeros
- [ ] Valgrind memory safety (0 leaks, 0 invalid reads/writes)
- [ ] Dependency CVE scan (0 CRITICAL, ≤3 HIGH)
- [ ] Audit log validation (100% CRITICAL type deallocations logged)

**Layer 5: E2E Smoke Tests** (15 min)
- [ ] Critical user journeys (login → crypto operation → logout)
- [ ] Performance regression check (P95/P99 ≤baseline +20%)
- [ ] Deployment smoke test (canary 1% → verify metrics)
- [ ] Chaos engineering (kill service → verify graceful degradation)

**Total Test Time**: 80 minutes (parallelized: 30 minutes wall-clock time)

### Pass/Fail Criteria

**Test Execution PASSES If**:
- ✅ All 5 layers complete successfully
- ✅ SAST findings ≤ baseline (0 new CRITICAL, ≤5 new HIGH)
- ✅ Security test coverage ≥95% (crypto/auth/session)
- ✅ Mutation score ≥90% (security-critical), ≥80% (general)
- ✅ P95 latency ≤baseline +20%, P99 ≤baseline +25%
- ✅ Memory zeroization verified (100% success rate, 24/24 tests pass)
- ✅ Valgrind clean (0 leaks, 0 errors)

**Test Execution FAILS If** (Rollback Triggered):
- ❌ Memory zeroization loss detected (any test failure in Layer 4)
- ❌ New HIGH/CRITICAL SAST findings
- ❌ Integration test failures (any failure in Layer 3)
- ❌ P95 performance >baseline +50%
- ❌ Memory leaks detected (Valgrind reports any leaks)
- ❌ Audit log coverage <100% for CRITICAL types

---

## 8. Timeline & Milestones

### Phase Breakdown

**Phase 0: Planning & Preparation** (Week 1: 5 days)
- [x] RDB draft created (this document)
- [ ] Domain expert consultations (2.5 hours total):
  - [ ] @crypto_team_lead - crypto buffer lifecycle (1h)
  - [ ] @auth_architect - ACL reference counting (1h)
  - [ ] @session_expert - session token in-use flag (30min)
- [ ] @security_verification draft RDB review (48h SLA)
- [ ] Task 5: Security baseline capture (4h) - **BLOCKING**
- [ ] RDB finalized with Task 5 findings incorporated
- [ ] Final approval gate

**Phase 1: Implementation** (Week 2: 5 days)
- [ ] Day 1-3: Create deallocation utility package + unit tests
- [ ] Day 3-4: Migrate CRITICAL instances (3) + unit tests
- [ ] Day 4-5: Migrate HIGH instances (10) + integration tests
- [ ] @security_verification checkpoint 3 review (2h, after Day 4)
- [ ] PR created

**Phase 2: Validation** (Week 3: 5 days)
- [ ] Day 1-2: 5-layer test suite execution (80 min runtime, iterate on failures)
- [ ] Day 2-3: Security regression testing (SAST, Valgrind, zeroization verification)
- [ ] Day 3-4: Performance validation (benchmark suite, P95/P99 analysis)
- [ ] Day 4-5: @security_verification checkpoint 4 review (2h) + fixes
- [ ] Final checkpoint 5 review + SRN (4h)
- [ ] PR approved and merged

**Phase 3: Deployment** (Week 4: 7+ days)
- [ ] Day 1-2: Canary 1% (24-48h bake, monitor metrics)
- [ ] Day 3-4: Canary 10% (48h bake, audit log comparison)
- [ ] Day 5-6: Canary 50% (48-72h bake, chaos engineering)
- [ ] Day 7+: Canary 100% (2-week production bake time)
- [ ] Week 5-6: Final metrics captured, retrospective conducted

**Total Timeline**: 4 weeks implementation + 2 weeks production bake = 6 weeks total

### Milestone Gates

**Gate 1: Design Approval** ✅
- RDB approved by @code_architect + @security_verification
- Task 5 baseline complete (4h scan)
- Domain expert consultations complete
- **Criteria**: All BLOCKING security findings resolved, hidden properties documented

**Gate 2: Implementation Complete** ✅
- All code changes committed (utility package + 13 instances migrated)
- Unit + integration tests pass (1,327 tests total)
- **Criteria**: Security checkpoints 3 & 4 passed, PR approved

**Gate 3: Validation Complete** ✅
- 5-layer test suite passes (80 min runtime)
- Security Review Note (SRN) issued
- **Criteria**: All pass/fail criteria met, 0 CRITICAL/HIGH findings

**Gate 4: Production Deployed** ✅
- 100% traffic on new deallocation utility
- 2-week bake time complete (monitoring, metrics)
- **Criteria**: Zero production incidents, metrics within targets, audit trail validated

---

## 9. Ownership & Responsibilities

### Team Assignments

**@code_architect (Design & Oversight)**:
- [x] RDB creation (this document)
- [x] Architecture design (centralized utility package)
- [ ] Security invariants documentation (after Task 5 scan)
- [ ] Risk assessment (this section)
- [ ] Final sign-off (after SRN approval)

**@code_refactor (Implementation)**:
- [ ] Deallocation utility package implementation
- [ ] Migration of 13 instances (3 CRITICAL + 10 HIGH)
- [ ] Unit test creation (80 tests)
- [ ] PR creation and code reviews
- [ ] Bug fixes during validation phase

**@test_stabilize (Testing & Validation)**:
- [ ] Test strategy definition (5-layer approach)
- [ ] Test infrastructure setup (Valgrind, zeroization verification)
- [ ] Test execution (80 min runtime, 1,327 tests)
- [ ] Performance monitoring (P95/P99, canary metrics)
- [ ] Metrics reporting (weekly updates during deployment)

**@security_verification (Security Review)**:
- [ ] Task 5: Security baseline capture (4h) - **BLOCKING for implementation**
- [ ] Security review checkpoint 1: Draft RDB (48h SLA)
- [ ] Security review checkpoint 3: Mid-implementation (2h)
- [ ] Security review checkpoint 4: Pre-deployment (2h)
- [ ] Security review checkpoint 5: Final SRN (4h)
- [ ] Incident response (if security regression detected)

**Stakeholders**:
- **Product Owner**: Approval for 4-week timeline, resource allocation
- **Compliance Team**: SOC2/GDPR audit trail validation

### Communication Plan

**Status Updates**:
- **Frequency**: Daily during implementation (Weeks 2-3), Weekly during deployment (Week 4+)
- **Channel**: AX messages board (Refactor cell)
- **Format**:
  - Current phase/step
  - Blockers (if any)
  - Next steps (next 24h)
  - Metrics (test pass rate, coverage, performance)

**Escalation Path**:
1. **Team-level issues**: Discuss among @code_architect, @code_refactor, @test_stabilize, @security_verification (resolve within 24h)
2. **Cross-team blockers**: Escalate to Tech Lead (if domain expert unavailable or Task 5 findings require scope change)
3. **Executive decisions**: Escalate to CTO (if P0/P1 security finding requires refactor cancellation or major timeline change)

---

## 10. Success Metrics

### Technical Metrics

**Code Quality**:
- **Code duplication**: 73 instances → 1 utility package (98.6% reduction) ✅ TARGET
- **Test coverage**: Deallocation utility 95%+ (security-critical), client code 80%+ ✅ TARGET
- **Mutation score**: ≥90% auth/crypto/session, ≥80% general code ✅ TARGET

**Performance**:
- **Deallocation latency**: ≤baseline ±5% (acceptable: slight increase due to zeroization + logging) ✅ TARGET
- **P50 latency**: No regression (baseline ±5%) ✅ TARGET
- **P95 latency**: ≤baseline +20% (acceptable under load) ✅ TARGET
- **P99 latency**: ≤baseline +25% (acceptable tail latency) ✅ TARGET
- **Memory usage**: No increase (zeroization is in-place, no additional allocations) ✅ TARGET

**Security**:
- **Memory zeroization coverage**: 15% → 100% for CRITICAL/HIGH types ✅ TARGET
- **SAST findings**: ≤baseline (0 new CRITICAL, ≤5 new HIGH) ✅ TARGET
- **Security test coverage**: ≥95% crypto/auth/session ✅ TARGET
- **Mutation score**: ≥90% security-critical code ✅ TARGET
- **Audit log coverage**: 0% → 100% for CRITICAL type deallocations ✅ TARGET
- **CVE count**: 0 CRITICAL, ≤3 HIGH (dependency scan) ✅ TARGET

**Reliability**:
- **Deallocation error rate**: <0.1% (same as baseline) ✅ TARGET
- **Deployment success rate**: 100% (canary deployment with rollback) ✅ TARGET
- **Rollback incidents**: 0 (feature flag + canary tested) ✅ TARGET
- **Memory leaks**: 0 (Valgrind verification) ✅ TARGET

### Business Metrics

**Delivery**:
- **Timeline adherence**: 4 weeks planned (6 weeks total with bake time)
- **Budget**: 160 hours estimated (4 weeks × 40h) ✅ ON BUDGET

**Quality**:
- **Production incidents**: 0 within 2-week bake time ✅ TARGET
- **Rollback events**: 0 ✅ TARGET
- **Customer impact**: None (behavior-preserving refactor) ✅ TARGET

### Definition of Done

**Technical DoD**:
- ✅ Deallocation utility package merged to main branch
- ✅ All 13 instances (3 CRITICAL + 10 HIGH) migrated
- ✅ All tests passing (1,327 tests: 1,247 existing + 80 new)
- ✅ Test coverage targets met (95% utility, 80% client code)
- ✅ SAST findings within acceptable thresholds (0 CRITICAL, ≤5 HIGH)
- ✅ Performance metrics within targets (P95 ≤+20%, P99 ≤+25%)
- ✅ Memory zeroization verified (100% success rate, 24/24 tests)
- ✅ Valgrind clean (0 leaks, 0 errors)
- ✅ Security Review Note (SRN) issued
- ✅ Documentation updated (package spec, client migration guide)

**Process DoD**:
- ✅ All 5 security review checkpoints passed
- ✅ Canary deployment complete (1% → 10% → 50% → 100%)
- ✅ 2-week bake time passed with no incidents
- ✅ Rollback capability validated (feature flag tested, <2min rollback)
- ✅ Retrospective conducted and learnings documented

**Compliance DoD**:
- ✅ Audit trail complete (RDB, ADR, SRN, test results, deployment logs)
- ✅ Audit log coverage met (100% CRITICAL type deallocations)
- ✅ SOC2/GDPR evidence (no credential leakage in memory dumps)

---

## 11. Dependencies & Blockers

### Prerequisites (Must Complete Before Starting)

**Blocking Dependencies**:
- [ ] **Task 5 security baseline** - @security_verification - 4h - **BLOCKING for implementation**
- [ ] **Domain expert consultations** (2.5h total):
  - [ ] @crypto_team_lead - crypto buffer lifecycle (1h)
  - [ ] @auth_architect - ACL reference counting (1h)
  - [ ] @session_expert - session token in-use flag (30min)
- [ ] **Draft RDB review** - @security_verification - 48h SLA
- [ ] **Test infrastructure setup** - @test_stabilize - 2 days:
  - Valgrind integration in CI/CD
  - Memory zeroization verification test framework
  - Feature flag infrastructure (if not already available)

### External Dependencies

**Third-Party Services**:
- **None** - This refactor is self-contained, no external service dependencies

**Tooling**:
- **Valgrind** - Memory leak detection, already available in CI/CD
- **AdaControl** - Static analysis, already available
- **GNAT** - Ada compiler, already available
- **Feature flag service** - Already available (used in other refactors)

### Known Blockers

**Current Blockers**:
- **Blocker 1**: Task 5 security baseline not yet started
  - **Owner**: @security_verification
  - **Resolution Plan**: Kick off Task 5 immediately after RDB approval (this document)
  - **Timeline**: 4 hours (unblocks implementation by Week 1, Day 4)

**Potential Blockers**:
- **Risk 1**: Domain experts unavailable for consultations
  - **Mitigation**: Schedule consultations in advance (Week 1, Day 1), have backup experts identified
- **Risk 2**: Task 5 scan discovers P0/P1 security findings requiring scope change
  - **Mitigation**: Build 2-day buffer in Week 1 for scope adjustment, escalate to Tech Lead if major change needed

---

## 12. Documentation & Artifacts

### Deliverables

**Design Documents**:
- [x] This RDB (RDB-003)
- [ ] Architecture Decision Record (ADR-004) - "Centralized Memory Deallocation with Zeroization" (if approved)
- [ ] Security Invariants Registry update (after Task 5 scan)
- [ ] Data flow diagrams (memory lifecycle: allocate → use → zeroize → deallocate)

**Implementation Artifacts**:
- [ ] Deallocation utility package (`/lib/deallocation/secure_deallocation.ads`, `.adb`)
- [ ] Unit tests (80 tests)
- [ ] Integration tests (18 tests)
- [ ] Security tests (24 zeroization verification tests)
- [ ] Migration guide for client code (how to instantiate generic)
- [ ] PR link (to be created in Week 2)

**Testing Artifacts**:
- [ ] Test strategy document (5-layer approach)
- [ ] Test execution results (1,327 tests)
- [ ] Coverage reports (95% utility, 80% client code)
- [ ] SAST scan results (baseline vs post-refactor comparison)
- [ ] Valgrind reports (0 leaks, 0 errors)
- [ ] Performance benchmarks (P50/P95/P99, before/after comparison)

**Security Artifacts**:
- [ ] Task 5: Security baseline report
- [ ] Security Review Note (SRN)
- [ ] Security Invariants Registry (updated with 13 new instances)
- [ ] Memory zeroization verification test results (24/24 pass)

**Operational Artifacts**:
- [ ] Deployment runbook (canary rollout steps, feature flag toggle)
- [ ] Rollback procedures (4-layer rollback guide)
- [ ] Monitoring dashboards (deallocation error rate, memory usage, audit log volume)
- [ ] Incident response plan (if memory zeroization regression detected)

### Knowledge Transfer

**Documentation Updates**:
- [ ] Deallocation utility package specification (`secure_deallocation.ads` comments)
- [ ] Client migration guide (how to replace manual deallocation with utility)
- [ ] Security annotations in code (which types require zeroization, why)
- [ ] README update (`/lib/deallocation/README.md` - new package overview)

**Training**:
- [ ] Team walkthrough (1-hour session, Week 1, after RDB approval)
  - Present design, security requirements, migration path
  - Q&A with domain experts (@crypto_team_lead, @auth_architect, @session_expert)
- [ ] Security patterns demo (30-min session, Week 2, before implementation)
  - Memory zeroization best practices
  - Compiler barrier usage
  - Audit logging integration
- [ ] Recorded session for future team members (link in RDB)

---

## 13. Lessons Learned & Retrospective

**To Be Completed After Refactor (Week 6)**

### What Went Well

- [To be filled after completion]

### What Could Be Improved

- [To be filled after completion]

### Action Items for Future Refactors

- [ ] [To be filled after completion]

### Hidden Security Properties Discovered

**New Invariants Found** (Add to Security Invariants Registry):
- [To be filled during Task 5 scan and implementation]

**"Magic" Code Explained**:
- [To be filled during domain expert consultations]

---

## Appendices

### Appendix A: Architecture Diagrams

**Current State (Before)**:
```
┌─────────────────────────────────────────────────────────────────┐
│                     73 Scattered Instances                       │
├─────────────────────────────────────────────────────────────────┤
│ Crypto (5)    Auth (5)     Session (3)    ... 60 more           │
│ ┌─────────┐  ┌─────────┐  ┌─────────┐                          │
│ │ Manual  │  │ Manual  │  │ Manual  │   ❌ 85% missing         │
│ │ zeroize │  │ zeroize │  │ zeroize │      zeroization         │
│ │ Free    │  │ Free    │  │ Free    │   ❌ No audit trail      │
│ └─────────┘  └─────────┘  └─────────┘   ❌ Inconsistent        │
└─────────────────────────────────────────────────────────────────┘
```

**Target State (After)**:
```
┌─────────────────────────────────────────────────────────────────┐
│                 Centralized Deallocation Utility                 │
├─────────────────────────────────────────────────────────────────┤
│                 /lib/deallocation/secure_deallocation            │
│                 ┌─────────────────────────────────┐             │
│                 │ Generic Secure_Free             │             │
│                 │  - Automatic zeroization        │             │
│                 │  - Audit logging                │             │
│                 │  - Error handling               │             │
│                 │  - Type-safe                    │             │
│                 └─────────────────────────────────┘             │
│                           ▲                                      │
│          ┌────────────────┼────────────────┐                    │
│          │                │                │                    │
│  ┌───────┴──────┐  ┌──────┴──────┐  ┌──────┴──────┐            │
│  │ Crypto (5)   │  │ Auth (5)    │  │ Session (3) │            │
│  │ Free_Key     │  │ Free_Cred   │  │ Free_Token  │            │
│  │ Free_Buffer  │  │ Free_ACL    │  │ Free_Cache  │            │
│  └──────────────┘  └─────────────┘  └─────────────┘            │
│                                                                  │
│  ✅ 100% zeroization    ✅ Centralized audit    ✅ Type-safe    │
└─────────────────────────────────────────────────────────────────┘
```

### Appendix B: Memory Lifecycle Diagram

```
┌─────────────────────────────────────────────────────────────────┐
│                    Secure Memory Lifecycle                       │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  1. Allocate                                                     │
│     Ptr := new T;                                               │
│                                                                  │
│  2. Use                                                          │
│     Crypto.Encrypt(Key => Ptr.all, ...);                        │
│                                                                  │
│  3. Zeroize (BEFORE deallocation)                               │
│     Zeroize(Ptr.all);  -- Overwrite with zeros                  │
│     ──────────────────────────────────                          │
│     Compiler barrier here (prevent optimization removal)         │
│                                                                  │
│  4. Deallocate                                                   │
│     Free(Ptr);  -- Ada.Unchecked_Deallocation                   │
│                                                                  │
│  5. Audit Log (if CRITICAL type)                                │
│     Log("AES_Key deallocated at " & Timestamp);                 │
│                                                                  │
│  Result: Zero credential leakage in memory dumps                 │
└─────────────────────────────────────────────────────────────────┘
```

### Appendix C: Security Invariants Registry

Link to `/security_verification/security-invariants-registry.yaml` (to be updated after Task 5 scan)

### Appendix D: Performance Benchmarks

**Baseline Metrics** (to be captured during Week 1):
- Deallocation latency (CRITICAL types): TBD
- Deallocation latency (HIGH types): TBD
- P50/P95/P99 system latency: TBD

### Appendix E: References

**Related Documents**:
- ADR-004: Centralized Memory Deallocation with Zeroization (to be created if approved)
- SECURITY-INSIGHTS-SUMMARY.md: Security vision and requirements
- RDB-TEMPLATE.md: Template used for this RDB

**External References**:
- Ada 2012 Language Reference Manual (LRM): Unchecked Deallocation
- OWASP: Memory Management Best Practices
- SOC2 Trust Services Criteria: CC6.6 (Logical and Physical Access Controls)

---

## Approval & Sign-Off

### Draft RDB Review

**Reviewer**: @security_verification
**Review Date**: TBD (48h SLA after submission)
**Status**: PENDING
**Feedback**: [To be provided by @security_verification]

### Final RDB Approval

**Approver**: @code_architect
**Approval Date**: TBD (after security review + Task 5 scan)
**Status**: DRAFT
**Conditions**:
- Task 5 baseline scan complete
- BLOCKING security findings resolved
- Domain expert consultations complete

### Security Review Note (SRN)

**Reviewer**: @security_verification
**Review Date**: TBD (Week 3, after validation)
**SRN ID**: SRN-003
**Status**: PENDING
**Link**: [To be provided after final review]

### Final Sign-Off

**Stakeholder**: Product Owner
**Sign-Off Date**: TBD (after 100% deployment + 2-week bake)
**Status**: PENDING

---

**Document Version**: 1.0
**Last Updated**: 2025-11-05
**Status**: DRAFT
