# Refactor Design Brief (RDB) - Phase 2 GIOP Protocol Consolidation

**RDB ID**: RDB-005
**Title**: GIOP Protocol Version Deduplication and Consolidation
**Author**: @code_architect
**Date**: 2025-11-06
**Status**: DRAFT

---

## Executive Summary

Deduplicate 200-300 lines of CORBA GIOP (General Inter-ORB Protocol) version-specific code across three implementation files (GIOP 1.0, 1.1, 1.2) by extracting common logic into a shared utility module and using the Strategy pattern for version-specific behavior. This refactor eliminates code duplication, simplifies protocol version maintenance, and provides a clean extension point for future GIOP versions.

**Key Points**:
- **Goal**: Reduce 200-300 LOC duplication to <50 LOC through consolidation
- **Scope**: 3 files (giop_1_0.adb, giop_1_1.adb, giop_1_2.adb) + 1 new utility module
- **Timeline**: 5 weeks (Design 1w, Protocol Diff 1w, Implementation 2w, Validation 1w)
- **Risk Level**: HIGH (protocol implementation, wire format compatibility, multiple versions)
- **Security Impact**: SECURITY-CRITICAL (affects all CORBA message handling)

---

## 1. Context & Motivation

### Current State (Problems)

**Key Issues**:
- **Issue 1**: Massive code duplication - 200-300 LOC shared across giop_1_0.adb, giop_1_1.adb, giop_1_2.adb
- **Issue 2**: Maintenance burden - Bug fixes require changes in 3 places, increasing defect risk
- **Issue 3**: Version evolution complexity - Adding GIOP 1.3 would require duplicating 200+ LOC again
- **Issue 4**: Inconsistent implementations - Subtle differences in duplicated code lead to version-specific bugs

**Current Implementation Structure**:
```ada
-- giop_1_0.adb (GIOP version 1.0)
procedure Process_Request is
  -- 200 lines of logic
  -- 80% shared with 1.1 and 1.2
  -- 20% version-specific
end Process_Request;

-- giop_1_1.adb (GIOP version 1.1)
procedure Process_Request is
  -- 200 lines of logic (90% copy-paste from 1.0)
  -- Minor differences for 1.1 features
end Process_Request;

-- giop_1_2.adb (GIOP version 1.2)
procedure Process_Request is
  -- 200 lines of logic (90% copy-paste from 1.0/1.1)
  -- Additional features for 1.2
end Process_Request;
```

**Impact of NOT Refactoring**:
- **Business impact**: High defect rate - bugs fixed in one version but not others (historical evidence)
- **Technical debt accumulation**: 300 LOC becomes 400+ when GIOP 1.3 added
- **Security risks**:
  - Inconsistent validation across versions (P1 finding in security audit)
  - Protocol-level vulnerabilities in duplicated code affect all versions
  - Message parsing bugs can lead to denial of service or data corruption
- **Maintenance cost**: 3x effort for every protocol change, bug fix, or enhancement

### Desired State (Goals)

**Measurable Outcomes**:
- **Goal 1**: Reduce code duplication from 200-300 LOC to <50 LOC (>80% reduction)
- **Goal 2**: Centralize common protocol logic in single utility module (100% of shared code)
- **Goal 3**: Establish Strategy pattern for version-specific behavior (clean extension point)
- **Goal 4**: Zero behavior change (wire format compatibility maintained across all versions)

**Success Criteria**:
- ✅ Shared logic extracted to common module (1 implementation serves 3 versions)
- ✅ Version-specific differences isolated in strategy implementations
- ✅ Contract tests pass for all 3 GIOP versions (interoperability validated)
- ✅ Zero new HIGH/CRITICAL SAST findings
- ✅ Performance within ±5% baseline for each version
- ✅ Security Review Note (SRN) approval from @security_verification

---

## 2. Scope & Non-Goals

### In Scope

**Phase 2B (This RDB) - GIOP Protocol Consolidation**:

**Modules/Services Affected**:
- **Primary**: `/src/giop/giop_1_0.adb` (GIOP 1.0 implementation)
- **Primary**: `/src/giop/giop_1_1.adb` (GIOP 1.1 implementation)
- **Primary**: `/src/giop/giop_1_2.adb` (GIOP 1.2 implementation)
- **New**: `/src/giop/giop_common.adb` (shared utility module)
- **New**: `/src/giop/giop_strategy.ads` (strategy interface)
- **Dependent**: Any modules using GIOP protocol handling (~20-30 estimated)

**Change Types**:
- [x] Code structure (extraction of common logic, Strategy pattern)
- [ ] API contracts (NO changes - internal implementation detail)
- [x] Data models (strategy interface types)
- [ ] Infrastructure (NO deployment changes)
- [ ] Dependencies (NO new external dependencies)

**Specific Changes**:
1. Extract 200-300 LOC of common logic to `giop_common.adb`
2. Define `GIOP_Strategy` interface for version-specific behavior
3. Implement strategy instances for GIOP 1.0, 1.1, 1.2
4. Refactor version-specific files to delegate to common module + strategy
5. Maintain wire format compatibility for all GIOP versions

### Out of Scope (Non-Goals)

**Explicitly Excluded**:
- **Non-goal 1**: TypeCode enumeration (separate Phase 2 option, RDB-004 - already complete)
- **Non-goal 2**: GIOP 1.3 implementation (future work, after consolidation validated)
- **Non-goal 3**: Performance optimization beyond "no regression" requirement
- **Non-goal 4**: Complete GIOP/CORBA redesign (out of scope for consolidation refactor)

**Rationale for Exclusions**:
- **TypeCode**: Already planned separately in RDB-004
- **GIOP 1.3**: Validate consolidation pattern first, then extend to new versions
- **Performance optimization**: Current performance acceptable; focus on maintainability
- **Complete redesign**: Too large; incremental improvement safer

---

## 3. Technical Design

### Current Architecture

```
┌────────────────────────────────────────────────────────────────┐
│  Current State: 3 Duplicated GIOP Implementation Files         │
├────────────────────────────────────────────────────────────────┤
│                                                                 │
│  giop_1_0.adb                 giop_1_1.adb                     │
│  ┌─────────────────┐         ┌─────────────────┐              │
│  │ 200 LOC         │         │ 200 LOC         │              │
│  │ - Parse header  │ 90%     │ - Parse header  │              │
│  │ - Validate      │ SAME    │ - Validate      │              │
│  │ - Process       │ ━━━━━━▶ │ - Process       │              │
│  │ - Marshal       │         │ - Marshal       │              │
│  │ - Error handle  │         │ - Error handle  │              │
│  │                 │         │ + Fragments (1.1)│              │
│  └─────────────────┘         └─────────────────┘              │
│         │                             │                         │
│         │ 80% DUPLICATED              │                         │
│         ▼                             ▼                         │
│  giop_1_2.adb                                                   │
│  ┌─────────────────┐                                            │
│  │ 200 LOC         │                                            │
│  │ - Parse header  │  ⚠️ BUG: Fixed in 1.0, not in 1.1/1.2    │
│  │ - Validate      │  ⚠️ VULNERABILITY: Inconsistent validation│
│  │ - Process       │  ⚠️ MAINTENANCE: 3x effort for changes   │
│  │ - Marshal       │                                            │
│  │ - Error handle  │                                            │
│  │ + Bidirectional │                                            │
│  └─────────────────┘                                            │
│                                                                 │
│  ❌ 200-300 LOC duplicated across 3 files                       │
│  ❌ Bug fixes require 3 separate changes                        │
│  ❌ Inconsistent implementations cause version-specific bugs    │
│  ❌ Adding GIOP 1.3 = copy-paste nightmare                      │
└────────────────────────────────────────────────────────────────┘
```

**Anti-Patterns Identified**:
- **Anti-pattern 1**: Shotgun surgery - Bug fixes require identical changes in 3 files
- **Anti-pattern 2**: Code duplication - 200-300 LOC of near-identical logic across versions
- **Anti-pattern 3**: Divergent implementations - Subtle inconsistencies in "duplicated" code

### Target Architecture

```
┌────────────────────────────────────────────────────────────────┐
│  Target State: Consolidated GIOP with Strategy Pattern         │
├────────────────────────────────────────────────────────────────┤
│                                                                 │
│  giop_common.adb (NEW - Shared Logic)                          │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │  procedure Process_Request(Strategy : GIOP_Strategy) is  │  │
│  │    -- Parse header (common)                              │  │
│  │    -- Validate message (common)                          │  │
│  │    Strategy.Process_Version_Specific();  -- Delegate     │  │
│  │    -- Marshal response (common)                          │  │
│  │    -- Error handling (common)                            │  │
│  │  end Process_Request;                                    │  │
│  │                                                           │  │
│  │  180 LOC of shared logic (extracted from 3 files)        │  │
│  └──────────────────────────────────────────────────────────┘  │
│                      ▲                                          │
│                      │ Delegates to                             │
│                      ▼                                          │
│  giop_strategy.ads (NEW - Strategy Interface)                  │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │  type GIOP_Strategy is interface;                        │  │
│  │  procedure Process_Version_Specific(                     │  │
│  │    Self : GIOP_Strategy) is abstract;                    │  │
│  └──────────────────────────────────────────────────────────┘  │
│         ▲                    ▲                    ▲             │
│         │                    │                    │             │
│  ┌──────┴──────┐     ┌──────┴──────┐     ┌──────┴──────┐      │
│  │ GIOP_1_0    │     │ GIOP_1_1    │     │ GIOP_1_2    │      │
│  │ Strategy    │     │ Strategy    │     │ Strategy    │      │
│  │ (20 LOC)    │     │ (30 LOC)    │     │ (40 LOC)    │      │
│  │ - Basic     │     │ - Fragments │     │ - Bidirctnal│      │
│  └─────────────┘     └─────────────┘     └─────────────┘      │
│                                                                 │
│  ✅ 180 LOC shared logic in 1 place                             │
│  ✅ 20-40 LOC version-specific per version                      │
│  ✅ Bug fixes in 1 place benefit all versions                   │
│  ✅ GIOP 1.3 = implement strategy interface only (~40 LOC)      │
│  ✅ Wire format unchanged (CORBA protocol compliance)           │
└────────────────────────────────────────────────────────────────┘
```

**Design Principles Applied**:
- **Principle 1**: Don't Repeat Yourself - 200-300 LOC duplication → single implementation
- **Principle 2**: Strategy Pattern - Version-specific behavior isolated and extensible
- **Principle 3**: Open/Closed Principle - Open for extension (new GIOP versions), closed for modification

### Migration Path

**Approach**: Strangler Fig + Feature Flag (incremental migration with safety net)

**Steps**:

**Step 1: Protocol Diff Analysis + Shared Logic Identification**
- Duration: 5 days (1 week)
- Validation:
  - Complete diff analysis of all 3 GIOP files
  - Shared logic identified (estimated 180 LOC)
  - Version-specific differences documented (estimated 20-40 LOC each)
  - Domain expert review (@giop_expert) complete
- Rollback: N/A (analysis phase, no code changes)

**Step 2: Create Common Module + Strategy Interface**
- Duration: 3 days
- Validation:
  - `giop_common.adb` created with shared logic
  - `giop_strategy.ads` interface defined
  - Compilation succeeds
  - Unit tests for common module (95%+ coverage)
- Rollback: Delete new files, no client changes made yet

**Step 3: Implement Strategy for GIOP 1.0 (Pilot)**
- Duration: 3 days
- Validation:
  - `giop_1_0_strategy.adb` implemented
  - `giop_1_0.adb` refactored to use common module + strategy
  - All GIOP 1.0 tests pass (unit + integration + contract)
  - Feature flag: Can toggle between legacy and new implementation
- Rollback: Feature flag to legacy, strategy code isolated

**Step 4: Implement Strategies for GIOP 1.1 and 1.2**
- Duration: 5 days
- Validation:
  - `giop_1_1_strategy.adb` and `giop_1_2_strategy.adb` implemented
  - Both version files refactored to use common module
  - All tests pass for both versions
  - Feature flag controls all 3 versions
- Rollback: Feature flag to legacy for affected versions

**Step 5: Comprehensive Testing (All Versions)**
- Duration: 3 days
- Validation:
  - Contract tests validate wire format for all 3 versions
  - Cross-version interoperability tests pass
  - Performance benchmarks within targets
  - Security regression tests pass
- Rollback: Feature flag to legacy if any test failures

**Step 6: Remove Legacy Code + Cleanup**
- Duration: 2 days
- Validation:
  - Old duplicated code removed
  - Feature flag removed (new implementation only)
  - Static analysis confirms no dead code
  - Documentation updated
- Rollback: Git revert, reinstate feature flag

---

## 4. Security Analysis

### Security Invariants

**What MUST NOT Break**:

**Protocol Integrity**:
- **Invariant 1**: GIOP wire format MUST remain unchanged for all versions (1.0, 1.1, 1.2)
- **Invariant 2**: Message validation logic MUST be consistent across all versions
- **Invariant 3**: Error handling MUST NOT leak sensitive information in error messages

**Input Validation**:
- **Invariant 4**: All GIOP message headers MUST be validated before processing
- **Invariant 5**: Message size limits MUST be enforced (DoS prevention)
- **Invariant 6**: Protocol version mismatches MUST be rejected with proper error codes

**Authentication & Authorization**:
- **Invariant 7**: Security context MUST be preserved across protocol version handling
- **Invariant 8**: Access control checks MUST occur before message processing

**Data Handling**:
- **Invariant 9**: Message buffers MUST NOT overflow (bounds checking critical)
- **Invariant 10**: Credential data in messages MUST be handled securely (no logging)

### Hidden Security Properties

**⚠️ CRITICAL: Undocumented Security Assumptions That Need Investigation**

**Potential Hidden Invariants**:
- **Property 1**: Version negotiation security - Verify if downgrade attacks are prevented
- **Property 2**: Message ordering assumptions - Check if any version relies on specific message sequences
- **Property 3**: Fragment reassembly security (GIOP 1.1+) - Confirm fragment validation is consistent
- **Property 4**: Bidirectional GIOP security (GIOP 1.2) - Validate callback security model

**Domain Experts to Consult**:
- **@giop_expert** - Domain: GIOP/CORBA protocol implementation, 10+ years experience
- **@corba_security_expert** - Domain: CORBA security architecture, original security design
- **@protocol_maintainer** - Domain: Protocol versioning and compatibility, knows all edge cases

**"Magic" Code Requiring Investigation**:
- `giop_1_0.adb:*` - Version-specific validation logic differences
- `giop_1_1.adb:*` - Fragment reassembly code (security implications)
- `giop_1_2.adb:*` - Bidirectional protocol handling (callback security)
- Version negotiation code - Potential downgrade attack vectors

**Pre-Refactor Actions Required**:
- [ ] Consult domain experts on hidden protocol properties
- [ ] @security_verification runs comprehensive baseline scan (4 hours - HIGH priority)
- [ ] Protocol diff analysis includes security-critical differences
- [ ] Document findings in Security Invariants section above

### Security Enhancements

**Opportunistic Hardening**:

**In-Scope Enhancements** (Low regression risk):
- **Enhancement 1**: Consistent input validation across all versions (fix inconsistencies found)
- **Enhancement 2**: Centralized message size limit enforcement (currently scattered)
- **Enhancement 3**: Improved error handling (no information leakage)

**Out-of-Scope Enhancements** (Defer to separate security sprint):
- **Enhancement 4**: Protocol downgrade attack prevention (requires protocol redesign)
- **Enhancement 5**: Enhanced audit logging for all GIOP operations (separate infrastructure)

**Rationale for In-Scope Enhancements**:
- Consistent validation: Natural consequence of consolidation, fixes existing vulnerability
- Centralized limits: Required for clean common module design
- Error handling: Part of consolidation, minimal additional risk

### Security Test Requirements

**Mandatory Security Testing**:

**Prevention (Before Deployment)**:
- [ ] SAST baseline comparison (0 new CRITICAL, ≤5 new HIGH findings)
- [ ] Protocol fuzzing for all 3 versions (input validation verification)
- [ ] Message size limit testing (DoS prevention validation)
- [ ] Security test coverage (≥95% for protocol parsing, validation, error handling)

**Detection (During Testing)**:
- [ ] Security integration tests (auth context, access control, error handling)
- [ ] Protocol interoperability tests (prevent version confusion attacks)
- [ ] Mutation testing (≥90% for security-critical paths)

**Response (Post-Deployment)**:
- [ ] Monitoring for protocol-level errors (should remain at baseline)
- [ ] Security event monitoring (protocol violations, DoS attempts)
- [ ] Rollback capability validated (100% success rate)

**Compliance Testing**:
- [ ] CORBA specification compliance (all 3 GIOP versions)
- [ ] Wire format validation (byte-for-byte compatibility)

### Security Review Checkpoints

**@security_verification Review Schedule**:

**Checkpoint 1: Draft RDB Review** (48h SLA - HIGH priority)
- **Timing**: After this RDB is complete
- **Artifacts**: Draft RDB, GIOP consolidation design
- **Expected Output**: BLOCKING findings or APPROVED with conditions
- **Turnaround**: 48 hours (HIGH risk level)

**Checkpoint 2: Protocol Diff Analysis Review** (4h)
- **Timing**: After Step 1 complete (protocol diff analysis)
- **Artifacts**: Diff analysis report, security-critical differences identified
- **Expected Output**: Validation of shared vs version-specific logic separation
- **Turnaround**: 4 hours

**Checkpoint 3: Pre-Implementation Baseline** (4h)
- **Timing**: Before Step 2 (implementation starts) - **BLOCKING**
- **Artifacts**: SAST baseline, protocol fuzzing baseline
- **Expected Output**: Security baseline report
- **Turnaround**: 4 hours

**Checkpoint 4: Mid-Implementation Review** (4h)
- **Timing**: After Step 3 (GIOP 1.0 strategy complete)
- **Artifacts**: Unit tests, integration tests, contract tests for 1.0
- **Expected Output**: Early feedback on implementation approach
- **Turnaround**: 4 hours

**Checkpoint 5: Pre-Deployment Review** (4h)
- **Timing**: After Step 5 (all versions tested)
- **Artifacts**: All test results, SAST comparison, protocol fuzzing results
- **Expected Output**: Validation of security boundaries
- **Turnaround**: 4 hours

**Checkpoint 6: Final Security Sign-Off** (4h)
- **Timing**: After all tests pass, before deployment
- **Artifacts**: Complete test results, security regression validation
- **Expected Output**: Security Review Note (SRN-005) - formal approval
- **Turnaround**: 4 hours (2h if zero issues)

**Total Security Review Time**: ~24 hours (HIGH risk level - protocol layer)

---

## 5. Risk Assessment & Mitigation

### Risk Matrix

| Risk | Likelihood | Impact | Severity | Mitigation |
|------|------------|--------|----------|------------|
| Wire format incompatibility breaks CORBA interoperability | MEDIUM | CRITICAL | P0 | Protocol diff analysis + comprehensive contract testing + feature flag |
| Inconsistent behavior across GIOP versions | MEDIUM | HIGH | P1 | Rigorous testing per version + cross-version validation + domain expert review |
| Performance regression >10% for any version | MEDIUM | HIGH | P1 | Baseline benchmarks + P95/P99 monitoring per version + early performance testing |
| Hidden protocol invariant breaks functionality | MEDIUM | HIGH | P1 | Domain expert consultation + protocol diff analysis + extensive testing |
| Security vulnerability in consolidated code | LOW | CRITICAL | P1 | Security review checkpoints + protocol fuzzing + SAST + mutation testing |
| Version downgrade attack vector introduced | LOW | HIGH | P2 | Protocol security analysis + interoperability testing |

**Risk Scoring**: Severity = Likelihood (1-5) × Impact (1-5)
- 20-25: P0 (critical - drop everything)
- 12-19: P1 (high - address immediately)
- 6-11: P2 (medium - plan mitigation)
- 1-5: P3 (low - monitor)

### Security-Specific Risks

**P0/P1 Security Risks** (Must address before/during refactor):
- **Critical Risk 1**: Wire format incompatibility → Protocol failure (P0)
  - **Mitigation**: Byte-level wire format validation, contract testing, feature flag for rollback
- **High Risk 1**: Inconsistent validation across versions → Security vulnerability (P1)
  - **Mitigation**: Protocol diff analysis identifies inconsistencies, consolidation fixes them
- **High Risk 2**: Input validation bypass → DoS or buffer overflow (P1)
  - **Mitigation**: Protocol fuzzing, security-focused unit tests, SAST analysis

**P2/P3 Security Risks** (Accept with mitigation):
- **Medium Risk 1**: Version negotiation weakness → Potential downgrade attacks (P2)
  - **Mitigation**: Document current behavior, add to future enhancement backlog (out of scope)
- **Medium Risk 2**: Information leakage in error messages (P2)
  - **Mitigation**: Review error handling in consolidated code, sanitize messages

### Blast Radius

**Affected Components**:
- **GIOP 1.0 handler**: HIGH impact (core protocol)
- **GIOP 1.1 handler**: HIGH impact (core protocol)
- **GIOP 1.2 handler**: HIGH impact (core protocol)
- **All CORBA clients/servers**: HIGH impact (protocol layer affects all communication)

**User Impact**:
- **Users affected**: All users (protocol layer is fundamental)
- **Affected workflows**: All CORBA communication
- **Downtime required**: NONE (rolling deployment with feature flag)

**Rollback Complexity**:
- **MEDIUM**: Feature flag toggle (instant), or git revert + redeploy (10-15min)
- No database schema changes
- No data migration required
- Feature flag provides instant rollback per version

---

## 6. Rollback Strategy

**Multi-Layer Rollback Defense**:

### Layer 1: Feature Flag (Per-Version Control)

```ada
-- Feature flag per GIOP version for granular control
type GIOP_Implementation is (Legacy, Consolidated);

GIOP_1_0_Mode : GIOP_Implementation := Consolidated;
GIOP_1_1_Mode : GIOP_Implementation := Consolidated;
GIOP_1_2_Mode : GIOP_Implementation := Consolidated;

-- Each version can be independently rolled back
if GIOP_1_0_Mode = Legacy then
  -- Use old giop_1_0.adb implementation
else
  -- Use new giop_common + strategy
end if;
```

**Capabilities**:
- Per-version rollback (if issue affects only GIOP 1.2, rollback that version only)
- Instant toggle (<2min)
- Runtime switchable (no recompilation)
- Automatic rollback on error thresholds

### Layer 2: Incremental Migration (Natural Rollback Points)

Each step in migration path is independently reversible:
- **Step 2 rollback**: Delete new modules, no client changes
- **Step 3 rollback**: Feature flag GIOP 1.0 to legacy, 1.1/1.2 unaffected
- **Step 4 rollback**: Feature flag affected versions to legacy
- **Step 6 rollback**: Git revert, reinstate feature flag

### Layer 3: Deployment Rollback (Standard)

```bash
# Git revert to previous commit
git revert <commit-hash>

# Rebuild and redeploy
make clean && make && deploy
```

**Rollback time**: 10-15 minutes (recompile + redeploy)

### Automated Rollback Triggers

**CRITICAL (Immediate Auto-Rollback)**:
- Contract test failures (protocol interoperability lost)
- Wire format validation failures
- Security events >10× baseline
- Error rate >1% for any GIOP version

**HIGH (Investigate + Manual Rollback)**:
- Integration test failures
- P95 latency >+25% baseline for any version
- Protocol-level errors >2× baseline

**MEDIUM (Monitor + Decide)**:
- P95 latency +10-25% baseline
- Minor SAST findings (MEDIUM severity)
- Protocol warnings increased

---

## 7. Testing Strategy

### Test Pyramid

```
┌─────────────────┐
│  E2E: 3 tests   │  5% - CORBA end-to-end per version
├─────────────────┤
│ Integration: 15 │  20% - Protocol integration
├─────────────────┤
│ Contract: 30    │  40% - GIOP protocol compliance (10 per version)
├─────────────────┤
│ Unit: 30 tests  │  35% - Common logic + strategies
└─────────────────┘
```

**Coverage Targets**:
- Unit: 95%+ for common module and strategy implementations
- Integration: 100% of protocol handling paths
- Contract: All GIOP message types for all 3 versions
- E2E: Critical CORBA interoperability paths per version

### 5-Layer Testing Approach

**Layer 1: Compilation Tests** (10 min)
- [ ] Builds successfully with new modules
- [ ] No new compiler warnings
- [ ] Static analysis passes (GNAT style checks)
- [ ] All 3 GIOP versions compile successfully

**Layer 2: Unit Tests** (20 min)
- [ ] All existing unit tests pass
- [ ] 30 new tests for common module and strategies
- [ ] 95%+ coverage for refactored code
- [ ] Mutation score ≥85% for protocol logic

**Layer 3: Integration Tests** (30 min)
- [ ] 15 integration tests for protocol handling
- [ ] All 3 GIOP versions tested independently
- [ ] Cross-version compatibility validated
- [ ] Error handling integration tests

**Layer 4: Contract Tests** (45 min)
- [ ] 30 contract tests (10 per GIOP version)
- [ ] Wire format validation for all message types
- [ ] Interoperability with external CORBA systems
- [ ] Protocol compliance verified

**Layer 5: E2E Smoke Tests** (15 min)
- [ ] 3 E2E tests (1 per GIOP version)
- [ ] Critical CORBA operations end-to-end
- [ ] Performance regression check per version

**Total Test Time**: 120 minutes (2 hours)

### Pass/Fail Criteria

**Test Execution PASSES If**:
- ✅ All layers complete successfully
- ✅ Contract tests confirm wire format unchanged for all versions
- ✅ All GIOP versions independently validated
- ✅ P95 latency within +10% baseline per version, P99 within +15%
- ✅ No compilation warnings or errors
- ✅ SAST findings ≤ baseline (0 new CRITICAL, ≤5 new HIGH)

**Test Execution FAILS If** (Rollback Triggered):
- ❌ Contract test failures (wire format broken for any version)
- ❌ Compilation failures
- ❌ P95 performance >+25% baseline for any version
- ❌ Integration test failures
- ❌ Security regression (new CRITICAL findings)

---

## 8. Timeline & Milestones

### Phase Breakdown

**Phase 0: Planning & Preparation** (1 week)
- [x] RDB draft created
- [ ] Domain expert consultation (@giop_expert, @corba_security_expert) - 8h
- [ ] @security_verification draft RDB review (48h SLA)
- [ ] @security_verification baseline scan (4h) - **BLOCKING**
- [ ] RDB finalized and approved

**Phase 1: Protocol Analysis** (1 week)
- [ ] Complete diff analysis of all 3 GIOP files (3 days)
- [ ] Identify shared logic (~180 LOC) (2 days)
- [ ] Document version-specific differences (2 days)
- [ ] @security_verification protocol diff review (4h)

**Phase 2: Implementation** (2 weeks)
- [ ] Create common module + strategy interface (3 days)
- [ ] Implement GIOP 1.0 strategy (pilot) (3 days)
- [ ] @security_verification mid-implementation review (4h)
- [ ] Implement GIOP 1.1 and 1.2 strategies (5 days)
- [ ] Remove legacy code + cleanup (2 days)
- [ ] PR created

**Phase 3: Validation** (1 week)
- [ ] 5-layer test suite execution (2 days)
- [ ] Contract testing (all 3 versions) (2 days)
- [ ] Performance validation (per version) (1 day)
- [ ] Protocol fuzzing + security tests (1 day)
- [ ] @security_verification final review + SRN (4h)
- [ ] PR approved and merged

**Phase 4: Deployment** (Gradual rollout)
- [ ] Deploy with feature flag (all versions = legacy initially)
- [ ] Enable GIOP 1.0 consolidated (monitor 48h)
- [ ] Enable GIOP 1.1 consolidated (monitor 48h)
- [ ] Enable GIOP 1.2 consolidated (monitor 48h)
- [ ] Remove feature flag after successful bake time

**Total Timeline**: 5 weeks

### Milestone Gates

**Gate 1: Design Approval** ✅
- RDB approved by @code_architect
- Protocol diff analysis complete
- Security baseline captured
- **Criteria**: All BLOCKING findings resolved

**Gate 2: Pilot Complete** ✅
- GIOP 1.0 strategy implemented and tested
- Feature flag functional
- Mid-implementation security review passed
- **Criteria**: GIOP 1.0 contract tests pass, performance within targets

**Gate 3: Implementation Complete** ✅
- All 3 GIOP versions refactored
- All code changes committed
- Feature flag controls all versions
- **Criteria**: Compilation succeeds, unit tests pass

**Gate 4: Validation Complete** ✅
- 5-layer test suite passes
- Contract tests validate all 3 versions
- Security Review Note (SRN-005) issued
- **Criteria**: All pass/fail criteria met

**Gate 5: Production Deployed** ✅
- All 3 versions running consolidated code
- Feature flag removed
- 1-week bake time complete
- **Criteria**: Zero incidents, metrics within targets

---

## 9. Ownership & Responsibilities

### Team Assignments

**@code_architect (Design & Oversight)**:
- [x] RDB creation and approval
- [ ] Architecture design and strategy pattern
- [ ] Protocol diff analysis oversight
- [ ] Risk assessment
- [ ] Final sign-off

**@code_refactor (Implementation)**:
- [ ] Protocol diff analysis execution
- [ ] Common module implementation
- [ ] Strategy implementations (all 3 versions)
- [ ] PR creation and reviews
- [ ] Feature flag implementation

**@test_stabilize (Testing & Validation)**:
- [ ] Test strategy execution
- [ ] Contract test development (per version)
- [ ] Performance monitoring (per version)
- [ ] Protocol fuzzing
- [ ] Test results reporting

**@security_verification (Security Review)**:
- [ ] Security baseline capture (4h)
- [ ] Draft RDB review (48h)
- [ ] Protocol diff security analysis (4h)
- [ ] Mid-implementation review (4h)
- [ ] Pre-deployment review (4h)
- [ ] Final security review + SRN-005 (4h)
- [ ] Protocol security validation

**Domain Experts**:
- **@giop_expert**: Protocol behavior validation, hidden invariants
- **@corba_security_expert**: Security architecture review
- **@protocol_maintainer**: Version compatibility validation

### Communication Plan

**Status Updates**:
- **Frequency**: Daily during Phases 2-3 (implementation & validation)
- **Channel**: AX messages board
- **Format**: Brief status, blockers, next steps, per-version progress

**Escalation Path**:
1. **Team-level issues**: Discuss among agents
2. **Domain expert questions**: Direct consultation (8h scheduled)
3. **Technical blockers**: Escalate to Tech Lead
4. **Security concerns**: Immediate escalation to @security_verification

---

## 10. Success Metrics

### Technical Metrics

**Code Quality**:
- Code duplication: 200-300 LOC → <50 LOC (>80% reduction)
- Cyclomatic complexity: Baseline → ≤Baseline (consolidated logic simpler)
- Test coverage: 95%+ for refactored code
- Mutation score: ≥85% for protocol logic

**Performance** (Per GIOP Version):
- P50 latency: Baseline → ≤+5% (expected no change)
- P95 latency: Baseline → ≤+10% (acceptable)
- P99 latency: Baseline → ≤+15% (acceptable)
- Throughput: Baseline (no regression)

**Security**:
- SAST findings: Baseline → ≤Baseline (0 new CRITICAL/HIGH)
- Protocol fuzzing: Zero crashes, no new vulnerabilities
- Contract test coverage: 30/30 tests pass (10 per version)
- Security test coverage: ≥95% for protocol parsing/validation

**Reliability**:
- Protocol error rate: Baseline (no increase)
- Compilation success rate: 100%
- Test pass rate: 100% (all 5 layers)
- Deployment success rate: 100%

### Business Metrics

**Delivery**:
- Timeline adherence: 5 weeks planned → Actual (TBD)
- Effort: 160 hours estimated → Actual (TBD)

**Quality**:
- Production incidents: 0 (expected - behavior-preserving refactor)
- Rollback events: 0 (expected)
- Version-specific bugs: Reduction expected (from consolidated code)

### Definition of Done

**Technical DoD**:
- ✅ 200-300 LOC duplication reduced to <50 LOC
- ✅ Common module + strategy pattern implemented
- ✅ All 3 GIOP versions refactored
- ✅ All tests passing (unit, integration, contract, E2E) for all versions
- ✅ Contract tests validate wire format for all versions
- ✅ Performance metrics within targets for all versions
- ✅ Security Review Note (SRN-005) issued
- ✅ Documentation updated

**Process DoD**:
- ✅ All security review checkpoints passed
- ✅ Domain expert consultations complete
- ✅ Protocol diff analysis complete and reviewed
- ✅ Feature flag removed after successful bake time
- ✅ Retrospective conducted

---

## 11. Dependencies & Blockers

### Prerequisites (Must Complete Before Starting)

**Blocking Dependencies**:
- [ ] Domain expert consultation (@giop_expert, @corba_security_expert) - 8 hours
- [ ] @security_verification baseline scan - 4 hours
- [ ] @code_architect RDB approval - 48 hours (HIGH priority review)
- [ ] Protocol diff analysis - 1 week (Phase 1)

### External Dependencies

**Tooling**:
- GNAT Ada compiler (available)
- Contract testing framework (available)
- CORBA test harness (available)
- Protocol fuzzing tools (may need setup)

### Known Blockers

**Current Blockers**:
- None identified

**Potential Blockers**:
- **Risk 1**: Domain experts unavailable → Mitigation: Schedule early, allow 2-week window
- **Risk 2**: Protocol diff reveals unexpected complexity → Mitigation: 1 week allocated for thorough analysis
- **Risk 3**: Hidden protocol invariants discovered → Mitigation: Feature flag allows safe experimentation

---

## 12. Documentation & Artifacts

### Deliverables

**Design Documents**:
- [x] This RDB (RDB-005)
- [ ] Protocol diff analysis report (Phase 1 deliverable)
- [ ] Strategy pattern design document
- [ ] Migration guide for future GIOP versions

**Implementation Artifacts**:
- [ ] Code changes (PR link - TBD)
- [ ] Common module (`giop_common.adb`)
- [ ] Strategy interface (`giop_strategy.ads`)
- [ ] Strategy implementations (3 files)
- [ ] Feature flag implementation

**Testing Artifacts**:
- [ ] Test execution results (all 5 layers)
- [ ] Contract test results (30 tests, 10 per version)
- [ ] Coverage reports (95%+ target)
- [ ] Performance benchmarks (per version)
- [ ] Protocol fuzzing results

**Security Artifacts**:
- [ ] Security baseline report
- [ ] Protocol diff security analysis
- [ ] Security Review Note (SRN-005)
- [ ] SAST comparison report
- [ ] Protocol fuzzing security findings

### Knowledge Transfer

**Documentation Updates**:
- [ ] Inline code comments (strategy pattern usage)
- [ ] PolyORB developer guide (GIOP consolidation pattern)
- [ ] Migration guide for GIOP 1.3 (future)
- [ ] Architecture diagrams (current → target)

**Training**:
- [ ] Team walkthrough (1-hour session before implementation)
- [ ] Strategy pattern demonstration
- [ ] Protocol diff analysis presentation

---

## 13. Lessons Learned & Retrospective

**To Be Completed After Refactor**

### What Went Well

- [TBD after completion]

### What Could Be Improved

- [TBD after completion]

### Action Items for Future Refactors

- [ ] [TBD after completion]

### Pattern Reusability

**This Pattern Applies To**:
- Other protocol version implementations in PolyORB
- Version-specific code consolidation across codebase
- Strategy pattern for extensible version handling

---

## Appendices

### Appendix A: GIOP Version Differences Summary

**GIOP 1.0** (Basic CORBA):
- Standard request/response
- Synchronous only
- No message fragmentation

**GIOP 1.1** (Fragments):
- All GIOP 1.0 features
- **+ Message fragmentation** (large messages split into chunks)
- Fragment reassembly logic

**GIOP 1.2** (Bidirectional):
- All GIOP 1.1 features
- **+ Bidirectional GIOP** (callbacks, both client and server can initiate)
- **+ Additional message types** (LocateRequest, LocateReply enhancements)

**Estimated Code Distribution**:
- Shared logic (all versions): ~180 LOC (80%)
- GIOP 1.0 specific: ~20 LOC (basic features)
- GIOP 1.1 specific: ~30 LOC (fragmentation)
- GIOP 1.2 specific: ~40 LOC (bidirectional + enhanced locate)

### Appendix B: Architecture Diagrams

```
┌───────────────────────────────────────────────────────────┐
│                  CORBA Application Layer                   │
├───────────────────────────────────────────────────────────┤
│                             ↕                              │
│                   GIOP Common Module                       │
│         (Shared: parsing, validation, marshaling)          │
│                             ↕                              │
├───────────────────────────────────────────────────────────┤
│                    GIOP Strategy Layer                     │
│   ┌───────────┐    ┌───────────┐    ┌───────────┐        │
│   │ GIOP 1.0  │    │ GIOP 1.1  │    │ GIOP 1.2  │        │
│   │ Strategy  │    │ Strategy  │    │ Strategy  │        │
│   └───────────┘    └───────────┘    └───────────┘        │
│        ↕                 ↕                 ↕               │
├───────────────────────────────────────────────────────────┤
│              Wire Format (CORBA/GIOP Protocol)             │
│         (Binary protocol - version-specific encoding)      │
└───────────────────────────────────────────────────────────┘
```

### Appendix C: Protocol Diff Analysis Template

**To be completed in Phase 1**:

```markdown
# GIOP Protocol Diff Analysis

## Shared Logic (Common Module)
- [ ] Request header parsing
- [ ] Response header parsing
- [ ] Message validation (common checks)
- [ ] Error handling (common paths)
- [ ] Marshaling utilities
- [ ] Unmarshaling utilities
- [ ] Buffer management
- [ ] Connection handling

## GIOP 1.0 Specific
- [ ] Version 1.0 header format
- [ ] Basic request/response only
- [ ] [TBD - to be identified in Phase 1]

## GIOP 1.1 Specific
- [ ] Fragment header handling
- [ ] Fragment reassembly logic
- [ ] Fragment validation
- [ ] [TBD - to be identified in Phase 1]

## GIOP 1.2 Specific
- [ ] Bidirectional protocol setup
- [ ] Callback handling
- [ ] Enhanced LocateRequest/Reply
- [ ] [TBD - to be identified in Phase 1]
```

### Appendix D: References

**Related Documents**:
- RDB-004: TypeCode Enumeration (Phase 2A - completed separately)
- RDB-003: Phase 1 Deallocation (demonstrates migration pattern)

**External References**:
- CORBA Specification: GIOP Protocol (OMG standard)
- GIOP 1.0, 1.1, 1.2 Specifications
- PolyORB Documentation: Protocol layer architecture

---

## Approval & Sign-Off

### Draft RDB Review

**Reviewer**: @security_verification
**Review Date**: [Pending]
**Status**: [PENDING]
**Feedback**: [To be provided - 48h SLA due to HIGH risk]

### Final RDB Approval

**Approver**: @code_architect
**Approval Date**: 2025-11-06
**Status**: ✅ DRAFT COMPLETE (Pending reviews)
**Conditions**: Subject to domain expert consultation + protocol diff analysis + security baseline

### Security Review Note (SRN)

**Reviewer**: @security_verification
**Review Date**: [To be scheduled]
**SRN ID**: SRN-005
**Status**: [PENDING]
**Link**: [To be created]

---

**Document Version**: 1.0
**Last Updated**: 2025-11-06
**Status**: DRAFT
