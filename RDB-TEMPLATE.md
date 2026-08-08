# Refactor Design Brief (RDB) Template

**RDB ID**: RDB-XXX
**Title**: [Concise refactor goal]
**Author**: @code_architect
**Date**: YYYY-MM-DD
**Status**: [DRAFT | REVIEW | APPROVED | IN PROGRESS | COMPLETE]

---

## Executive Summary

[2-3 sentence overview of what's being refactored and why]

**Key Points**:
- **Goal**: [Primary objective]
- **Scope**: [Number of files/modules/services affected]
- **Timeline**: [X weeks/months]
- **Risk Level**: [LOW | MEDIUM | HIGH]
- **Security Impact**: [STANDARD | SECURITY-CRITICAL]

---

## 1. Context & Motivation

### Current State (Problems)

[Describe the current situation and what's wrong]

**Key Issues**:
- [Issue 1: e.g., Code smell - god class with 3,000 LOC]
- [Issue 2: e.g., Performance - P95 latency 500ms, target <200ms]
- [Issue 3: e.g., Maintainability - 73 instances of duplicated deallocation logic]

**Impact of NOT Refactoring**:
- [Business impact]
- [Technical debt accumulation]
- [Security risks]

### Desired State (Goals)

[Describe what success looks like after the refactor]

**Measurable Outcomes**:
- [Goal 1 with metric: e.g., Reduce cyclomatic complexity by 30%]
- [Goal 2 with metric: e.g., P95 latency <200ms]
- [Goal 3 with metric: e.g., Centralize 73 instances into 1 utility package]

**Success Criteria**:
- ✅ [Criterion 1]
- ✅ [Criterion 2]
- ✅ [Criterion 3]

---

## 2. Scope & Non-Goals

### In Scope

[What will be changed]

**Modules/Services Affected**:
- [Module 1: /path/to/module.ext]
- [Module 2: /path/to/module.ext]
- [Service 3: service-name]

**Change Types**:
- [ ] Code structure (classes, functions, modules)
- [ ] API contracts (endpoints, interfaces)
- [ ] Data models (schemas, types)
- [ ] Infrastructure (deployment, configuration)
- [ ] Dependencies (libraries, frameworks)

### Out of Scope (Non-Goals)

[What will NOT be changed]

**Explicitly Excluded**:
- [Non-goal 1: e.g., Defer UI widget deallocations to Phase 1B]
- [Non-goal 2: e.g., No changes to authentication logic]
- [Non-goal 3: e.g., Performance optimization (separate effort)]

**Rationale for Exclusions**:
- [Why each non-goal is deferred/excluded]

---

## 3. Technical Design

### Current Architecture

```
[Diagram or description of current state]

Example:
┌─────────────────────────────────────┐
│  Component A (god class, 3K LOC)    │
│  - Responsibility 1                 │
│  - Responsibility 2                 │
│  - Responsibility 3                 │
└─────────────────────────────────────┘
```

**Anti-Patterns Identified**:
- [Anti-pattern 1: e.g., God class]
- [Anti-pattern 2: e.g., Shotgun surgery - 73 duplicated instances]
- [Anti-pattern 3: e.g., Leaky abstraction]

### Target Architecture

```
[Diagram or description of target state]

Example:
┌──────────────────┐     ┌──────────────────┐
│  Component A     │────▶│  Utility Package │
│  (focused, clean)│     │  (centralized)   │
└──────────────────┘     └──────────────────┘
```

**Design Principles Applied**:
- [Principle 1: e.g., Single Responsibility]
- [Principle 2: e.g., Don't Repeat Yourself]
- [Principle 3: e.g., Separation of Concerns]

### Migration Path

[Step-by-step transition from current to target state]

**Approach**: [Incremental | Big-Bang | Strangler Fig | Feature Flag]

**Steps**:
1. **Step 1**: [Description]
   - Duration: [X hours/days]
   - Validation: [How to verify success]
   - Rollback: [How to revert if needed]

2. **Step 2**: [Description]
   - Duration: [X hours/days]
   - Validation: [How to verify success]
   - Rollback: [How to revert if needed]

3. **Step 3**: [Description]
   - Duration: [X hours/days]
   - Validation: [How to verify success]
   - Rollback: [How to revert if needed]

---

## 4. Security Analysis

### Security Invariants

**What MUST NOT Break**:

**Authentication & Authorization**:
- [Invariant 1: e.g., All external requests MUST pass through Security.Authenticate]
- [Invariant 2: e.g., ACL checks MUST occur server-side, never client-side]

**Cryptography**:
- [Invariant 3: e.g., All crypto keys MUST be zeroized before deallocation]
- [Invariant 4: e.g., TLS 1.2+ required for all network communication]

**Memory Safety**:
- [Invariant 5: e.g., Credential types MUST use Controlled types or manual zeroization]
- [Invariant 6: e.g., No credential data in crash dumps or logs]

**Audit & Compliance**:
- [Invariant 7: e.g., All auth failures MUST be logged with timestamp/source]
- [Invariant 8: e.g., Privileged operations MUST be logged]

**Data Handling**:
- [Invariant 9: e.g., PII data must be encrypted at rest]
- [Invariant 10: e.g., Session tokens must be cryptographically random (≥128 bits)]

### Hidden Security Properties

**⚠️ CRITICAL: Undocumented Security Assumptions That Need Investigation**

**Potential Hidden Invariants**:
- [Property 1: e.g., Session token deallocation - Check if "in-use" flag is required]
- [Property 2: e.g., Crypto buffer cleanup - Verify if buffers are ever cached between operations]
- [Property 3: e.g., ACL inheritance - Confirm parent-child relationship assumptions]

**Domain Experts to Consult**:
- [@expert1] - [Domain: e.g., session management, 5 years on this code]
- [@expert2] - [Domain: e.g., crypto subsystem, original implementer]
- [@expert3] - [Domain: e.g., ACL design, knows all edge cases]

**"Magic" Code Requiring Investigation**:
- [File:Line] - [Description: e.g., Code that "just works" without clear reason]
- [File:Line] - [Description: e.g., Subtle timing dependencies]
- [File:Line] - [Description: e.g., Implicit state management]

**Pre-Refactor Actions Required**:
- [ ] Consult domain experts on hidden properties
- [ ] @security_verification runs Task 5 baseline scan (4 hours)
- [ ] Document findings in Security Invariants section above

### Security Enhancements

**Opportunistic Hardening (Clearly Marked as New Behavior)**:

**In-Scope Enhancements** (Low regression risk, aligns with refactor):
- [Enhancement 1: e.g., Add zeroization to 2 additional HIGH-priority types]
- [Enhancement 2: e.g., Implement audit logging for deallocation failures]

**Out-of-Scope Enhancements** (Defer to separate refactor):
- [Enhancement 3: e.g., Key rotation logic - separate security sprint]
- [Enhancement 4: e.g., Complete crypto subsystem redesign - RDB-XXX]

**Rationale for In-Scope Enhancements**:
- [Why each enhancement is safe to include]

### Security Test Requirements

**Mandatory Security Testing**:

**Prevention (Before Deployment)**:
- [ ] SAST baseline comparison (0 new CRITICAL, ≤5 new HIGH findings)
- [ ] Dependency CVE scan (0 CRITICAL, ≤3 HIGH)
- [ ] Secret scanning (0 secrets in code)
- [ ] Security test coverage (≥95% auth/authz/crypto, ≥85% input validation)

**Detection (During Testing)**:
- [ ] Memory safety testing (Valgrind, zeroization verification)
- [ ] Security integration tests (auth/authz/audit log validation)
- [ ] Mutation testing (≥90% auth/authz/crypto, ≥80% general code)

**Response (Post-Deployment)**:
- [ ] Security monitoring (MTTD <1h CRITICAL, <24h HIGH)
- [ ] Incident response readiness (MTTR <1h CRITICAL, <4h HIGH)
- [ ] Rollback capability validated (100% success rate)

**Compliance Testing** (if applicable):
- [ ] mTLS coverage (100% service-to-service)
- [ ] Authentication coverage (100% API endpoints except /health, /metrics)
- [ ] Audit log coverage (100% auth/authz/crypto operations)

### Security Review Checkpoints

**@security_verification Review Schedule**:

**Checkpoint 1: Draft RDB Review** (24h/48h SLA)
- **Timing**: After this RDB is 70% complete
- **Artifacts**: Draft RDB, affected modules list, architecture diagrams
- **Expected Output**: BLOCKING findings or APPROVED with advisory suggestions
- **Turnaround**: 24h standard, 48h security-critical

**Checkpoint 2: Pre-Implementation Baseline** (4h)
- **Timing**: Before implementation starts (BLOCKING)
- **Artifacts**: Task 5 baseline scan (SAST, coverage, security invariants)
- **Expected Output**: Security baseline report, memory zeroization test scripts
- **Turnaround**: 4 hours

**Checkpoint 3: Mid-Implementation Review** (2h)
- **Timing**: After unit tests complete
- **Artifacts**: Unit test results, mutation testing, initial coverage
- **Expected Output**: Early feedback on test gaps
- **Turnaround**: 2 hours

**Checkpoint 4: Pre-Deployment Review** (2h)
- **Timing**: After integration tests complete
- **Artifacts**: Integration test results, SAST comparison, memory tests
- **Expected Output**: Validation of security boundaries, no regressions
- **Turnaround**: 2 hours

**Checkpoint 5: Final Security Sign-Off** (4h)
- **Timing**: After all tests pass, before deployment
- **Artifacts**: Complete test results, SAST/DAST, threat model update
- **Expected Output**: Security Review Note (SRN) - formal approval
- **Turnaround**: 4 hours (2h if zero issues)

**Total Security Review Time**: ~16 hours spread across refactor lifecycle

---

## 5. Risk Assessment & Mitigation

### Risk Matrix

| Risk | Likelihood | Impact | Severity | Mitigation |
|------|------------|--------|----------|------------|
| [Risk 1: e.g., Hidden security invariant breaks] | MEDIUM | HIGH | P1 | Domain expert consultation + Task 5 scan |
| [Risk 2: e.g., Performance regression >50%] | LOW | MEDIUM | P2 | P95/P99 monitoring + rollback triggers |
| [Risk 3: e.g., Integration test failures] | MEDIUM | MEDIUM | P2 | Comprehensive test suite + canary deployment |

**Risk Scoring**: Risk = Likelihood (1-5) × Impact (1-5) × Exposure (1-5)
- 100-125: P0 (drop everything, fix now)
- 75-99: P1 (fix within 48h)
- 50-74: P2 (fix within 1 week)
- 25-49: P3 (fix within 1 month)
- 1-24: P4 (backlog)

### Security-Specific Risks

**P0/P1 Security Risks** (Must fix before refactor):
- [Critical Risk 1: e.g., Existing auth bypass vulnerability in module]
- [Critical Risk 2: e.g., HIGH-severity CVE in dependency]

**P2/P3 Security Risks** (Accept with mitigation OR fix opportunistically):
- [Medium Risk 1: e.g., Minor info disclosure in logs]
- [Medium Risk 2: e.g., Rate limiting disabled (compensating control: CloudFlare)]

**Risk Acceptance**:
- [If accepting P2/P3 risks, document approval: Product Owner + @security_verification]
- [Time-boxed acceptance period: 30/60/90 days]
- [Compensating controls in place]

### Blast Radius

**Affected Components**:
- [Component 1]: [Impact level: HIGH/MEDIUM/LOW]
- [Component 2]: [Impact level: HIGH/MEDIUM/LOW]
- [Component 3]: [Impact level: HIGH/MEDIUM/LOW]

**User Impact**:
- [Number of users affected]
- [Affected user workflows]
- [Downtime required: NONE | <5min | <30min | PLANNED MAINTENANCE]

**Rollback Complexity**:
- [LOW: Feature flag toggle]
- [MEDIUM: Kubernetes rollback, 5-10min]
- [HIGH: Database schema change, requires migration]

---

## 6. Rollback Strategy

**Multi-Layer Rollback Defense**:

### Layer 1: Feature Flags (Instant - <2min)

```yaml
# Example feature flag configuration
feature_flags:
  new_deallocation_utility:
    enabled: true
    rollout: 100  # 0-100%
    rollback_trigger:
      error_rate: 0.1%
      timeout: 5min
```

**Capabilities**:
- Instant toggle (new code → legacy code)
- Per-service granularity
- Automatic rollback on error thresholds

### Layer 2: Canary Deployment (Gradual)

**Rollout Schedule**:
- **Week 1**: 1% traffic (monitor 24-48h)
  - Rollback trigger: Error rate >0.1%
  - Metrics: Auth failures, ACL violations, latency
- **Week 2**: 10% traffic (monitor 48h)
  - Rollback trigger: P95 latency >+20% baseline
  - Security: Compare logs (old vs new)
- **Week 3**: 50% traffic (monitor 72h)
  - Rollback trigger: Any CRITICAL security event
  - Testing: Chaos engineering
- **Week 4**: 100% traffic (monitor 2 weeks)
  - Bake time: Full observability
  - Final: Security Review Note (SRN)

### Layer 3: Kubernetes Rollback (5-10min)

```bash
# Instant rollback to previous deployment
kubectl rollout undo deployment/[service-name] -n [namespace]

# Verify rollback status
kubectl rollout status deployment/[service-name] -n [namespace]
```

### Layer 4: Database Migration Rollback (If Applicable)

**Expand-Migrate-Contract Pattern**:
- **Expand**: Add new schema (old + new coexist)
- **Migrate**: Copy data incrementally
- **Contract**: Remove old schema ONLY after full cutover
- **Rollback**: Revert to old schema (intact during Expand)

### Automated Rollback Triggers

**CRITICAL (Immediate Auto-Rollback)**:
- Memory zeroization loss detected
- HIGH/CRITICAL SAST findings
- Auth failure rate >2× baseline for 2 minutes
- Error rate >0.5% for 5 minutes

**HIGH (Investigate + Manual Rollback)**:
- Integration test failures
- P95 latency >+50% baseline
- ACL violations detected
- Security event volume >5× baseline

**MEDIUM (Monitor + Decide)**:
- P95 latency +20-50% baseline
- Minor SAST findings (MEDIUM severity)
- Performance degradation within SLA

---

## 7. Testing Strategy

### Test Pyramid

```
┌─────────────────┐
│  E2E: X tests   │  5% - Critical user journeys
├─────────────────┤
│ Integration: Y  │  15% - Service boundaries
├─────────────────┤
│ Contract: Z     │  30% - API contracts (Pact CDC)
├─────────────────┤
│ Unit: N tests   │  50% - Business logic
└─────────────────┘
```

**Coverage Targets**:
- Unit: 95%+ for refactored code, 80%+ for changed code
- Integration: 100% of changed service boundaries
- Contract: All API endpoints involved in refactor
- E2E: Critical paths only (keep <5min total runtime)

### 5-Layer Testing Approach

**Layer 1: Compilation Tests** (X min)
- [ ] Builds successfully with changes
- [ ] No new compiler warnings
- [ ] Static analysis passes (language-specific linters)

**Layer 2: Unit Tests** (Y min)
- [ ] All existing unit tests pass
- [ ] New unit tests for refactored code (95%+ coverage)
- [ ] Mutation testing (≥90% score security-critical, ≥80% general)

**Layer 3: Integration Tests** (Z min)
- [ ] Service boundary tests
- [ ] Database integration (if applicable)
- [ ] External API integration (if applicable)
- [ ] End-to-end flows through refactored code

**Layer 4: Security Regression Tests** (W min)
- [ ] SAST comparison (baseline vs post-refactor)
- [ ] Memory safety (Valgrind, zeroization verification)
- [ ] Security integration tests (auth/authz/audit logs)
- [ ] Dependency CVE scan

**Layer 5: E2E Smoke Tests** (V min)
- [ ] Critical user journeys
- [ ] Performance regression check (P95/P99)
- [ ] Deployment smoke test

**Total Test Time**: [Sum of all layers] minutes

### Pass/Fail Criteria

**Test Execution PASSES If**:
- ✅ All layers complete successfully
- ✅ SAST findings ≤ baseline (0 new CRITICAL, ≤5 new HIGH)
- ✅ Security test coverage ≥ targets
- ✅ Mutation score ≥ thresholds
- ✅ P95 latency within +20% baseline, P99 within +25%
- ✅ Memory zeroization verified (for security-critical types)

**Test Execution FAILS If** (Rollback Triggered):
- ❌ Memory zeroization loss detected
- ❌ New HIGH/CRITICAL SAST findings
- ❌ Integration test failures
- ❌ P95 performance >+50% baseline
- ❌ Security coverage below thresholds

---

## 8. Timeline & Milestones

### Phase Breakdown

**Phase 0: Planning & Preparation** (X days/weeks)
- [ ] RDB draft created
- [ ] Domain expert consultation (hidden properties)
- [ ] @security_verification draft RDB review (24h/48h)
- [ ] Task 5: Security baseline capture (4h) - **BLOCKING**
- [ ] RDB finalized and approved

**Phase 1: Implementation** (Y days/weeks)
- [ ] Refactor code changes
- [ ] Unit tests implemented
- [ ] @security_verification checkpoint 1 review (2h)
- [ ] Integration tests implemented
- [ ] @security_verification checkpoint 2 review (2h)
- [ ] PR created

**Phase 2: Validation** (Z days/weeks)
- [ ] 5-layer test suite execution
- [ ] Security regression testing
- [ ] Performance validation
- [ ] @security_verification final review + SRN (4h)
- [ ] PR approved and merged

**Phase 3: Deployment** (W days/weeks)
- [ ] Canary deployment (1% → 10% → 50% → 100%)
- [ ] Production monitoring (2-week bake time)
- [ ] Final metrics captured
- [ ] Retrospective conducted

**Total Timeline**: [Sum of all phases]

### Milestone Gates

**Gate 1: Design Approval** ✅
- RDB approved by @code_architect
- Task 5 baseline complete
- **Criteria**: All BLOCKING security findings resolved

**Gate 2: Implementation Complete** ✅
- All code changes committed
- Unit + integration tests pass
- **Criteria**: Security checkpoints 1 & 2 passed

**Gate 3: Validation Complete** ✅
- 5-layer test suite passes
- Security Review Note (SRN) issued
- **Criteria**: All pass/fail criteria met

**Gate 4: Production Deployed** ✅
- 100% traffic on new code
- 2-week bake time complete
- **Criteria**: Zero production incidents, metrics within targets

---

## 9. Ownership & Responsibilities

### Team Assignments

**@code_architect (Design & Oversight)**:
- [ ] RDB creation and approval
- [ ] Architecture design
- [ ] Security invariants documentation
- [ ] Risk assessment
- [ ] Final sign-off

**@code_refactor (Implementation)**:
- [ ] Code changes implementation
- [ ] Unit test creation
- [ ] PR creation and reviews
- [ ] Bug fixes during testing

**@test_stabilize (Testing & Validation)**:
- [ ] Test strategy definition
- [ ] Test infrastructure setup
- [ ] Test execution (5 layers)
- [ ] Performance monitoring
- [ ] Metrics reporting

**@security_verification (Security Review)**:
- [ ] Task 5: Security baseline capture (4h)
- [ ] Security review checkpoints (3 during implementation)
- [ ] Final security review + SRN (4h)
- [ ] Security regression validation
- [ ] Incident response (if needed)

**Stakeholders**:
- [Stakeholder 1]: [Role in decision-making]
- [Stakeholder 2]: [Role in approval]

### Communication Plan

**Status Updates**:
- **Frequency**: [Daily/Weekly]
- **Channel**: [AX messages / Slack / Email]
- **Format**: [Brief status, blockers, next steps]

**Escalation Path**:
1. **Team-level issues**: Discuss among @code_architect, @code_refactor, @test_stabilize, @security_verification
2. **Cross-team blockers**: Escalate to [Tech Lead / Manager]
3. **Executive decisions**: Escalate to [CTO / VP Engineering]

---

## 10. Success Metrics

### Technical Metrics

**Code Quality**:
- Cyclomatic complexity: [Current] → [Target] ([X%] reduction)
- Test coverage: [Current] → [Target] ([X%] improvement)
- Code duplication: [Current] → [Target] ([X%] reduction)

**Performance**:
- P50 latency: [Current] → [Target] (no regression)
- P95 latency: [Current] → [Target] (≤+10% acceptable)
- P99 latency: [Current] → [Target] (≤+15% acceptable)
- Throughput: [Current] → [Target] (no regression)

**Security**:
- SAST findings: [Current] → [Target] (0 new CRITICAL, ≤5 new HIGH)
- Security test coverage: [Current] → [Target] (≥95% auth/authz/crypto)
- Mutation score: [Current] → [Target] (≥90% security-critical)
- CVE count: [Current] → [Target] (0 CRITICAL, ≤3 HIGH)

**Reliability**:
- Error rate: [Current] → [Target] (<0.1%)
- Deployment success rate: [Current] → [Target] (≥95%)
- Rollback incidents: 0 (target)

### Business Metrics

**Delivery**:
- Timeline adherence: [X weeks planned] → [Actual weeks] ([On time/Delayed])
- Budget: [Estimated effort] → [Actual effort] ([On budget/Over])

**Quality**:
- Production incidents: 0 within 2-week bake time
- Rollback events: 0
- Customer impact: None (behavior-preserving refactor)

### Definition of Done

**Technical DoD**:
- ✅ All code changes merged to main branch
- ✅ All tests passing (unit, integration, security, E2E)
- ✅ Test coverage targets met
- ✅ SAST findings within acceptable thresholds
- ✅ Performance metrics within targets
- ✅ Security Review Note (SRN) issued
- ✅ Documentation updated (API docs, architecture diagrams)

**Process DoD**:
- ✅ All security review checkpoints passed
- ✅ Canary deployment complete (100% traffic)
- ✅ 2-week bake time passed with no incidents
- ✅ Rollback capability validated
- ✅ Retrospective conducted and learnings documented

**Compliance DoD** (if applicable):
- ✅ Audit trail complete (RDB, ADR, SRN, test results)
- ✅ Compliance metrics met (mTLS, auth, audit logs)
- ✅ Threat model updated

---

## 11. Dependencies & Blockers

### Prerequisites (Must Complete Before Starting)

**Blocking Dependencies**:
- [ ] [Dependency 1: e.g., Task 5 security baseline - @security_verification - 4h]
- [ ] [Dependency 2: e.g., Docker test environment - @test_stabilize - 1 day]
- [ ] [Dependency 3: e.g., Domain expert consultation - @experts - 2h]

### External Dependencies

**Third-Party Services**:
- [Service 1]: [Dependency description, impact if unavailable]
- [Service 2]: [Dependency description, impact if unavailable]

**Tooling**:
- [Tool 1: e.g., Pact Broker for contract testing]
- [Tool 2: e.g., k6 for performance testing]
- [Tool 3: e.g., Trivy/Semgrep for SAST]

### Known Blockers

**Current Blockers**:
- [Blocker 1]: [Description, owner, resolution plan]
- [Blocker 2]: [Description, owner, resolution plan]

**Potential Blockers**:
- [Risk 1]: [Description, mitigation plan]
- [Risk 2]: [Description, mitigation plan]

---

## 12. Documentation & Artifacts

### Deliverables

**Design Documents**:
- [x] This RDB (RDB-XXX)
- [ ] Architecture Decision Record (ADR-XXX) - if architectural change
- [ ] Threat model update (if security-critical refactor)
- [ ] Data flow diagrams (if data handling changes)

**Implementation Artifacts**:
- [ ] Code changes (PR link)
- [ ] Unit tests
- [ ] Integration tests
- [ ] Security tests
- [ ] Migration scripts (if applicable)

**Testing Artifacts**:
- [ ] Test strategy document
- [ ] Test execution results
- [ ] Coverage reports
- [ ] SAST/DAST scan results
- [ ] Performance benchmarks

**Security Artifacts**:
- [ ] Task 5: Security baseline report
- [ ] Security Review Note (SRN)
- [ ] Threat model (updated)
- [ ] Security Invariants Registry (updated)

**Operational Artifacts**:
- [ ] Deployment runbook
- [ ] Rollback procedures
- [ ] Monitoring dashboards
- [ ] Incident response plan

### Knowledge Transfer

**Documentation Updates**:
- [ ] API documentation
- [ ] Architecture diagrams
- [ ] Code comments (security annotations)
- [ ] README files

**Training**:
- [ ] Team walkthrough (1-hour session before implementation)
- [ ] Security patterns demo (for security-critical refactors)
- [ ] Recorded session for future team members

---

## 13. Lessons Learned & Retrospective

**To Be Completed After Refactor**

### What Went Well

- [Item 1]
- [Item 2]
- [Item 3]

### What Could Be Improved

- [Item 1]
- [Item 2]
- [Item 3]

### Action Items for Future Refactors

- [ ] [Action 1]
- [ ] [Action 2]
- [ ] [Action 3]

### Hidden Security Properties Discovered

**New Invariants Found** (Add to Security Invariants Registry):
- [Invariant 1]: [Description, location, domain expert]
- [Invariant 2]: [Description, location, domain expert]

**"Magic" Code Explained**:
- [Code Location]: [Explanation of hidden behavior]

---

## Appendices

### Appendix A: Architecture Diagrams

[Include detailed architecture diagrams]

### Appendix B: Data Flow Diagrams

[Include data flow diagrams, especially for security-critical refactors]

### Appendix C: Threat Model

[Include threat model or link to separate document]

### Appendix D: Security Invariants Registry

[Link to `/security_verification/security-invariants-registry.yaml`]

### Appendix E: Performance Benchmarks

[Include baseline performance metrics]

### Appendix F: References

**Related Documents**:
- ADR-XXX: [Title]
- RDB-YYY: [Related refactor]
- Security Review Note (SRN-ZZZ)

**External References**:
- [Link 1]: [Description]
- [Link 2]: [Description]

---

## Approval & Sign-Off

### Draft RDB Review

**Reviewer**: @security_verification
**Review Date**: YYYY-MM-DD
**Status**: [BLOCKING | APPROVED | APPROVED WITH ADVISORY]
**Feedback**: [Link to review comments or inline notes]

### Final RDB Approval

**Approver**: @code_architect
**Approval Date**: YYYY-MM-DD
**Status**: ✅ APPROVED
**Conditions**: [Any conditions of approval]

### Security Review Note (SRN)

**Reviewer**: @security_verification
**Review Date**: YYYY-MM-DD
**SRN ID**: SRN-XXX
**Status**: [APPROVED | APPROVED WITH CONDITIONS | BLOCKED]
**Link**: [Path to SRN document]

### Final Sign-Off

**Stakeholder**: [Name/Role]
**Sign-Off Date**: YYYY-MM-DD
**Status**: ✅ COMPLETE

---

**Document Version**: 1.0
**Last Updated**: YYYY-MM-DD
**Status**: [DRAFT | IN REVIEW | APPROVED | IN PROGRESS | COMPLETE]
