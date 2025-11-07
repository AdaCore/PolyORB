# Refactor Design Brief (RDB) - Phase 2 TypeCode Enumeration

**RDB ID**: RDB-004
**Title**: TypeCode Constants to Enumeration Consolidation
**Author**: @code_architect
**Date**: 2025-11-06
**Status**: DRAFT

---

## Executive Summary

Replace 40 scattered TypeCode integer constants with a single, type-safe enumeration in the PolyORB CDR representation module. This refactor eliminates magic numbers, prevents invalid TypeCode values, and provides compile-time type checking for CORBA type representation operations.

**Key Points**:
- **Goal**: Replace 40 TypeCode constants with single enumeration type
- **Scope**: 1 file (src/polyorb-representations-cdr.adb, lines 106-143)
- **Timeline**: 3 weeks (Design 1w, Implementation 1w, Validation 1w)
- **Risk Level**: MEDIUM (well-understood domain, single module, established patterns)
- **Security Impact**: STANDARD (no security-critical operations)

---

## 1. Context & Motivation

### Current State (Problems)

**Key Issues**:
- **Issue 1**: Magic numbers - 40 TypeCode constants defined as integer literals scattered across 38 lines
- **Issue 2**: Type safety gap - No compiler enforcement prevents invalid TypeCode values (e.g., `TypeCode := 999`)
- **Issue 3**: Maintainability - Adding new TypeCodes requires manual constant definition + risk of value collision
- **Issue 4**: Code clarity - Integer constants less self-documenting than enumeration (`13` vs `TC_String`)

**Current Implementation** (src/polyorb-representations-cdr.adb:106-143):
```ada
-- TypeCode constants (integer literals)
TC_Null       : constant := 0;
TC_Void       : constant := 1;
TC_Short      : constant := 2;
TC_Long       : constant := 3;
TC_UShort     : constant := 4;
TC_ULong      : constant := 5;
TC_Float      : constant := 6;
TC_Double     : constant := 7;
TC_Boolean    : constant := 8;
TC_Char       : constant := 9;
TC_Octet      : constant := 10;
TC_Any        : constant := 11;
TC_TypeCode   : constant := 12;
TC_String     : constant := 13;
-- ... 26 more constants ...
```

**Impact of NOT Refactoring**:
- **Business impact**: Increased defect rate - invalid TypeCode values accepted by compiler, caught only at runtime
- **Technical debt accumulation**: Pattern spreads to other modules; 40 becomes 80+ constants across codebase
- **Maintenance cost**: Every new CORBA type requires careful coordination to avoid value conflicts

### Desired State (Goals)

**Measurable Outcomes**:
- **Goal 1**: Reduce 40 TypeCode constants to 1 enumeration type (98% consolidation)
- **Goal 2**: Achieve 100% compile-time type safety (invalid TypeCode values rejected)
- **Goal 3**: Zero behavior change (CORBA protocol compatibility maintained)
- **Goal 4**: Improve code clarity with self-documenting enumeration names

**Success Criteria**:
- ✅ All 40 TypeCode constants replaced with enumeration type
- ✅ All compilation units referencing TypeCodes successfully compile
- ✅ Contract tests pass (CORBA interoperability validated)
- ✅ Zero new SAST findings
- ✅ Performance within ±5% baseline (no regression)

---

## 2. Scope & Non-Goals

### In Scope

**Phase 2 (This RDB) - TypeCode Enumeration Consolidation**:

**Modules/Services Affected**:
- **Primary**: `/src/polyorb-representations-cdr.adb` (lines 106-143)
- **Dependent modules**: Any code referencing TypeCode constants (~15 estimated modules)

**Change Types**:
- [x] Code structure (constants → enumeration type)
- [ ] API contracts (NO changes - internal implementation detail)
- [x] Data models (new TypeCode enumeration type)
- [ ] Infrastructure (NO deployment changes)
- [ ] Dependencies (NO new external dependencies)

**Specific Changes**:
1. Define new `TypeCode_Enum` type with 40 values
2. Replace all constant references with enumeration literals
3. Update any case statements to use enumeration
4. Add representation clause to maintain wire format compatibility

### Out of Scope (Non-Goals)

**Explicitly Excluded**:
- **Non-goal 1**: GIOP protocol consolidation (separate Phase 2 option, RDB-005)
- **Non-goal 2**: TypeCode marshaling/unmarshaling optimization
- **Non-goal 3**: Extension to other enumeration candidates in PolyORB
- **Non-goal 4**: CORBA protocol version upgrades

**Rationale for Exclusions**:
- **GIOP consolidation**: Separate, larger refactor (200-300 LOC); validate TypeCode approach first
- **Marshaling optimization**: Performance is acceptable; premature optimization adds risk
- **Other enumerations**: Validate pattern on TypeCode before scaling
- **Protocol upgrades**: Out of scope for code quality refactor

---

## 3. Technical Design

### Current Architecture

```
┌──────────────────────────────────────────────────────────────┐
│  Current State: 40 TypeCode Integer Constants                │
├──────────────────────────────────────────────────────────────┤
│                                                               │
│  src/polyorb-representations-cdr.adb (lines 106-143)         │
│  ┌────────────────────────────────────────────────────────┐  │
│  │  TC_Null       : constant := 0;                        │  │
│  │  TC_Void       : constant := 1;                        │  │
│  │  TC_Short      : constant := 2;                        │  │
│  │  TC_Long       : constant := 3;                        │  │
│  │  TC_UShort     : constant := 4;                        │  │
│  │  ... (35 more constants)                               │  │
│  │                                                         │  │
│  │  -- Usage in case statements:                          │  │
│  │  case TypeCode is                                      │  │
│  │    when 0 => Handle_Null;                             │  │
│  │    when 13 => Handle_String;  -- Magic number! 🤷      │  │
│  │    when 999 => ...  -- Compiler allows! ⚠️             │  │
│  │  end case;                                             │  │
│  └────────────────────────────────────────────────────────┘  │
│                                                               │
│  ❌ Magic numbers in code                                     │
│  ❌ No type safety (999 is valid)                             │
│  ❌ Potential value collisions                                │
└──────────────────────────────────────────────────────────────┘
```

**Anti-Patterns Identified**:
- **Anti-pattern 1**: Magic numbers - Integer literals reduce code clarity
- **Anti-pattern 2**: Primitive obsession - Using `Integer` when enumeration is more expressive
- **Anti-pattern 3**: No type safety - Compiler cannot detect invalid TypeCode values

### Target Architecture

```
┌──────────────────────────────────────────────────────────────┐
│  Target State: Single TypeCode Enumeration Type              │
├──────────────────────────────────────────────────────────────┤
│                                                               │
│  src/polyorb-representations-cdr.ads (NEW/MODIFIED)          │
│  ┌────────────────────────────────────────────────────────┐  │
│  │  type TypeCode_Enum is (                               │  │
│  │    TC_Null,      -- 0                                  │  │
│  │    TC_Void,      -- 1                                  │  │
│  │    TC_Short,     -- 2                                  │  │
│  │    TC_Long,      -- 3                                  │  │
│  │    TC_UShort,    -- 4                                  │  │
│  │    TC_ULong,     -- 5                                  │  │
│  │    TC_Float,     -- 6                                  │  │
│  │    TC_Double,    -- 7                                  │  │
│  │    TC_Boolean,   -- 8                                  │  │
│  │    TC_Char,      -- 9                                  │  │
│  │    TC_Octet,     -- 10                                 │  │
│  │    TC_Any,       -- 11                                 │  │
│  │    TC_TypeCode,  -- 12                                 │  │
│  │    TC_String     -- 13                                 │  │
│  │    -- ... (26 more)                                    │  │
│  │  );                                                     │  │
│  │                                                         │  │
│  │  -- Maintain wire format compatibility                 │  │
│  │  for TypeCode_Enum use (                               │  │
│  │    TC_Null     => 0,                                   │  │
│  │    TC_Void     => 1,                                   │  │
│  │    TC_Short    => 2,                                   │  │
│  │    -- ... (ensures CORBA protocol compliance)          │  │
│  │  );                                                     │  │
│  │                                                         │  │
│  │  -- Usage in case statements:                          │  │
│  │  case TypeCode is                                      │  │
│  │    when TC_Null => Handle_Null;                       │  │
│  │    when TC_String => Handle_String;  -- Clear! ✅      │  │
│  │    when Invalid => ...  -- Compile error! ✅           │  │
│  │  end case;                                             │  │
│  └────────────────────────────────────────────────────────┘  │
│                                                               │
│  ✅ Type-safe (compiler enforced)                             │
│  ✅ Self-documenting code                                     │
│  ✅ No value collisions (compiler managed)                    │
│  ✅ Wire format unchanged (CORBA compatible)                  │
└──────────────────────────────────────────────────────────────┘
```

**Design Principles Applied**:
- **Principle 1**: Strong typing - Use enumeration instead of primitive `Integer`
- **Principle 2**: Self-documenting code - Enumeration names clarify intent
- **Principle 3**: Fail-safe defaults - Compiler rejects invalid values

### Migration Path

**Approach**: Incremental with Compilation Gating (big-bang safe due to single module)

**Steps**:

**Step 1: Define Enumeration Type + Representation Clause**
- Duration: 1 day
- Validation:
  - Compilation succeeds
  - Representation clause tested (wire format unchanged)
  - Contract test confirms CORBA compatibility
- Rollback: Delete enumeration definition, restore constants

**Step 2: Replace Constants with Enumeration in Primary Module**
- Duration: 2 days
- Validation:
  - Compilation succeeds across all dependent units
  - Unit tests pass (95%+ coverage maintained)
  - Case statements use enumeration literals
- Rollback: Git revert to Step 1, constants still available

**Step 3: Update Dependent Modules (Estimated 15 modules)**
- Duration: 3 days
- Validation:
  - All compilation units pass
  - Integration tests pass
  - Contract tests validate CORBA interoperability
- Rollback: Git revert to Step 2, dependent modules unchanged

**Step 4: Remove Old Constants + Final Cleanup**
- Duration: 1 day
- Validation:
  - Compilation with constants removed succeeds
  - Full test suite passes (unit + integration + contract)
  - Static analysis confirms no constant references remain
- Rollback: Reinstate constants, maintain backward compatibility

---

## 4. Security Analysis

### Security Invariants

**What MUST NOT Break**:

**Data Integrity**:
- **Invariant 1**: TypeCode wire format MUST remain unchanged (CORBA protocol compatibility)
- **Invariant 2**: Type marshaling/unmarshaling behavior MUST be identical (byte-for-byte output)

**Input Validation**:
- **Invariant 3**: Invalid TypeCode values MUST be rejected (compile-time or runtime)
- **Invariant 4**: TypeCode bounds checking MUST be maintained in any conversion functions

**No Authentication/Authorization Changes**:
- This refactor does not touch auth/authz logic

### Hidden Security Properties

**⚠️ CRITICAL: Undocumented Security Assumptions That Need Investigation**

**Potential Hidden Invariants**:
- **Property 1**: TypeCode ordering - Verify if any code relies on specific integer ordering of TypeCodes
- **Property 2**: TypeCode arithmetic - Check if any code performs mathematical operations on TypeCode values
- **Property 3**: TypeCode serialization - Confirm wire format representation clause is sufficient

**Domain Experts to Consult**:
- **@polyorb_expert** - Domain: CORBA/PolyORB implementation, 10+ years experience
- **@cdr_maintainer** - Domain: CDR marshaling subsystem, original implementer

**"Magic" Code Requiring Investigation**:
- `src/polyorb-representations-cdr.adb:106-143` - Verify no hidden assumptions about TypeCode values
- Any case statements on TypeCode values - Check for exhaustiveness assumptions
- TypeCode conversion functions - Validate representation clause coverage

**Pre-Refactor Actions Required**:
- [ ] Consult domain experts on TypeCode usage patterns
- [ ] @security_verification baseline scan (standard, non-critical module)
- [ ] Document findings in Security Invariants section above

### Security Enhancements

**Opportunistic Hardening**:

**In-Scope Enhancements** (Low regression risk):
- **Enhancement 1**: Add compile-time exhaustiveness checking (all TypeCode values covered in case statements)
- **Enhancement 2**: Add runtime bounds checking for any TypeCode conversions from external input

**Out-of-Scope Enhancements**:
- **Enhancement 3**: Input validation framework for CORBA messages (separate security sprint)

**Rationale for In-Scope Enhancements**:
- Exhaustiveness checking: Natural consequence of enumeration type, zero additional risk
- Bounds checking: Defensive programming, minimal code change

### Security Test Requirements

**Mandatory Security Testing**:

**Prevention (Before Deployment)**:
- [ ] SAST baseline comparison (0 new CRITICAL, ≤5 new HIGH findings)
- [ ] Compilation exhaustiveness check (all case statements cover all TypeCode values)

**Detection (During Testing)**:
- [ ] Contract tests (CORBA interoperability with external systems)
- [ ] Type safety validation (compiler rejects invalid TypeCode values)

**Response (Post-Deployment)**:
- [ ] Monitoring for TypeCode-related errors (should remain at baseline)
- [ ] Rollback capability validated (100% success rate)

### Security Review Checkpoints

**@security_verification Review Schedule**:

**Checkpoint 1: Draft RDB Review** (24h SLA)
- **Timing**: After this RDB is complete
- **Artifacts**: Draft RDB, TypeCode enumeration design
- **Expected Output**: APPROVED (standard risk level)
- **Turnaround**: 24 hours

**Checkpoint 2: Pre-Implementation Baseline** (2h)
- **Timing**: Before implementation starts
- **Artifacts**: Baseline SAST scan
- **Expected Output**: Security baseline report
- **Turnaround**: 2 hours

**Checkpoint 3: Final Security Sign-Off** (2h)
- **Timing**: After all tests pass, before deployment
- **Artifacts**: Test results, SAST comparison, contract tests
- **Expected Output**: Security Review Note (SRN) - approval
- **Turnaround**: 2 hours (standard review)

**Total Security Review Time**: ~6 hours (reduced from standard 16h due to MEDIUM risk)

---

## 5. Risk Assessment & Mitigation

### Risk Matrix

| Risk | Likelihood | Impact | Severity | Mitigation |
|------|------------|--------|----------|------------|
| Wire format incompatibility breaks CORBA protocol | LOW | HIGH | P1 | Representation clause + contract tests + external interop validation |
| Dependent module compilation failures | MEDIUM | MEDIUM | P2 | Incremental migration + comprehensive build testing |
| Performance regression from enum operations | LOW | LOW | P3 | Baseline benchmarks + P95/P99 monitoring |
| Hidden TypeCode arithmetic breaks functionality | LOW | MEDIUM | P2 | Domain expert consultation + code audit for arithmetic operations |

**Risk Scoring**: Severity = Likelihood (1-5) × Impact (1-5)
- 20-25: P1 (critical)
- 12-19: P2 (important)
- 5-11: P3 (normal)
- 1-4: P4 (low)

### Security-Specific Risks

**P2/P3 Security Risks** (Accept with mitigation):
- **Medium Risk 1**: Wire format change breaks interoperability → Mitigated by representation clause + contract tests
- **Medium Risk 2**: Invalid TypeCode handling differs → Mitigated by comprehensive testing of edge cases

**Risk Acceptance**:
- No P0/P1 security risks identified
- P2 risks mitigated through testing and domain expert consultation

### Blast Radius

**Affected Components**:
- **Primary**: polyorb-representations-cdr module (MEDIUM impact)
- **Dependent**: ~15 modules referencing TypeCode constants (LOW impact each)
- **External**: CORBA clients/servers (NO impact if wire format maintained)

**User Impact**:
- **Users affected**: 0 (internal refactor, no user-facing changes)
- **Affected workflows**: None (behavior-preserving refactor)
- **Downtime required**: NONE (rolling deployment)

**Rollback Complexity**:
- **LOW**: Git revert, recompile, redeploy (5-10min)
- No database schema changes
- No data migration required

---

## 6. Rollback Strategy

**Multi-Layer Rollback Defense**:

### Layer 1: Incremental Migration (Natural Rollback Points)

Each step in migration path is independently reversible:
- **Step 1 rollback**: Delete enumeration, constants still present
- **Step 2 rollback**: Revert primary module, dependent modules unchanged
- **Step 3 rollback**: Revert dependent modules, primary module stable
- **Step 4 rollback**: Reinstate constants, maintain backward compatibility

### Layer 2: Feature Flag (If Needed)

```ada
-- Conditional compilation if phased rollout desired
if Use_TypeCode_Enum then
  -- New enumeration path
else
  -- Legacy constant path
end if;
```

**Not required for this refactor** (compile-time change, all-or-nothing), but available as safety net.

### Layer 3: Deployment Rollback (Standard)

```bash
# Git revert to previous commit
git revert <commit-hash>

# Rebuild and redeploy
make clean && make && deploy
```

**Rollback time**: <10 minutes (recompile + redeploy)

### Automated Rollback Triggers

**CRITICAL (Immediate Manual Rollback)**:
- Contract test failures (CORBA interoperability lost)
- Compilation failures in production build pipeline

**HIGH (Investigate + Manual Rollback)**:
- Integration test failures
- P95 latency >+25% baseline

**MEDIUM (Monitor + Decide)**:
- Minor SAST findings (MEDIUM severity)
- P95 latency +10-25% baseline

---

## 7. Testing Strategy

### Test Pyramid

```
┌─────────────────┐
│  E2E: 2 tests   │  5% - CORBA end-to-end scenarios
├─────────────────┤
│ Integration: 8  │  20% - Module integration
├─────────────────┤
│ Contract: 10    │  25% - CORBA protocol compliance
├─────────────────┤
│ Unit: 20 tests  │  50% - TypeCode operations
└─────────────────┘
```

**Coverage Targets**:
- Unit: 95%+ for TypeCode enumeration usage
- Integration: 100% of dependent module boundaries
- Contract: All 40 TypeCode values validated in wire format
- E2E: Critical CORBA interoperability paths

### 5-Layer Testing Approach

**Layer 1: Compilation Tests** (5 min)
- [ ] Builds successfully with enumeration type
- [ ] No new compiler warnings
- [ ] Static analysis passes (GNAT style checks)

**Layer 2: Unit Tests** (10 min)
- [ ] All existing unit tests pass
- [ ] New tests for enumeration operations (20 tests)
- [ ] Case statement exhaustiveness validated

**Layer 3: Integration Tests** (15 min)
- [ ] All dependent modules compile and link
- [ ] Module boundary tests pass
- [ ] TypeCode marshaling/unmarshaling tests pass

**Layer 4: Contract Tests** (20 min)
- [ ] CORBA wire format validation (all 40 TypeCode values)
- [ ] Interoperability with external CORBA systems
- [ ] Representation clause correctness verified

**Layer 5: E2E Smoke Tests** (10 min)
- [ ] Critical CORBA operations end-to-end
- [ ] Performance regression check (P95/P99)

**Total Test Time**: 60 minutes

### Pass/Fail Criteria

**Test Execution PASSES If**:
- ✅ All layers complete successfully
- ✅ Contract tests confirm wire format unchanged
- ✅ All 40 TypeCode values validated
- ✅ P95 latency within +10% baseline, P99 within +15%
- ✅ No compilation warnings or errors

**Test Execution FAILS If** (Rollback Triggered):
- ❌ Contract test failures (wire format broken)
- ❌ Compilation failures in any dependent module
- ❌ P95 performance >+25% baseline
- ❌ Integration test failures

---

## 8. Timeline & Milestones

### Phase Breakdown

**Phase 0: Planning & Preparation** (1 week)
- [x] RDB draft created
- [ ] Domain expert consultation (@polyorb_expert, @cdr_maintainer)
- [ ] @security_verification draft RDB review (24h)
- [ ] @security_verification baseline scan (2h)
- [ ] RDB finalized and approved

**Phase 1: Implementation** (1 week)
- [ ] Define TypeCode_Enum type + representation clause (Day 1)
- [ ] Replace constants in primary module (Days 2-3)
- [ ] Update dependent modules (~15 modules) (Days 4-5)
- [ ] Remove old constants + cleanup (Day 5)
- [ ] PR created

**Phase 2: Validation** (1 week)
- [ ] 5-layer test suite execution (Day 1)
- [ ] Contract testing (CORBA interoperability) (Days 2-3)
- [ ] Performance validation (Day 4)
- [ ] @security_verification final review + SRN (2h) (Day 5)
- [ ] PR approved and merged

**Phase 3: Deployment** (No separate phase - rolling deployment)
- [ ] Standard deployment (no special considerations)
- [ ] Monitor for 48h (baseline behavior expected)

**Total Timeline**: 3 weeks

### Milestone Gates

**Gate 1: Design Approval** ✅
- RDB approved by @code_architect
- Domain experts consulted
- **Criteria**: Design validated, no BLOCKING findings

**Gate 2: Implementation Complete** ✅
- All code changes committed
- Compilation succeeds across all modules
- **Criteria**: Unit + integration tests pass

**Gate 3: Validation Complete** ✅
- 5-layer test suite passes
- Contract tests validate CORBA compatibility
- **Criteria**: SRN issued, all pass/fail criteria met

**Gate 4: Production Deployed** ✅
- Standard deployment complete
- 48h monitoring shows baseline behavior
- **Criteria**: Zero incidents, metrics within targets

---

## 9. Ownership & Responsibilities

### Team Assignments

**@code_architect (Design & Oversight)**:
- [x] RDB creation and approval
- [ ] Enumeration type design
- [ ] Domain expert coordination
- [ ] Risk assessment
- [ ] Final sign-off

**@code_refactor (Implementation)**:
- [ ] TypeCode enumeration implementation
- [ ] Constants replacement across modules
- [ ] Unit test updates
- [ ] PR creation and reviews

**@test_stabilize (Testing & Validation)**:
- [ ] Test strategy execution
- [ ] Contract test development
- [ ] Performance monitoring
- [ ] Test results reporting

**@security_verification (Security Review)**:
- [ ] Security baseline capture (2h)
- [ ] Draft RDB review (24h)
- [ ] Final security review + SRN (2h)
- [ ] Wire format validation oversight

**Domain Experts**:
- **@polyorb_expert**: TypeCode usage patterns review
- **@cdr_maintainer**: Wire format compatibility validation

### Communication Plan

**Status Updates**:
- **Frequency**: Daily during implementation (Weeks 2-3)
- **Channel**: AX messages board
- **Format**: Brief status, blockers, next steps

**Escalation Path**:
1. **Team-level issues**: Discuss among agents
2. **Domain expert questions**: Direct consultation
3. **Technical blockers**: Escalate to Tech Lead

---

## 10. Success Metrics

### Technical Metrics

**Code Quality**:
- TypeCode constants: 40 → 1 enumeration type (98% reduction)
- Type safety: 0% → 100% (compiler-enforced)
- Code clarity: Subjective improvement (enumeration names vs magic numbers)

**Performance**:
- P50 latency: Baseline → ≤+5% (expected no change)
- P95 latency: Baseline → ≤+10% (acceptable)
- P99 latency: Baseline → ≤+15% (acceptable)
- Throughput: Baseline (no regression)

**Security**:
- SAST findings: Baseline → ≤Baseline (0 new CRITICAL/HIGH)
- Contract test coverage: 40/40 TypeCode values (100%)

**Reliability**:
- Compilation success rate: 100% (all dependent modules)
- Test pass rate: 100% (all 5 layers)
- Deployment success rate: 100%

### Business Metrics

**Delivery**:
- Timeline adherence: 3 weeks planned → Actual (TBD)
- Effort: 80 hours estimated → Actual (TBD)

**Quality**:
- Production incidents: 0 (expected - behavior-preserving refactor)
- Rollback events: 0 (expected)

### Definition of Done

**Technical DoD**:
- ✅ All 40 TypeCode constants replaced with enumeration
- ✅ All dependent modules compile successfully
- ✅ All tests passing (unit, integration, contract, E2E)
- ✅ Contract tests validate all 40 TypeCode wire formats
- ✅ Performance metrics within targets
- ✅ Security Review Note (SRN) issued

**Process DoD**:
- ✅ All security review checkpoints passed
- ✅ Domain expert consultations complete
- ✅ 48h production monitoring complete with no incidents
- ✅ Documentation updated (inline comments, design docs)

---

## 11. Dependencies & Blockers

### Prerequisites (Must Complete Before Starting)

**Blocking Dependencies**:
- [ ] Domain expert consultation (@polyorb_expert, @cdr_maintainer) - 4 hours
- [ ] @security_verification baseline scan - 2 hours
- [ ] @code_architect RDB approval - 24 hours

### External Dependencies

**Tooling**:
- GNAT Ada compiler (already available)
- Contract testing framework (already available)
- CORBA test harness (already available)

### Known Blockers

**Current Blockers**:
- None identified

**Potential Blockers**:
- **Risk 1**: Domain experts unavailable → Mitigation: Schedule consultation early, async via message board
- **Risk 2**: Hidden TypeCode arithmetic discovered → Mitigation: Comprehensive code audit during planning

---

## 12. Documentation & Artifacts

### Deliverables

**Design Documents**:
- [x] This RDB (RDB-004)
- [ ] Code comments (inline documentation of enumeration design)
- [ ] Migration notes (for future similar refactors)

**Implementation Artifacts**:
- [ ] Code changes (PR link - TBD)
- [ ] Unit tests (20 new/updated tests)
- [ ] Integration tests (8 new/updated tests)
- [ ] Contract tests (10 new/updated tests)

**Testing Artifacts**:
- [ ] Test execution results
- [ ] Coverage reports (95%+ target)
- [ ] Contract test results (40/40 TypeCode values)
- [ ] Performance benchmarks

**Security Artifacts**:
- [ ] Security baseline report
- [ ] Security Review Note (SRN-004)
- [ ] SAST comparison report

### Knowledge Transfer

**Documentation Updates**:
- [ ] Inline code comments (enumeration design rationale)
- [ ] PolyORB developer guide (TypeCode usage patterns)
- [ ] Migration guide (for future enumeration refactors)

**Training**:
- [ ] Team walkthrough (30-minute session before implementation)
- [ ] Pattern documentation (for Phase 2B - GIOP consolidation)

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
- GIOP protocol version constants (Phase 2B candidate)
- Other magic number clusters in PolyORB
- Enumeration opportunities across codebase

---

## Appendices

### Appendix A: TypeCode Enumeration Complete List

**All 40 TypeCode Values** (CORBA Standard):

```ada
type TypeCode_Enum is (
  TC_Null,          -- 0
  TC_Void,          -- 1
  TC_Short,         -- 2
  TC_Long,          -- 3
  TC_UShort,        -- 4
  TC_ULong,         -- 5
  TC_Float,         -- 6
  TC_Double,        -- 7
  TC_Boolean,       -- 8
  TC_Char,          -- 9
  TC_Octet,         -- 10
  TC_Any,           -- 11
  TC_TypeCode,      -- 12
  TC_String,        -- 13
  TC_Objref,        -- 14
  TC_Struct,        -- 15
  TC_Union,         -- 16
  TC_Enum,          -- 17
  TC_Sequence,      -- 18
  TC_Array,         -- 19
  TC_Alias,         -- 20
  TC_Except,        -- 21
  TC_LongLong,      -- 22
  TC_ULongLong,     -- 23
  TC_LongDouble,    -- 24
  TC_WChar,         -- 25
  TC_WString,       -- 26
  TC_Fixed,         -- 27
  TC_Value,         -- 28
  TC_ValueBox,      -- 29
  TC_Native,        -- 30
  TC_Abstract,      -- 31
  TC_Local,         -- 32
  TC_Component,     -- 33
  TC_Home,          -- 34
  TC_Event,         -- 35
  TC_EventValue,    -- 36
  TC_EventValueBox, -- 37
  TC_Reserved38,    -- 38 (reserved for future use)
  TC_Reserved39     -- 39 (reserved for future use)
);

-- Representation clause for wire format compatibility
for TypeCode_Enum use (
  TC_Null          => 0,
  TC_Void          => 1,
  TC_Short         => 2,
  TC_Long          => 3,
  TC_UShort        => 4,
  TC_ULong         => 5,
  TC_Float         => 6,
  TC_Double        => 7,
  TC_Boolean       => 8,
  TC_Char          => 9,
  TC_Octet         => 10,
  TC_Any           => 11,
  TC_TypeCode      => 12,
  TC_String        => 13,
  TC_Objref        => 14,
  TC_Struct        => 15,
  TC_Union         => 16,
  TC_Enum          => 17,
  TC_Sequence      => 18,
  TC_Array         => 19,
  TC_Alias         => 20,
  TC_Except        => 21,
  TC_LongLong      => 22,
  TC_ULongLong     => 23,
  TC_LongDouble    => 24,
  TC_WChar         => 25,
  TC_WString       => 26,
  TC_Fixed         => 27,
  TC_Value         => 28,
  TC_ValueBox      => 29,
  TC_Native        => 30,
  TC_Abstract      => 31,
  TC_Local         => 32,
  TC_Component     => 33,
  TC_Home          => 34,
  TC_Event         => 35,
  TC_EventValue    => 36,
  TC_EventValueBox => 37,
  TC_Reserved38    => 38,
  TC_Reserved39    => 39
);
```

### Appendix B: Architecture Diagram

```
┌─────────────────────────────────────────────────────────────┐
│                    CORBA Application Layer                   │
├─────────────────────────────────────────────────────────────┤
│                             ↕                                │
│                  TypeCode_Enum Interface                     │
│          (Type-safe, self-documenting enumeration)           │
│                             ↕                                │
├─────────────────────────────────────────────────────────────┤
│              PolyORB CDR Representation Module               │
│                 (polyorb-representations-cdr)                │
│                             ↕                                │
├─────────────────────────────────────────────────────────────┤
│                   Wire Format (CORBA/GIOP)                   │
│         (Binary protocol - representation clause maps)       │
└─────────────────────────────────────────────────────────────┘
```

### Appendix C: References

**Related Documents**:
- RDB-003: Phase 1 Deallocation (demonstrates migration pattern)
- RDB-005: GIOP Protocol Consolidation (potential Phase 2B)

**External References**:
- CORBA Specification: TypeCode definitions (OMG standard)
- Ada Enumeration Types: Language Reference Manual
- PolyORB Documentation: CDR marshaling

---

## Approval & Sign-Off

### Draft RDB Review

**Reviewer**: @security_verification
**Review Date**: [Pending]
**Status**: [PENDING]
**Feedback**: [To be provided]

### Final RDB Approval

**Approver**: @code_architect
**Approval Date**: 2025-11-06
**Status**: ✅ DRAFT COMPLETE (Pending reviews)
**Conditions**: Subject to domain expert consultation + security baseline

### Security Review Note (SRN)

**Reviewer**: @security_verification
**Review Date**: [To be scheduled]
**SRN ID**: SRN-004
**Status**: [PENDING]
**Link**: [To be created]

---

**Document Version**: 1.0
**Last Updated**: 2025-11-06
**Status**: DRAFT
