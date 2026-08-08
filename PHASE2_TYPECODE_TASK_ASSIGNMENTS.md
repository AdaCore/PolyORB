# Phase 2 TypeCode Enumeration - Task Assignments

**RDB**: RDB-004
**Project**: TypeCode Constants to Enumeration Consolidation
**Timeline**: 3 weeks (15 working days)
**Total Estimated Effort**: 80 hours

---

## Task Breakdown by Phase

### Phase 0: Planning & Preparation (Week 1: Days 1-5)

#### Task 0.1: Domain Expert Consultation
**Owner**: @code_architect
**Duration**: 4 hours
**Dependencies**: None
**Priority**: HIGH (BLOCKING)

**Description**:
Consult with @polyorb_expert and @cdr_maintainer to validate TypeCode enumeration design and identify hidden invariants.

**Acceptance Criteria**:
- [ ] @polyorb_expert review complete (2h consultation)
- [ ] @cdr_maintainer review complete (2h consultation)
- [ ] Hidden invariants documented in RDB
- [ ] Design validated or adjusted based on feedback

**Deliverables**:
- Consultation notes (added to RDB Section 4)
- Updated security invariants if new properties discovered

---

#### Task 0.2: Security Baseline Capture
**Owner**: @security_verification
**Duration**: 2 hours
**Dependencies**: None
**Priority**: HIGH (BLOCKING)

**Description**:
Run SAST scan on current codebase to establish security baseline before refactoring.

**Acceptance Criteria**:
- [ ] SAST scan complete (Semgrep/similar tool)
- [ ] Baseline report generated
- [ ] CRITICAL/HIGH findings documented
- [ ] Baseline stored for comparison

**Deliverables**:
- Security baseline report
- SAST scan results (JSON/CSV)

---

#### Task 0.3: RDB Review & Approval
**Owner**: @security_verification (reviewer), @code_architect (approver)
**Duration**: 4 hours (2h review + 2h revisions)
**Dependencies**: Task 0.1, Task 0.2
**Priority**: HIGH (BLOCKING)

**Description**:
Review RDB-004 for security concerns, technical soundness, and completeness. Revise based on feedback.

**Acceptance Criteria**:
- [ ] @security_verification review complete (24h SLA)
- [ ] All BLOCKING findings addressed
- [ ] @code_architect final approval
- [ ] RDB status: DRAFT → APPROVED

**Deliverables**:
- Security review comments
- Updated RDB-004 (APPROVED status)

---

### Phase 1: Implementation (Week 2: Days 6-10)

#### Task 1.1: Define TypeCode Enumeration Type
**Owner**: @code_refactor
**Duration**: 8 hours (1 day)
**Dependencies**: Task 0.3 (RDB approval)
**Priority**: HIGH

**Description**:
Create TypeCode_Enum type with all 40 values and representation clause in polyorb-representations-cdr.ads.

**Acceptance Criteria**:
- [ ] TypeCode_Enum type defined with 40 values
- [ ] Representation clause added (wire format compatibility)
- [ ] Compilation succeeds (no errors/warnings)
- [ ] Code review complete

**Deliverables**:
- Modified `polyorb-representations-cdr.ads`
- Representation clause test (wire format verification)

**Files Changed**:
- `src/polyorb-representations-cdr.ads` (NEW or MODIFIED)

---

#### Task 1.2: Replace Constants in Primary Module
**Owner**: @code_refactor
**Duration**: 16 hours (2 days)
**Dependencies**: Task 1.1
**Priority**: HIGH

**Description**:
Replace all 40 TypeCode constants with enumeration literals in primary module (polyorb-representations-cdr.adb lines 106-143 + all usages).

**Acceptance Criteria**:
- [ ] All 40 constants replaced with enum literals
- [ ] Case statements use enumeration (not magic numbers)
- [ ] Compilation succeeds
- [ ] Unit tests pass

**Deliverables**:
- Modified `polyorb-representations-cdr.adb`
- Updated unit tests (if needed)

**Files Changed**:
- `src/polyorb-representations-cdr.adb` (lines 106-143 + usages)

---

#### Task 1.3: Update Dependent Modules
**Owner**: @code_refactor
**Duration**: 24 hours (3 days)
**Dependencies**: Task 1.2
**Priority**: HIGH

**Description**:
Update estimated 15 dependent modules that reference TypeCode constants to use enumeration.

**Acceptance Criteria**:
- [ ] All dependent modules identified (code search)
- [ ] All modules updated to use enumeration
- [ ] Compilation succeeds across all modules
- [ ] Integration tests pass

**Deliverables**:
- Modified dependent modules (~15 files)
- List of changed files

**Estimated Files** (~15 modules):
- Modules using TypeCode for marshaling/unmarshaling
- CORBA protocol handling modules
- Type representation utilities

---

#### Task 1.4: Remove Old Constants & Cleanup
**Owner**: @code_refactor
**Duration**: 4 hours (0.5 day)
**Dependencies**: Task 1.3
**Priority**: MEDIUM

**Description**:
Remove old TypeCode constants and perform final cleanup (unused code, comments, etc.).

**Acceptance Criteria**:
- [ ] Old constants removed from source
- [ ] Static analysis confirms no constant references remain
- [ ] Compilation succeeds
- [ ] Code review complete

**Deliverables**:
- Final cleaned-up codebase
- PR ready for review

---

#### Task 1.5: Create Pull Request
**Owner**: @code_refactor
**Duration**: 2 hours
**Dependencies**: Task 1.4
**Priority**: MEDIUM

**Description**:
Create comprehensive PR with all changes, clear description, and links to RDB-004.

**Acceptance Criteria**:
- [ ] PR created with descriptive title
- [ ] PR description includes RDB-004 link
- [ ] All changed files included
- [ ] CI/CD pipeline triggered

**Deliverables**:
- GitHub PR link
- PR description linking to RDB-004

---

### Phase 2: Validation (Week 3: Days 11-15)

#### Task 2.1: Unit Test Execution & Updates
**Owner**: @test_stabilize
**Duration**: 8 hours (1 day)
**Dependencies**: Task 1.5 (PR created)
**Priority**: HIGH

**Description**:
Execute all unit tests, create new tests for enumeration operations, achieve 95%+ coverage.

**Acceptance Criteria**:
- [ ] All existing unit tests pass
- [ ] 20 new/updated tests for TypeCode enumeration
- [ ] Coverage ≥95% for affected code
- [ ] Test report generated

**Deliverables**:
- Unit test results
- Coverage report
- New test files (if applicable)

---

#### Task 2.2: Integration Test Execution
**Owner**: @test_stabilize
**Duration**: 8 hours (1 day)
**Dependencies**: Task 2.1
**Priority**: HIGH

**Description**:
Execute integration tests to validate module boundaries and TypeCode marshaling/unmarshaling.

**Acceptance Criteria**:
- [ ] All integration tests pass (8+ tests)
- [ ] Module boundary validation complete
- [ ] Marshaling/unmarshaling tests pass
- [ ] Test report generated

**Deliverables**:
- Integration test results
- Any new integration tests created

---

#### Task 2.3: Contract Test Execution (CORBA Interoperability)
**Owner**: @test_stabilize
**Duration**: 12 hours (1.5 days)
**Dependencies**: Task 2.2
**Priority**: CRITICAL

**Description**:
Execute contract tests to validate CORBA wire format compatibility for all 40 TypeCode values.

**Acceptance Criteria**:
- [ ] All 40 TypeCode values tested in wire format
- [ ] Interoperability with external CORBA systems validated
- [ ] Representation clause correctness verified
- [ ] No wire format regressions

**Deliverables**:
- Contract test results (40/40 TypeCode values)
- Wire format validation report
- Interoperability test results

---

#### Task 2.4: Performance Validation
**Owner**: @test_stabilize
**Duration**: 4 hours (0.5 day)
**Dependencies**: Task 2.3
**Priority**: MEDIUM

**Description**:
Run performance benchmarks to ensure no regression (P95 ≤+10%, P99 ≤+15%).

**Acceptance Criteria**:
- [ ] Baseline performance metrics captured
- [ ] Post-refactor performance metrics captured
- [ ] P95 latency within +10% baseline
- [ ] P99 latency within +15% baseline
- [ ] Throughput at baseline (no regression)

**Deliverables**:
- Performance comparison report
- Benchmark results (before/after)

---

#### Task 2.5: SAST Comparison & Security Validation
**Owner**: @security_verification
**Duration**: 2 hours
**Dependencies**: Task 2.4
**Priority**: HIGH

**Description**:
Run SAST scan on refactored code and compare against baseline. Validate no new CRITICAL/HIGH findings.

**Acceptance Criteria**:
- [ ] SAST scan complete
- [ ] Comparison vs baseline generated
- [ ] 0 new CRITICAL findings
- [ ] ≤5 new HIGH findings (acceptable)
- [ ] Report generated

**Deliverables**:
- SAST comparison report
- New findings analysis (if any)

---

#### Task 2.6: Final Security Review & SRN Issuance
**Owner**: @security_verification
**Duration**: 2 hours
**Dependencies**: Task 2.5
**Priority**: HIGH (BLOCKING for merge)

**Description**:
Conduct final security review of all artifacts and issue Security Review Note (SRN-004).

**Acceptance Criteria**:
- [ ] All test results reviewed
- [ ] SAST comparison reviewed
- [ ] Contract tests validated
- [ ] SRN-004 issued (APPROVED status)

**Deliverables**:
- Security Review Note (SRN-004)
- Final security approval for merge

---

#### Task 2.7: PR Review & Merge
**Owner**: @code_architect (reviewer), @code_refactor (merger)
**Duration**: 2 hours
**Dependencies**: Task 2.6 (SRN approval)
**Priority**: HIGH

**Description**:
Final code review, address any comments, and merge PR to main branch.

**Acceptance Criteria**:
- [ ] @code_architect code review complete
- [ ] All review comments addressed
- [ ] SRN-004 approval confirmed
- [ ] PR merged to main

**Deliverables**:
- Merged PR
- Release notes (if applicable)

---

### Phase 3: Deployment & Monitoring (Post-merge)

#### Task 3.1: Standard Deployment
**Owner**: @code_refactor
**Duration**: 2 hours
**Dependencies**: Task 2.7 (PR merged)
**Priority**: HIGH

**Description**:
Deploy refactored code to production using standard deployment process.

**Acceptance Criteria**:
- [ ] Deployment successful
- [ ] Health checks pass
- [ ] No immediate errors in logs
- [ ] Monitoring dashboards active

**Deliverables**:
- Deployment confirmation
- Initial monitoring report

---

#### Task 3.2: 48-Hour Monitoring
**Owner**: @test_stabilize
**Duration**: Continuous (48 hours)
**Dependencies**: Task 3.1
**Priority**: MEDIUM

**Description**:
Monitor production system for 48 hours to ensure baseline behavior maintained.

**Acceptance Criteria**:
- [ ] 48 hours elapsed since deployment
- [ ] Zero production incidents
- [ ] Metrics within baseline ranges
- [ ] No TypeCode-related errors

**Deliverables**:
- 48h monitoring report
- Incident log (expected: empty)

---

#### Task 3.3: Retrospective
**Owner**: All agents
**Duration**: 2 hours
**Dependencies**: Task 3.2
**Priority**: LOW

**Description**:
Conduct team retrospective to capture lessons learned and identify improvements.

**Acceptance Criteria**:
- [ ] Retrospective session conducted
- [ ] "What went well" documented
- [ ] "What could improve" documented
- [ ] Action items for future refactors identified

**Deliverables**:
- Retrospective notes (added to RDB-004 Section 13)
- Action items for future patterns

---

## Summary: Effort by Agent

| Agent | Tasks | Total Hours | % of Total |
|-------|-------|-------------|------------|
| @code_architect | 0.1, 0.3 (partial), 2.7 (partial) | 8h | 10% |
| @code_refactor | 1.1, 1.2, 1.3, 1.4, 1.5, 3.1 | 56h | 70% |
| @test_stabilize | 2.1, 2.2, 2.3, 2.4, 3.2 | 32h + monitoring | 40% |
| @security_verification | 0.2, 0.3 (partial), 2.5, 2.6 | 10h | 12.5% |
| **TOTAL** | **23 tasks** | **~80 hours** | **100%** |

**Note**: Some tasks overlap (parallel execution possible). Total calendar time: 3 weeks with proper coordination.

---

## Critical Path

The critical path for this refactor (tasks that must be completed sequentially):

1. **Task 0.1** (Domain consultation) → 4h
2. **Task 0.2** (Security baseline) → 2h [Parallel with 0.1]
3. **Task 0.3** (RDB approval) → 4h
4. **Task 1.1** (Define enum) → 8h
5. **Task 1.2** (Replace constants primary) → 16h
6. **Task 1.3** (Update dependent modules) → 24h
7. **Task 1.4** (Cleanup) → 4h
8. **Task 1.5** (Create PR) → 2h
9. **Task 2.1** (Unit tests) → 8h
10. **Task 2.2** (Integration tests) → 8h
11. **Task 2.3** (Contract tests) → 12h
12. **Task 2.4** (Performance) → 4h [Can parallel with 2.3]
13. **Task 2.5** (SAST) → 2h
14. **Task 2.6** (SRN) → 2h
15. **Task 2.7** (Merge) → 2h
16. **Task 3.1** (Deploy) → 2h

**Critical Path Duration**: ~102 hours sequential work
**Calendar Time**: 3 weeks (with parallelization and 40h work weeks)

---

## Risk Mitigation per Task

### High-Risk Tasks (Extra Attention Required)

**Task 1.3: Update Dependent Modules** (24h)
- **Risk**: Compilation failures in dependent modules
- **Mitigation**: Incremental approach - update and test each module individually
- **Fallback**: Rollback to Task 1.2 state if critical module fails

**Task 2.3: Contract Test Execution** (12h)
- **Risk**: Wire format incompatibility breaks CORBA protocol
- **Mitigation**: Test all 40 TypeCode values individually; validate representation clause early
- **Fallback**: Revert to old constants, investigate representation clause issue

---

## Communication Schedule

**Daily Stand-up** (15 min) - During Weeks 2-3 (Implementation & Validation):
- Current task status
- Blockers
- Next 24h plan

**Mid-week Check-in** (30 min) - Week 2 & Week 3:
- Progress vs plan
- Risk assessment
- Adjust timeline if needed

**Phase Completion Review** (1h) - End of each phase:
- Deliverables review
- Gate criteria validation
- Go/no-go decision for next phase

---

## Contingency Plans

### If Timeline Slips

**Week 1 (Planning) Slips**:
- **Action**: Extend planning by 2-3 days
- **Impact**: Push implementation start, maintain 3-week total if possible
- **Escalation**: If >1 week delay, reassess scope

**Week 2 (Implementation) Slips**:
- **Action**: Identify tasks that can be parallelized (e.g., dependent modules)
- **Impact**: May extend validation week slightly
- **Escalation**: If >1 week delay, consider descoping dependent modules (do incrementally)

**Week 3 (Validation) Slips**:
- **Action**: Prioritize critical tests (contract tests > performance)
- **Impact**: May delay merge by 2-3 days
- **Escalation**: If blocking issues found, rollback to planning phase

---

## Success Criteria Summary

**Phase 0 Complete When**:
- ✅ RDB-004 approved
- ✅ Domain experts consulted
- ✅ Security baseline captured

**Phase 1 Complete When**:
- ✅ All code changes implemented
- ✅ Compilation succeeds across all modules
- ✅ PR created and ready for review

**Phase 2 Complete When**:
- ✅ All 5 test layers pass
- ✅ Contract tests validate 40/40 TypeCode values
- ✅ SRN-004 issued (APPROVED)
- ✅ PR merged

**Phase 3 Complete When**:
- ✅ Deployment successful
- ✅ 48h monitoring shows baseline behavior
- ✅ Retrospective complete

---

**Document Version**: 1.0
**Created**: 2025-11-06
**Owner**: @code_architect
**Status**: READY FOR EXECUTION (pending RDB-004 approval)
