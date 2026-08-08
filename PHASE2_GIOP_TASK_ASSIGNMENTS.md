# Phase 2 GIOP Protocol Consolidation - Task Assignments

**RDB**: RDB-005
**Project**: GIOP Protocol Version Deduplication and Consolidation
**Timeline**: 5 weeks (25 working days)
**Total Estimated Effort**: 160 hours

---

## Task Breakdown by Phase

### Phase 0: Planning & Preparation (Week 1: Days 1-5)

#### Task 0.1: Domain Expert Consultation
**Owner**: @code_architect
**Duration**: 8 hours (1 day)
**Dependencies**: None
**Priority**: HIGH (BLOCKING)

**Description**:
Consult with @giop_expert, @corba_security_expert, and @protocol_maintainer to validate GIOP consolidation design and identify hidden protocol invariants.

**Acceptance Criteria**:
- [ ] @giop_expert review complete (3h consultation)
- [ ] @corba_security_expert review complete (3h consultation)
- [ ] @protocol_maintainer review complete (2h consultation)
- [ ] Hidden invariants documented in RDB
- [ ] Version-specific security properties identified
- [ ] Design validated or adjusted based on feedback

**Deliverables**:
- Consultation notes (added to RDB Section 4)
- Updated security invariants
- Protocol-specific security concerns documented

---

#### Task 0.2: Security Baseline Capture
**Owner**: @security_verification
**Duration**: 4 hours
**Dependencies**: None
**Priority**: HIGH (BLOCKING)

**Description**:
Run comprehensive SAST scan and protocol security analysis on current GIOP implementations to establish security baseline.

**Acceptance Criteria**:
- [ ] SAST scan complete for all 3 GIOP files
- [ ] Protocol fuzzing baseline established
- [ ] Baseline report generated
- [ ] CRITICAL/HIGH findings documented
- [ ] Security baseline stored for comparison

**Deliverables**:
- Security baseline report
- SAST scan results (JSON/CSV)
- Protocol fuzzing baseline results

---

#### Task 0.3: RDB Review & Approval
**Owner**: @security_verification (reviewer), @code_architect (approver)
**Duration**: 6 hours (4h review + 2h revisions)
**Dependencies**: Task 0.1, Task 0.2
**Priority**: HIGH (BLOCKING)

**Description**:
Review RDB-005 for security concerns, protocol compatibility, and technical soundness. Revise based on feedback.

**Acceptance Criteria**:
- [ ] @security_verification review complete (48h SLA - HIGH risk)
- [ ] All BLOCKING findings addressed
- [ ] Protocol security concerns documented
- [ ] @code_architect final approval
- [ ] RDB status: DRAFT → APPROVED

**Deliverables**:
- Security review comments
- Updated RDB-005 (APPROVED status)

---

### Phase 1: Protocol Analysis (Week 2: Days 6-10)

#### Task 1.1: Complete Protocol Diff Analysis
**Owner**: @code_refactor
**Duration**: 24 hours (3 days)
**Dependencies**: Task 0.3 (RDB approval)
**Priority**: HIGH (BLOCKING)

**Description**:
Perform comprehensive diff analysis of giop_1_0.adb, giop_1_1.adb, and giop_1_2.adb to identify shared logic and version-specific differences.

**Acceptance Criteria**:
- [ ] All 3 GIOP files analyzed line-by-line
- [ ] Shared logic identified (~180 LOC estimated)
- [ ] Version-specific differences documented
- [ ] Function-by-function comparison complete
- [ ] Diff analysis report created

**Deliverables**:
- Protocol diff analysis report (detailed document)
- Shared logic identification (LOC breakdown)
- Version-specific differences list

**Estimated Findings**:
- Shared: ~180 LOC (80%)
- GIOP 1.0 specific: ~20 LOC
- GIOP 1.1 specific: ~30 LOC (fragmentation)
- GIOP 1.2 specific: ~40 LOC (bidirectional)

---

#### Task 1.2: Identify Security-Critical Differences
**Owner**: @security_verification
**Duration**: 8 hours (1 day)
**Dependencies**: Task 1.1
**Priority**: HIGH

**Description**:
Review protocol diff analysis to identify security-critical differences between GIOP versions (validation inconsistencies, etc.).

**Acceptance Criteria**:
- [ ] Security implications of each difference analyzed
- [ ] Inconsistent validation logic identified
- [ ] Security-critical paths documented
- [ ] Recommendations for consolidation provided

**Deliverables**:
- Security-focused diff analysis
- Critical differences report
- Consolidation security recommendations

---

#### Task 1.3: Design Common Module Architecture
**Owner**: @code_architect
**Duration**: 8 hours (1 day)
**Dependencies**: Task 1.1, Task 1.2
**Priority**: HIGH

**Description**:
Design the architecture for giop_common module and GIOP_Strategy interface based on diff analysis.

**Acceptance Criteria**:
- [ ] Common module API defined
- [ ] Strategy interface designed
- [ ] Version-specific strategy responsibilities defined
- [ ] Architecture diagram created
- [ ] Design review complete

**Deliverables**:
- Common module design document
- Strategy interface specification
- Architecture diagrams (Mermaid or similar)

---

### Phase 2: Implementation (Weeks 3-4: Days 11-20)

#### Task 2.1: Create Common Module + Strategy Interface
**Owner**: @code_refactor
**Duration**: 24 hours (3 days)
**Dependencies**: Task 1.3
**Priority**: HIGH

**Description**:
Implement giop_common.adb with shared logic (~180 LOC) and giop_strategy.ads interface definition.

**Acceptance Criteria**:
- [ ] `giop_common.adb` created with shared logic
- [ ] `giop_strategy.ads` interface defined
- [ ] Compilation succeeds (no errors/warnings)
- [ ] Unit tests for common module (95%+ coverage)
- [ ] Code review complete

**Deliverables**:
- `src/giop/giop_common.adb` (NEW)
- `src/giop/giop_strategy.ads` (NEW)
- Unit tests for common module (20+ tests)

**Files Created**:
- `src/giop/giop_common.adb`
- `src/giop/giop_strategy.ads`

---

#### Task 2.2: Implement GIOP 1.0 Strategy (Pilot)
**Owner**: @code_refactor
**Duration**: 16 hours (2 days)
**Dependencies**: Task 2.1
**Priority**: HIGH

**Description**:
Implement giop_1_0_strategy.adb and refactor giop_1_0.adb to use common module + strategy. This is the pilot to validate the approach.

**Acceptance Criteria**:
- [ ] `giop_1_0_strategy.adb` implemented (~20 LOC)
- [ ] `giop_1_0.adb` refactored to delegate to common + strategy
- [ ] All GIOP 1.0 unit tests pass
- [ ] Contract tests pass for GIOP 1.0
- [ ] Feature flag implemented and functional

**Deliverables**:
- `src/giop/giop_1_0_strategy.adb` (NEW)
- Modified `src/giop/giop_1_0.adb`
- Feature flag implementation
- Updated tests

**Files Changed**:
- `src/giop/giop_1_0.adb` (MODIFIED)
- `src/giop/giop_1_0_strategy.adb` (NEW)

---

#### Task 2.3: Mid-Implementation Security Review
**Owner**: @security_verification
**Duration**: 4 hours
**Dependencies**: Task 2.2
**Priority**: HIGH

**Description**:
Review GIOP 1.0 pilot implementation for security concerns and validate approach before extending to other versions.

**Acceptance Criteria**:
- [ ] Code review complete
- [ ] Security implications of consolidation validated
- [ ] GIOP 1.0 contract tests reviewed
- [ ] Feedback provided for 1.1/1.2 implementation

**Deliverables**:
- Mid-implementation security review report
- Recommendations for 1.1/1.2 implementation

---

#### Task 2.4: Implement GIOP 1.1 Strategy
**Owner**: @code_refactor
**Duration**: 20 hours (2.5 days)
**Dependencies**: Task 2.3
**Priority**: HIGH

**Description**:
Implement giop_1_1_strategy.adb (fragmentation logic) and refactor giop_1_1.adb to use common module + strategy.

**Acceptance Criteria**:
- [ ] `giop_1_1_strategy.adb` implemented (~30 LOC - fragmentation)
- [ ] `giop_1_1.adb` refactored to delegate to common + strategy
- [ ] All GIOP 1.1 unit tests pass
- [ ] Fragment handling tests pass
- [ ] Contract tests pass for GIOP 1.1
- [ ] Feature flag controls GIOP 1.1

**Deliverables**:
- `src/giop/giop_1_1_strategy.adb` (NEW)
- Modified `src/giop/giop_1_1.adb`
- Fragment handling tests

**Files Changed**:
- `src/giop/giop_1_1.adb` (MODIFIED)
- `src/giop/giop_1_1_strategy.adb` (NEW)

---

#### Task 2.5: Implement GIOP 1.2 Strategy
**Owner**: @code_refactor
**Duration**: 24 hours (3 days)
**Dependencies**: Task 2.4
**Priority**: HIGH

**Description**:
Implement giop_1_2_strategy.adb (bidirectional + enhanced features) and refactor giop_1_2.adb to use common module + strategy.

**Acceptance Criteria**:
- [ ] `giop_1_2_strategy.adb` implemented (~40 LOC - bidirectional)
- [ ] `giop_1_2.adb` refactored to delegate to common + strategy
- [ ] All GIOP 1.2 unit tests pass
- [ ] Bidirectional protocol tests pass
- [ ] Contract tests pass for GIOP 1.2
- [ ] Feature flag controls GIOP 1.2

**Deliverables**:
- `src/giop/giop_1_2_strategy.adb` (NEW)
- Modified `src/giop/giop_1_2.adb`
- Bidirectional protocol tests

**Files Changed**:
- `src/giop/giop_1_2.adb` (MODIFIED)
- `src/giop/giop_1_2_strategy.adb` (NEW)

---

#### Task 2.6: Remove Legacy Code & Cleanup
**Owner**: @code_refactor
**Duration**: 8 hours (1 day)
**Dependencies**: Task 2.5
**Priority**: MEDIUM

**Description**:
Remove duplicated legacy code from all 3 GIOP files and perform final cleanup.

**Acceptance Criteria**:
- [ ] Duplicated code removed from all files
- [ ] Static analysis confirms no dead code
- [ ] All 3 versions compile successfully
- [ ] Code review complete
- [ ] Documentation updated

**Deliverables**:
- Clean, consolidated codebase
- Updated inline documentation

---

#### Task 2.7: Create Pull Request
**Owner**: @code_refactor
**Duration**: 2 hours
**Dependencies**: Task 2.6
**Priority**: MEDIUM

**Description**:
Create comprehensive PR with all changes, clear description, and links to RDB-005.

**Acceptance Criteria**:
- [ ] PR created with descriptive title
- [ ] PR description includes RDB-005 link and protocol diff analysis summary
- [ ] All changed files included
- [ ] Feature flag documented
- [ ] CI/CD pipeline triggered

**Deliverables**:
- GitHub PR link
- PR description with context

---

### Phase 3: Validation (Week 5: Days 21-25)

#### Task 3.1: Unit Test Execution & Updates
**Owner**: @test_stabilize
**Duration**: 8 hours (1 day)
**Dependencies**: Task 2.7 (PR created)
**Priority**: HIGH

**Description**:
Execute all unit tests for common module and all 3 strategies, create new tests as needed, achieve 95%+ coverage.

**Acceptance Criteria**:
- [ ] All existing unit tests pass
- [ ] 30 new/updated tests for common module and strategies
- [ ] Coverage ≥95% for refactored code
- [ ] Mutation score ≥85% for protocol logic
- [ ] Test report generated

**Deliverables**:
- Unit test results
- Coverage report
- Mutation testing results

---

#### Task 3.2: Integration Test Execution
**Owner**: @test_stabilize
**Duration**: 12 hours (1.5 days)
**Dependencies**: Task 3.1
**Priority**: HIGH

**Description**:
Execute integration tests to validate protocol handling paths for all 3 GIOP versions.

**Acceptance Criteria**:
- [ ] 15 integration tests executed
- [ ] All 3 GIOP versions tested independently
- [ ] Cross-version compatibility validated
- [ ] Error handling integration tests pass
- [ ] Test report generated

**Deliverables**:
- Integration test results
- Cross-version compatibility report

---

#### Task 3.3: Contract Test Execution (All GIOP Versions)
**Owner**: @test_stabilize
**Duration**: 16 hours (2 days)
**Dependencies**: Task 3.2
**Priority**: CRITICAL

**Description**:
Execute contract tests to validate CORBA protocol compliance for all 3 GIOP versions (wire format, interoperability).

**Acceptance Criteria**:
- [ ] 30 contract tests executed (10 per version)
- [ ] Wire format validation for all message types per version
- [ ] Interoperability with external CORBA systems validated
- [ ] Protocol compliance verified for each version
- [ ] No wire format regressions

**Deliverables**:
- Contract test results (30/30 tests)
- Wire format validation report per version
- Interoperability test results

---

#### Task 3.4: Performance Validation (Per Version)
**Owner**: @test_stabilize
**Duration**: 8 hours (1 day)
**Dependencies**: Task 3.3
**Priority**: MEDIUM

**Description**:
Run performance benchmarks for each GIOP version to ensure no regression (P95 ≤+10%, P99 ≤+15%).

**Acceptance Criteria**:
- [ ] Baseline performance metrics captured per version
- [ ] Post-refactor performance metrics captured per version
- [ ] P95 latency within +10% baseline for all versions
- [ ] P99 latency within +15% baseline for all versions
- [ ] Throughput at baseline (no regression)

**Deliverables**:
- Performance comparison report (per version)
- Benchmark results (before/after)

---

#### Task 3.5: Protocol Fuzzing & Security Testing
**Owner**: @security_verification
**Duration**: 8 hours (1 day)
**Dependencies**: Task 3.3
**Priority**: HIGH

**Description**:
Perform protocol fuzzing for all 3 GIOP versions to identify input validation vulnerabilities.

**Acceptance Criteria**:
- [ ] Protocol fuzzing executed for all 3 versions
- [ ] Zero crashes from fuzz testing
- [ ] No new vulnerabilities discovered
- [ ] Input validation verified
- [ ] Fuzzing report generated

**Deliverables**:
- Protocol fuzzing results (per version)
- Security findings report (expected: none)

---

#### Task 3.6: SAST Comparison & Security Validation
**Owner**: @security_verification
**Duration**: 4 hours
**Dependencies**: Task 3.5
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

#### Task 3.7: Final Security Review & SRN Issuance
**Owner**: @security_verification
**Duration**: 4 hours
**Dependencies**: Task 3.6
**Priority**: HIGH (BLOCKING for merge)

**Description**:
Conduct final security review of all artifacts and issue Security Review Note (SRN-005).

**Acceptance Criteria**:
- [ ] All test results reviewed
- [ ] SAST comparison reviewed
- [ ] Protocol fuzzing results validated
- [ ] Contract tests for all versions validated
- [ ] SRN-005 issued (APPROVED status)

**Deliverables**:
- Security Review Note (SRN-005)
- Final security approval for merge

---

#### Task 3.8: PR Review & Merge
**Owner**: @code_architect (reviewer), @code_refactor (merger)
**Duration**: 4 hours
**Dependencies**: Task 3.7 (SRN approval)
**Priority**: HIGH

**Description**:
Final code review, address any comments, and merge PR to main branch.

**Acceptance Criteria**:
- [ ] @code_architect code review complete
- [ ] All review comments addressed
- [ ] SRN-005 approval confirmed
- [ ] Feature flag enabled (all versions = legacy initially)
- [ ] PR merged to main

**Deliverables**:
- Merged PR
- Release notes

---

### Phase 4: Deployment & Monitoring (Post-merge: Weeks 6-7)

#### Task 4.1: Deploy with Feature Flag
**Owner**: @code_refactor
**Duration**: 4 hours
**Dependencies**: Task 3.8 (PR merged)
**Priority**: HIGH

**Description**:
Deploy refactored code to production with feature flag (all versions using legacy implementation initially).

**Acceptance Criteria**:
- [ ] Deployment successful
- [ ] Feature flag functional (all versions = legacy)
- [ ] Health checks pass
- [ ] No immediate errors in logs
- [ ] Monitoring dashboards active

**Deliverables**:
- Deployment confirmation
- Initial monitoring report

---

#### Task 4.2: Enable GIOP 1.0 Consolidated (Monitor 48h)
**Owner**: @test_stabilize
**Duration**: Continuous (48 hours monitoring)
**Dependencies**: Task 4.1
**Priority**: HIGH

**Description**:
Enable consolidated implementation for GIOP 1.0 only. Monitor for 48 hours.

**Acceptance Criteria**:
- [ ] Feature flag toggled: GIOP 1.0 = consolidated
- [ ] 48 hours monitoring complete
- [ ] Zero incidents related to GIOP 1.0
- [ ] Metrics within baseline ranges
- [ ] No rollback required

**Deliverables**:
- 48h monitoring report (GIOP 1.0)
- Incident log (expected: empty)

---

#### Task 4.3: Enable GIOP 1.1 Consolidated (Monitor 48h)
**Owner**: @test_stabilize
**Duration**: Continuous (48 hours monitoring)
**Dependencies**: Task 4.2 (GIOP 1.0 successful)
**Priority**: HIGH

**Description**:
Enable consolidated implementation for GIOP 1.1. Monitor for 48 hours.

**Acceptance Criteria**:
- [ ] Feature flag toggled: GIOP 1.1 = consolidated
- [ ] 48 hours monitoring complete
- [ ] Zero incidents related to GIOP 1.1
- [ ] Metrics within baseline ranges
- [ ] Fragment handling working correctly

**Deliverables**:
- 48h monitoring report (GIOP 1.1)
- Incident log (expected: empty)

---

#### Task 4.4: Enable GIOP 1.2 Consolidated (Monitor 48h)
**Owner**: @test_stabilize
**Duration**: Continuous (48 hours monitoring)
**Dependencies**: Task 4.3 (GIOP 1.1 successful)
**Priority**: HIGH

**Description**:
Enable consolidated implementation for GIOP 1.2. Monitor for 48 hours.

**Acceptance Criteria**:
- [ ] Feature flag toggled: GIOP 1.2 = consolidated
- [ ] 48 hours monitoring complete
- [ ] Zero incidents related to GIOP 1.2
- [ ] Metrics within baseline ranges
- [ ] Bidirectional protocol working correctly

**Deliverables**:
- 48h monitoring report (GIOP 1.2)
- Incident log (expected: empty)

---

#### Task 4.5: Remove Feature Flag & Final Bake
**Owner**: @code_refactor
**Duration**: 8 hours (1 day) + 1 week bake time
**Dependencies**: Task 4.4 (All versions successful)
**Priority**: MEDIUM

**Description**:
Remove feature flag code (all versions use consolidated implementation). Monitor for 1 week.

**Acceptance Criteria**:
- [ ] Feature flag removed from codebase
- [ ] Recompile and redeploy successful
- [ ] 1 week bake time complete
- [ ] Zero incidents
- [ ] Metrics stable

**Deliverables**:
- Clean codebase (no feature flag)
- 1-week monitoring report

---

#### Task 4.6: Retrospective
**Owner**: All agents
**Duration**: 2 hours
**Dependencies**: Task 4.5
**Priority**: LOW

**Description**:
Conduct team retrospective to capture lessons learned and identify improvements.

**Acceptance Criteria**:
- [ ] Retrospective session conducted
- [ ] "What went well" documented
- [ ] "What could improve" documented
- [ ] Action items for future refactors identified
- [ ] Pattern reusability notes captured

**Deliverables**:
- Retrospective notes (added to RDB-005 Section 13)
- Action items for future consolidation efforts

---

## Summary: Effort by Agent

| Agent | Tasks | Total Hours | % of Total |
|-------|-------|-------------|------------|
| @code_architect | 0.1, 0.3 (partial), 1.3, 3.8 (partial) | 20h | 12.5% |
| @code_refactor | 1.1, 2.1, 2.2, 2.4, 2.5, 2.6, 2.7, 4.1, 4.5 | 110h | 68.8% |
| @test_stabilize | 3.1, 3.2, 3.3, 3.4, 4.2, 4.3, 4.4 | 44h + monitoring | 27.5% |
| @security_verification | 0.2, 0.3 (partial), 1.2, 2.3, 3.5, 3.6, 3.7 | 36h | 22.5% |
| **TOTAL** | **31 tasks** | **~160 hours** | **100%** |

**Note**: Some tasks overlap (monitoring is concurrent). Total calendar time: 5 weeks core + 2 weeks deployment = 7 weeks total.

---

## Critical Path

The critical path for this refactor (tasks that must be completed sequentially):

1. **Task 0.1** (Domain consultation) → 8h
2. **Task 0.2** (Security baseline) → 4h [Parallel with 0.1]
3. **Task 0.3** (RDB approval) → 6h
4. **Task 1.1** (Protocol diff analysis) → 24h
5. **Task 1.2** (Security diff analysis) → 8h [Can parallel with 1.3]
6. **Task 1.3** (Design common module) → 8h
7. **Task 2.1** (Create common module) → 24h
8. **Task 2.2** (GIOP 1.0 strategy) → 16h
9. **Task 2.3** (Mid-review) → 4h
10. **Task 2.4** (GIOP 1.1 strategy) → 20h
11. **Task 2.5** (GIOP 1.2 strategy) → 24h
12. **Task 2.6** (Cleanup) → 8h
13. **Task 2.7** (Create PR) → 2h
14. **Task 3.1** (Unit tests) → 8h
15. **Task 3.2** (Integration tests) → 12h
16. **Task 3.3** (Contract tests) → 16h
17. **Task 3.4** (Performance) → 8h [Can parallel with 3.5]
18. **Task 3.5** (Fuzzing) → 8h
19. **Task 3.6** (SAST) → 4h
20. **Task 3.7** (SRN) → 4h
21. **Task 3.8** (Merge) → 4h
22. **Task 4.1** (Deploy) → 4h
23. **Task 4.2-4.4** (Gradual enable) → 6 days monitoring
24. **Task 4.5** (Remove flag) → 8h + 1 week

**Critical Path Duration**: ~220 hours sequential work + ~10 days monitoring
**Calendar Time**: 5 weeks core work + 2 weeks deployment = **7 weeks total**

---

## Risk Mitigation per Task

### High-Risk Tasks (Extra Attention Required)

**Task 1.1: Protocol Diff Analysis** (24h)
- **Risk**: Incomplete analysis misses critical differences
- **Mitigation**: Line-by-line comparison, domain expert review, 3 days allocated
- **Fallback**: Extend analysis time if complexity higher than expected

**Task 2.2: GIOP 1.0 Strategy (Pilot)** (16h)
- **Risk**: Approach doesn't work, architectural redesign needed
- **Mitigation**: Pilot with simplest version (1.0), validate before extending
- **Fallback**: Rework strategy pattern if pilot fails

**Task 3.3: Contract Test Execution** (16h)
- **Risk**: Wire format incompatibility breaks CORBA protocol
- **Mitigation**: Test all message types per version, external system interoperability
- **Fallback**: Revert via feature flag, investigate wire format issues

**Task 3.5: Protocol Fuzzing** (8h)
- **Risk**: Vulnerabilities discovered in consolidated code
- **Mitigation**: Fuzz early and often, fix before merge
- **Fallback**: Address findings before SRN approval

**Task 4.2-4.4: Gradual Enablement** (6 days monitoring)
- **Risk**: Production issues with specific GIOP version
- **Mitigation**: Enable one version at a time, monitor 48h each, feature flag rollback
- **Fallback**: Instant rollback via feature flag for affected version

---

## Communication Schedule

**Daily Stand-up** (15 min) - During Weeks 2-5 (Implementation & Validation):
- Current task status per agent
- Blockers (especially protocol issues)
- Next 24h plan
- Per-version progress

**Mid-week Check-in** (30 min) - Weeks 2-5:
- Progress vs critical path
- Risk assessment (protocol, security, performance)
- Adjust timeline if needed

**Phase Completion Review** (1h) - End of each phase:
- Deliverables review
- Gate criteria validation
- Go/no-go decision for next phase

**Deployment Review** (30 min) - During gradual enablement:
- Per-version status (after each 48h monitoring period)
- Metrics review
- Decision to proceed to next version

---

## Contingency Plans

### If Timeline Slips

**Week 1 (Planning) Slips**:
- **Action**: Extend domain consultation, ensure thorough understanding
- **Impact**: Push protocol diff analysis start
- **Escalation**: If >1 week delay, reassess feasibility

**Week 2 (Protocol Diff) Slips**:
- **Action**: Allocate more time for diff analysis (critical foundation)
- **Impact**: May extend into Week 3, adjust implementation schedule
- **Escalation**: If complexity much higher, consider descoping or alternative approach

**Weeks 3-4 (Implementation) Slips**:
- **Action**: Focus on pilot (GIOP 1.0) first, validate approach
- **Impact**: May extend validation week
- **Escalation**: If pilot reveals fundamental issues, pause and redesign

**Week 5 (Validation) Slips**:
- **Action**: Prioritize critical tests (contract tests > performance)
- **Impact**: May delay merge by several days
- **Escalation**: If blocking issues found, rollback to planning with feature flag

**Deployment Slips**:
- **Action**: Extend monitoring periods if needed (48h → 72h)
- **Impact**: Adds 1-2 days per version
- **Escalation**: If issues persist, revert affected version and investigate

---

## Success Criteria Summary

**Phase 0 Complete When**:
- ✅ RDB-005 approved
- ✅ Domain experts consulted (8h)
- ✅ Security baseline captured (4h)

**Phase 1 Complete When**:
- ✅ Protocol diff analysis complete
- ✅ Shared logic identified (~180 LOC)
- ✅ Security-critical differences documented
- ✅ Common module architecture designed

**Phase 2 Complete When**:
- ✅ Common module + strategies implemented (all 3 versions)
- ✅ Compilation succeeds
- ✅ Feature flag functional
- ✅ PR created

**Phase 3 Complete When**:
- ✅ All 5 test layers pass (all 3 versions)
- ✅ Contract tests validate wire format (30/30)
- ✅ Protocol fuzzing clean (zero issues)
- ✅ SRN-005 issued (APPROVED)
- ✅ PR merged

**Phase 4 Complete When**:
- ✅ All 3 versions deployed with consolidated implementation
- ✅ Feature flag removed
- ✅ 1-week bake time complete with zero incidents
- ✅ Retrospective complete

---

**Document Version**: 1.0
**Created**: 2025-11-06
**Owner**: @code_architect
**Status**: READY FOR EXECUTION (pending RDB-005 approval)
