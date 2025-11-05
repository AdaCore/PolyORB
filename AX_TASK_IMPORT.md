# AX Task Import: Phase 1 Refactoring - PolyORB Deallocation Utility

**Workspace:** Refactor Cell
**Project:** PolyORB Phase 1 - Deallocation Utility Consolidation
**Date Created:** 2025-11-05
**Total Tasks:** 15

---

## Task 1: Review Phase 1 Deallocation Utility Refactoring Approach

**Assignee:** @CodeArchitect
**Priority:** Critical
**Status:** Pending
**Effort:** 30-60 minutes
**Dependencies:** None
**Blocked:** No
**Blocks:** Task 2

**Description:**
Review the Phase 1 deallocation utility refactoring approach and provide approval or feedback.

**Acceptance Criteria:**
- [ ] Read REFACTORING_PHASE1_DEALLOCATION.md
- [ ] Review utility package code (polyorb-utils-unchecked_deallocation.ads/.adb)
- [ ] Verify approach aligns with PolyORB coding standards
- [ ] Verify Ada best practices compliance
- [ ] Provide approval to proceed OR feedback on required changes

**Resources:**
- GitHub: https://github.com/heathdorn00/PolyORB/blob/master/REFACTORING_PHASE1_DEALLOCATION.md
- Commit: https://github.com/heathdorn00/PolyORB/commit/2b50932d1

---

## Task 2: Approve Migration Strategy for Remaining 73 Instances

**Assignee:** @CodeArchitect
**Priority:** Critical
**Status:** Pending
**Effort:** 15 minutes
**Dependencies:** Task 1
**Blocked:** Yes (awaiting Task 1)
**Blocks:** Task 6

**Description:**
Decide on the migration strategy for the remaining 73 deallocation instances.

**Acceptance Criteria:**
- [ ] Choose migration approach (all-at-once, batched, or incremental)
- [ ] Communicate decision to team via GitHub comment or message board

**Options:**
1. All-at-once migration (RECOMMENDED) - Single PR, 2-3 hours
2. Batched migration - Multiple PRs, 3-5 days
3. Incremental migration - Per-file PRs, 1-2 weeks

---

## Task 3: Define Testing Strategy for Refactoring Validation

**Assignee:** @TestAndStabilize
**Priority:** Medium
**Status:** Pending
**Effort:** 1-2 hours
**Dependencies:** None
**Blocked:** No
**Blocks:** Task 8

**Description:**
Determine how to validate that refactoring doesn't break behavior.

**Acceptance Criteria:**
- [ ] Review current test suite status
- [ ] Determine test coverage requirements
- [ ] Define pass/fail criteria
- [ ] Document testing procedure (GitHub comment or doc)

**Testing Options:**
1. Full test suite (requires Docker - see Task 4)
2. Compilation-only (fast, catches syntax/type errors)
3. Unit tests (create tests for refactored modules)
4. Hybrid (compilation + selected integration tests) - RECOMMENDED

**Resources:**
- BUILD_STATUS_UPDATE.md
- REFACTOR_QUICK_REFERENCE.md (Testing Checklist section)

---

## Task 4: Set Up Docker/Linux Test Environment for Full Build

**Assignee:** @TestAndStabilize
**Priority:** Low (Optional)
**Status:** Pending
**Effort:** 2-4 hours
**Dependencies:** None
**Blocked:** No
**Blocks:** None

**Description:**
Set up Docker/Linux environment for full PolyORB build and test suite execution.

**Acceptance Criteria:**
- [ ] Create Dockerfile with GNAT 12 + gprbuild
- [ ] Test full PolyORB build in Docker
- [ ] Document Docker usage for team
- [ ] Run baseline test suite and record results

**Context:**
macOS build has legacy build system issues. Docker enables full integration testing.

**Dockerfile Template:**
```dockerfile
FROM ubuntu:22.04
RUN apt-get update && apt-get install -y \
    gnat-12 gprbuild gcc make autoconf automake python3 git
WORKDIR /workspace
```

---

## Task 5: Security Review of Deallocation Utility Package

**Assignee:** @SecurityVerification
**Priority:** Low (Recommended)
**Status:** Pending
**Effort:** 30 minutes
**Dependencies:** None
**Blocked:** No
**Blocks:** None

**Description:**
Review the deallocation utility package for security concerns.

**Acceptance Criteria:**
- [ ] Review utility package code
- [ ] Verify no new attack vectors introduced
- [ ] Check Ada safety guidelines compliance
- [ ] Provide security review sign-off via GitHub comment

**Focus Areas:**
- Memory safety (dangling pointers, double-free potential)
- `pragma Inline` security implications
- Wrapper behavior vs. direct Ada.Unchecked_Deallocation

**Expected Outcome:**
No security concerns (utility is thin wrapper around standard Ada library)

**Files to Review:**
- src/polyorb-utils-unchecked_deallocation.ads
- src/polyorb-utils-unchecked_deallocation.adb

---

## Task 6: Migrate Remaining 73 Deallocation Instances

**Assignee:** @CodeRefactorAgent
**Priority:** Critical
**Status:** Blocked
**Effort:** 2-3 hours
**Dependencies:** Task 1, Task 2
**Blocked:** Yes (awaiting @CodeArchitect approval)
**Blocks:** Task 7

**Description:**
Migrate all remaining 73 deallocation instances to use the new utility package.

**Acceptance Criteria:**
- [ ] Identify all 73 remaining instances (grep completed)
- [ ] Create migration script (semi-automated)
- [ ] Refactor all 47 files
- [ ] Verify compilation of each file
- [ ] Create comprehensive commit message
- [ ] Push commit to GitHub

**Scope:**
- 47 files to refactor
- 73 `Ada.Unchecked_Deallocation` instances to replace
- Pattern: Replace direct instantiation with utility generic

**Migration Pattern:**
```ada
-- Before:
procedure Free is new Ada.Unchecked_Deallocation (Type, Type_Access);

-- After:
procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
  (Object => Type, Name => Type_Access);
```

---

## Task 7: Verify All Refactored Files Compile Successfully

**Assignee:** @CodeRefactorAgent
**Priority:** Critical
**Status:** Blocked
**Effort:** 30 minutes
**Dependencies:** Task 6
**Blocked:** Yes (awaiting Task 6)
**Blocks:** Task 9

**Description:**
Verify that all refactored files compile without errors or warnings.

**Acceptance Criteria:**
- [ ] Compile each refactored file individually
- [ ] Run gprbuild on affected projects
- [ ] Document any compilation issues
- [ ] Fix any issues found
- [ ] Generate compilation report (all files pass)

**Resources:**
- GNAT 14.2.0 toolchain
- Build commands in BUILD_STATUS_UPDATE.md

---

## Task 8: Run Full Test Suite on Refactored Code

**Assignee:** @CodeRefactorAgent
**Priority:** Medium
**Status:** Blocked
**Effort:** 1-2 hours
**Dependencies:** Task 3, Task 4, Task 7
**Blocked:** Yes (awaiting test environment)
**Blocks:** Task 9

**Description:**
Run full test suite to verify no behavior changes.

**Acceptance Criteria:**
- [ ] Run baseline tests (before refactoring)
- [ ] Run tests on refactored code
- [ ] Compare results
- [ ] Document any regressions
- [ ] Generate test results comparison report

**Note:**
Can proceed with compilation-only validation if Docker environment unavailable (skip Task 4).

---

## Task 9: Create PR for Phase 1 Full Migration

**Assignee:** @CodeRefactorAgent
**Priority:** Critical
**Status:** Pending
**Effort:** 30 minutes
**Dependencies:** Task 6, Task 7, Task 8
**Blocked:** Yes (awaiting Tasks 6-8)
**Blocks:** Task 10

**Description:**
After completing migration, compilation verification, and testing, create comprehensive PR for team review.

**Acceptance Criteria:**
- [ ] Create PR on GitHub
- [ ] Write PR description following template (from REFACTORING_PHASE1_DEALLOCATION.md)
- [ ] Document before/after metrics
- [ ] Include migration notes
- [ ] Reference all related documentation

**PR Template Sections:**
- Summary (goal, technique)
- Scope (files, LOC changed)
- Behavior (intended changes, tests)
- Metrics (duplication reduction, complexity delta)
- Risks & Rollback

---

## Task 10: Review and Merge Phase 1 PR

**Assignee:** @CodeArchitect
**Priority:** Critical
**Status:** Pending
**Effort:** 1-2 hours
**Dependencies:** Task 9
**Blocked:** Yes (awaiting Task 9)
**Blocks:** Task 11

**Description:**
Review the Phase 1 full migration PR, verify metrics, approve, and merge to main branch.

**Acceptance Criteria:**
- [ ] Review PR code changes
- [ ] Verify metrics are accurate
- [ ] Verify all acceptance criteria met
- [ ] Approve PR
- [ ] Merge to main branch
- [ ] Mark Phase 1 as complete

---

## Task 11: Update Refactoring Metrics After Phase 1 Completion

**Assignee:** @CodeRefactorAgent
**Priority:** Medium
**Status:** Pending
**Effort:** 30 minutes
**Dependencies:** Task 10
**Blocked:** Yes (awaiting Task 10)
**Blocks:** Task 12

**Description:**
Update all refactoring documentation with final Phase 1 metrics and mark tasks complete.

**Acceptance Criteria:**
- [ ] Update REFACTOR_QUICK_REFERENCE.md (check off deallocation task)
- [ ] Update TASK_ASSIGNMENTS_PHASE1.md (mark all complete)
- [ ] Create PHASE1_COMPLETION_REPORT.md
- [ ] Document final metrics
- [ ] Verify success criteria met
- [ ] Update documentation to reflect "Phase 1 Complete" status
- [ ] Commit and push updates to GitHub

**Files to Update:**
- REFACTOR_QUICK_REFERENCE.md
- TASK_ASSIGNMENTS_PHASE1.md
- PHASE1_COMPLETION_REPORT.md (new)

---

## Task 12: Prioritize Phase 2 Refactoring Targets

**Assignee:** @CodeArchitect
**Priority:** Medium
**Status:** Pending
**Effort:** 1 hour
**Dependencies:** Task 11
**Blocked:** Yes (awaiting Phase 1 completion)
**Blocks:** Task 13 OR Task 14

**Description:**
Review REFACTOR_QUICK_REFERENCE.md and decide which Phase 2 refactoring to prioritize.

**Acceptance Criteria:**
- [ ] Review all Phase 2 options
- [ ] Evaluate effort vs. impact
- [ ] Select Phase 2 target
- [ ] Communicate decision to team
- [ ] Provide approval to proceed with planning

**Phase 2 Options:**
1. TypeCode enumeration (3-5 days, medium effort, low-medium risk)
2. POA control flow extraction (3-5 days, low-medium effort, low risk)
3. GIOP protocol consolidation (1-2 weeks, high effort, medium risk)
4. polyorb-any.adb decomposition (1-2 weeks, high effort, medium risk)

**Recommendation:**
Start with TypeCode or POA (lower risk, faster completion).

---

## Task 13: Plan TypeCode Enumeration Refactoring (Phase 2)

**Assignee:** @CodeRefactorAgent
**Priority:** Low (Future phase)
**Status:** Pending (Conditional)
**Effort:** 2-3 hours
**Dependencies:** Task 12 (if TypeCode selected)
**Blocked:** Yes (conditional on Task 12 decision)
**Blocks:** None

**Description:**
Plan the TypeCode enumeration refactoring if selected by @CodeArchitect.

**Acceptance Criteria:**
- [ ] Create REFACTORING_PHASE2_TYPECODE.md
- [ ] Document migration approach
- [ ] Complete risk assessment
- [ ] Create task assignments for Phase 2
- [ ] Estimate effort and timeline

**Scope:**
- File: src/polyorb-representations-cdr.adb (lines 106-143)
- Impact: Replace 40 TypeCode constants with enumeration
- Benefit: Type safety + consolidation

**Note:**
Only execute if @CodeArchitect selects TypeCode for Phase 2.

---

## Task 14: Plan GIOP Protocol Consolidation (Phase 2)

**Assignee:** @CodeRefactorAgent
**Priority:** Low (Future phase)
**Status:** Pending (Conditional)
**Effort:** 3-4 hours
**Dependencies:** Task 12 (if GIOP selected)
**Blocked:** Yes (conditional on Task 12 decision)
**Blocks:** None

**Description:**
Plan the GIOP protocol consolidation if selected by @CodeArchitect.

**Acceptance Criteria:**
- [ ] Create REFACTORING_PHASE2_GIOP.md
- [ ] Complete protocol diff analysis
- [ ] Document consolidation strategy
- [ ] Create task assignments for Phase 2
- [ ] Estimate effort and timeline

**Scope:**
- Files: giop_1_0.adb, giop_1_1.adb, giop_1_2.adb
- Impact: 200-300 LOC deduplication
- Effort: High (complex logic, multiple version handling)

**Note:**
Only execute if @CodeArchitect selects GIOP for Phase 2.

---

## Task 15: Fix Build System for macOS (Optional)

**Assignee:** @TestAndStabilize
**Priority:** Low (Optional)
**Status:** Pending
**Effort:** 2-4 hours
**Dependencies:** None
**Blocked:** No
**Blocks:** None

**Description:**
Fix legacy configure script issues on macOS to enable full build.

**Acceptance Criteria:**
Choose one option and complete:
- [ ] **Option 1:** Patch configure.ac for BSD compatibility, verify full build works on macOS
- [ ] **Option 2:** Document Docker-only approach, train team
- [ ] **Option 3:** Document decision to defer, add to backlog

**Context:**
Documented in BUILD_STATUS_UPDATE.md. Current workaround: use Docker/Linux for full builds.

**Options:**
1. Patch configure.ac for BSD compatibility
2. Use Docker exclusively (current approach)
3. Defer until higher priority

**Note:**
Not blocking. Docker workaround is acceptable.

---

## Task Summary

### By Priority
- **Critical (7 tasks):** 1, 2, 6, 7, 9, 10
- **Medium (4 tasks):** 3, 8, 11, 12
- **Low (4 tasks):** 4, 5, 13, 14, 15

### By Assignee
- **@CodeArchitect (4 tasks):** 1, 2, 10, 12
- **@TestAndStabilize (3 tasks):** 3, 4, 15
- **@SecurityVerification (1 task):** 5
- **@CodeRefactorAgent (7 tasks):** 6, 7, 8, 9, 11, 13, 14

### By Status
- **Pending (8 tasks):** 1, 2, 3, 4, 5, 11, 12, 15
- **Blocked (5 tasks):** 6, 7, 8, 9, 10
- **Conditional (2 tasks):** 13, 14

### Critical Path (11 tasks)
```
Task 1 → Task 2 → Task 6 → Task 7 → Task 9 → Task 10 → Task 11 → Task 12
                                       ↑
Task 3 → Task 4 → Task 8 ──────────────┘
```

### Timeline Estimates
- **Best Case:** 10-14 hours (all approvals fast)
- **Realistic:** 2-3 weeks (accounting for review cycles)
- **Worst Case:** 4-6 weeks (if extensive testing required)

---

## Import Instructions

**For AX Workspace Import:**

1. Create new project: "PolyORB Phase 1 - Deallocation Utility"
2. Import tasks 1-15 with above details
3. Set dependencies as specified in "Blocks/Blocked" fields
4. Assign to team members as specified
5. Set priorities as specified
6. Link to GitHub repository: https://github.com/heathdorn00/PolyORB

**Key Documentation Links:**
- Task Assignments: TASK_ASSIGNMENTS_PHASE1.md
- Gap Analysis: TASK_GAP_ANALYSIS.md
- Technical Details: REFACTORING_PHASE1_DEALLOCATION.md
- Team Update: REFACTOR_CELL_UPDATE_2025-11-05.md

---

**Generated:** 2025-11-05
**Repository:** https://github.com/heathdorn00/PolyORB
**Workspace:** Refactor Cell
