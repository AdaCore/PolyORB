# PolyORB Refactoring Quick Reference

## At-a-Glance Summary

| Metric | Value | Status |
|--------|-------|--------|
| Total Ada Files | 1,144 | ✓ Well-organized |
| Source Lines | 177,521 | ⚠ Needs modularization |
| Files >500 LOC | 85 | ⚠ High refactoring candidates |
| Duplicated Patterns | 48+ | ⚠ High priority deduplication |
| XXX/FIXME Comments | 20+ | ⚠ Tech debt |
| Test Suite | Comprehensive | ✓ Good coverage |
| Documentation | Excellent | ✓ Well-documented |

---

## Critical Files Needing Refactoring

### Top 10 Largest Files

```
1. polyorb-any.adb                    4,302 LOC  → DECOMPOSE
2. templates_parser.adb               4,036 LOC  → EXTRACT
3. dynamicany-dynany-impl.adb         2,907 LOC  → DECOMPOSE
4. polyorb-representations-cdr.adb    2,737 LOC  → EXTRACT + SIMPLIFY
5. s-parint.adb                       2,726 LOC  → DECOMPOSE
6. aws-client.adb                     2,307 LOC  → EXTRACT
7. polyorb-representations-srp.adb    2,274 LOC  → EXTRACT
8. corba.adb                          2,093 LOC  → DECOMPOSE
9. polyorb-poa.adb                    1,711 LOC  → SIMPLIFY
10. polyorb-orb.adb                   1,506 LOC  → EXTRACT
```

---

## Quick Actions (Ranked by Impact/Effort)

### Immediate (1-2 Days)
- [ ] Extract `PolyORB.Utils.Deallocation` generic utility
  - **Impact:** 74 duplication patterns → 1 template
  - **Files:** 48 to refactor
  - **Effort:** Low (pattern matching + extraction)
  
### Short Term (3-5 Days)
- [ ] Replace TypeCode constants with enumeration
  - **File:** `src/polyorb-representations-cdr.adb` (lines 106-143)
  - **Impact:** Type safety + 40 const consolidation
  - **Effort:** Medium
  
- [ ] Extract control flow in POA management
  - **File:** `src/polyorb-poa.adb` (lines 240-263)
  - **Impact:** Reduce nesting from 4-5 → 2 levels
  - **Effort:** Low-Medium

### Medium Term (1-2 Weeks)
- [ ] Consolidate GIOP protocol versions
  - **Files:** giop_1_0.adb, giop_1_1.adb, giop_1_2.adb
  - **Impact:** 200-300 LOC deduplication
  - **Effort:** High (complex logic)

- [ ] Decompose `polyorb-any.adb`
  - **Strategy:** Split into 3 child packages
  - **Impact:** Improved testability + maintainability
  - **Effort:** High

### Documentation (Ongoing)
- [ ] Address XXX/FIXME comments (20+ locations)
- [ ] Add inline documentation to algorithms
- [ ] Create module dependency diagrams

---

## Risk Management

### Before Refactoring
1. Snapshot test results
2. Document current metrics (complexity, coverage)
3. Create feature branch per refactoring
4. Set up pre-commit hooks for critical paths

### During Refactoring
1. Keep changes atomic (one concern per PR)
2. Maintain <200 LOC per PR
3. Run full test suite before each push
4. Document behavior preservation

### After Refactoring
1. Compare metrics to baseline
2. Update documentation
3. Add migration notes if API changes
4. Close related XXX/FIXME comments

---

## Code Smell Locations

### Code Duplication
- **Deallocation:** 48 files, 74 occurrences
  - Most files: `src/polyorb-poa_types.ads` (6x)
  - Most files: `src/corba/portableserver-helper.adb` (7x)

- **GIOP Protocol:** 3,653 LOC across giop_1_0/1.1/1.2
  - Structural duplication in request/response handling
  - Version-specific differences embedded in similar logic

### Long Functions
- `polyorb-any.adb`: Generic instantiations (lines 270-399)
- `polyorb-poa.adb`: Policy application loop (lines 240-263)
- `polyorb-orb.adb`: Event loop (lines 500-572)
- `polyorb-representations-cdr.adb`: Marshalling logic (scattered)

### Poor Naming
- Single-letter functions: `C()` (enabled?), `O()` (output?)
- Abbreviated packages: `PL`, `PTM`, `PTCV`, `PSN`
- Inconsistent: `Unmarshall` vs `Unmarshal`
- Unclear: `From_Any_G`, `To_Any_G` (G suffix ambiguous)

### Magic Numbers
- TypeCode IDs (0-36, 16#ffffffff#)
- Buffer alignments
- Socket timeouts
- Stream element counts

### Deep Nesting
- 4-5 levels: `polyorb-poa.adb` (policy handling)
- 3-4 levels: `s-parint.adb` (partition init)
- 3-4 levels: `polyorb-protocols-giop-giop_1_2.adb` (unmarshalling)

---

## File Organization Strategy

### Current Issues
```
polyorb-any.adb (4,302 LOC) - Monolithic
├─ Elementary types (Elementary_Any instantiations)
├─ Aggregate types (sequences, arrays, structs)
├─ Equality testing (Agg_Container_Eq)
└─ Type checking & conversion functions
```

### Proposed Structure
```
polyorb-any.ads/adb                   (public API)
├─ polyorb-any-elementary.ads/adb     (element type handlers)
├─ polyorb-any-comparison.ads/adb     (equality & comparison)
├─ polyorb-any-aggregate.ads/adb      (complex type handlers)
└─ polyorb-any-conversion.ads/adb     (type conversions)
```

---

## Testing Strategy

### Test Coverage Baseline
- Run full testsuite before refactoring
- Document current pass/fail counts
- Identify any flaky tests

### Per-Refactoring Testing
1. Unit test the extracted component
2. Run affected module tests
3. Run full regression suite
4. Verify coverage didn't decrease

### Metrics to Track
- Total tests passing
- Code coverage (lines & branches)
- Cyclomatic complexity (before/after)
- Build time (should remain constant)

---

## Recommended Refactoring Order

### Safe to Start
1. ✓ Extract Deallocation utility (non-invasive)
2. ✓ TypeCode enumeration (well-defined scope)
3. ✓ Extract nested logic (isolated concerns)

### Proceed with Caution
4. ⚠ GIOP consolidation (protocol critical)
5. ⚠ polyorb-any decomposition (core functionality)
6. ⚠ DSA partition refactoring (distributed system)

### Only with Extensive Testing
7. ✗ ORB event loop changes
8. ✗ Marshalling algorithm changes
9. ✗ Public API modifications

---

## Useful Search Patterns

### Find all deallocation patterns
```bash
grep -r "procedure Free is new Ada.Unchecked_Deallocation" src/
```

### Find XXX/FIXME markers
```bash
grep -r "XXX\|FIXME\|TODO" src/ --include="*.adb" --include="*.ads"
```

### Find deeply nested code (3+ indentation levels)
```bash
grep -r "^      " src/ | head -20  # 6 spaces = 3 indents
```

### Find magic numbers
```bash
grep -r "[0-9]\{4\}[;,]" src/ --include="*.adb" | grep -v "^[[:space:]]*--"
```

### Find single-letter functions
```bash
grep -r "function [A-Z] \|procedure [A-Z]" src/ --include="*.ads"
```

---

## Checklist Before Creating PR

- [ ] Baseline tests passing
- [ ] Code changes <200 LOC
- [ ] No behavior changes (or clearly documented)
- [ ] New code follows Ada style guidelines
- [ ] Comments added for non-obvious logic
- [ ] Cycle complexity reduced (or documented)
- [ ] Tests updated/added for refactored code
- [ ] Documentation updated if API changed
- [ ] Migration notes in PR description
- [ ] Full test suite passing

---

## Communication Template

When opening a refactoring PR:

```
## Summary
- **Goal:** [brief goal statement]
- **Technique:** [Extract/Rename/Decompose/etc.]

## Scope
- **Files:** [list of files]
- **LOC Changed:** [insertion+deletion count]

## Behavior
- **Intended:** No behavior change
- **Tests:** [list tests that validate behavior preservation]

## Metrics
- **Complexity Δ:** Before → After (cyclomatic complexity)
- **Coverage Δ:** [coverage change]

## Risks & Rollback
- **Risk:** [Low/Medium/High]
- **Rollback:** git revert [commit]
```

---

*Last Updated: 2025-11-04*
