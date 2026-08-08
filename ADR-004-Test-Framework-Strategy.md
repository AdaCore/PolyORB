# ADR-004: Test Framework Selection and Testing Strategy

**Status**: DRAFT
**Date**: 2025-11-05
**Authors**: @CodeArchitect, @test_stabilize
**Reviewers**: @code_refactor, @security_verification
**Supersedes**: N/A
**Related**: RDB-002, RDB-001, ADR-001

## Context

Our microservices migration (RDB-001) has highlighted critical gaps in testing infrastructure:

**Current State**:
- 42% average test coverage (target: 80%+)
- Inverted test pyramid: 40% E2E, 35% integration, 25% unit
- 18-minute test suite execution time
- 8% flaky test rate
- No mutation testing or contract testing
- Manual performance regression detection
- Untested rollback paths

**Business Impact**:
- 15% deployment failure rate (target: <5%)
- 3 production incidents in Q4 2024 due to untested code paths
- 4-hour MTTR (target: <15 minutes)
- Cannot confidently deploy 5×/week (currently 1×/month)

**Technical Debt**:
- Weak assertions that pass despite broken logic
- Schema drift between services (no contract tests)
- Performance regressions caught in production
- Feature flags never tested in automation

We need a comprehensive testing strategy aligned with the principles established by @test_stabilize to achieve zero-regression refactors and enable continuous deployment.

## Decision

We will adopt a **multi-layered testing strategy** with the following frameworks and practices:

### Test Pyramid (50-30-15-5 Distribution)

**Layer 1: Unit Tests (50%)** - 3,600 tests
- **C++**: GoogleTest + GoogleMock
- **Ada**: AUnit
- **TypeScript**: Jest
- **Target**: <10ms per test, 70% coverage
- **Scope**: Business logic, domain models, pure functions

**Layer 2: Contract Tests (30%)** - 360 tests
- **Framework**: Pact CDC (Consumer-Driven Contracts)
- **Infrastructure**: Pact Broker for contract storage
- **Target**: <60s total, 100% of 256 service connections
- **Scope**: API contracts, message schemas

**Layer 3: Integration Tests (15%)** - 180 tests
- **C++**: GoogleTest + TestContainers
- **Ada**: AUnit + Docker Compose
- **TypeScript**: Jest + Testcontainers
- **Target**: <90s total, 15% coverage
- **Scope**: Service boundaries, database interactions

**Layer 4: E2E Tests (5%)** - 60 tests
- **Framework**: gRPC-based test harness
- **Target**: <5min total
- **Scope**: Critical user journeys only

### Advanced Testing Practices

**Mutation Testing**:
- **Framework**: Stryker (TS), mutmut (Ada-compatible), Mull (C++)
- **Threshold**: ≥80% mutation score (new code), ≥75% (legacy)
- **Execution**: Per-PR for changed files, nightly for full scan
- **CI Gate**: Fail build if mutation score <75%

**Property-Based Testing**:
- **Framework**: fast-check (TS), QuickCheck-Ada (Ada), RapidCheck (C++)
- **Target**: 20 domain invariants tested
- **Scope**: Payment calculations, data transformations, state machines

**Performance Testing**:
- **Framework**: k6 (load testing), autocannon (API benchmarking)
- **Thresholds**: P95 ≤+10%, P99 ≤+15%
- **Execution**: Every PR (baseline comparison)
- **CI Gate**: Fail if P95 regression >20%

**Chaos Engineering**:
- **Framework**: Chaos Mesh (Kubernetes-native)
- **Scenarios**: Pod termination, network latency (50-200ms), resource exhaustion
- **Frequency**: Weekly automated runs (Weeks 18-20+)

### Testing Workflow

**Before Refactor** (Characterization Phase):
1. Run baseline mutation tests
2. Write characterization tests (lock in current behavior)
3. Capture snapshot/golden tests for complex outputs
4. Record performance baselines (P50/P95/P99)

**During Refactor** (Dual Implementation):
1. Implement new code alongside old (Strangler Fig)
2. Run both paths in shadow mode
3. Log divergences for analysis
4. Use feature flags for gradual cutover (1% → 10% → 50% → 100%)

**After Refactor** (Validation):
1. Run contract tests (verify API compatibility)
2. Run mutation tests (ensure test quality)
3. Run performance tests (check latency regression)
4. Test rollback path (validate feature flag toggles)

## Consequences

### Positive

**Faster Feedback Loops** ✅
- Unit tests (<10ms) provide instant feedback during development
- Developers catch bugs before committing (shift-left testing)
- Expected: 80% of bugs caught in unit tests (vs 20% currently)

**Higher Quality Tests** ✅
- Mutation testing exposes weak assertions
- Contract tests prevent schema drift
- Property-based tests discover edge cases
- Expected: 60% reduction in production incidents

**Confident Deployments** ✅
- Automated rollback validation eliminates "hope-based deployments"
- Performance gates prevent latency regressions
- Feature flags enable instant rollback
- Expected: <5% deployment failure rate (from 15%)

**Reduced Test Maintenance** ✅
- Fewer E2E tests (200 → 60) means less flakiness
- Contract tests eliminate manual API coordination
- Test pyramid prevents brittle integration tests
- Expected: <0.5% flaky test rate (from 8%)

**Enables 5×/Week Deployments** ✅
- Fast test suite (<5min) doesn't block CI/CD
- High confidence (80% coverage, 80% mutation score) enables frequent deploys
- Automated gates (coverage, mutation, performance) enforce quality

### Negative

**Initial Time Investment** ⚠️
- 24 weeks to fully implement testing infrastructure
- Teams must learn new frameworks (Pact, Stryker, k6, fast-check)
- **Mitigation**:
  - Incremental rollout (pilot service → critical services → all services)
  - 2-hour training workshops per framework
  - Pair programming for first 10 tests
  - Clear documentation and examples

**Test Suite Complexity** ⚠️
- 4,200 tests (vs 1,600 currently) increases maintenance burden
- Multiple frameworks require expertise in GoogleTest, AUnit, Jest, Pact, k6, Stryker
- **Mitigation**:
  - Standardize patterns (test data factories, fixture builders)
  - Automated test generation where possible
  - Clear ownership per service (team responsible for their tests)

**CI/CD Pipeline Slowdown** ⚠️
- Mutation testing can slow down PR pipeline (10-30× slower than unit tests)
- Risk of >20min test suites if not optimized
- **Mitigation**:
  - Run mutation tests on changed files only (not full codebase)
  - Run full mutation scan nightly (not per-commit)
  - Parallelize tests (8 workers, test sharding)
  - Cache dependencies (Docker layer caching)
- **Acceptance**: Test suite must remain <5min for PR pipeline

**Tool Integration Overhead** ⚠️
- Pact Broker requires deployment and maintenance
- k6 requires baseline management and trend analysis
- Stryker requires configuration per service
- **Mitigation**:
  - Deploy Pact Broker as Kubernetes service (managed by ops)
  - Use Grafana for automated trend dashboards
  - Standardize Stryker config (template for all services)

### Neutral

**Coverage vs Mutation Score Trade-Off**
- 80% line coverage ≠ 80% quality
- Mutation score is better quality metric but slower to compute
- **Strategy**: Use coverage for fast feedback, mutation score for quality gate

**Contract Testing Coordination**
- 256 service connections require coordination between teams
- **Strategy**: Pact Broker automates "can-i-deploy" checks (no manual coordination needed)

## Alternatives Considered

### Alternative 1: Keep Current Testing Approach

**Approach**: Continue with ad-hoc testing, 42% coverage, no quality gates

**Pros**:
- No time investment required
- No learning curve
- No new tools to maintain

**Cons**:
- Cannot achieve 5×/week deployment velocity
- Production incidents continue (3 in Q4 2024)
- Refactors remain high-risk
- Technical debt compounds

**Rejected Because**: Incompatible with business goals (5×/week deploys, 99.9% uptime, <5% failure rate).

### Alternative 2: 100% End-to-End Testing

**Approach**: Double down on E2E tests, skip unit/contract tests

**Pros**:
- Tests full system behavior
- No mocking required
- Catches integration issues

**Cons**:
- Extremely slow (>1 hour test suite)
- Highly flaky (10-15% flaky rate typical)
- Difficult to debug failures
- Doesn't scale with microservices (256 service connections)

**Rejected Because**: Antithetical to fast feedback and continuous deployment.

### Alternative 3: Manual Testing Only

**Approach**: QA team manually tests before each release

**Pros**:
- No automation investment
- Human intuition catches UI/UX issues

**Cons**:
- Doesn't scale to 5×/week deployments
- Error-prone (human fatigue)
- Cannot test performance regressions systematically
- Blocks continuous deployment

**Rejected Because**: Manual testing is incompatible with continuous deployment.

### Alternative 4: Single Framework (GoogleTest for All)

**Approach**: Use only GoogleTest for C++, Ada, and TypeScript

**Pros**:
- Single framework to learn
- Consistent tooling

**Cons**:
- GoogleTest not idiomatic for Ada or TypeScript
- Misses language-specific features (Ada's contract-based programming, TS type inference)
- Community support weaker for non-C++ usage

**Rejected Because**: Ada and TypeScript have superior native frameworks (AUnit, Jest).

## Implementation Plan

See **RDB-002 Migration Strategy** for full 24-week roadmap.

**Phase 1 (Weeks 1-4)** - Foundation:
- Install GoogleTest, AUnit, Jest
- Deploy Pact Broker
- Run baseline mutation tests
- Implement k6 performance baselines

**Phase 2 (Weeks 5-12)** - Test Pyramid Rebalancing:
- Add 2,400 unit tests
- Create 256 Pact contracts
- Reduce E2E tests (200 → 140)

**Phase 3 (Weeks 13-20)** - Advanced Patterns:
- Fix surviving mutants (achieve 80% mutation score)
- Implement property-based tests
- Shadow testing for high-risk refactors
- Chaos engineering

**Phase 4 (Weeks 21-24)** - Optimization:
- Reduce E2E tests (140 → 60)
- Optimize test speed (<5min)
- Final coverage push (80%+)

## Metrics and Monitoring

**Test Quality Dashboard** (Grafana):
1. **Coverage Trends**: Line, branch, function coverage per service
2. **Mutation Score**: Surviving mutants, mutation score trend
3. **Contract Coverage**: % of 256 connections with Pact contracts
4. **Performance**: P50/P95/P99 latency trends, regression alerts
5. **Flakiness**: Flaky test rate, quarantined tests

**CI/CD Gates**:
- ✅ Coverage ≥80% on changed code
- ✅ Mutation score ≥75% on changed code
- ✅ All contract tests pass
- ✅ No performance regression >20% P95
- ✅ <0.5% flaky test rate

**Weekly Metrics** (Tracked in RDB-002):
| Metric | Target | Current | Trend |
|--------|--------|---------|-------|
| Overall Coverage | 80% | 42% | ↗ |
| Mutation Score | 80% | N/A | ➡ |
| Test Suite Time | <5min | 18min | ↘ |
| Flaky Test Rate | <0.5% | 8% | ↘ |

## Testing Principles Reference

Per @test_stabilize collaboration:

**Baseline Behavior Capture**:
- Characterization tests before refactor
- Snapshot/golden tests for complex outputs
- Traffic recording for high-risk APIs

**Behavioral Equivalence**:
- Dual implementation (Strangler Fig pattern)
- Shadow mode comparison
- Feature flag gradual cutover

**Performance Regression Detection**:
- Acceptable: P95 ≤+10%, P99 ≤+15%
- Must Fix: P95 >+20%, P99 >+25%
- k6 automated benchmarking

**Rollback Safety**:
- Feature flag validation
- Backward compatibility tests
- Automated rollback drills

**Mutation Testing**:
- Baseline before refactor
- Incremental on changed files
- Higher bar for new code (≥85% vs ≥75%)

## Risks and Mitigations

See **RDB-002 Risk Assessment** for detailed analysis.

**Critical Risks**:
1. **Test Suite Slowdown**: Parallelize, shard tests, run mutation tests nightly
2. **Flaky Test Proliferation**: Deterministic test data, retry 3×, quarantine flaky tests
3. **Contract Coordination**: Pact Broker automates "can-i-deploy" checks

## Decision Rationale

**Why This Strategy?**
1. **Test Pyramid**: Industry best practice (backed by Google Testing Blog, Martin Fowler)
2. **Mutation Testing**: Ensures test quality, not just coverage
3. **Contract Testing**: Microservices require API contract validation
4. **Performance Testing**: Latency regressions are production incidents
5. **Chaos Engineering**: Resilience testing is mandatory for 99.9% uptime

**Why Now?**
- Microservices migration (RDB-001) creates 256 service connections requiring contracts
- 15% deployment failure rate is unacceptable for continuous deployment
- Technical debt compounding (3 production incidents in Q4 2024)

**Why Not Wait?**
- Every week delays costs ~1 production incident ($50K-$200K impact)
- Refactors blocked by low test confidence
- Cannot achieve DORA metrics targets (5×/week, <1 day lead time)

## References

- **Collaboration**: @test_stabilize Q&A (2025-11-05) - 10 questions on testing strategy
- **Google Testing Blog**: [Test Sizes](https://testing.googleblog.com/2010/12/test-sizes.html)
- **Martin Fowler**: [Testing Strategies in a Microservice Architecture](https://martinfowler.com/articles/microservice-testing/)
- **Pact**: [Consumer-Driven Contracts](https://docs.pact.io/)
- **Mutation Testing**: [Stryker Mutator](https://stryker-mutator.io/)
- **k6**: [Load Testing](https://k6.io/docs/)

## Appendix: Framework Comparison

**Unit Testing Frameworks Evaluated**:
| Framework | Language | Pros | Cons | Decision |
|-----------|----------|------|------|----------|
| GoogleTest | C++ | Industry standard, rich matchers, mocking | - | ✅ SELECTED |
| Catch2 | C++ | Header-only, BDD style | Less mature than GoogleTest | ❌ REJECTED |
| AUnit | Ada | Native Ada support, contract-aware | - | ✅ SELECTED |
| Jest | TypeScript | Fast, snapshot testing, mocking | - | ✅ SELECTED |
| Vitest | TypeScript | Faster than Jest | Less mature ecosystem | ❌ REJECTED |

**Contract Testing Frameworks Evaluated**:
| Framework | Pros | Cons | Decision |
|-----------|------|------|----------|
| Pact | Industry standard, multi-language, "can-i-deploy" | Requires broker | ✅ SELECTED |
| Spring Cloud Contract | Java-focused, generates stubs | Doesn't support C++/Ada | ❌ REJECTED |
| OpenAPI Diff | Simple, schema-based | No consumer-driven contracts | ❌ REJECTED |

**Mutation Testing Frameworks Evaluated**:
| Framework | Language | Pros | Cons | Decision |
|-----------|----------|------|------|----------|
| Stryker | TypeScript/JS | Mature, fast, good reporting | - | ✅ SELECTED |
| Mull | C++ | LLVM-based, accurate | Slower than Stryker | ✅ SELECTED |
| mutmut | Python | Fast | No Ada support, adapted for Ada via custom tooling | ✅ ADAPTED |

---

**Status**: DRAFT
**Signed**: @CodeArchitect (2025-11-05)
**Next Review**: Pending @test_stabilize approval
