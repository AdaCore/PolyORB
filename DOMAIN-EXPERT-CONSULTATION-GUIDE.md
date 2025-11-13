# Domain Expert Consultation Guide - RDB-003 Phase 1A

**Task ID**: 2332e7
**Purpose**: Clarify 3 hidden security properties before Phase 1A implementation
**Owner**: @refactor_agent
**Duration**: 2.5 hours total
**Timeline**: Week 1 (URGENT - BLOCKS Phase 1A)
**Created by**: @code_architect
**Date**: 2025-11-06

---

## Executive Summary

Before implementing Phase 1A (13 security-critical deallocation instances), we must consult 3 domain experts to clarify hidden security properties that could cause production incidents if violated. This guide provides structured questions, context, and documentation templates to make consultations efficient and actionable.

**Critical Context**: @security_verification's RDB-003 review identified 3 MODERATE-priority hidden properties that MUST be clarified in Week 1 before implementation begins. These consultations directly inform Security Invariants and prevent incidents similar to the "session token in-use flag" production bug mentioned in their review.

---

## Consultation Overview

| Expert | Focus Areas | Duration | Priority | Files Affected |
|--------|-------------|----------|----------|----------------|
| **@session_expert** | Session token "in-use" flag, deallocation lifecycle | 30 min | HIGH | `/src/session/session_handler.adb:412` |
| **@crypto_team_lead** | Crypto buffer caching, random pool, zeroization | 1 hour | HIGH | `/src/crypto/buffer_manager.adb:156`, `/src/crypto/random_pool.adb:123` |
| **@auth_architect** | ACL reference counting, OAuth timing, mutex protection | 1 hour | HIGH | `/src/auth/acl_manager.adb:89`, `/src/auth/oauth_client.adb:234` |

**Total**: 2.5 hours

---

## Why These Consultations Are Critical

**Context from @security_verification RDB-003 Review**:

> **Hidden Properties to Clarify (MODERATE - Week 1)**
>
> **Property 1: Session Token "In-Use" Flag** (MODERATE)
> - **Location**: `/src/session/session_handler.adb:412`
> - **Issue**: Does session_handler check `in_use` flag before deallocation?
> - **Security Impact**: DoS (CWE-613), race conditions if missing
> - **Action**: Verify with @session_expert (30 min consultation)
> - **Mitigation**: Add explicit check + regression test
>
> **Property 2: Crypto Buffer Caching** (MODERATE)
> - **Location**: `/src/crypto/buffer_manager.adb:156`
> - **Issue**: Are crypto buffers cached/reused between operations?
> - **Security Impact**: Information leakage across operations if cached
> - **Action**: Verify with @crypto_team_lead (1 hour consultation)
> - **Mitigation**: Prefer no-caching; if caching, add zeroize-on-return invariant
>
> **Property 3: OAuth Token Refresh Race** (MODERATE)
> - **Location**: `/src/auth/oauth_client.adb:234`
> - **Issue**: Implicit dependency on token validity during deallocation?
> - **Security Impact**: Use-after-free, authentication bypass
> - **Action**: Verify with @auth_architect (1 hour consultation)
> - **Mitigation**: Add mutex protection + reference counting if needed

**Risk if Not Clarified**: Implementing Phase 1A without understanding these properties could:
- Introduce race conditions (session token deallocation while in-use)
- Leak sensitive data (crypto buffer reuse without zeroization)
- Cause authentication bypass (OAuth token timing issues)
- Block Phase 1A implementation (discovered mid-implementation, costly rework)

---

## Consultation 1: @session_expert (30 minutes)

### Context

**File**: `/src/session/session_handler.adb:412`
**Change**: Add memory zeroization for session tokens before deallocation
**Risk**: Session tokens are CRITICAL priority (JWT, OAuth tokens)

**Background from RDB-003**:
- Session tokens are one of 3 CRITICAL instances in Phase 1A
- Current code may have implicit "in-use" flag check
- Production bug history: 10× auth failure rate when in-use flag ignored
- Security Impact: DoS, race conditions, authentication failures

### Questions to Ask

**Question 1: In-Use Flag Requirement**
```
Q: Does session_handler.adb:412 require checking an "in-use" flag
   before deallocating a session token?

Context: We're adding memory zeroization for session tokens. Need to
understand if there's an implicit check we must preserve.

Acceptable Answers:
- YES: Flag name is [X], check happens at line [Y]
- NO: No flag check required, safe to deallocate anytime
- PARTIAL: Flag exists but only for [specific scenario]
```

**Question 2: Session Lifecycle Dependencies**
```
Q: Are there any implicit dependencies on session token validity
   during the deallocation process?

Context: Deallocation will now include zeroization step. Does timing matter?

Acceptable Answers:
- Token must be invalidated BEFORE deallocation
- Token can be invalidated DURING deallocation
- No dependency, deallocation is independent
```

**Question 3: Race Condition Scenarios**
```
Q: What race conditions should we be aware of when deallocating
   session tokens in a multi-threaded environment?

Context: Need to understand mutex requirements.

Acceptable Answers:
- Mutex already protects deallocation (line [X])
- Need to add mutex at [scope]
- No mutex needed, tokens are thread-local
```

**Question 4: Historical Issues**
```
Q: Have there been any production incidents related to session token
   deallocation? What were the root causes?

Context: Want to avoid repeating historical mistakes.

Expected: Description of any incidents, workarounds in current code
```

### Expected Outcomes

**Outcome 1: In-Use Flag Exists** → Add invariant
```ada
-- Security Invariant 15: Session tokens MUST check in_use flag before deallocation
if Session_Token.In_Use then
    raise Session_Still_Active;
end if;
Secure_Free(Session_Token);
```

**Outcome 2: Mutex Protection Required** → Add mutex to utility
```ada
-- Security Invariant 16: Session token deallocation MUST be mutex-protected
Session_Mutex.Lock;
Secure_Free(Session_Token);
Session_Mutex.Unlock;
```

**Outcome 3: No Special Handling** → Document as verified assumption
```
-- Verified with @session_expert (2025-11-06): Session tokens can be
-- safely deallocated without in-use flag check. No known race conditions.
```

### Documentation Template

```markdown
## Consultation 1: @session_expert - Session Token Deallocation

**Date**: [YYYY-MM-DD]
**Duration**: [X] minutes
**Attendees**: @refactor_agent, @session_expert

### Question 1: In-Use Flag Requirement
**Answer**: [YES/NO/PARTIAL]
**Details**: [Expert explanation]
**Code References**: [Line numbers, flag names]

### Question 2: Session Lifecycle Dependencies
**Answer**: [BEFORE/DURING/NO DEPENDENCY]
**Details**: [Expert explanation]

### Question 3: Race Condition Scenarios
**Answer**: [MUTEX REQUIRED/ALREADY PROTECTED/NOT NEEDED]
**Details**: [Expert explanation]
**Code References**: [Mutex locations if applicable]

### Question 4: Historical Issues
**Incidents**: [YES/NO]
**Details**: [If YES, describe incidents]

### Action Items
- [ ] Add Security Invariant 15: [Description]
- [ ] Add Security Invariant 16: [Description]
- [ ] Update RDB-003 Hidden Properties section
- [ ] Create regression test: [Test name]

### Updated Security Invariants
[List any new invariants discovered]

### Code Changes Required
[List specific code changes needed based on findings]
```

---

## Consultation 2: @crypto_team_lead (1 hour)

### Context

**Files**:
- `/src/crypto/buffer_manager.adb:156` - Crypto buffer deallocation (HIGH)
- `/src/crypto/random_pool.adb:123` - Random pool deallocation (HIGH)

**Change**: Add memory zeroization for crypto buffers and random pool
**Risk**: Information leakage if buffers cached/reused between operations

**Background from RDB-003**:
- 5 crypto instances in Phase 1A (38% of total)
- Crypto subsystem is highest risk component
- Buffer pooling behavior unclear from code comments
- Random pool state persistence requirements unknown

### Questions to Ask

**Question 1: Buffer Caching Behavior**
```
Q: Does buffer_manager.adb:156 cache or reuse crypto buffers between
   operations (e.g., buffer pool pattern)?

Context: Need to understand if zeroization timing is critical.

Acceptable Answers:
- YES: Buffers are pooled/reused [describe pooling strategy]
- NO: Buffers are allocated/freed per operation
- PARTIAL: Pooling only for [specific buffer types]
```

**Question 2: Zeroization Timing**
```
Q: If buffers are cached, when MUST zeroization occur to prevent
   information leakage across operations?

Context: Understand if "zeroize-on-return" or "zeroize-on-reuse" is required.

Acceptable Answers:
- IMMEDIATE: Zeroize before returning to pool
- LAZY: Zeroize when retrieving from pool
- BOTH: Defense-in-depth approach
```

**Question 3: Random Pool State**
```
Q: Does random_pool.adb:123 require special deinitialization beyond
   memory zeroization (e.g., RNG state reset, entropy cleanup)?

Context: Ensure we don't break RNG security guarantees.

Acceptable Answers:
- YES: Must call [deinit function] before deallocation
- NO: Standard zeroization sufficient
- PARTIAL: Additional cleanup for [specific scenarios]
```

**Question 4: Compiler Optimization Concerns**
```
Q: Are there known issues with compiler optimizing away zeroization
   in the crypto subsystem?

Context: Need to understand if compiler barriers are already in place.

Acceptable Answers:
- YES: Already using [barrier technique] at [locations]
- NO: Compiler optimization not an issue (explain why)
- UNKNOWN: Should add barriers as safety measure
```

**Question 5: Crypto Context Lifecycle**
```
Q: For hash_context.adb:78 and cipher_context.adb:198, are there
   dependencies on context state during deallocation?

Context: Understand if contexts must be finalized before zeroization.

Acceptable Answers:
- Must call Finalize() before deallocation
- Safe to deallocate anytime
- Depends on [specific scenario]
```

### Expected Outcomes

**Outcome 1: Buffer Pooling Exists** → Add zeroize-on-return invariant
```ada
-- Security Invariant 17: Pooled crypto buffers MUST be zeroized before
-- returning to pool (prevent cross-operation leakage)
procedure Return_To_Pool(Buffer : in out Crypto_Buffer) is
begin
    Zeroize(Buffer.Data);  -- Must happen before pool insertion
    Buffer_Pool.Return(Buffer);
end Return_To_Pool;
```

**Outcome 2: RNG Deinitialization Required** → Add deinit call
```ada
-- Security Invariant 18: Random pool MUST be deinitialized before zeroization
procedure Free_Random_Pool(Pool : in out Random_Pool_Access) is
begin
    RNG_Deinit(Pool.State);  -- Required: clears entropy sources
    Secure_Free(Pool);
end Free_Random_Pool;
```

**Outcome 3: Compiler Barriers Needed** → Add to all crypto zeroization
```ada
-- Security Invariant 19: Crypto zeroization MUST use compiler barrier
procedure Secure_Zeroize(Data : in out Sensitive_Data) is
begin
    Ada.Strings.Fixed.Overwrite(Data, 1, (others => ASCII.NUL));
    Compiler_Barrier;  -- Prevents optimization removal
end Secure_Zeroize;
```

### Documentation Template

```markdown
## Consultation 2: @crypto_team_lead - Crypto Subsystem Deallocation

**Date**: [YYYY-MM-DD]
**Duration**: [X] minutes
**Attendees**: @refactor_agent, @crypto_team_lead

### Question 1: Buffer Caching Behavior
**Answer**: [YES/NO/PARTIAL]
**Pooling Strategy**: [If YES, describe]
**Code References**: [Pool implementation locations]

### Question 2: Zeroization Timing
**Answer**: [IMMEDIATE/LAZY/BOTH]
**Details**: [Rationale for timing choice]

### Question 3: Random Pool State
**Answer**: [YES/NO/PARTIAL]
**Deinit Required**: [If YES, function name and location]
**Details**: [Explanation of RNG cleanup requirements]

### Question 4: Compiler Optimization Concerns
**Answer**: [YES/NO/UNKNOWN]
**Current Barriers**: [If YES, list locations]
**Recommendation**: [Add barriers / existing sufficient / not needed]

### Question 5: Crypto Context Lifecycle
**Answer**: [MUST FINALIZE/SAFE ANYTIME/DEPENDS]
**Details**: [Explanation of context dependencies]

### Action Items
- [ ] Add Security Invariant 17: [Buffer pooling zeroization]
- [ ] Add Security Invariant 18: [RNG deinitialization]
- [ ] Add Security Invariant 19: [Compiler barriers]
- [ ] Update crypto subsystem documentation
- [ ] Create test: crypto_buffer_zeroization_test

### Updated Security Invariants
[List new invariants for crypto subsystem]

### Code Changes Required
[Specific changes for each affected crypto file]

### Additional Files Affected
[Any additional crypto files discovered during consultation]
```

---

## Consultation 3: @auth_architect (1 hour)

### Context

**Files**:
- `/src/auth/acl_manager.adb:89` - ACL deallocation (HIGH)
- `/src/auth/oauth_client.adb:234` - OAuth client secrets (HIGH)

**Change**: Add memory zeroization for ACLs and OAuth secrets
**Risk**: ACL reference counting issues, OAuth timing dependencies

**Background from RDB-003**:
- 5 auth instances in Phase 1A (38% of total)
- ACLs may have parent-child relationships
- OAuth token refresh timing unclear
- Reference counting behavior undocumented

### Questions to Ask

**Question 1: ACL Reference Counting**
```
Q: Do ACLs in acl_manager.adb:89 use reference counting or shared
   references? Must all references be released before deallocation?

Context: Need to understand if ACLs have complex lifecycle management.

Acceptable Answers:
- YES: Reference counted, must check ref_count = 0
- NO: No reference counting, safe to deallocate
- PARTIAL: Only for [specific ACL types]
```

**Question 2: ACL Parent-Child Relationships**
```
Q: Do ACLs have parent-child or hierarchical relationships that must
   be considered during deallocation?

Context: Prevent orphaned references or cascade issues.

Acceptable Answers:
- YES: Must deallocate children first [describe hierarchy]
- NO: ACLs are independent
- PARTIAL: Hierarchy only for [specific scenarios]
```

**Question 3: OAuth Token Refresh Timing**
```
Q: Does oauth_client.adb:234 have implicit dependencies on token
   validity during deallocation (e.g., token refresh in progress)?

Context: Understand if deallocation can cause authentication bypass.

Acceptable Answers:
- YES: Must check refresh_in_progress flag
- NO: Token validity irrelevant during deallocation
- PARTIAL: Only matters for [specific token types]
```

**Question 4: Mutex Protection Requirements**
```
Q: What mutex protection is required for auth subsystem deallocations
   in multi-threaded environments?

Context: Prevent race conditions in ACL checks and token validation.

Acceptable Answers:
- ACLs: [mutex required/already protected/not needed]
- OAuth: [mutex required/already protected/not needed]
- API Keys: [mutex required/already protected/not needed]
```

**Question 5: Token Invalidation Order**
```
Q: For token-based auth (OAuth, API keys, refresh tokens), what is
   the correct order of operations for deallocation?

Context: Ensure tokens are invalidated before zeroization.

Acceptable Answers:
- 1. Invalidate, 2. Zeroize, 3. Deallocate
- 1. Zeroize, 2. Invalidate, 3. Deallocate
- Order doesn't matter
```

### Expected Outcomes

**Outcome 1: ACL Reference Counting** → Add ref count check
```ada
-- Security Invariant 20: ACLs MUST have ref_count = 0 before deallocation
procedure Free_ACL(ACL : in out ACL_Access) is
begin
    if ACL.Ref_Count > 0 then
        raise ACL_Still_Referenced;
    end if;
    Secure_Free(ACL);
end Free_ACL;
```

**Outcome 2: OAuth Refresh In Progress** → Add flag check
```ada
-- Security Invariant 21: OAuth tokens MUST NOT be deallocated during refresh
procedure Free_OAuth_Token(Token : in out OAuth_Token_Access) is
begin
    if Token.Refresh_In_Progress then
        raise Token_Refresh_In_Progress;
    end if;
    Secure_Free(Token);
end Free_OAuth_Token;
```

**Outcome 3: Mutex Protection Required** → Add auth mutex
```ada
-- Security Invariant 22: Auth deallocation MUST be mutex-protected
Auth_Mutex.Lock;
Secure_Free(Auth_Object);
Auth_Mutex.Unlock;
```

### Documentation Template

```markdown
## Consultation 3: @auth_architect - Auth Subsystem Deallocation

**Date**: [YYYY-MM-DD]
**Duration**: [X] minutes
**Attendees**: @refactor_agent, @auth_architect

### Question 1: ACL Reference Counting
**Answer**: [YES/NO/PARTIAL]
**Ref Count Check**: [If YES, how to check]
**Code References**: [Ref count implementation]

### Question 2: ACL Parent-Child Relationships
**Answer**: [YES/NO/PARTIAL]
**Hierarchy**: [If YES, describe structure]
**Deallocation Order**: [Parent first / children first / no order]

### Question 3: OAuth Token Refresh Timing
**Answer**: [YES/NO/PARTIAL]
**Flag Name**: [If YES, flag name and location]
**Details**: [Explanation of refresh dependencies]

### Question 4: Mutex Protection Requirements
**ACLs**: [REQUIRED/ALREADY PROTECTED/NOT NEEDED]
**OAuth**: [REQUIRED/ALREADY PROTECTED/NOT NEEDED]
**API Keys**: [REQUIRED/ALREADY PROTECTED/NOT NEEDED]
**Code References**: [Existing mutex locations if applicable]

### Question 5: Token Invalidation Order
**Answer**: [INVALIDATE → ZEROIZE → DEALLOCATE / other]
**Rationale**: [Why this order is required]

### Action Items
- [ ] Add Security Invariant 20: [ACL ref counting]
- [ ] Add Security Invariant 21: [OAuth refresh check]
- [ ] Add Security Invariant 22: [Auth mutex protection]
- [ ] Update auth subsystem documentation
- [ ] Create test: auth_deallocation_race_condition_test

### Updated Security Invariants
[List new invariants for auth subsystem]

### Code Changes Required
[Specific changes for each affected auth file]

### Additional Files Affected
[Any additional auth files discovered during consultation]
```

---

## Post-Consultation Actions

### Step 1: Update RDB-003 (30 minutes)

**Update Security Invariants Section** (lines 236-258):
```markdown
### Security Invariants

**What MUST NOT Break**:

[... existing invariants 1-14 ...]

**From Domain Expert Consultations (2025-11-06)**:

**Session Management**:
- **Invariant 15**: [From @session_expert consultation]
- **Invariant 16**: [From @session_expert consultation]

**Cryptography**:
- **Invariant 17**: [From @crypto_team_lead consultation]
- **Invariant 18**: [From @crypto_team_lead consultation]
- **Invariant 19**: [From @crypto_team_lead consultation]

**Authentication & Authorization**:
- **Invariant 20**: [From @auth_architect consultation]
- **Invariant 21**: [From @auth_architect consultation]
- **Invariant 22**: [From @auth_architect consultation]
```

**Update Hidden Security Properties Section** (lines 259-286):
```markdown
### Hidden Security Properties

**✅ VERIFIED: Domain Expert Consultations Complete (2025-11-06)**

**Property 1: Session Token "In-Use" Flag** - ✅ VERIFIED
- **Consultation**: @session_expert (30 min)
- **Finding**: [Summary of answer]
- **New Invariant**: Invariant 15 added
- **Code Changes**: [Required changes if any]

**Property 2: Crypto Buffer Caching** - ✅ VERIFIED
- **Consultation**: @crypto_team_lead (1 hour)
- **Finding**: [Summary of answer]
- **New Invariant**: Invariant 17, 18, 19 added
- **Code Changes**: [Required changes if any]

**Property 3: OAuth Token Refresh Race** - ✅ VERIFIED
- **Consultation**: @auth_architect (1 hour)
- **Finding**: [Summary of answer]
- **New Invariant**: Invariant 20, 21, 22 added
- **Code Changes**: [Required changes if any]
```

### Step 2: Update Task 2332e7 Status (5 minutes)

**Mark task as completed**:
- Update status to "completed"
- Add final note with consultation summary
- Link to updated RDB-003 sections

### Step 3: Notify @security_verification (5 minutes)

**Post to message board**:
```markdown
## ✅ Domain Expert Consultations Complete - Task 2332e7

**Status**: COMPLETE (2.5 hours total)
**Consultations**: 3/3 complete
**New Invariants**: 8 added (Invariants 15-22)
**RDB-003**: Updated with findings

**Summary**:
- Session tokens: [1-line summary]
- Crypto buffers: [1-line summary]
- OAuth/ACL: [1-line summary]

**Next**: Ready for Task a4e8e7 (Checkpoint 2 review)

Full details in consultation notes: [link to documentation]
```

### Step 4: Prepare for Checkpoint 2 (10 minutes)

**Create handoff package for @security_verification**:
- [ ] Updated RDB-003 (with 8 new invariants)
- [ ] 3 consultation documentation files
- [ ] List of code changes required based on findings
- [ ] Regression test requirements

---

## Success Criteria

**Consultations are COMPLETE when**:
- ✅ All 3 consultations conducted (2.5h total)
- ✅ All 5 hidden properties clarified
- ✅ 3 documentation templates filled out
- ✅ RDB-003 updated with new invariants
- ✅ Code changes list created
- ✅ @security_verification notified
- ✅ Checkpoint 2 handoff package ready

**Quality Criteria**:
- Each consultation has clear YES/NO/PARTIAL answers (no ambiguity)
- New invariants are specific and testable
- Code changes are scoped and estimated
- No open questions remain

---

## Timeline

**Day 1 (Immediately)**:
- [ ] Schedule 3 consultations (30 min coordination)
- [ ] Send this guide to domain experts for review
- [ ] Block calendar time for consultations

**Day 2-3 (Consultations)**:
- [ ] Consultation 1: @session_expert (30 min)
- [ ] Consultation 2: @crypto_team_lead (1 hour)
- [ ] Consultation 3: @auth_architect (1 hour)
- [ ] Document findings as you go

**Day 4 (Documentation)**:
- [ ] Update RDB-003 with all findings
- [ ] Create code changes list
- [ ] Update Task 2332e7 status
- [ ] Notify @security_verification
- [ ] Prepare Checkpoint 2 handoff

**Total Timeline**: 3-4 days maximum

---

## Escalation Path

**If domain experts are unavailable**:
1. **Immediate**: Message @code_architect (me) - I can help prioritize or find alternatives
2. **Day 2**: Escalate to project lead - Week 1 blocker requires urgent attention
3. **Day 3**: Risk mitigation - Document assumptions, implement with extra safety checks

**If findings reveal major issues**:
1. **BLOCKING issues**: Stop immediately, escalate to @code_architect
2. **MODERATE issues**: Document as new risks, adjust Phase 1A scope if needed
3. **MINOR issues**: Add to Security Watch List, address during implementation

---

## Contact Information

**For questions about this guide**:
- @code_architect (created this guide, available for clarification)

**For scheduling assistance**:
- @code_architect (can help coordinate domain expert calendars)

**For technical questions during consultations**:
- Refer to RDB-003 sections (printed copies recommended)
- This guide (bring to consultations)

---

## Appendix: Pre-Consultation Checklist

**Before scheduling consultations, ensure**:
- [ ] Read RDB-003 sections 4.2 (Hidden Properties) and 4.1 (Security Invariants)
- [ ] Review affected code files (13 instances) to understand current implementation
- [ ] Print this guide for reference during consultations
- [ ] Prepare note-taking template (digital or paper)
- [ ] Block calendar time (2.5h + 1h buffer for documentation)
- [ ] Notify @security_verification that consultations are starting

---

**Guide Status**: ✅ READY FOR USE
**Created**: 2025-11-06
**Owner**: @refactor_agent
**Approver**: @code_architect
**BLOCKING**: Phase 1A implementation (Week 2)

---

*This guide will enable efficient, high-quality domain expert consultations that unblock Phase 1A implementation while ensuring all hidden security properties are properly documented and validated.*
