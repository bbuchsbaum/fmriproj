# fmriproj ↔ fmridesign Integration Roadmap

**Created**: 2025-11-07
**Status**: In Progress
**Goal**: Eliminate duplication and leverage fmridesign infrastructure

---

## Overview

This document tracks the integration of fmriproj with fmridesign to eliminate ~40-50% code duplication while maintaining backward compatibility and preparing for future enhancements.

### Key Metrics
- **Code to Replace**: ~230 lines
- **Expected Code Reduction**: 40% by Phase 3
- **Timeline**: 8-12 weeks
- **Risk Level**: Medium (mitigated by phased approach)

---

## Phase 1: Foundation (Weeks 1-2) ✓ IN PROGRESS

**Goal**: Replace internal components with fmridesign equivalents while keeping API unchanged

### Tasks

#### 1.1 Dependencies ⏳
- [ ] Add `fmridesign` to DESCRIPTION Imports
- [ ] Add `fmrihrf` to DESCRIPTION Imports
- [ ] Update NAMESPACE for new imports
- [ ] Test package loads correctly

**Files**: `DESCRIPTION`, `NAMESPACE`

#### 1.2 HRF Replacement ⏳
- [ ] Replace `hrf_basis_spmg3_theta.R` with `fmrihrf::HRF_SPMG3`
- [ ] Update `build_design_matrix()` to accept `fmrihrf::HRF` objects
- [ ] Update all user-facing functions to use fmrihrf HRFs
- [ ] Add support for additional HRF types (Gamma, Gaussian, etc.)
- [ ] Deprecate custom HRF functions with warnings

**Files Modified**:
- `R/build_projector.R`
- `R/user_friendly_wrappers.R`
- `R/trialwise_design.R` (lines 155-204)

**Expected Reduction**: ~50 lines

#### 1.3 Onset Parsing Refactor ⏳
- [ ] Extract onset parsing to `R/onset_helpers.R`
- [ ] Create `parse_onsets_with_fmridesign()` internal function
- [ ] Use `fmridesign:::event()` for validation
- [ ] Maintain existing `onset_units` and `rounding` behavior
- [ ] Add comprehensive tests comparing old vs new

**Files Modified**:
- `R/trialwise_design.R` (lines 59-141)
- New: `R/onset_helpers.R`

**Expected Reduction**: ~80 lines

#### 1.4 Testing ⏳
- [ ] Create `tests/testthat/test-fmridesign-integration.R`
- [ ] Byte-for-byte comparison of design matrices (old vs new)
- [ ] Benchmark performance (old vs new)
- [ ] Test all onset_units modes (sec, index, auto)
- [ ] Test all rounding policies (nearest, floor, ceil)
- [ ] Test multi-run scenarios

**New Files**:
- `tests/testthat/test-fmridesign-integration.R`
- `tests/testthat/test-hrf-replacement.R`

### Phase 1 Deliverables
- ✅ fmridesign dependency added
- ✅ HRF handling modernized
- ✅ Onset parsing simplified
- ✅ All tests passing
- ✅ ~15% code reduction achieved

**Success Criteria**: All existing tests pass, new tests validate equivalence

---

## Phase 2: Adapter Layer (Weeks 3-5)

**Goal**: Create clean integration layer enabling optional formula API

### Tasks

#### 2.1 Adapter Design ⏳
- [ ] Design `fmridesign_adapter` S3 class
- [ ] Define conversion interface
- [ ] Document adapter assumptions and limitations

**New Files**: `R/fmridesign_adapter.R`

#### 2.2 Core Adapter Implementation ⏳
- [ ] Implement `as_trialwise_design()` converter
- [ ] Implement `as_fmridesign_event_model()` (reverse direction)
- [ ] Handle trial-wise vs condition-wise column expansion
- [ ] Preserve metadata (HRF info, diagnostics, etc.)
- [ ] Support sparse matrix output

**Function Signatures**:
```r
as_trialwise_design(event_model, sparse = TRUE)
as_fmridesign_event_model(event_model_list, sampling_frame)
```

#### 2.3 Formula API (Optional) ⏳
- [ ] Create `project_trials_formula()` wrapper
- [ ] Add vignette: "Formula-based design specification"
- [ ] Maintain backward compatibility with list-based API
- [ ] Add examples to documentation

**New Files**:
- `R/formula_api.R`
- `vignettes/formula-based-designs.Rmd`

#### 2.4 Integration Testing ⏳
- [ ] Test adapter with all fmridesign event types
- [ ] Test formula API end-to-end
- [ ] Validate against rMVPA integration
- [ ] Performance benchmarks
- [ ] Memory profiling

**New Files**:
- `tests/testthat/test-adapter.R`
- `tests/testthat/test-formula-api.R`

### Phase 2 Deliverables
- ✅ Trial-wise adapter functional
- ✅ Formula API available (opt-in)
- ✅ Documentation updated
- ✅ ~30% code reduction achieved

**Success Criteria**: Users can optionally use fmridesign directly, all existing code still works

---

## Phase 3: Upstream Contribution (Weeks 6-12)

**Goal**: Contribute trial-wise support to fmridesign and complete integration

### Tasks

#### 3.1 fmridesign Enhancement Proposal ⏳
- [ ] Draft RFC for `trialwise=TRUE` parameter
- [ ] Create proof-of-concept implementation
- [ ] Write comprehensive test suite
- [ ] Submit issue to fmridesign GitHub
- [ ] Iterate based on maintainer feedback

**Deliverables**:
- `docs/fmridesign_trialwise_proposal.md`
- Proof-of-concept code
- Test suite

#### 3.2 Collaboration with fmridesign ⏳
- [ ] Submit PR to fmridesign
- [ ] Address code review feedback
- [ ] Ensure tests pass in fmridesign CI
- [ ] Update fmridesign documentation
- [ ] Wait for release

**Timeline**: 4-6 weeks (dependent on maintainer availability)

#### 3.3 Full Integration ⏳
- [ ] Update fmriproj to require new fmridesign version
- [ ] Remove adapter layer (if no longer needed)
- [ ] Remove duplicated code from `trialwise_design.R`
- [ ] Migrate all internal code to use fmridesign directly
- [ ] Update all vignettes and examples

**Files to Remove/Deprecate**:
- Most of `R/trialwise_design.R` (keep only projection-specific code)
- `R/fmridesign_adapter.R` (potentially)
- `R/onset_helpers.R` (if fully replaced)

#### 3.4 Migration & Deprecation ⏳
- [ ] Create migration guide
- [ ] Add deprecation warnings to old functions
- [ ] 6-month transition period
- [ ] Remove deprecated code in next major version

**New Files**:
- `vignettes/migration-guide.Rmd`
- `NEWS.md` entries

### Phase 3 Deliverables
- ✅ Trial-wise support in fmridesign
- ✅ fmriproj fully integrated
- ✅ ~40% code reduction achieved
- ✅ Ecosystem-wide benefits

**Success Criteria**: fmriproj is a thin wrapper around fmridesign + projection logic

---

## Code Mapping

### Files to Modify

| File | Current Lines | After Phase 1 | After Phase 2 | After Phase 3 | Action |
|------|--------------|---------------|---------------|---------------|--------|
| `R/trialwise_design.R` | 236 | ~200 | ~180 | ~100 | Major refactor |
| `R/hrf_basis_spmg3_theta.R` | 50 | 0 | 0 | 0 | Remove |
| `R/build_projector.R` | 150 | ~145 | ~140 | ~130 | Minor refactor |
| `R/user_friendly_wrappers.R` | 200 | ~195 | ~190 | ~180 | Update APIs |
| **New: `R/onset_helpers.R`** | 0 | 50 | 50 | 0 | Temporary |
| **New: `R/fmridesign_adapter.R`** | 0 | 0 | 100 | 0 | Temporary |
| **New: `R/formula_api.R`** | 0 | 0 | 80 | 80 | Keep |

### Test Coverage

| Test File | Phase 1 | Phase 2 | Phase 3 |
|-----------|---------|---------|---------|
| `test-fmridesign-integration.R` | ✓ | ✓ | ✓ |
| `test-hrf-replacement.R` | ✓ | ✓ | ✓ |
| `test-adapter.R` | - | ✓ | ✓ |
| `test-formula-api.R` | - | ✓ | ✓ |
| `test-migration.R` | - | - | ✓ |

---

## Risk Management

### Identified Risks

#### Risk 1: Breaking Changes to User Code
**Probability**: Medium
**Impact**: High
**Mitigation**:
- Maintain 100% backward compatibility in Phases 1-2
- 6-month deprecation period
- Comprehensive migration guide
- Version detection and warnings

#### Risk 2: Performance Regression
**Probability**: Low
**Impact**: Medium
**Mitigation**:
- Benchmark at each phase
- Profile before/after
- fmridesign likely faster (sparse matrix optimizations)

#### Risk 3: fmridesign PR Not Accepted
**Probability**: Medium
**Impact**: Medium
**Mitigation**:
- Maintain adapter as permanent solution
- Still get 30% code reduction
- Collaborate early with maintainer

#### Risk 4: Increased Dependencies
**Probability**: Certain
**Impact**: Low
**Mitigation**:
- fmridesign is stable and maintained
- Same author as fmrihrf
- Reduces overall maintenance burden

---

## Success Metrics

### Quantitative

| Metric | Baseline | Phase 1 Target | Phase 2 Target | Phase 3 Target |
|--------|----------|----------------|----------------|----------------|
| Lines of Code (design logic) | 470 | 400 | 350 | 280 |
| Test Coverage (design code) | 75% | 85% | 90% | 95% |
| Number of HRF types | 1 | 7+ | 7+ | 7+ |
| Design validation errors caught | 5 | 15+ | 20+ | 25+ |

### Qualitative

- [ ] Code is more maintainable
- [ ] Error messages are clearer
- [ ] Documentation is comprehensive
- [ ] Community benefits from upstream contribution
- [ ] Integration with rMVPA is seamless

---

## Timeline

```
Week 1-2:   Phase 1 - Foundation
Week 3-5:   Phase 2 - Adapter Layer
Week 6-8:   Phase 3 - Proposal & PR
Week 9-12:  Phase 3 - Integration & Migration
```

**Milestone Dates**:
- Phase 1 Complete: Week 2
- Phase 2 Complete: Week 5
- fmridesign PR Submitted: Week 8
- Phase 3 Complete: Week 12

---

## Communication Plan

### Internal (fmriproj)
- Update NEWS.md at each phase
- Tag releases: v0.x.0-phase1, v0.x.0-phase2, etc.
- Update README with integration status

### External (fmridesign)
- Open issue before starting work
- Share proof-of-concept early
- Regular updates on PR
- Acknowledge in commit messages and release notes

---

## Rollback Plan

If integration causes unforeseen issues:

### Phase 1 Rollback
- Revert DESCRIPTION changes
- Restore original HRF functions
- Keep tests for future attempts

### Phase 2 Rollback
- Remove adapter code
- Keep Phase 1 improvements
- Document lessons learned

### Phase 3 Rollback
- If PR rejected: Keep adapter as permanent solution
- If bugs found: Revert to Phase 2 state while fixing

---

## Next Actions (Immediate)

1. ✅ Create this roadmap document
2. ⏳ Add dependencies to DESCRIPTION
3. ⏳ Read current `trialwise_design.R` thoroughly
4. ⏳ Create test baseline for comparison
5. ⏳ Begin HRF replacement

---

## Notes & Decisions

### Decision Log

**2025-11-07**: Decided to proceed with full Phase 3 integration
- Rationale: 40% code reduction + ecosystem benefits worth the effort
- Risk: Mitigated by phased approach and backward compatibility

### Open Questions

1. Should we deprecate list-based API entirely, or keep it alongside formula API?
   - **Tentative**: Keep both, formula as opt-in

2. How to handle fmriproj-specific features (theta-HRF, adaptive collapse)?
   - **Tentative**: Register as extension in fmridesign

3. Timeline for removing deprecated code?
   - **Tentative**: Major version bump (v2.0.0) after 6 months

---

## References

- Integration Analysis: `architecture_notes.md`
- fmridesign Documentation: `~/code/fmridesign/README.md`
- fmrihrf Package: https://CRAN.R-project.org/package=fmrihrf
- fmridesign Repository: (GitHub URL if available)

---

**Last Updated**: 2025-11-07
**Status**: Phase 1 In Progress
