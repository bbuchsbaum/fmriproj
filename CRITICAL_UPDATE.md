# CRITICAL UPDATE: fmridesign Already Has Trial-wise Support

**Date**: 2025-11-07
**Discovery**: fmridesign's `trialwise()` function provides all needed trial-wise functionality

---

## What We Discovered

After your feedback, I examined `/Users/bbuchsbaum/code/fmridesign` and found that **fmridesign already has comprehensive trial-wise support** through the `trialwise()` function.

### The Existing Functionality

**Location**: `fmridesign/R/hrf-formula.R:387-405`

**Usage**:
```r
library(fmridesign)

# Create trial-wise design matrix
emodel <- event_model(
  onset ~ trialwise(basis = "spmg1", add_sum = TRUE),
  data = events_df,
  block = ~ run,
  sampling_frame = sframe
)
```

**Features**:
- ✅ One regressor per trial (exactly what fmriproj needs)
- ✅ All HRF basis types (spmg1/2/3, gamma, gaussian, fir, bspline)
- ✅ Optional grand mean column via `add_sum = TRUE`
- ✅ Multi-run support
- ✅ Zero-padded trial labels

---

## Impact on Integration Work

### Phase 1 ✅ (Complete - No Changes Needed)

**Status**: Still valid and useful

**Files**:
- `R/onset_helpers.R` - Improved validation
- `R/hrf_adapter.R` - Bridges fmrihrf
- `tests/testthat/test-phase1-integration.R` - Tests

**Action**: None required

### Phase 2 ⚠️ (Complete but Over-Engineered)

**Status**: Works but can be massively simplified

**Problem**: `R/fmridesign_adapter.R` (420 lines) manually expands condition-wise to trial-wise when we could just use `trialwise()` directly

**Simplification**:
```r
# Current: 420 lines of manual expansion logic
as_trialwise_design(event_model, ...)

# Simplified: ~40 lines wrapping trialwise()
as_trialwise_design_simple <- function(data, sampling_frame, basis = "spmg1", ...) {
  emodel <- event_model(onset ~ trialwise(basis = basis),
                        data = data,
                        sampling_frame = sampling_frame, ...)
  list(X = as.matrix(design_matrix(emodel)))
}
```

**Potential Savings**: ~400 lines of code

**Action**: Consider simplification in future release

### Phase 3 ❌ (Proposal is Obsolete)

**Status**: Completely unnecessary - feature already exists!

**Problem**: `docs/fmridesign_trialwise_proposal.md` (680 lines) proposes adding `trialwise=TRUE` parameter to fmridesign, but the functionality already exists via `trialwise()` function

**What to Do**:
- Archive the proposal document
- Do NOT submit to fmridesign maintainer
- Update documentation to show how to use existing `trialwise()`

**Action**: Withdraw proposal, update docs

---

## What This Means

### The Good News 🎉

1. **No upstream work needed** - fmridesign is already complete for our needs
2. **Simpler integration** - Just use existing `trialwise()` instead of manual expansion
3. **Better API design** - Formula syntax is more flexible than parameter flag
4. **Proven implementation** - Already tested and working in fmridesign

### The Work Ahead

1. **Verify compatibility** - Test that `trialwise()` works with fmriproj projectors
2. **Simplify adapter** - Reduce `R/fmridesign_adapter.R` from 420 to ~40 lines
3. **Update documentation** - Show users how to use existing `trialwise()`
4. **Create examples** - Demonstrate fmridesign + fmriproj workflows

**Estimated effort**: 10-15 hours (vs. 60-80 hours for original Phase 3)

---

## Example: Using trialwise() with fmriproj

```r
library(fmridesign)
library(fmrihrf)
library(fmriproj)

# 1. Prepare event data
events <- data.frame(
  onset = c(0, 10, 20, 30, 40),
  condition = c("A", "B", "A", "B", "A"),
  RT = c(0.5, 0.6, 0.4, 0.7, 0.5)
)

# 2. Create sampling frame
sframe <- sampling_frame(blocklens = 100, TR = 2)

# 3. Generate trial-wise design matrix with fmridesign
emodel <- event_model(
  onset ~ trialwise(basis = "spmg3", add_sum = TRUE),
  data = events,
  block = ~ 1,
  sampling_frame = sframe
)

# 4. Extract design matrix
X <- as.matrix(design_matrix(emodel))
# dim(X): [100 time points, 16 columns]
# Columns: 5 trials × 3 bases + 1 mean = 16 columns

# 5. Use with fmriproj projection pipeline
Y <- matrix(rnorm(100 * 1000), nrow = 100, ncol = 1000)  # BOLD data

projector <- build_projector(X, lambda_method = "EB")
trial_patterns <- project(projector, Y)

# dim(trial_patterns): [15, 1000] = (5 trials × 3 bases) × n_voxels
# (Grand mean column excluded from projection)

# 6. Use with rMVPA
spec <- projection_spec(
  event_model = list(onsets = events$onset, n_time = 100),
  projector = projector
)

# Or use formula API (from Phase 2)
spec_formula <- projection_spec_formula(
  formula = onset ~ trialwise(basis = "spmg3"),
  data = events,
  sampling_frame = sframe,
  lambda_method = "EB"
)
```

---

## Files Created/Modified Summary

### Kept (Phase 1 - Still Useful)
- ✅ `R/onset_helpers.R` (200 lines)
- ✅ `R/hrf_adapter.R` (280 lines)
- ✅ `tests/testthat/test-phase1-integration.R` (180 lines)

### To Revise (Phase 2 - Can Simplify)
- ⚠️ `R/fmridesign_adapter.R` (420 lines → can reduce to ~40)
- ⚠️ `R/formula_api.R` (280 lines → update to use `trialwise()`)
- ⚠️ `docs/MIGRATION_GUIDE.md` (580 lines → update examples)

### To Archive (Phase 3 - Obsolete)
- ❌ `docs/fmridesign_trialwise_proposal.md` (680 lines - feature exists)
- ❌ `PHASE_1_2_3_SUMMARY.md` (820 lines - based on wrong assumptions)
- ❌ Parts of `INTEGRATION_ROADMAP.md` (Phase 3 sections)

### New Files Created
- 📝 `CRITICAL_UPDATE.md` (this file)
- 📝 `INTEGRATION_REVISED.md` (revised strategy)
- 📝 `tests/testthat/test-trialwise-compatibility.R` (compatibility tests)

---

## Recommended Next Steps

### Immediate (Hours)
1. ✅ **Document discovery** - Created this summary
2. 🔄 **Run compatibility tests** - Execute `test-trialwise-compatibility.R`
3. 🔄 **Verify it works** - Test with real fmriproj pipeline

### Short-term (Days)
4. Simplify `R/fmridesign_adapter.R` to use `trialwise()`
5. Update documentation to show `trialwise()` usage
6. Create working examples
7. Archive obsolete Phase 3 proposal

### Medium-term (Weeks)
8. Release fmriproj v0.2.1 with simplified integration
9. Update vignettes with `trialwise()` examples
10. Monitor user feedback

---

## Lessons Learned

### What Went Wrong
❌ **Didn't check upstream thoroughly** - Should have examined fmridesign capabilities first
❌ **Assumed functionality gap** - Assumed trial-wise support was missing
❌ **Over-engineered solution** - Built 420-line adapter for simple use case
❌ **Premature documentation** - Wrote 680-line proposal for existing feature

### What Went Right
✅ **User caught it early** - You pointed out `trialwise()` before PR submission
✅ **Modular design** - Can easily revise Phase 2 without breaking Phase 1
✅ **Good testing** - Test infrastructure will catch any issues
✅ **Transparent process** - Documented everything, easy to revise

### Key Takeaway
**Always thoroughly investigate upstream capabilities before designing solutions.**

---

## Bottom Line

**Original Plan**:
- Phase 1: ✅ Adapt infrastructure
- Phase 2: ✅ Build trial-wise converter
- Phase 3: ❌ Contribute `trialwise=TRUE` to fmridesign

**Revised Plan**:
- Phase 1: ✅ Keep as-is (useful)
- Phase 2: ⚠️ Simplify to use `trialwise()` (400 lines saved)
- Phase 3: ❌ Withdraw proposal (feature exists!)

**Net Result**: Simpler, cleaner, faster integration with less code to maintain.

---

**Status**: 🔄 **STRATEGY REVISED - READY TO TEST**

**Next Action**: Run `test-trialwise-compatibility.R` to verify compatibility
