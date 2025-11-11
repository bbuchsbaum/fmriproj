# Proposal: Add Trial-wise Design Matrix Support to fmridesign

**Author**: fmriproj Contributors
**Date**: 2025-11-07
**Target Package**: fmridesign
**Type**: Enhancement
**Scope**: New feature (backward compatible)

---

## Executive Summary

This proposal suggests adding **trial-wise design matrix** generation to fmridesign as an optional expansion mode. Currently, fmridesign creates **condition-wise** design matrices (one column per condition × basis function), which is ideal for standard GLM analysis. However, trial-wise matrices (one column per individual trial × basis function) are essential for:

- **Beta-series** analysis
- **Trial-level connectivity** studies
- **Multi-voxel pattern analysis (MVPA)** with trial-wise projection
- **Single-trial estimation** for downstream classification

Adding `trialwise=TRUE` as an optional parameter would enable these use cases while maintaining full backward compatibility.

---

## Motivation

### Current State

fmridesign excels at creating condition-wise design matrices:

```r
# Current behavior
emod <- event_model(onset ~ hrf(condition), data = events, ...)
dm <- design_matrix(emod)

# If 3 conditions × 1 HRF basis = 3 columns
dim(dm)  # [time_points, 3]
```

**Works perfectly for:**
- Standard GLM analysis
- Condition-level contrasts
- Group-level inference

**Doesn't support:**
- Beta-series modeling (Rissman et al., 2004)
- Trial-wise MVPA (Norman et al., 2006)
- Single-trial estimation

### The Gap

Users working with MVPA, connectivity, or trial-level analyses currently must:
1. Use other packages (fmrireg, custom code)
2. Duplicate event model infrastructure
3. Lose fmridesign's validation and formula syntax

This proposal bridges that gap by adding trial-wise expansion as an **opt-in feature**.

---

## Proposed Solution

### API Design

Add a `trialwise` parameter to `event_model()` and related functions:

```r
# NEW: Trial-wise mode
emod <- event_model(
  onset ~ hrf(condition),
  data = events,
  sampling_frame = sframe,
  trialwise = TRUE  # <-- NEW PARAMETER
)

dm <- design_matrix(emod)

# If 6 trials × 1 HRF basis = 6 columns (not 3!)
dim(dm)  # [time_points, 6]
```

**Key Features:**
- **Backward compatible**: Default `trialwise = FALSE` preserves existing behavior
- **Consistent API**: Uses same formula syntax
- **Metadata preserved**: Trial labels, conditions, timing all tracked
- **Works with all HRF types**: SPMG1/2/3, FIR, B-spline, etc.

### Implementation Strategy

#### Option A: Expand at `convolve()` Stage (Recommended)

Modify `convolve.event_term()` to optionally create per-trial columns instead of per-condition:

```r
# R/event_model_helpers.R

convolve.event_term <- function(x, sampling_frame, precision = 0.3, trialwise = FALSE, ...) {
  if (trialwise) {
    # Create one column per trial × basis
    return(convolve_trialwise(x, sampling_frame, precision))
  } else {
    # Existing condition-wise behavior
    return(convolve_conditionwise(x, sampling_frame, precision))
  }
}

convolve_trialwise <- function(event_term, sampling_frame, precision) {
  # For each individual trial (row in event table):
  #   1. Create isolated event sequence
  #   2. Convolve with HRF bases
  #   3. Stack columns horizontally

  events <- events(event_term)
  n_trials <- nrow(events)
  hrf <- attr(event_term, "hrfspec")$hrf

  # Build trial-wise columns
  trial_cols <- list()
  for (i in seq_len(n_trials)) {
    trial_event <- events[i, , drop = FALSE]
    trial_col <- convolve_single_trial(trial_event, hrf, sampling_frame, precision)
    trial_cols[[i]] <- trial_col
  }

  # Combine into matrix
  do.call(cbind, trial_cols)
}
```

**Pros:**
- Clean separation of concerns
- Minimal changes to existing code
- Easy to test independently

**Cons:**
- Requires refactoring convolve() method

#### Option B: Post-process Condition-wise Matrix

Transform condition-wise design to trial-wise after construction:

```r
# R/event_model.R

event_model <- function(..., trialwise = FALSE) {
  # Existing construction
  emod <- build_event_model_conditionwise(...)

  if (trialwise) {
    # Transform to trial-wise
    emod <- expand_to_trialwise(emod)
  }

  return(emod)
}

expand_to_trialwise <- function(event_model) {
  # Map trials to conditions
  # Replicate appropriate condition columns for each trial
  # Update metadata
}
```

**Pros:**
- Less invasive to existing code
- Reuses existing infrastructure

**Cons:**
- Less efficient (builds then transforms)
- Harder to track provenance

**Recommendation**: Implement **Option A** for cleaner architecture.

---

## Detailed Implementation Plan

### Phase 1: Core Functionality (Week 1-2)

**Files to Modify:**
- `R/event_model.R` - Add `trialwise` parameter
- `R/event_model_helpers.R` - Implement `convolve_trialwise()`
- `R/event_vector.R` - Ensure `event` class supports trial indexing

**New Functions:**
```r
convolve_trialwise(event_term, sampling_frame, precision)
make_trial_columns(events, hrf, sampling_frame)
label_trial_columns(design_matrix, events, trialwise_mode)
```

**Metadata Additions:**
```r
attr(event_model, "trialwise") <- TRUE/FALSE
attr(design_matrix, "trial_labels") <- c("trial001", "trial002", ...)
attr(design_matrix, "trial_to_condition") <- mapping vector
```

### Phase 2: Integration & Validation (Week 3)

**Column Naming Convention:**
```r
# Condition-wise (current):
colnames(dm) <- c("cond.A_b01", "cond.B_b01", "cond.C_b01")

# Trial-wise (proposed):
colnames(dm) <- c("trial001_b01", "trial002_b01", "trial003_b01", ...)
# With metadata linking trial001 → cond.A, trial002 → cond.B, etc.
```

**Tests:**
- `test_trialwise_basic.R` - Basic functionality
- `test_trialwise_multibasis.R` - SPMG3, FIR, B-spline
- `test_trialwise_multirun.R` - Block-wise handling
- `test_trialwise_metadata.R` - Metadata preservation
- `test_trialwise_contrasts.R` - Contrast compatibility

### Phase 3: Documentation & Examples (Week 4)

**Vignette:** `vignettes/trialwise_modeling.Rmd`

```r
---
title: "Trial-wise Modeling and Beta-series Analysis"
---

## Introduction

Trial-wise design matrices enable...

## Basic Usage

library(fmridesign)

# Standard condition-wise (default)
emod_cond <- event_model(onset ~ hrf(stim), ...)
dim(design_matrix(emod_cond))  # [time, n_conditions]

# Trial-wise mode
emod_trial <- event_model(onset ~ hrf(stim), ..., trialwise = TRUE)
dim(design_matrix(emod_trial))  # [time, n_trials]

## Beta-series Analysis

# Extract trial betas
beta_trials <- solve_glm(design_matrix(emod_trial), Y)

# Compute trial-trial correlations
cor_matrix <- cor(t(beta_trials))

## Integration with MVPA

# Use with classification pipelines...
```

**README Addition:**
```markdown
## Trial-wise Modeling (New in v0.x.0)

fmridesign now supports trial-wise design matrices for beta-series
and MVPA analyses:

event_model(..., trialwise = TRUE)

See `vignette("trialwise_modeling")` for details.
```

---

## API Examples

### Example 1: Basic Beta-series

```r
library(fmridesign)

# Event data
events <- data.frame(
  onset = c(0, 10, 20, 30, 40, 50),
  condition = factor(c("A", "B", "A", "B", "A", "B"))
)

sframe <- sampling_frame(blocklens = 100, TR = 2)

# Trial-wise design
emod <- event_model(
  onset ~ hrf(condition),
  data = events,
  sampling_frame = sframe,
  trialwise = TRUE
)

dm <- design_matrix(emod)
dim(dm)  # [50, 6] - 6 trials × 1 basis

# Get trial-to-condition mapping
attr(dm, "trial_to_condition")
# [1] "A" "B" "A" "B" "A" "B"
```

### Example 2: Multi-basis HRF

```r
# SPMG3 (3 bases per trial)
emod_spmg3 <- event_model(
  onset ~ hrf(condition, basis = "spmg3"),
  data = events,
  sampling_frame = sframe,
  trialwise = TRUE
)

dm3 <- design_matrix(emod_spmg3)
dim(dm3)  # [50, 18] - 6 trials × 3 bases

colnames(dm3)
# "trial001_b01" "trial001_b02" "trial001_b03"
# "trial002_b01" "trial002_b02" "trial002_b03"
# ...
```

### Example 3: Factorial Design

```r
# 2×2 factorial
events_factorial <- data.frame(
  onset = seq(0, 90, by = 10),
  stim = factor(c("face", "scene")),
  task = factor(c("remember", "forget"))
)

emod_factorial <- event_model(
  onset ~ hrf(stim, task),  # Interaction
  data = events_factorial,
  sampling_frame = sframe,
  trialwise = TRUE
)

dm_fact <- design_matrix(emod_factorial)
# Creates one column per trial (not per cell)
```

### Example 4: Parametric Modulation

```r
events_param <- data.frame(
  onset = c(0, 10, 20, 30),
  stim = c("A", "B", "A", "B"),
  RT = c(0.5, 0.6, 0.4, 0.7)
)

emod_param <- event_model(
  onset ~ hrf(stim, RT),
  data = events_param,
  sampling_frame = sframe,
  trialwise = TRUE
)

# Each trial gets columns for: intercept + RT modulation
```

---

## Backward Compatibility

### Guarantees

✅ **Default behavior unchanged**: `trialwise = FALSE` by default
✅ **Existing tests pass**: No modifications to condition-wise path
✅ **API unchanged**: Only adds optional parameter
✅ **Metadata compatible**: Existing attributes preserved

### Migration Path

For users who want to switch:

```r
# Old (still works):
emod <- event_model(onset ~ hrf(cond), ...)

# New (opt-in):
emod <- event_model(onset ~ hrf(cond), ..., trialwise = TRUE)
```

No breaking changes required.

---

## Benefits to fmridesign Ecosystem

### 1. Completes the Feature Set

fmridesign becomes a **comprehensive** design matrix toolkit covering:
- ✅ Condition-wise (GLM, contrasts)
- ✅ Trial-wise (beta-series, MVPA)  ← **NEW**
- ✅ Baseline modeling (drift, nuisance)

### 2. Attracts New Users

Researchers in:
- Connectivity analysis
- MVPA/decoding
- Representational similarity analysis (RSA)
- Single-trial modeling

...will adopt fmridesign as their design specification tool.

### 3. Reduces Ecosystem Fragmentation

Currently, users must switch between:
- fmridesign (condition-wise)
- fmrireg (some trial support)
- Custom code (most trial-wise work)

**After this enhancement:** fmridesign handles both modes.

### 4. Enables Integration

Packages like **fmriproj** can fully integrate with fmridesign instead of duplicating infrastructure. This creates a unified ecosystem:

```
fmridesign (design) → fmriproj (projection) → rMVPA (analysis)
```

---

## Testing Strategy

### Unit Tests

```r
# tests/testthat/test_trialwise.R

test_that("trialwise produces correct number of columns", {
  events <- data.frame(onset = c(0, 10, 20), cond = c("A","B","A"))
  emod <- event_model(onset ~ hrf(cond), ..., trialwise = TRUE)
  dm <- design_matrix(emod)

  expect_equal(ncol(dm), 3)  # 3 trials × 1 basis
})

test_that("trialwise metadata is correct", {
  emod <- event_model(..., trialwise = TRUE)
  expect_true(attr(emod, "trialwise"))
  expect_equal(length(attr(dm, "trial_labels")), n_trials)
})

test_that("trialwise works with multi-basis HRFs", {
  emod <- event_model(onset ~ hrf(c, basis="spmg3"), ..., trialwise = TRUE)
  dm <- design_matrix(emod)
  expect_equal(ncol(dm), n_trials * 3)
})
```

### Integration Tests

```r
test_that("trialwise integrates with contrasts", {
  # Can still define contrasts on trial-to-condition mapping
})

test_that("trialwise works with baseline_model", {
  # Combining trial-wise events + baseline drift
})
```

### Regression Tests

```r
test_that("trialwise FALSE produces identical output to current", {
  # Byte-for-byte comparison with current behavior
})
```

---

## Performance Considerations

### Memory

Trial-wise matrices are **larger** than condition-wise:
- Condition-wise: `n_time × (n_conditions × n_bases)`
- Trial-wise: `n_time × (n_trials × n_bases)`

**Mitigation:**
- Use sparse matrices by default (`Matrix::dgCMatrix`)
- Document memory usage clearly
- Provide `sparse = TRUE` option

### Computation

Trial-wise convolution is slower (more columns to compute).

**Mitigation:**
- Parallelize trial convolution if requested
- Reuse HRF evaluation across trials
- Profile and optimize critical paths

---

## Open Questions for Maintainer

1. **Preferred implementation**: Option A (convolve-stage) or Option B (post-process)?
2. **Column naming**: `trial001_b01` or `trial_1_basis_1` or other?
3. **Metadata storage**: Use attributes or formal S3 class slots?
4. **Contrast compatibility**: Should contrasts work on trial-to-condition mappings?
5. **Release timeline**: Target version for inclusion?

---

## Proposed Contribution Timeline

**Week 1-2**: Core implementation (Option A)
**Week 3**: Testing and validation
**Week 4**: Documentation and vignettes
**Week 5**: PR submission and code review
**Week 6-8**: Iterate based on feedback

**Estimated Effort**: 40-60 hours (can be done by fmriproj contributors)

---

## References

- Rissman, J., Gazzaley, A., & D'Esposito, M. (2004). *Measuring functional connectivity during distinct stages of a cognitive task.* NeuroImage.
- Norman, K. A., Polyn, S. M., Detre, G. J., & Haxby, J. V. (2006). *Beyond mind-reading: multi-voxel pattern analysis of fMRI data.* Trends in Cognitive Sciences.
- Mumford, J. A., Turner, B. O., Ashby, F. G., & Poldrack, R. A. (2012). *Deconvolving BOLD activation in event-related designs for multivoxel pattern classification analyses.* NeuroImage.

---

## Contact

For questions or discussion:
- **GitHub Issue**: [Create issue in fmridesign repo]
- **Email**: [Maintainer contact]
- **PoC Implementation**: Available in fmriproj as reference

---

## Appendix: Reference Implementation

A working reference implementation is available in the fmriproj package:
- `R/fmridesign_adapter.R` - Adapter showing trial-wise conversion
- `tests/testthat/test-phase1-integration.R` - Test examples

This can serve as a starting point or reference for the fmridesign implementation.

---

**Status**: Proposal Draft
**Next Steps**: Submit to fmridesign maintainer for feedback
