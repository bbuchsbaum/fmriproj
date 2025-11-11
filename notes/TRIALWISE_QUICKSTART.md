# Quick Start: Using fmridesign trialwise() with fmriproj

**Updated**: 2025-11-07
**For**: fmriproj users who want trial-wise (beta-series) analysis

---

## TL;DR

fmridesign already has `trialwise()` function - use it instead of manual trial-wise construction.

**Old way** (fmriproj only):
```r
# Manual setup - 20+ lines
event_model <- list(
  onsets = c(0, 10, 20, 30),
  n_time = 50,
  conditions = factor(1:4)  # Each trial = separate condition
)
design <- build_design_matrix(event_model, ...)
```

**New way** (fmridesign + fmriproj):
```r
# Clean, declarative - 5 lines
emodel <- event_model(
  onset ~ trialwise(basis = "spmg3"),
  data = events_df,
  sampling_frame = sframe
)
X <- as.matrix(design_matrix(emodel))
```

---

## Basic Example

```r
library(fmridesign)
library(fmrihrf)
library(fmriproj)

# 1. Event data (one row per trial)
events <- data.frame(
  onset = c(0, 10, 20, 30),          # Event times in seconds
  condition = c("A", "B", "A", "B")   # Optional metadata
)

# 2. Define sampling frame
sframe <- sampling_frame(
  blocklens = 50,  # 50 TRs
  TR = 2           # 2-second TR
)

# 3. Create trial-wise design
emodel <- event_model(
  onset ~ trialwise(basis = "spmg1"),
  data = events,
  block = ~ 1,
  sampling_frame = sframe
)

# 4. Extract design matrix
X <- as.matrix(design_matrix(emodel))
# dim(X): [50 time points, 4 trials]

# 5. Project BOLD data
Y <- read_bold_data(...)  # Your BOLD data [n_time, n_voxels]

projector <- build_projector(X, lambda_method = "EB")
patterns <- project(projector, Y)
# dim(patterns): [4 trials, n_voxels]
```

---

## Common Use Cases

### 1. Standard Beta-Series (SPMG1)

```r
# Single canonical HRF per trial
emodel <- event_model(
  onset ~ trialwise(basis = "spmg1"),
  data = events,
  block = ~ run,
  sampling_frame = sframe
)
```

**Output**: One column per trial

### 2. Flexible HRF (SPMG3)

```r
# Canonical + temporal + dispersion derivatives
emodel <- event_model(
  onset ~ trialwise(basis = "spmg3"),
  data = events,
  block = ~ run,
  sampling_frame = sframe
)
```

**Output**: 3 columns per trial (canonical + 2 derivatives)

### 3. With Grand Mean

```r
# Add average across trials as separate regressor
emodel <- event_model(
  onset ~ trialwise(basis = "spmg1", add_sum = TRUE),
  data = events,
  block = ~ run,
  sampling_frame = sframe
)
```

**Output**: n_trials + 1 columns (trials + mean)

### 4. FIR Basis (Data-Driven)

```r
# Estimate HRF shape from data with 8 FIR bases
emodel <- event_model(
  onset ~ trialwise(basis = "fir", nbasis = 8),
  data = events,
  block = ~ run,
  sampling_frame = sframe
)
```

**Output**: 8 columns per trial

### 5. Multi-Run Design

```r
events <- data.frame(
  onset = c(0, 10, 20,   # Run 1
            0, 10, 20),  # Run 2
  run = c(1, 1, 1, 2, 2, 2)
)

sframe <- sampling_frame(
  blocklens = c(30, 30),  # Two 30-TR runs
  TR = 2
)

emodel <- event_model(
  onset ~ trialwise(basis = "spmg2"),
  data = events,
  block = ~ run,
  sampling_frame = sframe
)

X <- as.matrix(design_matrix(emodel))
# dim(X): [60 time points, 12 columns] = 6 trials × 2 bases
```

---

## Advanced Examples

### Combining Trial-wise with Covariates

```r
# Trial-wise events + motion regressors
motion <- data.frame(
  mx = rnorm(50),
  my = rnorm(50),
  mz = rnorm(50)
)

emodel <- event_model(
  onset ~ trialwise(basis = "spmg3") +
          covariate(mx, my, mz, data = motion, id = "motion"),
  data = events,
  block = ~ 1,
  sampling_frame = sframe
)

# Design includes both trial-wise regressors and motion columns
```

### Custom Trial Labels

```r
# Use custom prefix for trial columns
emodel <- event_model(
  onset ~ trialwise(basis = "spmg1", label = "beta"),
  data = events,
  block = ~ run,
  sampling_frame = sframe
)

# Columns named: beta_beta.01, beta_beta.02, etc.
```

### Event Durations

```r
# Model 2-second events (boxcar convolution)
emodel <- event_model(
  onset ~ trialwise(basis = "spmg1"),
  data = events,
  block = ~ run,
  sampling_frame = sframe,
  durations = 2  # 2-second events
)
```

---

## Integration with fmriproj Pipeline

### Full Analysis Workflow

```r
library(fmridesign)
library(fmrihrf)
library(fmriproj)
library(rMVPA)

# 1. Setup
events <- data.frame(
  onset = seq(0, 90, by = 10),
  condition = rep(c("face", "scene"), each = 5),
  RT = rnorm(10, mean = 0.5, sd = 0.1)
)

sframe <- sampling_frame(blocklens = 100, TR = 2)

# 2. Create trial-wise design
emodel <- event_model(
  onset ~ trialwise(basis = "spmg3", add_sum = TRUE),
  data = events,
  block = ~ 1,
  sampling_frame = sframe
)

X <- as.matrix(design_matrix(emodel))

# 3. Load BOLD data
Y <- load_bold_data(...)  # [100 time points, 10000 voxels]

# 4. Build adaptive ridge projector
projector <- build_projector(
  X = X,
  lambda_method = "EB",  # Empirical Bayes
  collapse_method = "robust_svd"
)

# 5. Project to trial space
trial_patterns <- project(projector, Y)
# dim: [30, 10000] = (10 trials × 3 bases) × voxels
# Note: Grand mean column excluded from projection

# 6. Collapse to trial-level estimates
trial_betas <- collapse_betas(trial_patterns, n_bases = 3)
# dim: [10 trials, 10000 voxels]

# 7. Use with rMVPA for classification
# ... continue with MVPA analysis
```

### Using with rMVPA Searchlight

```r
# Create projection spec for searchlight
spec <- projection_spec_formula(
  formula = onset ~ trialwise(basis = "spmg3"),
  data = events,
  sampling_frame = sframe,
  lambda_method = "EB",
  collapse_method = "robust_svd"
)

# Create searchlight function
sl_fun <- make_rmvpa_searchlight_fun(spec)

# Run searchlight (requires rMVPA)
# results <- searchlight(dataset, sl_fun, ...)
```

---

## Comparison: Old vs New Approach

### Scenario: 5 trials, SPMG3 basis

#### Old Approach (fmriproj only)

```r
# 1. Manual event model construction
event_model <- list(
  onsets = c(0, 10, 20, 30, 40),
  n_time = 60,
  conditions = factor(1:5),  # Each trial = separate condition
  blockids = rep(1, 5)
)

# 2. Build design matrix
design <- build_design_matrix(
  event_model = event_model,
  hrf_basis_func = create_hrf_basis_func("spmg3"),
  TR = 2,
  onset_units = "sec"
)

X <- design$X
# [60 time points, 15 columns] = 5 trials × 3 bases
```

**Pros**: Works, familiar
**Cons**: Manual setup, verbose, error-prone

#### New Approach (fmridesign + fmriproj)

```r
# 1. Event data frame (cleaner)
events <- data.frame(onset = c(0, 10, 20, 30, 40))

# 2. Create trial-wise design (declarative)
sframe <- sampling_frame(blocklens = 60, TR = 2)

emodel <- event_model(
  onset ~ trialwise(basis = "spmg3"),
  data = events,
  block = ~ 1,
  sampling_frame = sframe
)

X <- as.matrix(design_matrix(emodel))
# [60 time points, 15 columns] = 5 trials × 3 bases
```

**Pros**: Cleaner, declarative, less error-prone, more features
**Cons**: Requires fmridesign dependency (but we already have it)

---

## Available HRF Basis Types

```r
# List all available HRF types
list_available_hrfs()
```

| Basis | nbasis | Description |
|-------|--------|-------------|
| spmg1 | 1 | SPM canonical HRF |
| spmg2 | 2 | Canonical + temporal derivative |
| spmg3 | 3 | Canonical + temporal + dispersion derivatives |
| gamma | 1 | Double gamma HRF |
| gaussian | 1 | Gaussian HRF |
| fir | N | Finite impulse response (specify nbasis) |
| bspline | N | B-spline basis (specify nbasis) |

---

## Tips & Best Practices

### 1. When to Use `add_sum = TRUE`

✅ **Use it when**:
- You want both trial-specific and condition-general effects
- You're comparing to traditional GLM results
- You want to include the grand mean as a nuisance regressor

❌ **Don't use it when**:
- You only care about trial-specific patterns
- You're doing pure beta-series correlation

### 2. Choosing HRF Basis

**SPMG1** (default):
- Standard beta-series analysis
- Assumes canonical HRF shape
- Fastest, most efficient

**SPMG3**:
- Allows HRF shape variability
- Captures timing and width variations
- Recommended for exploratory analyses

**FIR**:
- No HRF assumptions
- Fully data-driven
- Best for unusual response shapes
- Requires more data (more parameters)

### 3. Multi-Run Considerations

- Events with same onset in different runs are **separate trials**
- Use `block = ~ run` to correctly handle run structure
- Sampling frame `blocklens` must match number of TRs per run

### 4. Memory Considerations

Trial-wise designs create large matrices:
- 100 trials × SPMG3 (3 bases) = 300 columns
- For whole-brain (e.g., 100k voxels), this is manageable
- For searchlight, projector is rebuilt per searchlight sphere

---

## Troubleshooting

### Issue: "onset column not found"

```r
# Error: data must contain 'onset' column

# Fix: Ensure your data frame has an 'onset' column
events <- data.frame(
  onset = c(0, 10, 20),  # Must be named 'onset'
  ...
)
```

### Issue: Dimension mismatch

```r
# Error: design matrix rows != BOLD data rows

# Fix: Check sampling_frame matches BOLD data
sframe <- sampling_frame(
  blocklens = nrow(Y),  # Must match BOLD time points
  TR = 2
)
```

### Issue: "Failed to evaluate trialwise expression"

Usually means `onsets` variable not available. This shouldn't happen with `event_model()` but check that you're using the formula interface correctly.

---

## See Also

- **fmridesign vignettes**: `vignette("event_models", package = "fmridesign")`
- **fmrihrf HRF types**: `?fmrihrf::list_available_hrfs`
- **fmriproj projection**: `?fmriproj::build_projector`
- **Phase 1 integration tests**: `tests/testthat/test-phase1-integration.R`
- **Compatibility tests**: `tests/testthat/test-trialwise-compatibility.R`

---

## Next Steps

1. **Try it out**: Run examples above with your data
2. **Compare results**: Verify new approach matches old approach
3. **Provide feedback**: Report any issues on GitHub

**Questions?** Open an issue at https://github.com/bbuchsbaum/fmriproj/issues
