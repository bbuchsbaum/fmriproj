# Migration Guide: fmriproj fmridesign Integration

**Version**: 0.2.0 (Phase 1-3 Integration)
**Date**: 2025-11-07

This guide helps you migrate to the new fmridesign-integrated version of fmriproj and take advantage of enhanced design specification capabilities.

---

## Overview of Changes

### What's New

- **Phase 1** (Immediate): Improved onset parsing and HRF handling using fmridesign/fmrihrf
- **Phase 2** (Optional): Formula-based design API for advanced users
- **Phase 3** (Future): Full trial-wise support in fmridesign upstream

### Backward Compatibility

✅ **All existing code continues to work unchanged**

The integration is designed to be **100% backward compatible**. Your existing scripts will run without modification.

---

## Phase 1: Enhanced Infrastructure (Available Now)

### HRF Handling

**Before:**
```r
# Custom SPMG3 with theta parameters
hrf_func <- hrf_basis_spmg3_theta
```

**After (Recommended):**
```r
# Use fmrihrf for more options
library(fmrihrf)

# Standard SPMG3
hrf_func <- create_hrf_basis_func("spmg3")

# Or use directly
hrf_func <- create_hrf_basis_func(HRF_SPMG3)

# With lag
hrf_func <- create_hrf_basis_func(HRF_SPMG3, lag = 2)

# Other HRF types now available
hrf_gamma <- create_hrf_basis_func("gamma")
hrf_gaussian <- create_hrf_basis_func("gaussian")
hrf_fir <- create_hrf_basis_func("fir", nbasis = 10)
```

### List Available HRFs

```r
# See all available HRF types
list_available_hrfs()
#>    name   nbasis                     description
#> 1 spmg1        1             SPM canonical HRF
#> 2 spmg2        2  SPM canonical + temporal derivative
#> 3 spmg3        3  SPM canonical + temporal + dispersion derivatives
#> ...
```

### Design Matrix Construction

**Before:**
```r
design <- build_design_matrix(
  event_model = event_model,
  hrf_basis_func = hrf_basis_spmg3_theta,
  theta_params = c(1, 1),
  TR = 2
)
```

**After (Both work):**
```r
# Option 1: Continue using old way (still works)
design <- build_design_matrix(
  event_model = event_model,
  hrf_basis_func = hrf_basis_spmg3_theta,  # Deprecated but functional
  theta_params = c(1, 1),
  TR = 2
)

# Option 2: Use new fmrihrf-based approach (recommended)
design <- build_design_matrix(
  event_model = event_model,
  hrf_basis_func = create_hrf_basis_func("spmg3"),
  TR = 2
)
```

### Benefits of Phase 1

- ✅ More HRF options (7+ types)
- ✅ Better validation and error messages
- ✅ Improved robustness for multi-run scenarios
- ✅ Foundation for Phase 2 features

---

## Phase 2: Formula API (Optional New Feature)

### When to Use Formula API

**Use formula API if:**
- You have complex factorial designs
- You need parametric modulation
- You want clearer, more expressive code
- You appreciate better type safety

**Stick with list-based API if:**
- You have simple designs
- You prefer the current approach
- You don't need formula syntax

### Basic Formula Usage

**Old Way (Still Works):**
```r
event_model <- list(
  onsets = c(0, 10, 20, 30),
  n_time = 50,
  conditions = c("A", "B", "A", "B")
)

patterns <- project_trials(Y, event_model, lambda_method = "EB")
```

**New Way (Optional):**
```r
library(fmridesign)

events <- data.frame(
  onset = c(0, 10, 20, 30),
  condition = factor(c("A", "B", "A", "B"))
)

sframe <- sampling_frame(blocklens = 50, TR = 2)

patterns <- project_trials_formula(
  Y = Y,
  formula = onset ~ hrf(condition),
  data = events,
  sampling_frame = sframe,
  lambda_method = "EB"
)
```

### Advanced Formula Examples

**Parametric Modulation:**
```r
events <- data.frame(
  onset = c(0, 10, 20),
  condition = c("A", "B", "A"),
  RT = c(0.5, 0.6, 0.4)
)

# Include RT as modulator
patterns <- project_trials_formula(
  Y = Y,
  formula = onset ~ hrf(condition, RT),  # RT modulation
  data = events,
  sampling_frame = sframe
)
```

**Factorial Designs:**
```r
events <- data.frame(
  onset = seq(0, 90, by = 10),
  stimulus = factor(c("face", "scene")),
  task = factor(c("remember", "forget"))
)

# Automatic interaction
patterns <- project_trials_formula(
  Y = Y,
  formula = onset ~ hrf(stimulus, task),  # Creates interaction
  data = events,
  sampling_frame = sframe
)
```

**Multiple Terms:**
```r
# Task effects + parametric RT modulation
patterns <- project_trials_formula(
  Y = Y,
  formula = onset ~ hrf(task) + hrf(RT, basis = "gaussian"),
  data = events,
  sampling_frame = sframe
)
```

### Formula API Functions

| Function | Purpose |
|----------|---------|
| `project_trials_formula()` | Complete projection pipeline with formula |
| `build_design_matrix_formula()` | Design matrix only (for inspection) |
| `projection_spec_formula()` | Create spec for rMVPA integration |

### Migrating Existing Code

You **don't need to migrate**. But if you want to:

**Step 1**: Convert event model to data frame
```r
# Before
event_model <- list(
  onsets = my_onsets,
  n_time = 100,
  conditions = my_conditions,
  amplitudes = my_amps
)

# After
events_df <- data.frame(
  onset = my_onsets,
  condition = my_conditions,
  amplitude = my_amps  # If needed
)
```

**Step 2**: Create sampling frame
```r
sframe <- sampling_frame(blocklens = 100, TR = 2)
```

**Step 3**: Use formula API
```r
patterns <- project_trials_formula(
  Y = Y,
  formula = onset ~ hrf(condition),
  data = events_df,
  sampling_frame = sframe,
  lambda_method = "EB"
)
```

---

## Phase 3: Full fmridesign Integration (Planned)

### Current Status

Phase 3 depends on upstream contribution to fmridesign. See `docs/fmridesign_trialwise_proposal.md` for details.

### What Will Change

Once fmridesign supports `trialwise=TRUE`:

**Future (After fmridesign update):**
```r
library(fmridesign)

# fmridesign will directly support trial-wise
emod <- event_model(
  onset ~ hrf(condition),
  data = events,
  sampling_frame = sframe,
  trialwise = TRUE  # <-- NEW in fmridesign
)

# Use directly with fmriproj
trial_design <- as_trialwise_design(emod)  # Simpler adapter
```

### Timeline

- **Proposal submitted**: Week 8 (current phase)
- **fmridesign PR**: Weeks 9-12
- **fmriproj full integration**: After fmridesign release

---

## Deprecation Timeline

### Soft Deprecation (v0.2.0 - Current)

**Deprecated but still functional:**
- `hrf_basis_spmg3_theta()` - Use `create_hrf_basis_func("spmg3")` instead

**Deprecation warnings:**
```r
hrf_func <- hrf_basis_spmg3_theta(theta = c(1, 1), t = seq(0, 29))
#> Warning: `hrf_basis_spmg3_theta()` was deprecated in fmriproj 0.2.0.
#> Please use fmrihrf::HRF_SPMG3 instead.
```

### Hard Deprecation (v1.0.0 - Future)

**Will be removed in v1.0.0:**
- `hrf_basis_spmg3_theta()` function
- Internal onset parsing (fully replaced by fmridesign helpers)

**Migration period**: 6 months minimum before removal

### How to Update

**Replace This:**
```r
hrf_func <- hrf_basis_spmg3_theta
theta_params <- c(1, 1)
```

**With This:**
```r
hrf_func <- create_hrf_basis_func("spmg3")
theta_params <- NULL  # Not needed with fmrihrf
```

---

## Common Migration Scenarios

### Scenario 1: Simple Event-Related Design

**Before:**
```r
event_model <- list(onsets = c(0, 10, 20), n_time = 50)
design <- build_design_matrix(event_model,
                              hrf_basis_func = hrf_basis_spmg3_theta,
                              TR = 2)
```

**After:**
```r
# No change required! But recommended:
event_model <- list(onsets = c(0, 10, 20), n_time = 50)
design <- build_design_matrix(event_model,
                              hrf_basis_func = create_hrf_basis_func("spmg3"),
                              TR = 2)
```

### Scenario 2: Multi-run Analysis

**Before:**
```r
event_model <- list(
  onsets = c(0, 10, 0, 10),  # Per-run reset
  n_time = 100,
  n_time_per_run = c(50, 50)
)
run_id <- c(1, 1, 2, 2)

design <- build_design_matrix(event_model, ..., run_id = run_id)
```

**After:**
```r
# Works identically, but with better validation
# (Same code, improved error messages and robustness)
event_model <- list(
  onsets = c(0, 10, 0, 10),
  n_time = 100,
  n_time_per_run = c(50, 50)
)
run_id <- c(1, 1, 2, 2)

design <- build_design_matrix(event_model, ..., run_id = run_id)
```

### Scenario 3: Custom HRF Basis

**Before:**
```r
my_hrf <- matrix(c(0, 0.5, 1, 0.5, 0), ncol = 1)
design <- build_design_matrix(event_model,
                              hrf_basis_matrix = my_hrf)
```

**After:**
```r
# Still works! HRF matrix takes precedence
my_hrf <- matrix(c(0, 0.5, 1, 0.5, 0), ncol = 1)
design <- build_design_matrix(event_model,
                              hrf_basis_matrix = my_hrf)
```

### Scenario 4: Searchlight Analysis

**Before:**
```r
spec <- projection_spec(event_model, projector, ...)
sl_fun <- make_rmvpa_searchlight_fun(spec)
```

**After (Formula-based):**
```r
# Optional new way
spec <- projection_spec_formula(
  formula = onset ~ hrf(condition),
  data = events_df,
  sampling_frame = sframe,
  lambda_method = "EB"
)
sl_fun <- make_rmvpa_searchlight_fun(spec)
```

---

## Troubleshooting

### Issue: "Package 'fmridesign' is required"

**Solution:** Install dependencies
```r
# Install from GitHub
devtools::install_github("bbuchsbaum/fmridesign")
devtools::install_github("bbuchsbaum/fmrihrf")

# Or rely on fmriproj's automatic installation
install.packages("fmriproj")  # Will pull dependencies
```

### Issue: "Deprecated function warning"

**Solution:** Update to new HRF functions
```r
# Old
hrf_func <- hrf_basis_spmg3_theta

# New
hrf_func <- create_hrf_basis_func("spmg3")
```

### Issue: Formula API not working

**Check:**
1. Is fmridesign installed?
2. Is sampling_frame provided?
3. Are column names in data matching formula variables?

```r
# Debug
library(fmridesign)
sframe <- sampling_frame(blocklens = nrow(Y), TR = 2)
names(events_df)  # Check column names match formula
```

### Issue: Different results after upgrade

**Unlikely but possible:**
- Phase 1 has improved onset parsing validation
- Check that your onset times/indices are correct
- Compare design matrices explicitly:

```r
# Old version design
design_old <- build_design_matrix_old_version(...)

# New version design
design_new <- build_design_matrix(...)

# Compare
all.equal(design_old$X, design_new$X, tolerance = 1e-10)
```

---

## Best Practices

### 1. Gradual Migration

Don't rush to migrate everything at once:
- ✅ Start with new projects using formula API
- ✅ Keep existing projects on list-based API
- ✅ Migrate selectively based on project needs

### 2. Version Control

Before upgrading:
```r
# Record current version
packageVersion("fmriproj")

# Upgrade
devtools::install_github("bbuchsbaum/fmriproj")

# Test thoroughly
# If issues, can revert
```

### 3. Testing

After migration, validate results:
```r
# Run existing tests
devtools::test()

# Compare outputs
patterns_old <- readRDS("old_results.rds")
patterns_new <- project_trials(...)

cor(as.vector(patterns_old), as.vector(patterns_new))
# Should be > 0.999
```

### 4. Documentation

Update your analysis scripts:
```r
# Add version note at top of script
# fmriproj v0.2.0 with fmridesign integration
# Analysis date: 2025-11-07
```

---

## Getting Help

### Resources

- **Roadmap**: `INTEGRATION_ROADMAP.md` - Full integration plan
- **Proposal**: `docs/fmridesign_trialwise_proposal.md` - Phase 3 details
- **Examples**: `inst/examples/` - Updated examples
- **Tests**: `tests/testthat/test-phase*-integration.R` - Test examples

### Support

- **GitHub Issues**: https://github.com/bbuchsbaum/fmriproj/issues
- **Vignettes**: Browse vignettes for updated examples
- **NEWS**: Check NEWS.md for version changes

---

## Summary Checklist

When upgrading to v0.2.0:

- [ ] Review this migration guide
- [ ] Install fmridesign and fmrihrf dependencies
- [ ] Test existing code (should work unchanged)
- [ ] Update HRF functions to remove deprecation warnings (optional)
- [ ] Try formula API on new projects (optional)
- [ ] Provide feedback on GitHub

**Remember**: Migration is **optional**. Your existing code will continue working!

---

**Last Updated**: 2025-11-07
**For Version**: fmriproj 0.2.0 (Phase 1-2 complete)
