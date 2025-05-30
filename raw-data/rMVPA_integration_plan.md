# rMVPA Integration Plan for fmriproj

## Overview

Based on the rMVPA cheatsheet analysis, we can see that rMVPA is designed to work with pre-computed beta coefficients as features, while fmriproj's innovation is to bypass this step and work directly with time-series data. The integration strategy focuses on **minimizing changes to rMVPA** by providing adapter functions and compatible interfaces in fmriproj.

## Integration Strategy: Minimize rMVPA Changes

The core strategy is to make fmriproj "speak rMVPA's language" by:

1. **Matching Expected Signatures**: Searchlight functions accept `(Y_sl, coords, indices, ...)`
2. **Flexible Return Types**: Simple matrices for compatibility, lists when diagnostics needed
3. **Adapter Functions**: Wrap fmriproj functionality in rMVPA-compatible interfaces
4. **Data-Level Interception**: Create objects that look like mvpa_datasets but perform projection on-demand

## Key Integration Points

### 1. **Direct Time-Series to Trial Patterns Pipeline**

fmriproj will provide multiple output formats to match rMVPA expectations:

```r
# Standard rMVPA compatibility (no diagnostics):
sl_FUN(Y_sl) → matrix (N_trials x V_sl)

# With diagnostics:
sl_FUN(Y_sl) → list(data = matrix, diag_data = diagnostics)

# The matrix replaces traditional beta coefficients:
# - Each row is a trial pattern (not a condition beta)
# - Each column is a voxel in the searchlight
```

### 2. **Adapter Functions to Minimize rMVPA Changes**

fmriproj will provide wrapper functions that make projection transparent to rMVPA:

```r
# Drop-in replacement for run_searchlight
run_searchlight_projected <- function(model_spec, radius, ...) {
  # Intercepts at data level, returns standard rMVPA results
  # rMVPA code remains unchanged
}

# Creates rMVPA-compatible searchlight functions
make_rmvpa_searchlight_fun <- function(..., return_format = "matrix") {
  # return_format: "matrix" for standard rMVPA
  #                "mvpa_data" for dataset-like structure
  #                "list" for diagnostics
}

# Converts time-series to projected dataset
as_mvpa_dataset <- function(Y, event_model, ...) {
  # Returns object that rMVPA can use directly
}
```

### 3. **Integration with rMVPA Model Types**

All model types work without modification through data-level interception:

- **Standard MVPA** (`mvpa_model`): Trial patterns replace betas seamlessly
- **Vector RSA** (`vector_rsa_model`): Distance computations work on trial patterns
- **Feature RSA** (`feature_rsa_model`): Can use PP as feature selector
- **Contrast RSA** (`contrast_rsa_model`): Trial patterns enable new contrast types

### 4. **"Projecting Dataset" Pattern**

The key innovation to avoid rMVPA changes is the "projecting dataset" - an object that looks like an mvpa_dataset but performs projection on-demand:

```r
wrap_as_projecting_dataset <- function(Y, projection_fun, original_dataset) {
  structure(
    list(
      Y = Y,  # Time-series data
      projection_fun = projection_fun,  # fmriproj projection
      get_data = function(indices = NULL) {
        # Called by rMVPA when it needs data
        if (is.null(indices)) {
          projection_fun(Y)  # Full brain
        } else {
          projection_fun(Y[, indices])  # Searchlight
        }
      }
    ),
    class = c("projecting_dataset", class(original_dataset))
  )
}
```

This allows rMVPA to work unchanged while receiving projected data instead of betas.

### 5. **Progressive Projection Integration**

PP integrates as a standard rMVPA feature selector:

```r
pp_feature_selector <- function(method = "LDA", dims = 2) {
  structure(
    list(
      method = method,
      dims = dims,
      cutoff_type = "top_k",     # rMVPA compatibility
      cutoff_value = dims,       # rMVPA compatibility
      select_features = function(X, Y, ...) {
        # Store projection model in attribute
        pp_model <- fit_pp(X, Y, method = method, dims = dims)
        attr(X, "projection_function") <- function(X_new) {
          predict_pp(pp_model, X_new)
        }
        rep(TRUE, ncol(X))  # All features "selected"
      }
    ),
    class = "feature_selector"
  )
}
```

This works with rMVPA's existing feature selection interface without modifications.

### 6. **Simple Usage Pattern**

The integration is designed so users need minimal changes:

```r
# Traditional rMVPA workflow:
dataset <- mvpa_dataset(beta_maps, mask)
model <- mvpa_model(load_model("sda_notune"), dataset, design)
results <- run_searchlight(model, radius = 3)

# With fmriproj (minimal change):
dataset <- mvpa_dataset(timeseries, mask)  # Note: timeseries not betas
model <- mvpa_model(load_model("sda_notune"), dataset, design)
results <- run_searchlight_projected(model, radius = 3)  # Just add "_projected"
#                         ^^^^^^^^^^
```

### 7. **Implementation Priority**

1. **Phase 1: Basic Compatibility** ✓
   - Searchlight functions return simple matrices
   - Basic diagnostic combination
   - Manual integration possible

2. **Phase 2: Seamless Integration** (Current Focus)
   - `run_searchlight_projected()` wrapper
   - `make_rmvpa_searchlight_fun()` for compatibility
   - Projecting dataset pattern

3. **Phase 3: Advanced Features**
   - CV-aware projections
   - Batch processing optimization
   - Custom distance functions for trial patterns

### 8. **Complete Usage Examples**

```r
# Example 1: Minimal change required
library(fmriproj)
library(rMVPA)

# Standard rMVPA setup
Y <- read_nifti("bold_timeseries.nii")  # Time-series, not betas!
mask <- read_nifti("brain_mask.nii")
dataset <- mvpa_dataset(Y, mask)
design <- mvpa_design(y_train = trial_labels, block_var = run_ids)
model <- mvpa_model(load_model("sda_notune"), dataset, design)

# Only change: use run_searchlight_projected
results <- run_searchlight_projected(model, radius = 3)

# Example 2: With progressive projection
model_pp <- mvpa_model(
  load_model("sda_notune"), 
  dataset, 
  design,
  feature_selector = pp_feature_selector("LDA", dims = 10)
)
results_pp <- run_searchlight_projected(model_pp, radius = 3)

# Example 3: RSA with projected patterns
rsa_design <- vector_rsa_design(D = model_dissim_matrix, 
                               labels = trial_labels,
                               block_var = run_ids)
rsa_model <- vector_rsa_model(dataset, rsa_design)
rsa_results <- run_searchlight_projected(rsa_model, radius = 3)

# Example 4: Custom projecting dataset
proj_dataset <- as_mvpa_dataset(Y, event_model, mask)
# Use with any rMVPA function unchanged
regional_results <- run_regional(
  mvpa_model(load_model("corclass"), proj_dataset, design),
  region_mask = roi_mask
)
```

## Summary

The updated integration plan **minimizes changes to rMVPA** by providing adapter functions and compatible interfaces in fmriproj:

### Key Design Decisions:

1. **Data-Level Interception**: Create "projecting datasets" that look like mvpa_datasets but perform projection on-demand
2. **Flexible Return Types**: Searchlight functions return simple matrices for compatibility, lists when diagnostics needed
3. **Drop-in Replacements**: `run_searchlight_projected()` works exactly like `run_searchlight()`
4. **Standard Interfaces**: PP works as a standard rMVPA feature selector

### Benefits:

- **Minimal rMVPA Changes**: Core rMVPA code remains untouched
- **Simple Migration**: Users just change `run_searchlight` → `run_searchlight_projected`
- **Full Compatibility**: All rMVPA models, cross-validation schemes, and utilities work
- **Preserved Performance**: Direct projection advantage maintained

### Implementation Checklist:

- [x] Basic searchlight function with matrix return
- [x] Diagnostic combination function  
- [ ] `make_rmvpa_searchlight_fun()` helper
- [ ] `as_mvpa_dataset()` converter
- [ ] `run_searchlight_projected()` wrapper
- [ ] `wrap_as_projecting_dataset()` pattern
- [ ] `pp_feature_selector()` integration
- [ ] Complete test suite with rMVPA

This approach ensures that fmriproj's efficiency benefits are available to rMVPA users with minimal friction, while rMVPA's codebase remains stable and unchanged.