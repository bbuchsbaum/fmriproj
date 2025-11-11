# Debugging fmriproj → rMVPA run_searchlight Workflow

## Error Summary

**Error Message:**
```
Error in `dplyr::filter(., !error)`: ℹ In argument: `!error`.
Caused by error in `!error`:
! invalid argument type
```

**Location:** Line 811 in `rMVPA::searchlight.R` function `do_standard()`

**Call Chain:**
```
fmriproj::run_searchlight() 
  → rMVPA::run_searchlight() (generic)
  → rMVPA::run_searchlight.default()
  → rMVPA::run_searchlight_base()
  → rMVPA::do_standard()
  → rMVPA::mvpa_iterate()  [returns tibble `ret`]
  → dplyr::filter(ret, !error)  [FAILS HERE]
```

---

## 1. The rMVPA Workflow Structure

### A. run_searchlight_base() (lines 860-947)

Located: `/Users/bbuchsbaum/code/rMVPA/R/searchlight.R`

Dispatches to either `do_standard()` or `do_randomized()` based on `method` parameter.

### B. do_standard() (lines 802-832)

```r
do_standard <- function(model_spec, radius, mvpa_fun=mvpa_iterate, 
                        combiner=combine_standard, ...) {
  # Line 805: Get searchlight neighborhoods
  slight <- get_searchlight(model_spec$dataset, "standard", radius)
  
  # Line 808: Get masked voxel indices
  cind <- which(model_spec$dataset$mask > 0)
  
  # Line 810: Run MVPA iteration - this returns a tibble
  ret <- mvpa_fun(model_spec, slight, cind, analysis_type="searchlight", ...)
  
  # Line 811: FAILS HERE - tries to filter error column
  good_results <- ret %>% dplyr::filter(!error)
  bad_results <- ret %>% dplyr::filter(error == TRUE)
  
  # Line 831: Combine results
  combiner(model_spec, good_results, bad_results)
}
```

**Key Issue:** `ret` should be a tibble with an `error` column of type **logical**.

### C. mvpa_iterate() (lines 366-491)

Located: `/Users/bbuchsbaum/code/rMVPA/R/mvpa_iterate.R`

**Returns:** `dplyr::bind_rows(results)` where `results` is a list of tibbles.

Each tibble in `results` comes from:
- `run_future()` → processes ROI and creates result rows
- Filtered-out ROI handling → creates error rows manually

---

## 2. The Return Value Structure from mvpa_iterate()

### Expected Columns (from various result sources)

The returned tibble should have these columns (not all rows need all columns):

| Column | Type | Notes |
|--------|------|-------|
| `result` | list | Model result object (or NULL if error) |
| `indices` | list | Voxel indices in the ROI |
| `performance` | list | Performance metrics (or NULL) |
| `id` | integer | Voxel ID or region identifier |
| **`error`** | **logical** | **TRUE if failed, FALSE if succeeded** |
| `error_message` | character | Description of error (or "~") |
| `warning` | logical | Whether warning occurred |
| `warning_message` | character | Warning text (or "~") |
| `class` | list | Prediction classes (from crossval) |
| `probs` | list | Prediction probabilities |
| `y_true` | list | True labels |
| `fit` | list | Fitted model object |

### Critical: The `error` Column MUST be Logical

From `process_roi_default()` (lines 276-294 in `/Users/bbuchsbaum/code/rMVPA/R/allgeneric.R`):

**When SUCCEEDS:**
```r
result_set <- tibble::tibble(
  result = list(train_result_obj),
  error = FALSE,                    # ← Logical scalar
  error_message = "~"
)
```

**When FAILS:**
```r
result_set <- tibble::tibble(
  result = list(NULL),
  error = TRUE,                     # ← Logical scalar
  error_message = ifelse(is.null(error_msg), "Unknown training error", error_msg)
)
```

From `external_crossval()` (lines 140-141, 162-166 in `/Users/bbuchsbaum/code/rMVPA/R/mvpa_iterate.R`):

**Successful crossval:**
```r
tibble::tibble(
  result=list(cres),
  indices=list(ind),
  performance=list(compute_performance(mspec, cres)),
  id=id,
  error=FALSE,                      # ← Logical
  error_message="~",
  warning=!is.null(result$warning),
  warning_message=if (is.null(result$warning)) "~" else result$warning
)
```

---

## 3. The Root Cause: Missing get_samples() Method

### The Problem

In `mvpa_iterate()` at line 418:

```r
sf <- get_samples(mod_spec$dataset, vox_list[batch_ids[[i]]]) %>% 
  mutate(.id=batch_ids[[i]], rnum=rnums[[i]], size=size) %>% 
  filter(size>=2)
```

**For normal rMVPA datasets:** This calls `get_samples.mvpa_dataset()`

**For fmriproj's wrapped dataset:** There is NO `get_samples.projecting_dataset()` method!

### Why This Matters

When `S3` dispatch can't find `get_samples.projecting_dataset`, it falls back through the class hierarchy:

```r
class(projecting_dataset_obj)
# [1] "projecting_dataset" "mvpa_dataset" ...
```

The wrapped dataset has the right classes, BUT if there's a naming conflict or the default method gets called instead, it can cause issues:

1. The default/fallback method might not properly handle the wrapped structure
2. The `data_sample()` creation might fail
3. The result tibble structure might be malformed

### The get_samples() Implementation Chain

**File:** `/Users/bbuchsbaum/code/rMVPA/R/resample.R` (lines 20-22)

```r
#' @export
get_samples.mvpa_dataset <- function(obj, vox_list) {
  .get_samples(obj, vox_list)
}
```

Where `.get_samples()` is (lines 8-17):

```r
.get_samples <- function(obj, voxlist) {
  ret <- lapply(voxlist, function(vox) {
    sam <- data_sample(obj, vox)  # Create data_sample objects
  })
  
  n <- length(ret)
  df <- tibble::tibble(sample = ret)  # Return tibble with 'sample' column
  df[[".id"]] <- gen_id(n)            # Add ID column
  df
}
```

**Returns:** A tibble with columns: `sample`, `.id`

Then in `mvpa_iterate()`, this gets enhanced with:
```r
sf %>% mutate(.id=batch_ids[[i]], rnum=rnums[[i]], size=size)
```

---

## 4. The Complete rMVPA → mvpa_iterate → Result Flow

### Step 1: Extract Samples

```
get_samples(dataset, vox_list)
  ↓
Returns: tibble with columns: sample, .id
  ↓
sf <- ... %>% mutate(.id, rnum, size) %>% filter(size >= 2)
```

### Step 2: Extract ROI Data

```
For each sample:
  extract_roi(sample, dset, center_global_id)
  ↓
Returns: list(train_roi, test_roi)
```

### Step 3: Process ROI

```
process_roi(mod_spec, roi, rnum, center_global_id)
  ↓
Uses process_roi_default() or model-specific method
  ↓
Returns: tibble with: result, error, error_message, ...
```

### Step 4: Combine Results

```
rbind all result tibbles
  ↓
bind_rows(results) in mvpa_iterate()
  ↓
Returns: Single tibble with ALL rows combined
  ↓
do_standard receives this tibble
  ↓
FILTER on error column!
```

---

## 5. What fmriproj's wrap_as_projecting_dataset Does

**File:** `/Users/bbuchsbaum/code/fmriproj/R/rMVPA_integration.R` (lines 107-241)

Creates a wrapped dataset with:

```r
structure(
  list(
    Y = Y,                          # Time x voxels data
    projection_fun = projection_fun, # Projection function
    original = original_dataset,    # Original mvpa_dataset
    expected_trials = expected_trials,
    nfeatures = ...number of voxels,
    mask = original_dataset$mask,   # Delegate mask
    train_data = original_dataset$train_data, # Delegate train_data
    get_data = function(indices = NULL) {
      # Extract subset + perform projection
      Y_slice <- fetch_matrix(indices)
      result <- projection_fun(Y_slice)  # Apply projection
      return(result)
    }
  ),
  class = c("projecting_dataset", class(original_dataset))
)
```

**Critical Issue:** The wrapped dataset:
- ✓ Has the right `mask` and `train_data` fields
- ✓ Implements `get_data()` for projection
- ✗ **Does NOT implement `get_samples()`**

---

## 6. The Missing Link: get_samples.projecting_dataset()

### What the Method Should Do

```r
get_samples.projecting_dataset <- function(obj, vox_list) {
  # Delegate to the original dataset's get_samples method
  get_samples(obj$original, vox_list)
}
```

### Why This Works

1. `get_samples()` is called with the wrapped dataset
2. S3 dispatch looks for `get_samples.projecting_dataset`
3. It finds it and calls the method above
4. The method delegates to the original dataset
5. The original dataset's `get_samples.mvpa_dataset()` is used
6. Everything works as expected!

---

## 7. Why the dplyr::filter Error Occurs

### Hypothesis: Type Inconsistency in error Column

When `dplyr::bind_rows()` combines tibbles with inconsistent types for a column:

```r
# Scenario 1: Normal case
tibble1 <- tibble(error = FALSE)     # logical
tibble2 <- tibble(error = TRUE)      # logical
combined <- bind_rows(tibble1, tibble2)
# error column is logical ✓

# Scenario 2: If get_samples fails
# Maybe some rows are created with: error = list(NULL) or error = NULL
# Or missing error column that gets filled with NA
combined <- bind_rows(tibble1, tibble_with_na)
# error column becomes: logical/NULL/NA mixture
# dplyr::filter(!error) FAILS because ! operator can't handle mixed types
```

---

## 8. Complete Data Flow in run_searchlight

```
fmriproj::run_searchlight(fmri_dset, radius=3, ...)
  │
  ├─ Create projection_spec (for fMRI-specific processing)
  ├─ Build wrapped dataset via wrap_as_projecting_dataset()
  │   └─ Class: c("projecting_dataset", "mvpa_dataset")
  │   └─ Has: Y, projection_fun, original, get_data()
  │   └─ MISSING: get_samples()
  │
  └─ Call rMVPA::run_searchlight(model, radius, method, ...)
      │
      └─ rMVPA::run_searchlight_base(model_spec, radius, method, ...)
          │
          └─ rMVPA::do_standard(model_spec, radius, mvpa_iterate, combine_standard)
              │
              ├─ Get searchlight neighborhoods
              ├─ Call mvpa_iterate(model_spec, neighborhoods, ...)
              │   │
              │   ├─ For each batch:
              │   │   ├─ Call get_samples(dataset, vox_list)
              │   │   │   └─ [MISSING METHOD] No get_samples.projecting_dataset!
              │   │   │   └─ Falls back or fails
              │   │   │
              │   │   └─ Extract ROI, process, return tibble
              │   │
              │   └─ Return: bind_rows(all_result_tibbles)
              │       └─ Malformed error column (not pure logical)
              │
              └─ Filter: ret %>% dplyr::filter(!error)
                  └─ FAILS: error column type is invalid for ! operator
```

---

## 9. Solution Summary

### The Fix (One-liner)

Add this method to fmriproj's `rMVPA_integration.R`:

```r
#' @export
get_samples.projecting_dataset <- function(obj, vox_list) {
  get_samples(obj$original, vox_list)
}
```

### Why This Works

1. **Preserves S3 dispatch:** Provides explicit method for wrapped dataset class
2. **Delegates properly:** Uses the original dataset's implementation
3. **Minimal change:** No modification to rMVPA code needed
4. **Type safety:** Ensures `error` column stays logical throughout
5. **Maintains projection:** The wrapped dataset's `get_data()` is still used during ROI extraction

### Location

Add to: `/Users/bbuchsbaum/code/fmriproj/R/rMVPA_integration.R`

Right after the `wrap_as_projecting_dataset()` function definition, around line 241.

---

## 10. Related Methods to Consider

### data_sample() Compatibility

The `data_sample.mvpa_dataset()` method (in rMVPA) creates:
```r
data_sample <- list(data = NULL, vox = vox)
```

This should work fine with the wrapped dataset since the ROI extraction uses `obj$original` internally.

### as_roi() and filter_roi() Compatibility

These methods also need to work with the wrapped dataset. The wrapped dataset should inherit properly since:
- `class(wrapped_dataset)` includes the original classes
- The wrapped dataset preserves `mask` and `train_data` fields

However, if there are S3 dispatch issues with other methods, similar delegation patterns should be applied.

---

## 11. Testing the Fix

**Before fix:** `run_searchlight` test fails with the dplyr::filter error

**After fix:** 
1. `get_samples()` finds the method
2. Returns proper tibble structure
3. `mvpa_iterate()` combines results with consistent types
4. `dplyr::filter(!error)` works correctly
5. `do_standard()` proceeds to combiner function
6. Test should pass!

---

## Appendix: Key File Locations

| Component | File | Key Lines |
|-----------|------|-----------|
| run_searchlight wrapper | `/Users/bbuchsbaum/code/fmriproj/R/api_run_analysis.R` | 39-138 |
| wrap_as_projecting_dataset | `/Users/bbuchsbaum/code/fmriproj/R/rMVPA_integration.R` | 107-241 |
| do_standard (where error occurs) | `/Users/bbuchsbaum/code/rMVPA/R/searchlight.R` | 802-832 |
| mvpa_iterate | `/Users/bbuchsbaum/code/rMVPA/R/mvpa_iterate.R` | 366-491 |
| process_roi_default | `/Users/bbuchsbaum/code/rMVPA/R/allgeneric.R` | 236-301 |
| get_samples.mvpa_dataset | `/Users/bbuchsbaum/code/rMVPA/R/resample.R` | 20-27 |
| external_crossval | `/Users/bbuchsbaum/code/rMVPA/R/mvpa_iterate.R` | 89-175 |

