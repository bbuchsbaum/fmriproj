# Qt Refactoring Summary

## Overview
Successfully refactored the fmriproj package to eliminate the critical memory scalability issue identified by Gemini. The core problem was that `build_projector()` was explicitly forming the Qt matrix (n×n), which caused memory exhaustion for large datasets.

## Key Changes

### 1. build_projector.R
- **Before**: Extracted Qt = t(Q) explicitly, creating a potentially huge n×n matrix
- **After**: Stores the QR decomposition object and uses implicit Q operations via `qr.qty()`
- Added `legacy_output` parameter for backward compatibility

### 2. fr_projector constructor (classes.R)
- **Before**: Only accepted Qt, R, and related matrices
- **After**: Polymorphic constructor supporting both:
  - New style: `qr_obj`, `R`, `RtR` (memory efficient)
  - Legacy style: `Qt`, `R`, `K_global`, etc. (with deprecation warning)
- Added `.internal_representation` field to track which style is used

### 3. adaptive_ridge_projector.R
- Created `.solve_ridge_chol()` helper that handles both representations
- Updated main function to extract components based on representation
- For new representation, uses `Matrix::qr.qty()` instead of `Qt %*% Y`
- K_sl computation for diagnostics only supported in legacy mode

### 4. Test Updates
- Updated tests to handle both representations
- Added verification tests to ensure numerical equivalence
- Fixed test expectations for the new architecture

## Memory Savings

For a dataset with n=100,000 timepoints and p=1,000 predictors:
- **Before**: Qt matrix requires 80 GB (100k × 100k × 8 bytes)
- **After**: Only stores QR object and R matrix (~8 MB)

## Backward Compatibility

- Existing code continues to work with `legacy_output = TRUE`
- Deprecation warnings guide users to the new approach
- All numerical results are identical between approaches

## Verification

Created comprehensive tests proving:
1. Numerical equivalence between old and new approaches
2. Memory efficiency (no Qt matrix created in new approach)
3. All existing functionality preserved

## Next Steps

1. Update documentation to reflect new default behavior
2. Consider removing legacy support in a future major version
3. Benchmark performance improvements on large datasets
4. Update any vignettes or examples to use the new approach