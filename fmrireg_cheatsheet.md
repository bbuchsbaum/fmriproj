# fmrireg Cheatsheet

This cheatsheet summarizes useful functions from the `fmrireg` package used in the tests.

## Key functions

- `simulate_bold_signal(ncond, hrf, nreps, amps, isi, ampsd, TR)`
  simulates BOLD responses for the provided HRF and returns a list with `onset` and `mat`.
- `HRF_SPMG1`, `HRF_BSPLINE`, `HRF_TENT`
  basis generating functions for modeling the hemodynamic response.

## LS + SVD HRF recovery workflow

1. Simulate data with `simulate_bold_signal` using a canonical HRF.
2. Build a design matrix with `build_design_matrix`, supplying a flexible basis such as `HRF_BSPLINE` or `HRF_TENT`.
3. Fit an ordinary least squares projector with `build_projector` to obtain trial-wise estimates.
4. Reshape the estimates into a trial-by-basis matrix and apply `svd`.
5. The first left singular vector approximates the underlying HRF shape and can be compared to the ground truth.

See `tests/testthat/test-ls-svd-hrf-recovery.R` for an example.
