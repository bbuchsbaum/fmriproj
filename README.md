# fmriproj

<!-- badges: start -->
[![R-CMD-check](https://github.com/bbuchsbaum/fmriproj/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bbuchsbaum/fmriproj/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/bbuchsbaum/fmriproj/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/bbuchsbaum/fmriproj/actions/workflows/test-coverage.yaml)
[![Codecov test coverage](https://codecov.io/gh/bbuchsbaum/fmriproj/branch/main/graph/badge.svg)](https://app.codecov.io/gh/bbuchsbaum/fmriproj?branch=main)
[![pkgdown](https://github.com/bbuchsbaum/fmriproj/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/bbuchsbaum/fmriproj/actions/workflows/pkgdown.yaml)
<!-- badges: end -->

Projected Multivariate Pattern Analysis for fMRI Data

## Overview

`fmriproj` implements a three-layer framework for analyzing fMRI data:

1. **Trial-wise design matrix construction** with optimizable HRF parameters
2. **Adaptive ridge projection** for dimensionality reduction  
3. **Beta coefficient collapse strategies**

The package interfaces with `fmrireg` for experimental design modeling and `rMVPA` for multivariate pattern analysis execution.

Core data structures such as `fr_design_matrix` and `fr_projector` provide
lightweight containers for design matrices and projector components. Low-level
computations rely on RcppArmadillo helper functions for efficient sparse matrix
assembly and multiplication.

## Installation

Since this package depends on `fmrireg` and `rMVPA` which are hosted on GitHub, you'll need to install from GitHub:

```r
# Install devtools if you don't have it
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Install dependencies from GitHub
devtools::install_github("bbuchsbaum/fmrireg")
devtools::install_github("bbuchsbaum/rMVPA")

# Install fmriproj
devtools::install_github("bbuchsbaum/fmriproj")
```

Alternatively, you can use the `remotes` package:

```r
# Install remotes if you don't have it
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# Install fmriproj and its GitHub dependencies
remotes::install_github("bbuchsbaum/fmriproj")
```

## Main Functions

- `build_design_matrix()`: Constructs trial-wise design matrices
- `adaptive_ridge_projector()`: Performs adaptive ridge projection
- `collapse_beta()`: Implements beta coefficient collapse strategies
- `run_searchlight()`: Run a projected searchlight MVPA analysis
- `run_regional()`: Run a projected ROI-based MVPA analysis
- `optimize_hrf_mvpa()`: Optimizes HRF parameters for MVPA performance

### Gradient computation

`optimize_hrf_mvpa()` uses finite-difference gradients by default. A future
release may optionally integrate [TMB](https://github.com/kaskr/adcomp) for
analytic gradients, but there is currently **no TMB requirement**.

## License

GPL-3 