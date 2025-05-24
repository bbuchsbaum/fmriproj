# fmriproj

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
- `run_projected_searchlight()`: Runs projected MVPA searchlight analysis
- `optimize_hrf_mvpa()`: Optimizes HRF parameters for MVPA performance

## License

GPL-3 