# fmriproj Examples

This directory contains examples demonstrating the usage of all exported functions in the fmriproj package. The examples are organized by the layers of the projection pipeline.

## Core Pipeline Functions

### Layer 1: Design Matrix Construction
- **[example_build_design_matrix.R](example_build_design_matrix.R)** - Creating trial-wise design matrices
  - Basic usage with pre-computed HRF basis
  - Using parametric modulators
  - Dynamic HRF generation with custom functions
  - Handling large designs with safety checks

- **[example_hrf_basis_spmg3_theta.R](example_hrf_basis_spmg3_theta.R)** - Parameterized HRF basis generation
  - Standard SPMG3 basis with canonical, temporal, and dispersion derivatives
  - Effects of delay and dispersion parameters
  - Integration with optimization workflows

### Layer 2: Projection
- **[example_build_projector.R](example_build_projector.R)** - Building global projection components
  - Basic QR decomposition
  - Ridge-regularized projectors
  - Handling ill-conditioned matrices
  - Performance with sparse matrices

- **[example_adaptive_ridge_projector.R](example_adaptive_ridge_projector.R)** - Searchlight-specific adaptive projection
  - Fixed lambda (no adaptation)
  - Empirical Bayes adaptation
  - Local cross-validation
  - Comparing adaptation methods

### Layer 3: Beta Collapse
- **[example_collapse_beta.R](example_collapse_beta.R)** - Collapsing HRF basis coefficients
  - Root-sum-square (RSS) method
  - Principal component (PC) method
  - Supervised optimization of weights
  - Method comparisons

### Layer 4: Dimensionality Reduction
- **[example_progressive_projection.R](example_progressive_projection.R)** - Progressive projection pursuit
  - Linear Discriminant Analysis (LDA)
  - Partial Least Squares (PLS-DA)
  - Cross-validation integration
  - Handling edge cases

## High-Level Functions

- **[example_run_searchlight_projected.R](example_run_searchlight_projected.R)** - One-line searchlight analysis
  - Basic projected searchlight
  - Different adaptive methods
  - Dynamic HRF basis functions
  - Integration with rMVPA

- **[example_optimize_hrf_mvpa.R](example_optimize_hrf_mvpa.R)** - Joint HRF parameter optimization
  - Single-parameter optimization
  - Multi-parameter optimization
  - Supervised weight learning
  - Gradient-based methods

## Diagnostic Functions

- **[example_diagnostics.R](example_diagnostics.R)** - Diagnostic collection and visualization
  - Using `combine_projection_diagnostics()`
  - Creating diagnostic maps with `explain_projection_results()`
  - Memory management for large datasets
  - Custom diagnostic extraction

## Running the Examples

To run any example, simply source the file:

```r
source(system.file("examples/example_build_design_matrix.R", package = "fmriproj"))
```

Or run specific examples interactively by copying code sections.

## Prerequisites

Most examples use simulated data and don't require external dependencies beyond the fmriproj package itself. Some examples may produce better results when the following packages are available:
- `rMVPA` - For full searchlight integration
- `Matrix` - For sparse matrix operations (included in fmriproj dependencies)

## Tips for New Users

1. Start with `example_build_design_matrix.R` to understand the input format
2. Progress through the layers in order (1-4) to understand the pipeline
3. Use `example_run_searchlight_projected.R` to see the integrated workflow
4. Refer to `example_optimize_hrf_mvpa.R` for advanced optimization scenarios