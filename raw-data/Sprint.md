Okay, here's a potential "ticketed sprint" plan for the `fmriproj` package development, broken down into manageable tasks. This assumes a sprint-like structure (e.g., 2-week sprints, but adaptable). The goal is to get a functional core quickly, then add advanced features and polish.

---

**`fmriproj` Development Sprints**

**Sprint 0: Setup & Foundational Elements (Pre-Sprint)**

*   **Ticket FPJ-001:** Create `fmriproj` R package structure (DESCRIPTION, NAMESPACE, R/, src/, tests/).
*   **Ticket FPJ-002:** Set up Git repository, basic CI (R CMD check).
*   **Ticket FPJ-003:** Define core data structures/classes (if any specific to `fmriproj` beyond lists/matrices). **(done)**
*   **Ticket FPJ-004:** Draft initial RcppArmadillo helper functions for sparse matrix operations (e.g., sparse matrix assembly for `X(θ)`, efficient sparse-dense products for projections). **(done)**

**Sprint 1: Layer 1 - Sparse Trialwise Design Matrix `X(θ)`**

*   **Ticket FPJ-101:** Implement `fmriproj::make_trialwise_X()` core logic.
    *   Input: `fmrireg_event_model`, `hrf_basis_matrix` (fixed basis initially).
    *   Output: Sparse `Matrix::dgCMatrix` `X`.
    *   Focus on correct sparse assembly from event onsets and pre-convolved basis.
*   **Ticket FPJ-102:** Add support for `hrf_basis_func` and `theta_params` to `make_trialwise_X()`.
    *   Allow dynamic HRF basis generation based on `θ`.
*   **Ticket FPJ-103:** Implement parametric modulation scaling within `make_trialwise_X()`.
*   **Ticket FPJ-104:** Add `max_X_cols` safety check and warning to `make_trialwise_X()`.
*   **Ticket FPJ-105:** Unit tests for `make_trialwise_X()` (various event types, bases, modulators, sparse output verification).
*   **Ticket FPJ-106:** Basic diagnostics for `make_trialwise_X()` (dimensions, sparsity, build time).

**Sprint 2: Layer 2 (Global Part) - Projector Construction `build_projector`**

*   **Ticket FPJ-201:** Implement `fmriproj::build_projector()` using `Matrix::qr()` for sparse QR.
    *   Input: Sparse `X_theta` from Layer 1.
    *   Output: `list(Qt, R)`.
*   **Ticket FPJ-202:** Add `lambda_global` ridge regularization to `build_projector()`.
    *   Output: `K_global` projector if `lambda_global > 0`.
*   **Ticket FPJ-203:** Add condition number calculation for `R` (`cond_R`) and warning for high collinearity in `build_projector()`.
*   **Ticket FPJ-204:** Unit tests for `build_projector()` (sparse QR correctness, ridge application, `cond_R`).
*   **Ticket FPJ-205:** Basic diagnostics for `build_projector()`.

**Sprint 3: Layer 2 (Searchlight Part) & Layer 3 (Basic Collapse) - Adaptive Projection**

*   **Ticket FPJ-301:** Implement `fmriproj::adaptive_ridge_projector()` structure.
    *   Input: `Y_sl`, `projector_components` (from `build_projector()`), `lambda_floor_global`.
    *   Implement projection `Z_sl_raw = K_sl %*% Y_sl` using derived `K_sl`.
*   **Ticket FPJ-302:** Implement `lambda_adaptive_method = "none"` (uses `lambda_floor_global`) in `adaptive_ridge_projector()`.
*   **Ticket FPJ-303:** Implement `fmriproj::collapse_beta()` with `method = "rss"` (root-sum-square).
    *   Input: `Z_sl_raw`, `N_trials`, `K_hrf_bases`. Output: `A_sl`, `w_sl` (implicit).
*   **Ticket FPJ-304:** Unit tests for `adaptive_ridge_projector(method="none")` and `collapse_beta(method="rss")`.
*   **Ticket FPJ-305:** Basic diagnostics for these functions (e.g., `lambda_sl_chosen`).

**Sprint 4: Advanced Collapse & Adaptive Lambda Methods**

*   **Ticket FPJ-401:** Implement `lambda_adaptive_method = "EB"` (Empirical Bayes) in `adaptive_ridge_projector()`.
    *   Requires passing `X_theta_for_EB_residuals`.
    *   Careful implementation of `s_n_sq` and `s_b_sq`.
*   **Ticket FPJ-402:** Implement `lambda_adaptive_method = "LOOcv_local"` (stratified K-fold) in `adaptive_ridge_projector()`.
*   **Ticket FPJ-403:** Implement `collapse_beta()` with `method = "pc"` (SNR-optimal principal component).
*   **Ticket FPJ-404:** Unit tests for new adaptive lambda methods and PC collapse.
*   **Ticket FPJ-405:** Refine diagnostics for Layer 2 & 3.

**Sprint 5: Top-Level Orchestration (Fixed `θ`) & Basic Diagnostics**

*   **Ticket FPJ-501:** Implement `fmriproj::mvpa_projected_searchlight()` one-liner function.
    *   This function will call Layer 1 (`make_trialwise_X`) and Layer 2 global (`build_projector`), then define a `FUN` for `rMVPA::searchlight` that calls Layer 2 local (`adaptive_ridge_projector`) and Layer 3 (`collapse_beta`).
    *   Initially, `rMVPA` integration will be conceptual (i.e., `mvpa_projected_searchlight` sets up the call, but `rMVPA` part is a placeholder).
*   **Ticket FPJ-502:** Implement basic `diagnostics = TRUE` flag propagation in `mvpa_projected_searchlight` and underlying functions.
*   **Ticket FPJ-503:** Implement `fmriproj::combine_projection_diagnostics()` for `rMVPA`'s `.combine` argument (initial version focusing on collecting `lambda_sl`, `w_sl`).
*   **Ticket FPJ-504:** Start `fmriproj::explain_projection_results()`: NIfTI map generation for `lambda_sl`.
*   **Ticket FPJ-505:** Unit tests for `mvpa_projected_searchlight` focusing on parameter passing and execution flow (mocking `rMVPA::searchlight` for now).

**Sprint 6: Joint `θ` Optimization (Structure & Numerical Gradients)**

*   **Ticket FPJ-601:** Define `hrf_basis_spmg3_theta()` (or similar parameterizable HRF basis function).
*   **Ticket FPJ-602:** Implement `fmriproj::optimize_joint_hrf_mvpa()` structure.
    *   Outer `stats::optim()` loop.
    *   Internal `loss_fn_theta(theta, ...)` structure.
*   **Ticket FPJ-603:** Implement `loss_fn_theta` using numerical gradients for `θ` (`optim`'s default).
    *   This involves calling Layer 1 (`make_trialwise_X` with current `theta`) and Layer 2 global (`build_projector`), then running an "inner rMVPA CV loop" on resulting `A_sl` patterns to get loss.
*   **Ticket FPJ-604:** Unit tests for `optimize_joint_hrf_mvpa` basic execution flow with fixed inner loop behavior.
*   **Ticket FPJ-605:** Expand diagnostics to include `theta` optimization trace.

**Sprint 7: Advanced `w` Optimization & Progressive Projection Pursuit**

*   **Ticket FPJ-701:** Implement `collapse_beta()` with `method = "optim"` (supervised `w` optimization).
    *   Requires `classifier_for_w_optim` and `labels_for_w_optim`.
    *   Inner L-BFGS loop with gradient `∂ℓ/∂w_sl`.
*   **Ticket FPJ-702:** Implement `fmriproj::fit_pp()` and `predict_pp()`.
    *   Support `"LDA"` and `"PLS-DA"` methods initially.
    *   Handle fallback for missing classes in test folds.
*   **Ticket FPJ-703:** Unit tests for `optim` collapse and `fit_pp/predict_pp`.
*   **Ticket FPJ-704:** Refine `explain_projection_results()` to include `w_sl` component maps and effective HRF plots.

**Sprint 8: TMB Integration (Optional) & Edge Cases**

*   **Ticket FPJ-801:** (Optional Stretch Goal) Investigate and implement TMB for analytic gradients of `θ` in `optimize_joint_hrf_mvpa()`.
    *   Requires TMB-compatible `hrf_basis_func`. Check for `isSparse(object$env$X)`.
*   **Ticket FPJ-802:** Implement remaining edge-case safeguards:
    *   Tiny TR / Long HRF basis decimation.
    *   Memory ceiling for diagnostics (HDF5Array/fst proof-of-concept or clear guidance).
*   **Ticket FPJ-803:** Comprehensive testing of interactions between adaptive lambda, `w` optimization, and `θ` optimization.
*   **Ticket FPJ-804:** Finalize API function names (verb-noun style) and parameter names across the package.
*   **Ticket FPJ-805:** Comprehensive integration tests combining all layers with various parameter combinations.

**Sprint 9: Documentation & Vignettes**

*   **Ticket FPJ-901:** Write "From GLM to single-pass: understanding the projection" vignette.
    *   Include Haxby example.
    *   Show `explain_projection_results()` usage.
*   **Ticket FPJ-902:** Develop API documentation (roxygen2) for all exported functions.
*   **Ticket FPJ-903:** Create the "Cheat-Sheet PDF" outlining usage profiles.
*   **Ticket FPJ-904:** Prepare benchmark grid scripts and `targets` pipeline.

**Sprint 10: Pre-Release Polish & Benchmarking**

*   **Ticket FPJ-1001:** Run full benchmark grid.
*   **Ticket FPJ-1002:** Analyze benchmark results, update documentation/vignette with performance numbers.
*   **Ticket FPJ-1003:** Final code review, cleanup, and optimization.
*   **Ticket FPJ-1004:** Prepare pre-print draft and demo video materials.
*   **Ticket FPJ-1005:** CRAN submission checks (if applicable for an initial release, or prepare for GitHub-only release).

---

**Next Phase: `rMVPA` Integration**

Once `fmriproj` has a stable core (e.g., after Sprint 5 or 6), work can begin in parallel or sequentially on the `rMVPA` side:

*   **Ticket RMVPA-INT-001:** Adapt `rMVPA::searchlight` to seamlessly accept the `FUN` argument that incorporates `fmriproj::adaptive_ridge_projector` and `fmriproj::collapse_beta`. Ensure `sl_info` (including `center_global_id`) is correctly passed to this `FUN`.
*   **Ticket RMVPA-INT-002:** Adapt `rMVPA::mvpa_iterate` and default combiners (`combine_standard`, `combine_randomized`) to correctly handle the output structure of `fmriproj`'s `FUN` (which now returns `A_sl` features instead of a full model fit object per se for this path).
*   **Ticket RMVPA-INT-003:** Implement or adapt `rMVPA`'s pre-processing hooks to allow `fmriproj::fit_pp` to be used for Progressive Projection Pursuit on `A_sl` within `rMVPA`'s CV folds.
*   **Ticket RMVPA-INT-004:** Ensure diagnostic information collected by `fmriproj` (when `diagnostics=TRUE`) can be passed through `rMVPA::mvpa_iterate` and aggregated by `fmriproj::combine_projection_diagnostics` when used as `rMVPA`'s `.combine` function. This might involve `rMVPA::mvpa_iterate` returning a more complex list per iteration.
*   **Ticket RMVPA-INT-005:** Update `rMVPA` vignettes and examples to showcase the new workflow using `fmriproj` for feature generation.
*   **Ticket RMVPA-INT-006:** Ensure `fmriproj::optimize_joint_hrf_mvpa` can correctly call and interpret results from an "inner loop" `rMVPA` cross-validation on `A_sl` features to compute its loss.

This sprint plan prioritizes building a functional `fmriproj` with its core projection and collapse mechanisms first, allowing for early testing and benchmarking of the speed and basic accuracy improvements. The more advanced joint optimization and full `rMVPA` integration follow, building upon this stable foundation.