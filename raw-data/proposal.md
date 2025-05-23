This is now a very comprehensive and well-defined proposal. The progression through the "Door-Blasting Stack" is clear, the API sketch for `fmriproj` is sensible, and the integration points with `fmrireg` and `rMVPA` are well-articulated. The plan to include robust diagnostics and explainability tools is also crucial for user adoption and scientific rigor. The detailed responses to the clarifying questions have solidified the technical underpinnings.

Here's the full-blooded proposal, incorporating all the latest feedback and details, aiming for maximum clarity and readiness for implementation.

---

**Full Proposal: `fmriproj` – A Single-Pass, Adaptively Regularized, Jointly Optimized MVPA Projection Pipeline**

**1. Executive Summary & Vision:**

`fmriproj` will be a new, dedicated R package designed to revolutionize MVPA workflows by providing a high-performance, single-pass pipeline for transforming raw fMRI time-series data into optimized trial-specific feature patterns. It bridges the gap between `fmrireg`'s sophisticated event modeling and `rMVPA`'s powerful multivariate analysis engine. The core innovations include:

*   **Single-Pass Efficiency:** Eliminating the need for traditional, time-consuming voxel-wise GLM estimations and beta-series generation.
*   **Joint HRF Optimization:** Globally optimizing hemodynamic response function (HRF) shape parameters (`θ`) to maximize MVPA performance.
*   **Adaptive HRF Collapse:** Locally (per searchlight/ROI) optimizing or deriving weights (`w_sl`) to combine HRF basis components in a data-driven, task-relevant manner.
*   **Adaptive Regularization:** Employing searchlight-specific ridge regularization (`λ_sl`) to stabilize trial pattern estimates based on local data characteristics.
*   **Modularity & Control:** Offering a layered API allowing users to select the desired level of sophistication, from simple projections to full joint optimization.
*   **Transparency:** Providing comprehensive diagnostics and explainability tools.

The ultimate goal is to deliver a pipeline that is significantly faster and potentially more sensitive than current standard two-stage MVPA approaches, enabling more advanced and computationally intensive analyses.

**2. Package Ecosystem & Division of Labor:**

*   **`fmrireg` (Existing):**
    *   **Role:** Defines the experimental design: event onsets, durations, conditions, parametric modulators, block structure, and sampling frame.
    *   **Output:** A validated `fmrireg_event_model` object.
*   **`fmriproj` (New Package):**
    *   **Role:** Takes the BOLD time-series (`Y4d`) and `fmrireg_event_model`. Constructs a trial-specific design matrix `X(θ)`. Projects `Y` onto `X(θ)` using adaptive regularization to derive raw projected betas (`Z_sl`). Collapses these betas into trial-specific feature patterns (`A_sl`). Optionally optimizes global HRF parameters (`θ`) and local collapse weights (`w_sl`).
    *   **Output:** For each searchlight/ROI, an `N_trials x V_voxels_in_SL` (or `N_trials x D_reduced_dims_sl`) matrix of trial patterns (`A_sl`) to be consumed by `rMVPA`, plus rich diagnostic information.
*   **`rMVPA` (Existing):**
    *   **Role:** Takes the trial patterns (`A_sl`) from `fmriproj`. Performs MVPA (classification, RSA, etc.) using its established framework for cross-validation, model training, prediction, and performance evaluation.
    *   **Output:** Standard `rMVPA` result objects (e.g., accuracy/RSA maps) potentially augmented with diagnostics from `fmriproj`.

**3. The `fmriproj` "Door-Blasting Stack": Layers and API**

Each layer can be used progressively, offering increasing sophistication.

**Layer 1: Sparse Trialwise Design Matrix `X(θ)`**

*   **Purpose:** Construct the primary `T x (N_trials * K_hrf_bases)` design matrix `X`, parameterized by global HRF shape parameters `θ`.
*   **API Function:** `fmriproj::make_trialwise_X(fmrireg_event_model, hrf_basis_func, theta_params = NULL, hrf_basis_matrix = NULL, sparse = TRUE, max_X_cols = 15000)`
    *   `fmrireg_event_model`: The rich event specification from `fmrireg`.
    *   `hrf_basis_func`: User-supplied R function `(theta_params, time_vector) -> L x K_hrf_bases matrix`. Defines the HRF basis (e.g., SPMG3 where `theta_params` tune TTP/width).
    *   `theta_params`: Current values for `θ`.
    *   `hrf_basis_matrix`: Optional pre-computed `L x K` HRF basis (ignores `hrf_basis_func`, `theta_params`).
    *   `max_X_cols`: Safety threshold; issues warning if `ncol(X)` exceeds this.
*   **Mechanism:**
    *   Leverages `fmrireg`'s event table (onsets, durations, modulators).
    *   Efficient C++ sparse matrix assembly: for each non-zero in `sticks` (trial onsets/amplitudes), the `K_hrf_bases` columns (scaled by modulator if any) are shifted and inserted into `X`.
    *   **Cost:** ~15 ms for typical designs.
*   **Diagnostics:** `X_dims`, `X_sparsity`, `time_to_build_X`.

**Layer 2: Global Projector Construction & Optional Adaptive Ridge `λ_sl`**

*   **Purpose:** Create the main projection components (`Q`, `R`) from `X(θ)` and then, per searchlight, derive an adaptively regularized projector `K_sl` to transform BOLD data `Y_sl` into raw projected betas `Z_sl_raw`.
*   **API Function (Global Part - called once per `θ` update):** `fmriproj::build_projector(X_theta, lambda_global = 0)`
    *   `X_theta`: Sparse design matrix from Layer 1.
    *   `lambda_global`: Global ridge parameter.
    *   **Returns:** `list(Qt, R, K_global, cond_R)`
        *   `Qt`: `t(qr.Q(qr(X_theta)))`.
        *   `R`: `qr.R(qr(X_theta))`.
        *   `K_global`: Projector based on `lambda_global`. If `lambda_global == 0`, `K_global = Qt`.
        *   `cond_R`: Condition number of `R` (warn if > 1e6).
    *   **Cost:** ~70-90 ms for sparse QR + one solve if `lambda_global > 0`.
*   **API Function (Searchlight Local Part - called per searchlight):** `fmriproj::adaptive_ridge_projector(Y_sl, projector_components, adaptive_lambda_method = "EB", lambda_floor_global = 0, X_theta_for_EB_residuals = NULL, diagnostics = FALSE)`
    *   `Y_sl`: `T x V_sl` BOLD data.
    *   `projector_components`: Output from `build_projector()`.
    *   `adaptive_lambda_method`: `"EB"` (Empirical Bayes default), `"LOOcv_local"` (uses stratified K-fold internally for robustness, e.g., K=4), or `"none"` (uses `lambda_floor_global` directly).
    *   `lambda_floor_global`: Minimum lambda; typically set to the `lambda_global` from `build_projector` (default policy: `λ_eff_sl = max(lambda_floor_global, λ_sl_adaptive)`).
    *   `X_theta_for_EB_residuals`: Full `X_theta` matrix (needed for EB method to calculate residuals for `s_n_sq`).
    *   **Returns:** `list(Z_sl_raw = K_sl %*% Y_sl, diag_data = list(lambda_sl_chosen))`
    *   **Mechanism for `λ_sl` (if method != "none"):**
        1.  Uses `projector_components$Qt` and `projector_components$R` (derived from unregularized `X(θ)`).
        2.  Derives `λ_sl_adaptive` via specified `method`.
        3.  `λ_eff_sl = max(lambda_floor_global, λ_sl_adaptive)`.
        4.  `K_sl = solve(crossprod(R) + diag(λ_eff_sl, ncol(R)), t(R)) %*% Qt`. (Caches `crossprod(R)`).
        5.  `Z_sl_raw = K_sl %*% Y_sl`. `(N_trials*K_hrf_bases) x V_sl`.
    *   **Note:** The global `lambda_global` provides baseline regularization in `build_projector()`, while `λ_sl_adaptive` allows further searchlight-specific adjustment based on local data characteristics.
*   **Diagnostics:** `lambda_global_used`, `time_to_build_projector`, `cond_R`, (per SL) `lambda_sl_chosen`.

**Layer 3: HRF `β` Collapse & Optional Local `w_sl` Optimization**

*   **Purpose:** Collapse the `K_hrf_bases` projected beta coefficients for each trial and voxel into a single amplitude feature, using collapse weights `w_sl`. `w_sl` can be fixed (RSS), derived (PC), or optimized locally.
*   **API Function (Per searchlight):** `fmriproj::collapse_beta(Z_sl_raw, N_trials, K_hrf_bases, method = "rss", optim_w_params = list(maxit=5, tol=1e-3), labels_for_w_optim = NULL, classifier_for_w_optim = NULL, diagnostics = FALSE)`
    *   `Z_sl_raw`: `(N_trials*K_hrf_bases) x V_sl` matrix from Layer 2.
    *   `N_trials`, `K_hrf_bases`: Dimensions for reshaping `Z_sl_raw`.
    *   `method`: `"rss"` (root-sum-square), `"pc"` (SNR-optimal principal component), `"optim"` (fully learned supervised `w`).
    *   `optim_w_params`: Controls L-BFGS for `method="optim"`.
    *   `labels_for_w_optim`, `classifier_for_w_optim`: Required if `method="optim"`.
    *   **Returns:** `list(A_sl, w_sl_final, diag_data = list(beta_components_sl, w_optim_convergence_if_any))`
        *   `A_sl`: `N_trials x V_sl` collapsed feature matrix.
        *   `w_sl_final`: `K_hrf_bases x 1` (if PC/optim global to SL) or `K_hrf_bases x V_sl` (if voxel-wise, though current plan is SL-global `w`).
        *   `beta_components_sl`: The reshaped `N_trials x K_hrf_bases x V_sl` array.
*   **Mechanism for `method="optim"`:**
    1.  Initialize `w_sl`.
    2.  Inner L-BFGS loop: Objective `ℓ(v, w_sl)` = classifier loss on `A_sl_current_w = Z_sl_reshaped %*% w_sl`. `v` are classifier weights, re-estimated or held from main MVPA loop. Gradient `∂ℓ/∂w_sl` used. `||w_sl||=1` constraint enforced by re-normalization.
*   **Cost:** RSS/PC is cheap. `optim` adds O(N*Vsl) per L-BFGS iteration (typically 3-5 iter).

**Layer 4: Progressive Projection Pursuit (PP)**

*   **Purpose:** Optional supervised dimensionality reduction of `A_sl` within each `rMVPA` training fold.
*   **API Function (as `rMVPA` preproc):** `fmriproj::fit_pp(A_sl_train, labels_train, method = "LDA", dims = 2)`
    *   **Returns:** `list(Wz_projector, pp_method_details)`. `fmriproj::predict_pp(Wz_projector, A_sl_new)` applies it.
*   **Mechanism:** Trains LDA, PLS-DA, or F-score filter on `A_sl_train`. Handles missing classes in test folds by falling back (e.g., using fewer components, or skipping PP for that specific fold if `dims` cannot be met).
*   **Integration:** PP is applied as a preprocessing step within `rMVPA`'s cross-validation folds, transforming `A_sl` to lower-dimensional representations before classifier training.

**Layer 5: `rMVPA` Classifier & Evaluation**

*   Uses standard `rMVPA` functions (`run_searchlight`, `train_model`, `predict_model`, `performance`).
*   `rMVPA::searchlight`'s `FUN` argument will be a wrapper that calls `fmriproj::adaptive_ridge_projector` and `fmriproj::collapse_beta` in sequence.
*   `rMVPA::train_model` receives `A_sl` (or `A_sl_reduced`) as its `train_dat`.

**Top-Level `fmriproj` Orchestration Functions:**

1.  **One-Liner (Fixed `θ`):** `fmriproj::mvpa_projected_searchlight(...)`
    *   Wraps Layers 1-3 (with fixed `θ` from `hrf_basis_matrix` or default `hrf_basis_func(theta_params=NULL)`) and calls `rMVPA::run_searchlight`. User specifies global `lambda`, adaptive `λ` method, collapse method, PP options (as preproc for `rMVPA`), and `rMVPA` classifier settings.
2.  **Joint `θ` Optimization:** `fmriproj::optimize_joint_hrf_mvpa(...)`
    *   Manages `stats::optim()` loop for `θ`.
    *   Objective `loss_fn_theta(theta, ...)` internally calls Layers 1-3 (dynamic `X(θ)`), then runs an inner `rMVPA` cross-validation loop on the resulting `A_sl` (optionally with PP) to compute loss.
    *   Returns `theta_optimal` and detailed diagnostics. `theta_optimal` can then be used in `mvpa_projected_searchlight` for final map generation.
    *   Gradient for `θ` via numerical differences or TMB (if `hrf_basis_func` is TMB-compatible; requires TMB installation, warn if used but not available).

**6. Diagnostics and Explainability (`diagnostics = TRUE` mode):**

*   Top-level functions (`mvpa_projected_searchlight`, `optimize_joint_hrf_mvpa`) will have a `diagnostics` flag.
*   If `TRUE`, the `FUN` passed to `rMVPA::run_searchlight` (which wraps `fmriproj::adaptive_ridge_projector` and `fmriproj::collapse_beta`) returns its `diag_data`.
*   `rMVPA::run_searchlight` will use a custom `.combine` function: `fmriproj::combine_projection_diagnostics`. This function aggregates:
    *   Standard performance metrics from `rMVPA`.
    *   A list column where each element is the `diag_data` from one searchlight.
*   `fmriproj::explain_projection_results(searchlight_output_with_diagnostics, ...)`:
    *   **Output:** NIfTI maps (via `neuroim2::asNifti` or similar) of `lambda_sl`, components of `w_sl` (e.g., `w_sl[0]` for amplitude, `w_sl[1]` for latency contribution if basis is derivatives), variance explained by projector.
    *   **ROI/SL specific plots:** For user-selected ROIs/SLs:
        *   Global `θ`-optimized HRF basis functions (`B(θ̂)`).
        *   Scatter of `β⁰,β¹,β²` per voxel within the SL.
        *   The SL-specific `w_sl` vector.
        *   Effective HRF for that SL: `B(θ̂) %*% w_sl`.
        *   Scatter plot of `A_sl` trial patterns vs. traditional LSS betas.
    *   Output can be a list of ggplots and/or a rendered RMarkdown HTML report.
*   **Memory:** For very large datasets with `diagnostics=TRUE`, `combine_projection_diagnostics` can be configured to write per-searchlight diagnostics to disk (e.g., using `fst` or HDF5 via `DelayedArray` backends) instead of keeping all in RAM.

**7. Edge-Case Safeguards & User Experience:**

*   **`make_trialwise_X`**: `max_X_cols` warning.
*   **`build_projector`**: `cond(R)` warning if high.
*   **Adaptive `λ`**: Stratified K-fold for `"LOOcv_local"`. Cache `crossprod(R)`.
*   **Joint `θ` & `w`**: Iteration budget for `w` optimization. TMB `isSparse` checks if used.
*   **PP Pursuit**: Fallback for missing classes in test folds.
*   **Tiny TR/Long HRF (`L` large)**: `make_trialwise_X` or `hrf_basis_func` could warn or offer to decimate `B(θ)` post-convolution.
*   **Lifecycle Badges**: Use `lifecycle::badge("experimental")` for advanced/new features (`optim` collapse, `LOOcv_local` lambda, `optimize_joint_hrf_mvpa`).
*   **Cheat-Sheet PDF & Benchmark Script**: As planned.
*   **Pre-print Name**: "SPRINT-MVPA" or similar.

**8. Implementation Roadmap:**

1.  **Week 1-2 (Core `fmriproj` Scaffolding & Layer 1-2):**
    *   `fmriproj` package structure, RcppArmadillo helpers for sparse `X` construction and basic projection.
    *   `make_trialwise_X()`.
    *   `build_projector()`.
    *   `adaptive_ridge_projector()` with `method="none"` and `method="EB"`.
    *   Basic unit tests for matrix dimensions and simple cases.
2.  **Week 3 (Layer 3 - Collapse & Basic Searchlight):**
    *   `collapse_beta()` with `method="rss"` and `method="pc"`.
    *   `mvpa_projected_searchlight()` one-liner (fixed `θ`, no `w` optim, no PP).
    *   Integration with `rMVPA::run_searchlight` (passing a `FUN` that calls `adaptive_ridge_projector` and `collapse_beta`).
    *   Benchmark against LSS (Speed-only profile).
3.  **Week 4-5 (Advanced Collapse & Adaptive Lambda, Diagnostics):**
    *   `collapse_beta()` with `method="optim"`.
    *   `adaptive_ridge_projector()` with `method="LOOcv_local"` (stratified K-fold).
    *   Implement `diagnostics=TRUE` flag and `combine_projection_diagnostics`.
    *   Start `explain_projection_results()` for basic maps (lambda, w-components).
4.  **Week 6-7 (Joint `θ` Optimization & PP):**
    *   `optimize_joint_hrf_mvpa()` structure with numerical gradients for `θ`.
    *   (Optional, if time/need permits) TMB integration for analytic `θ` gradients.
    *   `fit_pp()` and `predict_pp()`. Integrate PP as an `rMVPA` preproc hook.
5.  **Week 8+ (Vignettes, Full Benchmarking, Polish):**
    *   "From GLM to single-pass" vignette.
    *   Full benchmark grid automation.
    *   Complete `explain_projection_results()`.
    *   Cheat-sheet and pre-print preparation.

This phased approach allows for early release of core speed benefits, with more advanced (and potentially complex) features rolled out progressively. The focus on modularity and diagnostics should make it a powerful yet understandable tool for the neuroimaging community.