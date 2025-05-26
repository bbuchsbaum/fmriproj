Okay, here's a "précis" for both `fmrireg` and `rMVPA`, tailored for an engineer implementing `fmriproj` and needing to interface with these packages.

---

**Précis: `fmrireg` for `fmriproj` Implementation**

**Core Purpose of `fmrireg` (from `fmriproj`'s perspective):**
`fmrireg` is the upstream engine that defines *what* experimental events occurred, *when* they occurred, and *how* they should initially be translated into time-varying regressors *before* any voxel-specific data is considered. `fmriproj` will consume the structured output of `fmrireg`'s event modeling to build its initial large design matrix `X(θ)`.

**Key `fmrireg` Concepts & Objects for `fmriproj`:**

1.  **`event_model` Object:**
    *   **What it is:** The central output of `fmrireg` that `fmriproj` will take as input. It's an S3 list object created by `fmrireg::event_model(...)`.
    *   **How it's made:** Users define it with a formula (e.g., `onsets ~ hrf(condition, RT) + trialwise(stim_type)`) and a `data.frame` containing columns for `onsets` (numeric, in seconds, relative to run start), `condition` (factor), `RT` (numeric modulator), `stim_type` (factor), and a `run` or `block` identifier.
    *   **Key Components for `fmriproj`:**
        *   `$model_spec$event_table`: A `tibble` that is essentially the user's input `data` after some processing (e.g., subsetting, block ID canonicalization). This contains the raw event information (onsets, durations, modulator values, condition labels) for *all trials across all runs*.
        *   `$sampling_frame`: An object (class `sampling_frame`) detailing the acquisition timing:
            *   `$TR`: Repetition Time (seconds).
            *   `$blocklens`: Numeric vector of scan counts per run.
            *   `$blockids`: A vector mapping each original event row to its run/block ID.
        *   `$terms`: A list of `event_term` objects, one for each term in the `event_model` formula (e.g., one for `hrf(condition, RT)`, one for `trialwise(stim_type)`). `fmriproj`'s `make_trialwise_X` will likely iterate through these terms or use the overall `event_table` and a unified `trialwise` approach specified by `fmriproj`.
        *   Information about parametric modulators and how they apply to specific conditions/trials.

2.  **`hrf()` and `trialwise()` Functions (within `fmrireg::event_model` formula):**
    *   **`hrf(...)`:** Specifies how a group of events (e.g., all "face" trials) or a parametric modulator should be modeled. Can take a `basis` argument (e.g., `HRF_SPMG1`, `HRF_SPMG3`, or a custom HRF function).
    *   **`trialwise(...)`:** A crucial function for `fmriproj`. It signals that *each individual event/trial* should get its own unique regressor (or set of `K_hrf_bases` regressors).
        *   `fmriproj::make_trialwise_X` will effectively ensure that every trial in the `event_table` is treated as if specified via `trialwise()` with the chosen `hrf_basis_func` (from `fmriproj`) and `theta_params`.

3.  **HRF Basis Functions (e.g., `HRF_SPMG1`, `HRF_SPMG3`, `fmrireg::hrf_gaussian`):**
    *   These are R functions that take a time vector (and potentially other parameters like `theta_params`) and return an `L x K_hrf_bases` matrix representing the HRF shape(s).
    *   `fmriproj` will need to call the user-specified `hrf_basis_func` (e.g., one that produces canonical + derivatives) with the current `theta_params` to get the `B(θ)` matrix for convolution.

4.  **Convolution (Implicitly handled by `fmriproj::make_trialwise_X`):**
    *   `fmrireg` internally handles convolving stick functions (derived from onsets in `event_table`) with HRF basis functions.
    *   `fmriproj::make_trialwise_X` will replicate this logic: for each trial, it takes its onset, duration (if any), and parametric modulator values, creates an appropriately scaled "neural event" representation, and convolves it with each of the `K_hrf_bases` from `B(θ)`.

**Interface Points for `fmriproj`:**

*   **Primary Input:** The `fmrireg_event_model` object.
*   **`fmriproj::make_trialwise_X` will need to:**
    1.  Access `fmrireg_event_model$model_spec$event_table` to get per-trial information (onsets, durations, condition labels, modulator values).
    2.  Access `fmrireg_event_model$sampling_frame` for `TR` (to define the time grid for `B(θ)` and `X`) and `blocklens` (to know the total time `T` per run if processing run-by-run, though `X(θ)` is often built for all runs concatenated).
    3.  Use the provided `hrf_basis_func` and `theta_params` to generate `B(θ)`.
    4.  Perform the convolution of each trial's (potentially modulated) stick function with each column of `B(θ)`.
    5.  Assemble the results into the sparse `X(θ)` matrix.

**Key Takeaway for `fmriproj` Engineer regarding `fmrireg`:**
`fmrireg` provides a rich, validated specification of the experimental design. Your `make_trialwise_X` function needs to parse this specification to correctly construct the columns of `X(θ)` by:
    a. Identifying each unique trial/event instance.
    b. Determining its onset and duration.
    c. Applying any specified parametric modulations (e.g., from RT) to scale the HRF for that trial.
    d. Convolving this trial-specific (modulated) "neural event" with each of the `K` HRF basis functions derived from the *global* `θ` (or a fixed `hrf_basis_matrix`).

---

**Précis: `rMVPA` for `fmriproj` Implementation**

**Core Purpose of `rMVPA` (from `fmriproj`'s perspective):**
`rMVPA` is the downstream engine that takes the trial-specific feature patterns (`A_sl` matrix, `N_trials x V_features`) generated by `fmriproj` for each searchlight/ROI and performs the actual multivariate analysis (classification, RSA, etc.). It handles cross-validation, model training on features, prediction, and performance metric calculation.

**Key `rMVPA` Concepts & Objects for `fmriproj`:**

1.  **`mvpa_dataset` Object:**
    *   **What it is:** Contains the raw 4D fMRI data (`train_data`, possibly `test_data`) and the `mask`.
    *   **`fmriproj` Interaction:** `fmriproj`'s top-level functions (like `mvpa_projected_searchlight` or `optimize_joint_hrf_mvpa`) will take the `mvpa_dataset` as input to access the 4D BOLD data (`Y4d`) and the `mask`. The `grab_block` helper within `fmriproj` will use these to extract `Y_sl` for each searchlight.

2.  **`mvpa_design` Object:**
    *   **What it is:** Contains trial labels (`y_train`, `y_test`), block/run information for cross-validation (`block_var`), and potentially other per-trial metadata.
    *   **`fmriproj` Interaction:**
        *   `fmriproj` (specifically `optimize_joint_hrf_mvpa`) needs `labels_train` and `fold_definition_for_theta_optim` (which can be derived from `mvpa_design$block_var` or a `cv_spec` object) to compute its internal cross-validated loss for `θ` optimization.
        *   If `collapse_beta(method="optim", ...)` is used, `fmriproj` needs `labels_for_w_optim` (likely `mvpa_design$y_train` restricted to the current searchlight's trials and CV fold) for the supervised `w` optimization.

3.  **`mvpa_model` Object (Classifier/RSA Spec):**
    *   **What it is:** Specifies the MVPA technique to be applied (e.g., SVM, LDA, correlation-based RSA), its parameters, the cross-validation scheme, and performance metrics.
    *   **`fmriproj` Interaction:**
        *   `fmriproj::optimize_joint_hrf_mvpa` requires an `rMVPA_classifier_model_for_loss` (an `mvpa_model` object) and an `inner_cv_spec_for_loss` to evaluate the quality of `A_sl` derived from a given `θ`.
        *   The final `rMVPA::run_searchlight` call (orchestrated by `fmriproj::mvpa_projected_searchlight`) will use a user-provided `rMVPA_classifier_model`.

4.  **`run_searchlight()` / `run_regional()` (from `rMVPA`):**
    *   **Core Iterators:** These `rMVPA` functions manage the iteration over searchlights or ROIs.
    *   **`FUN` Argument:** The key integration point. `fmriproj` will provide a custom `FUN` to `rMVPA::searchlight`. This `FUN` will encapsulate Layers 2 and 3 of the `fmriproj` stack (`adaptive_ridge_projector` and `collapse_beta`).
        *   This `FUN` will take `Y_sl_from_rMVPA` (the raw BOLD data for the current sphere, provided by `rMVPA`'s iterator) and `sl_info_from_rMVPA`.
        *   It will use the globally computed `projector_components_final` (derived from `X(θ_optimal)`) and `fmriproj`'s projection/collapse functions to produce `A_sl`.
        *   It then returns `A_sl` (N_trials x V_sl).
    *   **`rMVPA`'s internal CV loop then calls:**
        *   `train_model(classifier_spec, train_dat = A_sl_train_fold, y = labels_train_fold, ...)`
        *   `predict_model(trained_classifier, test_dat = A_sl_test_fold, ...)`
        *   `performance(...)`

5.  **Cross-Validation Objects (e.g., `blocked_cross_validation`):**
    *   Defined in `rMVPA`. Used by `fmriproj::optimize_joint_hrf_mvpa` for its internal loss calculation, and by the final `rMVPA::run_searchlight` call for the main MVPA.

6.  **Pre-processing Hooks (e.g., for Progressive Projection Pursuit):**
    *   If `fmriproj::fit_pp` is to be used, `rMVPA`'s `mvpa_model` can be configured with a `preproc` argument that points to a function wrapping `fit_pp` and `predict_pp`. `rMVPA` would call this on `A_sl` before `train_model`.

7.  **Combiner Function (`.combine` argument in `rMVPA::run_searchlight`):**
    *   If `diagnostics=TRUE` in `fmriproj`, `rMVPA` needs to use `fmriproj::combine_projection_diagnostics` as its `.combine` function to correctly aggregate both standard performance metrics and the rich diagnostic information from `fmriproj`.

**Interface Points for `fmriproj` with `rMVPA`:**

*   `fmriproj`'s top-level functions (`mvpa_projected_searchlight`, `optimize_joint_hrf_mvpa`) will *call* `rMVPA::run_searchlight` (or `rMVPA::run_regional`) internally, providing a custom `FUN` that executes the projection and collapse steps.
*   The `A_sl` matrix produced by `fmriproj`'s `FUN` becomes the primary data input for `rMVPA`'s `train_model` method for the chosen classifier.
*   `fmriproj::optimize_joint_hrf_mvpa` will internally use `rMVPA`'s CV mechanisms and classifiers to define its loss function based on the performance on `A_sl(θ)`.
*   `fmriproj` will provide `combine_projection_diagnostics` to `rMVPA` for aggregating rich diagnostic outputs.

**Key Takeaway for `fmriproj` Engineer regarding `rMVPA`:**
`rMVPA` provides the outer loop for iterating over brain locations (searchlights/ROIs) and for performing cross-validated model training/testing *on the feature patterns (`A_sl`) you provide*. `fmriproj`'s job is to efficiently generate these high-quality `A_sl` patterns for each location. Ensure the `FUN` you pass to `rMVPA::searchlight` correctly implements the `fmriproj` Layers 2 & 3 using the globally optimized `X(θ)` (from Layer 1) and returns an `A_sl` matrix with rows corresponding to trials and columns to features (voxels in `A_sl`, or reduced dimensions if PP applied *before* returning to `rMVPA`).

---