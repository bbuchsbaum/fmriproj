This is an excellent integration plan. It correctly identifies the core strategy: **Data-Level Interception** via a "Projecting Dataset" pattern. This is a powerful and non-invasive way to make `fmriproj` and `rMVPA` work together.

Your plan and my initial proposal are highly convergent. Your plan provides a strong design philosophy and excellent concrete examples (like the Progressive Projection feature selector), while my plan focuses on actionable tickets and refactoring the existing codebase.

Let's integrate them into a single, comprehensive plan that takes the best of both.

### Integrated `fmriproj` + `rMVPA` Plan

#### **Overall Strategy (from your plan, slightly refined):**

The integration will be achieved by making `fmriproj` "speak rMVPA's language" without modifying the `rMVPA` package. The key mechanism is a **"Projecting Dataset" adapter**. This object will present itself to `rMVPA` as a standard `mvpa_dataset`, but when `rMVPA` requests data for an ROI or searchlight, the adapter will intercept the call, perform the on-the-fly time-series projection using `fmriproj`'s pipeline, and return the resulting trial-wise patterns. This approach is efficient and ensures full compatibility with all of `rMVPA`'s models and utilities.

---

### Actionable Implementation Tickets (Integrated Plan)

Here are four precise tickets that combine the strengths of both plans.

---

#### **Ticket 1: Refactor and Solidify the Core Projection Infrastructure**

*   **Goal:** Create a clean, single, robust internal mechanism for on-the-fly projection, based on the best ideas from both plans.
*   **Problem:** The current `fmriproj` codebase has two competing implementations for searchlight integration. This needs to be consolidated.
*   **Implementation Plan:**
    1.  **Adopt the "Projecting Dataset" Pattern:** Fully commit to the `wrap_as_projecting_dataset` function from `rmvpa_wrappers.R` as the central integration pattern. This is the key insight from your plan.
    2.  **Consolidate Code:**
        *   Deprecate and **remove** the file `R/mvpa_projected_searchlight.R`. Its functionality is redundant and less integrated than the wrapper approach.
        *   Rename the existing `rmvpa_wrappers.R` to `rMVPA_integration.R` to better reflect its purpose.
    3.  **Encapsulate Projection Parameters (My Plan):** Create a simple `projection_spec()` constructor that bundles all parameters needed for projection (`event_model`, `projector_components`, `collapse_method`, `lambda_adaptive_method`, etc.) into a single object.
    4.  **Refine the Adapter (Your Plan):** Refactor `make_rmvpa_searchlight_fun` to accept the new `projection_spec` object. This makes the code cleaner and easier to maintain. This function's role is to return a closure that `wrap_as_projecting_dataset` can use.

*   **Acceptance Criteria:**
    *   The `R/mvpa_projected_searchlight.R` file is removed.
    *   The `wrap_as_projecting_dataset` pattern is the sole mechanism for data-level interception.
    *   All parameters needed for projection are passed cleanly via a `projection_spec` object.

---

#### **Ticket 2: Implement High-Level `run_searchlight` and `run_regional` API**

*   **Goal:** Provide simple, powerful, user-facing functions that hide the internal complexity of both `fmriproj` and `rMVPA` object creation.
*   **Problem:** The user needs an entry point that takes raw time-series data (`Y`) and an `event_model`, not a pre-constructed `rMVPA::mvpa_model`.
*   **Implementation Plan:**
    1.  **Create `fmriproj::run_searchlight()`:** This function, located in a user-facing API file (e.g., `api_run_analysis.R`), will have the signature: `run_searchlight(Y, event_model, mask, radius, classifier, projection_opts, ...)`.
    2.  **Create `fmriproj::run_regional()`:** Implement the missing regional analysis function with a parallel signature: `run_regional(Y, event_model, region_mask, classifier, projection_opts, ...)`.
    3.  **Internal Logic (for both functions):**
        *   Accept `Y` (time-series), `event_model`, and analysis parameters.
        *   Automatically build the `fmriproj` projector components from the `event_model` and `projection_opts`.
        *   Create the `projection_spec` object (from Ticket 1).
        *   Automatically construct the necessary `rMVPA` objects (`mvpa_dataset` using the raw `Y`, `mvpa_design` from the `event_model`, and `mvpa_model`).
        *   Wrap the `mvpa_dataset` using the "Projecting Dataset" pattern (`wrap_as_projecting_dataset`).
        *   Call the appropriate `rMVPA` engine (`rMVPA::run_searchlight` or `rMVPA::run_regional`) with the fully constructed and wrapped `mvpa_model`.
*   **Acceptance Criteria:**
    *   A user can run a complete searchlight or regional analysis from a time-series with a single function call.
    *   The creation of `fmriproj` components and `rMVPA` objects is completely abstracted from the user.
    *   The return values are standard `searchlight_result` and `regional_mvpa_result` objects.

---

#### **Ticket 3: Integrate Progressive Projection as an `rMVPA` Feature Selector**

*   **Goal:** Allow `fmriproj`'s progressive projection to be used as a seamless feature selection step within any `rMVPA` analysis.
*   **Problem:** Progressive Projection (PP) is a powerful feature reduction technique that needs a standard way to plug into the `rMVPA` pipeline.
*   **Implementation Plan (from your plan, which is excellent):**
    1.  Implement the `pp_feature_selector` function exactly as you designed it. It will conform to the `rMVPA` `feature_selector` class structure.
    2.  The `select_features` method within this object will not filter features but will instead:
        *   Fit the `fmriproj::fit_pp` model.
        *   Attach the fitted PP model as an attribute to the data matrix `X`.
        *   Return `rep(TRUE, ncol(X))` to signal that all features are "kept".
    3.  Modify the `rMVPA` `train_model` wrapper within `fmriproj` to check for this attribute. If `attr(X, "projection_function")` exists, it applies the projection function to the data *before* passing it to the final classifier.
*   **Acceptance Criteria:**
    *   A user can specify `feature_selector = pp_feature_selector(...)` in the `mvpa_model` creation step.
    *   When the model is run, the data for each searchlight/ROI is first reduced in dimensionality via Progressive Projection before being passed to the classifier.
    *   This works with both `run_searchlight` and `run_regional`.

---

#### **Ticket 4: Finalize API and Create Comprehensive Documentation**

*   **Goal:** Ensure the integrated workflow is easy to understand and use.
*   **Problem:** A powerful but undocumented API is not useful. The new, streamlined workflow needs to be clearly explained.
*   **Implementation Plan:**
    1.  **Deprecate `project_trials`:** The high-level `project_trials` function in `user_friendly_wrappers.R` becomes somewhat redundant once `run_regional` is implemented (a single-ROI regional analysis achieves the same result). Mark it as a lower-level helper or deprecate it in favor of the more powerful `run_regional`.
    2.  **Create a Vignette:** Write a new package vignette titled "Running `rMVPA` Analyses on Time-Series Data".
    3.  **Vignette Content:**
        *   Start with the simple usage patterns you outlined, showing how to go from `Y` and `event_model` to a full searchlight result.
        *   Provide clear examples for `run_searchlight` and `run_regional`.
        *   Include an example of using the `projection_opts` list to control regularization and beta collapse methods.
        *   Show an advanced example using Progressive Projection as a feature selector.
        *   Explain how to interpret the results and use the diagnostic functions.
    4.  **Update Function Documentation:** Ensure the roxygen documentation for `run_searchlight` and `run_regional` is comprehensive and user-friendly.

*   **Acceptance Criteria:**
    *   The package has a clear, discoverable API for time-series-based MVPA.
    *   A new vignette demonstrates the entire workflow from start to finish.
    *   The README is updated to point users to the new high-level functions.