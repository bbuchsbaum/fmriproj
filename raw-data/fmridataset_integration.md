Excellent. Acknowledging the prior refactoring and now moving to integrate `fmridataset` is the perfect next step. This sprint will create a much cleaner and more powerful user experience.

The key is to leverage `fmridataset` as the primary data container and use `rMVPA`'s `mvpa_iterate` and `run_searchlight`/`run_regional` infrastructure as the backend engine. `fmriproj`'s role is to provide the "glue" that projects the time-series data on-the-fly.

Here is a set of tickets for **Sprint 1: `fmriproj` integration with `fmridataset`**.

---

### **Sprint 1: `fmridataset` Integration**

*   **Sprint Goal:** Refactor `fmriproj` to use `fmridataset` as the primary input for `run_searchlight` and `run_regional` analyses, creating a seamless and powerful user workflow.
*   **Core Strategy:** The user provides a single `fmridataset` object. `fmriproj` extracts the time-series (`NeuroVec`), the event information (`event_table` and `sampling_frame`), and the mask from this object. It then uses the "Projecting Dataset" adapter to feed on-the-fly projected trial patterns into `rMVPA`'s analysis engine.

---

### **Ticket 1: Create the `fmridataset` -> `rMVPA` Bridge**

*   **Title:** Implement `fmridataset` to `rMVPA` Design and Data Adapters
*   **Problem:** The `run_searchlight` and `run_regional` functions in `fmriproj` need to translate a single `fmridataset` object into the multiple objects that `rMVPA` expects (`mvpa_dataset`, `mvpa_design`).
*   **Implementation Plan:**
    1.  **Create `create_mvpa_design_from_dataset()`:**
        *   This will be an internal helper function: `create_mvpa_design_from_dataset(fmri_dset, y_formula, block_formula)`.
        *   It will take an `fmridataset` object and user-provided formulas.
        *   It will extract the `event_table` from `fmri_dset$event_table`.
        *   It will use the formulas to parse the relevant columns from the `event_table` (e.g., `y_train` from `y_formula`, `block_var` from `block_formula`).
        *   It will call `rMVPA::mvpa_design()` with these extracted vectors and return a valid `mvpa_design` object.
    2.  **Create `create_mvpa_dataset_from_dataset()`:**
        *   This will be an internal helper function: `create_mvpa_dataset_from_dataset(fmri_dset)`.
        *   It will extract the time-series data using `fmridataset::get_data(fmri_dset)` to get the `NeuroVec`.
        *   It will extract the mask using `fmridataset::get_mask(fmri_dset)`.
        *   It will call `rMVPA::mvpa_dataset()` with the `NeuroVec` and `mask` to construct and return a valid `mvpa_dataset` object.
*   **Acceptance Criteria:**
    *   Two internal helper functions exist that can reliably convert an `fmridataset` into `rMVPA::mvpa_design` and `rMVPA::mvpa_dataset` objects.
    *   The functions correctly handle formula-based column selection from the `event_table`.

---

### **Ticket 2: Refactor `fmriproj::run_searchlight` to use `fmridataset`**

*   **Title:** Update `run_searchlight` to Accept an `fmridataset` Object
*   **Problem:** The existing `run_searchlight` API is based on separate `Y` and `event_model` inputs. It needs to be updated to use the superior `fmridataset` structure.
*   **Implementation Plan:**
    1.  Change the signature of `fmriproj::run_searchlight` in `api_run_analysis.R`:
        ```R
        run_searchlight(fmri_dset, radius, y_formula, block_formula,
                        classifier = "sda_notune", projection_opts = list(), ...)
        ```
    2.  Update the function's internal logic:
        *   Perform an `inherits(fmri_dset, "fmri_dataset")` check.
        *   Use the new helper functions from **Ticket 1** to create the `mvpa_dataset` and `mvpa_design` objects.
        *   Extract the `sampling_frame` and `event_table` from `fmri_dset` to create the `event_model` needed for `build_design_matrix`.
        *   Build the `projection_spec` object (from `projection_spec.R`).
        *   Create the `rMVPA::mvpa_model` object.
        *   Wrap the `mvpa_dataset` using the "Projecting Dataset" adapter (`wrap_as_projecting_dataset`).
        *   Call `rMVPA::run_searchlight` with the fully constructed and wrapped `mvpa_model`.
*   **Acceptance Criteria:**
    *   A user can run a full searchlight analysis by passing a single `fmridataset` object.
    *   The function correctly uses the `event_table` and `sampling_frame` from the dataset.
    *   Tests are updated to reflect the new `fmridataset`-centric API.

---

### **Ticket 3: Implement `fmriproj::run_regional` for `fmridataset`**

*   **Title:** Implement a `run_regional` API that Accepts an `fmridataset`
*   **Problem:** A `run_regional` function that works with `fmridataset` and on-the-fly projection does not yet exist.
*   **Implementation Plan:**
    1.  Create the user-facing `fmriproj::run_regional` function in `api_run_analysis.R` with the signature:
        ```R
        run_regional(fmri_dset, region_mask, y_formula, block_formula,
                     classifier = "sda_notune", projection_opts = list(), ...)
        ```
    2.  The internal logic will mirror the refactored `run_searchlight` from **Ticket 2**:
        *   Check that `fmri_dset` is a valid `fmridataset` and `region_mask` is a valid `NeuroVol`/`NeuroSurface`.
        *   Use the helper functions from **Ticket 1** to create the `rMVPA` objects (`mvpa_dataset`, `mvpa_design`).
        *   Build the `fmriproj` projection components (`projection_spec`).
        *   Construct the `rMVPA::mvpa_model`.
        *   Wrap the `mvpa_dataset` using the "Projecting Dataset" adapter.
        *   Call `rMVPA::run_regional` with the wrapped `mvpa_model` and the `region_mask`. The `rMVPA` backend will handle iterating through the ROIs.
*   **Acceptance Criteria:**
    *   A user can run a full regional analysis by passing an `fmridataset` and a `region_mask`.
    *   The function efficiently projects data only for the voxels within each ROI, one ROI at a time.
    *   The return value is a standard `regional_mvpa_result` object.

---

#### **Ticket 4: Update Documentation and Examples for the `fmridataset` Workflow**

*   **Goal:** Ensure the new, streamlined workflow is clearly documented for users.
*   **Problem:** All existing documentation and examples are based on the old `Y` + `event_model` paradigm and need to be updated.
*   **Implementation Plan:**
    1.  **Deprecate Old Wrappers:** Mark the old `project_trials` and `mvpa_searchlight` functions in `user_friendly_wrappers.R` as `@deprecated`, pointing users to the new `run_regional` and `run_searchlight` functions.
    2.  **Update Function Documentation:** Update the roxygen documentation for `run_searchlight` and `run_regional` to reflect the `fmridataset` input. Provide clear `@examples` that start with `matrix_dataset(...)` or `fmri_mem_dataset(...)`.
    3.  **Create a "Getting Started" Vignette:**
        *   Title: "A Unified Workflow: From `fmridataset` to MVPA Results".
        *   Demonstrate the creation of an `fmridataset` object from a matrix or `NeuroVec`.
        *   Provide a complete, copy-pasteable example for running `run_searchlight` and `run_regional` using the created `fmridataset`.
        *   Explain how to use formulas (`y_formula`, `block_formula`) to specify the design from the `event_table`.
*   **Acceptance Criteria:**
    *   The primary user-facing API is clearly centered on the `fmridataset` object.
    *   All examples and documentation reflect this new, cleaner workflow.
    *   A user can easily follow the vignette to perform a complete analysis.