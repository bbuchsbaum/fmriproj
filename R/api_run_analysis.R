#' Run MVPA searchlight on time-series data
#'
#' High-level wrapper that automatically builds the necessary
#' `rMVPA` objects and connects them to the fmriproj projection pipeline.
#' See the vignette "Running rMVPA Analyses on Time-Series Data" for a
#' complete walkthrough.
#'
#' @param Y Time-series matrix (time x voxels)
#' @param event_model Event model list describing trial onsets and labels
#' @param mask Brain mask for searchlight centers
#' @param radius Searchlight radius in voxels
#' @param classifier Name of classifier model to load with `rMVPA::load_model`
#' @param projection_opts List of options passed to the projection step
#' @param cross_validation Optional rMVPA cross-validation object
#' @param ... Additional arguments passed to `rMVPA::run_searchlight`
#'
#' @return An object of class `searchlight_result`
#' @export
run_searchlight <- function(Y,
                            event_model,
                            mask,
                            radius = 3,
                            classifier = "sda_notune",
                            projection_opts = list(),
                            cross_validation = NULL,
                            ...) {
  if (!requireNamespace("rMVPA", quietly = TRUE)) {
    stop("rMVPA package required. Install with: devtools::install_github('bbuchsbaum/rMVPA')")
  }

  proj_defaults <- list(
    lambda_adaptive_method = "EB",
    collapse_method = "rss",
    lambda_global = 0.1
  )
  proj_opts <- modifyList(proj_defaults, projection_opts)

  design_proj <- build_design_matrix(event_model)
  proj_comp <- build_projector(design_proj$X, lambda_global = proj_opts$lambda_global)

  spec <- projection_spec(
    event_model = event_model,
    projector_components = proj_comp,
    N_trials = length(event_model$onsets),
    K_hrf = ncol(design_proj$hrf_info$basis),
    lambda_adaptive_method = proj_opts$lambda_adaptive_method,
    lambda_global = proj_opts$lambda_global,
    collapse_method = proj_opts$collapse_method
  )

  sl_fun <- make_rmvpa_searchlight_fun(spec, return_format = "matrix")

  dataset <- rMVPA::mvpa_dataset(Y, mask)
  dataset <- wrap_as_projecting_dataset(Y, sl_fun, dataset)

  design <- rMVPA::mvpa_design(
    y_train = event_model$conditions,
    block_var = event_model$blocks
  )

  if (is.null(cross_validation)) {
    if (!is.null(event_model$blocks)) {
      cross_validation <- rMVPA::blocked_cross_validation(event_model$blocks)
    } else {
      cross_validation <- rMVPA::kfold_cross_validation(length(event_model$conditions), nfolds = 5)
    }
  }

  model <- rMVPA::mvpa_model(
    model = rMVPA::load_model(classifier),
    dataset = dataset,
    design = design,
    crossval = cross_validation
  )

  rMVPA::run_searchlight(model, radius = radius, ...)
}

#' Run MVPA on a single region of interest
#'
#' This function mirrors `run_searchlight()` but operates on a predefined
#' region mask instead of iterating searchlights. See the vignette
#' "Running rMVPA Analyses on Time-Series Data" for usage examples.
#'
#' @inheritParams run_searchlight
#' @param region_mask Logical vector or matrix defining the region of interest
#' @return An object of class `regional_mvpa_result`
#' @export
run_regional <- function(Y,
                         event_model,
                         region_mask,
                         classifier = "sda_notune",
                         projection_opts = list(),
                         cross_validation = NULL,
                         ...) {
  if (!requireNamespace("rMVPA", quietly = TRUE)) {
    stop("rMVPA package required. Install with: devtools::install_github('bbuchsbaum/rMVPA')")
  }

  proj_defaults <- list(
    lambda_adaptive_method = "EB",
    collapse_method = "rss",
    lambda_global = 0.1
  )
  proj_opts <- modifyList(proj_defaults, projection_opts)

  design_proj <- build_design_matrix(event_model)
  proj_comp <- build_projector(design_proj$X, lambda_global = proj_opts$lambda_global)

  spec <- projection_spec(
    event_model = event_model,
    projector_components = proj_comp,
    N_trials = length(event_model$onsets),
    K_hrf = ncol(design_proj$hrf_info$basis),
    lambda_adaptive_method = proj_opts$lambda_adaptive_method,
    lambda_global = proj_opts$lambda_global,
    collapse_method = proj_opts$collapse_method
  )

  reg_fun <- make_rmvpa_searchlight_fun(spec, return_format = "matrix")

  dataset <- rMVPA::mvpa_dataset(Y, region_mask)
  dataset <- wrap_as_projecting_dataset(Y, reg_fun, dataset)

  design <- rMVPA::mvpa_design(
    y_train = event_model$conditions,
    block_var = event_model$blocks
  )

  if (is.null(cross_validation)) {
    if (!is.null(event_model$blocks)) {
      cross_validation <- rMVPA::blocked_cross_validation(event_model$blocks)
    } else {
      cross_validation <- rMVPA::kfold_cross_validation(length(event_model$conditions), nfolds = 5)
    }
  }

  model <- rMVPA::mvpa_model(
    model = rMVPA::load_model(classifier),
    dataset = dataset,
    design = design,
    crossval = cross_validation
  )

  rMVPA::run_regional(model, ...)
}
