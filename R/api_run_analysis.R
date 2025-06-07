#' Run MVPA searchlight on an fmridataset
#'
#' High-level wrapper that automatically builds the necessary
#' `rMVPA` objects and connects them to the fmriproj projection pipeline.
#' See the vignette "Running rMVPA Analyses on Time-Series Data" for a
#' complete walkthrough.
#'
#' @param fmri_dset An object of class `fmri_dataset` containing the
#'   time-series, mask, event table, and sampling frame.
#' @param radius Searchlight radius in voxels
#' @param y_formula Formula selecting the label column from `event_table`.
#' @param block_formula Formula selecting the block/run column.
#' @param classifier Name of classifier model to load with `rMVPA::load_model`
#' @param projection_opts List of options passed to the projection step
#' @param ... Additional arguments passed to `rMVPA::run_searchlight`
#'
#' @return An object of class `searchlight_result`
#' @export
run_searchlight <- function(fmri_dset,
                            radius = 3,
                            y_formula,
                            block_formula,
                            classifier = "sda_notune",
                            projection_opts = list(),
                            ...) {
  if (!requireNamespace("rMVPA", quietly = TRUE)) {
    stop("rMVPA package required. Install with: devtools::install_github('bbuchsbaum/rMVPA')")
  }

  if (!inherits(fmri_dset, "fmri_dataset")) {
    stop("fmri_dset must be an 'fmri_dataset'")
  }

  proj_defaults <- list(
    lambda_adaptive_method = "EB",
    collapse_method = "rss",
    lambda_global = 0.1
  )
  proj_opts <- modifyList(proj_defaults, projection_opts)

  Y <- fmridataset::get_data(fmri_dset)
  event_table <- fmri_dset$event_table
  sampling_frame <- fmri_dset$sampling_frame

  if (is.null(event_table) || is.null(sampling_frame)) {
    stop("fmri_dset must contain an event_table and sampling_frame")
  }

  y_vals <- model.frame(y_formula, event_table, drop.unused.levels = TRUE)[[1]]
  block_vals <- model.frame(block_formula, event_table, drop.unused.levels = TRUE)[[1]]

  event_model <- list(
    onsets = event_table$onset,
    n_time = sum(sampling_frame$blocklens),
    conditions = y_vals,
    blocks = block_vals
  )

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

  dataset <- create_mvpa_dataset_from_dataset(fmri_dset)
  dataset <- wrap_as_projecting_dataset(Y, sl_fun, dataset)

  design <- create_mvpa_design_from_dataset(fmri_dset, y_formula, block_formula)

  if (!is.null(block_vals)) {
    cross_validation <- rMVPA::blocked_cross_validation(block_vals)
  } else {
    cross_validation <- rMVPA::kfold_cross_validation(length(y_vals), nfolds = 5)
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
#' @param fmri_dset An `fmri_dataset` containing time-series, mask and events
#' @param region_mask A `NeuroVol` or `NeuroSurface` defining the region of interest
#' @param y_formula Formula selecting the label column from `event_table`.
#' @param block_formula Formula selecting the block/run column.
#' @param classifier Name of classifier model to load with `rMVPA::load_model`
#' @param projection_opts List of options passed to the projection step
#' @param ... Additional arguments passed to `rMVPA::run_regional`
#'
#' @return An object of class `regional_mvpa_result`
#' @export
run_regional <- function(fmri_dset,
                         region_mask,
                         y_formula,
                         block_formula,
                         classifier = "sda_notune",
                         projection_opts = list(),
                         ...) {
  if (!requireNamespace("rMVPA", quietly = TRUE)) {
    stop("rMVPA package required. Install with: devtools::install_github('bbuchsbaum/rMVPA')")
  }

  if (!inherits(fmri_dset, "fmri_dataset")) {
    stop("fmri_dset must be an 'fmri_dataset'")
  }

  if (!inherits(region_mask, c("NeuroVol", "NeuroSurface"))) {
    stop("region_mask must be a NeuroVol or NeuroSurface")
  }

  proj_defaults <- list(
    lambda_adaptive_method = "EB",
    collapse_method = "rss",
    lambda_global = 0.1
  )
  proj_opts <- modifyList(proj_defaults, projection_opts)

  Y <- fmridataset::get_data(fmri_dset)
  event_table <- fmri_dset$event_table
  sampling_frame <- fmri_dset$sampling_frame

  if (is.null(event_table) || is.null(sampling_frame)) {
    stop("fmri_dset must contain an event_table and sampling_frame")
  }

  y_vals <- model.frame(y_formula, event_table, drop.unused.levels = TRUE)[[1]]
  block_vals <- model.frame(block_formula, event_table, drop.unused.levels = TRUE)[[1]]

  event_model <- list(
    onsets = event_table$onset,
    n_time = sum(sampling_frame$blocklens),
    conditions = y_vals,
    blocks = block_vals
  )

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
    y_train = y_vals,
    block_var = block_vals
  )

  if (!is.null(block_vals)) {
    cross_validation <- rMVPA::blocked_cross_validation(block_vals)
  } else {
    cross_validation <- rMVPA::kfold_cross_validation(length(y_vals), nfolds = 5)
  }

  model <- rMVPA::mvpa_model(
    model = rMVPA::load_model(classifier),
    dataset = dataset,
    design = design,
    crossval = cross_validation
  )

  rMVPA::run_regional(model, ...)
}
