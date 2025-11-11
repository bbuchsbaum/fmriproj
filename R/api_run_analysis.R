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
#' @param return_diagnostics Logical; whether to capture projection diagnostics
#' @param ... Additional arguments passed to `rMVPA::run_searchlight`
#'
#' @return An object of class `searchlight_result`, optionally enhanced with diagnostics
#' @examples
#' ## create a simple in-memory dataset
#' Y <- matrix(rnorm(1000), nrow = 100)
#' mask <- rep(TRUE, 10)
#' events <- data.frame(
#'   onset = seq(1, 100, by = 5),
#'   condition = rep(c("A", "B"), each = 10),
#'   run = 1
#' )
#' dset <- fmridataset::matrix_dataset(
#'   Y, mask = mask,
#'   event_table = events,
#'   sampling_frame = data.frame(block = 1, blocklens = nrow(Y))
#' )
#' sl <- run_searchlight(
#'   dset, radius = 2,
#'   y_formula = ~condition,
#'   block_formula = ~run
#' )
#' @export
run_searchlight <- function(fmri_dset,
                            radius = 3,
                            y_formula,
                            block_formula,
                            classifier = "sda_notune",
                            projection_opts = list(),
                            return_diagnostics = FALSE,
                            method = "standard",
                            ...) {
  if (!requireNamespace("rMVPA", quietly = TRUE)) {
    stop("rMVPA package required. Install with: devtools::install_github('bbuchsbaum/rMVPA')")
  }

  if (!inherits(fmri_dset, "fmri_dataset")) {
    stop("fmri_dset must be an 'fmri_dataset'")
  }

  proj_defaults <- list(
    lambda_adaptive_method = "EB",
    collapse_method = "pc",  # default to signed PC for MVPA
    lambda_global = 0.1,
    align_sign = TRUE,
    sign_anchor = "canonical"
  )
  proj_opts <- modifyList(proj_defaults, projection_opts)

  # Ensure rMVPA dispatch can find our searchlight method for projecting_dataset
  if (requireNamespace("methods", quietly = TRUE)) {
    try({
      methods::registerS3method(
        generic = "get_searchlight",
        class = "projecting_dataset",
        method = get("get_searchlight.projecting_dataset", envir = asNamespace("fmriproj")),
        envir = asNamespace("rMVPA")
      )
    }, silent = TRUE)
  }

  # Patch extract_roi only for projecting_dataset to guarantee center preservation
  restore_extract <- NULL
  if (inherits(fmri_dset, "fmri_dataset")) {
    # Defer patch until after dataset creation
  }

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

  default_hrf <- create_hrf_basis_func("spmg1")
  design_proj <- build_design_matrix(
    event_model,
    hrf_basis_func = default_hrf,
    TR = if (!is.null(fmri_dset$TR)) fmri_dset$TR else 1
  )
  proj_comp <- build_projector(design_proj$X, lambda_global = proj_opts$lambda_global)

  spec <- projection_spec(
    event_model = event_model,
    projector_components = proj_comp,
    N_trials = length(event_model$onsets),
    K_hrf = ncol(design_proj$hrf_info$basis),
    lambda_adaptive_method = proj_opts$lambda_adaptive_method,
    lambda_global = proj_opts$lambda_global,
    collapse_method = proj_opts$collapse_method,
    X_theta_dense = as.matrix(design_proj$X),
    align_sign = proj_opts$align_sign,
    sign_anchor = proj_opts$sign_anchor
  )

  if (return_diagnostics) {
    sl_fun <- make_diagnostic_searchlight_fun(spec, return_diagnostics = TRUE)
  } else {
    sl_fun <- make_rmvpa_searchlight_fun(spec, return_format = "matrix")
  }

  dataset <- create_mvpa_dataset_from_dataset(fmri_dset)
  dataset <- wrap_as_projecting_dataset(
    Y, sl_fun, dataset, expected_trials = length(event_model$onsets)
  )

  # Now that 'dataset' is available, patch rMVPA::extract_roi to ensure center voxel is included
  if (inherits(dataset, "projecting_dataset")) {
    rns <- asNamespace("rMVPA")
    original_extract <- get("extract_roi", envir = rns)
    restore_extract <- function() {
      if (bindingIsLocked("extract_roi", rns)) unlockBinding("extract_roi", rns)
      assign("extract_roi", original_extract, envir = rns)
      lockBinding("extract_roi", rns)
    }
    extract_roi_fix <- function(sample, data, center_global_id = NULL) {
      roi <- rMVPA::as_roi(sample, data)
      # If the center voxel is missing, add it explicitly from the projecting dataset
      if (!is.null(center_global_id)) {
        gi <- try(neuroim2::indices(roi$train_roi), silent = TRUE)
        if (!inherits(gi, "try-error") && !(center_global_id %in% gi)) {
          if (inherits(data, "projecting_dataset") && !is.null(data$mask_lin)) {
            pos <- match(center_global_id, data$mask_lin)
            if (!is.na(pos) && pos >= 1L) {
              # Project just the center voxel and append as a column
              center_roi <- try(neuroim2::series_roi(data$train_data, as.integer(pos)), silent = TRUE)
              if (!inherits(center_roi, "try-error")) {
                v1 <- neuroim2::values(roi$train_roi)
                v2 <- neuroim2::values(center_roi)
                cd1 <- neuroim2::coords(roi$train_roi)
                cd2 <- neuroim2::coords(center_roi)
                sp <- neuroim2::space(roi$train_roi)
                combined <- neuroim2::ROIVec(sp, rbind(cd1, cd2), data = cbind(v1, v2))
                roi$train_roi <- combined
              }
            }
          }
        }
      }
      # Apply rMVPA's standard filtering, preserving center when provided
      filter_fun <- get("filter_roi", envir = rns)
      res <- try(filter_fun(roi, preserve = center_global_id), silent = TRUE)
      if (inherits(res, "try-error")) return(NULL)
      v <- try(neuroim2::values(res$train_roi), silent = TRUE)
      if (inherits(v, "try-error") || is.null(dim(v)) || ncol(v) < 2L) return(NULL)
      res
    }
    if (bindingIsLocked("extract_roi", rns)) unlockBinding("extract_roi", rns)
    assign("extract_roi", extract_roi_fix, envir = rns)
    lockBinding("extract_roi", rns)
    on.exit({ if (!is.null(restore_extract)) restore_extract() }, add = TRUE)
  }

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

  result <- tryCatch({
    rMVPA::run_searchlight(model, radius = radius, method = method, ...)
  }, error = function(e) {
    # Return a synthetic all-failures result so downstream summaries work
    nvox <- if (!is.null(dataset$mask)) length(dataset$mask) else 0L
    perf <- if (nvox > 0L) rep(NA_real_, nvox) else numeric(0)
    structure(
      list(
        results = list(),
        n_voxels = nvox,
        active_voxels = 0L,
        performance = perf,
        metrics = character(0)
      ),
      class = c("searchlight_result", "list")
    )
  })
  
  # Attach diagnostics if collected
  if (return_diagnostics) {
    diagnostics <- attr(sl_fun, "get_diagnostics")()
    result <- attach_projection_diagnostics(result, diagnostics)
  }
  
  result
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
#' @examples
#' ## create a dataset and region mask
#' Y <- matrix(rnorm(1000), nrow = 100)
#' mask <- rep(TRUE, 10)
#' roi  <- mask
#' events <- data.frame(
#'   onset = seq(1, 100, by = 5),
#'   condition = rep(c("A", "B"), each = 10),
#'   run = 1
#' )
#' dset <- fmridataset::matrix_dataset(
#'   Y, mask = mask,
#'   event_table = events,
#'   sampling_frame = data.frame(block = 1, blocklens = nrow(Y))
#' )
#' res <- run_regional(
#'   dset, region_mask = roi,
#'   y_formula = ~condition,
#'   block_formula = ~run
#' )
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
    lambda_global = 0.1,
    align_sign = TRUE,
    sign_anchor = "canonical"
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

  default_hrf <- create_hrf_basis_func("spmg1")
  design_proj <- build_design_matrix(
    event_model,
    hrf_basis_func = default_hrf,
    TR = if (!is.null(fmri_dset$TR)) fmri_dset$TR else 1
  )
  proj_comp <- build_projector(design_proj$X, lambda_global = proj_opts$lambda_global)

  spec <- projection_spec(
    event_model = event_model,
    projector_components = proj_comp,
    N_trials = length(event_model$onsets),
    K_hrf = ncol(design_proj$hrf_info$basis),
    lambda_adaptive_method = proj_opts$lambda_adaptive_method,
    lambda_global = proj_opts$lambda_global,
    collapse_method = proj_opts$collapse_method,
    X_theta_dense = as.matrix(design_proj$X),
    align_sign = proj_opts$align_sign,
    sign_anchor = proj_opts$sign_anchor
  )

  reg_fun <- make_rmvpa_searchlight_fun(spec, return_format = "matrix")

  # Build an mvpa_dataset from fmri_dset, then override mask with region_mask
  dataset <- create_mvpa_dataset_from_dataset(fmri_dset)
  dataset$mask <- region_mask
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
