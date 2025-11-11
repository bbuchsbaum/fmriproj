#' Formula-Based Design API (Phase 2 Optional Feature)
#'
#' Optional formula-based interface for creating trial-wise projections using
#' fmridesign's rich design specification language.
#'
#' @name formula_api
NULL

#' Project trials using formula-based design specification
#'
#' Advanced interface that combines fmridesign's formula syntax with
#' fmriproj's projection pipeline. This is an optional, opt-in feature
#' for users who want more expressive design specifications.
#'
#' @param Y Time-series data matrix (time × voxels)
#' @param formula Formula specifying the design (fmridesign syntax)
#' @param data Data frame containing event information
#' @param block Block/run specification (formula or vector)
#' @param sampling_frame Sampling frame object from fmridesign
#' @param TR Repetition time (if not in sampling_frame)
#' @param lambda_method Adaptive regularization method
#' @param collapse_method HRF basis collapse method
#' @param lambda_global Global ridge penalty
#' @param verbose Print progress messages
#' @param ... Additional arguments passed to build_projector or adaptive_ridge_projector
#'
#' @return Matrix of trial patterns (trials × voxels)
#'
#' @details
#' This function provides a formula-based alternative to the standard list-based
#' event model specification. It leverages fmridesign for design construction
#' then seamlessly integrates with fmriproj's projection pipeline.
#'
#' **Benefits over standard API:**
#' - Rich formula syntax: `onset ~ hrf(condition, RT)`
#' - Automatic interaction handling
#' - Built-in contrast specifications
#' - Better type safety and validation
#' - Clearer intent in code
#'
#' **When to use:**
#' - Complex factorial designs
#' - Parametric modulation
#' - Multiple conditions with interactions
#' - When you want better error messages
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(fmridesign)
#'
#' # Create event data
#' events <- data.frame(
#'   onset = c(0, 10, 20, 30, 40, 50),
#'   condition = factor(c("A", "B", "A", "B", "A", "B")),
#'   RT = c(0.5, 0.6, 0.4, 0.7, 0.5, 0.6)
#' )
#'
#' # Create sampling frame
#' sframe <- sampling_frame(blocklens = 100, TR = 2)
#'
#' # Project using formula
#' patterns <- project_trials_formula(
#'   Y = bold_data,
#'   formula = onset ~ hrf(condition, RT),
#'   data = events,
#'   sampling_frame = sframe,
#'   lambda_method = "EB",
#'   collapse_method = "rss"
#' )
#'
#' # Equivalent to but clearer than:
#' # event_model <- list(onsets = events$onset, ...)
#' # patterns <- project_trials(Y, event_model, ...)
#' }
project_trials_formula <- function(Y,
                                   formula,
                                   data,
                                   block = NULL,
                                   sampling_frame = NULL,
                                   TR = 2,
                                   lambda_method = c("EB", "none", "LOOcv_local"),
                                   collapse_method = c("rss", "pc", "optim"),
                                   lambda_global = 0.1,
                                   verbose = TRUE,
                                   ...) {

  if (!requireNamespace("fmridesign", quietly = TRUE)) {
    stop("Package 'fmridesign' is required for formula API.\n",
         "Install it with: devtools::install_github('bbuchsbaum/fmridesign')",
         call. = FALSE)
  }

  lambda_method <- match.arg(lambda_method)
  collapse_method <- match.arg(collapse_method)

  if (verbose) message("Creating fmridesign event model from formula...")

  # Create sampling frame if not provided
  if (is.null(sampling_frame)) {
    if (verbose) message("  Creating sampling frame (TR = ", TR, ")...")
    n_time <- nrow(Y)
    sampling_frame <- fmridesign::sampling_frame(blocklens = n_time, TR = TR)
  }

  # Build fmridesign event model
  fmridesign_emod <- fmridesign::event_model(
    formula = formula,
    data = data,
    block = block,
    sampling_frame = sampling_frame
  )

  if (verbose) message("Converting to trial-wise design...")

  # Convert to trial-wise format using adapter
  trial_design <- as_trialwise_design(
    event_model = fmridesign_emod,
    sparse = TRUE,
    preserve_metadata = TRUE
  )

  if (verbose) message("Building projector...")

  # Build projector
  projector <- build_projector(
    X_theta = trial_design$X,
    lambda_global = lambda_global
  )

  if (verbose) message("Projecting data...")

  # Prepare dense matrix if needed for EB/CV
  X_dense <- if (lambda_method %in% c("EB", "LOOcv_local")) {
    as.matrix(trial_design$X)
  } else {
    NULL
  }

  # Project
  proj_result <- adaptive_ridge_projector(
    Y_sl = Y,
    projector_components = projector,
    lambda_adaptive_method = lambda_method,
    lambda_floor_global = lambda_global,
    X_theta_for_EB_residuals = X_dense,
    ...
  )

  if (verbose) message("Collapsing HRF components...")

  # Collapse
  event_table <- fmridesign::events(fmridesign_emod)
  n_trials <- nrow(event_table)
  k_bases <- ncol(trial_design$hrf_info$basis)

  collapsed <- collapse_beta(
    Z_sl_raw = proj_result$Z_sl_raw,
    N_trials = n_trials,
    K_hrf_bases = k_bases,
    method = collapse_method
  )

  if (verbose) message("Done! Projected ", n_trials, " trials.")

  # Attach metadata
  attr(collapsed$A_sl, "fmridesign_metadata") <- list(
    formula = formula,
    event_table = event_table,
    conditions = fmridesign::conditions(fmridesign_emod),
    original_event_model = fmridesign_emod
  )

  return(collapsed$A_sl)
}

#' Build design matrix using formula interface
#'
#' Create a trial-wise design matrix using fmridesign's formula syntax.
#' This is a lower-level function for users who want design matrix construction
#' without the full projection pipeline.
#'
#' @param formula Formula specifying the design
#' @param data Data frame with event information
#' @param block Block/run specification
#' @param sampling_frame Sampling frame object
#' @param TR Repetition time (if sampling_frame not provided)
#' @param sparse Return sparse matrix?
#' @param ... Additional arguments passed to event_model()
#'
#' @return An fr_design_matrix object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' design <- build_design_matrix_formula(
#'   formula = onset ~ hrf(condition),
#'   data = events,
#'   sampling_frame = sframe
#' )
#'
#' # Now use with projection pipeline
#' projector <- build_projector(design$X)
#' }
build_design_matrix_formula <- function(formula,
                                        data,
                                        block = NULL,
                                        sampling_frame = NULL,
                                        TR = 2,
                                        sparse = TRUE,
                                        ...) {

  if (!requireNamespace("fmridesign", quietly = TRUE)) {
    stop("Package 'fmridesign' is required", call. = FALSE)
  }

  # Create sampling frame if needed
  if (is.null(sampling_frame)) {
    # Estimate n_time from data
    if ("n_time" %in% names(data)) {
      n_time <- data$n_time[1]
    } else {
      # Estimate from max onset + buffer
      n_time <- ceiling(max(data$onset) / TR) + 50
    }
    sampling_frame <- fmridesign::sampling_frame(blocklens = n_time, TR = TR)
  }

  # Build fmridesign event model
  fmridesign_emod <- fmridesign::event_model(
    formula = formula,
    data = data,
    block = block,
    sampling_frame = sampling_frame,
    ...
  )

  # Convert to trial-wise without relying on fmridesign::events(event_model)
  # (some fmridesign versions do not export an events() method for event_model)
  
  # Extract condition-wise design and terms
  design_matrix <- fmridesign::design_matrix(fmridesign_emod)
  event_terms <- fmridesign::event_terms(fmridesign_emod)
  hrf_info <- extract_hrf_info_from_fmridesign(fmridesign_emod)

  # Use the provided data as the canonical event table
  event_table <- data

  tw <- expand_to_trialwise(
    design_matrix = design_matrix,
    event_table = event_table,
    event_terms = event_terms,
    hrf_info = hrf_info,
    method = "indicator",
    sparse = sparse
  )

  # Build fmriproj-compatible event model list
  fmriproj_event_model <- create_fmriproj_event_model(event_table)

  out <- fr_design_matrix(
    X = tw$X,
    event_model = fmriproj_event_model,
    hrf_info = list(basis = tw$hrf_basis)
  )
  # Attach fmridesign metadata
  attr(out, "fmridesign_source") <- list(
    original_event_model = fmridesign_emod,
    event_table = event_table,
    conditions = if ("condition" %in% names(event_table)) event_table$condition else NULL
  )
  
  return(out)
}

#' Create projection specification from formula
#'
#' Build a complete projection_spec object using formula-based design.
#' This provides a high-level interface for creating projection specifications
#' that can be used with rMVPA integration or searchlight analyses.
#'
#' @param formula Formula specifying the design
#' @param data Event data frame
#' @param block Block specification
#' @param sampling_frame Sampling frame
#' @param TR Repetition time
#' @param lambda_method Adaptive lambda method
#' @param collapse_method Basis collapse method
#' @param lambda_global Global ridge penalty
#' @param ... Additional arguments
#'
#' @return A projection_spec object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' spec <- projection_spec_formula(
#'   formula = onset ~ hrf(condition, RT),
#'   data = events,
#'   sampling_frame = sframe,
#'   lambda_method = "EB"
#' )
#'
#' # Use with searchlight
#' sl_fun <- make_rmvpa_searchlight_fun(spec)
#' }
projection_spec_formula <- function(formula,
                                    data,
                                    block = NULL,
                                    sampling_frame = NULL,
                                    TR = 2,
                                    lambda_method = "EB",
                                    collapse_method = "rss",
                                    lambda_global = 0.1,
                                    ...) {

  # Build design
  trial_design <- build_design_matrix_formula(
    formula = formula,
    data = data,
    block = block,
    sampling_frame = sampling_frame,
    TR = TR,
    sparse = TRUE,
    ...
  )

  # Build projector
  projector <- build_projector(
    X_theta = trial_design$X,
    lambda_global = lambda_global
  )

  # Get trial count
  event_table <- attr(trial_design, "fmridesign_source")$event_table
  n_trials <- nrow(event_table)
  k_bases <- ncol(trial_design$hrf_info$basis)

  # Create projection_spec
  spec <- projection_spec(
    event_model = trial_design$event_model,
    projector_components = projector,
    N_trials = n_trials,
    K_hrf = k_bases,
    lambda_adaptive_method = lambda_method,
    lambda_global = lambda_global,
    collapse_method = collapse_method,
    X_theta_dense = as.matrix(trial_design$X)
  )

  # Attach formula metadata
  attr(spec, "formula") <- formula
  attr(spec, "fmridesign_event_table") <- event_table

  return(spec)
}
