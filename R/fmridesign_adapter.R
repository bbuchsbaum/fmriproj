#' fmridesign Adapter (Phase 2 Integration)
#'
#' Adapter functions to convert between fmridesign event models and fmriproj's
#' trial-wise design matrices. This enables using fmridesign's rich formula-based
#' design specification with fmriproj's projection pipeline.
#'
#' @name fmridesign_adapter
NULL

#' Convert fmridesign event_model to trial-wise design
#'
#' Takes an event_model object from fmridesign and converts it to fmriproj's
#' trial-wise design matrix format. This is the key adapter enabling Phase 2
#' integration.
#'
#' @param event_model An event_model object from fmridesign
#' @param sparse Logical; return sparse matrix if TRUE
#' @param trialwise_expansion How to expand condition-wise design to trial-wise.
#'   Options: "replicate" (replicate condition columns per trial),
#'   "indicator" (create binary indicators per trial)
#' @param preserve_metadata Logical; attach fmridesign metadata as attributes
#'
#' @return An fr_design_matrix object compatible with fmriproj pipeline
#'
#' @details
#' fmridesign creates condition-wise design matrices (one column per condition × basis),
#' while fmriproj needs trial-wise matrices (one column per trial × basis).
#'
#' This adapter implements the critical conversion by:
#' 1. Extracting event timing and structure from fmridesign
#' 2. Reorganizing columns from condition-wise to trial-wise
#' 3. Preserving HRF basis information
#' 4. Maintaining compatibility with fmriproj's projection pipeline
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(fmridesign)
#'
#' # Create fmridesign event model
#' sframe <- sampling_frame(blocklens = 100, TR = 2)
#' des <- data.frame(onset = c(0, 10, 20), condition = c("A", "B", "A"))
#' emod <- event_model(onset ~ hrf(condition), data = des,
#'                     sampling_frame = sframe)
#'
#' # Convert to trial-wise format for fmriproj
#' trial_design <- as_trialwise_design(emod)
#'
#' # Use with projection pipeline
#' projector <- build_projector(trial_design$X, lambda_global = 0.1)
#' }
as_trialwise_design <- function(event_model,
                                sparse = TRUE,
                                trialwise_expansion = c("replicate", "indicator"),
                                preserve_metadata = TRUE,
                                event_table_override = NULL) {

  if (!requireNamespace("fmridesign", quietly = TRUE)) {
    stop("Package 'fmridesign' is required for this function.\n",
         "Install it with: devtools::install_github('bbuchsbaum/fmridesign')",
         call. = FALSE)
  }

  trialwise_expansion <- match.arg(trialwise_expansion)

  # Extract design matrix from fmridesign
  design_matrix <- fmridesign::design_matrix(event_model)

  # Extract event information
  event_terms <- fmridesign::event_terms(event_model)
  if (length(event_terms) == 0) {
    stop("event_model contains no event terms", call. = FALSE)
  }

  # Get canonical event table (robust to fmridesign versions)
  event_table <- NULL
  if (!is.null(event_table_override)) {
    event_table <- event_table_override
  } else {
    event_table <- tryCatch(fmridesign::events(event_model), error = function(e) NULL)
    if (is.null(event_table)) {
      # Best-effort: try to recover data from the event model
      if (!is.null(attr(event_model, "data"))) {
        event_table <- attr(event_model, "data")
      } else if (!is.null(event_model$data)) {
        event_table <- event_model$data
      }
    }
    if (is.null(event_table)) {
      stop("Unable to extract event table from fmridesign event_model; provide event_table_override.")
    }
  }

  # Extract HRF basis information
  hrf_info <- extract_hrf_info_from_fmridesign(event_model)

  # Convert from condition-wise to trial-wise
  trial_design <- expand_to_trialwise(
    design_matrix = design_matrix,
    event_table = event_table,
    event_terms = event_terms,
    hrf_info = hrf_info,
    method = trialwise_expansion,
    sparse = sparse
  )

  # Create compatible event_model list for fmriproj
  fmriproj_event_model <- create_fmriproj_event_model(event_table)

  # Construct fr_design_matrix object
  result <- fr_design_matrix(
    X = trial_design$X,
    event_model = fmriproj_event_model,
    hrf_info = list(basis = trial_design$hrf_basis)
  )

  # Attach metadata if requested
  if (preserve_metadata) {
    attr(result, "fmridesign_source") <- list(
      original_event_model = event_model,
      event_table = event_table,
      conditions = fmridesign::conditions(event_model),
      expansion_method = trialwise_expansion
    )
  }

  return(result)
}

#' Expand condition-wise design to trial-wise
#'
#' @keywords internal
expand_to_trialwise <- function(design_matrix, event_table, event_terms,
                                hrf_info, method, sparse) {

  # Get number of trials and conditions
  n_trials <- nrow(event_table)
  n_time <- nrow(design_matrix)
  n_hrf_bases <- hrf_info$n_bases

  if (method == "replicate") {
    # Method 1: Replicate condition columns for each trial
    # This is more direct but creates larger matrices

    # Get condition assignments per trial
    trial_conditions <- event_table$condition

    # Build trial-wise matrix by selecting appropriate condition columns
    # for each trial and stacking horizontally
    X_trial <- expand_replicate_method(
      design_matrix = design_matrix,
      trial_conditions = trial_conditions,
      n_hrf_bases = n_hrf_bases,
      n_time = n_time,
      n_trials = n_trials,
      sparse = sparse
    )

  } else {
    # Method 2: Create binary indicators per trial
    # More compatible with fmriproj's existing structure

    X_trial <- expand_indicator_method(
      event_table = event_table,
      hrf_info = hrf_info,
      n_time = n_time,
      n_trials = n_trials,
      n_hrf_bases = n_hrf_bases,
      sparse = sparse
    )
  }

  list(
    X = X_trial,
    hrf_basis = hrf_info$basis_matrix
  )
}

#' Expand using replication method
#'
#' @keywords internal
expand_replicate_method <- function(design_matrix, trial_conditions,
                                    n_hrf_bases, n_time, n_trials, sparse) {

  # Extract unique conditions
  conditions <- unique(trial_conditions)
  n_conditions <- length(conditions)

  # Map trial to condition index
  trial_to_cond <- match(trial_conditions, conditions)

  # Initialize output matrix
  ncol_out <- n_trials * n_hrf_bases

  if (sparse) {
    # Build using triplet format for efficiency
    trip_i <- integer(0)
    trip_j <- integer(0)
    trip_x <- numeric(0)

    for (trial_idx in seq_len(n_trials)) {
      cond_idx <- trial_to_cond[trial_idx]

      for (basis_idx in seq_len(n_hrf_bases)) {
        # Column in original design matrix
        src_col <- (cond_idx - 1) * n_hrf_bases + basis_idx

        # Column in trial-wise matrix
        dest_col <- (trial_idx - 1) * n_hrf_bases + basis_idx

        # Copy column
        col_data <- design_matrix[, src_col]
        nonzero_idx <- which(col_data != 0)

        if (length(nonzero_idx) > 0) {
          trip_i <- c(trip_i, nonzero_idx - 1)
          trip_j <- c(trip_j, rep(dest_col - 1, length(nonzero_idx)))
          trip_x <- c(trip_x, col_data[nonzero_idx])
        }
      }
    }

    X_trial <- make_spmat_triplet(trip_i, trip_j, trip_x, n_time, ncol_out)

  } else {
    # Dense matrix version
    X_trial <- matrix(0, nrow = n_time, ncol = ncol_out)

    for (trial_idx in seq_len(n_trials)) {
      cond_idx <- trial_to_cond[trial_idx]

      for (basis_idx in seq_len(n_hrf_bases)) {
        src_col <- (cond_idx - 1) * n_hrf_bases + basis_idx
        dest_col <- (trial_idx - 1) * n_hrf_bases + basis_idx
        X_trial[, dest_col] <- design_matrix[, src_col]
      }
    }
  }

  return(X_trial)
}

#' Expand using indicator method (build from scratch)
#'
#' @keywords internal
expand_indicator_method <- function(event_table, hrf_info, n_time,
                                    n_trials, n_hrf_bases, sparse) {

  # Build trial-wise design by convolving each trial individually
  # This is essentially what fmriproj does natively

  onsets <- event_table$onset
  durations <- if ("duration" %in% names(event_table)) {
    event_table$duration
  } else {
    rep(0, n_trials)
  }

  # Get amplitudes/modulators if present
  amplitudes <- if ("amplitude" %in% names(event_table)) {
    event_table$amplitude
  } else {
    rep(1, n_trials)
  }

  # Build using triplet format
  hrf_basis <- hrf_info$basis_matrix
  basis_length <- nrow(hrf_basis)

  ncol_out <- n_trials * n_hrf_bases
  est_nz <- n_trials * n_hrf_bases * basis_length

  trip_i <- integer(est_nz)
  trip_j <- integer(est_nz)
  trip_x <- numeric(est_nz)
  idx <- 1L

  for (n in seq_len(n_trials)) {
    onset_idx <- onsets[n]
    amp <- amplitudes[n]

    for (k in seq_len(n_hrf_bases)) {
      col_index <- (n - 1) * n_hrf_bases + (k - 1)
      rows <- onset_idx + seq_len(basis_length) - 1
      valid <- rows < n_time

      nv <- sum(valid)
      if (nv > 0L) {
        idx_end <- idx + nv - 1L
        trip_i[idx:idx_end] <- rows[valid]
        trip_j[idx:idx_end] <- rep(col_index, nv)
        trip_x[idx:idx_end] <- amp * hrf_basis[valid, k]
        idx <- idx_end + 1L
      }
    }
  }

  final_len <- idx - 1L
  if (final_len < est_nz) {
    trip_i <- trip_i[seq_len(final_len)]
    trip_j <- trip_j[seq_len(final_len)]
    trip_x <- trip_x[seq_len(final_len)]
  }

  X_trial <- make_spmat_triplet(trip_i, trip_j, trip_x, n_time, ncol_out)

  if (!sparse) {
    X_trial <- as.matrix(X_trial)
  }

  return(X_trial)
}

#' Extract HRF information from fmridesign event_model
#'
#' @keywords internal
extract_hrf_info_from_fmridesign <- function(event_model) {

  event_terms <- fmridesign::event_terms(event_model)

  # Get HRF from first term (assume consistent across terms)
  if (length(event_terms) > 0) {
    first_term <- event_terms[[1]]

    # Extract HRF specification
    hrf_spec <- attr(first_term, "hrfspec")

    if (!is.null(hrf_spec) && !is.null(hrf_spec$hrf)) {
      hrf_obj <- hrf_spec$hrf

      # Evaluate HRF to get basis matrix
      # Default to 30 time points if not specified
      basis_length <- if (!is.null(hrf_spec$duration)) {
        ceiling(hrf_spec$duration)
      } else {
        30
      }

      time_vec <- seq(0, basis_length - 1)

      if (requireNamespace("fmrihrf", quietly = TRUE)) {
        basis_matrix <- fmrihrf::evaluate(hrf_obj, time_vec)
      } else {
        stop("fmrihrf package required to evaluate HRF", call. = FALSE)
      }

      # Ensure matrix format
      if (is.vector(basis_matrix)) {
        basis_matrix <- matrix(basis_matrix, ncol = 1)
      }

      n_bases <- ncol(basis_matrix)

    } else {
      # Fallback: assume single basis
      basis_matrix <- matrix(1, nrow = 30, ncol = 1)
      n_bases <- 1
    }

  } else {
    # No terms - use default
    basis_matrix <- matrix(1, nrow = 30, ncol = 1)
    n_bases <- 1
  }

  list(
    basis_matrix = basis_matrix,
    n_bases = n_bases
  )
}

#' Create fmriproj-compatible event_model list
#'
#' @keywords internal
create_fmriproj_event_model <- function(event_table) {

  list(
    onsets = event_table$onset,
    n_time = max(event_table$onset) + 100,  # Approximate
    conditions = if ("condition" %in% names(event_table)) {
      event_table$condition
    } else {
      NULL
    },
    amplitudes = if ("amplitude" %in% names(event_table)) {
      event_table$amplitude
    } else {
      rep(1, nrow(event_table))
    },
    modulator = rep(1, nrow(event_table)),
    blocks = if ("block" %in% names(event_table)) {
      event_table$block
    } else {
      NULL
    }
  )
}

#' Convert fmriproj event_model to fmridesign format
#'
#' Reverse adapter: converts fmriproj's list-based event model to
#' fmridesign's structured event_model object.
#'
#' @param event_model_list List with fmriproj event model structure
#' @param TR Repetition time in seconds
#' @param hrf HRF specification (character, HRF object, or function)
#'
#' @return fmridesign event_model object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # fmriproj event model
#' events <- list(
#'   onsets = c(0, 10, 20),
#'   n_time = 50,
#'   conditions = c("A", "B", "A")
#' )
#'
#' # Convert to fmridesign
#' emod <- as_fmridesign_event_model(events, TR = 2, hrf = "spmg3")
#'
#' # Now can use fmridesign functions
#' plot(emod)
#' design_matrix(emod)
#' }
as_fmridesign_event_model <- function(event_model_list, TR = 2,
                                      hrf = "spmg1") {

  if (!requireNamespace("fmridesign", quietly = TRUE)) {
    stop("Package 'fmridesign' is required", call. = FALSE)
  }

  # Create data frame for fmridesign
  event_df <- data.frame(
    onset = event_model_list$onsets
  )

  if (!is.null(event_model_list$conditions)) {
    event_df$condition <- event_model_list$conditions
  }

  # Create sampling frame
  n_time <- event_model_list$n_time
  blocklens <- if (!is.null(event_model_list$n_time_per_run)) {
    event_model_list$n_time_per_run
  } else {
    n_time
  }

  sframe <- fmridesign::sampling_frame(blocklens = blocklens, TR = TR)

  # Build event model
  if (!is.null(event_model_list$conditions)) {
    emod <- fmridesign::event_model(
      onset ~ fmridesign::hrf(condition, basis = hrf),
      data = event_df,
      sampling_frame = sframe
    )
  } else {
    # Single condition
    event_df$trial <- seq_len(nrow(event_df))
    emod <- fmridesign::event_model(
      onset ~ fmridesign::trialwise(basis = hrf),
      data = event_df,
      sampling_frame = sframe
    )
  }

  return(emod)
}
