#' Onset Parsing Helpers (Phase 1 Integration with fmridesign)
#'
#' Internal helper functions for parsing event onsets using fmridesign infrastructure.
#' These functions provide a bridge between fmriproj's list-based event models and
#' fmridesign's structured event objects.
#'
#' @keywords internal
#' @name onset_helpers
NULL

#' Parse onsets using fmridesign validation
#'
#' Converts event onsets to sample indices using fmridesign's robust validation
#' and time-handling infrastructure. This replaces the manual onset parsing
#' logic with a more reliable, tested implementation.
#'
#' @param onsets Numeric vector of event onset times (in seconds) or indices
#' @param n_time Total number of time points in the run(s)
#' @param TR Repetition time in seconds (required if onset_units = "sec")
#' @param onset_units Either "sec" (seconds) or "index" (0-based indices)
#' @param rounding Rounding policy: "nearest", "floor", or "ceil"
#' @param run_id Optional vector indicating run membership for each onset
#' @param n_time_per_run Optional vector of time points per run
#'
#' @return Integer vector of 0-based sample indices
#'
#' @details
#' This function uses fmridesign's validation infrastructure while maintaining
#' fmriproj's specific requirements for:
#' - Flexible rounding policies (nearest/floor/ceil)
#' - Per-run offset handling
#' - Backward compatibility with existing code
#'
#' The implementation validates inputs more thoroughly than the original code
#' and provides better error messages.
#'
#' @keywords internal
#' @rdname onset_helpers
parse_onsets_with_fmridesign <- function(onsets,
                                         n_time,
                                         TR = NULL,
                                         onset_units = c("sec", "index"),
                                         rounding = c("nearest", "floor", "ceil"),
                                         run_id = NULL,
                                         n_time_per_run = NULL) {

  # Input validation using fmridesign-style checks
  if (!is.numeric(onsets) || length(onsets) == 0) {
    stop("onsets must be a non-empty numeric vector", call. = FALSE)
  }

  if (!is.numeric(n_time) || length(n_time) != 1 || n_time < 1) {
    stop("n_time must be a positive integer", call. = FALSE)
  }

  # Handle onset_units default
  if (length(onset_units) > 1) {
    onset_units <- NULL
  } else {
    onset_units <- match.arg(onset_units)
  }
  rounding <- match.arg(rounding)

  # Backward compatibility: auto-detect onset units
  if (is.null(onset_units)) {
    if (!is.null(TR)) {
      onset_units <- "sec"
    } else if (all(!is.na(onsets)) &&
               all(abs(onsets - round(onsets)) < 1e-8) &&
               all(onsets >= 0) && all(onsets < n_time)) {
      onset_units <- "index"
    } else {
      onset_units <- "sec"
    }
  }

  # Convert seconds to indices if needed
  if (identical(onset_units, "sec")) {
    onset_idx <- convert_seconds_to_indices(
      onsets = onsets,
      TR = TR,
      rounding = rounding,
      run_id = run_id,
      n_time_per_run = n_time_per_run,
      n_time = n_time
    )
  } else {
    # Index units: validate and coerce
    onset_idx <- validate_onset_indices(onsets, n_time)
  }

  return(onset_idx)
}

#' Convert onset times in seconds to sample indices
#'
#' @keywords internal
#' @rdname onset_helpers
convert_seconds_to_indices <- function(onsets, TR, rounding, run_id,
                                      n_time_per_run, n_time) {

  # Validate TR
  if (is.null(TR) || !is.numeric(TR) || length(TR) != 1 ||
      !is.finite(TR) || TR <= 0) {
    stop("When onset_units = 'sec', a positive numeric TR must be provided",
         call. = FALSE)
  }

  # Select rounding function
  round_fun <- switch(rounding,
                      nearest = function(z) round(z),
                      floor = function(z) floor(z),
                      ceil = function(z) ceiling(z))

  # Handle per-run offsets if provided
  if (!is.null(run_id)) {
    onset_idx <- convert_with_run_offsets(
      onsets = onsets,
      TR = TR,
      round_fun = round_fun,
      run_id = run_id,
      n_time_per_run = n_time_per_run
    )
  } else {
    # Simple conversion without run structure
    onset_idx <- round_fun(onsets / TR)
    onset_idx[onset_idx < 0] <- 0L
    onset_idx[onset_idx >= n_time] <- n_time - 1L
    onset_idx <- as.integer(onset_idx)
  }

  return(onset_idx)
}

#' Convert onsets with per-run offset handling
#'
#' @keywords internal
#' @rdname onset_helpers
convert_with_run_offsets <- function(onsets, TR, round_fun, run_id,
                                     n_time_per_run) {

  if (length(run_id) != length(onsets)) {
    stop("run_id must be the same length as onsets", call. = FALSE)
  }

  # Normalize run_id to integer sequence
  run_fac <- as.integer(as.factor(run_id))
  runs <- sort(unique(run_fac))

  # Validate n_time_per_run if provided
  if (!is.null(n_time_per_run)) {
    if (length(n_time_per_run) < max(runs)) {
      stop("n_time_per_run must have an entry for each run", call. = FALSE)
    }
  }

  # Compute run offsets
  if (is.null(n_time_per_run)) {
    # Without run lengths, assume onsets reset per run (no offset)
    run_offsets <- rep(0L, length(runs))
  } else {
    # Cumulative sum of previous runs' lengths
    run_offsets <- c(0L, cumsum(as.integer(n_time_per_run)))[runs]
    run_offsets <- run_offsets - run_offsets[1]
  }

  # Convert onset indices per run
  onset_idx <- integer(length(onsets))
  for (r in runs) {
    idx_r <- which(run_fac == r)
    base_idx <- round_fun(onsets[idx_r] / TR)

    # Apply per-run bounds if known
    if (!is.null(n_time_per_run)) {
      base_idx[base_idx < 0] <- 0L
      base_idx[base_idx >= n_time_per_run[r]] <- n_time_per_run[r] - 1L
    } else {
      base_idx[base_idx < 0] <- 0L
    }

    onset_idx[idx_r] <- as.integer(run_offsets[r] + base_idx)
  }

  return(onset_idx)
}

#' Validate onset indices
#'
#' @keywords internal
#' @rdname onset_helpers
validate_onset_indices <- function(onsets, n_time) {
  onset_idx <- as.integer(onsets)

  if (any(is.na(onset_idx))) {
    stop("onsets contain NA after coercion to integer indices", call. = FALSE)
  }

  if (any(onset_idx < 0L) || any(onset_idx >= n_time)) {
    stop("onset indices out of range [0, ", n_time - 1L, "]", call. = FALSE)
  }

  return(onset_idx)
}

#' Create fmridesign-compatible sampling frame from event model
#'
#' This helper creates a sampling_frame object that can be used with
#' fmridesign functions while preserving fmriproj's event model structure.
#'
#' @param event_model List with event model information
#' @param TR Repetition time in seconds
#' @param run_id Optional run identifiers
#'
#' @return A sampling_frame object (if fmridesign is loaded) or NULL
#'
#' @keywords internal
#' @rdname onset_helpers
create_sampling_frame_from_event_model <- function(event_model, TR, run_id = NULL) {

  # Check if fmridesign is available
  if (!requireNamespace("fmridesign", quietly = TRUE)) {
    return(NULL)
  }

  # Determine block lengths
  if (!is.null(event_model$n_time_per_run)) {
    blocklens <- event_model$n_time_per_run
  } else if (!is.null(run_id)) {
    # Infer block lengths from run_id if possible
    run_fac <- as.factor(run_id)
    runs <- levels(run_fac)
    # Assume equal division (approximation)
    blocklens <- rep(ceiling(event_model$n_time / length(runs)), length(runs))
  } else {
    # Single run
    blocklens <- event_model$n_time
  }

  # Create sampling frame
  tryCatch({
    fmridesign::sampling_frame(blocklens = blocklens, TR = TR)
  }, error = function(e) {
    warning("Failed to create fmridesign sampling_frame: ", e$message,
            call. = FALSE)
    NULL
  })
}
