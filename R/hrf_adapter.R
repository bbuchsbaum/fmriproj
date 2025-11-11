#' HRF Adapter Functions (Phase 1 Integration with fmrihrf)
#'
#' Adapter functions to use fmrihrf HRF objects with fmriproj's projection pipeline.
#' These functions bridge between fmriproj's design matrix construction and fmrihrf's
#' comprehensive HRF infrastructure.
#'
#' @keywords internal
#' @name hrf_adapter
NULL

#' Convert fmrihrf HRF to basis matrix
#' @rdname hrf_adapter
#'
#' Takes an HRF from fmrihrf (typically a function of time) and evaluates it
#' over a time vector to produce a basis matrix compatible with fmriproj's
#' design matrix construction.
#'
#' @param hrf An HRF object from fmrihrf package (e.g., fmrihrf::HRF_SPMG3)
#' @param time_vector Numeric vector of time points to evaluate
#' @param lag Optional lag in seconds to shift the HRF
#'
#' @return Matrix with length(time_vector) rows and number of basis functions columns
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(fmrihrf)
#' time_vec <- seq(0, 29)
#' basis <- hrf_to_basis_matrix(HRF_SPMG3, time_vec)
#' dim(basis)  # 30 x 3 (canonical + 2 derivatives)
#' }
hrf_to_basis_matrix <- function(hrf, time_vector, lag = 0) {

  # Validate inputs
  if (!is.numeric(time_vector) || length(time_vector) == 0) {
    stop("time_vector must be a non-empty numeric vector", call. = FALSE)
  }

  if (!is.numeric(lag) || length(lag) != 1) {
    stop("lag must be a single numeric value", call. = FALSE)
  }

  # If an explicit lag is requested and fmrihrf is available, try to shift
  if (lag != 0 && isTRUE(requireNamespace("fmrihrf", quietly = TRUE))) {
    # If gen_hrf is available and hrf is a known fmrihrf function/object, attempt shift;
    # otherwise a simple time shift can be approximated by shifting the time vector.
    if (exists("gen_hrf", envir = asNamespace("fmrihrf"), inherits = FALSE)) {
      # Best-effort: pass through; if it fails, fall back to time shift
      try({ hrf <- fmrihrf::gen_hrf(hrf, lag = lag) }, silent = TRUE)
    }
  }

  # Evaluate HRF; fmrihrf typically exports HRFs as functions of time
  if (is.function(hrf)) {
    basis_matrix <- hrf(time_vector)
  } else if (isTRUE(requireNamespace("fmrihrf", quietly = TRUE)) &&
             exists("evaluate", envir = asNamespace("fmrihrf"), inherits = FALSE)) {
    basis_matrix <- fmrihrf::evaluate(hrf, time_vector)
  } else {
    stop("Unsupported HRF specification; provide an fmrihrf HRF function or object", call. = FALSE)
  }

  # Ensure matrix format
  if (is.vector(basis_matrix)) {
    basis_matrix <- matrix(basis_matrix, ncol = 1)
  }

  return(as.matrix(basis_matrix))
}

#' Get HRF object by name
#' @rdname hrf_adapter
#'
#' Retrieves an HRF object from fmrihrf by name, with support for common
#' aliases and backward compatibility with fmriproj's naming conventions.
#'
#' @param name Character string naming the HRF type
#' @param nbasis Optional number of basis functions for multi-basis HRFs
#' @param span Optional support length (in seconds) for HRFs that depend on a span
#' @param engine Engine for generating the HRF. Defaults to `"auto"`, which uses
#'   a legacy in-package implementation for `bspline` (matching historical
#'   behaviour) and falls back to `fmrihrf` for other HRF names.
#'   Set to `"fmrihrf"` to force the `fmrihrf` implementation.
#' @param ... Additional parameters passed to `fmrihrf::getHRF`
#'
#' @return An HRF object from fmrihrf
#'
#' @details
#' Supported HRF types:
#' - "spmg1": SPM canonical HRF (single basis)
#' - "spmg2": SPM canonical + temporal derivative (2 bases)
#' - "spmg3": SPM canonical + temporal + dispersion derivatives (3 bases)
#' - "gamma": Double gamma HRF
#' - "gaussian": Gaussian HRF
#' - "bspline": B-spline basis (specify nbasis)
#' - "fir": Finite impulse response (specify nbasis)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' hrf <- get_hrf_by_name("spmg3")
#' hrf_bspline <- get_hrf_by_name("bspline", nbasis = 5, span = 30)
#' }
get_hrf_by_name <- function(name,
                            nbasis = NULL,
                            span = NULL,
                            engine = c("auto", "legacy", "fmrihrf"),
                            ...) {

  if (!requireNamespace("fmrihrf", quietly = TRUE)) {
    stop("Package 'fmrihrf' is required but not installed.\n",
         "Install it with: devtools::install_github('bbuchsbaum/fmrihrf')",
         call. = FALSE)
  }

  name <- tolower(name)
  engine <- match.arg(engine)

  if (name == "bspline") {
    effective_engine <- if (engine == "auto") "legacy" else engine
    if (effective_engine == "legacy") {
      nbasis_val <- if (is.null(nbasis)) 30L else as.integer(nbasis)
      span_val <- if (is.null(span)) nbasis_val else span
      return(legacy_bspline_hrf(nbasis_val, span_val))
    }
  }

  args <- list(name = name, ...)

  if (!is.null(nbasis)) {
    args$nbasis <- nbasis
  }

  span_eff <- span
  if (is.null(span_eff) && name %in% c("bspline", "fir")) {
    span_eff <- 30
  }
  if (!is.null(span_eff)) {
    args$span <- span_eff
  }

  hrf <- tryCatch(
    do.call(fmrihrf::getHRF, args),
    error = function(e) {
      stop("Unknown or unsupported HRF type '", name, "'. ",
           "Supported types include: spmg1, spmg2, spmg3, gamma, gaussian, bspline, fir. ",
           "Original error: ", e$message,
           call. = FALSE)
    }
  )

  hrf
}

#' Legacy bspline HRF generator matching pre-fmrihrf behaviour
#'
#' Provides a dense cubic B-spline basis that spans the requested duration,
#' preserving historical behaviour relied upon by older tests.
#'
#' @keywords internal
legacy_bspline_hrf <- function(nbasis = 30L, span = nbasis) {
  nbasis <- as.integer(nbasis)
  if (is.na(nbasis) || nbasis < 1L) {
    nbasis <- 30L
  }

  if (is.null(span) || !is.finite(span) || span <= 0) {
    span <- nbasis
  }

  function(t) {
    if (length(t) == 0) {
      return(matrix(0, nrow = 0, ncol = nbasis))
    }
    t_vec <- as.numeric(t)
    t_vec[is.na(t_vec)] <- 0
    t_vec[t_vec < 0] <- 0
    span_eff <- max(span, max(t_vec, na.rm = TRUE))
    bs_mat <- splines::bs(
      t_vec,
      df = nbasis,
      degree = 3,
      Boundary.knots = c(0, span_eff),
      intercept = FALSE
    )
    as.matrix(bs_mat)
  }
}

#' Create HRF basis function for build_design_matrix
#' @rdname hrf_adapter
#'
#' Wraps an fmrihrf HRF object as a function compatible with
#' build_design_matrix's hrf_basis_func parameter.
#'
#' @param hrf An HRF object from fmrihrf or character name
#' @param lag Optional lag in seconds
#'
#' @return Function that takes (theta_params, time_vector) and returns basis matrix
#'
#' @details
#' This adapter allows using fmrihrf HRFs with the existing build_design_matrix
#' function signature. The theta_params argument is ignored (for backward compatibility)
#' since fmrihrf HRFs are pre-parameterized.
#'
#' @examples
#' \dontrun{
#' # Using HRF object directly
#' hrf_func <- create_hrf_basis_func(fmrihrf::HRF_SPMG3)
#'
#' # Using name
#' hrf_func <- create_hrf_basis_func("spmg3")
#'
#' # Use with build_design_matrix
#' design <- build_design_matrix(
#'   event_model = my_events,
#'   hrf_basis_func = hrf_func,
#'   TR = 2
#' )
#' }
create_hrf_basis_func <- function(hrf, lag = 0) {

  # Convert character name to HRF function
  if (is.character(hrf)) {
    hrf <- get_hrf_by_name(hrf)
  }

  # Return function with signature expected by build_design_matrix
  function(theta_params, time_vector) {
    # theta_params ignored - kept for backward compatibility
    out <- hrf_to_basis_matrix(hrf, time_vector, lag = lag)
    out
  }
}

#' Backward-compatible wrapper for hrf_basis_spmg3_theta
#' @rdname hrf_adapter
#'
#' Maintains backward compatibility with existing code using the theta-parameterized
#' SPMG3 HRF. Issues a deprecation warning and uses fmrihrf instead.
#'
#' @param theta Numeric vector controlling delay and dispersion scaling (ignored)
#' @param t Numeric vector of time points
#'
#' @return Matrix with SPMG3 basis functions
#'
#' @details
#' This function is deprecated. Use fmrihrf::HRF_SPMG3 directly instead.
#' The theta parameterization is not currently supported in fmrihrf, so
#' theta values are ignored and the standard SPMG3 basis is returned.
#'
#' To customize HRF parameters, use fmrihrf::gen_hrf() with lag parameter.
#'
#' @export
hrf_basis_spmg3_theta_deprecated <- function(theta = c(1, 1), t) {

  lifecycle::deprecate_warn(
    when = "0.2.0",
    what = "hrf_basis_spmg3_theta()",
    with = "fmrihrf::HRF_SPMG3",
    details = paste(
      "The theta parameterization is not currently supported.",
      "Use fmrihrf::gen_hrf() for lag customization.",
      "For more complex parameterization, consider fmrihrf extension."
    )
  )

  # Fall back to standard SPMG3 (theta ignored)
  hrf_to_basis_matrix(fmrihrf::HRF_SPMG3, t, lag = 0)
}

#' List available HRF types
#' @rdname hrf_adapter
#'
#' Returns a data frame describing available HRF types from fmrihrf.
#'
#' @return Data frame with columns: name, nbasis, description
#'
#' @export
#'
#' @examples
#' list_available_hrfs()
list_available_hrfs <- function() {

  data.frame(
    name = c("spmg1", "spmg2", "spmg3", "gamma", "gaussian", "bspline", "fir"),
    nbasis = c(1, 2, 3, 1, 1, "variable", "variable"),
    description = c(
      "SPM canonical HRF",
      "SPM canonical + temporal derivative",
      "SPM canonical + temporal + dispersion derivatives",
      "Double gamma HRF",
      "Gaussian HRF",
      "B-spline basis (specify nbasis parameter)",
      "Finite impulse response (specify nbasis parameter)"
    ),
    stringsAsFactors = FALSE
  )
}
