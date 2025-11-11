#' fmriproj Design Matrix Object
#'
#' A lightweight container for the trial-wise design matrix and
#' associated metadata used throughout the pipeline.
#'
#' @param X A sparse matrix (\code{Matrix::dgCMatrix}) or dense matrix
#'   containing the design matrix.
#' @param event_model An optional \code{fmrireg_event_model} object describing
#'   the experimental design.
#' @param hrf_info Optional list describing the HRF basis used.
#'
#' @return An object of class \code{fr_design_matrix}.
#' @export
fr_design_matrix <- function(X, event_model = NULL, hrf_info = list()) {
  if (!is.matrix(X) && !inherits(X, "dgCMatrix")) {
    stop("X must be a base matrix or Matrix::dgCMatrix")
  }
  if (!is.null(event_model) &&
      !is.list(event_model) &&
      !inherits(event_model, "fmrireg_event_model")) {
    stop("event_model must be a list or fmrireg_event_model")
  }
  if (!is.list(hrf_info)) {
    stop("hrf_info must be a list")
  }
  structure(
    list(X = X, event_model = event_model, hrf_info = hrf_info),
    class = "fr_design_matrix"
  )
}

#' Projector Components Object
#'
#' Container for matrices produced by \code{build_projector()} that are
#' reused across searchlights. Supports both legacy (explicit Qt) and new
#' (qr_obj) representations.
#'
#' @param qr_obj QR decomposition object from Matrix::qr (new approach).
#' @param R Upper triangular R matrix from QR.
#' @param RtR Optional precomputed \eqn{R^\top R} matrix.
#' @param lambda_global Ridge regularization parameter used.
#' @param Qt Transposed Q matrix from a thin QR decomposition (legacy).
#' @param K_global Optional global projector matrix (legacy).
#' @param tRQt Optional precomputed \eqn{R^\top Q^\top} matrix (legacy).
#'
#' @return An object of class \code{fr_projector}.
#' @export
fr_projector <- function(qr_obj = NULL, R = NULL, RtR = NULL, lambda_global = 0,
                        # Legacy arguments
                        Qt = NULL, K_global = NULL, tRQt = NULL) {
  
  # New, preferred path
  if (!is.null(qr_obj)) {
    # Validate new-style arguments
    if (!inherits(qr_obj, c("qr", "sparseQR"))) {
      stop("qr_obj must be a QR decomposition object")
    }
    if (is.null(R) || !is.matrix(R)) {
      stop("R must be provided as a matrix when using qr_obj")
    }
    if (!is.null(RtR) && !is.matrix(RtR)) {
      stop("RtR must be a matrix")
    }
    if (!is.numeric(lambda_global) || length(lambda_global) != 1 || lambda_global < 0) {
      stop("lambda_global must be a non-negative numeric scalar")
    }
    
    out <- structure(
      list(
        qr_obj = qr_obj,
        R = R,
        RtR = RtR,
        lambda_global = lambda_global,
        .internal_representation = "qr"
      ),
      class = "fr_projector"
    )
    
  } else if (!is.null(Qt)) {
    # Legacy path
    warning("The 'Qt', 'tRQt', 'K_global' arguments for fr_projector are deprecated. ",
            "Use 'qr_obj', 'R', 'RtR' instead.", call. = FALSE)
    
    if (!is.matrix(Qt)) {
      stop("Qt must be a matrix")
    }
    if (!is.matrix(R)) {
      stop("R must be a matrix")
    }
    stopifnot(nrow(Qt) == ncol(R))
    
    if (!is.null(K_global)) {
      if (!is.matrix(K_global)) {
        stop("K_global must be a matrix")
      }
      if (!all(dim(K_global) == dim(Qt))) {
        stop("K_global must have same dimensions as Qt")
      }
    }
    if (!is.null(RtR)) {
      if (!is.matrix(RtR)) {
        stop("RtR must be a matrix")
      }
      if (!all(dim(RtR) == c(ncol(R), ncol(R)))) {
        stop("RtR must be square with dimension equal to ncol(R)")
      }
    }
    if (!is.null(tRQt)) {
      if (!is.matrix(tRQt)) {
        stop("tRQt must be a matrix")
      }
      if (!all(dim(tRQt) == dim(Qt))) {
        stop("tRQt must have same dimensions as Qt")
      }
    }
    
    out <- structure(
      list(Qt = Qt, R = R, K_global = K_global, RtR = RtR, tRQt = tRQt,
           lambda_global = lambda_global,
           .internal_representation = "explicit"),
      class = "fr_projector"
    )
    
  } else {
    stop("Invalid arguments for fr_projector constructor. ",
         "Either provide 'qr_obj' and 'R' (new style) or 'Qt' and 'R' (legacy).")
  }
  
  return(out)
}

#' Check for `fr_design_matrix` object
#'
#' Convenience function to test whether an object was created by
#' `fr_design_matrix`.
#'
#' @param x Object to test.
#'
#' @return `TRUE` if `x` inherits from `fr_design_matrix`.
#' @export
is.fr_design_matrix <- function(x) {
  inherits(x, "fr_design_matrix")
}

#' Check for `fr_projector` object
#'
#' Convenience function to test whether an object was created by
#' `fr_projector`.
#'
#' @param x Object to test.
#'
#' @return `TRUE` if `x` inherits from `fr_projector`.
#' @export
is.fr_projector <- function(x) {
  inherits(x, "fr_projector")
}
