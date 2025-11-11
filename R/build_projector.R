#' Build projector components from design matrix
#'
#' Perform a sparse QR decomposition of the trial-wise design matrix and
#' optionally construct a ridge-regularized projector.
#'
#' @param X_theta Sparse design matrix from \code{make_trialwise_X()}.
#' @param lambda_global Non-negative ridge regularization parameter.
#' @param diagnostics Logical; attach timing and condition number.
#' @param pivot Logical; use fill-in reducing column pivoting (default TRUE).
#' @param legacy_output Logical; if TRUE, returns legacy format with explicit
#'   Qt matrix for backward compatibility. Default FALSE for memory efficiency.
#'
#' @return An object of class \code{fr_projector}. If \code{legacy_output = FALSE}
#'   (default), contains \code{qr_obj}, \code{R}, and \code{RtR} for memory-efficient
#'   computation. If \code{legacy_output = TRUE}, contains \code{Qt}, \code{R}, 
#'   \code{K_global}, and precomputed matrices \code{RtR} and \code{tRQt} for 
#'   backward compatibility.
#' @export
build_projector <- function(X_theta, lambda_global = 0, diagnostics = FALSE,
                           pivot = TRUE, legacy_output = FALSE) {
  if (!inherits(X_theta, c("matrix", "Matrix"))) {
    stop("X_theta must be a matrix or Matrix")
  }
  if (anyNA(X_theta)) {
    stop("X_theta must not contain missing values")
  }
  if (!is.numeric(lambda_global) || length(lambda_global) != 1 ||
      lambda_global < 0) {
    stop("lambda_global must be a single non-negative numeric value")
  }

  start_time <- proc.time()["elapsed"]

  qr_obj <- Matrix::qr(X_theta, order = if (pivot) 3L else 0L)
  
  # Extract R (small p x p matrix, so OK to store)
  R <- as.matrix(Matrix::qr.R(qr_obj))
  RtR <- crossprod(R)
  
  cond_R <- 1 / Matrix::rcond(R)
  if (is.finite(cond_R) && cond_R > 1e6) {
    warning("High collinearity detected in design matrix: cond(R) > 1e6")
  }
  
  if (legacy_output) {
    # Extract Qt for backward compatibility
    Qt <- as.matrix(t(Matrix::qr.Q(qr_obj)))
    tRQt <- t(R) %*% Qt
    
    # Compute K_global if requested
    K_global <- NULL
    if (lambda_global == 0) {
      # OLS case
      # Prefer triangular solve when R is reasonably conditioned; fall back to ginv otherwise
      R_for_solve <- R
      # If a pivot is present (sparseQR), reorder columns of R back to original order for consistency
      pivot_idx <- tryCatch(qr_obj@q + 1L, error = function(e) NULL)
      if (!is.null(pivot_idx) && any(pivot_idx != seq_len(ncol(R_for_solve)))) {
        R_for_solve <- R_for_solve[, order(pivot_idx), drop = FALSE]
      }
      if (is.finite(cond_R) && cond_R <= 1e6) {
        K_global <- backsolve(R_for_solve, Qt, upper = TRUE)
      } else {
        K_global <- MASS::ginv(R) %*% Qt
      }
    } else {
      # Ridge case
      lhs <- RtR
      diag(lhs) <- diag(lhs) + lambda_global
      cho <- chol(lhs)
      K_global <- backsolve(cho, backsolve(cho, tRQt, transpose = TRUE))
    }
    
    build_time <- proc.time()["elapsed"] - start_time
    diag_list <- NULL
    if (diagnostics) {
      dl <- list(cond_R = as.numeric(cond_R),
                 lambda_global_used = lambda_global,
                 build_time = build_time,
                 qr_rank = if (inherits(qr_obj, "sparseQR")) ncol(R) else qr_obj$rank)
      diag_list <- cap_diagnostics(dl)
    }
    
    # Use legacy constructor
    out <- fr_projector(
      Qt = Qt,
      R = R,
      K_global = K_global,
      RtR = RtR,
      tRQt = tRQt,
      lambda_global = lambda_global
    )
    attr(out, "diagnostics") <- diag_list
    return(out)
  }
  
  # Default: memory-efficient approach without Qt
  build_time <- proc.time()["elapsed"] - start_time
  diag_list <- NULL
  if (diagnostics) {
    # Handle rank for both qr and sparseQR objects
    qr_rank <- if (inherits(qr_obj, "sparseQR")) {
      # For sparseQR, rank is the number of columns in R
      ncol(R)
    } else {
      # Regular qr object has $rank
      qr_obj$rank
    }
    
    dl <- list(cond_R = as.numeric(cond_R),
               lambda_global_used = lambda_global,
               build_time = build_time,
               qr_rank = qr_rank)
    diag_list <- cap_diagnostics(dl)
  }

  # Use new constructor style
  out <- fr_projector(
    qr_obj = qr_obj,
    R = R,
    RtR = RtR,
    lambda_global = lambda_global
  )
  attr(out, "diagnostics") <- diag_list
  out
}
