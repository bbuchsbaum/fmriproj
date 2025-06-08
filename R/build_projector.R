#' Build projector components from design matrix
#'
#' Perform a sparse QR decomposition of the trial-wise design matrix and
#' optionally construct a ridge-regularized projector.
#'
#' @param X_theta Sparse design matrix from \code{make_trialwise_X()}.
#' @param lambda_global Non-negative ridge regularization parameter.
#' @param diagnostics Logical; attach timing and condition number.
#' @param pivot Logical; use fill-in reducing column pivoting (default TRUE).
#'
#' @return An object of class \code{fr_projector} containing \code{Qt},
#'   \code{R}, \code{K_global}, and precomputed matrices \code{RtR} and
#'   \code{tRQt}. When \code{lambda_global} is zero \code{K_global} is the
#'   (pseudo-)inverse of \code{R} times \code{Qt}; otherwise ridge
#'   regularization is applied.
#' @export
build_projector <- function(X_theta, lambda_global = 0, diagnostics = FALSE,
                           pivot = TRUE) {
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
  Qt <- as.matrix(t(Matrix::qr.Q(qr_obj)))

  R <- as.matrix(Matrix::qr.R(qr_obj))
  RtR <- crossprod(R)
  tRQt <- t(R) %*% Qt

  cond_R <- 1 / Matrix::rcond(R)
  if (is.finite(cond_R) && cond_R > 1e6) {
    warning("High collinearity detected in design matrix: cond(R) > 1e6")
  }

  if (lambda_global > 0) {
    RtR <- crossprod(R)
    diag(RtR) <- diag(RtR) + lambda_global
    tRQt <- t(R) %*% Qt
    cho <- chol(RtR)
    K_global <- backsolve(cho, backsolve(cho, tRQt, transpose = TRUE))

  } else {
    K_global <- tryCatch(
      solve(R, Qt),
      error = function(e) MASS::ginv(R) %*% Qt
    )
  }

  build_time <- proc.time()["elapsed"] - start_time
  diag_list <- NULL
  if (diagnostics) {
    dl <- list(cond_R = as.numeric(cond_R),
               lambda_global_used = lambda_global,
               build_time = build_time)
    diag_list <- cap_diagnostics(dl)
  }

  out <- fr_projector(Qt, R, K_global, RtR = RtR, tRQt = tRQt)
  attr(out, "diagnostics") <- diag_list
  out
}
