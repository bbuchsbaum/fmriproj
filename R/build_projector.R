#' Build projector components from design matrix
#'
#' Perform a sparse QR decomposition of the trial-wise design matrix and
#' optionally construct a ridge-regularized projector.
#'
#' @param X_theta Sparse design matrix from \code{make_trialwise_X()}.
#' @param lambda_global Non-negative ridge regularization parameter.
#' @param diagnostics Logical; attach timing and condition number.
#'
#' @return An object of class \code{fr_projector} containing \code{Qt},
#'   \code{R} and \code{K_global} if requested.
#' @export
build_projector <- function(X_theta, lambda_global = 0, diagnostics = FALSE) {
  start_time <- proc.time()["elapsed"]

  qr_obj <- Matrix::qr(X_theta)
  Qt <- t(Matrix::qr.Q(qr_obj))
  R <- Matrix::qr.R(qr_obj)

  cond_R <- kappa(R)
  if (is.finite(cond_R) && cond_R > 1e6) {
    warning("High collinearity detected in design matrix: cond(R) > 1e6")
  }

  if (lambda_global > 0) {
    m <- ncol(R)
    K_global <- solve(crossprod(R) + diag(lambda_global, m), t(R) %*% Qt)
  } else {
    K_global <- Qt
  }

  build_time <- proc.time()["elapsed"] - start_time
  diag_list <- NULL
  if (diagnostics) {
    diag_list <- list(cond_R = as.numeric(cond_R),
                      lambda_global_used = lambda_global,
                      build_time = build_time)
  }

  out <- fr_projector(Qt, R, K_global)
  attr(out, "diagnostics") <- diag_list
  out
}
