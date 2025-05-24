#' Adaptive Ridge Projection for a Searchlight
#'
#' Computes a ridge-regularized projector for a searchlight's BOLD data using
#' components from `build_projector()`. Supports empirical Bayes ("EB") and
#' simple cross-validated ("LOOcv_local") selection of the ridge parameter in
#' addition to the default "none" method.
#'
#' @param Y_sl Matrix of BOLD data for the searchlight (T x V_sl).
#' @param projector_components Object returned by `build_projector()`.
#' @param lambda_adaptive_method Method for choosing searchlight-specific lambda.
#'   Defaults to "none" which simply uses `lambda_floor_global`.
#' @param lambda_floor_global Minimum ridge penalty to apply.
#' @param X_theta_for_EB_residuals Optional design matrix `X(θ)` used for
#'   computing residuals when `lambda_adaptive_method = "EB"` or when
#'   cross-validating local lambda. Can be a dense matrix or a sparse
#'   `Matrix::dgCMatrix`.
#' @param diagnostics Logical; return diagnostic information.
#' @return A list with elements:
#'   \item{Z_sl_raw}{Projected coefficients ((N*K) x V_sl).}
#'   \item{diag_data}{List of diagnostic information if requested.}
#' @export
adaptive_ridge_projector <- function(Y_sl,
                                     projector_components,
                                     lambda_adaptive_method = "none",
                                     lambda_floor_global = 0,
                                     X_theta_for_EB_residuals = NULL,
                                     diagnostics = FALSE) {
  Qt <- projector_components$Qt
  R <- projector_components$R

  lambda_sl_raw <- NA
  s_n_sq <- NA
  s_b_sq <- NA

  if (lambda_adaptive_method == "none") {
    lambda_eff <- lambda_floor_global
  } else if (lambda_adaptive_method == "EB") {
    if (is.null(X_theta_for_EB_residuals)) {
      stop("X_theta_for_EB_residuals must be provided for EB method")
    }
    beta_ols <- solve(R, Qt %*% Y_sl)
    resid_mat <- Y_sl - X_theta_for_EB_residuals %*% beta_ols
    T_obs <- nrow(Y_sl)
    V_sl <- ncol(Y_sl)
    m <- ncol(R)
    s_n_sq <- sum(resid_mat^2) / ((T_obs - m) * V_sl)
    s_b_sq <- sum(beta_ols^2) / (m * V_sl)
    lambda_sl_raw <- s_n_sq / s_b_sq
    lambda_eff <- max(lambda_floor_global, lambda_sl_raw)
  } else if (lambda_adaptive_method == "LOOcv_local") {
    if (is.null(X_theta_for_EB_residuals)) {
      stop("X_theta_for_EB_residuals must be provided for LOOcv_local")
    }
    X <- X_theta_for_EB_residuals
    T_obs <- nrow(X)
    folds <- rep_len(seq_len(min(4L, T_obs)), T_obs)
    lambda_grid <- c(0, 0.1, 1, 10) + lambda_floor_global
    cv_err <- numeric(length(lambda_grid))
    for (i in seq_along(lambda_grid)) {
      lam <- lambda_grid[i]
      err <- 0
      for (f in unique(folds)) {
        idx_te <- which(folds == f)
        idx_tr <- setdiff(seq_len(T_obs), idx_te)
        qr_tr <- qr(X[idx_tr, , drop = FALSE])
        Qt_tr <- t(qr.Q(qr_tr))
        R_tr <- qr.R(qr_tr)
        beta_tr <- solve(crossprod(R_tr) + diag(lam, ncol(R_tr)),
                         t(R_tr) %*% Qt_tr %*% Y_sl[idx_tr, , drop = FALSE])
        pred <- X[idx_te, , drop = FALSE] %*% beta_tr
        err <- err + sum((Y_sl[idx_te, , drop = FALSE] - pred)^2)
      }
      cv_err[i] <- err
    }
    lambda_sl_raw <- lambda_grid[which.min(cv_err)]
    lambda_eff <- max(lambda_floor_global, lambda_sl_raw)
  } else {
    stop("Unknown lambda_adaptive_method")
  }

  m <- ncol(R)
  RtR <- crossprod(R)
  K_sl <- solve(RtR + diag(lambda_eff, m), t(R) %*% Qt)

  Z_sl_raw <- K_sl %*% Y_sl

  diag_list <- NULL
  if (diagnostics) {
    dl <- list(lambda_sl_chosen = lambda_eff,
               lambda_sl_raw = lambda_sl_raw,
               s_n_sq = s_n_sq,
               s_b_sq = s_b_sq)
    diag_list <- cap_diagnostics(dl)
  }

  list(Z_sl_raw = Z_sl_raw, diag_data = diag_list)
}
