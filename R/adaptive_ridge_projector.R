#' Internal helper: Solve ridge regression using Cholesky decomposition
#' @noRd
.solve_ridge_chol <- function(R, Qt, Y, lambda) {
  lhs <- crossprod(R)
  diag(lhs) <- diag(lhs) + lambda
  tRQt <- t(R) %*% Qt
  
  cho <- chol(lhs)
  K <- backsolve(cho, backsolve(cho, tRQt, transpose = TRUE))
  K %*% Y
}

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
#' @param lambda_floor_global Minimum ridge penalty to apply. Must be
#'   non-negative.
#' @param X_theta_for_EB_residuals Optional design matrix `X(θ)` used for
#'   computing residuals when `lambda_adaptive_method = "EB"` or when
#'   cross-validating local lambda.
#' @param lambda_grid_local Optional numeric vector of candidate penalties used
#'   when `lambda_adaptive_method = "LOOcv_local"` or `"KFold_cv_local"`.
#'   Defaults to `c(0, 0.1, 1, 10)`.
#' @param folds_local_cv Optional vector of fold assignments for local
#'   cross-validation. Length must match `nrow(Y_sl)`. By default, a balanced
#'   sequence of up to 4 folds is used.
#' @param diagnostics Logical; return diagnostic information. Note: When TRUE,
#'   this stores the full K_sl projector matrix which can consume significant
#'   memory in whole-brain analyses.
#' @return A list with elements:
#'   \item{Z_sl_raw}{Projected coefficients ((N*K) x V_sl).}
#'   \item{diag_data}{List of diagnostic information if requested. When
#'     `diagnostics = TRUE`, this includes the searchlight projector `K_sl`.}
#' @export
adaptive_ridge_projector <- function(Y_sl,
                                     projector_components,
                                     lambda_adaptive_method = "none",
                                     lambda_floor_global = 0,
                                     X_theta_for_EB_residuals = NULL,
                                     lambda_grid_local = c(0, 0.1, 1, 10),
                                     folds_local_cv = NULL,
                                     diagnostics = FALSE) {
  Qt <- projector_components$Qt
  R <- projector_components$R
  RtR <- projector_components$RtR
  tRQt <- projector_components$tRQt

  if (!is.numeric(lambda_floor_global) || length(lambda_floor_global) != 1 ||
      is.na(lambda_floor_global) || lambda_floor_global < 0) {
    stop("lambda_floor_global must be a non-negative numeric scalar")
  }
  if (anyNA(Y_sl)) {
    warning("Y_sl contains missing values")
    return(list(Z_sl_raw = NULL, diag_data = NULL))
  }
  stopifnot(ncol(Qt) == nrow(Y_sl))
  if (!is.null(X_theta_for_EB_residuals)) {
    if (anyNA(X_theta_for_EB_residuals)) {
      stop("X_theta_for_EB_residuals contains missing values")
    }
    stopifnot(nrow(X_theta_for_EB_residuals) == nrow(Y_sl))
    stopifnot(ncol(X_theta_for_EB_residuals) == ncol(R))
  }

  lambda_sl_raw <- NA
  s_n_sq_vec <- NA
  s_b_sq_vec <- NA

  if (lambda_adaptive_method == "none") {
    lambda_eff <- lambda_floor_global
  } else if (lambda_adaptive_method == "EB") {
    if (is.null(X_theta_for_EB_residuals))
      stop("X_theta_for_EB_residuals must be provided for EB method")

    T_obs <- nrow(Y_sl)
    m     <- ncol(R)

    ## not enough timepoints → fall back to floor
    if (T_obs <= m) {
      warning("T_obs ≤ number of regressors; skipping EB for this search-light")
      lambda_eff <- lambda_floor_global
    } else {
      ##  OLS, upper-triangular solve
      beta_ols  <- backsolve(R, Qt %*% Y_sl, upper = TRUE)
      resid_mat <- Y_sl - X_theta_for_EB_residuals %*% beta_ols

      ## voxel-wise variance estimates
      s_n_sq_vec <- colSums(resid_mat^2) / (T_obs - m)
      s_b_sq_vec <- (colSums(beta_ols^2) / m) + .Machine$double.eps

      lambda_sl_raw <- median(s_n_sq_vec / s_b_sq_vec, na.rm = TRUE)
      # Cap maximum lambda to prevent extreme shrinkage
      lambda_sl_raw <- min(lambda_sl_raw, 1e6)
      lambda_eff    <- max(lambda_floor_global, lambda_sl_raw)
    }
  } else if (lambda_adaptive_method == "LOOcv_local" || lambda_adaptive_method == "KFold_cv_local") {
    if (is.null(X_theta_for_EB_residuals)) {
      stop("X_theta_for_EB_residuals must be provided for LOOcv_local")
    }
    X <- X_theta_for_EB_residuals
    T_obs <- nrow(X)
    if (is.null(folds_local_cv)) {
      folds <- rep_len(seq_len(min(4L, T_obs)), T_obs)
    } else {
      stopifnot(length(folds_local_cv) == T_obs)
      folds <- folds_local_cv
    }
    lambda_grid <- unique(lambda_grid_local + lambda_floor_global)
    cv_err <- numeric(length(lambda_grid))
    
    # Loop over folds first (more efficient)
    for (f in unique(folds)) {
      idx_te <- which(folds == f)
      idx_tr <- setdiff(seq_len(T_obs), idx_te)
      
      X_tr <- X[idx_tr, , drop = FALSE]
      Y_tr <- Y_sl[idx_tr, , drop = FALSE]
      
      # Single QR decomposition per fold
      qr_tr <- tryCatch(qr(X_tr),
                        error = function(e) {
                          stop("QR decomposition failed in fold ", f, ": ", e$message)
                        })
      Qt_tr <- t(qr.Q(qr_tr))
      R_tr <- qr.R(qr_tr)
      
      # Pre-compute parts that don't depend on lambda
      lhs_base <- crossprod(R_tr)
      rhs_base <- t(R_tr) %*% Qt_tr %*% Y_tr
      
      # Now loop over lambdas for this fold
      for (i in seq_along(lambda_grid)) {
        lam <- lambda_grid[i]
        
        # Copy base matrix and add regularization
        lhs <- lhs_base
        diag(lhs) <- diag(lhs) + lam
        
        beta_tr <- tryCatch(solve(lhs, rhs_base),
                            error = function(e) {
                              # Return matrix of Inf to signal failure
                              matrix(Inf, nrow = ncol(X), ncol = ncol(Y_sl))
                            })
        
        pred <- X[idx_te, , drop = FALSE] %*% beta_tr
        err_fold <- sum((Y_sl[idx_te, , drop = FALSE] - pred)^2)
        cv_err[i] <- cv_err[i] + err_fold
      }
    }
    # Check if all errors are Inf (total failure)
    if (all(is.infinite(cv_err))) {
      warning("Cross-validation failed for all lambdas; falling back to lambda_floor_global")
      lambda_sl_raw <- lambda_floor_global
    } else {
      lambda_sl_raw <- lambda_grid[which.min(cv_err)]
    }
    lambda_eff <- max(lambda_floor_global, lambda_sl_raw)
  } else {
    stop("Unknown lambda_adaptive_method")
  }

  m <- ncol(R)

  # Use helper function for ridge solve
  Z_sl_raw <- tryCatch({
    .solve_ridge_chol(R, Qt, Y_sl, lambda_eff)
  },
  error = function(e) {
    if (grepl("not positive definite", e$message)) {
      stop("Ridge solve failed with lambda ", lambda_eff, 
           ". This can happen if the design matrix is rank-deficient and lambda is too small. ",
           "Original error: ", e$message)
    } else {
      stop("Ridge solve failed with lambda ", lambda_eff, ": ", e$message)
    }
  })
  
  # Compute K_sl if needed for diagnostics
  K_sl <- NULL
  if (diagnostics) {
    lhs <- crossprod(R)
    diag(lhs) <- diag(lhs) + lambda_eff
    tRQt <- t(R) %*% Qt
    cho <- chol(lhs)
    K_sl <- backsolve(cho, backsolve(cho, tRQt, transpose = TRUE))
  }

  diag_list <- NULL
  if (diagnostics) {
    dl <- list(lambda_sl_chosen = lambda_eff,
               lambda_sl_raw = lambda_sl_raw,
               s_n_sq_vec = s_n_sq_vec,
               s_b_sq_vec = s_b_sq_vec,
               K_sl = K_sl)

    diag_list <- cap_diagnostics(dl)
  }

  list(Z_sl_raw = Z_sl_raw, diag_data = diag_list)
}
