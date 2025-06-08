#' Collapse Projected Betas Across HRF Bases
#'
#' Collapses the raw projected coefficients `Z_sl_raw` into trial-wise
#' feature patterns. Supports the default root-sum-square (`method = "rss"`),
#' an SNR-optimal principal component (`method = "pc"`), and optional
#' supervised optimization of the collapse weights (`method = "optim"`).
#' The resulting weights `w_sl` are normalized to unit length.
#'
#' @param Z_sl_raw Matrix of raw projected coefficients with
#'   `(N_trials * K_hrf_bases)` rows and `V_sl` columns.
#' @param N_trials Number of trials.
#' @param K_hrf_bases Number of HRF basis functions used. Defaults to
#'   `nrow(Z_sl_raw) / N_trials`.
#' @param method Collapse method. One of "rss", "pc", or "optim".
#' @param diagnostics Logical; return diagnostic information.
#' @param labels_for_w_optim Trial labels used when `method = "optim"`.
#' @param classifier_for_w_optim Function returning loss and gradient given
#'   `A_sl` and labels when `method = "optim"`.
#' @param optim_w_params List of controls passed to `stats::optim` when
#'   `method = "optim"`. The `maxit` element defaults to 5.
#' @return A list with elements:
#'   \item{A_sl}{Collapsed trial pattern matrix `N_trials x V_sl`.}
#'   \item{w_sl}{Normalized collapse weights (unit length) used for combining HRF bases.}
#'   \item{diag_data}{Optional diagnostics.}
#' @export
collapse_beta <- function(Z_sl_raw, N_trials,
                          K_hrf_bases = nrow(Z_sl_raw) / N_trials,
                          method = c("rss", "pc", "optim"),
                          diagnostics = FALSE,
                          labels_for_w_optim = NULL,
                          classifier_for_w_optim = NULL,
                          optim_w_params = list()) {

  method <- match.arg(method)
  stopifnot(N_trials > 0,
            K_hrf_bases > 0,
            nrow(Z_sl_raw) == N_trials * K_hrf_bases)


  V_sl <- ncol(Z_sl_raw)
  Zmat <- matrix(Z_sl_raw, K_hrf_bases, N_trials * V_sl, byrow = FALSE)
  A_vec <- numeric(N_trials * V_sl)
  w_sl <- rep(1 / sqrt(K_hrf_bases), K_hrf_bases)

  if (method == "rss") {
    A_vec <- sqrt(colSums(Zmat^2))
  } else if (method == "pc") {
    num_obs <- N_trials * V_sl
    if (num_obs <= 1) {
      warning("Not enough observations to compute covariance; returning zeros")
      w_sl <- rep(0, K_hrf_bases)
      A_vec <- rep(0, num_obs)
    } else {
      C_z <- tcrossprod(Zmat) / (num_obs - 1)
      eig <- tryCatch(eigen(C_z, symmetric = TRUE), error = function(e) NULL)
      if (is.null(eig)) {
        warning("PCA failed; falling back to rss")
        w_sl <- rep(1 / sqrt(K_hrf_bases), K_hrf_bases)
        A_vec <- sqrt(colSums(Zmat^2))
        # If PCA failed due to NAs, propagate NAs to the result
        if (any(is.na(Zmat))) {
          A_vec[] <- NA
        }
      } else {
        w_sl <- eig$vectors[, 1]
        w_sl <- w_sl / sqrt(sum(w_sl^2) + 1e-12)
        A_vec <- c(drop(w_sl %*% Zmat))
      }
    }
  } else if (method == "optim") {
    if (is.null(classifier_for_w_optim) || is.null(labels_for_w_optim)) {
      stop("classifier_for_w_optim and labels_for_w_optim must be provided for method='optim'")
    }

    stopifnot(length(labels_for_w_optim) == N_trials)
    Z_arr <- array(Z_sl_raw, dim = c(K_hrf_bases, N_trials, V_sl))

    if (length(labels_for_w_optim) != N_trials) {
      stop("length(labels_for_w_optim) must equal N_trials")
    }

    fn_gr <- function(w) {
      A_tmp_vec <- drop(w %*% Zmat)
      dim(A_tmp_vec) <- c(N_trials, V_sl)
      res <- classifier_for_w_optim(A_tmp_vec, labels_for_w_optim)
      grad_A_vec <- c(res$grad)
      grad_w <- Zmat %*% grad_A_vec
      list(value = res$loss, grad = as.numeric(grad_w))
    }

    cached <- make_cached_fn_gr(fn_gr)
    if (is.null(optim_w_params$maxit)) optim_w_params$maxit <- 5
    opt_res <- stats::optim(par = rep(1 / sqrt(K_hrf_bases), K_hrf_bases),
                            fn = cached$fn,
                            gr = cached$gr,
                            method = "L-BFGS-B",
                            control = optim_w_params)
    w_sl <- opt_res$par
    w_sl <- w_sl / sqrt(sum(w_sl^2) + 1e-12)
    A_vec <- drop(w_sl %*% Zmat)
  }

  dim(A_vec) <- c(N_trials, V_sl)
  diag_list <- NULL
  if (diagnostics) {
    dl <- list(method = method, w_sl = w_sl)
    if (exists("opt_res")) {
      dl$optim_details <- opt_res
    }
    diag_list <- cap_diagnostics(dl)
  }

  list(A_sl = A_vec, w_sl = w_sl, diag_data = diag_list)
}

#' Create cached fn and gr wrappers
#'
#' Given a function that returns value and gradient as a list with elements
#' `value` and `grad`, return list of `fn` and `gr` functions that cache the
#' result for the most recent parameter vector `w`.
#'
#' @param fn_gr Function taking `w` and returning list with `value` and `grad`.
#' @keywords internal
make_cached_fn_gr <- function(fn_gr) {
  env <- new.env(parent = emptyenv())
  env$w <- NULL
  env$res <- NULL

  fn <- function(w) {
    if (is.null(env$w) || !isTRUE(all.equal(env$w, w))) {
      env$res <- fn_gr(w)
      env$w <- w
    }
    env$res$value
  }

  gr <- function(w) {
    if (is.null(env$w) || !isTRUE(all.equal(env$w, w))) {
      env$res <- fn_gr(w)
      env$w <- w
    }
    env$res$grad
  }

  list(fn = fn, gr = gr, env = env)
}
