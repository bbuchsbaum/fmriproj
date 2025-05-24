#' Collapse Projected Betas Across HRF Bases
#'
#' Collapses the raw projected coefficients `Z_sl_raw` into trial-wise
#' feature patterns. Supports the default root-sum-square (`method = "rss"`),
#' an SNR-optimal principal component (`method = "pc"`), and optional
#' supervised optimization of the collapse weights (`method = "optim"`).
#'
#' @param Z_sl_raw Matrix of raw projected coefficients with
#'   `(N_trials * K_hrf_bases)` rows and `V_sl` columns.
#' @param N_trials Number of trials.
#' @param K_hrf_bases Number of HRF basis functions used.
#' @param method Collapse method. One of "rss", "pc", or "optim".
#' @param diagnostics Logical; return diagnostic information.
#' @param labels_for_w_optim Trial labels used when `method = "optim"`.
#' @param classifier_for_w_optim Function returning loss and gradient given
#'   `A_sl` and labels when `method = "optim"`.
#' @param optim_w_params List of controls passed to `stats::optim` when
#'   `method = "optim"`.
#' @return A list with elements:
#'   \item{A_sl}{Collapsed trial pattern matrix `N_trials x V_sl`.}
#'   \item{w_sl}{Collapse weights (NULL for "rss").}
#'   \item{diag_data}{Optional diagnostics.}
#' @export
collapse_beta <- function(Z_sl_raw, N_trials, K_hrf_bases,
                          method = "rss", diagnostics = FALSE,
                          labels_for_w_optim = NULL,
                          classifier_for_w_optim = NULL,
                          optim_w_params = list()) {
  if (!method %in% c("rss", "pc", "optim")) {
    stop("method must be either 'rss', 'pc', or 'optim'")
  }

  if (nrow(Z_sl_raw) != N_trials * K_hrf_bases) {
    stop("nrow(Z_sl_raw) must equal N_trials * K_hrf_bases")
  }

  V_sl <- ncol(Z_sl_raw)
  A_sl <- matrix(0, nrow = N_trials, ncol = V_sl)
  w_sl <- NULL

  if (method == "rss") {
    for (n in seq_len(N_trials)) {
      rows <- ((n - 1) * K_hrf_bases + 1):(n * K_hrf_bases)
      A_sl[n, ] <- sqrt(colSums(Z_sl_raw[rows, , drop = FALSE]^2))
    }
  } else if (method == "pc") {
    Z_stack <- matrix(0, nrow = N_trials * V_sl, ncol = K_hrf_bases)
    for (n in seq_len(N_trials)) {
      rows <- ((n - 1) * K_hrf_bases + 1):(n * K_hrf_bases)
      Z_stack[((n - 1) * V_sl + 1):(n * V_sl), ] <- t(Z_sl_raw[rows, , drop = FALSE])
    }
    C_z <- crossprod(Z_stack) / (nrow(Z_stack) - 1)
    eig <- eigen(C_z)
    w_sl <- eig$vectors[, 1]
    w_sl <- w_sl / sqrt(sum(w_sl^2))
    for (n in seq_len(N_trials)) {
      rows <- ((n - 1) * K_hrf_bases + 1):(n * K_hrf_bases)
      A_sl[n, ] <- crossprod(w_sl, Z_sl_raw[rows, , drop = FALSE])
    }
  } else if (method == "optim") {
    if (is.null(classifier_for_w_optim) || is.null(labels_for_w_optim)) {
      stop("classifier_for_w_optim and labels_for_w_optim must be provided for method='optim'")
    }
    Z_arr <- array(Z_sl_raw, dim = c(K_hrf_bases, N_trials, V_sl))
    fn_gr <- function(w) {
      A_tmp <- apply(Z_arr, c(2, 3), function(z) sum(z * w))
      res <- classifier_for_w_optim(A_tmp, labels_for_w_optim)
      grad_A <- res$grad
      grad_w <- vapply(seq_along(w), function(j) {
        sum(Z_arr[j, , ] * grad_A)
      }, numeric(1))
      list(value = res$loss, grad = grad_w)
    }
    cached <- make_cached_fn_gr(fn_gr)
    if (is.null(optim_w_params$maxit)) optim_w_params$maxit <- 5
    opt_res <- stats::optim(par = rep(1 / sqrt(K_hrf_bases), K_hrf_bases),
                            fn = cached$fn,
                            gr = cached$gr,
                            method = "L-BFGS-B",
                            control = optim_w_params)
    w_sl <- opt_res$par
    w_sl <- w_sl / sqrt(sum(w_sl^2))
    for (n in seq_len(N_trials)) {
      rows <- ((n - 1) * K_hrf_bases + 1):(n * K_hrf_bases)
      A_sl[n, ] <- crossprod(w_sl, Z_sl_raw[rows, , drop = FALSE])
    }
  }

  diag_list <- NULL
  if (diagnostics) {
    dl <- list(method = method, w_sl = w_sl)
    if (exists("opt_res")) {
      dl$optim_details <- opt_res
    }
    diag_list <- cap_diagnostics(dl)
  }

  list(A_sl = A_sl, w_sl = w_sl, diag_data = diag_list)
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
