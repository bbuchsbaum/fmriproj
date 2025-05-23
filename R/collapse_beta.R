#' Collapse Projected Betas Across HRF Bases
#'
#' Collapses the raw projected coefficients `Z_sl_raw` into trial-wise
#' feature patterns. Supports the default root-sum-square (`method = "rss"`)
#' collapse and an SNR-optimal principal component (`method = "pc"`).
#'
#' @param Z_sl_raw Matrix of raw projected coefficients with
#'   `(N_trials * K_hrf_bases)` rows and `V_sl` columns.
#' @param N_trials Number of trials.
#' @param K_hrf_bases Number of HRF basis functions used.
#' @param method Collapse method. Either "rss" or "pc".
#' @param diagnostics Logical; return diagnostic information.
#' @return A list with elements:
#'   \item{A_sl}{Collapsed trial pattern matrix `N_trials x V_sl`.}
#'   \item{w_sl}{Collapse weights (NULL for "rss").}
#'   \item{diag_data}{Optional diagnostics.}
#' @export
collapse_beta <- function(Z_sl_raw, N_trials, K_hrf_bases,
                          method = "rss", diagnostics = FALSE) {
  if (!method %in% c("rss", "pc")) {
    stop("method must be either 'rss' or 'pc'")
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
  }

  diag_list <- NULL
  if (diagnostics) {
    diag_list <- list(method = method, w_sl = w_sl)
  }

  list(A_sl = A_sl, w_sl = w_sl, diag_data = diag_list)
}
