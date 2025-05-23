#' Collapse Projected Betas Across HRF Bases
#'
#' Collapses the raw projected coefficients `Z_sl_raw` into trial-wise
#' feature patterns. Currently only `method = "rss"` (root-sum-square)
#' is implemented.
#'
#' @param Z_sl_raw Matrix of raw projected coefficients with
#'   `(N_trials * K_hrf_bases)` rows and `V_sl` columns.
#' @param N_trials Number of trials.
#' @param K_hrf_bases Number of HRF basis functions used.
#' @param method Collapse method. Only "rss" is implemented.
#' @param diagnostics Logical; return diagnostic information.
#' @return A list with elements:
#'   \item{A_sl}{Collapsed trial pattern matrix `N_trials x V_sl`.}
#'   \item{w_sl}{Collapse weights (NULL for "rss").}
#'   \item{diag_data}{Optional diagnostics.}
#' @export
collapse_beta <- function(Z_sl_raw, N_trials, K_hrf_bases,
                          method = "rss", diagnostics = FALSE) {
  if (method != "rss") {
    stop("Only method = 'rss' is implemented")
  }

  if (nrow(Z_sl_raw) != N_trials * K_hrf_bases) {
    stop("nrow(Z_sl_raw) must equal N_trials * K_hrf_bases")
  }

  V_sl <- ncol(Z_sl_raw)
  A_sl <- matrix(0, nrow = N_trials, ncol = V_sl)

  for (n in seq_len(N_trials)) {
    rows <- ((n - 1) * K_hrf_bases + 1):(n * K_hrf_bases)
    A_sl[n, ] <- sqrt(colSums(Z_sl_raw[rows, , drop = FALSE]^2))
  }

  diag_list <- NULL
  if (diagnostics) {
    diag_list <- list()
  }

  list(A_sl = A_sl, w_sl = NULL, diag_data = diag_list)
}
