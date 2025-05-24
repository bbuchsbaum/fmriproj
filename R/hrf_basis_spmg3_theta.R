#' Parameterized SPMG3 HRF basis
#'
#' Generates a simple SPMG3-style HRF basis with optional scaling of
#' the canonical delay and dispersion parameters via `theta`.
#'
#' @param theta Numeric vector of length 2 controlling delay and
#'   dispersion scaling. Defaults to `c(1, 1)`.
#' @param t Numeric vector of time points at which to evaluate the basis.
#'
#' @return Matrix with length(t) rows and 3 columns: canonical HRF,
#'   temporal derivative, and dispersion derivative.
#' @export
hrf_basis_spmg3_theta <- function(theta = c(1, 1), t) {
  delay_scale <- theta[1]
  disp_scale <- if (length(theta) >= 2) theta[2] else theta[1]

  p1 <- 6 * delay_scale
  p2 <- 16 * delay_scale
  d1 <- 1 * disp_scale
  d2 <- 1 * disp_scale

  hrf <- stats::dgamma(t, shape = p1, rate = d1) -
    0.35 * stats::dgamma(t, shape = p2, rate = d2)
  if (max(hrf) != 0) hrf <- hrf / max(hrf)

  dt <- if (length(t) > 1) mean(diff(t)) else 1
  deriv1 <- c(diff(hrf) / dt, 0)
  deriv2 <- c(diff(deriv1) / dt, 0)

  cbind(hrf, deriv1, deriv2)
}
