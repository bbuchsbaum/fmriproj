#' Parameterized SPMG3 HRF basis
#'
#' Generates a simple SPMG3-style HRF basis with optional scaling of
#' the canonical delay and dispersion parameters via `theta`.
#'
#' @param theta Numeric vector of length 1 or 2 controlling delay and
#'   dispersion scaling. If of length 1, the same value is used for both
#'   delay and dispersion. Defaults to `c(1, 1)`.
#' @param t Numeric vector of time points at which to evaluate the basis.
#'
#' @return Matrix with length(t) rows and 3 columns: canonical HRF,
#'   temporal derivative, and dispersion derivative.
#'
#' Derivatives are estimated using a central difference scheme that
#' accommodates irregularly spaced `t`. Forward and backward
#' differences are applied at the boundaries.
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

  n <- length(hrf)
  deriv1 <- numeric(n)
  deriv2 <- numeric(n)

  if (n > 1) {
    deriv1[1] <- (hrf[2] - hrf[1]) / (t[2] - t[1])
    deriv1[n] <- (hrf[n] - hrf[n - 1]) / (t[n] - t[n - 1])
    if (n > 2) {
      deriv1[2:(n - 1)] <- (hrf[3:n] - hrf[1:(n - 2)]) /
        (t[3:n] - t[1:(n - 2)])
    }

    deriv2[1] <- (deriv1[2] - deriv1[1]) / (t[2] - t[1])
    deriv2[n] <- (deriv1[n] - deriv1[n - 1]) / (t[n] - t[n - 1])
    if (n > 2) {
      deriv2[2:(n - 1)] <- (deriv1[3:n] - deriv1[1:(n - 2)]) /
        (t[3:n] - t[1:(n - 2)])
    }
  }

  cbind(hrf, deriv1, deriv2)
}
