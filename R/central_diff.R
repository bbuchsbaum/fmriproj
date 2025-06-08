#' Central differences with boundary handling
#'
#' Computes derivatives of a numeric vector using a central difference scheme
#' that supports irregular spacing. Forward and backward differences are used
#' at the boundaries.
#'
#' @param y Numeric vector to differentiate.
#' @param t Numeric vector of sampling points corresponding to `y`.
#' @param order Derivative order to compute. Either 1 or 2. Defaults to 1.
#'
#' @return Numeric vector of the same length as `y` containing the derivative.
#' @keywords internal
central_diff <- function(y, t, order = 1) {
  stopifnot(length(y) == length(t))
  n <- length(y)
  if (n <= 1) return(numeric(n))

  deriv <- numeric(n)
  deriv[1] <- (y[2] - y[1]) / (t[2] - t[1])
  deriv[n] <- (y[n] - y[n - 1]) / (t[n] - t[n - 1])
  if (n > 2) {
    deriv[2:(n - 1)] <- (y[3:n] - y[1:(n - 2)]) / (t[3:n] - t[1:(n - 2)])
  }

  if (order == 1) {
    deriv
  } else if (order == 2) {
    central_diff(deriv, t, order = 1)
  } else {
    stop("order must be 1 or 2")
  }
}
