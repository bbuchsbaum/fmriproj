context("hrf_basis_spmg3_theta")

test_that("returned matrix has expected dimensions", {
  t <- seq(0, 10, by = 0.5)
  B <- hrf_basis_spmg3_theta(t = t)
  expect_equal(dim(B), c(length(t), 3))
})

test_that("normalization to max=1 works", {
  t <- seq(0, 30, by = 0.1)
  B <- hrf_basis_spmg3_theta(t = t)
  expect_equal(max(B[, 1]), 1)
})

test_that("derivatives behave as expected for simple t", {
  t <- 0:4
  B <- hrf_basis_spmg3_theta(t = t)

  p1 <- 6
  p2 <- 16
  d1 <- 1
  d2 <- 1
  hrf <- stats::dgamma(t, shape = p1, rate = d1) -
    0.35 * stats::dgamma(t, shape = p2, rate = d2)
  hrf_max <- max(hrf)
  if (hrf_max != 0) hrf <- hrf / hrf_max
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

  expected <- cbind(hrf, deriv1, deriv2)

  expect_equal(B, expected)
})
