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
  if (max(hrf) != 0) hrf <- hrf / max(hrf)
  dt <- mean(diff(t))
  deriv1 <- c(diff(hrf) / dt, 0)
  deriv2 <- c(diff(deriv1) / dt, 0)
  expected <- cbind(hrf, deriv1, deriv2)

  expect_equal(B, expected)
})
