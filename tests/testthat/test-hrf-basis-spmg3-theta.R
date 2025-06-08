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

  canonical <- B[, 1]
  deriv1 <- B[, 2]
  deriv2 <- B[, 3]

  # First derivative at a few points
  expect_equal(
    deriv1[1],
    (canonical[2] - canonical[1]) / (t[2] - t[1])
  )
  expect_equal(
    deriv1[3],
    (canonical[4] - canonical[2]) / (t[4] - t[2])
  )
  expect_equal(
    deriv1[5],
    (canonical[5] - canonical[4]) / (t[5] - t[4])
  )

  # Second derivative at the same points
  expect_equal(
    deriv2[1],
    (deriv1[2] - deriv1[1]) / (t[2] - t[1])
  )
  expect_equal(
    deriv2[3],
    (deriv1[4] - deriv1[2]) / (t[4] - t[2])
  )
  expect_equal(
    deriv2[5],
    (deriv1[5] - deriv1[4]) / (t[5] - t[4])
  )
})
