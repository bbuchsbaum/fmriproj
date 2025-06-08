context("central_diff")

test_that("first derivative matches manual difference", {
  t <- 0:4
  y <- t^2
  manual <- numeric(length(y))
  manual[1] <- (y[2] - y[1]) / (t[2] - t[1])
  manual[length(y)] <- (y[length(y)] - y[length(y)-1]) / (t[length(y)] - t[length(y)-1])
  if (length(y) > 2) {
    manual[2:(length(y)-1)] <- (y[3:length(y)] - y[1:(length(y)-2)]) /
      (t[3:length(y)] - t[1:(length(y)-2)])
  }
  expect_equal(central_diff(y, t), manual)
})

test_that("second derivative computed recursively", {
  t <- 0:4
  y <- t^3
  d1 <- central_diff(y, t)
  manual_d2 <- central_diff(d1, t)
  expect_equal(central_diff(y, t, order = 2), manual_d2)
})
