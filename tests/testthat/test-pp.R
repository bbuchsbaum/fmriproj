context("progressive projection pursuit")

test_that("fit_pp and predict_pp work for LDA", {
  set.seed(1)
  A <- rbind(matrix(rnorm(10, mean = -1), ncol = 2),
             matrix(rnorm(10, mean = 1), ncol = 2))
  labels <- rep(c("a", "b"), each = 5)
  model <- fit_pp(A, labels, method = "LDA", dims = 1)
  proj <- predict_pp(model, A)
  expect_equal(ncol(proj), 1)
  expect_equal(nrow(proj), nrow(A))
})

test_that("fit_pp and predict_pp work for PLS-DA", {
  set.seed(2)
  A <- matrix(rnorm(20), ncol = 2)
  labels <- rep(c("a", "b"), each = 5)
  model <- fit_pp(A, labels, method = "PLS-DA", dims = 1)
  proj <- predict_pp(model, A)
  expect_equal(ncol(proj), 1)
  expect_equal(nrow(proj), nrow(A))
})

test_that("fit_pp handles single class by returning identity", {
  A <- matrix(rnorm(10), ncol = 2)
  labels <- rep("a", nrow(A))
  model <- fit_pp(A, labels, method = "LDA", dims = 2)
  expect_equal(model$W, diag(ncol(A)))
})
