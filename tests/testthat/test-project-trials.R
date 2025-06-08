context("project_trials")

test_that("project_trials runs complete pipeline", {
  Y <- matrix(1, nrow = 6, ncol = 1)
  em <- list(onsets = c(0L, 2L), n_time = 6L)
  basis <- matrix(c(1,0,0,
                    0,1,0), nrow = 3, byrow = FALSE)
  res <- suppressWarnings(
    project_trials(Y, em,
                   lambda_method = "none",
                   collapse_method = "rss",
                   hrf_basis = basis,
                   verbose = FALSE)
  )
  expect_equal(dim(res), c(length(em$onsets), ncol(Y)))
  expect_true(all(res >= 0))
})

test_that("project_trials supports EB and LOOcv_local methods", {
  Y <- matrix(1, nrow = 6, ncol = 1)
  em <- list(onsets = c(0L, 2L), n_time = 6L)
  basis <- matrix(c(1,0,0,
                    0,1,0), nrow = 3, byrow = FALSE)

  res_eb <- suppressWarnings(
    project_trials(Y, em,
                   lambda_method = "EB",
                   collapse_method = "rss",
                   hrf_basis = basis,
                   verbose = FALSE)
  )
  expect_equal(dim(res_eb), c(length(em$onsets), ncol(Y)))

  res_cv <- suppressWarnings(
    project_trials(Y, em,
                   lambda_method = "LOOcv_local",
                   collapse_method = "rss",
                   hrf_basis = basis,
                   verbose = FALSE)
  )
  expect_equal(dim(res_cv), c(length(em$onsets), ncol(Y)))
})

test_that("project_trials accepts CV alias", {
  Y <- matrix(1, nrow = 6, ncol = 1)
  em <- list(onsets = c(0L, 2L), n_time = 6L)
  basis <- matrix(c(1,0,0,
                    0,1,0), nrow = 3, byrow = FALSE)
  res <- suppressWarnings(
    project_trials(Y, em,
                   lambda_method = "CV",
                   collapse_method = "rss",
                   hrf_basis = basis,
                   verbose = FALSE)
  )
  expect_equal(dim(res), c(length(em$onsets), ncol(Y)))
})
