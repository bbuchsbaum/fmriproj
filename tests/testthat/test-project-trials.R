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

test_that("project_trials honors custom lambda_global", {
  set.seed(1)
  Y <- matrix(rnorm(6), nrow = 6, ncol = 1)
  em <- list(onsets = c(0L, 2L), n_time = 6L)
  basis <- matrix(c(1,0,0,
                    0,1,0), nrow = 3, byrow = FALSE)
  res_default <- suppressWarnings(
    project_trials(Y, em,
                   lambda_method = "none",
                   collapse_method = "rss",
                   hrf_basis = basis,
                   verbose = FALSE)
  )
  res_custom <- suppressWarnings(
    project_trials(Y, em,
                   lambda_method = "none",
                   collapse_method = "rss",
                   lambda_global = 1,
                   hrf_basis = basis,
                   verbose = FALSE)
  )
  expect_false(isTRUE(all.equal(res_default, res_custom)))
})
