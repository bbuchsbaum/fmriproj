context("run API")

em <- list(onsets = 1L, n_time = 2L, conditions = 1L)
Y <- matrix(1, nrow = 2, ncol = 1)

mask <- matrix(1)

# These functions require rMVPA, so check that they error when missing

test_that("run_searchlight errors without rMVPA", {
  expect_error(run_searchlight(Y, em, mask), "rMVPA package required")
})

test_that("run_regional errors without rMVPA", {
  expect_error(run_regional(Y, em, mask), "rMVPA package required")
})
