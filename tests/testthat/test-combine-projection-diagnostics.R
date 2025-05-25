context("combine_projection_diagnostics")

test_that("diagnostics retained per result", {
  y1 <- list(A_sl = matrix(1), diag_data = list(lambda_sl = 0.1))
  out <- combine_projection_diagnostics(NULL, y1)
  y2 <- list(A_sl = matrix(2), diag_data = list(lambda_sl = 0.2))
  out <- combine_projection_diagnostics(out, y2)
  expect_equal(length(out$results), 2L)
  expect_equal(length(out$diagnostics), 2L)
  expect_equal(out$diagnostics[[1]]$lambda_sl, 0.1)
  expect_equal(out$diagnostics[[2]]$lambda_sl, 0.2)
})

test_that("NULL diagnostics preserve length", {
  old <- options(fmriproj.diagnostics_memory_limit = 0)
  on.exit(options(old), add = TRUE)
  y1 <- list(A_sl = matrix(1), diag_data = list(lambda_sl = 0.1))
  res <- combine_projection_diagnostics(NULL, y1)
  expect_equal(length(res$diagnostics), 1L)
  expect_null(res$diagnostics[[1]])
})
