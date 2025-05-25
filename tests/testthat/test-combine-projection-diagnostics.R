context("combine_projection_diagnostics")

test_that("successive calls keep diagnostics aligned", {
  y1 <- list(diag_data = list(a = 1))
  res <- combine_projection_diagnostics(NULL, y1)
  expect_equal(length(res$results), 1L)
  expect_equal(length(res$diagnostics), 1L)

  y2 <- list(diag_data = list(a = 2))
  res <- combine_projection_diagnostics(res, y2)
  expect_equal(length(res$results), 2L)
  expect_equal(length(res$diagnostics), 2L)
})


test_that("capped diagnostics retain placeholder", {
  y <- list(diag_data = list(a = 1))
  res <- combine_projection_diagnostics(NULL, y)

  old <- options(fmriproj.diagnostics_memory_limit = 1)
  on.exit(options(old), add = TRUE)

  y_big <- list(diag_data = list(a = 1))
  res2 <- combine_projection_diagnostics(res, y_big)
  expect_equal(length(res2$results), 2L)
  expect_equal(length(res2$diagnostics), 2L)
  expect_null(res2$diagnostics[[2]])
})
