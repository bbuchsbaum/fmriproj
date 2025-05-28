context("cap_diagnostics")

test_that("cap_diagnostics discards large diagnostics", {
  diag_list <- list(a = rnorm(100))
  expect_warning(
    res <- cap_diagnostics(diag_list, memory_limit = object.size(diag_list) - 10),
    "Diagnostics exceed"
  )
  expect_null(res)
})
