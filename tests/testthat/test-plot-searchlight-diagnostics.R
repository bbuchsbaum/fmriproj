context("plot_searchlight_diagnostics")

test_that("plot_searchlight_diagnostics errors when ggplot2 is missing", {
  results <- list(diagnostics = list(list(lambda_sl = 0.5, w_sl = c(1, 2))))
  get_searchlight_index <- function(results, voxel) 1
  expect_error(
    plot_searchlight_diagnostics(results, voxel = 1),
    "ggplot2 required for plotting"
  )
})
