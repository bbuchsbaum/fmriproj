context("plot_searchlight_diagnostics")

test_that("plot_searchlight_diagnostics errors when ggplot2 is missing", {
  results <- list(diagnostics = list(list(lambda_sl = 0.5, w_sl = c(1, 2))))
  expect_error(
    plot_searchlight_diagnostics(results, voxel = 1),
    "ggplot2 required for plotting"
  )
})

test_that("get_searchlight_index uses voxel_indices", {
  res <- list(
    diagnostics = list(
      list(lambda_sl = 0.5, w_sl = c(1,2)),
      list(lambda_sl = 0.6, w_sl = c(3,4))
    ),
    voxel_indices = c(10, 20)
  )
  expect_equal(get_searchlight_index(res, 20), 2L)
})

test_that("plot_searchlight_diagnostics returns ggplot", {
  skip_if_not_installed("ggplot2")
  res <- list(
    diagnostics = list(list(lambda_sl = 0.5, w_sl = c(1,2))),
    voxel_indices = 5
  )
  p <- plot_searchlight_diagnostics(res, voxel = 5, plot_type = "weights")
  expect_s3_class(p, "ggplot")
})
