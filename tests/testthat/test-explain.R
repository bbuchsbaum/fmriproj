context("explain projection results")

test_that("explain_projection_results returns maps", {
  sl_res <- list(diagnostics = list(
    list(lambda_sl = 0.5, w_sl = c(1,0)),
    list(lambda_sl = 0.6, w_sl = c(0,1))
  ))
  basis <- matrix(c(1,0,
                    0,1), nrow = 2, byrow = FALSE)
  res <- explain_projection_results(sl_res, mask_dims = c(2), hrf_basis_matrix = basis)
  expect_equal(length(res$lambda_map), 2)
  expect_equal(length(res$w_maps[[1]]), 2)
  expect_equal(length(res$effective_hrf), nrow(basis))
})

test_that("explain_projection_results inserts NA for missing w_sl", {
  sl_res <- list(diagnostics = list(
    list(lambda_sl = 0.5, w_sl = c(1,0)),
    list(lambda_sl = 0.6, w_sl = NULL),
    list(lambda_sl = 0.7, w_sl = c(0,1))
  ))
  res <- explain_projection_results(sl_res, mask_dims = c(3))
  expect_equal(length(res$w_maps[[1]]), 3)
  expect_true(is.na(res$w_maps[[1]][2]))
})
