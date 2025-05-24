context("mvpa_projected_searchlight")

test_that("mvpa_projected_searchlight returns FUN and components when rMVPA missing", {
  em <- list(onsets = c(0L,2L), n_time = 6L)
  basis <- matrix(c(1,0,0,
                    0,1,0), nrow = 3, byrow = FALSE)
  Y <- matrix(1, nrow = 6, ncol = 2)
  res <- mvpa_projected_searchlight(Y, em, hrf_basis_matrix = basis,
                                    lambda_global = 0.5, diagnostics = TRUE)
  expect_true(is.function(res$FUN))
  expect_s3_class(res$projector, "fr_projector")
  sl_res <- res$FUN(Y)
  expect_equal(dim(sl_res$A_sl), c(length(em$onsets), ncol(Y)))
  expect_true(!is.null(sl_res$diag_data))
})
