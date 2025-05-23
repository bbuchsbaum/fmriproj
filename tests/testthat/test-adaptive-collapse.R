context("adaptive_ridge_projector and collapse_beta")

test_that("adaptive_ridge_projector with method none works", {
  em <- list(onsets = c(0L,2L), n_time = 6L)
  basis <- matrix(c(1,0,0,
                    0,1,0), nrow = 3, byrow = FALSE)
  X <- make_trialwise_X(em, hrf_basis_matrix = basis)$X
  proj <- build_projector(X)
  Y_sl <- matrix(1, nrow = 6, ncol = 2)
  res <- adaptive_ridge_projector(Y_sl, proj, lambda_adaptive_method = "none",
                                  lambda_floor_global = 0.5, diagnostics = TRUE)
  expect_equal(dim(res$Z_sl_raw), c(ncol(X), 2L))
  diag <- res$diag_data
  expect_true(!is.null(diag))
  expect_equal(diag$lambda_sl_chosen, 0.5)
})

test_that("collapse_beta rss works", {
  N_trials <- 2
  K <- 2
  Z_sl_raw <- matrix(c(1,2,3,4), nrow = N_trials*K, ncol = 1)
  res <- collapse_beta(Z_sl_raw, N_trials, K, method = "rss")
  A_sl <- res$A_sl
  expect_equal(dim(A_sl), c(N_trials,1))
  expected <- c(sqrt(1^2 + 2^2), sqrt(3^2 + 4^2))
  expect_equal(as.numeric(A_sl[,1]), expected)
})
