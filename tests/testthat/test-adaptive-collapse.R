context("adaptive_ridge_projector and collapse_beta")

test_that("adaptive_ridge_projector with method none works", {
  em <- list(onsets = c(0L,2L), n_time = 6L)
  basis <- matrix(c(1,0,0,
                    0,1,0), nrow = 3, byrow = FALSE)
  X <- build_design_matrix(em, hrf_basis_matrix = basis)$X
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

test_that("adaptive_ridge_projector EB works", {
  em <- list(onsets = c(0L,2L), n_time = 6L)
  basis <- matrix(c(1,0,0,
                    0,1,0), nrow = 3, byrow = FALSE)
  X_obj <- build_design_matrix(em, hrf_basis_matrix = basis)
  X <- as.matrix(X_obj$X)
  proj <- build_projector(X)
  Y_sl <- matrix(rnorm(12), nrow = 6, ncol = 2)
  res <- adaptive_ridge_projector(Y_sl, proj,
                                  lambda_adaptive_method = "EB",
                                  lambda_floor_global = 0.1,
                                  X_theta_for_EB_residuals = X,
                                  diagnostics = TRUE)
  expect_equal(dim(res$Z_sl_raw), c(ncol(X), 2L))
  diag <- res$diag_data
  expect_true(!is.null(diag))
  expect_true(is.finite(diag$lambda_sl_chosen))
  expect_true(is.finite(diag$s_n_sq))
  expect_true(is.finite(diag$s_b_sq))
})

test_that("adaptive_ridge_projector LOOcv_local works", {
  em <- list(onsets = c(0L,2L), n_time = 6L)
  basis <- matrix(c(1,0,0,
                    0,1,0), nrow = 3, byrow = FALSE)
  X_obj <- build_design_matrix(em, hrf_basis_matrix = basis)
  X <- as.matrix(X_obj$X)
  proj <- build_projector(X)
  Y_sl <- matrix(rnorm(12), nrow = 6, ncol = 2)
  res <- adaptive_ridge_projector(Y_sl, proj,
                                  lambda_adaptive_method = "LOOcv_local",
                                  lambda_floor_global = 0.1,
                                  X_theta_for_EB_residuals = X,
                                  diagnostics = TRUE)
  expect_equal(dim(res$Z_sl_raw), c(ncol(X), 2L))
  expect_true(is.finite(res$diag_data$lambda_sl_chosen))
})

test_that("collapse_beta pc works", {
  N_trials <- 2
  K <- 2
  Z_sl_raw <- matrix(c(1,2,3,4), nrow = N_trials*K, ncol = 1)
  res <- collapse_beta(Z_sl_raw, N_trials, K, method = "pc", diagnostics = TRUE)
  expect_equal(length(res$w_sl), K)
  expect_equal(dim(res$A_sl), c(N_trials,1))
  expect_equal(res$diag_data$w_sl, res$w_sl)
})

test_that("collapse_beta pc handles small N_trials", {
  N_trials <- 1
  K <- 2
  Z_sl_raw <- matrix(c(1, 2), nrow = N_trials * K, ncol = 1)
  expect_warning(res <- collapse_beta(Z_sl_raw, N_trials, K, method = "pc"),
                 "Not enough")
  expect_equal(dim(res$A_sl), c(N_trials, 1))
  expect_true(all(res$A_sl == 0))
  expect_true(all(res$w_sl == 0))
})

test_that("collapse_beta optim works", {
  N_trials <- 3
  K <- 2
  Z_sl_raw <- matrix(c(1,0,
                        0,1,
                        1,0), nrow = N_trials*K, byrow = TRUE)
  labels <- c(1,0,1)
  clf <- function(A, y) {
    pred <- A[,1]
    loss <- sum((pred - y)^2)
    grad <- matrix(2*(pred - y), nrow = length(y), ncol = ncol(A))
    list(loss = loss, grad = grad)
  }
  res <- collapse_beta(Z_sl_raw, N_trials, K, method = "optim",
                       labels_for_w_optim = labels,
                       classifier_for_w_optim = clf,
                       optim_w_params = list(maxit = 20),
                       diagnostics = TRUE)
  expect_equal(dim(res$A_sl), c(N_trials, 1))
  expect_true(abs(res$w_sl[1] - 1) < 1e-3)
  expect_true(abs(res$w_sl[2]) < 1e-3)
  expect_true(!is.null(res$diag_data$optim_details))
})

