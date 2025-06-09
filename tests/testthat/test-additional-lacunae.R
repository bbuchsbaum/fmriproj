library(testthat)

# Additional coverage tests

# build_design_matrix missing basis

test_that("build_design_matrix requires HRF basis specification", {
  em <- list(onsets = c(0L), n_time = 3L)
  expect_error(build_design_matrix(em), "Provide either")
})

# build_design_matrix warns when columns exceed max_X_cols

test_that("build_design_matrix warns when exceeding max_X_cols", {
  em <- list(onsets = 0:2, n_time = 6L)
  basis <- matrix(1, nrow = 2, ncol = 1)
  expect_warning(build_design_matrix(em, hrf_basis_matrix = basis, max_X_cols = 2),
                 "max_X_cols")
})

# adaptive_ridge_projector validation branches

test_that("adaptive_ridge_projector validates lambda method", {
  em <- list(onsets = c(0L), n_time = 2L)
  basis <- matrix(1, nrow = 1, ncol = 1)
  X <- build_design_matrix(em, hrf_basis_matrix = basis)$X
  proj <- build_projector(X)
  Y <- matrix(1, nrow = 2, ncol = 1)
  expect_error(adaptive_ridge_projector(Y, proj, lambda_adaptive_method = "bogus"),
               "Unknown")
})

test_that("adaptive_ridge_projector checks lambda_floor_global", {
  em <- list(onsets = c(0L), n_time = 2L)
  basis <- matrix(1, nrow = 1, ncol = 1)
  X <- build_design_matrix(em, hrf_basis_matrix = basis)$X
  proj <- build_projector(X)
  Y <- matrix(1, nrow = 2, ncol = 1)
  expect_error(adaptive_ridge_projector(Y, proj, lambda_floor_global = -1),
               "non-negative")
})

test_that("adaptive_ridge_projector requires X for EB method", {
  em <- list(onsets = c(0L), n_time = 2L)
  basis <- matrix(1, nrow = 1, ncol = 1)
  X <- build_design_matrix(em, hrf_basis_matrix = basis)$X
  proj <- build_projector(X)
  Y <- matrix(1, nrow = 2, ncol = 1)
  expect_error(adaptive_ridge_projector(Y, proj, lambda_adaptive_method = "EB"),
               "X_theta_for_EB_residuals")
})

# collapse_beta validation branches

test_that("collapse_beta validates method", {
  Z <- matrix(1, nrow = 2, ncol = 1)
  expect_error(collapse_beta(Z, N_trials = 1, K_hrf_bases = 2, method = "bad"),
               "arg")
})

test_that("collapse_beta optim requires labels and classifier", {
  Z <- matrix(1, nrow = 2, ncol = 1)
  expect_error(collapse_beta(Z, N_trials = 1, K_hrf_bases = 2, method = "optim"),
               "classifier_for_w_optim")
})

# predict_pp dimension check

test_that("predict_pp checks dimensions", {
  model <- list(W = matrix(1, nrow = 2, ncol = 1))
  expect_error(predict_pp(model, matrix(1, nrow = 1, ncol = 3)), "TRUE")
})

# optimize_hrf_mvpa inner function validation

test_that("optimize_hrf_mvpa validates inner_cv_fn return", {
  Y <- matrix(1, nrow = 2, ncol = 1)
  em <- list(onsets = c(0L), n_time = 2L, basis_length = 1L)
  basis_fun <- function(theta, t) matrix(1, nrow = length(t), ncol = 1)
  bad_fn <- function(A) NA
  expect_error(
    optimize_hrf_mvpa(theta_init = c(1),
                      Y = Y,
                      event_model = em,
                      inner_cv_fn = bad_fn,
                      hrf_basis_func = basis_fun,
                      optim_method = "Brent",
                      lower = 0.5,
                      upper = 2),
    "must return"
  )
})

# fit_pp method validation

test_that("fit_pp rejects unknown methods", {
  A <- matrix(rnorm(4), ncol = 2)
  labs <- c("a", "b")
  expect_error(fit_pp(A, labs, method = "bogus"), "method must be")
})

# collapse_beta PC fallback when eigen fails

test_that("collapse_beta pc falls back to rss on eigen failure", {
  Z <- matrix(c(1, NA, 3, 4), nrow = 4, ncol = 1)
  expect_warning(res <- collapse_beta(Z, N_trials = 2, K_hrf_bases = 2,
                                      method = "pc"), "Z_sl_raw contains missing values")
  expect_equal(res$w_sl, rep(1 / sqrt(2), 2))
  expect_true(all(is.na(res$A_sl)))
})


# optimize_hrf_mvpa argument validation

test_that("optimize_hrf_mvpa validates arguments", {
  Y <- matrix(1, nrow = 2, ncol = 1)
  em <- list(onsets = c(0L), n_time = 2L, basis_length = 1L)
  basis_fun <- function(theta, t) matrix(1, nrow = length(t), ncol = 1)
  inner_fn <- sum

  expect_error(
    optimize_hrf_mvpa(theta_init = "a",
                      Y = Y,
                      event_model = em,
                      inner_cv_fn = inner_fn,
                      hrf_basis_func = basis_fun,
                      optim_method = "Nelder-Mead"),
    "theta_init must be numeric"
  )

  bad_Y <- matrix("a", nrow = 2, ncol = 1)
  expect_error(
    optimize_hrf_mvpa(theta_init = c(1),
                      Y = bad_Y,
                      event_model = em,
                      inner_cv_fn = inner_fn,
                      hrf_basis_func = basis_fun,
                      optim_method = "Nelder-Mead"),
    "Y must be a numeric matrix"
  )
  expect_error(
    optimize_hrf_mvpa(theta_init = c(1),
                      Y = Y,
                      event_model = em,
                      inner_cv_fn = 5,
                      hrf_basis_func = basis_fun,
                      optim_method = "Nelder-Mead"),
    "inner_cv_fn must be a function"
  )

  expect_error(
    optimize_hrf_mvpa(theta_init = c(1),
                      Y = Y,
                      event_model = em,
                      inner_cv_fn = inner_fn,
                      hrf_basis_func = 5,
                      optim_method = "Nelder-Mead"),
    "hrf_basis_func must be a function"
  )

  em_bad <- list(onsets = c(0L))
  expect_error(
    optimize_hrf_mvpa(theta_init = c(1),
                      Y = Y,
                      event_model = em_bad,
                      inner_cv_fn = inner_fn,
                      hrf_basis_func = basis_fun,
                      optim_method = "Nelder-Mead"),
    "event_model missing required fields"
  )
})

test_that("optimize_hrf_mvpa validates optim_method", {
  Y <- matrix(1, nrow = 2, ncol = 1)
  em <- list(onsets = c(0L), n_time = 2L, basis_length = 1L)
  basis_fun <- function(theta, t) matrix(theta[1], nrow = length(t), ncol = 1)
  expect_error(
    optimize_hrf_mvpa(theta_init = c(1),
                      Y = Y,
                      event_model = em,
                      inner_cv_fn = sum,
                      hrf_basis_func = basis_fun,
                      optim_method = "bogus"),
    "one of"
  )
})

test_that("optimize_hrf_mvpa warns when bounds ignored", {
  Y <- matrix(1, nrow = 2, ncol = 1)
  em <- list(onsets = c(0L), n_time = 2L, basis_length = 1L)
  basis_fun <- function(theta, t) matrix(theta[1], nrow = length(t), ncol = 1)
  expect_warning(
    optimize_hrf_mvpa(theta_init = c(1),
                      Y = Y,
                      event_model = em,
                      inner_cv_fn = sum,
                      hrf_basis_func = basis_fun,
                      optim_method = "Nelder-Mead",
                      lower = 0,
                      upper = 1),
    "ignored"
  )
})
