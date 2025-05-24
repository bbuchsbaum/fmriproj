context("optimize_hrf_mvpa")

test_that("optimize_hrf_mvpa basic flow", {
  Y <- matrix(1, nrow = 6, ncol = 2)
  em <- list(onsets = c(0L, 2L), n_time = 6L, basis_length = 2L)
  basis_fun <- function(theta, t) {
    matrix(theta[1], nrow = length(t), ncol = 1)
  }
  inner_fn <- function(A) {
    sum(A)
  }
  res <- optimize_hrf_mvpa(theta_init = c(1),
                                 Y = Y,
                                 event_model = em,
                                 inner_cv_fn = inner_fn,
                                 hrf_basis_func = basis_fun,
                                 lambda_global = 0,
                                 diagnostics = TRUE,
                                 optim_method = "Nelder-Mead")
  expect_true(is.numeric(res$theta_hat))
  expect_true(!is.null(res$diagnostics$theta_trace))
  expect_true(nrow(res$diagnostics$theta_trace) >= 1)
})
