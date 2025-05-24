context("integration pipeline")

test_that("adaptive lambda and w optimization with theta optimization", {
  set.seed(1)
  Y <- matrix(rnorm(12), nrow = 6, ncol = 2)
  em <- list(onsets = c(0L,3L), n_time = 6L, basis_length = 2L)
  basis_fun <- function(theta, t) matrix(theta[1], nrow = length(t), ncol = 1)
  clf <- function(A, y) {
    pred <- A[,1]
    loss <- sum((pred - y)^2)
    grad <- matrix(2*(pred - y), nrow = length(y), ncol = ncol(A))
    list(loss=loss, grad=grad)
  }
  inner_fn <- function(A) sum(A)
  res <- optimize_hrf_mvpa(theta_init = c(1),
                           Y = Y,
                           event_model = em,
                           inner_cv_fn = inner_fn,
                           hrf_basis_func = basis_fun,
                           lambda_global = 0.1,
                           lambda_adaptive_method = "EB",
                           collapse_method = "optim",
                           labels_for_w_optim = c(1,0),
                           classifier_for_w_optim = clf,
                           optim_w_params = list(maxit = 5),
                           diagnostics = TRUE,
                           optim_method = "Nelder-Mead")
  expect_true(is.numeric(res$theta_hat))
  expect_true(!is.null(res$diagnostics$theta_trace))
})


test_that("diagnostic memory ceiling works", {
  old <- options(fmriproj.diagnostics_memory_limit = 1)
  on.exit(options(old), add = TRUE)
  em <- list(onsets=c(0L), n_time=2L)
  basis <- matrix(1, nrow=1, ncol=1)
  res <- build_design_matrix(em, hrf_basis_matrix = basis, diagnostics = TRUE)
  expect_null(attr(res, "diagnostics"))
})
