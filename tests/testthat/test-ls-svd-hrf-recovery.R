context("ls+svd HRF recovery")

test_that("LS+SVD recovers SPMG1 HRF using BSPLINE basis", {
  if (!requireNamespace("fmrireg", quietly = TRUE)) {
    skip("fmrireg not installed")
  }

  set.seed(123)
  sim <- fmrireg::simulate_bold_signal(ncond = 1,
                                       hrf = fmrireg::HRF_SPMG1,
                                       nreps = 40,
                                       isi = c(1, 3),
                                       TR = 1)

  time <- sim$mat[, 1]
  bold <- sim$mat[, 2]

  em <- list(onsets = as.integer(round(sim$onset)),
             n_time = length(time),
             basis_length = 30L)

  X_obj <- build_design_matrix(em,
                               hrf_basis_func = fmrireg::HRF_BSPLINE,
                               theta_params = list(df = 10),
                               sparse = FALSE)
  X <- X_obj$X

  proj <- build_projector(X, lambda_global = 0)
  beta_hat <- drop(proj$K_global %*% bold)
  nbasis <- ncol(X) / length(em$onsets)
  beta_mat <- matrix(beta_hat, ncol = nbasis, byrow = TRUE)

  sv <- svd(beta_mat)
  hrf_est <- sv$u[, 1]
  if (sum(hrf_est) < 0) hrf_est <- -hrf_est

  true_hrf <- fmrireg::HRF_SPMG1(seq_along(hrf_est) - 1)
  expect_gt(cor(hrf_est, true_hrf), 0.9)
})
