context("build_projector")

test_that("build_projector sparse QR works", {
  em <- list(onsets = c(0L, 2L), n_time = 6L)
  basis <- matrix(c(1,0,0,
                    0,1,0), nrow = 3, byrow = FALSE)
  X <- build_design_matrix(em, hrf_basis_matrix = basis)$X
  proj <- build_projector(X)
  qr_obj <- qr(X)
  Qt_exp <- t(qr.Q(qr_obj))
  R_exp <- qr.R(qr_obj)
  expect_equal(proj$Qt, Qt_exp)
  expect_equal(proj$R, R_exp)
})

test_that("build_projector applies ridge", {
  em <- list(onsets = c(0L,2L), n_time = 6L)
  basis <- matrix(c(1,0,0,
                    0,1,0), nrow = 3, byrow = FALSE)
  X <- build_design_matrix(em, hrf_basis_matrix = basis)$X
  lambda <- 0.5
  proj <- build_projector(X, lambda_global = lambda)
  qr_obj <- qr(X)
  Qt <- t(qr.Q(qr_obj))
  R <- qr.R(qr_obj)
  K_exp <- solve(crossprod(R) + diag(lambda, ncol(R)), t(R) %*% Qt)
  expect_equal(proj$K_global, K_exp)
})

test_that("build_projector diagnostics", {
  em <- list(onsets = c(0L), n_time = 2L)
  basis <- matrix(1, nrow = 1, ncol = 1)
  X <- build_design_matrix(em, hrf_basis_matrix = basis)$X
  proj <- build_projector(X, diagnostics = TRUE)
  diag <- attr(proj, "diagnostics")
  expect_true(!is.null(diag))
  expect_equal(diag$cond_R, kappa(proj$R))
})

test_that("build_projector warns on high condition number", {
  X <- Matrix::Matrix(matrix(c(1,1,1,1), 2, 2), sparse = TRUE)
  expect_warning(build_projector(X))
})
