context("build_projector")

test_that("build_projector sparse QR works", {
  em <- list(onsets = c(0L, 2L), n_time = 6L)
  basis <- matrix(c(1,0,0,
                    0,1,0), nrow = 3, byrow = FALSE)
  X <- build_design_matrix(em, hrf_basis_matrix = basis)$X
  proj <- build_projector(X, pivot = TRUE)
  qr_obj <- Matrix::qr(X)
  Qt_exp <- t(Matrix::qr.Q(qr_obj))
  R_exp <- Matrix::qr.R(qr_obj)
  pivot_idx <- tryCatch(qr_obj@q + 1L, error = function(e) NULL)
  if (!is.null(pivot_idx) && any(pivot_idx != seq_len(ncol(R_exp)))) {
    R_exp <- R_exp[, order(pivot_idx), drop = FALSE]
  }
  expect_equal(proj$Qt, Qt_exp)
  expect_equal(proj$R, R_exp)
  K_exp <- solve(R_exp, Qt_exp)
  expect_equal(proj$K_global, K_exp)
})

test_that("build_projector can disable pivoting", {
  em <- list(onsets = c(0L, 2L), n_time = 6L)
  basis <- matrix(c(1,0,0,
                    0,1,0), nrow = 3, byrow = FALSE)
  X <- build_design_matrix(em, hrf_basis_matrix = basis)$X
  proj <- build_projector(X, pivot = FALSE)
  qr_obj <- qr(as.matrix(X))
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
  qr_obj <- Matrix::qr(X)
  Qt <- t(Matrix::qr.Q(qr_obj))
  R <- Matrix::qr.R(qr_obj)
  pivot_idx <- tryCatch(qr_obj@q + 1L, error = function(e) NULL)
  if (!is.null(pivot_idx) && any(pivot_idx != seq_len(ncol(R)))) {
    R <- R[, order(pivot_idx), drop = FALSE]
  }
  K_exp <- solve(crossprod(R) + diag(lambda, ncol(R)), t(R) %*% Qt)
  expect_equal(proj$K_global, K_exp)
})

test_that("lambda_global 0 returns OLS projector", {
  em <- list(onsets = c(0L,2L), n_time = 6L)
  basis <- matrix(c(1,0,0,
                    0,1,0), nrow = 3, byrow = FALSE)
  X <- build_design_matrix(em, hrf_basis_matrix = basis)$X
  proj <- build_projector(X, lambda_global = 0)
  qr_obj <- Matrix::qr(X)
  Qt <- t(Matrix::qr.Q(qr_obj))
  R <- Matrix::qr.R(qr_obj)
  pivot_idx <- tryCatch(qr_obj@q + 1L, error = function(e) NULL)
  if (!is.null(pivot_idx) && any(pivot_idx != seq_len(ncol(R)))) {
    R <- R[, order(pivot_idx), drop = FALSE]
  }
  K_exp <- solve(R, Qt)
  expect_equal(proj$K_global, K_exp)
})

test_that("build_projector ridge uses sparse diagonal", {
  em <- list(onsets = c(0L,2L), n_time = 6L)
  basis <- matrix(c(1,0,0,
                    0,1,0), nrow = 3, byrow = FALSE)
  X <- build_design_matrix(em, hrf_basis_matrix = basis)$X
  lambda <- 0.5
  proj <- build_projector(X, lambda_global = lambda)
  qr_obj <- Matrix::qr(X)
  Qt <- t(Matrix::qr.Q(qr_obj))
  R <- Matrix::qr.R(qr_obj)
  pivot_idx <- tryCatch(qr_obj@q + 1L, error = function(e) NULL)
  if (!is.null(pivot_idx) && any(pivot_idx != seq_len(ncol(R)))) {
    R <- R[, order(pivot_idx), drop = FALSE]
  }
  K_exp <- solve(crossprod(R) + Matrix::Diagonal(ncol(R), lambda),
                 t(R) %*% Qt)
  expect_equal(proj$K_global, K_exp)
})

test_that("build_projector diagnostics", {
  em <- list(onsets = c(0L), n_time = 2L)
  basis <- matrix(1, nrow = 1, ncol = 1)
  X <- build_design_matrix(em, hrf_basis_matrix = basis)$X
  proj <- build_projector(X, diagnostics = TRUE)
  diag <- attr(proj, "diagnostics")
  expect_true(!is.null(diag))
  cond_exp <- 1 / Matrix::rcond(proj$R)
  expect_equal(diag$cond_R, cond_exp)
})

test_that("build_projector warns on high condition number", {
  X <- Matrix::Matrix(matrix(c(1,1,1,1), 2, 2), sparse = TRUE)
  expect_warning(build_projector(X))
})


test_that("build_projector uses ginv when R is singular", {
  X <- Matrix::Matrix(matrix(c(1,1,1,1), 2, 2), sparse = TRUE)
  proj <- suppressWarnings(build_projector(X))
  qr_obj <- Matrix::qr(X)
  Qt_exp <- t(Matrix::qr.Q(qr_obj))
  R_exp <- Matrix::qr.R(qr_obj)
  pivot_idx <- tryCatch(qr_obj@q + 1L, error = function(e) NULL)
  if (!is.null(pivot_idx) && any(pivot_idx != seq_len(ncol(R_exp)))) {
    R_exp <- R_exp[, order(pivot_idx), drop = FALSE]
  }
  K_exp <- MASS::ginv(R_exp) %*% Qt_exp
  expect_equal(proj$K_global, K_exp)
})

test_that("build_projector validates inputs", {
  expect_error(build_projector(list()), "X_theta must be")
  X <- matrix(1, nrow = 2, ncol = 2)
  expect_error(build_projector(X, lambda_global = c(1, 2)),
               "lambda_global must be")
  expect_error(build_projector(X, lambda_global = -1),
               "lambda_global must be")

})
