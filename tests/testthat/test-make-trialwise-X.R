test_that("build_design_matrix constructs sparse matrix with basis matrix", {
  em <- list(onsets = c(0L, 2L), n_time = 6L)
  basis <- matrix(c(1, 0, 0,
                    0, 1, 0), nrow = 3, byrow = FALSE)
  res <- build_design_matrix(em, hrf_basis_matrix = basis)
  X <- res$X
  expect_s4_class(X, "dgCMatrix")
  expect_equal(dim(X), c(6L, 6L))
  dense <- as.matrix(X)
  expect_equal(dense[1,1], 1)
  expect_equal(dense[3,4], 1)
})

test_that("build_design_matrix uses hrf_basis_func and theta_params", {
  em <- list(onsets = c(1L), n_time = 5L, basis_length = 3L)
  hfun <- function(theta, t) {
    matrix(theta[1] + t, nrow = 3, ncol = 1)
  }
  res <- build_design_matrix(em, hrf_basis_func = hfun, theta_params = c(2))
  X <- res$X
  dense <- as.matrix(X)
  expect_equal(dense[2,1], 2)
  expect_equal(dense[3,1], 3)
})

test_that("parametric modulation scales columns", {
  em <- list(onsets = c(0L,2L), n_time = 6L,
             modulator = c(1,2))
  basis <- matrix(c(1,0), nrow = 2)
  res <- build_design_matrix(em, hrf_basis_matrix = basis)
  X <- as.matrix(res$X)
  expect_equal(X[1,1], 1)
  expect_equal(X[3,2], 2)
})

test_that("long HRF basis is decimated", {
  em <- list(onsets = c(0L), n_time = 4L, basis_length = 10L)
  basis_fun <- function(theta, t) matrix(1, nrow = length(t), ncol = 1)
  res <- build_design_matrix(em, hrf_basis_func = basis_fun)
  expect_equal(nrow(res$hrf_info$basis), 4L)
})

test_that("input lengths are validated", {
  basis <- matrix(1, nrow = 2, ncol = 1)

  em_amp <- list(onsets = c(0L, 2L), n_time = 6L,
                 amplitudes = c(1))
  expect_error(build_design_matrix(em_amp, hrf_basis_matrix = basis),
               "amplitudes")

  em_mod <- list(onsets = c(0L, 2L), n_time = 6L,
                 modulator = c(1))
  expect_error(build_design_matrix(em_mod, hrf_basis_matrix = basis),
               "modulator")
})

