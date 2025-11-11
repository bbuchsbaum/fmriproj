# Verification test for Qt refactoring
# This test ensures the new qr_obj implementation is numerically equivalent to the legacy Qt approach

context("Refactoring verification - Qt to qr_obj")

test_that("New qr_obj approach produces identical results to legacy Qt approach", {
  set.seed(123)
  
  # Create test data
  n_time <- 50
  n_trials <- 10
  n_basis <- 3
  n_voxels <- 5
  
  # Build design matrix
  X <- Matrix::rsparsematrix(n_time, n_trials * n_basis, density = 0.3)
  
  # Test data for projection
  Y <- matrix(rnorm(n_time * n_voxels), n_time, n_voxels)
  
  # Build projectors with both approaches
  proj_new <- build_projector(X, lambda_global = 0.1)
  proj_legacy <- build_projector(X, lambda_global = 0.1, legacy_output = TRUE)
  
  # Test adaptive_ridge_projector with both
  res_new <- adaptive_ridge_projector(
    Y_sl = Y,
    projector_components = proj_new,
    lambda_adaptive_method = "none",
    lambda_floor_global = 0.1
  )
  
  res_legacy <- adaptive_ridge_projector(
    Y_sl = Y,
    projector_components = proj_legacy,
    lambda_adaptive_method = "none",
    lambda_floor_global = 0.1
  )
  
  # Results should be numerically identical
  expect_equal(
    as.numeric(res_new$Z_sl_raw), 
    as.numeric(res_legacy$Z_sl_raw),
    tolerance = 1e-12
  )
})

test_that("Memory usage: new approach doesn't create Qt matrix", {
  set.seed(456)
  
  # Create a larger test case
  X <- Matrix::rsparsematrix(100, 30, density = 0.3)
  
  # New approach should not have Qt
  proj_new <- build_projector(X, lambda_global = 0.5)
  expect_null(proj_new$Qt)
  expect_null(proj_new$tRQt)
  expect_null(proj_new$K_global)
  expect_true(!is.null(proj_new$qr_obj))
  expect_equal(proj_new$.internal_representation, "qr")
  
  # Legacy approach should have Qt
  proj_legacy <- build_projector(X, lambda_global = 0.5, legacy_output = TRUE)
  expect_true(!is.null(proj_legacy$Qt))
  expect_true(!is.null(proj_legacy$tRQt))
  expect_true(!is.null(proj_legacy$K_global))
  expect_null(proj_legacy$qr_obj)
  expect_equal(proj_legacy$.internal_representation, "explicit")
})

test_that("qr.qty produces same result as Qt %*% Y", {
  set.seed(789)
  
  # Small example for direct comparison
  X <- Matrix::rsparsematrix(20, 6, density = 0.5)
  Y <- matrix(rnorm(20 * 3), 20, 3)
  
  # Get QR decomposition
  qr_obj <- Matrix::qr(X)
  Qt <- t(as.matrix(Matrix::qr.Q(qr_obj)))
  
  # Compare Qt %*% Y with qr.qty
  result_explicit <- Qt %*% Y
  result_qty <- Matrix::qr.qty(qr_obj, Y)
  
  # They should be identical (qr.qty returns full length, we need first p rows)
  p <- ncol(X)
  expect_equal(
    as.matrix(result_explicit),
    as.matrix(result_qty[1:p, , drop = FALSE]),
    tolerance = 1e-12
  )
})