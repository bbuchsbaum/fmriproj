context("fr_projector")

test_that("fr_projector validates matrix inputs for legacy format", {
  Qt <- matrix(0, nrow = 2, ncol = 3)
  R <- matrix(0, nrow = 2, ncol = 2)
  expect_error(suppressWarnings(fr_projector(Qt = 1, R = R)), "Qt must be a matrix")
  expect_error(suppressWarnings(fr_projector(Qt = Qt, R = 1)), "R must be a matrix")
})

test_that("fr_projector checks dimension compatibility for legacy format", {
  Qt <- matrix(0, nrow = 2, ncol = 3)
  R_bad <- matrix(0, nrow = 3, ncol = 3)
  expect_error(suppressWarnings(fr_projector(Qt = Qt, R = R_bad)), "nrow\\(Qt\\) == ncol\\(R\\)")
})

test_that("fr_projector validates K_global dimensions for legacy format", {
  Qt <- matrix(0, nrow = 2, ncol = 3)
  R <- matrix(0, nrow = 2, ncol = 2)
  Kg_bad <- matrix(0, nrow = 2, ncol = 2)
  expect_error(suppressWarnings(fr_projector(Qt = Qt, R = R, K_global = Kg_bad)), 
               "K_global.*same dimensions as Qt")
  Kg_good <- matrix(0, nrow = 2, ncol = 3)
  proj <- suppressWarnings(fr_projector(Qt = Qt, R = R, K_global = Kg_good))
  expect_s3_class(proj, "fr_projector")
})

test_that("fr_projector accepts optional RtR and tRQt for legacy format", {
  Qt <- matrix(0, nrow = 2, ncol = 3)
  R <- matrix(0, nrow = 2, ncol = 2)
  RtR <- crossprod(R)
  tRQt <- t(R) %*% Qt
  proj <- suppressWarnings(fr_projector(Qt = Qt, R = R, RtR = RtR, tRQt = tRQt))
  expect_equal(proj$RtR, RtR)
  expect_equal(proj$tRQt, tRQt)
})

test_that("fr_projector works with new qr_obj format", {
  # Create a small test matrix
  X <- matrix(rnorm(12), 6, 2)
  qr_obj <- qr(X)
  R <- qr.R(qr_obj)
  RtR <- crossprod(R)
  
  # New format should work without warnings
  proj <- fr_projector(qr_obj = qr_obj, R = R, RtR = RtR, lambda_global = 0.1)
  expect_s3_class(proj, "fr_projector")
  expect_equal(proj$.internal_representation, "qr")
  expect_equal(proj$lambda_global, 0.1)
})
