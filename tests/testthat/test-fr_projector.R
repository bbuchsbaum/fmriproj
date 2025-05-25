context("fr_projector")

test_that("fr_projector validates matrix inputs", {
  Qt <- matrix(0, nrow = 2, ncol = 3)
  R <- matrix(0, nrow = 2, ncol = 2)
  expect_error(fr_projector(1, R), "Qt must be a matrix")
  expect_error(fr_projector(Qt, 1), "R must be a matrix")
})

test_that("fr_projector checks dimension compatibility", {
  Qt <- matrix(0, nrow = 2, ncol = 3)
  R_bad <- matrix(0, nrow = 3, ncol = 3)
  expect_error(fr_projector(Qt, R_bad), "nrow(Qt) must equal ncol(R)")
})

test_that("fr_projector validates K_global dimensions", {
  Qt <- matrix(0, nrow = 2, ncol = 3)
  R <- matrix(0, nrow = 2, ncol = 2)
  Kg_bad <- matrix(0, nrow = 2, ncol = 2)
  expect_error(fr_projector(Qt, R, Kg_bad), "K_global must have same dimensions as Qt")
  Kg_good <- matrix(0, nrow = 2, ncol = 3)
  proj <- fr_projector(Qt, R, Kg_good)
  expect_s3_class(proj, "fr_projector")
})
