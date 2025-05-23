test_that("make_spmat_triplet creates expected sparse matrix", {
  i <- c(0L, 1L)
  j <- c(0L, 1L)
  x <- c(1, 2)
  sm <- make_spmat_triplet(i, j, x, 2L, 2L)
  expect_s4_class(sm, "dgCMatrix")
  expect_equal(as.matrix(sm), matrix(c(1, 0, 0, 2), 2, 2))
})

test_that("spmat_dense_prod multiplies correctly", {
  i <- c(0L, 1L)
  j <- c(0L, 1L)
  x <- c(1, 2)
  sm <- make_spmat_triplet(i, j, x, 2L, 2L)
  dense <- matrix(1, 2, 2)
  res <- spmat_dense_prod(sm, dense)
  expect_equal(res, as.matrix(sm %*% dense))
})
