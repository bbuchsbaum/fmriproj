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

test_that("make_spmat_triplet validates inputs", {
  expect_error(make_spmat_triplet(0L, c(0L, 1L), 1, 2L, 2L))
  expect_error(make_spmat_triplet(c(0L, NA), c(0L, 1L), c(1, 2), 2L, 2L),
               "must not contain NA")
  expect_error(make_spmat_triplet(c(-1L, 0L), c(0L, 1L), c(1, 2), 2L, 2L),
               "non-negative")
  expect_error(make_spmat_triplet(c(0L, 1L), c(0L, 2L), c(1, 2), 2L, 2L),
               "out of range")
})

test_that("spmat_dense_prod validates inputs", {
  sm <- make_spmat_triplet(0L, 0L, 1, 2L, 2L)
  expect_error(spmat_dense_prod(as.matrix(sm), matrix(1, 2, 2)), "dgCMatrix")
  expect_error(spmat_dense_prod(sm, 1), "matrix")
  expect_error(spmat_dense_prod(sm, matrix(1, 3, 2)), "Non-conformable")
})
