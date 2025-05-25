test_that("make_cached_fn_gr caches results", {
  counter <- 0
  fn_gr <- function(w) {
    counter <<- counter + 1
    list(value = sum(w), grad = rep(1, length(w)))
  }
  cache <- fmriproj:::make_cached_fn_gr(fn_gr)
  w1 <- c(1, 2)
  v1 <- cache$fn(w1)
  g1 <- cache$gr(w1)
  expect_equal(counter, 1)
  expect_equal(v1, sum(w1))
  expect_equal(g1, rep(1, length(w1)))
  g1b <- cache$gr(w1)
  expect_equal(counter, 1)
  w2 <- c(2, 3)
  v2 <- cache$fn(w2)
  expect_equal(counter, 2)
  g2 <- cache$gr(w2)
  expect_equal(counter, 2)
  expect_equal(v2, sum(w2))
  expect_equal(g2, rep(1, length(w2)))
})
