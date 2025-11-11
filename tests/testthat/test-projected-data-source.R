context("ProjectedDataSource proxy object")

test_that("ProjectedDataSource creates valid proxy", {
  # Test data
  X <- matrix(rnorm(100), 20, 5)
  P <- matrix(rnorm(15), 5, 3)
  
  # Create proxy
  proxy <- ProjectedDataSource$new(X, P)
  
  expect_equal(proxy$nrow, 20)
  expect_equal(proxy$ncol, 3)
  expect_equal(proxy$dim, c(20, 3))
})

test_that("ProjectedDataSource performs lazy projection", {
  X <- matrix(rnorm(100), 20, 5)
  P <- matrix(rnorm(15), 5, 3)
  
  proxy <- ProjectedDataSource$new(X, P)
  
  # Should not be cached initially
  expect_false(proxy$is_cached())
  
  # Access triggers projection
  result <- proxy$as.matrix()
  
  expect_equal(dim(result), c(20, 3))
  expect_true(proxy$is_cached())
  
  # Second access uses cache
  result2 <- proxy$as.matrix()
  expect_identical(result, result2)
})

test_that("ProjectedDataSource preserves metadata", {
  X <- matrix(rnorm(100), 20, 5)
  rownames(X) <- paste0("Sample", 1:20)
  P <- matrix(rnorm(15), 5, 3)
  
  metadata <- list(
    feature_names = c("PC1", "PC2", "PC3"),
    attributes = list(
      study = "test_study",
      version = 1.0
    )
  )
  
  proxy <- ProjectedDataSource$new(X, P, metadata)
  result <- proxy$as.matrix()
  
  expect_equal(rownames(result), paste0("Sample", 1:20))
  expect_equal(colnames(result), c("PC1", "PC2", "PC3"))
  expect_equal(attr(result, "study"), "test_study")
  expect_equal(attr(result, "version"), 1.0)
})

test_that("ProjectedDataSource supports function-based projection", {
  X <- matrix(rnorm(100), 20, 5)
  
  # PCA projection function
  proj_fun <- function(data) {
    # Don't scale if only testing with one row
    if (nrow(data) == 1) {
      # Simple projection for single row test
      data[, 1:2, drop = FALSE]
    } else {
      pc <- prcomp(data, scale. = TRUE)
      pc$x[, 1:2]
    }
  }
  
  proxy <- ProjectedDataSource$new(X, proj_fun)
  
  expect_equal(proxy$nrow, 20)
  expect_equal(proxy$ncol, 2)
  
  result <- proxy$as.matrix()
  expect_equal(dim(result), c(20, 2))
})

test_that("ProjectedDataSource supports subsetting", {
  X <- matrix(rnorm(100), 20, 5)
  P <- matrix(rnorm(15), 5, 3)
  
  proxy <- ProjectedDataSource$new(X, P)
  
  # Row subsetting
  sub1 <- proxy[1:5, ]
  expect_equal(dim(sub1), c(5, 3))
  
  # Column subsetting
  sub2 <- proxy[, 1:2]
  expect_equal(dim(sub2), c(20, 2))
  
  # Both
  sub3 <- proxy[1:5, 1:2]
  expect_equal(dim(sub3), c(5, 2))
})

test_that("ProjectedDataSource get_rows provides chunked access", {
  X <- matrix(rnorm(1000), 200, 5)
  P <- matrix(rnorm(15), 5, 3)
  
  proxy <- ProjectedDataSource$new(X, P)
  
  # Get specific rows without caching full matrix
  chunk <- proxy$get_rows(1:10)
  
  expect_equal(dim(chunk), c(10, 3))
  expect_false(proxy$is_cached())  # Should not cache full matrix
})

test_that("as.matrix S3 method works", {
  X <- matrix(rnorm(100), 20, 5)
  P <- matrix(rnorm(15), 5, 3)
  
  proxy <- ProjectedDataSource$new(X, P)
  
  # S3 method
  result <- as.matrix(proxy)
  
  expect_equal(dim(result), c(20, 3))
  expect_equal(as.numeric(result), as.numeric(X %*% P))
})

test_that("ProjectedDataSource handles sparse matrices", {
  skip_if_not_installed("Matrix")
  
  # Create sparse matrix
  X <- Matrix::rsparsematrix(20, 5, density = 0.3)
  P <- matrix(rnorm(15), 5, 3)
  
  proxy <- ProjectedDataSource$new(X, P)
  result <- proxy$as.matrix()
  
  expect_equal(dim(result), c(20, 3))
  expect_true(is.matrix(result))  # Result should be dense
})

test_that("ProjectedDataSource validates inputs", {
  X <- matrix(rnorm(100), 20, 5)
  P <- matrix(rnorm(12), 4, 3)  # Wrong dimensions
  
  expect_error(
    ProjectedDataSource$new(X, P),
    "data columns .* must match projection rows"
  )
  
  expect_error(
    ProjectedDataSource$new("not a matrix", P),
    "data must be a matrix"
  )
  
  expect_error(
    ProjectedDataSource$new(X, "not a projection"),
    "projection must be a matrix or function"
  )
})

test_that("ProjectedDataSource active bindings work", {
  X <- matrix(rnorm(100), 20, 5)
  P <- matrix(rnorm(15), 5, 3)
  
  proxy <- ProjectedDataSource$new(X, P)
  
  # data binding triggers projection
  result <- proxy$data
  expect_equal(dim(result), c(20, 3))
  expect_true(proxy$is_cached())
  
  # original_data binding
  orig <- proxy$original_data
  expect_identical(orig, X)
  
  # projection_matrix binding
  proj <- proxy$projection_matrix
  expect_identical(proj, P)
})

test_that("clear_cache works", {
  X <- matrix(rnorm(100), 20, 5)
  P <- matrix(rnorm(15), 5, 3)
  
  proxy <- ProjectedDataSource$new(X, P)
  
  # Cache data
  result1 <- proxy$as.matrix()
  expect_true(proxy$is_cached())
  
  # Clear cache
  proxy$clear_cache()
  expect_false(proxy$is_cached())
  
  # Re-access creates new projection
  result2 <- proxy$as.matrix()
  expect_equal(result1, result2)  # Results should be identical
  expect_true(proxy$is_cached())
})

test_that("transpose works correctly", {
  X <- matrix(rnorm(100), 20, 5)
  P <- matrix(rnorm(15), 5, 3)
  
  proxy <- ProjectedDataSource$new(X, P)
  
  # Transpose
  result_t <- t(proxy)
  
  # Should match manual transpose
  expected <- t(X %*% P)
  expect_equal(as.numeric(result_t), as.numeric(expected))
  expect_equal(dim(result_t), c(3, 20))
})

test_that("optimized row subsetting avoids full projection", {
  X <- matrix(rnorm(1000), 200, 5)
  P <- matrix(rnorm(15), 5, 3)
  
  proxy <- ProjectedDataSource$new(X, P)
  
  # Row-only subsetting should not cache full matrix
  rows <- proxy[1:10, ]
  expect_false(proxy$is_cached())
  expect_equal(dim(rows), c(10, 3))
  
  # Column subsetting requires full projection
  cols <- proxy[, 1:2]
  expect_true(proxy$is_cached())
  expect_equal(dim(cols), c(200, 2))
})