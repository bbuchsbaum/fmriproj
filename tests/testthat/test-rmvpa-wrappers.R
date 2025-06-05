context("rMVPA compatibility wrappers")

test_that("as_mvpa_dataset builds mvpa_dataset structure", {
  Y <- matrix(1, nrow = 6, ncol = 2)
  em <- list(onsets = c(0L, 2L), n_time = 6L)
  basis <- matrix(c(1, 0, 0,
                    0, 1, 0), nrow = 3, byrow = FALSE)
  mask <- matrix(TRUE, nrow = 1, ncol = 2)
  ds <- as_mvpa_dataset(Y, em, mask = mask, hrf_basis_matrix = basis)
  expect_s3_class(ds, "fmriproj_mvpa_dataset")
  expect_true(inherits(ds, "mvpa_dataset"))
  expect_equal(dim(ds$data), c(length(em$onsets), ncol(Y)))
  expect_identical(ds$mask, mask)
  expect_true(is.list(ds$projection_info))
  expect_equal(length(ds$projection_info$collapse_weights), ncol(basis))
})

test_that("pp_feature_selector attaches projection function", {
  set.seed(123)
  X <- matrix(rnorm(20), nrow = 10, ncol = 2)
  labels <- rep(c("a", "b"), each = 5)
  fs <- pp_feature_selector(method = "LDA", dims = 1)
  sel <- fs$select_features(X, labels)
  expect_true(is.logical(sel) && length(sel) == ncol(X) && all(sel))
  pf <- attr(X, "projection_function")
  expect_true(is.function(pf))
  proj <- pf(X)
  expect_equal(dim(proj), c(nrow(X), fs$dims))
})
