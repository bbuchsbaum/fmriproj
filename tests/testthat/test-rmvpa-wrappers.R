context("rMVPA wrappers")

# Create simple event model and basis
em <- list(onsets = c(0L, 2L), n_time = 6L)
basis <- matrix(c(1, 0, 0,
                  0, 1, 0), nrow = 3, byrow = FALSE)

design <- build_design_matrix(em, hrf_basis_matrix = basis)
proj <- build_projector(design$X)

Y <- matrix(1, nrow = 6, ncol = 2)

# Test make_rmvpa_searchlight_fun for different return formats

test_that("make_rmvpa_searchlight_fun returns expected formats", {
  spec <- projection_spec(em, proj, length(em$onsets), ncol(basis))
  fun_mat <- make_rmvpa_searchlight_fun(spec, return_format = "matrix")
  res_mat <- fun_mat(Y)
  expect_equal(dim(res_mat), c(length(em$onsets), ncol(Y)))

  fun_mvpa <- make_rmvpa_searchlight_fun(spec, return_format = "mvpa_data")
  res_mvpa <- fun_mvpa(Y)
  expect_equal(res_mvpa$data, res_mat)
  expect_equal(res_mvpa$nobs, length(em$onsets))

  fun_list <- make_rmvpa_searchlight_fun(spec, return_format = "list")
  res_list <- fun_list(Y)
  expect_equal(res_list$data, res_mat)
  expect_true(is.numeric(res_list$lambda_sl))
  expect_length(res_list$w_sl, ncol(basis))
})

# Test as_mvpa_dataset returns correct structure

test_that("as_mvpa_dataset creates mvpa_dataset object", {
  ds <- as_mvpa_dataset(Y, em, hrf_basis_matrix = basis)
  expect_s3_class(ds, "fmriproj_mvpa_dataset")
  expect_equal(dim(ds$data), c(length(em$onsets), ncol(Y)))
  expect_true(!is.null(ds$projection_info$design))
  expect_true(!is.null(ds$projection_info$projector))
})

# Test wrap_as_projecting_dataset and pp_feature_selector

test_that("wrap_as_projecting_dataset applies projection", {
  proj_fun <- function(Y_sl) Y_sl * 2
  wd <- wrap_as_projecting_dataset(Y, proj_fun, list(dummy = TRUE))
  res_full <- wd$get_data()
  expect_equal(res_full, Y * 2)
  res_idx <- wd$get_data(indices = 1)
  expect_equal(res_idx, Y[,1,drop=FALSE] * 2)
})


test_that("pp_feature_selector stores projection function", {
  fs <- pp_feature_selector(method = "LDA", dims = 1)
  X <- matrix(rnorm(20), ncol = 2)  # 10 rows, 2 columns
  lab <- rep(c("a","b"), each = 5)  # 10 labels total
  
  # Call select_features
  sel <- fs$select_features(X, lab)
  expect_true(all(sel))
  
  # Get projection function from the feature selector
  proj_fun <- fs$get_projection_function()
  expect_false(is.null(proj_fun))
  
  if (!is.null(proj_fun)) {
    proj <- proj_fun(X)
    expect_equal(nrow(proj), nrow(X))
    expect_equal(ncol(proj), fs$dims)
  }
})
