# Phase 1 Integration Tests: fmridesign/fmrihrf Integration
# Tests to ensure backward compatibility and validate new infrastructure

test_that("parse_onsets_with_fmridesign maintains backward compatibility", {
  skip_if_not_installed("fmridesign")

  # Test 1: Simple second-based onsets
  onsets_sec <- c(0, 10, 20, 30)
  n_time <- 50
  TR <- 2

  result <- parse_onsets_with_fmridesign(
    onsets = onsets_sec,
    n_time = n_time,
    TR = TR,
    onset_units = "sec",
    rounding = "nearest"
  )

  expect_equal(result, c(0, 5, 10, 15))
  expect_type(result, "integer")

  # Test 2: Index-based onsets
  onsets_idx <- c(0, 5, 10, 15)
  result_idx <- parse_onsets_with_fmridesign(
    onsets = onsets_idx,
    n_time = n_time,
    TR = TR,
    onset_units = "index"
  )

  expect_equal(result_idx, as.integer(onsets_idx))

  # Test 3: Rounding policies
  onsets_subsec <- c(0.5, 10.3, 20.7, 30.1)

  result_nearest <- parse_onsets_with_fmridesign(
    onsets_subsec, n_time, TR, "sec", "nearest"
  )
  expect_equal(result_nearest, c(0, 5, 10, 15))

  result_floor <- parse_onsets_with_fmridesign(
    onsets_subsec, n_time, TR, "sec", "floor"
  )
  expect_equal(result_floor, c(0, 5, 10, 15))

  result_ceil <- parse_onsets_with_fmridesign(
    onsets_subsec, n_time, TR, "sec", "ceil"
  )
  expect_equal(result_ceil, c(1, 6, 11, 16))
})

test_that("parse_onsets_with_fmridesign handles multi-run scenarios", {
  skip_if_not_installed("fmridesign")

  # Multi-run with per-run onsets
  onsets <- c(0, 10, 0, 10)  # Resets per run
  run_id <- c(1, 1, 2, 2)
  n_time_per_run <- c(25, 25)
  n_time <- 50
  TR <- 2

  result <- parse_onsets_with_fmridesign(
    onsets = onsets,
    n_time = n_time,
    TR = TR,
    onset_units = "sec",
    rounding = "nearest",
    run_id = run_id,
    n_time_per_run = n_time_per_run
  )

  # Run 1 onsets: 0, 10 sec → indices 0, 5
  # Run 2 onsets: 0, 10 sec → indices 25, 30 (offset by 25)
  expect_equal(result, c(0, 5, 25, 30))
})

test_that("parse_onsets_with_fmridesign validates inputs", {
  skip_if_not_installed("fmridesign")

  # Missing TR when using seconds
  expect_error(
    parse_onsets_with_fmridesign(
      onsets = c(0, 10),
      n_time = 50,
      TR = NULL,
      onset_units = "sec"
    ),
    "positive numeric TR must be provided"
  )

  # Out of range indices
  expect_error(
    parse_onsets_with_fmridesign(
      onsets = c(0, 10, 100),
      n_time = 50,
      onset_units = "index"
    ),
    "onset indices out of range"
  )

  # Invalid run_id length
  expect_error(
    parse_onsets_with_fmridesign(
      onsets = c(0, 10, 20),
      n_time = 50,
      TR = 2,
      run_id = c(1, 1),  # Wrong length
      onset_units = "sec"
    ),
    "run_id must be the same length as onsets"
  )
})

test_that("build_design_matrix with new onset parsing produces identical results", {
  skip_if_not_installed("fmridesign")

  # Create test event model
  event_model <- list(
    onsets = c(0, 10, 20, 30),
    n_time = 50,
    amplitudes = c(1, 1, 1, 1),
    modulator = c(1, 1, 1, 1),
    basis_length = 10
  )

  # Simple HRF basis for testing (starts with non-zero value at onset)
  hrf_basis <- matrix(
    c(0.1, 0.2, 0.5, 0.8, 1.0, 0.8, 0.5, 0.2, 0.1, 0),
    ncol = 1
  )

  # Build design matrix
  design <- build_design_matrix(
    event_model = event_model,
    hrf_basis_matrix = hrf_basis,
    TR = 2,
    onset_units = "sec",
    rounding = "nearest",
    sparse = FALSE
  )

  # Validate structure
  expect_s3_class(design, "fr_design_matrix")
  expect_equal(nrow(design$X), 50)  # n_time
  expect_equal(ncol(design$X), 4)   # 4 trials × 1 basis

  # Validate non-zero entries occur at expected onsets
  # Trial 1 at onset 0 (index 0)
  expect_true(design$X[1, 1] > 0)

  # Trial 2 at onset 10s (index 5, TR=2)
  expect_true(design$X[6, 2] > 0)
})

test_that("HRF adapter functions work correctly", {
  skip_if_not_installed("fmrihrf")

  # Test get_hrf_by_name
  hrf_spmg3 <- get_hrf_by_name("spmg3")
  expect_s3_class(hrf_spmg3, "HRF")

  # Test hrf_to_basis_matrix
  time_vec <- seq(0, 29)
  basis_matrix <- hrf_to_basis_matrix(hrf_spmg3, time_vec)

  expect_equal(nrow(basis_matrix), 30)
  expect_equal(ncol(basis_matrix), 3)  # SPMG3 has 3 bases
  expect_type(basis_matrix, "double")

  # Test with lag
  basis_lagged <- hrf_to_basis_matrix(hrf_spmg3, time_vec, lag = 2)
  expect_equal(dim(basis_lagged), c(30, 3))
  # Lagged version should be shifted
  expect_false(identical(basis_matrix, basis_lagged))
})

test_that("create_hrf_basis_func produces compatible function", {
  skip_if_not_installed("fmrihrf")

  # Create basis function
  hrf_func <- create_hrf_basis_func("spmg3")
  expect_type(hrf_func, "closure")

  # Test that it works with build_design_matrix signature
  time_vec <- seq(0, 29)
  result <- hrf_func(theta_params = NULL, time_vector = time_vec)

  expect_equal(nrow(result), 30)
  expect_equal(ncol(result), 3)
})

test_that("list_available_hrfs returns expected data", {
  hrf_list <- list_available_hrfs()

  expect_s3_class(hrf_list, "data.frame")
  expect_true("spmg3" %in% hrf_list$name)
  expect_true("gamma" %in% hrf_list$name)
  expect_true(all(c("name", "nbasis", "description") %in% names(hrf_list)))
})

test_that("build_design_matrix works with fmrihrf HRFs", {
  skip_if_not_installed("fmrihrf")

  event_model <- list(
    onsets = c(0, 10, 20),
    n_time = 40,
    basis_length = 15
  )

  # Use fmrihrf HRF
  hrf_func <- create_hrf_basis_func("spmg3")

  design <- build_design_matrix(
    event_model = event_model,
    hrf_basis_func = hrf_func,
    TR = 2,
    sparse = FALSE
  )

  expect_s3_class(design, "fr_design_matrix")
  expect_equal(nrow(design$X), 40)
  expect_equal(ncol(design$X), 9)  # 3 trials × 3 SPMG3 bases
  expect_equal(ncol(design$hrf_info$basis), 3)
})

test_that("Backward compatibility with different HRF specifications", {
  skip_if_not_installed("fmrihrf")

  event_model <- list(
    onsets = c(0, 10),
    n_time = 30,
    basis_length = 10
  )

  # Test 1: Pre-computed matrix (old way)
  hrf_matrix_manual <- matrix(
    c(0, 0.5, 1.0, 0.5, 0.2, 0.1, 0, 0, 0, 0),
    ncol = 1
  )

  design1 <- build_design_matrix(
    event_model = event_model,
    hrf_basis_matrix = hrf_matrix_manual,
    TR = 2
  )

  expect_equal(ncol(design1$hrf_info$basis), 1)

  # Test 2: Function-based (new way with fmrihrf)
  hrf_func <- create_hrf_basis_func("spmg1")

  design2 <- build_design_matrix(
    event_model = event_model,
    hrf_basis_func = hrf_func,
    TR = 2
  )

  expect_equal(ncol(design2$hrf_info$basis), 1)

  # Both should have same structure
  expect_equal(dim(design1$X), dim(design2$X))
})
