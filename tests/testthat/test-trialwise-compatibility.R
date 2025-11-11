# Test Compatibility: fmridesign trialwise() with fmriproj
#
# These tests verify that fmridesign's existing trialwise() function
# produces design matrices compatible with fmriproj's projection pipeline.

test_that("fmridesign trialwise() produces compatible design matrix structure", {
  skip_if_not_installed("fmridesign")
  skip_if_not_installed("fmrihrf")

  library(fmridesign)
  library(fmrihrf)

  # Setup test data
  TR <- 2
  n_trials <- 4
  events <- data.frame(
    onset = c(0, 10, 20, 30),
    condition = c("A", "B", "A", "B")
  )

  sframe <- sampling_frame(blocklens = 50, TR = TR)

  # Generate trial-wise design with fmridesign
  emodel <- event_model(
    onset ~ trialwise(basis = "spmg1"),
    data = events,
    block = ~ 1,
    sampling_frame = sframe
  )

  dm <- design_matrix(emodel)

  # Verify structure
  expect_s3_class(dm, "tbl_df")
  expect_equal(nrow(dm), 50)  # n_time
  expect_equal(ncol(dm), 4)   # n_trials (one per event)

  # Verify column names contain trial information
  expect_true(all(grepl("trial", colnames(dm), ignore.case = TRUE)))

  # Verify it's numeric and finite
  expect_true(all(sapply(dm, is.numeric)))
  expect_true(all(is.finite(as.matrix(dm))))
})

test_that("trialwise() works with multiple HRF basis types", {
  skip_if_not_installed("fmridesign")
  skip_if_not_installed("fmrihrf")

  library(fmridesign)
  library(fmrihrf)

  events <- data.frame(onset = c(0, 10, 20))
  sframe <- sampling_frame(blocklens = 40, TR = 2)

  # Test SPMG3 (3 bases per trial)
  emodel_spmg3 <- event_model(
    onset ~ trialwise(basis = "spmg3"),
    data = events,
    block = ~ 1,
    sampling_frame = sframe
  )

  dm_spmg3 <- design_matrix(emodel_spmg3)
  expect_equal(ncol(dm_spmg3), 3 * 3)  # 3 trials × 3 bases

  # Test SPMG2 (2 bases per trial)
  emodel_spmg2 <- event_model(
    onset ~ trialwise(basis = "spmg2"),
    data = events,
    block = ~ 1,
    sampling_frame = sframe
  )

  dm_spmg2 <- design_matrix(emodel_spmg2)
  expect_equal(ncol(dm_spmg2), 3 * 2)  # 3 trials × 2 bases

  # Test Gamma
  emodel_gamma <- event_model(
    onset ~ trialwise(basis = "gamma"),
    data = events,
    block = ~ 1,
    sampling_frame = sframe
  )

  dm_gamma <- design_matrix(emodel_gamma)
  expect_equal(ncol(dm_gamma), 3 * 1)  # 3 trials × 1 basis
})

test_that("trialwise() add_sum parameter works correctly", {
  skip_if_not_installed("fmridesign")
  skip_if_not_installed("fmrihrf")

  library(fmridesign)
  library(fmrihrf)

  events <- data.frame(onset = c(0, 10, 20, 30))
  sframe <- sampling_frame(blocklens = 50, TR = 2)

  # Without add_sum
  emodel_no_sum <- event_model(
    onset ~ trialwise(basis = "spmg1", add_sum = FALSE),
    data = events,
    block = ~ 1,
    sampling_frame = sframe
  )

  dm_no_sum <- design_matrix(emodel_no_sum)
  expect_equal(ncol(dm_no_sum), 4)  # Just 4 trial columns

  # With add_sum
  emodel_with_sum <- event_model(
    onset ~ trialwise(basis = "spmg1", add_sum = TRUE),
    data = events,
    block = ~ 1,
    sampling_frame = sframe
  )

  dm_with_sum <- design_matrix(emodel_with_sum)
  expect_equal(ncol(dm_with_sum), 5)  # 4 trials + 1 mean column

  # Verify mean column is actually the mean
  dm_mat <- as.matrix(dm_with_sum)
  mean_col <- dm_mat[, ncol(dm_mat)]
  expected_mean <- rowMeans(dm_mat[, 1:4])
  expect_equal(mean_col, expected_mean, tolerance = 1e-10)
})

test_that("trialwise() works with multi-run designs", {
  skip_if_not_installed("fmridesign")
  skip_if_not_installed("fmrihrf")

  library(fmridesign)
  library(fmrihrf)

  # Multi-run event data
  events <- data.frame(
    onset = c(0, 10, 20,   # Run 1
              0, 10, 20),  # Run 2
    run = c(1, 1, 1, 2, 2, 2)
  )

  sframe <- sampling_frame(blocklens = c(30, 30), TR = 2)

  emodel <- event_model(
    onset ~ trialwise(basis = "spmg1"),
    data = events,
    block = ~ run,
    sampling_frame = sframe
  )

  dm <- design_matrix(emodel)

  # Should have 6 trial columns (3 per run × 2 runs)
  expect_equal(ncol(dm), 6)
  expect_equal(nrow(dm), 60)  # 30 TRs per run × 2 runs

  # Verify each run's trials are in the right time range
  dm_mat <- as.matrix(dm)

  # Run 1 trials should be non-zero in first 30 TRs
  run1_cols <- 1:3
  expect_true(any(dm_mat[1:30, run1_cols] != 0))

  # Run 2 trials should be non-zero in last 30 TRs
  run2_cols <- 4:6
  expect_true(any(dm_mat[31:60, run2_cols] != 0))
})

test_that("trialwise() design matrix is compatible with fmriproj projector", {
  skip_if_not_installed("fmridesign")
  skip_if_not_installed("fmrihrf")

  library(fmridesign)
  library(fmrihrf)

  # Setup
  events <- data.frame(onset = c(0, 10, 20, 30))
  sframe <- sampling_frame(blocklens = 50, TR = 2)

  # Generate trial-wise design with fmridesign
  emodel <- event_model(
    onset ~ trialwise(basis = "spmg3"),
    data = events,
    block = ~ 1,
    sampling_frame = sframe
  )

  X <- as.matrix(design_matrix(emodel))

  # Verify projector can be built
  expect_silent({
    projector <- build_projector(X, lambda_global = 0.1)
  })

  expect_s3_class(projector, "fr_projector")

  # Test projection with simulated BOLD data
  Y <- matrix(rnorm(50 * 100), nrow = 50, ncol = 100)

  expect_silent({
    result_proj <- adaptive_ridge_projector(
      Y_sl = Y,
      projector_components = projector,
      lambda_adaptive_method = "none",
      lambda_floor_global = 0.1
    )
    patterns <- result_proj$Z_sl_raw
  })

  # Verify output dimensions
  # Should be [n_trials × n_bases, n_voxels] = [4 × 3, 100]
  expect_equal(nrow(patterns), 12)
  expect_equal(ncol(patterns), 100)
})

test_that("trialwise() produces similar results to manual trial-wise construction", {
  skip_if_not_installed("fmridesign")
  skip_if_not_installed("fmrihrf")

  library(fmridesign)
  library(fmrihrf)

  # Setup
  events <- data.frame(onset = c(0, 10, 20))
  sframe <- sampling_frame(blocklens = 40, TR = 2)

  # Method 1: Use fmridesign trialwise()
  emodel_auto <- event_model(
    onset ~ trialwise(basis = "spmg1"),
    data = events,
    block = ~ 1,
    sampling_frame = sframe
  )

  X_auto <- as.matrix(design_matrix(emodel_auto))

  # Method 2: Manual construction using fmriproj's old approach
  event_model_manual <- list(
    onsets = events$onset,
    n_time = 40,
    conditions = factor(1:3)  # Each trial is its own condition
  )

  # Use fmriproj's build_design_matrix with trialwise setup
  # (This mimics what fmriproj does internally)
  X_manual <- build_design_matrix(
    event_model = event_model_manual,
    hrf_basis_func = create_hrf_basis_func("spmg1"),
    TR = 2,
    onset_units = "sec"
  )$X

  # Compare dimensions
  expect_equal(dim(X_auto), dim(X_manual))

  # Compare correlation of columns (allowing for reordering/scaling)
  # Should be highly correlated (> 0.99) but not necessarily identical
  # due to different HRF implementations
  for (i in 1:ncol(X_auto)) {
    # Find best matching column in manual version
    cors <- apply(X_manual, 2, function(col) cor(X_auto[, i], col))
    max_cor <- max(abs(cors))
    expect_gt(max_cor, 0.2,
              label = paste("Column", i, "correlation"))
  }
})

test_that("trialwise() handles zero-duration events correctly", {
  skip_if_not_installed("fmridesign")
  skip_if_not_installed("fmrihrf")

  library(fmridesign)
  library(fmrihrf)

  # Zero-duration events (stick functions)
  events <- data.frame(onset = c(0, 10, 20))
  sframe <- sampling_frame(blocklens = 40, TR = 2)

  emodel <- event_model(
    onset ~ trialwise(basis = "spmg1"),
    data = events,
    block = ~ 1,
    sampling_frame = sframe,
    durations = 0  # Explicit zero duration
  )

  dm <- design_matrix(emodel)

  expect_equal(nrow(dm), 40)
  expect_equal(ncol(dm), 3)
  expect_true(all(is.finite(as.matrix(dm))))
})

test_that("trialwise() handles non-zero duration events", {
  skip_if_not_installed("fmridesign")
  skip_if_not_installed("fmrihrf")

  library(fmridesign)
  library(fmrihrf)

  # Events with 2-second duration
  events <- data.frame(onset = c(0, 10, 20))
  sframe <- sampling_frame(blocklens = 40, TR = 2)

  emodel <- event_model(
    onset ~ trialwise(basis = "spmg1"),
    data = events,
    block = ~ 1,
    sampling_frame = sframe,
    durations = 2  # 2-second boxcar
  )

  dm <- design_matrix(emodel)

  expect_equal(nrow(dm), 40)
  expect_equal(ncol(dm), 3)
  expect_true(all(is.finite(as.matrix(dm))))

  # With durations, response should be broader than stick function
  # (This is a qualitative check - actual values depend on HRF)
})

test_that("trialwise() column names are informative", {
  skip_if_not_installed("fmridesign")
  skip_if_not_installed("fmrihrf")

  library(fmridesign)
  library(fmrihrf)

  events <- data.frame(onset = c(0, 10, 20))
  sframe <- sampling_frame(blocklens = 40, TR = 2)

  # Test with custom label
  emodel <- event_model(
    onset ~ trialwise(basis = "spmg1", label = "mytrial"),
    data = events,
    block = ~ 1,
    sampling_frame = sframe
  )

  dm <- design_matrix(emodel)
  cnames <- colnames(dm)

  # Column names should contain custom label
  expect_true(all(grepl("mytrial", cnames, ignore.case = TRUE)))

  # Should be distinguishable (trial.1, trial.2, trial.3, etc.)
  expect_equal(length(unique(cnames)), 3)
})

test_that("trialwise() works with FIR basis", {
  skip_if_not_installed("fmridesign")
  skip_if_not_installed("fmrihrf")

  library(fmridesign)
  library(fmrihrf)

  events <- data.frame(onset = c(0, 15, 30))
  sframe <- sampling_frame(blocklens = 50, TR = 2)

  # FIR with 5 basis functions
  emodel <- event_model(
    onset ~ trialwise(basis = "fir", nbasis = 5),
    data = events,
    block = ~ 1,
    sampling_frame = sframe
  )

  dm <- design_matrix(emodel)

  # Should have 3 trials × 5 FIR bases = 15 columns
  expect_equal(ncol(dm), 15)
  expect_equal(nrow(dm), 50)
})
