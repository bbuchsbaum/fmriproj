context("fmridataset bridge")

skip_if_not_installed("rMVPA")
skip_if_not_installed("fmridataset")

test_that("create_mvpa_design_from_dataset creates proper design", {
  # Create a simple test dataset
  Y <- matrix(rnorm(40 * 10), nrow = 40, ncol = 10)
  events <- data.frame(
    onset = seq(1, 40, by = 10),
    condition = factor(c("A", "B", "A", "B")),
    run = 1
  )

  dset <- fmridataset::matrix_dataset(
    datamat = Y,
    TR = 2,
    run_length = nrow(Y),
    event_table = events
  )

  design <- create_mvpa_design_from_dataset(dset, ~condition, ~run)

  expect_s3_class(design, "mvpa_design")
  expect_true(!is.null(design$train_design))
  expect_equal(nrow(design$train_design), 4)  # 4 events
  expect_true("condition" %in% names(design$train_design))
  expect_true("run" %in% names(design$train_design))
})

test_that("create_mvpa_dataset_from_dataset creates proper dataset", {
  # Create a simple test dataset
  Y <- matrix(rnorm(40 * 10), nrow = 40, ncol = 10)
  events <- data.frame(
    onset = seq(1, 40, by = 10),
    condition = factor(c("A", "B", "A", "B")),
    run = 1
  )

  dset <- fmridataset::matrix_dataset(
    datamat = Y,
    TR = 2,
    run_length = nrow(Y),
    event_table = events
  )

  mvpa_dset <- create_mvpa_dataset_from_dataset(dset)

  expect_s3_class(mvpa_dset, "mvpa_dataset")
  expect_true(!is.null(mvpa_dset$mask))
  expect_true(inherits(mvpa_dset$mask, "NeuroVol"))
  expect_true(inherits(mvpa_dset$train_data, "NeuroVec"))
})

test_that("create_mvpa_dataset_from_dataset handles numeric mask", {
  skip_if_not_installed("neuroim2")

  # Create dataset with numeric mask
  Y <- matrix(rnorm(40 * 10), nrow = 40, ncol = 10)
  events <- data.frame(
    onset = seq(1, 40, by = 10),
    condition = factor(c("A", "B", "A", "B")),
    run = 1
  )

  dset <- fmridataset::matrix_dataset(
    datamat = Y,
    TR = 2,
    run_length = nrow(Y),
    event_table = events
  )

  # matrix_dataset creates numeric mask
  expect_true(is.numeric(dset$mask))

  # Should convert to NeuroVol
  mvpa_dset <- create_mvpa_dataset_from_dataset(dset)
  expect_true(inherits(mvpa_dset$mask, "NeuroVol"))
})

test_that("create_mvpa_design_from_dataset requires event_table", {
  bad_dset <- list()
  class(bad_dset) <- "fmri_dataset"

  expect_error(
    create_mvpa_design_from_dataset(bad_dset, ~y, ~block),
    "must contain an event_table"
  )
})

test_that("create_mvpa_dataset_from_dataset validates data", {
  # Dataset without proper get_data method
  bad_dset <- list()
  class(bad_dset) <- "fmri_dataset"

  expect_error(
    create_mvpa_dataset_from_dataset(bad_dset),
    "Failed to extract data"
  )
})
