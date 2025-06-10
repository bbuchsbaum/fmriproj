context("fmridataset integration")

skip_if_not_installed("rMVPA")
skip_if_not_installed("fmridataset")

# Helper to create a simple fmri_dataset
make_simple_dataset <- function() {
  Y <- matrix(rnorm(20), nrow = 5, ncol = 4)
  mask <- rep(TRUE, 4)
  events <- data.frame(
    onset = 1:5,
    cond = factor(rep(c("A", "B"), length.out = 5)),
    run = 1
  )
  fmridataset::matrix_dataset(
    Y,
    mask = mask,
    event_table = events,
    sampling_frame = data.frame(block = 1, blocklens = nrow(Y))
  )
}


test_that("create_mvpa_design_from_dataset extracts design information", {
  dset <- make_simple_dataset()
  design <- create_mvpa_design_from_dataset(dset, ~cond, ~run)
  expect_s3_class(design, "mvpa_design")
  expect_equal(design$y_train, dset$event_table$cond)
  expect_equal(design$block_var, dset$event_table$run)
})


test_that("create_mvpa_dataset_from_dataset returns mvpa_dataset", {
  dset <- make_simple_dataset()
  ds <- create_mvpa_dataset_from_dataset(dset)
  expect_s3_class(ds, "mvpa_dataset")
  expect_equal(ds$data, fmridataset::get_data(dset))
  expect_equal(ds$mask, fmridataset::get_mask(dset))
})


test_that("create_mvpa_design_from_dataset errors without event table", {
  dset <- make_simple_dataset()
  dset$event_table <- NULL
  expect_error(
    create_mvpa_design_from_dataset(dset, ~cond, ~run),
    "event_table"
  )
})

