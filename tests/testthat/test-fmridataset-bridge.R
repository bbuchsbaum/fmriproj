context("fmridataset bridge")

# Minimal mock dataset with required structure
mock_dset <- list(event_table = data.frame(y = 1:2, block = c(1,1)))
class(mock_dset) <- "fmri_dataset"

# These helpers require rMVPA, so they should error when the package is missing

test_that("create_mvpa_design_from_dataset errors without rMVPA", {
  expect_error(
    create_mvpa_design_from_dataset(mock_dset, ~y, ~block),
    "rMVPA package required"
  )
})

test_that("create_mvpa_dataset_from_dataset errors without rMVPA", {
  expect_error(
    create_mvpa_dataset_from_dataset(mock_dset),
    "rMVPA package required"
  )
})
