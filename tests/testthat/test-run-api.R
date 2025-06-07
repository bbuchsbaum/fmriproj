context("run API")

fmri_dset <- list(
  event_table = data.frame(y = 1L, block = 1L, onset = 1L),
  sampling_frame = list(blocklens = 2L),
  data = matrix(1, nrow = 2, ncol = 1),
  mask = matrix(1)
)
class(fmri_dset) <- "fmri_dataset"

Y <- matrix(1, nrow = 2, ncol = 1)
mask <- matrix(1)
em <- list(onsets = 1L, n_time = 2L, conditions = 1L)

# These functions require rMVPA, so check that they error when missing

test_that("run_searchlight errors without rMVPA", {
  expect_error(
    run_searchlight(fmri_dset, radius = 3, y_formula = ~y, block_formula = ~block),
    "rMVPA package required"
  )
})

test_that("run_regional errors without rMVPA", {
  expect_error(
    run_regional(fmri_dset, region_mask = mask, y_formula = ~y, block_formula = ~block),
    "rMVPA package required"
  )
})
