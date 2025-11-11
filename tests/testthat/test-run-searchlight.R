context("run_searchlight end-to-end")

test_that("run_searchlight returns searchlight_result object", {
  skip_if_not_installed("rMVPA")
  skip_if_not_installed("neuroim2")

  # Build a small 3D NeuroVec + NeuroVol so rMVPA can form neighborhoods
  set.seed(123)
  nx <- 6; ny <- 6; nz <- 4; nt <- 40
  space_xyz  <- neuroim2::NeuroSpace(c(nx, ny, nz))
  space_xyzt <- neuroim2::add_dim(space_xyz, nt)
  arr <- array(rnorm(nx * ny * nz * nt), dim = c(nx, ny, nz, nt))
  bold_vec <- neuroim2::DenseNeuroVec(arr, space_xyzt)
  mask_vol <- neuroim2::NeuroVol(array(1, dim = c(nx, ny, nz)), space_xyz)

  # Simple events: 8 trials alternating conditions
  events <- data.frame(
    onset = seq(2, 32, by = 4),
    condition = rep(c("A", "B"), times = 4),
    run = 1
  )

  # Minimal fmri_dset stub with required fields
  fmri_dset <- list(
    event_table = events,
    sampling_frame = data.frame(block = 1, blocklens = nt)
  )
  class(fmri_dset) <- "fmri_dataset"

  # Provide S3 methods so fmridataset::get_data/get_mask can retrieve Neuro* objects
  get_data.fmri_dataset <- function(x, ...) bold_vec
  get_mask.fmri_dataset <- function(x, ...) mask_vol
  # Register into search path for this test
  assign("get_data.fmri_dataset", get_data.fmri_dataset, envir = .GlobalEnv)
  assign("get_mask.fmri_dataset", get_mask.fmri_dataset, envir = .GlobalEnv)

  on.exit({
    rm(get_data.fmri_dataset, envir = .GlobalEnv)
    rm(get_mask.fmri_dataset, envir = .GlobalEnv)
  }, add = TRUE)

  # Run wrapper
  res <- run_searchlight(
    fmri_dset,
    radius = 1,
    y_formula = ~condition,
    block_formula = ~run
  )

  expect_s3_class(res, "searchlight_result")
})
