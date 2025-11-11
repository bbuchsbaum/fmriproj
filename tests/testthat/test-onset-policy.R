context("Onset policy: seconds to indices conversion")

test_that("build_design_matrix converts seconds using rounding policies", {
  n_time <- 20L
  TR <- 2
  onsets_sec <- c(0.0, 1.5, 3.0)
  # Simple basis: L = 3, K = 1
  basis <- matrix(c(1, 2, 3), nrow = 3, ncol = 1)

  # nearest rounding
  em <- list(onsets = onsets_sec, n_time = n_time)
  Xn <- build_design_matrix(em, hrf_basis_matrix = basis, TR = TR, rounding = "nearest")$X
  idx_nearest <- c(0, 1, 2)
  for (t in seq_along(onsets_sec)) {
    col <- (t - 1) * 1 + 1
    # Nonzero rows are onset_idx + 0:(L-1); convert to 1-based
    expect_equal(which(Xn[, col] != 0), idx_nearest[t] + 1 + 0:2)
  }

  # floor rounding
  Xf <- build_design_matrix(em, hrf_basis_matrix = basis, TR = TR, rounding = "floor")$X
  idx_floor <- c(0, 0, 1)
  for (t in seq_along(onsets_sec)) {
    col <- (t - 1) * 1 + 1
    expect_equal(which(Xf[, col] != 0), idx_floor[t] + 1 + 0:2)
  }

  # ceil rounding
  Xc <- build_design_matrix(em, hrf_basis_matrix = basis, TR = TR, rounding = "ceil")$X
  idx_ceil <- c(0, 1, 2) # 0/2=0, 1.5/2=0.75->1, 3/2=1.5->2
  for (t in seq_along(onsets_sec)) {
    col <- (t - 1) * 1 + 1
    expect_equal(which(Xc[, col] != 0), idx_ceil[t] + 1 + 0:2)
  }
})

test_that("build_design_matrix handles per-run onsets with n_time_per_run", {
  # Two runs of equal length
  n_time_per_run <- c(10L, 10L)
  n_time <- sum(n_time_per_run)
  TR <- 1
  # Onsets: last sample in run1 (~9), start and overrun in run2 (~0.6 and 10.1)
  onsets_sec <- c(9.4, 0.6, 10.1)
  run_id <- c(1, 2, 2)
  basis <- matrix(c(1, 2, 3), nrow = 3, ncol = 1)
  em <- list(onsets = onsets_sec, n_time = n_time, n_time_per_run = n_time_per_run)

  X <- build_design_matrix(em, hrf_basis_matrix = basis, TR = TR, rounding = "nearest", run_id = run_id)$X
  # Expected 0-based onset indices with run offsets (0, 10) and rounding + clamp
  # 9.4 -> 9 in run1 (offset 0)
  # 0.6 -> 1 in run2 (offset 10) => 11
  # 10.1 -> 10 in run2, clamp to 9, offset 10 => 19
  idx0 <- c(9, 11, 19)
  for (t in seq_along(onsets_sec)) {
    col <- (t - 1) * 1 + 1
    expect_equal(which(X[, col] != 0)[1], idx0[t] + 1)
  }
})

test_that("build_design_matrix errors without TR when onset_units = 'sec' is implied", {
  n_time <- 10
  em <- list(onsets = c(0.1, 1.3), n_time = n_time)
  expect_error(build_design_matrix(em, hrf_basis_matrix = matrix(1, 3, 1)),
               "TR must be provided")
})

