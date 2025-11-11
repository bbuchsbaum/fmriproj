context("ls+svd HRF recovery")

test_that("LS projection with bspline basis recovers SPMG1 HRF", {
  if (!requireNamespace("fmrireg", quietly = TRUE) ||
      !requireNamespace("fmrihrf", quietly = TRUE)) {
    skip("fmrireg and fmrihrf required")
  }

  set.seed(123)
  sim <- fmrireg::simulate_bold_signal(ncond = 1,
                                       hrf = fmrihrf::HRF_SPMG1,
                                       nreps = 40,
                                       isi = c(1, 3),
                                       TR = 1)

  time <- sim$mat[, 1]
  bold <- sim$mat[, 2]

  em <- list(onsets = as.integer(round(sim$onset)),
             n_time = length(time),
             basis_length = 30L)

  # Use fmrihrf B-spline basis via adapter
  hrf_func <- create_hrf_basis_func(get_hrf_by_name("bspline"))
  design_obj <- build_design_matrix(em,
                                    hrf_basis_func = hrf_func,
                                    sparse = FALSE)
  basis <- design_obj$hrf_info$basis
  n_time <- length(time)
  stim <- rep(0, n_time)
  onset_idx <- em$onsets + 1L
  onset_idx <- onset_idx[onset_idx >= 1 & onset_idx <= n_time]
  stim[onset_idx] <- 1

  # Build convolution-style design where each column corresponds to an HRF basis
  conv_design <- matrix(0, n_time, ncol(basis))
  L <- nrow(basis)
  for (k in seq_len(ncol(basis))) {
    h <- basis[, k]
    for (onset in onset_idx) {
      len <- min(L, n_time - onset + 1L)
      if (len > 0L) {
        conv_design[onset:(onset + len - 1), k] <-
          conv_design[onset:(onset + len - 1), k] + h[seq_len(len)]
      }
    }
  }

  beta_basis <- MASS::ginv(conv_design) %*% bold
  hrf_est <- drop(basis %*% beta_basis)
  if (sum(hrf_est) < 0) hrf_est <- -hrf_est

  true_hrf <- fmrihrf::HRF_SPMG1(seq_along(hrf_est) - 1)
  expect_gt(cor(hrf_est, true_hrf), 0.9)
})
