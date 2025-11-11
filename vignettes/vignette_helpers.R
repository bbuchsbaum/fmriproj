# Vignette Helper Functions
#
# These functions create small, synthetic fMRI datasets for vignette examples.
# They are designed to run quickly while demonstrating key functionality.

#' Create a simple synthetic fMRI dataset
#'
#' Creates a minimal matrix-based fMRI dataset with known structure for testing
#' and demonstration purposes. The dataset is small enough to run quickly but
#' large enough to demonstrate core functionality.
#'
#' @param n_voxels Number of voxels (default: 100)
#' @param n_timepoints Number of time points (default: 200)
#' @param n_trials Number of experimental trials (default: 40)
#' @param n_conditions Number of experimental conditions (default: 2)
#' @param TR Repetition time in seconds (default: 2)
#' @param signal_strength Strength of simulated signal (default: 0.5)
#' @return A list with components:
#'   - Y: matrix of simulated fMRI data (voxels x timepoints)
#'   - event_table: data frame with trial information
#'   - TR: repetition time
#'   - mask: simple ROI mask (logical vector)
#' @keywords internal
create_synthetic_fmri <- function(n_voxels = 100,
                                   n_timepoints = 200,
                                   n_trials = 40,
                                   n_conditions = 2,
                                   TR = 2,
                                   signal_strength = 0.5) {

  # Create event table with evenly spaced trials
  trial_onsets <- seq(10, n_timepoints * TR - 20, length.out = n_trials)
  trial_durations <- rep(2, n_trials)  # 2-second trials

  event_table <- data.frame(
    onset = trial_onsets,
    duration = trial_durations,
    condition = rep(paste0("cond", 1:n_conditions), length.out = n_trials),
    trial_id = 1:n_trials,
    run = 1  # Single run for simplicity
  )

  # Create synthetic fMRI data
  # Start with white noise
  Y <- matrix(rnorm(n_voxels * n_timepoints), nrow = n_voxels, ncol = n_timepoints)

  # Add simple signal at trial onsets
  # Convert onsets from seconds to TRs
  trial_trs <- round(trial_onsets / TR)

  # Add signal to a subset of "active" voxels
  n_active <- round(n_voxels * 0.3)  # 30% of voxels are "active"
  active_voxels <- 1:n_active

  # Simple box-car signal for each trial
  for (i in seq_along(trial_trs)) {
    tr_onset <- trial_trs[i]
    # Signal lasts for ~4 TRs (roughly HRF duration)
    tr_end <- min(tr_onset + 4, n_timepoints)

    # Different pattern for each condition
    cond_idx <- (i - 1) %% n_conditions + 1
    pattern <- rnorm(n_active, mean = signal_strength * cond_idx, sd = 0.2)

    Y[active_voxels, tr_onset:tr_end] <-
      Y[active_voxels, tr_onset:tr_end] +
      outer(pattern, rep(1, tr_end - tr_onset + 1))
  }

  # Create simple mask (all voxels included for simplicity)
  mask <- rep(TRUE, n_voxels)

  list(
    Y = Y,
    event_table = event_table,
    TR = TR,
    mask = mask,
    n_trials = n_trials,
    n_conditions = n_conditions
  )
}

#' Create synthetic fMRI dataset for searchlight demonstration
#'
#' Creates a 3D synthetic dataset with spatial structure suitable for
#' demonstrating searchlight analysis. Kept minimal for speed.
#'
#' @param dims Dimensions of 3D volume (default: c(5, 5, 5))
#' @param n_timepoints Number of time points (default: 150)
#' @param n_trials Number of trials (default: 30)
#' @param TR Repetition time (default: 2)
#' @return A list similar to create_synthetic_fmri but with 3D spatial info
#' @keywords internal
create_synthetic_searchlight_data <- function(dims = c(5, 5, 5),
                                               n_timepoints = 150,
                                               n_trials = 30,
                                               TR = 2) {

  n_voxels <- prod(dims)

  # Use the simple fmri creator
  data <- create_synthetic_fmri(
    n_voxels = n_voxels,
    n_timepoints = n_timepoints,
    n_trials = n_trials,
    TR = TR
  )

  # Add spatial information
  indices <- which(data$mask, arr.ind = FALSE)
  coords <- arrayInd(indices, .dim = dims)

  data$dims <- dims
  data$coords <- coords

  data
}

#' Create minimal event table for quick examples
#'
#' @param n_trials Number of trials (default: 20)
#' @param n_conditions Number of conditions (default: 2)
#' @return A data frame with onset, duration, condition, trial_id
#' @keywords internal
create_simple_events <- function(n_trials = 20, n_conditions = 2) {
  data.frame(
    onset = seq(10, 200, length.out = n_trials),
    duration = 2,
    condition = rep(paste0("cond", 1:n_conditions), length.out = n_trials),
    trial_id = 1:n_trials
  )
}
