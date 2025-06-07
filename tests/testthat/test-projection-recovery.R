test_that("projection accurately recovers known trial patterns from simulated data", {
  # This test creates synthetic data with known trial patterns and verifies
  # that the projection pipeline can recover them accurately
  
  set.seed(42)
  
  # Simulation parameters
  n_time <- 200
  n_voxels <- 50
  n_trials <- 10
  TR <- 2  # seconds
  
  # Create known trial patterns (ground truth)
  true_patterns <- matrix(rnorm(n_trials * n_voxels), 
                         nrow = n_trials, 
                         ncol = n_voxels)
  
  # Scale patterns to have different amplitudes
  trial_amplitudes <- seq(0.5, 2, length.out = n_trials)
  for (i in 1:n_trials) {
    true_patterns[i, ] <- true_patterns[i, ] * trial_amplitudes[i]
  }
  
  # Create event model with well-spaced trials
  trial_onsets <- seq(10, 180, length.out = n_trials)
  event_model <- list(
    onsets = trial_onsets,
    n_time = n_time,
    amplitudes = rep(1, n_trials)
  )
  
  # Generate true HRF (double gamma)
  true_hrf <- function(t) {
    # SPM canonical HRF parameters
    a1 <- 6; b1 <- 1
    a2 <- 16; b2 <- 1
    c <- 1/6
    
    h <- dgamma(t, a1, b1) - c * dgamma(t, a2, b2)
    h / max(h)
  }
  
  # Create time vector for HRF
  hrf_length <- 30  # seconds
  t_hrf <- seq(0, hrf_length, by = TR)
  hrf_values <- true_hrf(t_hrf)
  
  # Generate synthetic BOLD signal
  Y <- matrix(0, nrow = n_time, ncol = n_voxels)
  
  for (trial in 1:n_trials) {
    onset_idx <- round(trial_onsets[trial] / TR)
    
    # Convolve trial pattern with HRF
    for (t in 1:length(hrf_values)) {
      time_idx <- onset_idx + t - 1
      if (time_idx <= n_time) {
        Y[time_idx, ] <- Y[time_idx, ] + 
          true_patterns[trial, ] * hrf_values[t]
      }
    }
  }
  
  # Add realistic noise
  noise_level <- 0.5
  Y <- Y + matrix(rnorm(n_time * n_voxels, sd = noise_level), 
                  nrow = n_time)
  
  # Build projection pipeline
  hrf_basis <- matrix(hrf_values, ncol = 1)  # Use true HRF as basis
  
  design <- build_design_matrix(
    event_model = event_model,
    hrf_basis_matrix = hrf_basis,
    sparse = TRUE
  )
  
  expect_equal(nrow(design$X), n_time)
  expect_equal(ncol(design$X), n_trials)  # One column per trial
  
  # Build projector with small regularization
  proj_comp <- build_projector(
    X_theta = design$X,
    lambda_global = 0.01
  )
  
  # Apply projection
  proj_res <- adaptive_ridge_projector(
    Y_sl = Y,
    projector_components = proj_comp,
    lambda_adaptive_method = "none",
    lambda_floor_global = 0.01
  )
  
  # Collapse (no collapse needed with single basis)
  recovered_patterns <- matrix(proj_res$Z_sl_raw, 
                              nrow = n_trials, 
                              ncol = n_voxels)
  
  # Check recovery accuracy
  correlations <- numeric(n_trials)
  relative_errors <- numeric(n_trials)
  
  for (trial in 1:n_trials) {
    # Correlation between true and recovered patterns
    correlations[trial] <- cor(true_patterns[trial, ], 
                              recovered_patterns[trial, ])
    
    # Relative error in amplitude
    true_norm <- sqrt(sum(true_patterns[trial, ]^2))
    recovered_norm <- sqrt(sum(recovered_patterns[trial, ]^2))
    relative_errors[trial] <- abs(recovered_norm - true_norm) / true_norm
  }
  
  # Diagnostics
  cat("\nPattern recovery correlations:", round(correlations, 3), "\n")
  cat("Amplitude relative errors:", round(relative_errors, 3), "\n")
  cat("Mean correlation:", round(mean(correlations), 3), "\n")
  cat("Mean relative error:", round(mean(relative_errors), 3), "\n")
  
  # Assertions
  expect_true(all(correlations > 0.7), 
              info = "All trial patterns should be well recovered")
  expect_true(mean(correlations) > 0.85, 
              info = "Average correlation should be high")
  expect_true(all(relative_errors < 0.3), 
              info = "Amplitude errors should be reasonable")
  
  # Test that worse SNR degrades recovery predictably
  Y_noisy <- Y + matrix(rnorm(n_time * n_voxels, sd = 2), 
                        nrow = n_time)
  
  proj_res_noisy <- adaptive_ridge_projector(
    Y_sl = Y_noisy,
    projector_components = proj_comp,
    lambda_adaptive_method = "none",
    lambda_floor_global = 0.01
  )
  
  recovered_noisy <- matrix(proj_res_noisy$Z_sl_raw, 
                           nrow = n_trials, 
                           ncol = n_voxels)
  
  correlations_noisy <- numeric(n_trials)
  for (trial in 1:n_trials) {
    correlations_noisy[trial] <- cor(true_patterns[trial, ], 
                                    recovered_noisy[trial, ])
  }
  
  # Noisy data should have lower correlations
  expect_true(mean(correlations_noisy) < mean(correlations),
              info = "Noisier data should have worse recovery")
  
  # But still above chance
  expect_true(mean(correlations_noisy) > 0.5,
              info = "Even with noise, recovery should be above chance")
})


test_that("adaptive regularization responds appropriately to local SNR", {
  # This test verifies that the adaptive ridge regularization correctly
  # adjusts lambda based on local signal-to-noise characteristics
  
  set.seed(123)
  
  # Create three searchlights with different SNR characteristics
  n_time <- 100
  n_trials <- 8
  n_voxels_per_sl <- 27
  
  # Searchlight 1: High SNR (strong signal, low noise)
  sl1_signal_strength <- 3.0
  sl1_noise_level <- 0.5
  
  # Searchlight 2: Medium SNR  
  sl2_signal_strength <- 1.5
  sl2_noise_level <- 1.0
  
  # Searchlight 3: Low SNR (weak signal, high noise)
  sl3_signal_strength <- 0.5
  sl3_noise_level <- 2.0
  
  # Create event model
  trial_onsets <- seq(10, 80, length.out = n_trials)
  event_model <- list(
    onsets = trial_onsets,
    n_time = n_time
  )
  
  # Simple HRF basis (canonical + derivative)
  hrf_basis <- matrix(
    c(0, 0.2, 0.8, 1.0, 0.8, 0.4, 0.1, 0,     # Canonical
      0, 0.4, 0.4, 0, -0.4, -0.4, 0, 0),      # Derivative
    ncol = 2
  )
  
  # Build design matrix
  design <- build_design_matrix(
    event_model = event_model,
    hrf_basis_matrix = hrf_basis
  )
  
  # Build projector components
  proj_comp <- build_projector(
    X_theta = design$X,
    lambda_global = 0
  )
  
  # Function to generate searchlight data with specified SNR
  generate_sl_data <- function(signal_strength, noise_level) {
    Y_sl <- matrix(0, nrow = n_time, ncol = n_voxels_per_sl)
    
    # Add signal for each trial
    for (trial in 1:n_trials) {
      onset_idx <- round(trial_onsets[trial])
      signal_pattern <- rnorm(n_voxels_per_sl) * signal_strength
      
      # Add convolved signal
      for (t in 1:nrow(hrf_basis)) {
        if (onset_idx + t - 1 <= n_time) {
          Y_sl[onset_idx + t - 1, ] <- Y_sl[onset_idx + t - 1, ] + 
            signal_pattern * hrf_basis[t, 1]  # Use canonical HRF
        }
      }
    }
    
    # Add noise
    Y_sl + matrix(rnorm(n_time * n_voxels_per_sl, sd = noise_level), 
                  nrow = n_time)
  }
  
  # Generate data for each searchlight
  Y_sl1 <- generate_sl_data(sl1_signal_strength, sl1_noise_level)
  Y_sl2 <- generate_sl_data(sl2_signal_strength, sl2_noise_level)
  Y_sl3 <- generate_sl_data(sl3_signal_strength, sl3_noise_level)
  
  # Apply adaptive projection with EB method
  X_dense <- as.matrix(design$X)
  
  result_sl1 <- adaptive_ridge_projector(
    Y_sl = Y_sl1,
    projector_components = proj_comp,
    lambda_adaptive_method = "EB",
    lambda_floor_global = 0.001,
    X_theta_for_EB_residuals = X_dense,
    diagnostics = TRUE
  )
  
  result_sl2 <- adaptive_ridge_projector(
    Y_sl = Y_sl2,
    projector_components = proj_comp,
    lambda_adaptive_method = "EB",
    lambda_floor_global = 0.001,
    X_theta_for_EB_residuals = X_dense,
    diagnostics = TRUE
  )
  
  result_sl3 <- adaptive_ridge_projector(
    Y_sl = Y_sl3,
    projector_components = proj_comp,
    lambda_adaptive_method = "EB",
    lambda_floor_global = 0.001,
    X_theta_for_EB_residuals = X_dense,
    diagnostics = TRUE
  )
  
  # Extract chosen lambdas
  lambda_sl1 <- result_sl1$diag_data$lambda_sl_chosen
  lambda_sl2 <- result_sl2$diag_data$lambda_sl_chosen
  lambda_sl3 <- result_sl3$diag_data$lambda_sl_chosen
  
  cat("\nAdaptive lambda values:\n")
  cat("High SNR searchlight:", lambda_sl1, "\n")
  cat("Medium SNR searchlight:", lambda_sl2, "\n")
  cat("Low SNR searchlight:", lambda_sl3, "\n")
  
  # Lambda should increase as SNR decreases
  expect_true(lambda_sl1 < lambda_sl2, 
              info = "High SNR should have lower lambda than medium SNR")
  expect_true(lambda_sl2 < lambda_sl3,
              info = "Medium SNR should have lower lambda than low SNR")
  
  # The differences should be meaningful
  expect_true(lambda_sl3 / lambda_sl1 > 2,
              info = "Lambda should vary substantially with SNR")
  
  # Test projection quality
  # For each searchlight, check if projection preserves signal structure
  check_projection_quality <- function(result, original_Y, desc) {
    Z <- result$Z_sl_raw
    
    # Reshape to trials x bases x voxels
    Z_array <- array(Z, dim = c(n_trials, 2, n_voxels_per_sl))
    
    # Check that canonical basis captures most variance
    var_canonical <- mean(apply(Z_array[, 1, ], 2, var))
    var_derivative <- mean(apply(Z_array[, 2, ], 2, var))
    
    expect_true(var_canonical > var_derivative,
                info = paste(desc, ": Canonical should capture more variance"))
    
    # Check that projection reduces noise
    # Project back to time series
    X_subset <- as.matrix(design$X)
    Y_reconstructed <- X_subset %*% Z
    
    # Residual should be mostly noise
    residuals <- original_Y - Y_reconstructed
    
    # In high SNR, residual variance should be close to noise variance
    residual_var <- mean(apply(residuals, 2, var))
    
    list(
      canonical_var = var_canonical,
      derivative_var = var_derivative,
      residual_var = residual_var,
      description = desc
    )
  }
  
  quality_sl1 <- check_projection_quality(result_sl1, Y_sl1, "High SNR")
  quality_sl2 <- check_projection_quality(result_sl2, Y_sl2, "Medium SNR")
  quality_sl3 <- check_projection_quality(result_sl3, Y_sl3, "Low SNR")
  
  # High SNR should have better signal preservation
  expect_true(quality_sl1$canonical_var > quality_sl3$canonical_var,
              info = "High SNR should preserve more signal variance")
  
  # Test cross-validation lambda selection
  result_cv <- adaptive_ridge_projector(
    Y_sl = Y_sl2,  # Use medium SNR
    projector_components = proj_comp,
    lambda_adaptive_method = "LOOcv_local",
    lambda_floor_global = 0.001,
    X_theta_for_EB_residuals = X_dense,
    lambda_grid_local = c(0.001, 0.01, 0.1, 1, 10),
    diagnostics = TRUE
  )
  
  lambda_cv <- result_cv$diag_data$lambda_sl_chosen
  
  # CV lambda should be reasonable (not at extremes of grid)
  expect_true(lambda_cv > 0.001 && lambda_cv < 10,
              info = "CV should select intermediate lambda")
  
  # CV and EB should give similar results for medium SNR
  expect_true(abs(log10(lambda_cv) - log10(lambda_sl2)) < 1,
              info = "CV and EB methods should give similar lambdas")
})