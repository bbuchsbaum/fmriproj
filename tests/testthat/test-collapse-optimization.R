test_that("collapse methods correctly combine HRF basis functions", {
  # This test verifies that different collapse methods (RSS, PC, optim)
  # produce sensible results and that optimized weights improve classification
  
  set.seed(789)
  
  # Create a scenario where different HRF components matter
  n_trials <- 40
  n_voxels <- 30
  k_bases <- 3
  
  # Create two classes with different HRF characteristics
  # Class 1: Standard HRF timing (high canonical, low derivatives)
  # Class 2: Shifted HRF timing (lower canonical, higher derivatives)
  
  class_labels <- rep(c(0, 1), each = n_trials/2)
  
  # Generate Z_sl_raw with class-specific patterns
  Z_sl_raw <- matrix(0, nrow = n_trials * k_bases, ncol = n_voxels)
  
  for (trial in 1:n_trials) {
    for (voxel in 1:n_voxels) {
      base_signal <- rnorm(1)
      
      if (class_labels[trial] == 0) {
        # Class 0: Standard timing
        Z_sl_raw[(trial-1)*k_bases + 1, voxel] <- base_signal * 2.0  # Strong canonical
        Z_sl_raw[(trial-1)*k_bases + 2, voxel] <- base_signal * 0.3  # Weak temporal deriv
        Z_sl_raw[(trial-1)*k_bases + 3, voxel] <- base_signal * 0.2  # Weak dispersion
      } else {
        # Class 1: Shifted timing  
        Z_sl_raw[(trial-1)*k_bases + 1, voxel] <- base_signal * 1.0  # Moderate canonical
        Z_sl_raw[(trial-1)*k_bases + 2, voxel] <- base_signal * 1.5  # Strong temporal deriv
        Z_sl_raw[(trial-1)*k_bases + 3, voxel] <- base_signal * 0.8  # Moderate dispersion
      }
    }
  }
  
  # Add noise
  Z_sl_raw <- Z_sl_raw + matrix(rnorm(length(Z_sl_raw), sd = 0.3), 
                                 nrow = nrow(Z_sl_raw))
  
  # Test 1: RSS collapse (unsupervised)
  result_rss <- collapse_beta(
    Z_sl_raw = Z_sl_raw,
    N_trials = n_trials,
    K_hrf_bases = k_bases,
    method = "rss",
    diagnostics = TRUE
  )
  
  expect_equal(dim(result_rss$A_sl), c(n_trials, n_voxels))
  
  # RSS should produce all positive values
  expect_true(all(result_rss$A_sl >= 0),
              info = "RSS collapse should produce non-negative values")
  
  # Test 2: PC collapse (unsupervised, SNR-optimal)
  result_pc <- collapse_beta(
    Z_sl_raw = Z_sl_raw,
    N_trials = n_trials,
    K_hrf_bases = k_bases,
    method = "pc",
    diagnostics = TRUE
  )
  
  # PC weights should be normalized
  expect_equal(sum(result_pc$w_sl^2), 1, tolerance = 1e-6,
               info = "PC weights should be unit normalized")
  
  # PC can produce negative values (unlike RSS)
  expect_true(any(result_pc$A_sl < 0),
              info = "PC collapse can produce negative values")
  
  # Test 3: Optimized collapse (supervised)
  
  # Define a simple classifier for optimization
  classifier_func <- function(A_sl, labels) {
    # Linear discriminant classifier
    n <- nrow(A_sl)
    
    # Compute class means
    mean_0 <- colMeans(A_sl[labels == 0, , drop = FALSE])
    mean_1 <- colMeans(A_sl[labels == 1, , drop = FALSE])
    
    # LDA direction
    diff_means <- mean_1 - mean_0
    
    # Project data onto discriminant direction
    scores <- A_sl %*% diff_means
    
    # Compute loss (negative correlation with true labels)
    loss <- -cor(scores, labels)
    
    # Gradient w.r.t. A_sl
    grad_scores <- -1/n * (labels - mean(labels)) / sd(labels) / sd(scores)
    grad <- grad_scores %*% t(diff_means)
    
    list(loss = loss, grad = grad)
  }
  
  result_optim <- collapse_beta(
    Z_sl_raw = Z_sl_raw,
    N_trials = n_trials,
    K_hrf_bases = k_bases,
    method = "optim",
    labels_for_w_optim = class_labels,
    classifier_for_w_optim = classifier_func,
    optim_w_params = list(maxit = 20, method = "L-BFGS-B"),
    diagnostics = TRUE
  )
  
  # Optimized weights should be normalized
  expect_equal(sum(result_optim$w_sl^2), 1, tolerance = 1e-6,
               info = "Optimized weights should be unit normalized")
  
  # Test classification performance with each collapse method
  evaluate_classification <- function(A_sl, labels, method_name) {
    # Simple leave-one-out classification
    correct <- 0
    
    for (i in 1:n_trials) {
      # Training data (all except i)
      A_train <- A_sl[-i, , drop = FALSE]
      y_train <- labels[-i]
      
      # Test point
      A_test <- A_sl[i, , drop = FALSE]
      y_test <- labels[i]
      
      # Compute class means
      mean_0 <- colMeans(A_train[y_train == 0, , drop = FALSE])
      mean_1 <- colMeans(A_train[y_train == 1, , drop = FALSE])
      
      # Classify based on nearest mean
      dist_0 <- sum((A_test - mean_0)^2)
      dist_1 <- sum((A_test - mean_1)^2)
      
      pred <- ifelse(dist_0 < dist_1, 0, 1)
      correct <- correct + (pred == y_test)
    }
    
    accuracy <- correct / n_trials
    cat("\n", method_name, "accuracy:", accuracy, "\n")
    accuracy
  }
  
  acc_rss <- evaluate_classification(result_rss$A_sl, class_labels, "RSS")
  acc_pc <- evaluate_classification(result_pc$A_sl, class_labels, "PC")
  acc_optim <- evaluate_classification(result_optim$A_sl, class_labels, "Optimized")
  
  # Optimized should perform better than unsupervised methods
  expect_true(acc_optim >= acc_rss,
              info = "Optimized collapse should match or beat RSS")
  expect_true(acc_optim >= acc_pc,
              info = "Optimized collapse should match or beat PC")
  
  # All methods should be above chance
  expect_true(acc_rss > 0.5,
              info = "RSS should be above chance")
  expect_true(acc_pc > 0.5,
              info = "PC should be above chance")
  expect_true(acc_optim > 0.6,
              info = "Optimized should be well above chance")
  
  # Examine the learned weights
  cat("\nLearned weights:\n")
  cat("RSS (implicit):", "equal weighting\n")
  cat("PC weights:", round(result_pc$w_sl, 3), "\n")
  cat("Optimized weights:", round(result_optim$w_sl, 3), "\n")
  
  # For this specific problem, optimized weights should emphasize
  # the temporal derivative (component 2) since it differentiates classes
  expect_true(abs(result_optim$w_sl[2]) > abs(result_optim$w_sl[3]),
              info = "Temporal derivative should have high weight")
  
  # Test edge cases
  
  # Edge case 1: Single basis function (no collapse needed)
  Z_single <- matrix(rnorm(n_trials * n_voxels), nrow = n_trials)
  result_single <- collapse_beta(
    Z_sl_raw = Z_single,
    N_trials = n_trials,
    K_hrf_bases = 1,
    method = "rss"
  )
  
  # Should essentially pass through
  expect_equal(result_single$A_sl, abs(Z_single),
               info = "Single basis RSS should just take absolute value")
  
  # Edge case 2: Very few observations for PC
  Z_tiny <- matrix(rnorm(2 * 2 * 3), nrow = 2 * 2)  # 2 trials, 2 bases, 3 voxels
  expect_warning({
    result_tiny <- collapse_beta(
      Z_sl_raw = Z_tiny,
      N_trials = 2,
      K_hrf_bases = 2,
      method = "pc"
    )
  }, "Not enough observations")
  
  # Should fall back gracefully
  expect_equal(dim(result_tiny$A_sl), c(2, 3))
})


test_that("HRF optimization improves MVPA performance on appropriate problems", {
  # This test verifies that optimizing HRF parameters (theta) actually
  # improves classification when there are subject/session-specific HRF differences
  
  set.seed(456)
  
  # Simulate data with non-standard HRF
  n_time <- 150
  n_voxels <- 100
  n_trials <- 12
  TR <- 2
  
  # True HRF has delayed peak (7s instead of typical 5s)
  true_peak_time <- 7
  
  # Create two conditions with different spatial patterns
  condition_labels <- rep(c(0, 1), each = n_trials/2)
  
  # Spatial patterns for each condition
  pattern_cond0 <- rnorm(n_voxels)
  pattern_cond0[1:50] <- pattern_cond0[1:50] + 1  # First half active
  
  pattern_cond1 <- rnorm(n_voxels)  
  pattern_cond1[51:100] <- pattern_cond1[51:100] + 1  # Second half active
  
  # Event timing
  trial_onsets <- seq(15, 135, length.out = n_trials)
  event_model <- list(
    onsets = trial_onsets,
    n_time = n_time,
    conditions = condition_labels
  )
  
  # Generate data with true (shifted) HRF
  generate_data_with_hrf <- function(peak_time) {
    Y <- matrix(0, nrow = n_time, ncol = n_voxels)
    
    # True HRF shape
    t_hrf <- seq(0, 20, by = TR)
    hrf <- dgamma(t_hrf, shape = peak_time, rate = 1)
    hrf <- hrf / max(hrf)
    
    # Generate signal
    for (trial in 1:n_trials) {
      onset_idx <- round(trial_onsets[trial] / TR)
      
      if (condition_labels[trial] == 0) {
        trial_pattern <- pattern_cond0
      } else {
        trial_pattern <- pattern_cond1
      }
      
      # Convolve with HRF
      for (t in 1:length(hrf)) {
        if (onset_idx + t - 1 <= n_time) {
          Y[onset_idx + t - 1, ] <- Y[onset_idx + t - 1, ] + 
            trial_pattern * hrf[t]
        }
      }
    }
    
    # Add noise
    Y + matrix(rnorm(n_time * n_voxels, sd = 1), nrow = n_time)
  }
  
  Y <- generate_data_with_hrf(true_peak_time)
  
  # Define flexible HRF basis function  
  flexible_hrf <- function(theta_params, time_vector) {
    peak <- theta_params[1]
    
    # Main function
    hrf_main <- dgamma(time_vector, shape = peak, rate = 1)
    if (max(hrf_main) > 0) hrf_main <- hrf_main / max(hrf_main)
    
    # Derivative
    hrf_deriv <- c(0, diff(hrf_main))
    
    cbind(hrf_main, hrf_deriv)
  }
  
  # Inner CV function for optimization
  inner_cv <- function(A_sl, labels = condition_labels) {
    # Leave-one-out CV
    correct <- 0
    
    for (i in 1:nrow(A_sl)) {
      train_idx <- setdiff(1:nrow(A_sl), i)
      
      # LDA-style classification
      mean_0 <- colMeans(A_sl[train_idx[labels[train_idx] == 0], ])
      mean_1 <- colMeans(A_sl[train_idx[labels[train_idx] == 1], ])
      
      test_point <- A_sl[i, ]
      dist_0 <- sum((test_point - mean_0)^2)
      dist_1 <- sum((test_point - mean_1)^2)
      
      pred <- ifelse(dist_0 < dist_1, 0, 1)
      correct <- correct + (pred == labels[i])
    }
    
    # Return loss (1 - accuracy)
    1 - (correct / nrow(A_sl))
  }
  
  # Test 1: Performance with wrong HRF (standard peak at 5s)
  wrong_theta <- c(5)  # Standard HRF peak
  
  design_wrong <- build_design_matrix(
    event_model = event_model,
    hrf_basis_func = flexible_hrf,
    theta_params = wrong_theta
  )
  
  proj_wrong <- build_projector(design_wrong$X, lambda_global = 0.1)
  
  result_wrong <- adaptive_ridge_projector(
    Y_sl = Y,
    projector_components = proj_wrong,
    lambda_adaptive_method = "none",
    lambda_floor_global = 0.1
  )
  
  collapsed_wrong <- collapse_beta(
    result_wrong$Z_sl_raw,
    N_trials = n_trials,
    K_hrf_bases = 2,
    method = "rss"
  )
  
  loss_wrong <- inner_cv(collapsed_wrong$A_sl)
  acc_wrong <- 1 - loss_wrong
  
  cat("\nAccuracy with wrong HRF (peak=5s):", acc_wrong, "\n")
  
  # Test 2: Performance with correct HRF
  correct_theta <- c(true_peak_time)
  
  design_correct <- build_design_matrix(
    event_model = event_model,
    hrf_basis_func = flexible_hrf,
    theta_params = correct_theta
  )
  
  proj_correct <- build_projector(design_correct$X, lambda_global = 0.1)
  
  result_correct <- adaptive_ridge_projector(
    Y_sl = Y,
    projector_components = proj_correct,
    lambda_adaptive_method = "none",
    lambda_floor_global = 0.1
  )
  
  collapsed_correct <- collapse_beta(
    result_correct$Z_sl_raw,
    N_trials = n_trials,
    K_hrf_bases = 2,
    method = "rss"
  )
  
  loss_correct <- inner_cv(collapsed_correct$A_sl)
  acc_correct <- 1 - loss_correct
  
  cat("Accuracy with correct HRF (peak=7s):", acc_correct, "\n")
  
  # Correct HRF should give better performance
  expect_true(acc_correct > acc_wrong,
              info = "Correct HRF should improve classification")
  
  # Test 3: Optimize HRF parameters
  result_optim <- optimize_hrf_mvpa(
    theta_init = c(5),  # Start with wrong value
    Y = Y,
    event_model = event_model,
    inner_cv_fn = inner_cv,
    hrf_basis_func = flexible_hrf,
    lambda_global = 0.1,
    lambda_adaptive_method = "none",
    collapse_method = "rss",
    optim_method = "Brent",
    lower = 3,
    upper = 10,
    diagnostics = TRUE
  )
  
  optimized_peak <- result_optim$theta_hat[1]
  
  cat("Optimized HRF peak:", optimized_peak, "s\n")
  cat("True HRF peak:", true_peak_time, "s\n")
  
  # Should recover approximately correct peak
  expect_equal(optimized_peak, true_peak_time, tolerance = 1.5,
               info = "Should recover approximately correct HRF peak")
  
  # Performance with optimized HRF should be good
  final_loss <- result_optim$optim_details$value
  acc_optimized <- 1 - final_loss
  
  cat("Accuracy with optimized HRF:", acc_optimized, "\n")
  
  expect_true(acc_optimized >= acc_wrong,
              info = "Optimized HRF should match or beat wrong HRF")
  
  expect_true(acc_optimized > 0.7,
              info = "Optimized HRF should achieve good accuracy")
  
  # Check optimization trajectory
  if (!is.null(result_optim$diagnostics$theta_trace)) {
    trace <- result_optim$diagnostics$theta_trace
    
    # Should show improvement over iterations
    initial_loss <- trace$loss[1]
    final_loss <- trace$loss[nrow(trace)]
    
    expect_true(final_loss <= initial_loss,
                info = "Optimization should improve or maintain loss")
    
    # Theta should move toward true value
    initial_theta <- trace$theta_1[1]
    final_theta <- trace$theta_1[nrow(trace)]
    
    initial_error <- abs(initial_theta - true_peak_time)
    final_error <- abs(final_theta - true_peak_time)
    
    expect_true(final_error < initial_error,
                info = "Optimization should move toward true HRF")
  }
  
  # Test 4: Robustness to initialization
  # Try different starting points
  starting_peaks <- c(3, 5, 9)
  recovered_peaks <- numeric(length(starting_peaks))
  
  for (i in seq_along(starting_peaks)) {
    result_i <- optimize_hrf_mvpa(
      theta_init = starting_peaks[i],
      Y = Y,
      event_model = event_model,
      inner_cv_fn = inner_cv,
      hrf_basis_func = flexible_hrf,
      lambda_global = 0.1,
      lambda_adaptive_method = "none",
      collapse_method = "rss",
      optim_method = "Brent",
      lower = 3,
      upper = 10,
      diagnostics = FALSE
    )
    
    recovered_peaks[i] <- result_i$theta_hat[1]
  }
  
  cat("\nRecovered peaks from different starting points:\n")
  cat("Started at:", starting_peaks, "\n")
  cat("Recovered:", round(recovered_peaks, 2), "\n")
  
  # Should converge to similar values
  expect_true(sd(recovered_peaks) < 1,
              info = "Optimization should be robust to initialization")
})