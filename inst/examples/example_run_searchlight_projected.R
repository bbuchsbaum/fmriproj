## Example 1: Basic projected searchlight analysis
# Simulate a small fMRI dataset
set.seed(789)
n_time <- 200
n_voxels <- 1000
n_trials <- 10

# Create event model with trials spread across the run
trial_onsets <- seq(10, 190, length.out = n_trials)
event_model <- list(
  onsets = trial_onsets,
  n_time = n_time,
  amplitudes = rep(1, n_trials)
)

# Simulate BOLD data with embedded signal
Y <- matrix(rnorm(n_time * n_voxels), nrow = n_time)

# Add signal to specific voxels for specific trials
signal_voxels <- 100:150
for (i in 1:5) {  # First 5 trials have signal
  onset_idx <- round(trial_onsets[i])
  Y[(onset_idx):(onset_idx + 5), signal_voxels] <- 
    Y[(onset_idx):(onset_idx + 5), signal_voxels] + 2
}

# Define HRF basis
hrf_basis <- matrix(
  c(0, 0.2, 0.8, 1.0, 0.8, 0.4, 0.1, 0,    # Main HRF
    0, 0.1, 0.2, 0.1, -0.1, -0.2, -0.1, 0), # Derivative
  nrow = 8, ncol = 2
)

# Run projected searchlight (returns components if rMVPA not available)
result <- run_searchlight_projected(
  Y = Y,
  event_model = event_model,
  hrf_basis_matrix = hrf_basis,
  lambda_global = 0.1,
  lambda_adaptive_method = "none",
  collapse_method = "rss",
  diagnostics = FALSE
)

# If rMVPA is not installed, we get the components
if (is.list(result) && "searchlight_fun" %in% names(result)) {
  cat("Components returned (rMVPA not available):\n")
  print(names(result))
  
  # Can manually apply the searchlight function
  sl_indices <- list(
    sl1 = 1:50,
    sl2 = 51:100,
    sl3 = 101:150  # This should show signal
  )
  
  for (sl_name in names(sl_indices)) {
    sl_result <- result$searchlight_fun(Y[, sl_indices[[sl_name]]])
    cat("\n", sl_name, "dimensions:", dim(sl_result), "\n")
  }
}

## Example 2: Using different adaptive methods
# With Empirical Bayes adaptation
result_eb <- run_searchlight_projected(
  Y = Y,
  event_model = event_model,
  hrf_basis_matrix = hrf_basis,
  lambda_global = 0.01,
  lambda_adaptive_method = "EB",
  collapse_method = "rss",
  diagnostics = TRUE
)

# With local cross-validation
result_cv <- run_searchlight_projected(
  Y = Y,
  event_model = event_model,
  hrf_basis_matrix = hrf_basis,
  lambda_global = 0.01,
  lambda_adaptive_method = "LOOcv_local",
  collapse_method = "pc",  # Also use PC collapse
  diagnostics = TRUE
)

## Example 3: Using dynamic HRF basis function
# Define parameterizable HRF function
flexible_hrf <- function(theta_params, time_vector) {
  # theta_params[1] = time to peak
  # theta_params[2] = width
  ttp <- theta_params[1]
  width <- theta_params[2]
  
  # Gamma-like HRF
  hrf_main <- dgamma(time_vector, shape = ttp/width, scale = width)
  hrf_max <- max(hrf_main)
  hrf_main <- hrf_main / hrf_max
  
  # Simple derivative
  hrf_deriv <- c(0, diff(hrf_main))
  
  cbind(hrf_main, hrf_deriv)
}

# Run with specific HRF parameters
result_flex <- run_searchlight_projected(
  Y = Y,
  event_model = event_model,
  hrf_basis_func = flexible_hrf,
  theta_params = c(5, 1),  # Peak at 5s, width 1s
  lambda_global = 0.1,
  lambda_adaptive_method = "none",
  collapse_method = "rss"
)

## Example 4: Integrated pipeline with all options
# This demonstrates the full pipeline with diagnostics
result_full <- run_searchlight_projected(
  Y = Y,
  event_model = event_model,
  hrf_basis_matrix = hrf_basis,
  lambda_global = 0.05,
  lambda_adaptive_method = "EB",
  collapse_method = "pc",
  diagnostics = TRUE
)

# Access diagnostic information if available
if (is.list(result_full) && "diagnostics" %in% names(result_full)) {
  diag <- result_full$diagnostics
  cat("\nDiagnostic information available:\n")
  cat("Design matrix build time:", diag$design_matrix$time_to_build, "sec\n")
  cat("Projector build time:", diag$projector$time_to_build, "sec\n")
  cat("Condition number:", diag$projector$cond_R, "\n")
}

## Example 5: Handling large datasets efficiently
# For large datasets, sparse matrices are crucial
large_event_model <- list(
  onsets = seq(10, 990, by = 20),  # 50 trials
  n_time = 1000
)

# Simulate large but sparse Y
Y_large_sparse <- Matrix::Matrix(0, nrow = 1000, ncol = 10000, sparse = TRUE)
# Add some random non-zero elements
idx <- sample(length(Y_large_sparse), size = 50000)
Y_large_sparse[idx] <- rnorm(50000)

# This will use sparse operations throughout
result_large <- run_searchlight_projected(
  Y = as.matrix(Y_large_sparse[, 1:100]),  # Use subset for example
  event_model = large_event_model,
  hrf_basis_matrix = hrf_basis,
  lambda_global = 0.1,
  lambda_adaptive_method = "none",
  collapse_method = "rss",
  diagnostics = TRUE
)