## Example 1: Basic usage with pre-computed HRF basis
# Simulate a simple event model with 3 trials
event_model <- list(
  onsets = c(2, 8, 14),  # Trial onsets in seconds
  n_time = 20,           # Total time points (e.g., 20 TRs)
  amplitudes = c(1, 1, 1) # Equal amplitude for all trials
)

# Create a simple HRF basis matrix (2 basis functions, 5 time points)
hrf_basis <- matrix(
  c(0, 0.5, 1, 0.5, 0,   # First basis function (main HRF)
    0, 0.1, 0.2, 0.1, 0), # Second basis function (derivative)
  nrow = 5, ncol = 2
)

# Build the design matrix
design <- build_design_matrix(
  event_model = event_model,
  hrf_basis_matrix = hrf_basis,
  sparse = TRUE
)

# Inspect the result
print(design)
dim(design$X)  # Should be 20 x 6 (3 trials × 2 basis functions)

## Example 2: Using parametric modulators
# Event model with parametric modulation (e.g., stimulus intensity)
event_model_pm <- list(
  onsets = c(2, 8, 14),
  n_time = 20,
  amplitudes = c(1, 1, 1),
  modulator = c(0.5, 1.0, 1.5)  # Different intensities
)

design_pm <- build_design_matrix(
  event_model = event_model_pm,
  hrf_basis_matrix = hrf_basis,
  sparse = TRUE,
  diagnostics = TRUE
)

# Check build time
attr(design_pm, "diagnostics")$time_to_build

## Example 3: Using an HRF basis function with parameters
# Define a simple parameterizable HRF basis function
simple_hrf_func <- function(theta_params, time_vector) {
  # theta_params[1] controls the peak time
  peak_time <- theta_params[1]
  hrf <- dnorm(time_vector, mean = peak_time, sd = 1)
  hrf_max <- max(hrf)
  hrf <- hrf / hrf_max  # Normalize
  
  # Return as matrix (length x 1 basis)
  matrix(hrf, ncol = 1)
}

# Generate design matrix with custom HRF
design_custom <- build_design_matrix(
  event_model = event_model,
  hrf_basis_func = simple_hrf_func,
  theta_params = c(3),  # Peak at 3 seconds
  sparse = FALSE  # Use dense matrix for this example
)

## Example 4: Warning for large design matrices
# Create event model that would generate many columns
large_event_model <- list(
  onsets = 1:100,  # 100 trials
  n_time = 500     # 500 time points
)

# This should trigger a warning if using many basis functions
tryCatch({
  design_large <- build_design_matrix(
    event_model = large_event_model,
    hrf_basis_matrix = matrix(rnorm(10 * 200), nrow = 10, ncol = 200),
    max_X_cols = 15000  # Safety threshold
  )
}, warning = function(w) {
  message("Warning caught: ", w$message)
})