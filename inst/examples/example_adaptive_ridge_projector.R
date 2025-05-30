## Example 1: Basic adaptive projection with no adaptation
# Setup: Create design matrix and projector
set.seed(42)
n_time <- 100
n_voxels <- 50

event_model <- list(
  onsets = c(10, 30, 50, 70, 90),
  n_time = n_time
)

hrf_basis <- matrix(
  c(0, 0.5, 1, 0.5, 0,
    0, 0.1, 0.2, 0.1, 0),
  nrow = 5, ncol = 2
)

design <- build_design_matrix(
  event_model = event_model,
  hrf_basis_matrix = hrf_basis
)

proj_comp <- build_projector(
  X_theta = design$X,
  lambda_global = 0.1
)

# Simulate searchlight BOLD data
Y_searchlight <- matrix(rnorm(n_time * n_voxels), nrow = n_time)

# Apply adaptive projection with no adaptation (uses floor value)
result_none <- adaptive_ridge_projector(
  Y_sl = Y_searchlight,
  projector_components = proj_comp,
  lambda_adaptive_method = "none",
  lambda_floor_global = 0.1,
  diagnostics = TRUE
)

# Check dimensions
dim(result_none$Z_sl_raw)  # Should be (n_trials * K_bases) x n_voxels

## Example 2: Empirical Bayes adaptive lambda
# Need the full design matrix for EB residuals
X_dense <- as.matrix(design$X)

result_eb <- adaptive_ridge_projector(
  Y_sl = Y_searchlight,
  projector_components = proj_comp,
  lambda_adaptive_method = "EB",
  lambda_floor_global = 0.1,
  X_theta_for_EB_residuals = X_dense,
  diagnostics = TRUE
)

# Compare lambdas
cat("Floor lambda:", 0.1, "\n")
cat("EB-selected lambda:", result_eb$diag_data$lambda_sl_chosen, "\n")

## Example 3: Local cross-validation for lambda selection
# Define custom folds for CV
n_folds <- 4
folds <- rep(1:n_folds, length.out = n_time)

result_cv <- adaptive_ridge_projector(
  Y_sl = Y_searchlight,
  projector_components = proj_comp,
  lambda_adaptive_method = "LOOcv_local",
  lambda_floor_global = 0.01,
  X_theta_for_EB_residuals = X_dense,
  lambda_grid_local = c(0, 0.01, 0.1, 1, 10),  # Grid of lambdas to test
  folds_local_cv = folds,
  diagnostics = TRUE
)

cat("CV-selected lambda:", result_cv$diag_data$lambda_sl_chosen, "\n")

## Example 4: Handling missing data in searchlight
# Create searchlight with some missing voxels
Y_missing <- Y_searchlight
Y_missing[, 10:15] <- NA

result_missing <- adaptive_ridge_projector(
  Y_sl = Y_missing,
  projector_components = proj_comp,
  lambda_adaptive_method = "none",
  lambda_floor_global = 0.1
)

# Should return NULL due to missing data
is.null(result_missing$Z_sl_raw)

## Example 5: Comparing different adaptation methods
# Function to apply all methods
compare_methods <- function(Y_sl, proj_comp, X_dense) {
  methods <- c("none", "EB", "LOOcv_local")
  lambdas <- numeric(length(methods))
  
  for (i in seq_along(methods)) {
    result <- adaptive_ridge_projector(
      Y_sl = Y_sl,
      projector_components = proj_comp,
      lambda_adaptive_method = methods[i],
      lambda_floor_global = 0.01,
      X_theta_for_EB_residuals = X_dense,
      diagnostics = TRUE
    )
    lambdas[i] <- result$diag_data$lambda_sl_chosen
  }
  
  data.frame(method = methods, lambda = lambdas)
}

# Compare on different noise levels
Y_low_noise <- Y_searchlight * 0.1
Y_high_noise <- Y_searchlight * 10

comparison_low <- compare_methods(Y_low_noise, proj_comp, X_dense)
comparison_high <- compare_methods(Y_high_noise, proj_comp, X_dense)

print("Low noise searchlight:")
print(comparison_low)
print("\nHigh noise searchlight:")
print(comparison_high)