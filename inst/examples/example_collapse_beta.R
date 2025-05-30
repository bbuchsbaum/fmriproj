## Example 1: Root-sum-square (RSS) collapse
# Setup: Generate example projected coefficients
set.seed(123)
n_trials <- 5
n_voxels <- 20
k_bases <- 3

# Simulate Z_sl_raw from projection step
Z_sl_raw <- matrix(rnorm(n_trials * k_bases * n_voxels), 
                   nrow = n_trials * k_bases,
                   ncol = n_voxels)

# Apply RSS collapse (default method)
result_rss <- collapse_beta(
  Z_sl_raw = Z_sl_raw,
  N_trials = n_trials,
  K_hrf_bases = k_bases,
  method = "rss",
  diagnostics = TRUE
)

# Check output dimensions
dim(result_rss$A_sl)  # Should be n_trials x n_voxels

# RSS weights are implicit - each voxel/trial gets dynamic weights
print(result_rss$w_sl)  # Should show equal weights as starting point

## Example 2: Principal component (PC) collapse
# PC method finds the optimal linear combination based on variance
result_pc <- collapse_beta(
  Z_sl_raw = Z_sl_raw,
  N_trials = n_trials,
  K_hrf_bases = k_bases,
  method = "pc",
  diagnostics = TRUE
)

# PC weights represent the first principal component
print("PC weights:")
print(result_pc$w_sl)
sum(result_pc$w_sl^2)  # Should be 1 (normalized)

## Example 3: Supervised optimization of weights
# Create simple labels for supervised learning
labels <- c(1, 0, 1, 0, 1)  # Binary classification

# Define a simple classifier function (squared error loss)
simple_classifier <- function(A_sl, labels) {
  # Use first voxel as simple predictor
  pred <- A_sl[, 1]
  loss <- sum((pred - labels)^2)
  
  # Gradient w.r.t. predictions
  grad_pred <- 2 * (pred - labels)
  
  # Gradient needs to be n_trials x n_voxels
  grad <- matrix(0, nrow = length(labels), ncol = ncol(A_sl))
  grad[, 1] <- grad_pred
  
  list(loss = loss, grad = grad)
}

# Optimize weights using the classifier
result_optim <- collapse_beta(
  Z_sl_raw = Z_sl_raw,
  N_trials = n_trials,
  K_hrf_bases = k_bases,
  method = "optim",
  labels_for_w_optim = labels,
  classifier_for_w_optim = simple_classifier,
  optim_w_params = list(maxit = 10, method = "L-BFGS-B"),
  diagnostics = TRUE
)

print("Optimized weights:")
print(result_optim$w_sl)

## Example 4: Comparing collapse methods
# Function to compare signal preservation
compare_methods <- function(Z_sl_raw, n_trials, k_bases) {
  methods <- c("rss", "pc")
  results <- list()
  
  for (method in methods) {
    res <- collapse_beta(
      Z_sl_raw = Z_sl_raw,
      N_trials = n_trials,
      K_hrf_bases = k_bases,
      method = method
    )
    
    # Compute variance explained
    var_collapsed <- var(as.vector(res$A_sl))
    var_original <- var(as.vector(Z_sl_raw))
    
    results[[method]] <- list(
      var_ratio = var_collapsed / var_original,
      mean_amplitude = mean(abs(res$A_sl))
    )
  }
  
  return(results)
}

comparison <- compare_methods(Z_sl_raw, n_trials, k_bases)
print("Method comparison:")
print(comparison)

## Example 5: Handling edge cases
# Very few observations
Z_small <- matrix(rnorm(1 * 2 * 1), nrow = 2, ncol = 1)
result_small <- collapse_beta(
  Z_sl_raw = Z_small,
  N_trials = 1,
  K_hrf_bases = 2,
  method = "pc",  # PC method needs special handling for small data
  diagnostics = TRUE
)

# Large number of basis functions
k_large <- 10
Z_large_k <- matrix(rnorm(n_trials * k_large * n_voxels),
                    nrow = n_trials * k_large,
                    ncol = n_voxels)

result_large_k <- collapse_beta(
  Z_sl_raw = Z_large_k,
  N_trials = n_trials,
  K_hrf_bases = k_large,
  method = "rss"
)

cat("Collapsed", k_large, "basis functions to", 
    nrow(result_large_k$A_sl), "trial patterns\n")