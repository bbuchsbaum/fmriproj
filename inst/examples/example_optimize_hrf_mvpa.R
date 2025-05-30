## Example 1: Basic HRF optimization for classification
# Setup: Simulate fMRI data with known HRF shape
set.seed(101)
n_time <- 100
n_voxels <- 50
n_trials <- 8

# True HRF peaks at 5 seconds
true_peak <- 5
event_model <- list(
  onsets = seq(10, 80, by = 10),
  n_time = n_time
)

# Generate data with true HRF
true_hrf <- dnorm(0:10, mean = true_peak, sd = 1.5)
true_hrf <- true_hrf / max(true_hrf)

# Create Y with embedded signal
Y <- matrix(rnorm(n_time * n_voxels), nrow = n_time)
# Add class-specific signals
class_labels <- rep(c(0, 1), each = n_trials/2)
for (i in 1:n_trials) {
  onset <- round(event_model$onsets[i])
  if (class_labels[i] == 1) {
    # Class 1: positive signal in voxels 1:25
    for (t in 1:length(true_hrf)) {
      if (onset + t - 1 <= n_time) {
        Y[onset + t - 1, 1:25] <- Y[onset + t - 1, 1:25] + 2 * true_hrf[t]
      }
    }
  } else {
    # Class 0: positive signal in voxels 26:50
    for (t in 1:length(true_hrf)) {
      if (onset + t - 1 <= n_time) {
        Y[onset + t - 1, 26:50] <- Y[onset + t - 1, 26:50] + 2 * true_hrf[t]
      }
    }
  }
}

# Define flexible HRF basis function
hrf_basis_spmg3_theta <- function(theta_params, time_vector) {
  # theta_params[1] = time to peak
  peak <- theta_params[1]
  hrf <- dnorm(time_vector, mean = peak, sd = 1.5)
  matrix(hrf / max(hrf), ncol = 1)
}

# Inner CV function: simple classification accuracy
inner_cv_fn <- function(A_sl, labels = class_labels) {
  # Leave-one-out cross-validation
  n <- nrow(A_sl)
  errors <- 0
  
  for (i in 1:n) {
    # Training data (all except i)
    A_train <- A_sl[-i, , drop = FALSE]
    y_train <- labels[-i]
    
    # Test point
    A_test <- A_sl[i, , drop = FALSE]
    y_test <- labels[i]
    
    # Simple centroid classifier
    mean_0 <- colMeans(A_train[y_train == 0, , drop = FALSE])
    mean_1 <- colMeans(A_train[y_train == 1, , drop = FALSE])
    
    # Predict based on closest centroid
    dist_0 <- sum((A_test - mean_0)^2)
    dist_1 <- sum((A_test - mean_1)^2)
    pred <- ifelse(dist_0 < dist_1, 0, 1)
    
    errors <- errors + (pred != y_test)
  }
  
  # Return loss (error rate)
  return(errors / n)
}

# Optimize HRF parameters
result <- optimize_hrf_mvpa(
  theta_init = c(3),  # Initial guess: peak at 3s
  Y = Y,
  event_model = event_model,
  inner_cv_fn = inner_cv_fn,
  hrf_basis_func = hrf_basis_spmg3_theta,
  lambda_global = 0.1,
  lambda_adaptive_method = "none",
  collapse_method = "rss",
  optim_method = "Brent",  # For 1D optimization
  lower = 1,  # Peak must be >= 1s
  upper = 10, # Peak must be <= 10s
  diagnostics = TRUE
)

cat("True peak:", true_peak, "seconds\n")
cat("Estimated peak:", result$theta_hat, "seconds\n")
cat("Final loss:", result$optim_details$value, "\n")

## Example 2: Multi-parameter HRF optimization
# More complex HRF with two parameters
hrf_two_param <- function(theta_params, time_vector) {
  # theta_params[1] = time to peak
  # theta_params[2] = dispersion (width)
  peak <- theta_params[1]
  width <- theta_params[2]
  
  # Double gamma HRF
  hrf_main <- dgamma(time_vector, shape = peak/width, scale = width)
  hrf_main <- hrf_main / max(hrf_main)
  
  # Derivative
  hrf_deriv <- c(0, diff(hrf_main))
  
  cbind(hrf_main, hrf_deriv)
}

# Optimize both peak and width
result_2d <- optimize_hrf_mvpa(
  theta_init = c(4, 1),  # Initial: peak=4s, width=1s
  Y = Y,
  event_model = event_model,
  inner_cv_fn = inner_cv_fn,
  hrf_basis_func = hrf_two_param,
  lambda_global = 0.1,
  lambda_adaptive_method = "EB",  # Use adaptive lambda
  collapse_method = "pc",  # Use PC collapse
  optim_method = "Nelder-Mead",
  diagnostics = TRUE
)

cat("\n2D optimization results:\n")
cat("Estimated peak:", result_2d$theta_hat[1], "seconds\n")
cat("Estimated width:", result_2d$theta_hat[2], "\n")

## Example 3: Optimization with supervised weight learning
# Classifier function for collapse_beta optimization
classifier_fn <- function(A_sl, labels) {
  # Simple least squares loss
  pred <- rowMeans(A_sl[, 1:25]) - rowMeans(A_sl[, 26:50])
  loss <- sum((pred - labels)^2)
  
  # Gradient
  grad <- matrix(0, nrow = length(labels), ncol = ncol(A_sl))
  for (i in 1:length(labels)) {
    grad[i, 1:25] <- 2 * (pred[i] - labels[i]) / 25
    grad[i, 26:50] <- -2 * (pred[i] - labels[i]) / 25
  }
  
  list(loss = loss, grad = grad)
}

# Optimize with supervised collapse
result_optim_w <- optimize_hrf_mvpa(
  theta_init = c(3),
  Y = Y,
  event_model = event_model,
  inner_cv_fn = inner_cv_fn,
  hrf_basis_func = hrf_basis_spmg3_theta,
  lambda_global = 0.1,
  lambda_adaptive_method = "none",
  collapse_method = "optim",  # Optimize collapse weights
  labels_for_w_optim = class_labels,
  classifier_for_w_optim = classifier_fn,
  optim_w_params = list(maxit = 5),
  optim_method = "Brent",
  lower = 1,
  upper = 10,
  diagnostics = TRUE
)

## Example 4: Examining optimization trajectory
# Plot the optimization path
if (!is.null(result$diagnostics$theta_trace)) {
  trace_df <- result$diagnostics$theta_trace
  plot(trace_df$iter, trace_df$theta_1, 
       type = "b", pch = 19,
       xlab = "Iteration", 
       ylab = "HRF Peak Time (s)",
       main = "HRF Parameter Optimization Path")
  abline(h = true_peak, col = "red", lty = 2)
  legend("topright", 
         legend = c("Optimization path", "True value"),
         col = c("black", "red"),
         lty = c(1, 2))
}

## Example 5: Using finite difference gradients
# Enable gradient computation
result_grad <- optimize_hrf_mvpa(
  theta_init = c(3),
  Y = Y,
  event_model = event_model,
  inner_cv_fn = inner_cv_fn,
  hrf_basis_func = hrf_basis_spmg3_theta,
  lambda_global = 0.1,
  use_fd_grad = TRUE,  # Use finite differences
  optim_method = "L-BFGS-B",  # Gradient-based method
  lower = 1,
  upper = 10,
  diagnostics = TRUE
)

cat("\nWith gradient-based optimization:\n")
cat("Estimated peak:", result_grad$theta_hat, "seconds\n")
cat("Number of iterations:", result_grad$optim_details$counts["function"], "\n")