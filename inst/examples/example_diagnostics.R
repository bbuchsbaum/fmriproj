## Example: Diagnostic functions (combine_projection_diagnostics & explain_projection_results)

# These functions are primarily used with rMVPA integration, but here's a 
# standalone example showing how they work

# Simulate searchlight results with diagnostics
set.seed(202)

# Create mock searchlight results as would be returned by the searchlight function
n_searchlights <- 100
mock_sl_results <- list()

for (i in 1:n_searchlights) {
  # Each searchlight returns diagnostics
  mock_result <- list(
    accuracy = runif(1, 0.4, 0.8),  # Random accuracy
    diag_data = list(
      lambda_sl = runif(1, 0.01, 1),  # Adaptive lambda chosen
      w_sl = rnorm(3)  # Collapse weights for 3 basis functions
    )
  )
  
  # Use combine_projection_diagnostics to accumulate results
  if (i == 1) {
    mock_sl_results <- combine_projection_diagnostics(NULL, mock_result)
  } else {
    mock_sl_results <- combine_projection_diagnostics(mock_sl_results, mock_result)
  }
}

# Check structure of combined results
str(mock_sl_results, max.level = 2)
cat("Number of searchlights processed:", length(mock_sl_results$results), "\n")
cat("Number of diagnostic entries:", length(mock_sl_results$diagnostics), "\n")

## Example 2: Extract diagnostic maps with explain_projection_results
# Define brain mask dimensions (e.g., 10x10 slice)
mask_dims <- c(10, 10)  # 100 searchlights in 10x10 grid

# Define HRF basis used in analysis
hrf_basis <- matrix(
  c(0, 0.5, 1, 0.5, 0,      # Main HRF
    0, 0.1, 0.2, 0.1, 0,    # Time derivative
    0, 0.05, 0.1, 0.05, 0), # Dispersion derivative
  nrow = 5, ncol = 3
)

# Extract diagnostic maps
diag_maps <- explain_projection_results(
  sl_results = mock_sl_results,
  mask_dims = mask_dims,
  hrf_basis_matrix = hrf_basis
)

# Visualize lambda map
cat("\nLambda map dimensions:", dim(diag_maps$lambda_map), "\n")
cat("Lambda range:", range(diag_maps$lambda_map), "\n")

# Simple visualization of lambda map
image(diag_maps$lambda_map, 
      main = "Searchlight Lambda Values",
      xlab = "X coordinate", 
      ylab = "Y coordinate",
      col = heat.colors(20))

## Example 3: Examine collapse weights across searchlights
if (!is.null(diag_maps$w_maps)) {
  # w_maps has dimensions [mask_dims, n_basis_functions]
  cat("\nCollapse weight maps dimensions:", dim(diag_maps$w_maps), "\n")
  
  # Plot weight for first basis function
  image(diag_maps$w_maps[,,1], 
        main = "Weight for HRF Basis 1 (Amplitude)",
        xlab = "X coordinate", 
        ylab = "Y coordinate",
        col = heat.colors(20))
}

# Examine effective HRF
if (!is.null(diag_maps$effective_hrf)) {
  cat("\nEffective HRF (average across searchlights):\n")
  plot(diag_maps$effective_hrf, type = "l", lwd = 2,
       xlab = "Time points", 
       ylab = "Amplitude",
       main = "Average Effective HRF")
  
  # Show how it relates to the basis
  matplot(hrf_basis, type = "l", lty = 2, col = 1:3,
          xlab = "Time points", 
          ylab = "Amplitude",
          main = "HRF Basis Functions")
  lines(diag_maps$effective_hrf, lwd = 3, col = "red")
  legend("topright", 
         legend = c("Basis 1", "Basis 2", "Basis 3", "Effective"),
         col = c(1:3, "red"),
         lty = c(2, 2, 2, 1),
         lwd = c(1, 1, 1, 3))
}

## Example 4: Using diagnostics with memory limits
# The cap_diagnostics function (used internally) respects memory limits
old_opt <- options(fmriproj.diagnostics_memory_limit = 0.001)  # 1KB limit

# This will trigger memory capping
large_diag <- list(
  lambda_sl = 0.5,
  w_sl = rnorm(100),
  beta_components_sl = matrix(rnorm(10000), 100, 100)  # Large matrix
)

mock_large_result <- list(
  accuracy = 0.7,
  diag_data = large_diag
)

# Combine with memory limit
capped_results <- combine_projection_diagnostics(NULL, mock_large_result)

# The large diagnostic data should be NULL due to memory limit
is.null(capped_results$diagnostics[[1]])

# Restore options
options(old_opt)

## Example 5: Custom diagnostic extraction
# Extract specific diagnostics for further analysis
extract_lambda_stats <- function(sl_results) {
  if (is.null(sl_results$diagnostics)) {
    return(NULL)
  }
  
  lambdas <- sapply(sl_results$diagnostics, function(d) d$lambda_sl)
  
  list(
    mean_lambda = mean(lambdas, na.rm = TRUE),
    sd_lambda = sd(lambdas, na.rm = TRUE),
    median_lambda = median(lambdas, na.rm = TRUE),
    range_lambda = range(lambdas, na.rm = TRUE),
    histogram = hist(lambdas, plot = FALSE)
  )
}

lambda_stats <- extract_lambda_stats(mock_sl_results)
cat("\nLambda statistics across searchlights:\n")
cat("Mean:", lambda_stats$mean_lambda, "\n")
cat("SD:", lambda_stats$sd_lambda, "\n")
cat("Median:", lambda_stats$median_lambda, "\n")
cat("Range:", lambda_stats$range_lambda, "\n")

# Plot distribution
hist(sapply(mock_sl_results$diagnostics, function(d) d$lambda_sl),
     main = "Distribution of Adaptive Lambda Values",
     xlab = "Lambda",
     col = "lightblue")