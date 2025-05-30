## Example 1: Basic SPMG3 HRF basis generation
# Generate standard SPMG3 basis at 1-second resolution
time_points <- seq(0, 30, by = 0.5)  # 0 to 30 seconds
hrf_basis <- hrf_basis_spmg3_theta(theta = c(1, 1), t = time_points)

# Visualize the three basis functions
matplot(time_points, hrf_basis, type = "l", lwd = 2,
        xlab = "Time (seconds)", 
        ylab = "Amplitude",
        main = "SPMG3 HRF Basis Functions",
        col = c("black", "red", "blue"))
legend("topright", 
       legend = c("Canonical HRF", "Temporal Derivative", "Dispersion Derivative"),
       col = c("black", "red", "blue"),
       lty = 1, lwd = 2)

## Example 2: Varying the delay parameter
# Compare different delay scalings
delays <- c(0.5, 1.0, 1.5, 2.0)
par(mfrow = c(2, 2))

for (i in seq_along(delays)) {
  hrf <- hrf_basis_spmg3_theta(theta = c(delays[i], 1), t = time_points)
  plot(time_points, hrf[, 1], type = "l", lwd = 2,
       xlab = "Time (seconds)", 
       ylab = "Amplitude",
       main = paste("Delay scale =", delays[i]),
       ylim = c(-0.2, 1))
  grid()
}
par(mfrow = c(1, 1))

## Example 3: Varying the dispersion parameter
# Compare different dispersion scalings
dispersions <- c(0.5, 1.0, 1.5, 2.0)
colors <- rainbow(length(dispersions))

plot(NA, xlim = c(0, 30), ylim = c(-0.2, 1),
     xlab = "Time (seconds)", 
     ylab = "Amplitude",
     main = "Effect of Dispersion Scaling on Canonical HRF")

for (i in seq_along(dispersions)) {
  hrf <- hrf_basis_spmg3_theta(theta = c(1, dispersions[i]), t = time_points)
  lines(time_points, hrf[, 1], col = colors[i], lwd = 2)
}

legend("topright", 
       legend = paste("Dispersion =", dispersions),
       col = colors, lty = 1, lwd = 2)

## Example 4: Using single theta value
# When only one theta is provided, it's used for both delay and dispersion
hrf_single <- hrf_basis_spmg3_theta(theta = 1.5, t = time_points)

# Compare with explicit specification
hrf_double <- hrf_basis_spmg3_theta(theta = c(1.5, 1.5), t = time_points)

# Should be identical
all.equal(hrf_single, hrf_double)

## Example 5: Integration with optimization
# This shows how theta parameters affect HRF shape for optimization
# Define objective function that measures HRF peak location
find_peak_time <- function(theta) {
  t <- seq(0, 20, by = 0.1)
  hrf <- hrf_basis_spmg3_theta(theta = theta, t = t)
  canonical <- hrf[, 1]
  peak_idx <- which.max(canonical)
  return(t[peak_idx])
}

# Test different theta values
theta_test <- seq(0.5, 2, by = 0.1)
peak_times <- sapply(theta_test, find_peak_time)

plot(theta_test, peak_times, type = "l", lwd = 2,
     xlab = "Theta (delay scale)", 
     ylab = "Peak time (seconds)",
     main = "HRF Peak Time vs Delay Parameter")
grid()

## Example 6: Irregular time points
# The function handles irregularly spaced time points
irregular_t <- c(0, 0.5, 1, 2, 3, 5, 8, 13, 21)
hrf_irregular <- hrf_basis_spmg3_theta(theta = c(1, 1), t = irregular_t)

plot(irregular_t, hrf_irregular[, 1], type = "b", pch = 19,
     xlab = "Time (seconds)", 
     ylab = "Amplitude",
     main = "HRF at Irregular Time Points")

# Add smooth version for comparison
lines(time_points, hrf_basis[, 1], col = "red", lty = 2)
legend("topright", 
       legend = c("Irregular sampling", "Regular sampling"),
       col = c("black", "red"),
       lty = c(1, 2))