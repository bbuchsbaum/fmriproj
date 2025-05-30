## Example 1: Basic projector construction
# First create a design matrix
event_model <- list(
  onsets = c(2, 8, 14),
  n_time = 20,
  amplitudes = c(1, 1, 1)
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

# Build projector without regularization
proj <- build_projector(
  X_theta = design$X,
  lambda_global = 0,
  diagnostics = TRUE
)

# Inspect components
str(proj)
print(proj)  # Uses print method

# Check condition number
attr(proj, "diagnostics")$cond_R

## Example 2: Ridge-regularized projector
# Build with regularization to handle collinearity
proj_ridge <- build_projector(
  X_theta = design$X,
  lambda_global = 0.1,  # Small ridge penalty
  diagnostics = TRUE
)

# The K_global component now includes regularization
dim(proj_ridge$K_global)

## Example 3: Handling ill-conditioned design matrices
# Create a design matrix with high collinearity
set.seed(123)
n_time <- 50
n_events <- 10

# Events very close together (causes collinearity)
close_onsets <- sort(runif(n_events, min = 0, max = 10))
event_model_ill <- list(
  onsets = close_onsets,
  n_time = n_time
)

design_ill <- build_design_matrix(
  event_model = event_model_ill,
  hrf_basis_matrix = hrf_basis
)

# This should warn about high condition number
proj_ill <- build_projector(
  X_theta = design_ill$X,
  lambda_global = 0,
  diagnostics = TRUE
)

# With regularization, condition improves
proj_ill_reg <- build_projector(
  X_theta = design_ill$X,
  lambda_global = 1.0,  # Stronger regularization
  diagnostics = TRUE
)

## Example 4: Using pivot for fill-reduction
# For sparse matrices, pivoting can improve efficiency
proj_pivot <- build_projector(
  X_theta = design$X,
  lambda_global = 0,
  pivot = TRUE,  # Use column pivoting
  diagnostics = TRUE
)

# Compare computation time with/without pivot
proj_no_pivot <- build_projector(
  X_theta = design$X,
  lambda_global = 0,
  pivot = FALSE,
  diagnostics = TRUE
)

cat("With pivot:", attr(proj_pivot, "diagnostics")$time_to_build, "seconds\n")
cat("Without pivot:", attr(proj_no_pivot, "diagnostics")$time_to_build, "seconds\n")