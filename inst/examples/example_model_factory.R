# Example: Using the elegant model factory architecture

library(fmriproj)

# Generate example data
n_time <- 200
n_voxels <- 100
n_trials <- 40

set.seed(123)
Y <- matrix(rnorm(n_time * n_voxels), n_time, n_voxels)

# Create event model
event_model <- list(
  onsets = seq(0, 180, length.out = n_trials),
  n_time = n_time,
  conditions = rep(c("face", "house"), each = n_trials/2),
  blocks = rep(seq_len(n_trials/5), each = 5)
)

# Method 1: Using model_spec and create_projected_model
# =====================================================

# First, project time-series to trial patterns
trial_data <- project_trials(Y, event_model, verbose = FALSE)

# Create a simple PCA projection matrix
pca <- prcomp(trial_data)
projection_matrix <- pca$rotation[, 1:10]  # Keep first 10 PCs

# Create model specification
spec <- model_spec(
  type = "classification",
  data = trial_data,
  labels = event_model$conditions,
  algorithm = "lda"
)

# Create projected model (elegant S3 dispatch, no switch!)
model <- create_projected_model(spec, projection_matrix)

# The model now transparently uses projected data
# rMVPA sees the ProjectedDataSource as regular data


# Method 2: Direct projection from time-series
# ===========================================

# Create projection specification for time-series → trials
proj_spec <- projection_spec(
  event_model = event_model,
  projector_components = build_projector(
    build_design_matrix(event_model)$X
  ),
  N_trials = n_trials,
  K_hrf = 3,
  lambda_adaptive_method = "EB",
  collapse_method = "rss"
)

# Model spec with time-series data
ts_spec <- model_spec(
  type = "rsa",
  data = Y,  # Time-series, not trials!
  distance_method = "correlation"
)

# Create model with time-series projection
rsa_model <- create_projected_model(ts_spec, proj_spec)


# Method 3: Different model types with same architecture
# =====================================================

# Feature RSA
feature_matrix <- matrix(rnorm(n_trials * 5), n_trials, 5)
feature_spec <- model_spec(
  type = "feature_rsa",
  data = trial_data,
  features = feature_matrix,
  labels = event_model$conditions,
  blocks = event_model$blocks
)
feature_model <- create_projected_model(feature_spec, projection_matrix)

# Vector RSA
vector_spec <- model_spec(
  type = "vector_rsa",
  data = trial_data,
  contrasts = list(
    face_vs_house = c(1, -1)
  )
)
vector_model <- create_projected_model(vector_spec, projection_matrix)


# Method 4: Function-based projection
# ==================================

# Custom projection function
my_projection <- function(data) {
  # Normalize then reduce dims
  normalized <- scale(data)
  svd_result <- svd(normalized)
  normalized %*% svd_result$v[, 1:5]
}

# Use with any model type
custom_spec <- model_spec(
  type = "classification",
  data = trial_data,
  labels = event_model$conditions
)
custom_model <- create_projected_model(custom_spec, my_projection)


# Method 5: Convenience wrapper
# ============================

# One-liner for common cases
quick_model <- create_mvpa_model_projected(
  "classification",
  dataset = trial_data,
  labels = event_model$conditions,
  projection = projection_matrix,
  algorithm = "sda_notune"
)


# The beauty of this architecture:
# 1. No giant switch statements
# 2. Easy to add new model types (just add S3 method)
# 3. Projection is transparent to rMVPA
# 4. Lazy evaluation saves memory
# 5. Works with any projection type (matrix, function, spec)
# 6. Clean separation of concerns
