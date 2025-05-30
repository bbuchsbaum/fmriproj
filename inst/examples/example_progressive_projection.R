## Example 1: Basic LDA projection
# Generate example trial patterns (after collapse_beta step)
set.seed(456)
n_trials <- 30
n_voxels <- 100

# Simulate two classes with different patterns
class1_trials <- 15
class2_trials <- 15

# Class 1: Higher activation in first 50 voxels
A_class1 <- matrix(rnorm(class1_trials * n_voxels, mean = c(rep(1, 50), rep(0, 50))), 
                   nrow = class1_trials)

# Class 2: Higher activation in last 50 voxels  
A_class2 <- matrix(rnorm(class2_trials * n_voxels, mean = c(rep(0, 50), rep(1, 50))),
                   nrow = class2_trials)

# Combine into training data
A_sl_train <- rbind(A_class1, A_class2)
labels_train <- c(rep(1, class1_trials), rep(2, class2_trials))

# Fit LDA projection to 2 dimensions
pp_lda <- fit_pp(
  A_sl_train = A_sl_train,
  labels_train = labels_train,
  method = "LDA",
  dims = 2
)

# Apply projection to training data
A_projected <- predict_pp(pp_lda, A_sl_train)

# Visualize the projection
plot(A_projected[,1], A_projected[,2], 
     col = labels_train, pch = 19,
     xlab = "LDA Dimension 1", 
     ylab = "LDA Dimension 2",
     main = "Trial patterns in LDA space")
legend("topright", legend = c("Class 1", "Class 2"), 
       col = 1:2, pch = 19)

## Example 2: PLS-DA projection
# Same data, but using PLS-DA
pp_pls <- fit_pp(
  A_sl_train = A_sl_train,
  labels_train = labels_train,
  method = "PLS-DA",
  dims = 2
)

A_pls_projected <- predict_pp(pp_pls, A_sl_train)

# Compare projections
par(mfrow = c(1, 2))
plot(A_projected[,1], A_projected[,2], 
     col = labels_train, pch = 19,
     main = "LDA Projection")
plot(A_pls_projected[,1], A_pls_projected[,2], 
     col = labels_train, pch = 19,
     main = "PLS-DA Projection")
par(mfrow = c(1, 1))

## Example 3: Handling new test data
# Generate new test trials
n_test <- 10
A_test_class1 <- matrix(rnorm(5 * n_voxels, mean = c(rep(1, 50), rep(0, 50))), 
                        nrow = 5)
A_test_class2 <- matrix(rnorm(5 * n_voxels, mean = c(rep(0, 50), rep(1, 50))),
                        nrow = 5)
A_sl_test <- rbind(A_test_class1, A_test_class2)
labels_test <- c(rep(1, 5), rep(2, 5))

# Project test data using fitted model
A_test_projected <- predict_pp(pp_lda, A_sl_test)

# Plot train and test together
plot(A_projected[,1], A_projected[,2], 
     col = labels_train, pch = 19,
     xlab = "LDA Dimension 1", 
     ylab = "LDA Dimension 2",
     main = "Train (filled) and Test (open) patterns")
points(A_test_projected[,1], A_test_projected[,2],
       col = labels_test, pch = 1, cex = 1.5)

## Example 4: Handling edge cases
# Single class (no discrimination possible)
labels_single <- rep(1, n_trials)
pp_single <- fit_pp(
  A_sl_train = A_sl_train,
  labels_train = labels_single,
  method = "LDA",
  dims = 2
)
# Should return identity projection
all(diag(pp_single$W) == 1)

# More dimensions requested than possible
pp_many_dims <- fit_pp(
  A_sl_train = A_sl_train[1:5, 1:3],  # Only 3 voxels
  labels_train = labels_train[1:5],
  method = "LDA", 
  dims = 10  # Request 10 dims but only 3 possible
)
ncol(pp_many_dims$W)  # Should be min(3, 10) = 3

## Example 5: Cross-validation with progressive projection
# Function to evaluate PP in cross-validation
cv_with_pp <- function(A_sl, labels, n_folds = 5) {
  n <- nrow(A_sl)
  fold_ids <- rep(1:n_folds, length.out = n)
  accuracies <- numeric(n_folds)
  
  for (fold in 1:n_folds) {
    # Split data
    train_idx <- fold_ids != fold
    test_idx <- !train_idx
    
    A_train <- A_sl[train_idx, ]
    A_test <- A_sl[test_idx, ]
    labels_train <- labels[train_idx]
    labels_test <- labels[test_idx]
    
    # Fit PP on training data
    pp_model <- fit_pp(A_train, labels_train, method = "LDA", dims = 2)
    
    # Project both sets
    A_train_proj <- predict_pp(pp_model, A_train)
    A_test_proj <- predict_pp(pp_model, A_test)
    
    # Simple nearest centroid classifier
    centroids <- aggregate(A_train_proj, by = list(labels_train), mean)
    centroids <- as.matrix(centroids[, -1])
    
    # Predict test labels
    pred_labels <- numeric(length(labels_test))
    for (i in 1:length(labels_test)) {
      dists <- apply(centroids, 1, function(c) {
        sum((A_test_proj[i, ] - c)^2)
      })
      pred_labels[i] <- which.min(dists)
    }
    
    accuracies[fold] <- mean(pred_labels == labels_test)
  }
  
  return(list(
    mean_accuracy = mean(accuracies),
    fold_accuracies = accuracies
  ))
}

# Run cross-validation
cv_results <- cv_with_pp(A_sl_train, labels_train)
cat("Cross-validation accuracy:", cv_results$mean_accuracy, "\n")
cat("Per-fold accuracies:", cv_results$fold_accuracies, "\n")