#' Create a memory-efficient chunked projecting dataset
#'
#' This wrapper performs projection in chunks to handle very large datasets
#' that cannot fit in memory when fully projected.
#'
#' @param Y Time-series data or data access function
#' @param projection_fun Projection function from fmriproj
#' @param original_dataset Original rMVPA dataset
#' @param chunk_size Number of voxels to process at once
#' @param cache_results Logical; whether to cache projection results
#' @return A chunked_projecting_dataset object
#' @export
wrap_as_chunked_projecting_dataset <- function(Y, 
                                              projection_fun, 
                                              original_dataset,
                                              chunk_size = 1000,
                                              cache_results = TRUE) {
  
  # Determine data dimensions
  if (is.function(Y)) {
    # Y is a data accessor function (for file-based datasets)
    test_chunk <- Y(indices = 1:min(10, original_dataset$nfeatures))
    n_timepoints <- nrow(test_chunk)
    n_voxels <- original_dataset$nfeatures
    data_accessor <- Y
  } else if (is.matrix(Y) || inherits(Y, "Matrix")) {
    n_timepoints <- nrow(Y)
    n_voxels <- ncol(Y)
    data_accessor <- function(indices = NULL) {
      if (is.null(indices)) Y else Y[, indices, drop = FALSE]
    }
  } else {
    stop("Y must be a matrix or data accessor function")
  }
  
  # Test projection to get expected output size
  test_data <- data_accessor(indices = 1:min(10, n_voxels))
  test_proj <- projection_fun(test_data)
  expected_trials <- nrow(test_proj)
  
  # Initialize cache if requested
  cache_env <- if (cache_results) {
    new.env(parent = emptyenv())
  } else {
    NULL
  }
  
  structure(
    list(
      data_accessor = data_accessor,
      projection_fun = projection_fun,
      original = original_dataset,
      expected_trials = expected_trials,
      n_voxels = n_voxels,
      chunk_size = chunk_size,
      cache_env = cache_env,
      
      get_data = function(indices = NULL) {
        if (is.null(indices)) {
          # Full brain projection - process in chunks
          chunks <- split(1:n_voxels, 
                         ceiling(seq_along(1:n_voxels) / chunk_size))
          
          # Pre-allocate result matrix
          result <- matrix(NA, nrow = expected_trials, ncol = n_voxels)
          
          # Process each chunk
          for (i in seq_along(chunks)) {
            chunk_indices <- chunks[[i]]
            
            # Check cache first
            cache_key <- paste0("chunk_", i)
            if (!is.null(cache_env) && exists(cache_key, cache_env)) {
              result[, chunk_indices] <- cache_env[[cache_key]]
            } else {
              # Get chunk data and project
              chunk_data <- data_accessor(indices = chunk_indices)
              chunk_result <- projection_fun(chunk_data)
              
              # Validate chunk result
              if (nrow(chunk_result) != expected_trials) {
                stop("Chunk projection returned wrong number of trials")
              }
              
              # Store in result and cache
              result[, chunk_indices] <- chunk_result
              
              if (!is.null(cache_env)) {
                cache_env[[cache_key]] <- chunk_result
              }
            }
          }
          
          result
          
        } else {
          # Subset projection - check if we can use cached chunks
          if (!is.null(cache_env) && length(indices) > chunk_size / 2) {
            # For large subsets, try to use cached chunks
            result <- matrix(NA, nrow = expected_trials, ncol = length(indices))
            
            # Group indices by chunk
            chunk_assignments <- ceiling(indices / chunk_size)
            unique_chunks <- unique(chunk_assignments)
            
            for (chunk_id in unique_chunks) {
              # Get indices belonging to this chunk
              mask <- chunk_assignments == chunk_id
              subset_indices <- indices[mask]
              within_chunk_indices <- subset_indices - (chunk_id - 1) * chunk_size
              
              cache_key <- paste0("chunk_", chunk_id)
              if (exists(cache_key, cache_env)) {
                # Use cached chunk
                result[, mask] <- cache_env[[cache_key]][, within_chunk_indices, drop = FALSE]
              } else {
                # Need to compute this chunk
                chunk_start <- (chunk_id - 1) * chunk_size + 1
                chunk_end <- min(chunk_id * chunk_size, n_voxels)
                chunk_indices <- chunk_start:chunk_end
                
                chunk_data <- data_accessor(indices = chunk_indices)
                chunk_result <- projection_fun(chunk_data)
                
                # Cache the full chunk
                if (!is.null(cache_env)) {
                  cache_env[[cache_key]] <- chunk_result
                }
                
                # Extract requested subset
                result[, mask] <- chunk_result[, within_chunk_indices, drop = FALSE]
              }
            }
            
            result
            
          } else {
            # For small subsets, just project directly
            subset_data <- data_accessor(indices = indices)
            projection_fun(subset_data)
          }
        }
      },
      
      clear_cache = function() {
        if (!is.null(cache_env)) {
          rm(list = ls(cache_env), envir = cache_env)
        }
      }
    ),
    class = c("chunked_projecting_dataset", "projecting_dataset", class(original_dataset))
  )
}

#' Configure memory-efficient projection
#'
#' Helper function to determine optimal chunk size based on available memory
#'
#' @param n_timepoints Number of time points in the data
#' @param n_voxels Total number of voxels
#' @param n_trials Expected number of trials after projection
#' @param memory_limit_gb Maximum memory to use in gigabytes
#' @return Recommended chunk size
#' @export
recommend_chunk_size <- function(n_timepoints, n_voxels, n_trials, 
                                memory_limit_gb = 2) {
  # Estimate memory usage
  # Input: n_timepoints x chunk_size (double precision = 8 bytes)
  # Output: n_trials x chunk_size
  bytes_per_voxel <- 8 * (n_timepoints + n_trials)
  
  # Add overhead for projection computation (rough estimate: 2x)
  bytes_per_voxel <- bytes_per_voxel * 2
  
  # Convert memory limit to bytes
  memory_limit_bytes <- memory_limit_gb * 1024^3
  
  # Calculate chunk size
  chunk_size <- floor(memory_limit_bytes / bytes_per_voxel)
  
  # Apply reasonable bounds
  chunk_size <- max(100, min(chunk_size, n_voxels))
  
  message(sprintf(
    "Recommended chunk size: %d voxels (%.1f MB per chunk)",
    chunk_size,
    (chunk_size * bytes_per_voxel) / 1024^2
  ))
  
  chunk_size
}

#' S3 methods for chunked_projecting_dataset
#'
#' @rdname chunked_projecting_dataset_methods
#' @export
print.chunked_projecting_dataset <- function(x, ...) {
  cat("Chunked Projecting Dataset (fmriproj)\n")
  cat("  Total voxels:", x$n_voxels, "\n")
  cat("  Chunk size:", x$chunk_size, "\n")
  cat("  Cache enabled:", !is.null(x$cache_env), "\n")
  if (!is.null(x$cache_env)) {
    n_cached <- length(ls(x$cache_env))
    n_total_chunks <- ceiling(x$n_voxels / x$chunk_size)
    cat("  Cached chunks:", n_cached, "/", n_total_chunks, "\n")
  }
  invisible(x)
}