#' S3 methods for projecting_dataset class
#'
#' These methods ensure that projecting_dataset objects work seamlessly
#' with rMVPA's data access patterns.
#'
#' @name projecting_dataset_methods
NULL

# Define generics if not already defined by rMVPA
if (!exists("get_data", mode = "function")) {
  get_data <- function(x, ...) UseMethod("get_data")
}

if (!exists("get_samples", mode = "function")) {
  get_samples <- function(x, ...) UseMethod("get_samples")
}

if (!exists("data_sample", mode = "function")) {
  data_sample <- function(x, ...) UseMethod("data_sample")
}

if (!exists("get_mask", mode = "function")) {
  get_mask <- function(x, ...) UseMethod("get_mask")
}

#' Extract data from a projecting_dataset
#'
#' @param x A projecting_dataset object
#' @param indices Optional indices to extract specific voxels
#' @param ... Additional arguments (ignored)
#' @return Projected data matrix (trials x voxels)
#' @export
get_data.projecting_dataset <- function(x, indices = NULL, ...) {
  x$get_data(indices)
}

#' Extract data samples from a projecting_dataset
#'
#' This method is called by rMVPA when extracting multiple ROI samples.
#' Each element of the indices list corresponds to voxel indices for one ROI.
#'
#' @param x A projecting_dataset object
#' @param indices List of index vectors, one per ROI
#' @param ... Additional arguments (ignored)
#' @return Tibble with a `sample` column of data_sample objects and an `.id` column
#' @export
get_samples.projecting_dataset <- function(x, indices, ...) {
  # Build data_sample objects so rMVPA can call extract_roi/as_roi
  samples <- lapply(indices, function(vox) {
    structure(list(data = x, vox = vox), class = "data_sample")
  })
  n <- length(samples)
  width <- max(2, nchar(n))
  ids <- sprintf(paste0("%0", width, "d"), seq_len(n))
  tibble::tibble(sample = samples, .id = ids)
}

#' Extract a single data sample from a projecting_dataset
#'
#' @param x A projecting_dataset object
#' @param indices Voxel indices for the sample
#' @param ... Additional arguments (ignored)
#' @return A `data_sample` object referencing `x` and the voxel spec
#' @export
data_sample.projecting_dataset <- function(x, indices, ...) {
  structure(list(data = x, vox = indices), class = "data_sample")
}

#' Get mask from a projecting_dataset
#'
#' @param x A projecting_dataset object
#' @param ... Additional arguments (ignored)
#' @return The mask from the original dataset
#' @export
get_mask.projecting_dataset <- function(x, ...) {
  if (!is.null(x$original) && inherits(x$original, "mvpa_dataset")) {
    x$original$mask
  } else {
    NULL
  }
}

#' Get number of observations from a projecting_dataset
#'
#' @param x A projecting_dataset object
#' @param ... Additional arguments (ignored)
#' @return Number of trials after projection
#' @export
nobs.projecting_dataset <- function(x, ...) {
  # Get a small sample to determine number of trials
  test_data <- x$get_data(1)
  nrow(test_data)
}

#' Print method for projecting_dataset
#'
#' @param x A projecting_dataset object
#' @param ... Additional arguments (ignored)
#' @export
print.projecting_dataset <- function(x, ...) {
  cat("Projecting dataset (fmriproj)\n")
  cat("  Time-series dimensions:", paste(dim(x$Y), collapse = " x "), "\n")
  if (!is.null(x$original)) {
    cat("  Original dataset class:", class(x$original)[1], "\n")
  }
  invisible(x)
}

#' Subset a projecting_dataset
#'
#' This method allows rMVPA to subset the dataset spatially.
#'
#' @param x A projecting_dataset object
#' @param i Row indices (not used for projecting datasets)
#' @param j Column (voxel) indices
#' @param ... Additional arguments
#' @param drop Should dimensions be dropped (default FALSE)
#' @return A new projecting_dataset with subsetted voxels
#' @export
`[.projecting_dataset` <- function(x, i, j, ..., drop = FALSE) {
  if (!missing(i)) {
    warning("Row subsetting ignored for projecting_dataset (trials determined by projection)")
  }
  
  if (!missing(j)) {
    # Create a new projecting dataset that only uses specified voxels
    Y_subset <- x$Y[, j, drop = FALSE]
    
    # Create a modified projection function that handles the subset
    proj_fun_subset <- function(Y_sl) {
      x$projection_fun(Y_sl)
    }
    
    structure(
      list(
        Y = Y_subset,
        projection_fun = proj_fun_subset,
        original = x$original,
        get_data = function(indices = NULL) {
          if (is.null(indices)) {
            x$projection_fun(Y_subset)
          } else {
            x$projection_fun(Y_subset[, indices, drop = FALSE])
          }
        }
      ),
      class = class(x)
    )
  } else {
    x
  }
}

#' Check if object is a projecting_dataset
#'
#' @param x Object to test
#' @return Logical indicating if x is a projecting_dataset
#' @export
is.projecting_dataset <- function(x) {
  inherits(x, "projecting_dataset")
}

#' Get searchlight for projecting_dataset
#'
#' Ensure the order of searchlight windows aligns with which(mask > 0)
#' so that rMVPA's center ids correspond to the ROIs returned here.
#'
#' @param obj A projecting_dataset
#' @param type "standard" or "randomized" (only "standard" implemented)
#' @param radius Searchlight radius
#' @param ... Ignored
#' @export
get_searchlight.projecting_dataset <- function(obj, type = c("standard", "randomized"), radius = 8, ...) {
  type <- match.arg(type)
  if (type != "standard") {
    # Fallback to default for randomized; tests use standard
    return(rMVPA::get_searchlight(obj$original, type = type, radius = radius, ...))
  }
  mask <- obj$mask
  if (!inherits(mask, "NeuroVol")) {
    stop("projecting_dataset mask must be a NeuroVol for volumetric searchlight")
  }
  ids <- which(mask[] > 0)
  f <- function(k) {
    ctr <- neuroim2::index_to_grid(mask, ids[k])
    neuroim2::spherical_roi(mask, ctr, radius = radius, nonzero = TRUE)
  }
  deflist::deflist(f, length(ids))
}
