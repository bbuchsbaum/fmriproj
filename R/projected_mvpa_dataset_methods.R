#' S3 Methods for projected_mvpa_dataset
#'
#' Methods to make ProjectedDataSource objects compatible with rMVPA's
#' mvpa_dataset interface.
#'
#' @name projected_mvpa_dataset_methods
#' @keywords internal
NULL

#' Get data from projected_mvpa_dataset
#'
#' Extract data from the wrapped ProjectedDataSource, optionally subsetting
#' to specific voxel indices.
#'
#' @param x A projected_mvpa_dataset object
#' @param indices Optional voxel indices to extract
#' @param ... Additional arguments (unused)
#' @return Matrix of projected data
#' @keywords internal
#' @export
get_data.projected_mvpa_dataset <- function(x, indices = NULL, ...) {
  # Access R6 method directly to avoid potential method dispatch loops
  if (inherits(x$data, "R6")) {
    if (is.null(indices)) {
      x$data$as.matrix()
    } else {
      x$data$as.matrix()[, indices, drop = FALSE]
    }
  } else {
    if (is.null(indices)) {
      as.matrix(x$data)
    } else {
      as.matrix(x$data[, indices, drop = FALSE])
    }
  }
}

#' Get mask from projected_mvpa_dataset
#'
#' ProjectedDataSource doesn't have a spatial mask, so this returns NULL.
#'
#' @param x A projected_mvpa_dataset object
#' @param ... Additional arguments (unused)
#' @return NULL (ProjectedDataSource has no mask)
#' @keywords internal
#' @export
get_mask.projected_mvpa_dataset <- function(x, ...) {
  x$mask
}

#' Get dimensions from projected_mvpa_dataset
#'
#' @param x A projected_mvpa_dataset object
#' @return Numeric vector c(nrow, ncol) from the wrapped ProjectedDataSource
#' @keywords internal
#' @export
dim.projected_mvpa_dataset <- function(x) {
  # Access R6 field directly to avoid potential recursion
  if (inherits(x$data, "R6")) {
    x$data$dim
  } else {
    base::dim(x$data)
  }
}

#' Print method for projected_mvpa_dataset
#'
#' @param x A projected_mvpa_dataset object
#' @param ... Additional arguments passed to print
#' @keywords internal
#' @export
print.projected_mvpa_dataset <- function(x, ...) {
  cat("Projected MVPA Dataset\n")
  dims <- if (inherits(x$data, "R6")) x$data$dim else base::dim(x$data)
  cat("  Dimensions:", paste(dims, collapse = " x "), "\n")
  cat("  Class: ProjectedDataSource (wrapped for rMVPA compatibility)\n")
  invisible(x)
}
