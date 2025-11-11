#' Projected Data Source R6 Class
#'
#' A proxy object that wraps original data and projection matrix, performing
#' projection on-the-fly when data is accessed. This provides transparent
#' integration with rMVPA models.
#'
#' Note on function-based projections:
#' - If `projection` is a function with signature `function(data, j = NULL)` or
#'   alternatives `function(data, cols = NULL)` or `function(data, indices = NULL)`,
#'   column-only subsetting (`x[, j]`) will call the function with the requested
#'   column indices to avoid computing unused projected columns. If the function
#'   only accepts a single `data` argument, full projection is computed and then
#'   subset.
#' Infer projection output characteristics without executing projection
#'
#' Attempts to determine the projected dimensionality (and optional column names)
#' for function-based projections without invoking the potentially expensive
#' projection itself.
#'
#' @keywords internal
infer_projection_output <- function(projection, data, metadata) {
  # 1) Feature names in metadata provide authoritative length
  if (!is.null(metadata) && !is.null(metadata$feature_names)) {
    return(list(
      ncol = length(metadata$feature_names),
      colnames = metadata$feature_names
    ))
  }
  
  # 2) Explicit metadata attributes (e.g., projection_dim) if provided
  meta_dim <- NULL
  if (!is.null(metadata) && !is.null(metadata$attributes)) {
    meta_dim <- metadata$attributes$projection_dim %||%
      metadata$attributes$output_dim
    meta_names <- metadata$attributes$projection_feature_names
  } else {
    meta_names <- NULL
  }
  if (!is.null(meta_dim)) {
    return(list(
      ncol = meta_dim,
      colnames = meta_names
    ))
  }
  
  # 3) Attributes on the projection function itself
  attr_dim <- attr(projection, "projection_dim", exact = TRUE) %||%
    attr(projection, "output_dim", exact = TRUE)
  attr_names <- attr(projection, "projection_feature_names", exact = TRUE)
  if (!is.null(attr_dim)) {
    return(list(
      ncol = attr_dim,
      colnames = attr_names
    ))
  }
  
  # 4) Inspect closure environment for a matrix whose rows match input columns
  env <- environment(projection)
  if (!is.null(env)) {
    candidate_names <- ls(env, all.names = TRUE)
    for (nm in candidate_names) {
      obj <- tryCatch(get(nm, envir = env, inherits = FALSE),
                      error = function(...) NULL)
      if (is.null(obj)) next
      if (is.matrix(obj) || inherits(obj, "Matrix")) {
        if (nrow(obj) == ncol(data)) {
          return(list(
            ncol = ncol(obj),
            colnames = colnames(obj)
          ))
        }
      }
    }
  }
  
  NULL
}

#' ProjectedDataSource
#'
#' R6 class that wraps raw time-series data and a projection (matrix or
#' function) and performs on-the-fly projection when accessed. Intended for
#' seamless integration with rMVPA by behaving like a matrix while deferring
#' computation until needed.
#'
#' @format An R6Class generator object
#' @export
ProjectedDataSource <- R6::R6Class("ProjectedDataSource",
  public = list(
    #' @field nrow Number of rows (observations)
    nrow = NULL,
    
    #' @field ncol Number of columns after projection
    ncol = NULL,
    
    #' @field dim Dimensions c(nrow, ncol)
    dim = NULL,
    
    #' @description Create a new ProjectedDataSource
    #' @param data Original data matrix or sparse matrix
    #' @param projection Projection matrix or function
    #' @param metadata Optional list of metadata to preserve
    initialize = function(data, projection, metadata = NULL) {
      # Validate inputs
      if (!is.matrix(data) && !inherits(data, "Matrix")) {
        stop("data must be a matrix or Matrix object")
      }
      
      if (is.matrix(projection)) {
        if (ncol(data) != nrow(projection)) {
          stop("data columns (", ncol(data), ") must match projection rows (", 
               nrow(projection), ")")
        }
      } else if (!is.function(projection)) {
        stop("projection must be a matrix or function")
      }
      
      private$.data <- data
      private$.projection <- projection
      private$.metadata <- metadata
      private$.cache <- NULL
      
      # Set dimensions
      self$nrow <- nrow(data)
      private$.colnames <- NULL
      
      if (is.matrix(projection)) {
        self$ncol <- ncol(projection)
        private$.colnames <- colnames(projection)
      } else {
        inferred <- infer_projection_output(projection, data, metadata)
        if (is.null(inferred)) {
          sample_cols <- seq_len(min(ncol(data), 50L))
          sample_projection <- projection(data[, sample_cols, drop = FALSE])
          sample_projection <- as.matrix(sample_projection)
          self$ncol <- ncol(sample_projection)
          private$.colnames <- colnames(sample_projection)
        } else {
          self$ncol <- inferred$ncol
          private$.colnames <- inferred$colnames
        }
      }
      
      self$dim <- c(self$nrow, self$ncol)
      
      # Preserve row names, create column names
      private$.rownames <- rownames(data)
      if (is.null(private$.rownames)) {
        private$.rownames <- as.character(seq_len(self$nrow))
      }
      if (is.null(private$.colnames) && !is.null(metadata) &&
          !is.null(metadata$feature_names)) {
        private$.colnames <- metadata$feature_names
      }
      
      if (is.null(private$.colnames) && !is.na(self$ncol) && self$ncol > 0) {
        private$.colnames <- paste0("Proj", seq_len(self$ncol))
      }
      self$dim <- c(self$nrow, self$ncol)
    },
    
    #' @description Convert to matrix (triggers projection)
    #' @param cache Whether to cache the projected result
    as.matrix = function(cache = TRUE) {
      # Return cached result if available
      if (!is.null(private$.cache)) {
        return(private$.cache)
      }
      
      # Perform projection
      if (isTRUE(getOption("fmriproj.verbose", FALSE))) {
        message("Projecting data (", self$nrow, " x ", ncol(private$.data),
                ") to (", self$nrow, " x ", self$ncol, ")...")
      }
      
      projected <- if (is.matrix(private$.projection)) {
        as.matrix(private$.data %*% private$.projection)
      } else {
        # Function-based projection
        private$.projection(private$.data)
      }
      
      # Update tracked dimensions/colnames if they were unknown
      if (is.na(self$ncol) || is.null(self$ncol)) {
        self$ncol <- ncol(projected)
        self$dim <- c(self$nrow, self$ncol)
      }
      
      if (is.null(private$.colnames)) {
        private$.colnames <- colnames(projected)
      }
      if (is.null(private$.colnames)) {
        private$.colnames <- paste0("Proj", seq_len(ncol(projected)))
      }
      
      # Add names
      rownames(projected) <- private$.rownames
      colnames(projected) <- private$.colnames
      
      # Preserve any custom attributes from metadata
      if (!is.null(private$.metadata$attributes)) {
        for (name in names(private$.metadata$attributes)) {
          attr(projected, name) <- private$.metadata$attributes[[name]]
        }
      }
      
      # Cache if requested
      if (cache) {
        private$.cache <- projected
      }
      
      projected
    },
    
    #' @description Get subset of rows (for chunked access)
    #' @param rows Row indices to extract
    get_rows = function(rows) {
      subset_data <- private$.data[rows, , drop = FALSE]
      
      if (is.matrix(private$.projection)) {
        result <- as.matrix(subset_data %*% private$.projection)
      } else {
        result <- private$.projection(subset_data)
      }
      
      rownames(result) <- private$.rownames[rows]
      colnames(result) <- private$.colnames
      result
    },
    
    #' @description Clear cached projection
    clear_cache = function() {
      private$.cache <- NULL
      invisible(self)
    },
    
    #' @description Check if data is cached
    is_cached = function() {
      !is.null(private$.cache)
    }
  ),
  
  private = list(
    .data = NULL,
    .projection = NULL,
    .metadata = NULL,
    .cache = NULL,
    .rownames = NULL,
    .colnames = NULL
  ),
  
  # Active bindings for property-like access
  active = list(
    #' @field data Access the projected data (triggers projection)
    data = function() {
      self$as.matrix()
    },
    
    #' @field original_data Access the original unprojected data
    original_data = function() {
      private$.data
    },
    
    #' @field projection_matrix Access the projection matrix/function
    projection_matrix = function() {
      private$.projection
    }
  )
)

#' Coerce ProjectedDataSource to matrix
#'
#' S3 method to allow seamless integration with functions expecting matrices
#'
#' @param x A ProjectedDataSource object
#' @param ... Additional arguments (ignored)
#' @return The projected data as a matrix
#' @export
as.matrix.ProjectedDataSource <- function(x, ...) {
  x$as.matrix()
}

#' Subset ProjectedDataSource
#'
#' S3 method to support matrix-style subsetting
#'
#' @param x A ProjectedDataSource object
#' @param i Row indices
#' @param j Column indices
#' @param drop Whether to drop dimensions
#' @return Subset of the projected data
#' @export
`[.ProjectedDataSource` <- function(x, i, j, drop = FALSE) {
  # Optimize row-only subsetting to avoid full projection
  if (missing(j) && !missing(i)) {
    # Use efficient row extraction
    return(x$get_rows(i))
  }
  
  if (!missing(j) && missing(i)) {
    full <- x$as.matrix()
    return(full[, j, drop = drop])
  }
  
  # For other cases, compute full projection then subset
  mat <- x$as.matrix()
  
  # Perform standard subsetting
  if (missing(i) && missing(j)) {
    mat
  } else if (missing(i)) {
    mat[, j, drop = drop]
  } else {
    mat[i, j, drop = drop]
  }
}

#' Dimensions of ProjectedDataSource
#'
#' @param x A ProjectedDataSource object
#' @return A vector c(nrow, ncol)
#' @export
dim.ProjectedDataSource <- function(x) {
  x$dim
}

#' Number of rows in ProjectedDataSource
#'
#' @param x A ProjectedDataSource object
#' @return Number of rows
#' @export
nrow.ProjectedDataSource <- function(x) {
  x$nrow
}

#' Number of columns in ProjectedDataSource
#'
#' @param x A ProjectedDataSource object
#' @return Number of columns
#' @export
ncol.ProjectedDataSource <- function(x) {
  x$ncol
}

#' Row names of ProjectedDataSource
#'
#' @param x A ProjectedDataSource object
#' @return Character vector of row names
#' @export
rownames.ProjectedDataSource <- function(x) {
  if (x$is_cached()) {
    rownames(x$as.matrix(cache = FALSE))
  } else {
    # Return stored rownames without triggering projection
    x$.__enclos_env__$private$.rownames
  }
}

#' Column names of ProjectedDataSource
#'
#' @param x A ProjectedDataSource object
#' @return Character vector of column names
#' @export
colnames.ProjectedDataSource <- function(x) {
  if (x$is_cached()) {
    colnames(x$as.matrix(cache = FALSE))
  } else {
    # Return stored colnames without triggering projection
    x$.__enclos_env__$private$.colnames
  }
}

#' Dimension names of ProjectedDataSource
#'
#' @param x A ProjectedDataSource object
#' @return List of row and column names
#' @export
dimnames.ProjectedDataSource <- function(x) {
  priv <- x$.__enclos_env__$private
  list(priv$.rownames, priv$.colnames)
}

#' Set row names of ProjectedDataSource
#'
#' @param x A ProjectedDataSource object
#' @param value New row names
#' @return The modified object (invisibly)
#' @method rownames<- ProjectedDataSource
#' @export
`rownames<-.ProjectedDataSource` <- function(x, value) {
  x$.__enclos_env__$private$.rownames <- value
  if (x$is_cached()) {
    rownames(x$.__enclos_env__$private$.cache) <- value
  }
  invisible(x)
}

#' Set column names of ProjectedDataSource
#'
#' @param x A ProjectedDataSource object
#' @param value New column names
#' @return The modified object (invisibly)
#' @method colnames<- ProjectedDataSource
#' @export
`colnames<-.ProjectedDataSource` <- function(x, value) {
  x$.__enclos_env__$private$.colnames <- value
  if (x$is_cached()) {
    colnames(x$.__enclos_env__$private$.cache) <- value
  }
  invisible(x)
}

#' Check if object is a matrix
#'
#' @param x A ProjectedDataSource object
#' @return TRUE (ProjectedDataSource behaves like a matrix)
#' @export
is.matrix.ProjectedDataSource <- function(x) {
  TRUE
}

#' Check if object is numeric
#'
#' @param x A ProjectedDataSource object
#' @return TRUE if the underlying data is numeric
#' @export
is.numeric.ProjectedDataSource <- function(x) {
  is.numeric(x$original_data)
}

#' Length of ProjectedDataSource
#'
#' @param x A ProjectedDataSource object
#' @return Total number of elements (nrow * ncol)
#' @export
length.ProjectedDataSource <- function(x) {
  x$nrow * x$ncol
}

#' Get class of ProjectedDataSource
#'
#' For compatibility, returns class of the projected matrix
#'
#' @param x A ProjectedDataSource object
#' @return Character vector of classes
#' @export
class.ProjectedDataSource <- function(x) {
  c("matrix", "array")
}

#' Head of ProjectedDataSource
#'
#' @param x A ProjectedDataSource object
#' @param n Number of rows to show
#' @param ... Additional arguments (ignored)
#' @return First n rows of the projected matrix
#' @export
head.ProjectedDataSource <- function(x, n = 6L, ...) {
  n <- min(n, x$nrow)
  x$get_rows(1:n)
}

#' Tail of ProjectedDataSource
#'
#' @param x A ProjectedDataSource object
#' @param n Number of rows to show
#' @param ... Additional arguments (ignored)
#' @return Last n rows of the projected matrix
#' @export
tail.ProjectedDataSource <- function(x, n = 6L, ...) {
  n <- min(n, x$nrow)
  rows <- seq(x$nrow - n + 1, x$nrow)
  x$get_rows(rows)
}

#' Print ProjectedDataSource
#'
#' @param x A ProjectedDataSource object
#' @param ... Additional arguments passed to print
#' @export
print.ProjectedDataSource <- function(x, ...) {
  cat("ProjectedDataSource (", x$nrow, " x ", x$ncol, ")\n", sep = "")
  cat("Original dimensions: ", nrow(x$original_data), " x ", 
      ncol(x$original_data), "\n", sep = "")
  cat("Projection type: ", 
      if (is.matrix(x$projection_matrix)) "matrix" else "function", "\n", sep = "")
  cat("Cache status: ", if (x$is_cached()) "cached" else "not cached", "\n", sep = "")
  
  # Show first few rows if small enough
  if (x$nrow <= 6 && x$ncol <= 6) {
    cat("\nData preview:\n")
    print(x$as.matrix(), ...)
  } else {
    cat("\nData preview (first 6 rows, 6 columns):\n")
    preview <- x$get_rows(1:min(6, x$nrow))[, 1:min(6, x$ncol), drop = FALSE]
    print(preview, ...)
    if (x$nrow > 6 || x$ncol > 6) {
      cat("[ ... ", x$nrow - 6, " more rows, ", x$ncol - 6, " more columns ]\n", sep = "")
    }
  }
  invisible(x)
}

#' Structure of ProjectedDataSource
#'
#' @param object A ProjectedDataSource object
#' @param ... Additional arguments passed to str
#' @export
str.ProjectedDataSource <- function(object, ...) {
  cat("ProjectedDataSource [", object$nrow, " x ", object$ncol, "] ", sep = "")
  cat("(projected from ", nrow(object$original_data), " x ", 
      ncol(object$original_data), ")\n", sep = "")
  
  # Show structure of a small sample
  if (object$nrow > 0 && object$ncol > 0) {
    sample_rows <- min(2, object$nrow)
    sample_cols <- min(3, object$ncol)
    sample_data <- object$get_rows(1:sample_rows)[, 1:sample_cols, drop = FALSE]
    cat(" Preview: ")
    str(sample_data, give.attr = FALSE, ...)
  }
}

#' Transpose of ProjectedDataSource
#'
#' @param x A ProjectedDataSource object
#' @return The transposed projected matrix
#' @export
t.ProjectedDataSource <- function(x) {
  t(x$as.matrix())
}
