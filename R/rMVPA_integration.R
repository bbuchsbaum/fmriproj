#' Extract time-series data from rMVPA model specification
#' @keywords internal
get_timeseries_from_model_spec <- function(model_spec) {
  if (!is.null(model_spec$dataset$Y)) {
    return(model_spec$dataset$Y)
  } else if (!is.null(model_spec$dataset$data)) {
    return(model_spec$dataset$data)
  } else {
    stop("Cannot extract time-series data from model_spec")
  }
}

#' Extract event model from rMVPA design
#' @keywords internal  
get_event_model_from_design <- function(design) {
  if (!is.null(design$event_model)) {
    return(design$event_model)
  }
  
  # Reconstruct minimal event model from design
  list(
    onsets = design$onsets %||% seq_along(design$y_train),
    n_time = design$n_time %||% (max(design$onsets) + 20),
    conditions = design$y_train,
    blocks = design$block_var
  )
}

#' Null-coalescing operator
#'
#' @name null-default
#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Projected searchlight analysis with rMVPA
#'
#' A wrapper that makes fmriproj projection work seamlessly with rMVPA's
#' run_searchlight function. Designed to minimize changes to rMVPA code.
#'
#' @param model_spec An rMVPA model specification
#' @param radius Searchlight radius
#' @param method Searchlight method ("standard" or "randomized")
#' @param projection_opts List of projection options (lambda_method, collapse_method, etc.)
#' @param ... Additional arguments passed to rMVPA::run_searchlight
#' @return rMVPA searchlight result object
#' @export
run_searchlight_projected <- function(model_spec, 
                                     radius = 3,
                                     method = "standard",
                                     projection_opts = list(),
                                     ...) {
  
  # Extract time-series data from model_spec
  Y <- get_timeseries_from_model_spec(model_spec)
  event_model <- get_event_model_from_design(model_spec$design)
  
  # Set default projection options
  proj_opts <- modifyList(
    list(
      lambda_adaptive_method = "EB",
      collapse_method = "pc",
      lambda_global = 0.1,
      align_sign = TRUE,
      sign_anchor = "canonical"
    ),
    projection_opts
  )
  
  # Build projection components once
  design <- build_design_matrix(event_model)
  proj_comp <- build_projector(design$X, lambda_global = proj_opts$lambda_global)
  
  # Bundle projection parameters
  spec <- projection_spec(
    event_model = event_model,
    projector_components = proj_comp,
    N_trials = length(event_model$onsets),
    K_hrf = ncol(design$hrf_info$basis),
    lambda_adaptive_method = proj_opts$lambda_adaptive_method,
    lambda_global = proj_opts$lambda_global,
    collapse_method = proj_opts$collapse_method,
    X_theta_dense = as.matrix(design$X),
    align_sign = proj_opts$align_sign,
    sign_anchor = proj_opts$sign_anchor
  )

  # Create searchlight function that returns what rMVPA expects
  sl_fun <- make_rmvpa_searchlight_fun(
    spec,
    return_format = "matrix"  # rMVPA expects simple matrix
  )
  
  # Replace the data in model_spec with our projection function
  # This is the key: we intercept at the data extraction level
  model_spec$dataset <- wrap_as_projecting_dataset(
    Y, sl_fun, model_spec$dataset, expected_trials = length(event_model$onsets)
  )
  
  # Now run standard rMVPA searchlight - no changes needed!
  rMVPA::run_searchlight(model_spec, radius = radius, method = method, ...)
}

#' Create a projecting dataset wrapper
#'
#' This creates an object that looks like an mvpa_dataset to rMVPA
#' but actually performs projection on-the-fly when data is requested.
#'
#' @keywords internal
wrap_as_projecting_dataset <- function(Y, projection_fun, original_dataset, expected_trials = NULL) {
  # Validate projection function
  if (!is.function(projection_fun)) {
    stop("projection_fun must be a function")
  }

  # Precompute mask mapping and nfeatures when using NeuroVec
  neuro_mask_arr <- NULL
  neuro_mask_lin <- NULL
  neuro_spatial_dim <- NULL
  neuro_nt <- NULL
  if (!(is.matrix(Y) || inherits(Y, "Matrix"))) {
    mask <- original_dataset$mask
    if (is.null(mask)) {
      stop("Original dataset mask is required to map voxel indices for NeuroVec data")
    }
    neuro_mask_arr <- try(as.array(mask), silent = TRUE)
    if (inherits(neuro_mask_arr, "try-error") || is.null(dim(neuro_mask_arr))) {
      mask_vals <- try(mask[], silent = TRUE)
      if (inherits(mask_vals, "try-error")) {
        stop("Unable to coerce mask to array for voxel mapping")
      }
      neuro_mask_arr <- array(mask_vals, dim = dim(mask))
    }
    neuro_spatial_dim <- dim(neuro_mask_arr)
    neuro_mask_lin <- which(neuro_mask_arr != 0)

    # Probe Y dims to get time length
    Y_arr_probe <- try(as.array(Y), silent = TRUE)
    if (!(inherits(Y_arr_probe, "try-error") || is.null(dim(Y_arr_probe)) || length(dim(Y_arr_probe)) < 4)) {
      neuro_nt <- dim(Y_arr_probe)[4]
    }
  }

  # Helper: fetch a time x voxels matrix for given indices
  fetch_matrix <- function(idxs = NULL) {
    # Case 1: in-memory matrix
    if (is.matrix(Y) || inherits(Y, "Matrix")) {
      if (is.null(idxs)) Y else Y[, idxs, drop = FALSE]
    } else {
      # Case 2: Y is NeuroVec; build matrix by indexing with precomputed mask mapping
      sel_masked <- if (is.null(idxs)) {
        neuro_mask_lin
      } else if (is.numeric(idxs)) {
        if (length(idxs) == 0) stop("Empty indices provided")
        if (any(idxs < 1) || any(idxs > length(neuro_mask_lin))) {
          stop("Invalid voxel indices: must be within masked voxel count (", length(neuro_mask_lin), ")")
        }
        neuro_mask_lin[as.integer(idxs)]
      } else if (inherits(idxs, "ROIVolWindow") || methods::is(idxs, "ROIVolWindow")) {
        # Convert ROI GLOBAL indices to MASKED positions
        roi_global <- neuroim2::indices(idxs)
        masked_pos <- match(roi_global, neuro_mask_lin)
        masked_pos <- masked_pos[!is.na(masked_pos)]
        if (length(masked_pos) == 0) integer(0) else neuro_mask_lin[masked_pos]
      } else {
        stop("Unsupported index type for NeuroVec extraction: ", paste(class(idxs), collapse = ", "))
      }
      # Coerce NeuroVec to 4D array
      Y_arr <- try(as.array(Y), silent = TRUE)
      if (inherits(Y_arr, "try-error") || length(dim(Y_arr)) < 4) {
        # fallback via bracket extraction using dims
        Y_vals <- try(Y[], silent = TRUE)
        Y_dim <- dim(Y)
        if (inherits(Y_vals, "try-error") || is.null(Y_dim) || length(Y_dim) < 4) {
          stop("Unable to coerce NeuroVec to 4D array for extraction")
        }
        Y_arr <- array(Y_vals, dim = Y_dim)
      }
      nt <- if (is.null(neuro_nt)) dim(Y_arr)[4] else neuro_nt
      ncol_out <- length(sel_masked)
      out <- matrix(NA_real_, nrow = nt, ncol = ncol_out)
      coords <- arrayInd(sel_masked, .dim = neuro_spatial_dim)
      for (j in seq_len(ncol_out)) {
        cx <- coords[j, 1]; cy <- coords[j, 2]; cz <- coords[j, 3]
        out[, j] <- Y_arr[cx, cy, cz, ]
      }
      out
    }
  }

  # Determine expected trials from caller, else infer on small probe
  if (is.null(expected_trials)) {
    sample_cols <- if (is.matrix(Y) || inherits(Y, "Matrix")) 1 else 1
    test_input <- fetch_matrix(sample_cols)
    test_proj <- tryCatch({
      projection_fun(test_input)
    }, error = function(e) {
      stop("Error testing projection function: ", e$message)
    })
    if (!is.matrix(test_proj)) {
      stop("projection_fun must return a matrix")
    }
    expected_trials <- nrow(test_proj)
  }

  # Create a projecting_train_data object that wraps the projection function
  # This will intercept series_roi calls from rMVPA
  projecting_train_obj <- structure(
    list(
      Y = Y,
      projection_fun = projection_fun,
      fetch_matrix = fetch_matrix,
      expected_trials = expected_trials,
      mask_lin = neuro_mask_lin,
      mask_space = if (!is.null(original_dataset$mask)) neuroim2::space(original_dataset$mask) else NULL
    ),
    class = "projecting_train_data"
  )

  result <- structure(
    list(
      Y = Y,
      projection_fun = projection_fun,
      original = original_dataset,
      expected_trials = expected_trials,
      mask_lin = neuro_mask_lin,
      mask_space = if (!is.null(original_dataset$mask)) neuroim2::space(original_dataset$mask) else NULL,
      # Number of features (masked voxels) for rMVPA bookkeeping
      nfeatures = if (is.matrix(Y) || inherits(Y, "Matrix")) ncol(Y) else length(neuro_mask_lin),
      # Delegate mask to original dataset (required by rMVPA searchlight)
      mask = original_dataset$mask,
      # Use our custom projecting_train_data object instead of original$train_data
      train_data = projecting_train_obj,
      get_data = function(indices = NULL) {
        # guard: empty indices
        if (!is.null(indices) && length(indices) == 0) {
          stop("Empty indices provided")
        }

        # Extract subset as matrix (time x voxels)
        Y_slice <- fetch_matrix(indices)

        # Defensive check on bounds if Y is a matrix
        if (is.matrix(Y)) {
          if (!is.null(indices)) {
            if (any(indices < 1) || any(indices > ncol(Y))) {
              stop("Invalid voxel indices: must be between 1 and ", ncol(Y))
            }
          }
        }

        # Perform projection
        result <- tryCatch({
          projection_fun(Y_slice)
        }, error = function(e) {
          stop("Projection failed: ", e$message)
        })

        if (!is.matrix(result)) {
          stop("Projection must return a matrix, got ", class(result))
        }
        if (nrow(result) != expected_trials) {
          stop("Projection returned ", nrow(result), " trials, expected ", expected_trials)
        }
        result
      }
    ),
    class = c("projecting_dataset", class(original_dataset))
  )

  result
}

## get_samples.projecting_dataset defined in R/projecting_dataset_methods.R

# Define S4 class for projecting_train_data
#' @keywords internal
setOldClass("projecting_train_data")

#' S4 method for series_roi on projecting_train_data
#'
#' Implements series_roi for our projecting_train_data class.
#' This gets called by rMVPA's as_roi.data_sample when it tries to extract ROI data.
#'
#' @param x A projecting_train_data object
#' @param i Voxel indices (typically a ROIVolWindow from neuroim2)
#' @param ... Additional arguments
#' @return A ROIVec object with projected data
#' @keywords internal
#' @export
#' @importFrom neuroim2 series_roi
setMethod("series_roi", signature(x = "projecting_train_data"),
  function(x, i, ...) {
    # Extract the data for these voxels
    Y_slice <- x$fetch_matrix(i)

    # Apply projection (guard small ROIs so extract_roi can filter them later)
    if (ncol(Y_slice) < 2) {
      proj_mat <- matrix(NA_real_, nrow = x$expected_trials, ncol = ncol(Y_slice))
    } else {
      proj_mat <- x$projection_fun(Y_slice)
    }

    # Determine MASKED-space positions (1..#masked) so rMVPA center id matches
    masked_pos <- if (inherits(i, "ROIVolWindow") || methods::is(i, "ROIVolWindow")) {
      roi_global <- neuroim2::indices(i)
      pos <- match(roi_global, x$mask_lin)
      pos[!is.na(pos)]
    } else if (is.numeric(i)) {
      as.integer(i)
    } else {
      stop("Unexpected vox type: ", paste(class(i), collapse = ", "))
    }

    # Build ROIVec in GLOBAL mask space so indices() match which(mask > 0)
    global_idx <- if (inherits(i, "ROIVolWindow") || methods::is(i, "ROIVolWindow")) {
      neuroim2::indices(i)
    } else {
      # numeric masked positions -> convert to global
      x$mask_lin[masked_pos]
    }
    mspace <- x$mask_space
    if (is.null(mspace)) {
      # Fallback to Y's spatial dims if mask space unavailable
      ydim <- dim(as.array(x$Y))
      mspace <- neuroim2::NeuroSpace(ydim[1:3])
    }
    coords <- neuroim2::index_to_grid(mspace, global_idx)
    neuroim2::ROIVec(mspace, coords, data = proj_mat)
  }
)


#' Convert data_sample to ROI for projecting_dataset
#'
#' S3 method to extract ROI data when the dataset is a projecting_dataset.
#' This intercepts the normal series_roi extraction and applies projection.
#'
#' @param x A data_sample object
#' @param data A projecting_dataset object
#' @param ... Additional arguments
#' @return List with train_roi and test_roi components
#' @keywords internal
#' @export
as_roi.data_sample <- function(x, data, ...) {
  # For projecting datasets, delegate to series_roi so our S4 method runs
  if (inherits(data, "projecting_dataset")) {
    train_roi <- neuroim2::series_roi(data$train_data, x$vox)
    return(list(train_roi = train_roi, test_roi = NULL))
  }

  # Fall back to default behavior for non-projecting datasets
  # This requires neuroim2
  if (!requireNamespace("neuroim2", quietly = TRUE)) {
    stop("neuroim2 package required for ROI extraction")
  }

  train_roi <- try(neuroim2::series_roi(data$train_data, x$vox), silent = TRUE)

  test_roi <- if (!is.null(data$test_data)) {
    try(neuroim2::series_roi(data$test_data, x$vox), silent = TRUE)
  } else {
    NULL
  }

  list(train_roi = train_roi, test_roi = test_roi)
}

# Register projected_roi as an S4 class
setOldClass("projected_roi")

#' S4 method for values on projected_roi
#'
#' Extract the projected matrix data from a projected_roi object.
#'
#' @param x A projected_roi object
#' @return Matrix of projected data
#' @keywords internal
#' @importFrom neuroim2 values
#' @export
setMethod("values", signature(x = "projected_roi"),
  function(x) {
    x$data
  }
)

#' S4 method for indices on projected_roi
#'
#' Extract the voxel indices from a projected_roi object.
#'
#' @param x A projected_roi object
#' @return Vector of voxel indices
#' @keywords internal
#' @importFrom neuroim2 indices
#' @export
setMethod("indices", signature(x = "projected_roi"),
  function(x) {
    x$indices
  }
)

#' Filter ROI for projected_roi data
#'
#' S3 method to filter ROI when the train_roi is a projected_roi object.
#' Since projected data is already filtered and processed during projection,
#' we skip the typical filtering for NA and zero variance.
#'
#' @param roi List with train_roi and test_roi components
#' @param preserve Optional voxel index to preserve (ignored for projected data)
#' @param ... Additional arguments
#' @return Filtered ROI list
#' @keywords internal
#' @export
filter_roi.projected_roi <- function(roi, preserve = NULL, ...) {
  # For projected data, the data is already filtered and processed
  # No need for additional filtering - just return as-is
  roi
}

#' Progressive projection feature selector for rMVPA
#'
#' Creates an rMVPA-compatible feature selector that uses fmriproj's
#' progressive projection instead of traditional feature selection.
#'
#' @param method PP method ("LDA" or "PLS-DA")
#' @param dims Number of dimensions
#' @return Feature selector object compatible with rMVPA
#' @export
pp_feature_selector <- function(method = "LDA", dims = 2) {
  # Create environment to store projection state
  proj_env <- new.env(parent = emptyenv())
  
  structure(
    list(
      method = method,
      dims = dims,
      cutoff_type = "top_k",  # rMVPA compatibility
      cutoff_value = dims,    # rMVPA compatibility
      projection_env = proj_env,
      
      # This is what rMVPA calls
      select_features = function(X, Y, ...) {
        # Instead of returning logical vector, return projection function
        pp_model <- fit_pp(X, Y, method = method, dims = dims)
        
        # Store the projection model and function
        proj_env$model <- pp_model
        proj_env$projection_function <- function(X_new) {
          predict_pp(pp_model, X_new)
        }
        
        # Return all features as "selected" - projection happens elsewhere
        rep(TRUE, ncol(X))
      },
      
      # Method to get the projection function
      get_projection_function = function() {
        proj_env$projection_function
      }
    ),
    class = "feature_selector"
  )
}

#' Create rMVPA model with automatic projection support
#'
#' Convenience wrapper that creates an rMVPA model with projection applied
#' to the dataset. This is a simpler alternative to using model_spec() and
#' create_projected_model() separately.
#'
#' @param model_type Model type: "classification", "regression", "rsa", etc.
#' @param dataset rMVPA dataset or raw data matrix
#' @param projection Optional projection matrix or specification
#' @param ... Additional arguments passed to model constructor
#' @return rMVPA model object with projection support
#' @export
#' @examples
#' \dontrun{
#' # Quick creation with projection
#' model <- create_mvpa_model_projected(
#'   "classification",
#'   dataset = my_data,
#'   labels = my_labels,
#'   projection = my_projection_matrix
#' )
#' }
create_mvpa_model_projected <- function(model_type, dataset, projection = NULL, ...) {
  # Build model specification
  spec <- model_spec(type = model_type, data = dataset, ...)
  
  if (!is.null(projection)) {
    # Use new architecture with projection
    create_projected_model(spec, projection)
  } else if (inherits(dataset, "projecting_dataset")) {
    # Dataset already has projection built-in
    build_rmvpa_model(type = model_type, data = dataset, ...)
  } else {
    # No projection, standard model
    build_rmvpa_model(type = model_type, data = dataset, ...)
  }
}

#' Run cross-validation with projection
#'
#' Wrapper for rMVPA's mvpa_crossval that ensures projection is applied
#' correctly during cross-validation.
#'
#' @param model_spec rMVPA model specification
#' @param crossval Cross-validation scheme
#' @param projection_opts Optional projection parameters to override defaults
#' @param ... Additional arguments passed to mvpa_crossval
#' @return Cross-validation results
#' @keywords internal
#' @export
mvpa_crossval_projected <- function(model_spec, 
                                   crossval,
                                   projection_opts = NULL,
                                   ...) {
  
  if (!requireNamespace("rMVPA", quietly = TRUE)) {
    stop("rMVPA package required")
  }
  
  # If using a projecting_dataset, just run standard crossval
  if (inherits(model_spec$dataset, "projecting_dataset")) {
    return(rMVPA::mvpa_crossval(model_spec, crossval, ...))
  }
  
  # Otherwise, wrap the dataset with projection
  if (!is.null(projection_opts)) {
    # Extract time-series and create projecting dataset
    Y <- get_timeseries_from_model_spec(model_spec)
    event_model <- get_event_model_from_design(model_spec$design)
    
    # Build projection function
    design <- build_design_matrix(event_model)
    proj_comp <- build_projector(design$X)
    
    spec <- projection_spec(
      event_model = event_model,
      projector_components = proj_comp,
      N_trials = length(event_model$onsets),
      K_hrf = ncol(design$hrf_info$basis),
      lambda_adaptive_method = projection_opts$lambda_adaptive_method %||% "EB",
      lambda_global = projection_opts$lambda_global %||% 0.1,
      collapse_method = projection_opts$collapse_method %||% "pc",
      X_theta_dense = as.matrix(design$X),
      align_sign = projection_opts$align_sign %||% TRUE,
      sign_anchor = projection_opts$sign_anchor %||% "canonical"
    )
    
    sl_fun <- make_rmvpa_searchlight_fun(spec, return_format = "matrix")
    
    # Replace dataset
    model_spec$dataset <- wrap_as_projecting_dataset(
      Y, sl_fun, model_spec$dataset, expected_trials = length(event_model$onsets)
    )
  }
  
  rMVPA::mvpa_crossval(model_spec, crossval, ...)
}

#' Run iterative analysis with projection
#'
#' Wrapper for rMVPA's mvpa_iterate that applies projection before
#' each iteration.
#'
#' @param model_spec rMVPA model specification
#' @param crossval Cross-validation scheme
#' @param iterations Number of iterations
#' @param projection_opts Optional projection parameters
#' @param ... Additional arguments passed to mvpa_iterate
#' @return Iteration results
#' @keywords internal
#' @export
mvpa_iterate_projected <- function(model_spec,
                                  crossval, 
                                  iterations = 10,
                                  projection_opts = NULL,
                                  ...) {
  
  if (!requireNamespace("rMVPA", quietly = TRUE)) {
    stop("rMVPA package required")
  }
  
  # Apply projection wrapper if needed
  if (!inherits(model_spec$dataset, "projecting_dataset") && !is.null(projection_opts)) {
    model_spec <- mvpa_crossval_projected(model_spec, crossval, projection_opts, ...)
  }
  
  rMVPA::mvpa_iterate(model_spec, crossval, iterations, ...)
}
