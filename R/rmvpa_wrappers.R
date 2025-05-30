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
      collapse_method = "rss",
      lambda_global = 0.1
    ),
    projection_opts
  )
  
  # Build projection components once
  design <- build_design_matrix(event_model)
  proj_comp <- build_projector(design$X, lambda_global = proj_opts$lambda_global)
  
  # Create searchlight function that returns what rMVPA expects
  sl_fun <- make_rmvpa_searchlight_fun(
    event_model = event_model,
    projector_components = proj_comp,
    N_trials = length(event_model$onsets),
    K_hrf = ncol(design$hrf_info$basis),
    lambda_adaptive_method = proj_opts$lambda_adaptive_method,
    collapse_method = proj_opts$collapse_method,
    return_format = "matrix"  # rMVPA expects simple matrix
  )
  
  # Replace the data in model_spec with our projection function
  # This is the key: we intercept at the data extraction level
  model_spec$dataset <- wrap_as_projecting_dataset(
    Y, sl_fun, model_spec$dataset
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
wrap_as_projecting_dataset <- function(Y, projection_fun, original_dataset) {
  structure(
    list(
      Y = Y,
      projection_fun = projection_fun,
      original = original_dataset,
      get_data = function(indices = NULL) {
        if (is.null(indices)) {
          # Full brain projection
          projection_fun(Y)
        } else {
          # Searchlight projection
          projection_fun(Y[, indices, drop = FALSE])
        }
      }
    ),
    class = c("projecting_dataset", class(original_dataset))
  )
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
  structure(
    list(
      method = method,
      dims = dims,
      cutoff_type = "top_k",  # rMVPA compatibility
      cutoff_value = dims,    # rMVPA compatibility
      
      # This is what rMVPA calls
      select_features = function(X, Y, ...) {
        # Instead of returning logical vector, return projection function
        pp_model <- fit_pp(X, Y, method = method, dims = dims)
        
        # Return a function that projects new data
        attr(X, "projection_function") <- function(X_new) {
          predict_pp(pp_model, X_new)
        }
        
        # Return all features as "selected" - projection happens elsewhere
        rep(TRUE, ncol(X))
      }
    ),
    class = "feature_selector"
  )
}