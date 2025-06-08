#' Enhanced searchlight function with diagnostics
#'
#' Creates a searchlight function that captures projection diagnostics
#' alongside the projected data for later analysis.
#'
#' @param spec A projection_spec object
#' @param return_diagnostics Logical; whether to store diagnostics
#' @return A function suitable for rMVPA searchlight with diagnostic capture
#' @export
make_diagnostic_searchlight_fun <- function(spec, return_diagnostics = TRUE) {
  
  # Extract components from spec
  event_model <- spec$event_model
  projector_components <- spec$projector_components
  N_trials <- spec$N_trials
  K_hrf <- spec$K_hrf
  lambda_adaptive_method <- spec$lambda_adaptive_method
  lambda_global <- spec$lambda_global
  collapse_method <- spec$collapse_method
  X_theta_dense <- spec$X_theta_dense
  
  # Storage for diagnostics
  diagnostics_env <- new.env(parent = emptyenv())
  diagnostics_env$diagnostics <- list()
  diagnostics_env$counter <- 0
  
  # The searchlight function
  sl_fun <- function(Y_sl, coords = NULL, indices = NULL, ...) {
    # Core projection pipeline
    proj_res <- adaptive_ridge_projector(
      Y_sl,
      projector_components,
      lambda_adaptive_method = lambda_adaptive_method,
      lambda_floor_global = lambda_global,
      X_theta_for_EB_residuals = X_theta_dense
    )
    
    coll_res <- collapse_beta(
      proj_res$Z_sl_raw,
      N_trials,
      K_hrf,
      method = collapse_method
    )
    
    # Store diagnostics if requested
    if (return_diagnostics) {
      diagnostics_env$counter <- diagnostics_env$counter + 1
      diagnostics_env$diagnostics[[diagnostics_env$counter]] <- list(
        indices = indices,
        coords = coords,
        lambda_sl = proj_res$diag_data$lambda_sl_chosen,
        w_sl = coll_res$w_sl,
        projection_quality = list(
          residual_variance = proj_res$diag_data$residual_var,
          effective_df = proj_res$diag_data$effective_df
        )
      )
    }
    
    # Return projected data
    coll_res$A_sl
  }
  
  # Attach function to retrieve diagnostics
  attr(sl_fun, "get_diagnostics") <- function() {
    diagnostics_env$diagnostics
  }
  
  # Attach function to reset diagnostics
  attr(sl_fun, "reset_diagnostics") <- function() {
    diagnostics_env$diagnostics <- list()
    diagnostics_env$counter <- 0
  }
  
  sl_fun
}

#' Attach projection diagnostics to rMVPA results
#'
#' Enhances rMVPA searchlight results with projection-specific diagnostics
#' for interpretation and quality assessment.
#'
#' @param mvpa_result Result from rMVPA::run_searchlight
#' @param projection_diagnostics List of diagnostics from projection
#' @return Enhanced result object with diagnostics attached
#' @export
attach_projection_diagnostics <- function(mvpa_result, projection_diagnostics) {
  # Create enhanced result
  enhanced_result <- mvpa_result
  
  # Add projection diagnostics as an attribute
  attr(enhanced_result, "projection_diagnostics") <- projection_diagnostics
  
  # Add summary statistics
  if (length(projection_diagnostics) > 0) {
    lambda_values <- sapply(projection_diagnostics, function(x) x$lambda_sl)
    
    attr(enhanced_result, "projection_summary") <- list(
      lambda_stats = summary(lambda_values),
      lambda_spatial_map = create_spatial_map(
        values = lambda_values,
        coords = lapply(projection_diagnostics, function(x) x$coords),
        mask = mvpa_result$mask
      ),
      mean_effective_df = mean(sapply(projection_diagnostics, 
                                     function(x) x$projection_quality$effective_df),
                               na.rm = TRUE)
    )
  }
  
  class(enhanced_result) <- c("fmriproj_searchlight_result", class(enhanced_result))
  enhanced_result
}

#' Create spatial map from diagnostic values
#'
#' Helper function to map diagnostic values back to brain space
#'
#' @param values Vector of values (one per searchlight)
#' @param coords List of coordinate vectors
#' @param mask Original brain mask
#' @return NeuroVol object with diagnostic values
#' @keywords internal
create_spatial_map <- function(values, coords, mask) {
  if (!requireNamespace("neuroim2", quietly = TRUE)) {
    warning("neuroim2 required for spatial maps")
    return(NULL)
  }
  
  # Initialize output volume
  out_vol <- array(NA, dim = dim(mask))
  
  # Map values to voxel locations
  for (i in seq_along(values)) {
    if (!is.null(coords[[i]])) {
      out_vol[coords[[i]]] <- values[i]
    }
  }
  
  # Create NeuroVol
  neuroim2::NeuroVol(out_vol, space = neuroim2::space(mask))
}

#' Plot projection diagnostics
#'
#' Visualize projection diagnostics from enhanced searchlight results
#'
#' @param result Enhanced searchlight result with diagnostics
#' @param diagnostic Type of diagnostic to plot
#' @param slice Slice number for visualization
#' @return ggplot object
#' @export
plot_projection_diagnostics <- function(result, 
                                      diagnostic = c("lambda", "effective_df", "residual_var"),
                                      slice = NULL) {
  
  if (!inherits(result, "fmriproj_searchlight_result")) {
    stop("Result must be from fmriproj with diagnostics enabled")
  }
  
  diagnostic <- match.arg(diagnostic)
  
  if (diagnostic == "lambda") {
    if (is.null(attr(result, "projection_summary")$lambda_spatial_map)) {
      stop("Lambda spatial map not available")
    }
    
    # Plot lambda map
    plot_data <- attr(result, "projection_summary")$lambda_spatial_map
    # Implementation would use neuroim2 plotting functions
    
  } else {
    stop("Diagnostic type not yet implemented")
  }
}

#' Summary method for enhanced searchlight results
#'
#' @param object Enhanced searchlight result
#' @param ... Additional arguments (ignored)
#' @export
summary.fmriproj_searchlight_result <- function(object, ...) {
  # Standard rMVPA summary
  mvpa_summary <- NextMethod()
  
  # Add projection summary if available
  proj_summary <- attr(object, "projection_summary")
  if (!is.null(proj_summary)) {
    cat("\nProjection Diagnostics:\n")
    cat("  Lambda statistics:\n")
    print(proj_summary$lambda_stats)
    cat("\n  Mean effective degrees of freedom:", 
        round(proj_summary$mean_effective_df, 2), "\n")
  }
  
  invisible(list(mvpa = mvpa_summary, projection = proj_summary))
}