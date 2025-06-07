#' Create rMVPA-compatible searchlight function
#'
#' Wraps fmriproj's projection pipeline to produce output that matches
#' what rMVPA expects from a feature extraction function.
#'
#' @param spec A `projection_spec` object describing the projection
#'   parameters.
#' @param return_format Format of return value: "matrix" (just features), 
#'   "mvpa_data" (rMVPA data structure), or "list" (with diagnostics)
#' @return A function suitable for rMVPA::run_searchlight
#' @export
make_rmvpa_searchlight_fun <- function(spec,
                                       return_format = c("matrix", "mvpa_data", "list")) {
  
  return_format <- match.arg(return_format)

  event_model <- spec$event_model
  projector_components <- spec$projector_components
  N_trials <- spec$N_trials
  K_hrf <- spec$K_hrf
  lambda_adaptive_method <- spec$lambda_adaptive_method
  lambda_global <- spec$lambda_global
  collapse_method <- spec$collapse_method
  X_theta_dense <- spec$X_theta_dense

  function(Y_sl, coords = NULL, indices = NULL, ...) {
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
    
    # Return in requested format
    switch(return_format,
      matrix = coll_res$A_sl,  # Simple matrix for basic rMVPA
      
      mvpa_data = {
        # Return structure that mimics mvpa_dataset output
        list(
          data = coll_res$A_sl,
          nobs = N_trials,
          mask = indices,
          coords = coords
        )
      },
      
      list = {
        # Full output with diagnostics
        list(
          data = coll_res$A_sl,
          lambda_sl = proj_res$diag_data$lambda_sl_chosen,
          w_sl = coll_res$w_sl,
          coords = coords,
          indices = indices
        )
      }
    )
  }
}

#' Create rMVPA-compatible dataset from time-series
#'
#' Converts time-series data to trial patterns using fmriproj, returning
#' an object that can be used directly with rMVPA functions.
#'
#' @param Y Time-series data (T x voxels)
#' @param event_model Event model from fmrireg
#' @param mask Brain mask (same dimensions as Y spatial dims)
#' @param ... Additional parameters passed to projection pipeline
#' @return An rMVPA-compatible dataset object
#' @export
as_mvpa_dataset <- function(Y, event_model, mask = NULL, ...) {
  # Build projection components
  design <- build_design_matrix(event_model, ...)
  proj_comp <- build_projector(design$X, ...)
  
  # Project full brain data
  N_trials <- length(event_model$onsets)
  K_hrf <- ncol(design$hrf_info$basis)
  
  # Apply projection
  proj_res <- adaptive_ridge_projector(Y, proj_comp, ...)
  coll_res <- collapse_beta(proj_res$Z_sl_raw, N_trials, K_hrf, ...)
  
  # Create rMVPA-compatible structure
  structure(
    list(
      data = coll_res$A_sl,
      mask = mask,
      nobs = N_trials,
      event_model = event_model,
      projection_info = list(
        design = design,
        projector = proj_comp,
        collapse_weights = coll_res$w_sl
      )
    ),
    class = c("fmriproj_mvpa_dataset", "mvpa_dataset")
  )
}