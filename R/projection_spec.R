#' Projection specification object
#'
#' Convenience constructor that bundles all parameters required for
#' on-the-fly projection when interfacing with rMVPA.
#'
#' @param event_model Event model describing trial onsets.
#' @param projector_components Pre-computed projector from `build_projector()`.
#' @param N_trials Number of trials in the event model.
#' @param K_hrf Number of HRF basis functions.
#' @param lambda_adaptive_method Adaptive lambda method.
#' @param lambda_global Global ridge penalty used when building the projector.
#' @param collapse_method Method for `collapse_beta()`.
#' @param X_theta_dense Optional dense design matrix for empirical Bayes.
#'
#' @return An object of class `projection_spec`.
#' @export
projection_spec <- function(event_model,
                            projector_components,
                            N_trials,
                            K_hrf,
                            lambda_adaptive_method = "none",
                            lambda_global = 0,
                            collapse_method = "rss",
                            X_theta_dense = NULL) {
  structure(
    list(
      event_model = event_model,
      projector_components = projector_components,
      N_trials = N_trials,
      K_hrf = K_hrf,
      lambda_adaptive_method = lambda_adaptive_method,
      lambda_global = lambda_global,
      collapse_method = collapse_method,
      X_theta_dense = X_theta_dense
    ),
    class = "projection_spec"
  )
}
