#' Run projected MVPA searchlight (placeholder)
#'
#' Constructs the trial-wise design matrix, builds global projector components,
#' and prepares a searchlight function that applies adaptive ridge projection and
#' beta collapse. If the `rMVPA` package is installed, this function will call
#' `rMVPA::searchlight` with the prepared function. Otherwise a list containing
#' the components and searchlight function is returned.
#'
#' @param Y BOLD data matrix (time points x voxels).
#' @param event_model List describing the experimental events passed to
#'   \code{make_trialwise_X}.
#' @param hrf_basis_func Optional HRF basis generating function.
#' @param theta_params Optional parameters for \code{hrf_basis_func}.
#' @param hrf_basis_matrix Optional pre-computed HRF basis matrix.
#' @param lambda_global Global ridge penalty.
#' @param lambda_adaptive_method Method for searchlight-specific lambda.
#' @param collapse_method Method for \code{collapse_beta}.
#' @param diagnostics Logical; return diagnostic information.
#' @param ... Additional arguments passed to \code{rMVPA::searchlight} when that
#'   package is available.
#'
#' @return If \code{rMVPA} is installed, the result of
#'   \code{rMVPA::searchlight}. Otherwise a list containing the prepared
#'   components and searchlight function.
#' @export
run_projected_searchlight <- function(Y,
                                      event_model,
                                      hrf_basis_func = NULL,
                                      theta_params = NULL,
                                      hrf_basis_matrix = NULL,
                                      lambda_global = 0,
                                      lambda_adaptive_method = "none",
                                      collapse_method = "rss",
                                      diagnostics = FALSE,
                                      ...) {
  X_obj <- build_design_matrix(event_model,
                               hrf_basis_func = hrf_basis_func,
                               theta_params = theta_params,
                               hrf_basis_matrix = hrf_basis_matrix,
                               diagnostics = diagnostics)
  X_theta <- X_obj$X
  proj_comp <- build_projector(X_theta,
                               lambda_global = lambda_global,
                               diagnostics = diagnostics)

  N_trials <- length(event_model$onsets)
  K_hrf <- ncol(as.matrix(X_obj$hrf_info$basis))

  sl_FUN <- function(Y_sl, sl_info = NULL) {
    proj_res <- adaptive_ridge_projector(
      Y_sl,
      proj_comp,
      lambda_adaptive_method = lambda_adaptive_method,
      lambda_floor_global = lambda_global,
      X_theta_for_EB_residuals = as.matrix(X_theta),
      diagnostics = diagnostics
    )
    coll_res <- collapse_beta(
      proj_res$Z_sl_raw,
      N_trials,
      K_hrf,
      method = collapse_method,
      diagnostics = diagnostics
    )
    diag_out <- NULL
    if (diagnostics) {
      dl <- list()
      lambda_val <- NA
      if (!is.null(proj_res$diag_data)) {
        if (!is.null(proj_res$diag_data$lambda_sl_chosen)) {
          lambda_val <- proj_res$diag_data$lambda_sl_chosen
        }
      }
      dl$lambda_sl <- lambda_val
      if (!is.null(coll_res$w_sl)) {
        dl$w_sl <- coll_res$w_sl
      }
      diag_out <- cap_diagnostics(dl)
    }
    list(A_sl = coll_res$A_sl, diag_data = diag_out)
  }

  if (requireNamespace("rMVPA", quietly = TRUE)) {
    res <- rMVPA::searchlight(
      Y,
      FUN = sl_FUN,
      .combine = combine_projection_diagnostics,
      ...
    )
    if (diagnostics) {
      dl <- list(layer1 = attr(X_obj, "diagnostics"),
                 layer2 = attr(proj_comp, "diagnostics"))
      attr(res, "diagnostics") <- cap_diagnostics(dl)
    }
    return(res)
  }

  message("rMVPA package not available - returning components for manual use")
  out <- list(FUN = sl_FUN, design = X_obj, projector = proj_comp)
  if (diagnostics) {
    dl <- list(layer1 = attr(X_obj, "diagnostics"),
               layer2 = attr(proj_comp, "diagnostics"))
    out$diagnostics <- cap_diagnostics(dl)
  }
  out
}

mvpa_projected_searchlight <- run_projected_searchlight
