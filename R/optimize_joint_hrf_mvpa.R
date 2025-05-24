#' Optimize HRF parameters for MVPA performance
#'
#' Runs an outer `stats::optim` loop over HRF parameters `theta`.
#' The loss for a given `theta` is computed by constructing the
#' corresponding design matrix, projecting the data, collapsing the
#' betas, and calling a user-supplied inner cross-validation function
#' on the resulting trial patterns.
#'
#' @param theta_init Initial values for the HRF parameters.
#' @param Y BOLD data matrix (time points x voxels).
#' @param event_model Event model list passed to `make_trialwise_X`.
#' @param inner_cv_fn Function taking `A_sl` and returning a numeric loss.
#' @param hrf_basis_func HRF basis generating function.
#' @param lambda_global Global ridge penalty.
#' @param lambda_adaptive_method Method passed to `adaptive_ridge_projector`.
#' @param collapse_method Collapse method for `collapse_beta`.
#' @param optim_method Optimization method for `stats::optim`.
#' @param diagnostics Logical; return optimization trace.
#' @param ... Additional arguments passed to `inner_cv_fn`.
#'
#' @return A list with elements `theta_hat`, `optim_details`, and optional
#'   `diagnostics` containing the optimization trace.
#' @export
optimize_joint_hrf_mvpa <- function(theta_init,
                                    Y,
                                    event_model,
                                    inner_cv_fn,
                                    hrf_basis_func = hrf_basis_spmg3_theta,
                                    lambda_global = 0,
                                    lambda_adaptive_method = "none",
                                    collapse_method = "rss",
                                    optim_method = "Nelder-Mead",
                                    diagnostics = FALSE,
                                    ...) {
  trace_env <- new.env(parent = emptyenv())
  trace_env$df <- data.frame()

  loss_fn_theta <- function(theta) {
    X_obj <- make_trialwise_X(event_model,
                              hrf_basis_func = hrf_basis_func,
                              theta_params = theta,
                              diagnostics = FALSE)
    X_theta <- X_obj$X
    proj_comp <- build_projector(X_theta,
                                 lambda_global = lambda_global,
                                 diagnostics = FALSE)
    proj_res <- adaptive_ridge_projector(
      Y,
      proj_comp,
      lambda_adaptive_method = lambda_adaptive_method,
      lambda_floor_global = lambda_global,
      X_theta_for_EB_residuals = as.matrix(X_theta),
      diagnostics = FALSE
    )
    N_trials <- length(event_model$onsets)
    K_hrf <- ncol(as.matrix(X_obj$hrf_info$basis))
    coll_res <- collapse_beta(proj_res$Z_sl_raw,
                              N_trials,
                              K_hrf,
                              method = collapse_method,
                              diagnostics = FALSE)
    loss <- inner_cv_fn(coll_res$A_sl, ...)

    if (isTRUE(diagnostics)) {
      row <- c(loss = loss,
               setNames(as.numeric(theta),
                        paste0("theta", seq_along(theta))))
      trace_env$df <- rbind(trace_env$df, row)
    }
    loss
  }

  optim_res <- stats::optim(par = theta_init,
                            fn = loss_fn_theta,
                            method = optim_method)

  diag_list <- NULL
  if (diagnostics) {
    colnames(trace_env$df) <- c("loss",
                               paste0("theta", seq_along(theta_init)))
    diag_list <- list(theta_trace = trace_env$df)
  }

  list(theta_hat = optim_res$par,
       optim_details = optim_res,
       diagnostics = diag_list)
}
