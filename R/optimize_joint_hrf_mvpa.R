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
#' @param inner_cv_fn Function taking `A_sl` and returning a numeric scalar loss.
#' @param hrf_basis_func HRF basis generating function.
#' @param lambda_global Global ridge penalty.
#' @param lambda_adaptive_method Method passed to `adaptive_ridge_projector`.
#' @param collapse_method Collapse method for `collapse_beta`.
#' @param optim_method Optimization method for `stats::optim`.
#' @param labels_for_w_optim Trial labels for supervised weight optimization. 
#'   Required when `collapse_method = "optim"`.
#' @param classifier_for_w_optim Function for supervised weight optimization.
#'   Should take `A_sl` and `labels` and return a list with `loss` and `grad`.
#'   Required when `collapse_method = "optim"`.
#' @param optim_w_params List of control parameters passed to the optimizer
#'   for weight optimization when `collapse_method = "optim"`.
#' @param diagnostics Logical; return optimization trace
#' @param use_fd_grad Logical; compute gradient using finite differences.
#'   An optional TMB-based implementation can be added, but there is no
#'   requirement for TMB.
#' @param use_tmb `r lifecycle::badge('deprecated')` Use `use_fd_grad` instead.
#' @param lower Lower bounds for optimization (for methods that support bounds).
#' @param upper Upper bounds for optimization (for methods that support bounds).
#' @param ... Additional arguments passed to `inner_cv_fn`.
#'
#' @return A list with elements `theta_hat`, `optim_details`, and optional
#'   `diagnostics` containing the optimization trace.
#' @aliases optimize_joint_hrf_mvpa
#' @export
optimize_hrf_mvpa <- function(theta_init,
                              Y,
                              event_model,
                              inner_cv_fn,
                              hrf_basis_func = hrf_basis_spmg3_theta,
                              lambda_global = 0,
                              lambda_adaptive_method = "none",
                              collapse_method = "rss",
                              optim_method = "Nelder-Mead",
                              labels_for_w_optim = NULL,
                              classifier_for_w_optim = NULL,
                              optim_w_params = list(),
                              use_fd_grad = FALSE,
                              use_tmb = NULL,
                              diagnostics = FALSE,
                              lower = -Inf,
                              upper = Inf,
                              ...) {
  trace_env <- new.env(parent = emptyenv())

  if (!is.null(use_tmb)) {
    lifecycle::deprecate_warn(
      "0.1.1",
      "optimize_hrf_mvpa(use_tmb)",
      "optimize_hrf_mvpa(use_fd_grad)"
    )
    use_fd_grad <- use_tmb
  }

  N_trials <- length(event_model$onsets)
  trace_env$rows <- list()


  loss_fn_theta <- function(theta, record = TRUE) {
    X_obj <- build_design_matrix(event_model,
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
    K_hrf <- ncol(X_obj$hrf_info$basis)
    coll_res <- collapse_beta(
      proj_res$Z_sl_raw,
      N_trials,
      K_hrf,
      method = collapse_method,
      diagnostics = FALSE,
      labels_for_w_optim = labels_for_w_optim,
      classifier_for_w_optim = classifier_for_w_optim,
      optim_w_params = optim_w_params
    )
    loss <- inner_cv_fn(coll_res$A_sl, ...)
    if (!is.numeric(loss) || length(loss) != 1 || !is.finite(loss)) {
      stop("`inner_cv_fn` must return a finite numeric scalar 'loss'.", call. = FALSE)
    }

    if (isTRUE(record) && isTRUE(diagnostics)) {
      row <- c(loss = loss,
               setNames(as.numeric(theta),
                        paste0("theta", seq_along(theta))))
      trace_env$rows[[length(trace_env$rows) + 1]] <- row
    }
    loss
  }

  grad_fn <- NULL
  if (use_fd_grad) {
    grad_fn <- function(th) {
      eps <- 1e-6
      base_loss <- loss_fn_theta(th, record = FALSE)
      sapply(seq_along(th), function(i) {
        th_eps <- th
        th_eps[i] <- th_eps[i] + eps
        (loss_fn_theta(th_eps, record = FALSE) - base_loss) / eps
      })
    }
  }

  valid_methods <-
    c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent")
  if (!optim_method %in% valid_methods) {
    stop(paste0("`optim_method` must be one of: ",
                paste(valid_methods, collapse = ", ")), call. = FALSE)
  }

  bounds_finite <- any(is.finite(lower)) || any(is.finite(upper))
  if (bounds_finite && !optim_method %in% c("L-BFGS-B", "Brent")) {
    warning(paste0("Bounds specified but ignored for method '",
                   optim_method, "'."), call. = FALSE)
  }

  optim_args <- list(par = theta_init,
                     fn = loss_fn_theta,
                     gr = grad_fn,
                     method = optim_method)
  if (bounds_finite && optim_method %in% c("L-BFGS-B", "Brent")) {
    optim_args$lower <- lower
    optim_args$upper <- upper
  }

  optim_res <- do.call(stats::optim, optim_args)

  diag_list <- NULL
  if (diagnostics) {
    trace_df <- as.data.frame(do.call(rbind, trace_env$rows))
    colnames(trace_df) <- c("loss",
                           paste0("theta", seq_along(theta_init)))
    dl <- list(theta_trace = trace_df)
    diag_list <- cap_diagnostics(dl)
  }

  list(theta_hat = optim_res$par,
       optim_details = optim_res,
       diagnostics = diag_list)
}

optimize_joint_hrf_mvpa <- optimize_hrf_mvpa
