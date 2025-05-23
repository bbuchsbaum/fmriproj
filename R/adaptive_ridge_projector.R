#' Adaptive Ridge Projection for a Searchlight
#'
#' Computes a ridge-regularized projector for a searchlight's BOLD data using
#' components from `build_projector()`. Only `lambda_adaptive_method = "none"`
#' is currently implemented.
#'
#' @param Y_sl Matrix of BOLD data for the searchlight (T x V_sl).
#' @param projector_components Object returned by `build_projector()`.
#' @param lambda_adaptive_method Method for choosing searchlight-specific lambda.
#'   Defaults to "none" which simply uses `lambda_floor_global`.
#' @param lambda_floor_global Minimum ridge penalty to apply.
#' @param diagnostics Logical; return diagnostic information.
#' @return A list with elements:
#'   \item{Z_sl_raw}{Projected coefficients ((N*K) x V_sl).}
#'   \item{diag_data}{List of diagnostic information if requested.}
#' @export
adaptive_ridge_projector <- function(Y_sl,
                                     projector_components,
                                     lambda_adaptive_method = "none",
                                     lambda_floor_global = 0,
                                     diagnostics = FALSE) {
  Qt <- projector_components$Qt
  R <- projector_components$R

  if (lambda_adaptive_method == "none") {
    lambda_eff <- lambda_floor_global
  } else {
    stop("Only lambda_adaptive_method = 'none' is implemented")
  }

  m <- ncol(R)
  RtR <- crossprod(R)
  K_sl <- solve(RtR + diag(lambda_eff, m), t(R) %*% Qt)

  Z_sl_raw <- K_sl %*% Y_sl

  diag_list <- NULL
  if (diagnostics) {
    diag_list <- list(lambda_sl_chosen = lambda_eff)
  }

  list(Z_sl_raw = Z_sl_raw, diag_data = diag_list)
}
