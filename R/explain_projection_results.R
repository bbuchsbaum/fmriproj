#' Create NIfTI map of searchlight diagnostics
#'
#' This function extracts the chosen searchlight ridge penalties (`lambda_sl`)
#' and optionally the collapse weights (`w_sl`) from the output of
#' `combine_projection_diagnostics` and returns simple NIfTI-like arrays for
#' visualisation. If the HRF basis matrix used during projection is provided,
#' an "effective HRF" (basis %*% mean(`w_sl`)) is also returned.
#'
#' @param sl_results Result object from `combine_projection_diagnostics` that
#'   contains a `diagnostics` list.
#' @param mask_dims Numeric vector giving the dimensions of the brain mask.
#' @param hrf_basis_matrix Optional HRF basis matrix used for computing an
#'   effective HRF from the average `w_sl`.
#'
#' @return A list with `lambda_map`, `w_maps` (or `NULL` if unavailable) and an
#'   optional `effective_hrf` vector. In a full implementation these would be
#'   `neuroim2` objects.
#' @export
explain_projection_results <- function(sl_results, mask_dims, hrf_basis_matrix = NULL) {
  if (is.null(sl_results$diagnostics)) {
    stop("No diagnostics found in searchlight results")
  }
  lambda_vec <- vapply(sl_results$diagnostics,
                       function(d) d$lambda_sl,
                       numeric(1))
  lambda_map <- array(lambda_vec, dim = mask_dims)

  w_list <- lapply(sl_results$diagnostics, function(d) d$w_sl)
  w_maps <- NULL
  effective_hrf <- NULL
  if (!any(vapply(w_list, is.null, logical(1)))) {
    K <- length(w_list[[1]])
    w_mat <- vapply(w_list, identity, numeric(K))
    w_maps <- vector("list", K)
    for (k in seq_len(K)) {
      w_maps[[k]] <- array(w_mat[k, ], dim = mask_dims)
    }
    if (!is.null(hrf_basis_matrix)) {
      w_mean <- rowMeans(w_mat)
      stopifnot(ncol(hrf_basis_matrix) == length(w_mean))
      effective_hrf <- as.numeric(hrf_basis_matrix %*% w_mean)
    }
  }

  list(lambda_map = lambda_map, w_maps = w_maps, effective_hrf = effective_hrf)
}
