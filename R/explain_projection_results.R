#' Create NIfTI map of searchlight diagnostics
#'
#' This function extracts the chosen searchlight ridge penalties (`lambda_sl`)
#' from the output of `combine_projection_diagnostics` and returns a NIfTI-like
#' array for visualization.
#'
#' @param sl_results Result object from `combine_projection_diagnostics` that
#'   contains a `diagnostics` list.
#' @param mask_dims Numeric vector giving the dimensions of the brain mask.
#'
#' @return An array of dimension `mask_dims` containing the `lambda_sl` values.
#'   In a full implementation this would return a `neuroim2` NIfTI object.
#' @export
explain_projection_results <- function(sl_results, mask_dims) {
  if (is.null(sl_results$diagnostics)) {
    stop("No diagnostics found in searchlight results")
  }
  lambda_vec <- vapply(sl_results$diagnostics,
                       function(d) d$lambda_sl,
                       numeric(1))
  array(lambda_vec, dim = mask_dims)
}
