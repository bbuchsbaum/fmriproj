#' Combine projection diagnostics from searchlight runs
#'
#' Simple helper intended for use as the `.combine` function in
#' `rMVPA::searchlight`. It collects the searchlight-specific diagnostic
#' information (currently `lambda_sl` and `w_sl`) from each call.
#'
#' @param x Accumulated results.
#' @param y New searchlight result to combine.
#'
#' @return A list with `results` (all searchlight outputs) and `diagnostics`,
#'   a list of per-searchlight diagnostic entries.
#' @export
combine_projection_diagnostics <- function(x, y) {
  if (is.null(x)) {
    return(list(results = list(y),
                diagnostics = list(y$diag_data)))
  }
  x$results <- c(x$results, list(y))
  x$diagnostics <- c(x$diagnostics, list(y$diag_data))
  x
}
