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
    dl <- list(y$diag_data)
    return(list(results = list(y),
                diagnostics = cap_diagnostics(dl)))
  }
  x$results <- c(x$results, list(y))
  new_diag <- cap_diagnostics(list(y$diag_data))
  x$diagnostics <- c(x$diagnostics, new_diag)
  x
}
