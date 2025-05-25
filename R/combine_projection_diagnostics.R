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
  x$results[[length(x$results) + 1]] <- y
  new_diag <- cap_diagnostics(list(y$diag_data))
  if (!is.null(new_diag)) {
    x$diagnostics[[length(x$diagnostics) + 1]] <- new_diag[[1]]
  }
  x
}
