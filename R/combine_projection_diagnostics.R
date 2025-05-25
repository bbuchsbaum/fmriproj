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
  diag_val <- cap_diagnostics(list(y$diag_data))
  if (is.null(x)) {
    return(list(results = list(y),
                diagnostics = list(diag_val)))
  }

  x$results[[length(x$results) + 1]] <- y
  new_diag <- cap_diagnostics(list(y$diag_data))
  if (!is.null(new_diag)) {
    x$diagnostics[[length(x$diagnostics) + 1]] <- diag_val
  }

  x
}
