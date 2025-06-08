cap_diagnostics <- function(diag_list, memory_limit = getOption("fmriproj.diagnostics_memory_limit", 1e7)) {
  if (is.null(diag_list)) return(NULL)
  if (object.size(diag_list) > memory_limit) {
    warning("Diagnostics exceed memory limit - discarding")
    return(NULL)
  }
  diag_list
}

#' Get index of searchlight diagnostics for a voxel
#'
#' Searchlight result objects produced by fmriproj store per-searchlight
#' diagnostic information in `results$diagnostics`. The optional
#' `voxel_indices` vector specifies which voxel belongs to each entry in
#' that list. When `voxel_indices` is absent, diagnostics are assumed to
#' be ordered by voxel index.
#'
#' @param results Searchlight results containing a `diagnostics` list and
#'   optionally a `voxel_indices` vector identifying the central voxel of
#'   each searchlight.
#' @param voxel Voxel index whose diagnostics should be retrieved.
#' @return Integer index into `results$diagnostics` corresponding to the
#'   requested voxel.
#' @export
get_searchlight_index <- function(results, voxel) {
  if (!is.null(results$voxel_indices)) {
    idx <- match(voxel, results$voxel_indices)
    if (is.na(idx)) {
      stop("Voxel not found in results$voxel_indices")
    }
    return(idx)
  }
  idx <- as.integer(voxel)
  if (idx < 1 || idx > length(results$diagnostics)) {
    stop("Voxel index out of range for diagnostics")
  }
  idx
}
