cap_diagnostics <- function(diag_list, memory_limit = getOption("fmriproj.diagnostics_memory_limit", 1e7)) {
  if (is.null(diag_list)) return(NULL)
  if (object.size(diag_list) > memory_limit) {
    warning("Diagnostics exceed memory limit - discarding")
    return(NULL)
  }
  diag_list
}
