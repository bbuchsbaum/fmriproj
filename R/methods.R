#' Print methods for fmriproj objects
#'
#' Simple summaries of `fr_design_matrix` and `fr_projector` objects.
#'
#' @param x Object to print.
#' @param ... Additional arguments ignored.
#' @return Invisibly returns `x`.
#' @name print_methods
NULL

#' @rdname print_methods
#' @export
print.fr_design_matrix <- function(x, ...) {
  dims <- dim(x$X)
  cat("fr_design_matrix\n")
  cat(" - X dims:", dims[1], "x", dims[2])
  if (inherits(x$X, "dgCMatrix")) cat(" [sparse]\n") else cat(" [dense]\n")
  if (is.null(x$event_model)) {
    cat(" - event_model: none\n")
  } else {
    cat(" - event_model present\n")
  }
  invisible(x)
}

#' @rdname print_methods
#' @export
print.fr_projector <- function(x, ...) {
  cat("fr_projector\n")
  if (!is.null(x$Qt)) {
    dQt <- dim(x$Qt)
    cat(" - Qt dims:", dQt[1], "x", dQt[2], "\n")
  }
  if (!is.null(x$R)) {
    dR <- dim(x$R)
    cat(" - R dims:", dR[1], "x", dR[2], "\n")
  }
  if (is.null(x$K_global)) {
    cat(" - K_global: none\n")
  } else {
    dK <- dim(x$K_global)
    cat(" - K_global dims:", dK[1], "x", dK[2], "\n")
  }
  invisible(x)
}
