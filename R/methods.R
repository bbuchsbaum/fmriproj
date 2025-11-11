#' Print methods for fmriproj objects
#'
#' Simple summaries of `fr_design_matrix` and `fr_projector` objects.
#'
#' @param x Object to print.
#' @param quiet Logical; suppress messages if `TRUE`.
#' @param ... Additional arguments ignored.
#' @return Invisibly returns `x`.
#' @name print_methods
NULL

#' @rdname print_methods
#' @export
print.fr_design_matrix <- function(x, quiet = FALSE, ...) {
  dims <- dim(x$X)
  lines <- c(
    "fr_design_matrix",
    sprintf(" - X dims: %d x %d%s",
            dims[1], dims[2],
            if (inherits(x$X, 'dgCMatrix')) " [sparse]" else " [dense]"),
    if (is.null(x$event_model)) " - event_model: none" else " - event_model present"
  )
  if (!quiet) cat(paste(lines, collapse = "\n"), "\n", sep = "")
  invisible(x)
}

#' @rdname print_methods
#' @export
print.fr_projector <- function(x, quiet = FALSE, ...) {
  lines <- "fr_projector"
  
  # Check internal representation
  if (!is.null(x$.internal_representation)) {
    lines <- c(lines, sprintf(" - Representation: %s", x$.internal_representation))
  }
  
  # New style with qr_obj
  if (!is.null(x$qr_obj)) {
    # Handle both regular qr and sparseQR objects
    qr_dims <- dim(x$qr_obj)
    if (inherits(x$qr_obj, "sparseQR")) {
      # sparseQR doesn't have $rank directly
      lines <- c(lines, sprintf(" - Original dims: %d x %d", 
                               qr_dims[1], qr_dims[2]))
    } else {
      # Regular qr object
      lines <- c(lines, sprintf(" - QR rank: %d", x$qr_obj$rank))
      lines <- c(lines, sprintf(" - Original dims: %d x %d", 
                               qr_dims[1], x$qr_obj$rank))
    }
  }
  
  # Legacy style with Qt
  if (!is.null(x$Qt)) {
    dQt <- dim(x$Qt)
    lines <- c(lines, sprintf(" - Qt dims: %d x %d", dQt[1], dQt[2]))
  }
  
  if (!is.null(x$R)) {
    dR <- dim(x$R)
    lines <- c(lines, sprintf(" - R dims: %d x %d", dR[1], dR[2]))
  }
  
  if (!is.null(x$lambda_global)) {
    lines <- c(lines, sprintf(" - Lambda: %.4f", x$lambda_global))
  }
  
  if (is.null(x$K_global)) {
    lines <- c(lines, " - K_global: none")
  } else {
    dK <- dim(x$K_global)
    lines <- c(lines, sprintf(" - K_global dims: %d x %d", dK[1], dK[2]))
  }
  
  if (!quiet) cat(paste(lines, collapse = "\n"), "\n", sep = "")
  invisible(x)
}
