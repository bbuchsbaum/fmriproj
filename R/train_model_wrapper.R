#' Train an rMVPA model with optional progressive projection
#'
#' This method wraps `rMVPA::train_model` for objects of class `mvpa_model`.
#' If the training data matrix has an attribute named `"projection_function"`,
#' the function stored in that attribute is applied to the matrix before
#' delegating to `rMVPA`.
#'
#' @param obj An `mvpa_model` object from the `rMVPA` package.
#' @param X Training data matrix. If missing, the argument is passed through
#'   to the underlying `rMVPA` method unchanged.
#' @param ... Additional arguments passed to `rMVPA::train_model`.
#' @return The fitted model as returned by `rMVPA::train_model`.
#' @export
train_model.mvpa_model <- function(obj, X, ...) {
  if (!requireNamespace("rMVPA", quietly = TRUE)) {
    stop("rMVPA package required. Install with: devtools::install_github('bbuchsbaum/rMVPA')")
  }

  if (!missing(X) && !is.null(attr(X, "projection_function"))) {
    proj_fun <- attr(X, "projection_function")
    X <- proj_fun(X)
  }

  orig <- getS3method("train_model", "mvpa_model", envir = asNamespace("rMVPA"))
  orig(obj, X, ...)
}
