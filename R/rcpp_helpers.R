#' Create a sparse matrix from triplet representation
#'
#' Wrapper around the C++ function `triplet_to_spmat_cpp()` which constructs
#' an Armadillo sparse matrix from triplet indices.
#'
#' @note Both `i` and `j` use zero-based indexing.
#'
#' @param i Integer vector of row indices (0-based).
#' @param j Integer vector of column indices (0-based).
#' @param x Numeric vector of values.
#' @param nrow Number of rows in the resulting matrix.
#' @param ncol Number of columns in the resulting matrix.
#' @return A `dgCMatrix` representing the sparse matrix.
#'
#' @examples
#' i <- c(0L, 1L)
#' j <- c(0L, 1L)
#' x <- c(1, 2)
#' make_spmat_triplet(i, j, x, 2L, 2L)
#' @export
make_spmat_triplet <- function(i, j, x, nrow, ncol) {
  stopifnot(length(i) == length(j), length(i) == length(x))
  if (anyNA(i) || anyNA(j) || anyNA(x))
    stop("Indices and values must not contain NA")
  if (any(i < 0) || any(j < 0))
    stop("Indices must be non-negative")
  if (any(i >= nrow) || any(j >= ncol))
    stop("Indices out of range")

  triplet_to_spmat_cpp(as.integer(i), as.integer(j), as.numeric(x),
                       as.integer(nrow), as.integer(ncol))
}

#' Sparse matrix - dense matrix product
#'
#' Efficient multiplication of a sparse matrix with a dense matrix.
#'
#' @param A Sparse matrix (`dgCMatrix`).
#' @param B Dense matrix.
#' @return Dense product matrix `A %*% B`.
#'
#' @examples
#' sm <- make_spmat_triplet(c(0L, 1L), c(0L, 1L), c(1, 2), 2L, 2L)
#' dense <- matrix(1, 2, 2)
#' spmat_dense_prod(sm, dense)
#' @export
spmat_dense_prod <- function(A, B) {
  if (!inherits(A, "dgCMatrix"))
    stop("A must be a 'dgCMatrix'")
  if (!is.matrix(B))
    stop("B must be a matrix")
  if (ncol(A) != nrow(B))
    stop("Non-conformable matrices: ncol(A) != nrow(B)")

  spmat_dense_prod_cpp(A, B)
}
