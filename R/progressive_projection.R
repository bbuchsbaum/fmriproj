#' Progressive Projection Pursuit (fit)
#'
#' Fits a simple supervised dimensionality reduction model to the
#' searchlight trial patterns. Currently supports Linear Discriminant
#' Analysis ("LDA") and a rudimentary Partial Least Squares variant
#' ("PLS-DA").
#'
#' @param A_sl_train Matrix of size `N_train x V_sl` containing trial
#'   patterns.
#' @param labels_train Vector of class labels for each row of
#'   `A_sl_train`.
#' @param method Reduction method. Either "LDA" or "PLS-DA".
#' @param dims Number of projection dimensions to return.
#' @param tol Numeric tolerance added to the within-class scatter
#'   matrix when computing the projection for LDA. Increasing this
#'   value can help when the scatter matrix is nearly singular.
#' @return An object with elements `W` (projection matrix) and
#'   `method`.
#' @export
fit_pp <- function(A_sl_train, labels_train, method = "LDA", dims = 2,
                   tol = 1e-6) {
  if (!method %in% c("LDA", "PLS-DA")) {
    stop("method must be 'LDA' or 'PLS-DA'")
  }

  if (length(unique(labels_train)) < 2) {
    return(list(W = diag(ncol(A_sl_train)), method = method))
  }

  if (method == "LDA") {
    classes <- unique(labels_train)
    V <- ncol(A_sl_train)
    overall_mean <- colMeans(A_sl_train)
    S_W <- matrix(0, V, V)
    S_B <- matrix(0, V, V)
    for (cl in classes) {
      Xc <- A_sl_train[labels_train == cl, , drop = FALSE]
      m_c <- colMeans(Xc)
      S_W <- S_W + crossprod(scale(Xc, center = m_c, scale = FALSE))
      diff <- m_c - overall_mean
      S_B <- S_B + nrow(Xc) * tcrossprod(diff)
    }
    mat <- tryCatch(solve(S_W + diag(tol, V), S_B), error = function(e) NULL)
    if (is.null(mat)) {
      W <- diag(ncol(A_sl_train))
    } else {
      eig <- eigen(mat)
      idx <- order(Re(eig$values), decreasing = TRUE)
      W <- Re(eig$vectors[, idx[seq_len(min(dims, ncol(eig$vectors)))]])
    }
  } else { # PLS-DA
    Y_ind <- model.matrix(~ labels_train - 1)
    cov_mat <- crossprod(A_sl_train, Y_ind)
    sv <- svd(cov_mat)
    W <- sv$u[, seq_len(min(dims, ncol(sv$u))), drop = FALSE]
  }
  list(W = W, method = method)
}

#' Apply progressive projection to new data
#'
#' @param pp_model Object returned by `fit_pp`.
#' @param A_sl_new New trial pattern matrix.
#' @return Matrix of projected data.
#' @export
predict_pp <- function(pp_model, A_sl_new) {
  if (is.null(pp_model$W) || !is.matrix(pp_model$W)) {
    stop("pp_model must contain a matrix component 'W'")
  }
  if (!is.numeric(A_sl_new)) {
    stop("A_sl_new must be numeric")
  }
  if (ncol(A_sl_new) != nrow(pp_model$W)) {
    stop("Non-conformable matrices: ncol(A_sl_new) = ", ncol(A_sl_new),
         " but nrow(pp_model$W) = ", nrow(pp_model$W))
  }
  (A_sl_new %*% pp_model$W)[, , drop = FALSE]
}

