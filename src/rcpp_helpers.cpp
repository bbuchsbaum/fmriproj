#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::sp_mat triplet_to_spmat_cpp(const arma::uvec& i,
                                  const arma::uvec& j,
                                  const arma::vec& x,
                                  unsigned int nrow,
                                  unsigned int ncol) {
  arma::sp_mat out(i, j, x, nrow, ncol);
  return out;
}

// [[Rcpp::export]]
arma::mat spmat_dense_prod_cpp(const arma::sp_mat& A,
                               const arma::mat& B) {
  return arma::mat(A * B);
}
