#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::sp_mat triplet_to_spmat_cpp(const arma::uvec& i,
                                  const arma::uvec& j,
                                  const arma::vec& x,
                                  unsigned int nrow,
                                  unsigned int ncol) {
  // Create locations matrix: 2 x n_vals, where first row is i, second is j
  arma::umat locations(2, i.n_elem);
  locations.row(0) = i.t();
  locations.row(1) = j.t();
  
  arma::sp_mat out(locations, x, nrow, ncol);
  return out;
}

// [[Rcpp::export]]
arma::mat spmat_dense_prod_cpp(const arma::sp_mat& A,
                               const arma::mat& B) {
  return arma::mat(A * B);
}
