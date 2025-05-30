// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// triplet_to_spmat_cpp
arma::sp_mat triplet_to_spmat_cpp(const arma::uvec& i, const arma::uvec& j, const arma::vec& x, unsigned int nrow, unsigned int ncol);
RcppExport SEXP _fmriproj_triplet_to_spmat_cpp(SEXP iSEXP, SEXP jSEXP, SEXP xSEXP, SEXP nrowSEXP, SEXP ncolSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::uvec& >::type i(iSEXP);
    Rcpp::traits::input_parameter< const arma::uvec& >::type j(jSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type x(xSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type nrow(nrowSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type ncol(ncolSEXP);
    rcpp_result_gen = Rcpp::wrap(triplet_to_spmat_cpp(i, j, x, nrow, ncol));
    return rcpp_result_gen;
END_RCPP
}
// spmat_dense_prod_cpp
arma::mat spmat_dense_prod_cpp(const arma::sp_mat& A, const arma::mat& B);
RcppExport SEXP _fmriproj_spmat_dense_prod_cpp(SEXP ASEXP, SEXP BSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::sp_mat& >::type A(ASEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type B(BSEXP);
    rcpp_result_gen = Rcpp::wrap(spmat_dense_prod_cpp(A, B));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_fmriproj_triplet_to_spmat_cpp", (DL_FUNC) &_fmriproj_triplet_to_spmat_cpp, 5},
    {"_fmriproj_spmat_dense_prod_cpp", (DL_FUNC) &_fmriproj_spmat_dense_prod_cpp, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_fmriproj(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
