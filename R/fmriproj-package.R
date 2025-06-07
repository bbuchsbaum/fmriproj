#' @keywords internal
"_PACKAGE"

#' @import fmrireg
#' @import rMVPA
#' @import Matrix
#' @importFrom dplyr %>%
#' @importFrom tibble tibble
#' @importFrom assertthat assert_that
#' @importFrom purrr map
#' @importFrom glmnet glmnet
#' @importFrom RSpectra svds
#' @importFrom foreach foreach %dopar%
#' @importFrom doParallel registerDoParallel
#' @useDynLib fmriproj, .registration = TRUE
#' @importFrom Rcpp evalCpp
NULL
