#' fmriproj: Projected Multivariate Pattern Analysis for fMRI Data
#'
#' This package implements a three-layer framework for analyzing fMRI data:
#' \enumerate{
#'   \item Trial-wise design matrix construction with optimizable HRF parameters
#'   \item Adaptive ridge projection for dimensionality reduction  
#'   \item Beta coefficient collapse strategies
#' }
#'
#' The package interfaces with 'fmrireg' for experimental design modeling and 
#' 'rMVPA' for multivariate pattern analysis execution.
#'
#' @section Main Functions:
#' \describe{
#'   \item{\code{make_trialwise_X}}{Constructs trial-wise design matrices}
#'   \item{\code{adaptive_ridge_projector}}{Performs adaptive ridge projection}
#'   \item{\code{collapse_beta}}{Implements beta coefficient collapse strategies}
#'   \item{\code{mvpa_projected_searchlight}}{Runs projected MVPA searchlight analysis}
#'   \item{\code{optimize_joint_hrf_mvpa}}{Optimizes HRF parameters for MVPA performance}
#' }
#'
#' @docType package
#' @name fmriproj-package
#' @aliases fmriproj
#' @import fmrireg
#' @import rMVPA
#' @importFrom Matrix Matrix
#' @importFrom dplyr %>%
#' @importFrom tibble tibble
#' @importFrom assertthat assert_that
#' @importFrom purrr map
#' @importFrom glmnet glmnet
#' @importFrom RSpectra svds
#' @importFrom foreach foreach %dopar%
#' @importFrom doParallel registerDoParallel
NULL 