library(testthat)

# Tests for fr_design_matrix input validation

mat <- matrix(0, nrow = 2, ncol = 2)

# X must be matrix or dgCMatrix
expect_error(fr_design_matrix(1), "X must be a base matrix or Matrix::dgCMatrix")

# event_model must be list or fmrireg_event_model
expect_error(fr_design_matrix(mat, event_model = 1), "event_model must be a list or fmrireg_event_model")

# hrf_info must be list
expect_error(fr_design_matrix(mat, hrf_info = 1), "hrf_info must be a list")
