context("print methods")

test_that("print.fr_design_matrix outputs summary", {
  em <- list(onsets = c(0L, 2L), n_time = 6L)
  basis <- matrix(c(1,0,0,
                    0,1,0), nrow = 3, byrow = FALSE)
  dm <- build_design_matrix(em, hrf_basis_matrix = basis)
  expect_snapshot_output(print(dm))
})

test_that("print.fr_projector outputs summary", {
  em <- list(onsets = c(0L,2L), n_time = 6L)
  basis <- matrix(c(1,0,0,
                    0,1,0), nrow = 3, byrow = FALSE)
  dm <- build_design_matrix(em, hrf_basis_matrix = basis)
  proj <- build_projector(dm$X)
  expect_snapshot_output(print(proj))
})
