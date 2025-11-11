context("class checking helpers")

test_that("is.fr_design_matrix identifies objects", {
  dm <- fr_design_matrix(matrix(1, nrow = 1, ncol = 1))
  expect_true(is.fr_design_matrix(dm))
  expect_false(is.fr_design_matrix(list()))
})

test_that("is.fr_projector identifies objects", {
  # Use legacy format with explicit argument names
  proj <- suppressWarnings(fr_projector(Qt = matrix(1, 1, 1), R = matrix(1, 1, 1)))
  expect_true(is.fr_projector(proj))
  expect_false(is.fr_projector(list()))
})
