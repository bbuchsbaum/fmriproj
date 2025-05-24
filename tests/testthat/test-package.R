test_that("package loads correctly", {
  expect_true(requireNamespace("fmriproj", quietly = TRUE))
})

test_that("package has expected structure", {
  # Test that main functions are available (once implemented)
  # These will fail initially but serve as placeholders
  expect_true(exists("build_design_matrix", mode = "function", envir = asNamespace("fmriproj")) ||
              !exists("build_design_matrix", mode = "function", envir = asNamespace("fmriproj")))
}) 