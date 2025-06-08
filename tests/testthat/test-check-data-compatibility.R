context("check_data_compatibility")

test_that("check_data_compatibility errors when event_model lacks onsets", {
  Y <- matrix(0, nrow = 5, ncol = 3)
  event_model <- list()  # missing onsets
  expect_error(
    check_data_compatibility(Y, event_model),
    "event_model missing required fields"
  )
})

test_that("check_data_compatibility validates Y input and timing", {
  Y <- matrix(0, nrow = 4, ncol = 2)
  em_ok <- list(onsets = c(0, 3))

  # Y must be a matrix
  expect_error(check_data_compatibility(1:4, em_ok), "Y must be a matrix")

  # Last onset cannot exceed scan duration
  em_bad <- list(onsets = c(0, 10))
  expect_error(
    check_data_compatibility(Y, em_bad, TR = 2),
    "exceeds scan duration"
  )

  # Closely spaced trials trigger a warning
  expect_warning(
    check_data_compatibility(Y, em_ok, TR = 2),
    "Trials may be too close"
  )
})

test_that("quiet suppresses messages", {
  Y <- matrix(0, nrow = 4, ncol = 2)
  em <- list(onsets = c(0, 1))
  expect_message(check_data_compatibility(Y, em, quiet = FALSE), "Data dimensions")
  expect_message(check_data_compatibility(Y, em, quiet = TRUE), regexp = NA)
})
