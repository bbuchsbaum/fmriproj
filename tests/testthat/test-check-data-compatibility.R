context("check_data_compatibility")

test_that("check_data_compatibility errors when event_model lacks onsets", {
  Y <- matrix(0, nrow = 5, ncol = 3)
  event_model <- list()  # missing onsets
  expect_error(
    check_data_compatibility(Y, event_model),
    "event_model missing required fields"
  )
})
