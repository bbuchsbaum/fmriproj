context("chunked projection dataset")

test_that("wrap_as_chunked_projecting_dataset caches results", {
  Y <- matrix(seq_len(40), nrow = 4) # 4 timepoints x 10 voxels
  counter <- 0
  proj_fun <- function(data) {
    counter <<- counter + 1
    data + 1
  }
  ds <- wrap_as_chunked_projecting_dataset(
    Y, proj_fun,
    original_dataset = list(nfeatures = ncol(Y)),
    chunk_size = 5, cache_results = TRUE
  )

  full1 <- ds$get_data()
  expect_equal(counter, ceiling(ncol(Y) / 5))
  expect_equal(full1, Y + 1)

  counter_before <- counter
  sub1 <- ds$get_data(indices = 1:10)
  expect_equal(counter, counter_before)
  expect_equal(sub1, full1[, 1:10])

  ds$clear_cache()
  sub2 <- ds$get_data(indices = 1:5)
  expect_equal(counter, counter_before + 1)
  expect_equal(sub2, full1[, 1:5])
})
