context("recommend_chunk_size")

test_that("recommend_chunk_size respects bounds and messages", {
  # Set small memory limit so chunk size floored to 100
  expect_message(cs <- recommend_chunk_size(100, 1000, 50, memory_limit_gb = 0.0001),
                 "Recommended chunk size")
  expect_equal(cs, 100)

  # Large memory should cap at n_voxels
  cs2 <- recommend_chunk_size(10, 200, 5, memory_limit_gb = 1000)
  expect_equal(cs2, 200)
})
