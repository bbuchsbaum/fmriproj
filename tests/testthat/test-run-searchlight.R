context("run_searchlight end-to-end")

# Skip test if required packages are not available
skip_if_not_installed("rMVPA")
skip_if_not_installed("fmridataset")

set.seed(123)
Y <- matrix(rnorm(200), nrow = 20, ncol = 10)
mask <- rep(TRUE, 10)

# simple events: 4 trials alternating conditions
events <- data.frame(
  onset = seq(1, 20, by = 5),
  condition = rep(c("A", "B"), each = 2),
  run = 1
)

dset <- fmridataset::matrix_dataset(
  Y,
  mask = mask,
  event_table = events,
  sampling_frame = data.frame(block = 1, blocklens = nrow(Y))
)

res <- run_searchlight(
  dset,
  radius = 1,
  y_formula = ~condition,
  block_formula = ~run
)

# Should return rMVPA searchlight_result object
expect_s3_class(res, "searchlight_result")

