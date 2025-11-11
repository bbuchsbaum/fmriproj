context("train_model.mvpa_model")

mvpa_model_obj <- structure(list(dummy = TRUE), class = "mvpa_model")
X <- matrix(rnorm(20), ncol = 4)
proj_fun <- function(m) m * 2


test_that("train_model.mvpa_model errors when rMVPA missing", {
  old_opts <- options(fmriproj.requireNamespace = function(pkg, quietly = TRUE) FALSE)
  on.exit(options(old_opts), add = TRUE)
  expect_error(train_model.mvpa_model(mvpa_model_obj, X), "rMVPA package required")
})


test_that("projection_function attribute is applied when rMVPA present", {
  fake_env <- new.env()
  X_seen <- NULL
  fake_env$train_model.mvpa_model <- function(obj, X, ...) { X_seen <<- X; list(ok = TRUE) }

  old_opts <- options(
    fmriproj.requireNamespace = function(pkg, quietly = TRUE) TRUE,
    fmriproj.asNamespace = function(pkg) fake_env,
    fmriproj.getS3method = function(fn, class, envir = fake_env) fake_env$train_model.mvpa_model
  )
  on.exit(options(old_opts), add = TRUE)

  attr(X, "projection_function") <- proj_fun
  result <- train_model.mvpa_model(mvpa_model_obj, X)

  expect_equal(X_seen, proj_fun(X))
  expect_true(result$ok)
})
