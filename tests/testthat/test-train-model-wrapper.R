context("train_model.mvpa_model")

mvpa_model_obj <- structure(list(dummy = TRUE), class = "mvpa_model")
X <- matrix(rnorm(20), ncol = 4)
proj_fun <- function(m) m * 2


test_that("train_model.mvpa_model errors when rMVPA missing", {
  if ("rMVPA" %in% loadedNamespaces()) detach("package:rMVPA", unload = TRUE, character.only = TRUE)
  expect_error(train_model.mvpa_model(mvpa_model_obj, X), "rMVPA package required")
})


test_that("projection_function attribute is applied when rMVPA present", {
  skip_if_not_installed("methods") # ensure base environment

  fake_env <- new.env()
  X_seen <- NULL
  fake_env$train_model.mvpa_model <- function(obj, X, ...) { X_seen <<- X; list(ok = TRUE) }

  env <- environment(train_model.mvpa_model)
  orig_require <- env$requireNamespace
  orig_asNamespace <- env$asNamespace
  orig_getS3method <- env$getS3method

  env$requireNamespace <- function(pkg, quietly = TRUE) TRUE
  env$asNamespace <- function(pkg) fake_env
  env$getS3method <- function(fn, class, envir = fake_env) fake_env$train_model.mvpa_model

  attr(X, "projection_function") <- proj_fun
  result <- train_model.mvpa_model(mvpa_model_obj, X)

  env$requireNamespace <- orig_require
  env$asNamespace <- orig_asNamespace
  env$getS3method <- orig_getS3method

  expect_equal(X_seen, proj_fun(X))
  expect_true(result$ok)
})
