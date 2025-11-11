context("Model factory with S3 dispatch")

# Mock rMVPA functions for testing
setup_rmvpa_mocks <- function() {
  if (!"mock_rMVPA" %in% search()) {
    mock_env <- new.env()
    
    # Mock model constructors
    mock_env$mvpa_model <- function(...) {
      args <- list(...)
      structure(args, class = c("mvpa_model", "list"))
    }
    
    mock_env$rsa_model <- function(...) {
      args <- list(...)
      structure(args, class = c("rsa_model", "mvpa_model", "list"))
    }
    
    mock_env$feature_rsa_model <- function(...) {
      args <- list(...)
      structure(args, class = c("feature_rsa_model", "mvpa_model", "list"))
    }
    
    mock_env$vector_rsa_model <- function(...) {
      args <- list(...)
      structure(args, class = c("vector_rsa_model", "mvpa_model", "list"))
    }
    
    mock_env$load_model <- function(name) {
      list(algorithm = name)
    }
    
    mock_env$mvpa_design <- function(...) {
      list(...)
    }
    
    attach(mock_env, name = "mock_rMVPA")
  }
}

teardown_rmvpa_mocks <- function() {
  if ("mock_rMVPA" %in% search()) {
    detach("mock_rMVPA")
  }
}

test_that("model_spec creates proper specifications", {
  # Classification spec
  spec1 <- model_spec(
    type = "classification",
    data = matrix(1:20, 10, 2),
    labels = rep(c("A", "B"), 5)
  )
  
  expect_s3_class(spec1, "classification")
  expect_s3_class(spec1, "model_spec")
  expect_equal(spec1$type, "classification")
  expect_equal(dim(spec1$data), c(10, 2))
  
  # RSA spec
  spec2 <- model_spec(
    type = "rsa",
    data = matrix(1:20, 10, 2),
    distance_method = "euclidean"
  )
  
  expect_s3_class(spec2, "rsa")
  expect_equal(spec2$distance_method, "euclidean")
})

test_that("create_projected_model dispatches correctly", {
  setup_rmvpa_mocks()
  on.exit(teardown_rmvpa_mocks())
  
  # Test data
  X <- matrix(rnorm(50), 10, 5)
  P <- matrix(rnorm(10), 5, 2)
  
  # Classification model
  spec <- model_spec(
    type = "classification",
    data = X,
    labels = rep(c("A", "B"), 5),
    algorithm = "lda"
  )

  # Use proper namespace mocking
  local_mocked_bindings(
    build_rmvpa_model = function(type, data, ...) {
      # Verify proxy was created
      expect_s3_class(data, "ProjectedDataSource")
      expect_equal(data$nrow, 10)
      expect_equal(data$ncol, 2)

      list(type = type, data_class = class(data)[1], ...)
    },
    .package = "fmriproj"
  )

  result <- create_projected_model(spec, P)

  expect_equal(result$type, "classification")
  expect_equal(result$data_class, "ProjectedDataSource")
})

test_that("create_projected_model handles list specs", {
  X <- matrix(rnorm(50), 10, 5)
  P <- matrix(rnorm(10), 5, 2)

  # List without class
  spec <- list(
    type = "regression",
    data = X,
    response = rnorm(10)
  )

  # Use proper namespace mocking
  local_mocked_bindings(
    build_rmvpa_model = function(type, data, ...) {
      result <- list(type = type, data_class = class(data)[1], ...)
      class(result) <- c(type, "model_spec", "list")
      result
    },
    .package = "fmriproj"
  )

  # Should add class and dispatch
  expect_s3_class(
    create_projected_model.default(spec, P),
    "regression"
  )
})

test_that("create_projection_proxy handles different projection types", {
  X <- matrix(rnorm(50), 10, 5)
  
  # Matrix projection
  P <- matrix(rnorm(10), 5, 2)
  proxy1 <- create_projection_proxy(X, P)
  expect_s3_class(proxy1, "ProjectedDataSource")
  expect_equal(proxy1$ncol, 2)
  
  # Function projection
  proj_fun <- function(data) data[, 1:2]
  proxy2 <- create_projection_proxy(X, proj_fun)
  expect_s3_class(proxy2, "ProjectedDataSource")
  
  # projection_spec projection
  proj_spec <- projection_spec(
    event_model = list(onsets = 1:10),
    projector_components = list(dummy = TRUE),
    N_trials = 10,
    K_hrf = 3,
    lambda_adaptive_method = "EB",
    lambda_global = 0.1,
    collapse_method = "rss"
  )
  
  # Mock the projection functions
  orig_arp <- adaptive_ridge_projector
  orig_cb <- collapse_beta
  
  adaptive_ridge_projector <<- function(...) {
    list(Z_sl_raw = matrix(rnorm(30), 10, 3))
  }
  
  collapse_beta <<- function(...) {
    list(A_sl = matrix(rnorm(20), 10, 2))
  }
  
  proxy3 <- create_projection_proxy(X, proj_spec)
  expect_s3_class(proxy3, "ProjectedDataSource")
  
  # Restore
  adaptive_ridge_projector <<- orig_arp
  collapse_beta <<- orig_cb
})

test_that("S3 dispatch works for all model types", {
  X <- matrix(rnorm(50), 10, 5)
  P <- matrix(rnorm(10), 5, 2)
  
  # Test each model type has a method
  model_types <- c("classification", "regression", "rsa", 
                   "feature_rsa", "vector_rsa", "contrast_rsa")
  
  for (type in model_types) {
    spec <- structure(list(type = type, data = X), class = c(type, "list"))
    
    # Check method exists
    method_name <- paste0("create_projected_model.", type)
    expect_true(
      exists(method_name),
      info = paste("Method", method_name, "should exist")
    )
  }
})

test_that("extract_model_metadata works correctly", {
  # With metadata
  spec1 <- list(
    feature_names = c("F1", "F2", "F3"),
    attributes = list(version = 1),
    design = "test_design"
  )
  
  meta1 <- extract_model_metadata(spec1)
  expect_equal(meta1$feature_names, c("F1", "F2", "F3"))
  expect_equal(meta1$attributes$version, 1)
  
  # Without metadata
  meta2 <- extract_model_metadata(NULL)
  expect_null(meta2)
  
  # Empty spec
  meta3 <- extract_model_metadata(list())
  expect_equal(names(meta3), c("feature_names", "attributes", "design"))
  expect_null(meta3$feature_names)
})

test_that("create_mvpa_model_projected provides convenience wrapper", {
  setup_rmvpa_mocks()
  on.exit(teardown_rmvpa_mocks())

  X <- matrix(rnorm(50), 10, 5)
  P <- matrix(rnorm(10), 5, 2)

  # Use proper namespace mocking
  local_mocked_bindings(
    create_projected_model = function(spec, projection) {
      list(created_with = "projection", spec = spec, projection = projection)
    },
    build_rmvpa_model = function(...) {
      list(created_with = "no_projection", ...)
    },
    .package = "fmriproj"
  )

  # With projection
  result1 <- create_mvpa_model_projected(
    "classification",
    dataset = X,
    projection = P,
    labels = rep(c("A", "B"), 5)
  )

  expect_equal(result1$created_with, "projection")

  # Without projection
  result2 <- create_mvpa_model_projected(
    "classification",
    dataset = X,
    labels = rep(c("A", "B"), 5)
  )

  expect_equal(result2$created_with, "no_projection")
})