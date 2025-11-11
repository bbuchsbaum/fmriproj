#' Create rMVPA Model with Projection Support
#'
#' Main entry point for creating rMVPA models with transparent projection.
#' Uses S3 dispatch to handle different model types elegantly.
#'
#' @param model_spec A model specification object or list with class indicating type
#' @param projection Projection matrix or projection specification
#' @param ... Additional arguments passed to model constructors
#' @return An rMVPA model object with projection support
#' @export
#' @examples
#' \dontrun{
#' # Create a classification model spec
#' spec <- model_spec(
#'   type = "classification",
#'   algorithm = "sda",
#'   data = my_data,
#'   labels = my_labels
#' )
#' 
#' # Create projected model
#' model <- create_projected_model(spec, projection_matrix)
#' }
create_projected_model <- function(model_spec, projection, ...) {
  UseMethod("create_projected_model")
}

#' Default method for create_projected_model
#' @export
create_projected_model.default <- function(model_spec, projection, ...) {
  # If model_spec is a list, try to determine type from contents
  if (is.list(model_spec) && !is.null(model_spec$type)) {
    class(model_spec) <- c(model_spec$type, class(model_spec))
    return(create_projected_model(model_spec, projection, ...))
  }
  
  stop("Don't know how to create projected model for object of class ", 
       paste(class(model_spec), collapse = ", "))
}

#' Create projected classification model
#' @export
create_projected_model.classification <- function(model_spec, projection, ...) {
  # Wrap data in ProjectedDataSource
  data_proxy <- create_projection_proxy(model_spec$data, projection, model_spec)
  
  # Build model with proxy
  build_rmvpa_model(
    type = "classification",
    algorithm = model_spec$algorithm,
    data = data_proxy,
    labels = model_spec$labels,
    ...
  )
}

#' Create projected regression model
#' @export
create_projected_model.regression <- function(model_spec, projection, ...) {
  data_proxy <- create_projection_proxy(model_spec$data, projection, model_spec)
  
  build_rmvpa_model(
    type = "regression",
    algorithm = model_spec$algorithm,
    data = data_proxy,
    response = model_spec$response,
    ...
  )
}

#' Create projected RSA model
#' @export
create_projected_model.rsa <- function(model_spec, projection, ...) {
  data_proxy <- create_projection_proxy(model_spec$data, projection, model_spec)

  build_rmvpa_model(
    type = "rsa",
    data = data_proxy,
    design = model_spec$design,
    distmethod = model_spec$distance_method %||% "pearson",
    ...
  )
}

#' Create projected feature RSA model
#' @export
create_projected_model.feature_rsa <- function(model_spec, projection, ...) {
  data_proxy <- create_projection_proxy(model_spec$data, projection, model_spec)
  design <- ensure_feature_rsa_design(model_spec)
  
  build_rmvpa_model(
    type = "feature_rsa",
    data = data_proxy,
    features = model_spec$features,
    design = design,
    ...
  )
}

#' Create projected vector RSA model
#' @export
create_projected_model.vector_rsa <- function(model_spec, projection, ...) {
  data_proxy <- create_projection_proxy(model_spec$data, projection, model_spec)
  
  build_rmvpa_model(
    type = "vector_rsa",
    data = data_proxy,
    contrasts = model_spec$contrasts,
    design = model_spec$design,
    ...
  )
}

#' Create projected contrast RSA model
#' @export  
create_projected_model.contrast_rsa <- function(model_spec, projection, ...) {
  data_proxy <- create_projection_proxy(model_spec$data, projection, model_spec)
  
  build_rmvpa_model(
    type = "contrast_rsa",
    data = data_proxy,
    contrasts = model_spec$contrasts,
    design = model_spec$design,
    ...
  )
}

#' Create projection proxy from data and projection
#'
#' Helper function to create appropriate ProjectedDataSource based on inputs
#'
#' @param data Original data
#' @param projection Projection matrix, function, or specification
#' @param model_spec Full model specification for metadata extraction
#' @return A ProjectedDataSource object
#' @keywords internal
create_projection_proxy <- function(data, projection, model_spec = NULL) {
  # Validate data
  if (!is.matrix(data) && !inherits(data, "Matrix")) {
    stop("Data must be a matrix or Matrix object")
  }
  
  # Validate projection compatibility
  if (is.matrix(projection)) {
    if (ncol(data) != nrow(projection)) {
      stop("Incompatible dimensions: data has ", ncol(data), 
           " columns but projection has ", nrow(projection), " rows")
    }
  }
  
  # Handle different projection types
  if (inherits(projection, "projection_spec")) {
    # fmriproj projection specification
    proj_fun <- function(Y) {
      result <- adaptive_ridge_projector(
        Y,
        projection$projector_components,
        lambda_adaptive_method = projection$lambda_adaptive_method,
        lambda_floor_global = projection$lambda_global
      )
      
      collapsed <- collapse_beta(
        result$Z_sl_raw,
        projection$N_trials,
        projection$K_hrf,
        method = projection$collapse_method,
        align_sign = projection$align_sign %||% TRUE,
        sign_anchor = projection$sign_anchor %||% "canonical"
      )
      
      collapsed$A_sl
    }
    
    ProjectedDataSource$new(
      data = data,
      projection = proj_fun,
      metadata = list(
        feature_names = paste0("Proj", seq_len(projection$N_trials)),
        attributes = list(projection_spec = projection)
      )
    )
    
  } else if (is.matrix(projection)) {
    # Simple matrix projection
    ProjectedDataSource$new(
      data = data,
      projection = projection,
      metadata = extract_model_metadata(model_spec)
    )
    
  } else if (is.function(projection)) {
    # Function-based projection
    ProjectedDataSource$new(
      data = data,
      projection = projection,
      metadata = extract_model_metadata(model_spec)
    )
    
  } else {
    stop("projection must be a matrix, function, or projection_spec")
  }
}

#' Extract metadata from model specification
#' @keywords internal
extract_model_metadata <- function(model_spec) {
  if (is.null(model_spec)) return(NULL)
  
  list(
    feature_names = model_spec$feature_names,
    attributes = model_spec$attributes,
    design = model_spec$design
  )
}

#' Ensure a feature RSA design is available
#'
#' If a design was not supplied in the model specification, automatically create
#' one using rMVPA's `feature_rsa_design()` helper. Requires labels plus either
#' a feature matrix (`features`) or a similarity matrix (`similarity_matrix`/`S`).
#'
#' @keywords internal
ensure_feature_rsa_design <- function(model_spec) {
  if (!is.null(model_spec$design)) {
    return(model_spec$design)
  }
  
  if (!requireNamespace("rMVPA", quietly = TRUE)) {
    stop("rMVPA package required. Install with: devtools::install_github('bbuchsbaum/rMVPA')")
  }
  
  labels <- model_spec$labels
  if (is.null(labels)) {
    stop("Feature RSA specifications must include `labels` when no `design` is provided.")
  }
  
  similarity_matrix <- model_spec$similarity_matrix %||% model_spec$S
  feature_matrix <- model_spec$features
  if (is.null(similarity_matrix) && is.null(feature_matrix)) {
    stop("Feature RSA specifications require either `features` or `similarity_matrix` unless `design` is supplied.")
  }
  
  design_args <- list(
    S = similarity_matrix,
    F = feature_matrix,
    labels = labels,
    block_var = model_spec$blocks,
    k = model_spec$k,
    max_comps = model_spec$max_comps
  )
  design_args <- design_args[!vapply(design_args, is.null, logical(1))]
  
  do.call(rMVPA::feature_rsa_design, design_args)
}

#' Wrap ProjectedDataSource as mvpa_dataset
#'
#' Ensures ProjectedDataSource objects are properly wrapped with mvpa_dataset class
#' for compatibility with rMVPA functions.
#'
#' @param data Either a ProjectedDataSource or existing mvpa_dataset
#' @return Object with mvpa_dataset class
#' @keywords internal
wrap_dataset_for_rmvpa <- function(data) {
  # If already has mvpa_dataset class, return as-is
  if (inherits(data, "mvpa_dataset")) {
    return(data)
  }

  # If it's a ProjectedDataSource, wrap it
  if (inherits(data, "ProjectedDataSource")) {
    structure(
      list(
        data = data,
        mask = default_projection_mask(data)
      ),
      class = c("projected_mvpa_dataset", "mvpa_dataset")
    )
  } else {
    # For other types, assume it's already compatible or wrap minimally
    data
  }
}

#' Create a minimal mask for projected datasets
#'
#' Feature RSA models in rMVPA inspect the dataset mask to ensure more than one
#' voxel is available. Projected data sources do not carry spatial information,
#' so we synthesize a simple mask whose second dimension matches the projected
#' feature count (or a sensible fallback when unknown).
#'
#' @keywords internal
default_projection_mask <- function(data_source) {
  n_features <- data_source$ncol
  if (is.null(n_features) || is.na(n_features) || n_features < 2) {
    original_cols <- tryCatch(
      ncol(data_source$original_data),
      error = function(...) NULL
    )
    n_features <- original_cols %||% 2L
  }
  n_features <- max(2L, as.integer(n_features))
  array(TRUE, dim = c(1L, n_features, 1L))
}

#' Build rMVPA model using appropriate constructor
#'
#' Internal function that calls the actual rMVPA constructors
#'
#' @param type Model type
#' @param ... Arguments for the constructor
#' @return rMVPA model object
#' @keywords internal
build_rmvpa_model <- function(type, ...) {
  if (!requireNamespace("rMVPA", quietly = TRUE)) {
    stop("rMVPA package required. Install with: devtools::install_github('bbuchsbaum/rMVPA')")
  }
  
  args <- list(...)
  
  switch(type,
    classification = {
      algorithm <- args$algorithm %||% "sda_notune"
      dataset <- wrap_dataset_for_rmvpa(args$data)
      labels <- args$labels
      crossval <- args$crossval
      feature_selector <- args$feature_selector
      design <- create_mvpa_design(labels, args$blocks)

      # Provide default crossval if not specified
      if (is.null(crossval)) {
        if (!is.null(args$blocks)) {
          crossval <- rMVPA::blocked_cross_validation(args$blocks)
        } else {
          # Default to 5-fold cross-validation if no blocks
          crossval <- rMVPA::kfold_cross_validation(length(labels), nfolds = 5)
        }
      }

      call_args <- list(
        model = rMVPA::load_model(algorithm),
        dataset = dataset,
        design = design,
        model_type = "classification",
        crossval = crossval,
        feature_selector = feature_selector
      )
      call_args <- call_args[!vapply(call_args, is.null, logical(1))]
      do.call(rMVPA::mvpa_model, call_args)
    },
    
    regression = {
      algorithm <- args$algorithm %||% "svr"
      dataset <- wrap_dataset_for_rmvpa(args$data)
      response <- args$response
      crossval <- args$crossval
      feature_selector <- args$feature_selector
      design <- create_mvpa_design(response, args$blocks)

      # Provide default crossval if not specified
      if (is.null(crossval)) {
        if (!is.null(args$blocks)) {
          crossval <- rMVPA::blocked_cross_validation(args$blocks)
        } else {
          # Default to 5-fold cross-validation if no blocks
          crossval <- rMVPA::kfold_cross_validation(length(response), nfolds = 5)
        }
      }

      call_args <- list(
        model = rMVPA::load_model(algorithm),
        dataset = dataset,
        design = design,
        model_type = "regression",
        crossval = crossval,
        feature_selector = feature_selector
      )
      call_args <- call_args[!vapply(call_args, is.null, logical(1))]
      do.call(rMVPA::mvpa_model, call_args)
    },
    
    rsa = {
      args$data <- wrap_dataset_for_rmvpa(args$data)
      do.call(rMVPA::rsa_model, args)
    },
    
    feature_rsa = {
      args$data <- wrap_dataset_for_rmvpa(args$data)
      do.call(rMVPA::feature_rsa_model, args)
    },
    
    vector_rsa = {
      args$data <- wrap_dataset_for_rmvpa(args$data)
      do.call(rMVPA::vector_rsa_model, args)
    },
    
    contrast_rsa = {
      args$data <- wrap_dataset_for_rmvpa(args$data)
      do.call(rMVPA::contrast_rsa_model, args)
    },
    
    stop("Unknown model type: ", type)
  )
}

#' Create MVPA design from labels or response
#' @keywords internal
create_mvpa_design <- function(y, blocks = NULL) {
  # If rMVPA is unavailable, return a minimal structure for downstream stubs
  if (!requireNamespace("rMVPA", quietly = TRUE)) {
    return(list(y_train = y, block_var = blocks))
  }

  n <- length(y)
  train_design <- tibble::tibble(.row = seq_len(n))
  rMVPA::mvpa_design(train_design = train_design,
                     y_train = y,
                     block_var = blocks)
}

#' Create model specification
#'
#' Helper function to create properly structured model specifications
#'
#' @param type Model type (classification, regression, rsa, etc.)
#' @param ... Model-specific parameters
#' @return A model specification object with appropriate class
#' @export
model_spec <- function(type = c("classification", "regression", "rsa", 
                               "feature_rsa", "vector_rsa", "contrast_rsa"),
                      ...) {
  type <- match.arg(type)
  
  spec <- list(type = type, ...)
  class(spec) <- c(type, "model_spec", "list")
  spec
}
