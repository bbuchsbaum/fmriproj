#' Project trials from time-series data
#'
#' \strong{Deprecated}. The high-level [`run_regional()`] function now
#' provides a more flexible interface for projecting trial patterns from
#' time-series data. `project_trials()` remains as a thin helper around the
#' core projection steps but will be removed in a future release.
#'
#' A user-friendly wrapper that handles the complete projection pipeline
#' in one function call. This is the simplest way to get trial patterns
#' from fMRI time-series data.
#'
#' @param Y Time-series data matrix (time x voxels)
#' @param event_model Event model from fmrireg or a list with onsets
#' @param lambda_method Method for adaptive regularization: "none", "EB", or "CV"
#' @param collapse_method Method for combining HRF bases: "rss", "pc", or "optim"
#' @param hrf_basis Optional custom HRF basis matrix
#' @param verbose Print progress messages
#' @return Matrix of trial patterns (trials x voxels)
#' @export
#' @deprecated Use `run_regional()` for single-ROI analyses.
#' @examples
#' # Simple usage
#' trial_patterns <- project_trials(bold_data, event_model)
#' 
#' # With adaptive regularization
#' trial_patterns <- project_trials(bold_data, event_model, 
#'                                  lambda_method = "EB")
project_trials <- function(Y,
                          event_model,
                          lambda_method = c("EB", "none", "CV"),
                          collapse_method = c("rss", "pc", "optim"),
                          hrf_basis = NULL,
                          verbose = TRUE) {

  .Deprecated("run_regional")
  
  lambda_method <- match.arg(lambda_method)
  collapse_method <- match.arg(collapse_method)
  
  if (verbose) message("Building design matrix...")
  
  # Step 1: Design matrix
  design <- build_design_matrix(
    event_model = event_model,
    hrf_basis_matrix = hrf_basis
  )
  
  if (verbose) message("Creating projector...")
  
  # Step 2: Projector
  lambda_global <- switch(lambda_method,
                         none = 0,
                         EB = 0.1,
                         CV = 0.1)
  
  projector <- build_projector(
    X_theta = design$X,
    lambda_global = lambda_global
  )
  
  if (verbose) message("Projecting data...")
  
  # Step 3: Project
  X_dense <- if (lambda_method %in% c("EB", "CV")) {
    as.matrix(design$X)
  } else NULL
  
  proj_result <- adaptive_ridge_projector(
    Y_sl = Y,
    projector_components = projector,
    lambda_adaptive_method = lambda_method,
    lambda_floor_global = lambda_global,
    X_theta_for_EB_residuals = X_dense
  )
  
  if (verbose) message("Collapsing HRF components...")
  
  # Step 4: Collapse
  n_trials <- length(event_model$onsets)
  k_bases <- ncol(design$hrf_info$basis)
  
  collapsed <- collapse_beta(
    Z_sl_raw = proj_result$Z_sl_raw,
    N_trials = n_trials,
    K_hrf_bases = k_bases,
    method = collapse_method
  )
  
  if (verbose) message("Done!")
  
  return(collapsed$A_sl)
}


#' Run MVPA searchlight analysis with projection
#'
#' @deprecated Use `run_searchlight()` with an `fmridataset`.
#'
#' High-level wrapper that combines projection with rMVPA searchlight analysis.
#' Handles all the integration details automatically.
#'
#' @param Y Time-series data (time x voxels)
#' @param event_model Event model with trial timing
#' @param mask Brain mask defining searchlight centers
#' @param radius Searchlight radius in voxels
#' @param classifier Name of classifier from MVPAModels (e.g., "sda_notune")
#' @param cross_validation Cross-validation scheme (rMVPA object)
#' @param projection_opts List of projection options
#' @param n_cores Number of parallel cores to use
#' @return Searchlight results with performance maps
#' @export
mvpa_searchlight <- function(Y,
                            event_model,
                            mask,
                            radius = 3,
                            classifier = "sda_notune",
                            cross_validation = NULL,
                            projection_opts = list(),
                            n_cores = 1) {
  .Deprecated("run_searchlight")
  
  # Check dependencies
  if (!requireNamespace("rMVPA", quietly = TRUE)) {
    stop("rMVPA package required. Install with: devtools::install_github('bbuchsbaum/rMVPA')")
  }
  
  # Set default projection options
  proj_defaults <- list(
    lambda_adaptive_method = "EB",
    collapse_method = "rss",
    use_progressive_projection = FALSE,
    pp_dims = NULL
  )
  
  projection_opts <- modifyList(proj_defaults, projection_opts)
  
  # Create rMVPA dataset
  dataset <- rMVPA::mvpa_dataset(Y, mask)
  
  # Create design
  design <- rMVPA::mvpa_design(
    y_train = event_model$conditions,
    block_var = event_model$blocks
  )
  
  # Set up cross-validation if not provided
  if (is.null(cross_validation)) {
    if (!is.null(event_model$blocks)) {
      cross_validation <- rMVPA::blocked_cross_validation(event_model$blocks)
    } else {
      cross_validation <- rMVPA::kfold_cross_validation(
        length(event_model$conditions), 
        nfolds = 5
      )
    }
  }
  
  # Create model
  model <- rMVPA::mvpa_model(
    model = rMVPA::load_model(classifier),
    dataset = dataset,
    design = design,
    crossval = cross_validation
  )
  
  # Add progressive projection if requested
  if (projection_opts$use_progressive_projection && !is.null(projection_opts$pp_dims)) {
    model$feature_selector <- pp_feature_selector(
      method = "LDA",
      dims = projection_opts$pp_dims
    )
  }
  
  # Run searchlight with projection
  run_searchlight_projected(
    model_spec = model,
    radius = radius,
    method = "standard",
    projection_opts = projection_opts,
    .cores = n_cores
  )
}


#' Check data and event model compatibility
#'
#' Diagnostic function to verify your data and events are properly formatted
#' before running analyses.
#'
#' @param Y Time-series data
#' @param event_model Event model
#' @param TR Repetition time in seconds
#' @return Invisible TRUE if all checks pass, otherwise errors/warnings
#' @export
check_data_compatibility <- function(Y, event_model, TR = NULL) {
  
  # Check dimensions
  if (!is.matrix(Y) && !inherits(Y, "Matrix")) {
    stop("Y must be a matrix (time x voxels)")
  }
  
  n_time <- nrow(Y)
  n_voxels <- ncol(Y)
  
  cat("Data dimensions:\n")
  cat("  Time points:", n_time, "\n")
  cat("  Voxels:", n_voxels, "\n")
  
  # Check event model
  if (!is.list(event_model)) {
    stop("event_model must be a list")
  }
  
  required_fields <- c("onsets")
  missing <- setdiff(required_fields, names(event_model))
  if (length(missing) > 0) {
    stop("event_model missing required fields: ", paste(missing, collapse = ", "))
  }
  
  n_trials <- length(event_model$onsets)
  cat("\nEvent model:\n")
  cat("  Trials:", n_trials, "\n")
  
  # Check timing
  if (!is.null(TR)) {
    max_onset <- max(event_model$onsets)
    scan_duration <- n_time * TR
    
    if (max_onset >= scan_duration) {
      stop("Latest trial onset (", max_onset, "s) exceeds scan duration (", 
           scan_duration, "s)")
    }
    
    # Check trial spacing
    trial_spacing <- diff(sort(event_model$onsets))
    min_spacing <- min(trial_spacing)
    
    cat("  Min trial spacing:", round(min_spacing, 1), "s\n")
    
    if (min_spacing < 2 * TR) {
      warning("Trials may be too close together for reliable separation")
    }
  }
  
  # Check conditions if provided
  if (!is.null(event_model$conditions)) {
    n_conditions <- length(unique(event_model$conditions))
    cat("  Conditions:", n_conditions, "\n")
    print(table(event_model$conditions))
  }
  
  # Check for common issues
  if (n_trials > n_time / 2) {
    warning("Very high trial density - consider longer runs")
  }
  
  if (n_voxels < 100) {
    warning("Very few voxels - is this an ROI analysis?")
  }
  
  # Test basic projection
  cat("\nTesting projection pipeline...")
  
  test_result <- tryCatch({
    small_test <- project_trials(
      Y[, 1:min(100, n_voxels)], 
      event_model,
      verbose = FALSE
    )
    cat(" SUCCESS\n")
    cat("  Output dimensions:", dim(test_result), "\n")
    TRUE
  }, error = function(e) {
    cat(" FAILED\n")
    cat("  Error:", e$message, "\n")
    FALSE
  })
  
  invisible(test_result)
}


#' Plot searchlight diagnostics
#'
#' Visualize diagnostic information for a specific searchlight to understand
#' the projection process.
#'
#' @param results Results object with diagnostics
#' @param voxel Central voxel index
#' @param plot_type Type of plot: "weights", "lambda", "patterns"
#' @export
plot_searchlight_diagnostics <- function(results, 
                                       voxel, 
                                       plot_type = c("weights", "lambda", "patterns")) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 required for plotting")
  }
  
  plot_type <- match.arg(plot_type)
  
  # Extract diagnostics for this searchlight
  sl_idx <- get_searchlight_index(results, voxel)
  diag <- results$diagnostics[[sl_idx]]
  
  if (is.null(diag)) {
    stop("No diagnostics available. Run with diagnostics = TRUE")
  }
  
  if (plot_type == "weights") {
    # Plot HRF collapse weights
    weights_df <- data.frame(
      basis = factor(1:length(diag$w_sl)),
      weight = diag$w_sl
    )
    
    p <- ggplot2::ggplot(weights_df, ggplot2::aes(x = basis, y = weight)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::labs(title = "HRF Collapse Weights",
                    x = "Basis Function",
                    y = "Weight") +
      ggplot2::theme_minimal()
    
  } else if (plot_type == "lambda") {
    # Show adaptive lambda in context
    all_lambdas <- sapply(results$diagnostics, function(d) d$lambda_sl)
    
    p <- ggplot2::ggplot(data.frame(lambda = all_lambdas), 
                         ggplot2::aes(x = lambda)) +
      ggplot2::geom_histogram(bins = 30) +
      ggplot2::geom_vline(xintercept = diag$lambda_sl, 
                         color = "red", linewidth = 2) +
      ggplot2::scale_x_log10() +
      ggplot2::labs(title = "Adaptive Lambda Distribution",
                    subtitle = "Red line = this searchlight",
                    x = "Lambda (log scale)",
                    y = "Count") +
      ggplot2::theme_minimal()
    
  } else if (plot_type == "patterns") {
    # Visualization would need actual pattern data
    stop("Pattern plotting not yet implemented")
  }
  
  return(p)
}