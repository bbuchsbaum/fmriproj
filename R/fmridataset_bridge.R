#' Create rMVPA design from an fmridataset
#'
#' Internal helper that extracts label and block information from the
#' `event_table` of an `fmridataset` and constructs an `rMVPA::mvpa_design`
#' object. This function properly creates the required `train_design` data frame
#' and passes formulas (not vectors) to `mvpa_design()` so that rMVPA can
#' evaluate them in the correct context.
#'
#' @param fmri_dset An object of class `fmri_dataset` containing an `event_table`.
#' @param y_formula Formula selecting the label column from `event_table`
#'   (e.g., `~ condition` for classification or `~ response` for regression).
#' @param block_formula Formula selecting the block/run column from `event_table`
#'   (e.g., `~ run`) for blocked cross-validation.
#'
#' @return An `mvpa_design` object suitable for use with `rMVPA::mvpa_model()`.
create_mvpa_design_from_dataset <- function(fmri_dset,
                                            y_formula,
                                            block_formula) {
  if (!requireNamespace("rMVPA", quietly = TRUE)) {
    stop("rMVPA package required. Install with: devtools::install_github('bbuchsbaum/rMVPA')")
  }

  if (!inherits(y_formula, "formula")) {
    stop("y_formula must be a formula")
  }
  if (!inherits(block_formula, "formula")) {
    stop("block_formula must be a formula")
  }

  et <- fmri_dset$event_table
  if (is.null(et)) {
    stop("fmri_dset must contain an event_table")
  }

  # Extract values from event table to determine structure
  y_vals <- model.frame(y_formula, et, drop.unused.levels = TRUE)[[1]]
  block_vals <- model.frame(block_formula, et, drop.unused.levels = TRUE)[[1]]

  # Ensure block_vals is a factor for cross-validation
  if (!is.null(block_vals) && !is.factor(block_vals)) {
    block_vals <- factor(block_vals)
  }

  # Get variable names from formulas
  y_varname <- all.vars(y_formula)[1]
  block_varname <- all.vars(block_formula)[1]

  # Create train_design data frame (REQUIRED by rMVPA::mvpa_design)
  # Include row indices and the actual variables that formulas will reference
  train_design <- data.frame(.row = seq_along(y_vals))
  train_design[[y_varname]] <- y_vals
  if (!is.null(block_vals)) {
    train_design[[block_varname]] <- block_vals
  }

  # Pass formulas (not vectors) so rMVPA can evaluate them in train_design context
  rMVPA::mvpa_design(
    train_design = train_design,
    y_train = y_formula,
    block_var = if (!is.null(block_vals)) block_formula else NULL
  )
}

#' Create rMVPA dataset from an fmridataset
#'
#' Internal helper that extracts the time-series `NeuroVec` and mask from an
#' `fmridataset` and constructs an `rMVPA::mvpa_dataset` object. If the data
#' is a matrix rather than a NeuroVec, it performs conversion using neuroim2.
#'
#' **Matrix Conversion Logic:**
#' When the input is a matrix (time × voxels), this function creates a NeuroVec
#' with spatial dimensions (n_voxels, 1, 1) and time as the 4th dimension. This
#' artificial 3D structure is appropriate for:
#' - ROI-based analyses where spatial structure is already lost
#' - Matrix datasets from `fmridataset::matrix_dataset()`
#'
#' For whole-brain 3D/4D NIfTI images, use `fmridataset` objects that preserve
#' the native spatial structure, avoiding unnecessary matrix conversion.
#'
#' @param fmri_dset An object of class `fmri_dataset` containing time-series data
#'   (either as NeuroVec or matrix) and optionally a mask.
#'
#' @return An `mvpa_dataset` object suitable for use with rMVPA analysis functions.
create_mvpa_dataset_from_dataset <- function(fmri_dset) {
  if (!requireNamespace("rMVPA", quietly = TRUE)) {
    stop("rMVPA package required. Install with: devtools::install_github('bbuchsbaum/rMVPA')")
  }

  # Extract data - be tolerant of mock objects in tests
  Y <- tryCatch(
    fmridataset::get_data(fmri_dset),
    error = function(e) {
      stop(
        "Failed to extract data from fmri_dset: ", e$message,
        "\nEnsure fmri_dset is a valid fmridataset object.",
        call. = FALSE
      )
    }
  )

  # Extract mask (may be NULL)
  mask <- tryCatch(fmridataset::get_mask(fmri_dset), error = function(e) NULL)

  # If data is already NeuroVec, use it directly
  if (inherits(Y, "NeuroVec")) {
    # Validate mask matches NeuroVec spatial structure
    if (!is.null(mask) && !inherits(mask, c("NeuroVol", "NeuroSurface"))) {
      warning("Mask is not a NeuroVol/NeuroSurface; ignoring it")
      mask <- NULL
    }
    return(rMVPA::mvpa_dataset(train_data = Y, mask = mask))
  }

  # Convert matrix to NeuroVec
  if (!is.matrix(Y) && !inherits(Y, "Matrix")) {
    stop("Data must be a matrix (time × voxels) or NeuroVec, got: ", class(Y)[1],
         call. = FALSE)
  }

  if (!requireNamespace("neuroim2", quietly = TRUE)) {
    stop("neuroim2 package required to convert matrix data for rMVPA", call. = FALSE)
  }

  # Matrix dimensions: rows = time, cols = voxels
  n_time <- nrow(Y)
  n_vox <- ncol(Y)

  if (n_time < 2) {
    stop("Data must have at least 2 time points, got: ", n_time, call. = FALSE)
  }
  if (n_vox < 2) {
    stop("Data must have at least 2 voxels (rMVPA requirement), got: ", n_vox, call. = FALSE)
  }

  # Create artificial 3D spatial structure: (n_voxels, 1, 1)
  # This is appropriate for ROI data where spatial structure is already lost
  spatial_space <- neuroim2::NeuroSpace(c(n_vox, 1, 1))
  vec_space <- neuroim2::add_dim(spatial_space, n_time)

  # Reshape matrix to 4D array: (n_vox, 1, 1, n_time)
  # Note: This transposes time × voxels → voxels × time in the process
  arr <- array(t(Y), dim = c(n_vox, 1, 1, n_time))
  Y <- neuroim2::DenseNeuroVec(arr, vec_space)

  # Create or validate mask
  if (is.null(mask)) {
    # No mask provided - create one with all voxels active
    mask <- neuroim2::NeuroVol(array(1, dim = c(n_vox, 1, 1)), spatial_space)
  } else if (!inherits(mask, c("NeuroVol", "NeuroSurface"))) {
    # mask is a numeric/logical vector from matrix_dataset - convert to NeuroVol
    if (is.logical(mask)) {
      mask_vals <- as.numeric(mask)
    } else if (is.numeric(mask)) {
      mask_vals <- mask
    } else {
      warning("Mask has unexpected type: ", class(mask)[1], "; creating default mask")
      mask_vals <- rep(1, n_vox)
    }

    # Validate mask length matches number of voxels
    if (length(mask_vals) != n_vox) {
      warning("Mask length (", length(mask_vals), ") doesn't match n_voxels (",
              n_vox, "); creating default mask")
      mask_vals <- rep(1, n_vox)
    }

    # Convert to 3D array and create NeuroVol
    mask_arr <- array(mask_vals, dim = c(n_vox, 1, 1))
    mask <- neuroim2::NeuroVol(mask_arr, spatial_space)
  }
  # else: mask is already NeuroVol/NeuroSurface, use as-is

  rMVPA::mvpa_dataset(train_data = Y, mask = mask)
}
