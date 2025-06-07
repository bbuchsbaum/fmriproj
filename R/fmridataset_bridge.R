#' Create rMVPA design from an fmridataset
#'
#' Internal helper that extracts label and block information from the
#' `event_table` of an `fmridataset` and constructs an `rMVPA::mvpa_design`
#' object.
#'
#' @param fmri_dset An object of class `fmri_dataset`.
#' @param y_formula Formula selecting the label column from `event_table`.
#' @param block_formula Formula selecting the block/run column.
#'
#' @return An `mvpa_design` object.
#' @keywords internal
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

  y_vals <- model.frame(y_formula, et, drop.unused.levels = TRUE)[[1]]
  block_vals <- model.frame(block_formula, et, drop.unused.levels = TRUE)[[1]]

  rMVPA::mvpa_design(y_train = y_vals, block_var = block_vals)
}

#' Create rMVPA dataset from an fmridataset
#'
#' Internal helper that extracts the time-series `NeuroVec` and mask from an
#' `fmridataset` and constructs an `rMVPA::mvpa_dataset` object.
#'
#' @param fmri_dset An object of class `fmri_dataset`.
#'
#' @return An `mvpa_dataset` object.
#' @keywords internal
create_mvpa_dataset_from_dataset <- function(fmri_dset) {
  if (!requireNamespace("rMVPA", quietly = TRUE)) {
    stop("rMVPA package required. Install with: devtools::install_github('bbuchsbaum/rMVPA')")
  }

  Y <- fmridataset::get_data(fmri_dset)
  mask <- fmridataset::get_mask(fmri_dset)

  rMVPA::mvpa_dataset(Y, mask)
}

