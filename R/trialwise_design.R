#' Build trial-wise design matrix
#'
#' Creates a trial-wise design matrix by convolving event onsets with an
#' HRF basis. Either a pre-computed basis matrix can be supplied or a
#' basis function with parameters.
#'
#' @param event_model List describing events. Requires `onsets` and `n_time`.
#'   By default, `onsets` are interpreted as times in seconds since the start
#'   of each run (time 0) and are converted to integer sample indices using `TR`.
#'   Alternatively, set `onset_units = 'index'` to pass 0-based integer indices
#'   directly. Optional `amplitudes` and `modulator` vectors must be the same
#'   length as `onsets`.
#' @param hrf_basis_func Optional function generating an HRF basis matrix
#'   as `hrf_basis_func(theta_params, time_vector)`.
#' @param theta_params Optional parameters passed to `hrf_basis_func`.
#' @param hrf_basis_matrix Optional precomputed HRF basis matrix
#'   (rows = time points, cols = basis functions). Overrides
#'   `hrf_basis_func` and `theta_params` if supplied.
#' @param TR Repetition time in seconds when `onset_units = 'sec'`.
#' @param onset_units Either `'sec'` (seconds, default) or `'index'` (0-based
#'   integer sample indices). If omitted and `TR` is `NULL`, strictly integer
#'   `onsets` within `[0, n_time - 1]` are treated as indices for backward
#'   compatibility.
#' @param rounding Rounding policy when converting seconds to indices; one of
#'   `'nearest'` (default), `'floor'`, or `'ceil'`.
#' @param run_id Optional integer or factor vector the same length as `onsets`,
#'   indicating the run for each event. Onsets reset to 0 per run. When provided,
#'   you may also supply `n_time_per_run` in `event_model` to validate bounds.
#' @param sparse Logical; return a sparse `dgCMatrix` if TRUE.
#' @param max_X_cols Safety threshold for number of columns.
#' @param diagnostics Logical; attach basic diagnostic information.
#'
#' @return An object of class `fr_design_matrix` containing the design
#'   matrix and metadata.
#' @aliases make_trialwise_X
#' @export
build_design_matrix <- function(event_model,
                                hrf_basis_func = NULL,
                                theta_params = NULL,
                                hrf_basis_matrix = NULL,
                                TR = NULL,
                                onset_units = c("sec", "index"),
                                rounding = c("nearest", "floor", "ceil"),
                                run_id = NULL,
                                sparse = TRUE,
                                max_X_cols = 15000,
                                diagnostics = FALSE) {
  start_time <- proc.time()["elapsed"]

  # Validate required fields
  onsets <- event_model$onsets
  if (is.null(onsets)) {
    stop("event_model must contain 'onsets'")
  }
  n_time <- event_model$n_time
  if (is.null(n_time)) {
    stop("event_model must contain 'n_time'")
  }

  # Parse onsets using improved helper (Phase 1 integration)
  onset_idx <- parse_onsets_with_fmridesign(
    onsets = onsets,
    n_time = n_time,
    TR = TR,
    onset_units = onset_units,
    rounding = rounding,
    run_id = run_id,
    n_time_per_run = event_model$n_time_per_run
  )

  amplitudes <- event_model$amplitudes
  if (is.null(amplitudes)) amplitudes <- rep(1, length(onsets))
  modulators <- event_model$modulator
  if (is.null(modulators)) modulators <- rep(1, length(onsets))

  if (length(amplitudes) != length(onsets)) {
    stop("'amplitudes' must be the same length as 'onsets'")
  }
  if (length(modulators) != length(onsets)) {
    stop("'modulator' must be the same length as 'onsets'")
  }

  if (is.null(hrf_basis_matrix)) {
    if (is.null(hrf_basis_func)) {
      stop("Provide either hrf_basis_matrix or hrf_basis_func")
    }
    # time vector length is inferred from event_model$basis_length if present
    L <- event_model$basis_length
    if (is.null(L)) L <- 30
    time_vec <- seq(0, L - 1)
    hrf_basis_matrix <- hrf_basis_func(theta_params, time_vec)
  }

  B <- as.matrix(hrf_basis_matrix)
  if (nrow(B) > n_time) {
    warning("HRF basis longer than run length - decimating")
    idx <- round(seq(1, nrow(B), length.out = n_time))
    B <- B[idx, , drop = FALSE]
  }
  L <- nrow(B)
  K <- ncol(B)

  N <- length(onset_idx)
  ncol_X <- N * K
  if (ncol_X > max_X_cols) {
    warning("ncol(X) exceeds max_X_cols")
  }

  est_nz <- N * K * L
  trip_i <- integer(est_nz)
  trip_j <- integer(est_nz)
  trip_x <- numeric(est_nz)
  idx <- 1L

  for (n in seq_len(N)) {
    onset <- onset_idx[n]
    amp <- amplitudes[n] * modulators[n]
    for (k in seq_len(K)) {
      col_index <- (n - 1) * K + (k - 1)
      rows <- onset + seq_len(L) - 1
      valid <- rows < n_time
      nv <- sum(valid)
      if (nv > 0L) {
        idx_end <- idx + nv - 1L
        trip_i[idx:idx_end] <- rows[valid]
        trip_j[idx:idx_end] <- rep(col_index, nv)
        trip_x[idx:idx_end] <- amp * B[valid, k]
        idx <- idx_end + 1L
      }
    }
  }

  final_len <- idx - 1L
  if (final_len < est_nz) {
    trip_i <- trip_i[seq_len(final_len)]
    trip_j <- trip_j[seq_len(final_len)]
    trip_x <- trip_x[seq_len(final_len)]
  }

  X_sp <- make_spmat_triplet(trip_i, trip_j, trip_x, n_time, ncol_X)
  if (sparse) {
    X <- X_sp
  } else {
    X <- as.matrix(X_sp)
  }

  diag_list <- NULL
  if (diagnostics) {
    build_time <- proc.time()["elapsed"] - start_time
    dl <- list(X_dims = dim(X_sp),
               X_sparsity = length(trip_x) / (n_time * ncol_X),
               build_time = build_time)
    diag_list <- cap_diagnostics(dl)
  }

  out <- fr_design_matrix(X, event_model = event_model,
                          hrf_info = list(basis = B))
  attr(out, "diagnostics") <- diag_list
  out
}


make_trialwise_X <- build_design_matrix
