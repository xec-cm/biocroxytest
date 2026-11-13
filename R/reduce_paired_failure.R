#' Reduce a failure in paired experimental data
#'
#' Pilot reduction for a SummarizedExperiment with exactly two conditions
#' and one sample per condition in every pair. Remove assay rows and whole
#' pairs while keeping the original order. Subsetting uses the container's
#' methods and retains its assay backend.
#'
#' The design must be an additive, one-sided formula with the pair and
#' condition as main effects. Additional plain numeric or categorical
#' covariates are supported. Each candidate retains all observed levels
#' of non-pair categorical covariates, a full-rank design matrix and
#' positive residual degrees of freedom. The matrix uses the current
#' R contrast options. Pair and condition labels must be characters or
#' factors, and the pair factor must be unordered. Unused pair levels
#' are dropped when pairs are removed. Drop unused factor levels before
#' supplying the original object. Pair factors use the current contrast
#' options and cannot have custom contrasts.
#' Include any extra constraints, such as a minimum number of pairs per
#' stratum, in the checker.
#'
#' The checker should run the operation and return a stable identifier
#' for the specific failure, or `NULL` if it does not occur. Use different
#' identifiers for different failures. An unexpected error in the checker
#' stops reduction. The checker should be repeatable and free of side
#' effects. The same base R seed is used for each check. Set any
#' backend-specific seed in the checker if needed.
#'
#' The original and the final subset are checked explicitly. The search is
#' greedy and bounded; it may leave removable rows or pairs. Review the
#' reduced object before using it as an ordinary test fixture.
#'
#' @param x A [SummarizedExperiment::SummarizedExperiment] with paired samples.
#' @param pair,condition Names of columns in the sample metadata.
#' @param design One-sided additive formula using columns in sample metadata.
#' @param check Function taking one SummarizedExperiment and returning a
#'   nonempty character string identifying a failure, or `NULL`.
#' @param seed Integer seed for the checker.
#' @param max_checks Maximum number of checker calls, including the original
#'   and final checks. At least two.
#' @return A list containing `data`, the checked subset, `rows` and `columns`
#'   (indices in the original object), `failure` (the matched identifier),
#'   `checks` (the number of checker calls) and `budget_exhausted`.
#' @export
#' @examples
#' if (requireNamespace("SummarizedExperiment", quietly = TRUE)) {
#'   counts <- matrix(1:24, nrow = 4)
#'   samples <- data.frame(
#'     subject = rep(letters[1:3], each = 2),
#'     visit = rep(c("before", "after"), times = 3)
#'   )
#'   x <- SummarizedExperiment::SummarizedExperiment(
#'     assays = list(counts = counts), colData = samples
#'   )
#'   assert_counts <- function(data) {
#'     if (any(SummarizedExperiment::assay(data) == 16L)) {
#'       stop(errorCondition("Sentinel count rejected", class = "sentinel_count"))
#'     }
#'   }
#'   check <- function(data) {
#'     tryCatch({
#'       assert_counts(data)
#'       NULL
#'     }, sentinel_count = function(cnd) "sentinel count rejected")
#'   }
#'   reduced <- reduce_paired_failure(x, "subject", "visit", ~ subject + visit, check)
#'   check(reduced$data)
#' }
reduce_paired_failure <- function(x, pair, condition, design, check,
                                  seed = 1L, max_checks = 100L) {
  if (!requireNamespace("SummarizedExperiment", quietly = TRUE)) {
    cli::cli_abort("Install SummarizedExperiment to reduce a paired failure.", call = NULL)
  }
  if (!inherits(x, "SummarizedExperiment") || nrow(x) == 0L) {
    cli::cli_abort("Supply a SummarizedExperiment with at least one row.", call = NULL)
  }
  if (!is.function(check) || !valid_scalar_integer(seed) ||
      !valid_scalar_integer(max_checks) || max_checks < 2L) {
    cli::cli_abort("Supply a checker function, an integer seed and at least two checks.", call = NULL)
  }

  pairing <- paired_reduction_design(x, pair, condition, design)
  groups <- pairing$groups
  state <- new.env(parent = emptyenv())
  state$checks <- 0L
  run_check <- function(data) {
    state$checks <- state$checks + 1L
    failure <- withr::with_seed(
      seed, check(data),
      .rng_kind = "Mersenne-Twister", .rng_normal_kind = "Inversion",
      .rng_sample_kind = "Rejection"
    )
    if (!is.null(failure) && (!is.character(failure) || length(failure) != 1L ||
        !is.null(dim(failure)) || is.na(failure) || !nzchar(trimws(failure)))) {
      cli::cli_abort("The checker must return one failure identifier or NULL.", call = NULL)
    }
    failure
  }

  original <- run_check(x)
  if (is.null(original)) {
    cli::cli_abort("The checker did not reproduce a failure on the original data.", call = NULL)
  }

  selection <- list(rows = seq_len(nrow(x)), pairs = unique(groups))
  subset <- function(rows, columns) subset_paired_data(x, rows, columns, pair)
  available <- function() state$checks < max_checks - 1L
  repeat {
    previous <- selection
    for (axis in names(selection)) {
      selection <- reduce_paired_axis(selection, axis, available, function(candidate) {
        columns <- which(groups %in% candidate$pairs)
        data <- subset(candidate$rows, columns)
        if (axis == "pairs" && !valid_paired_design(
          paired_sample_data(data, pairing$variables), design, pairing$levels
        )) {
          return(FALSE)
        }
        identical(run_check(data), original)
      })
    }
    if (identical(selection, previous) || !available()) {
      break
    }
  }

  columns <- which(groups %in% selection$pairs)
  reduced <- subset(selection$rows, columns)
  exhausted <- !available()
  if (!identical(run_check(reduced), original)) {
    cli::cli_abort("The final subset did not reproduce the original failure.", call = NULL)
  }
  list(
    data = reduced, rows = selection$rows, columns = columns,
    failure = original, checks = state$checks, budget_exhausted = exhausted
  )
}

subset_paired_data <- function(x, rows, columns, pair) {
  data <- x[rows, columns]
  if (length(columns) < ncol(x)) {
    samples <- SummarizedExperiment::colData(data)
    if (is.factor(samples[[pair]])) {
      samples[[pair]] <- droplevels(samples[[pair]])
      SummarizedExperiment::colData(data) <- samples
    }
  }
  data
}

paired_reduction_design <- function(x, pair, condition, design) {
  metadata_names <- names(SummarizedExperiment::colData(x))
  columns <- c(pair, condition)
  if (!is.character(pair) || length(pair) != 1L ||
      !is.character(condition) || length(condition) != 1L ||
      anyNA(columns) || anyDuplicated(columns) || !all(columns %in% metadata_names)) {
    cli::cli_abort("Identify two distinct sample metadata columns for pair and condition.", call = NULL)
  }
  if (!inherits(design, "formula") || length(design) != 2L ||
      !is_additive_design(design[[2L]])) {
    cli::cli_abort("Use an additive formula with plain metadata columns for pair and condition.", call = NULL)
  }
  variables <- all.vars(design)
  if (!all(columns %in% variables) || !all(variables %in% metadata_names)) {
    cli::cli_abort("The design must include pair and condition and use only metadata columns.", call = NULL)
  }
  if (anyDuplicated(metadata_names)) {
    cli::cli_abort("Sample metadata must have unique column names.", call = NULL)
  }

  samples_df <- paired_sample_data(x, variables)
  if (!(is.character(samples_df[[pair]]) ||
        (is.factor(samples_df[[pair]]) && !is.ordered(samples_df[[pair]])))) {
    cli::cli_abort("Pair labels must be characters or an unordered factor.", call = NULL)
  }
  if (!is.null(attr(samples_df[[pair]], "contrasts"))) {
    cli::cli_abort("Use the default contrast coding for the pair factor.", call = NULL)
  }
  if (!(is.character(samples_df[[condition]]) || is.factor(samples_df[[condition]]))) {
    cli::cli_abort("Condition labels must be characters or a factor.", call = NULL)
  }

  for (variable in variables) {
    value <- samples_df[[variable]]
    if (!is.null(dim(value)) || anyNA(value) || anyNA(levels(value)) ||
        !(is.character(value) || is.factor(value) || is.logical(value) ||
          (is.numeric(value) && all(is.finite(value))))) {
      cli::cli_abort("Unsupported or missing design values in {.val {variable}}.", call = NULL)
    }
    if (is.factor(value) && !all(levels(value) %in% value)) {
      cli::cli_abort("Drop unused factor levels in {.val {variable}} before reduction.", call = NULL)
    }
  }
  groups <- as.character(samples_df[[pair]])
  counts <- table(groups, samples_df[[condition]])
  if (ncol(counts) != 2L || !all(counts == 1L) || sum(counts) != nrow(samples_df)) {
    cli::cli_abort("Each pair must have one sample in each of two conditions.", call = NULL)
  }

  categorical <- vapply(samples_df, function(value) {
    is.factor(value) || is.character(value) || is.logical(value)
  }, logical(1))
  categorical[pair] <- FALSE
  levels_list <- lapply(samples_df[categorical], function(value) {
    if (is.factor(value)) levels(value) else unique(value)
  })
  if (!valid_paired_design(samples_df, design, levels_list)) {
    cli::cli_abort("The original design must be full rank with positive residual degrees of freedom.", call = NULL)
  }
  list(groups = groups, levels = levels_list, variables = variables)
}

paired_sample_data <- function(x, variables) {
  samples <- SummarizedExperiment::colData(x)
  data_df <- data.frame(row.names = seq_len(ncol(x)))
  for (variable in variables) {
    data_df[[variable]] <- samples[[variable]]
  }
  data_df
}

is_additive_design <- function(term) {
  is.symbol(term) && !identical(term, quote(.)) ||
    (is.call(term) && identical(term[[1L]], as.name("+")) && length(term) == 3L &&
       is_additive_design(term[[2L]]) && is_additive_design(term[[3L]]))
}

valid_paired_design <- function(samples_df, design, levels_list) {
  for (name in names(levels_list)) {
    if (!all(levels_list[[name]] %in% samples_df[[name]])) {
      return(FALSE)
    }
  }

  model <- tryCatch(stats::model.matrix(design, samples_df), error = function(cnd) NULL)
  !is.null(model) && all(is.finite(model)) && nrow(model) == nrow(samples_df) &&
    nrow(model) > ncol(model) && qr(model)$rank == ncol(model)
}

reduce_paired_axis <- function(selection, axis, available, accepts) {
  block <- max(1L, ceiling(length(selection[[axis]]) / 2))
  while (length(selection[[axis]]) > 1L && available()) {
    values <- selection[[axis]]
    block <- min(block, length(values) - 1L)
    accepted <- FALSE
    for (start in seq.int(1L, length(values), by = block)) {
      if (!available()) {
        break
      }
      remove <- seq.int(start, min(start + block - 1L, length(values)))
      candidate <- selection
      candidate[[axis]] <- values[-remove]
      if (accepts(candidate)) {
        selection <- candidate
        accepted <- TRUE
        break
      }
    }
    if (!accepted) {
      if (block == 1L) {
        break
      }
      block <- max(1L, floor(block / 2))
    }
  }
  selection
}
