paired_reduction_fixture <- function() {
  subjects <- paste0("s", seq_len(6L))
  samples_df <- data.frame(
    subject = factor(rep(subjects, times = 2L)),
    visit = factor(rep(c("before", "after"), each = 6L), levels = c("before", "after")),
    batch = rep(rep(c("a", "b"), each = 3L), times = 2L),
    row.names = paste0("sample", seq_len(12L))
  )
  counts <- matrix(
    seq_len(96L), nrow = 8L,
    dimnames = list(paste0("gene", seq_len(8L)), rownames(samples_df))
  )
  SummarizedExperiment::SummarizedExperiment(
    assays = list(counts = counts, doubled = 2L * counts), colData = samples_df
  )
}

test_that("reduction retains whole pairs and the original failure", {
  skip_if_not_installed("SummarizedExperiment")
  x <- paired_reduction_fixture()
  design <- ~ subject + visit
  count_sum <- function(data) {
    counts <- SummarizedExperiment::assay(data, "counts")
    sum(counts[counts < 96L])
  }
  check <- function(data) {
    if (!"s6" %in% data$subject) {
      return("different failure")
    }
    if (count_sum(data) != sum(SummarizedExperiment::assay(data, "counts"))) {
      return("original failure")
    }
    NULL
  }
  reduced <- reduce_paired_failure(x, "subject", "visit", design, check)

  expect_identical(reduced$failure, "original failure")
  expect_identical(reduced$rows, 8L)
  expect_identical(dim(reduced$data), c(1L, 4L))
  expect_true(all(table(reduced$data$subject, reduced$data$visit) == 1L))
  expect_identical(rownames(reduced$data), rownames(x)[reduced$rows])
  expect_identical(colnames(reduced$data), colnames(x)[reduced$columns])
  expect_identical(SummarizedExperiment::assay(reduced$data, "doubled"),
                   SummarizedExperiment::assay(x, "doubled")[reduced$rows, reduced$columns, drop = FALSE])
  model <- stats::model.matrix(design, as.data.frame(SummarizedExperiment::colData(reduced$data)))
  expect_identical(qr(model)$rank, ncol(model))
  expect_gt(nrow(model), ncol(model))
  expect_identical(check(reduced$data), check(x))
  expect_identical(dim(x), c(8L, 12L))

  expected <- sum(SummarizedExperiment::assay(reduced$data, "counts"))
  expect_failure(expect_identical(count_sum(reduced$data), expected))
  count_sum <- function(data) sum(SummarizedExperiment::assay(data, "counts"))
  expect_identical(count_sum(reduced$data), expected)
})

test_that("invalid designs stop before the checker is called", {
  skip_if_not_installed("SummarizedExperiment")
  x <- paired_reduction_fixture()
  check <- function(data) stop("checker must not run")
  expect_error(reduce_paired_failure(x[, -1L], "subject", "visit", ~ subject + visit, check),
               "one sample in each")
  expect_error(reduce_paired_failure(x, "subject", "visit", ~ subject + visit * batch, check),
               "additive formula")
  expect_error(reduce_paired_failure(x, "subject", "visit", ~ subject + visit + batch, check),
               "full rank")
  with_unused <- x
  with_unused$subject <- factor(x$subject, levels = c("unused", levels(x$subject)))
  expect_error(reduce_paired_failure(with_unused, "subject", "visit", ~ subject + visit, check, max_checks = 2L),
               "Drop unused factor levels")
  with_na_level <- x
  labels <- as.character(x$subject)
  labels[1L] <- NA_character_
  with_na_level$subject <- factor(labels, exclude = NULL)
  expect_error(reduce_paired_failure(with_na_level, "subject", "visit", ~ subject + visit, check),
               "missing design values")
  stats::contrasts(x$subject) <- stats::contr.sum(nlevels(x$subject))
  expect_error(reduce_paired_failure(x, "subject", "visit", ~ subject + visit, check),
               "default contrast coding")
})

test_that("reduction keeps covariate levels and skips invalid candidates", {
  skip_if_not_installed("SummarizedExperiment")
  x <- paired_reduction_fixture()
  x$batch <- factor(c(rep("a", 6L), "b", "a", "a", "a", "a", "a"))
  design <- ~ subject + visit + batch
  check <- function(data) {
    samples_df <- as.data.frame(SummarizedExperiment::colData(data))
    model <- stats::model.matrix(design, samples_df)
    if (qr(model)$rank != ncol(model) || !any(samples_df$batch == "b")) {
      stop("invalid design was tested")
    }
    "target failure"
  }
  reduced <- reduce_paired_failure(x, "subject", "visit", design, check)
  expect_identical(dim(reduced$data), c(1L, 6L))
  expect_true("s1" %in% reduced$data$subject)
  expect_true(all(table(reduced$data$subject, reduced$data$visit) == 1L))
  expect_identical(check(reduced$data), "target failure")
})

test_that("pair and condition use the original metadata names", {
  skip_if_not_installed("SummarizedExperiment")
  x <- paired_reduction_fixture()
  samples <- SummarizedExperiment::colData(x)
  names(samples)[1:2] <- c("subject id", "visit time")
  samples$extra <- matrix(seq_len(24L), nrow = 12L)
  samples <- samples[c("extra", "subject id", "visit time", "batch")]
  SummarizedExperiment::colData(x) <- samples
  colnames(x) <- rep(letters[1:6], times = 2L)
  design <- stats::reformulate(c("`subject id`", "`visit time`"))
  check <- function(data) "target failure"
  reduced <- reduce_paired_failure(x, "subject id", "visit time", design, check)
  expect_identical(dim(reduced$data), c(1L, 4L))
  expect_identical(names(SummarizedExperiment::colData(reduced$data)), names(samples))
  expect_identical(colnames(reduced$data), colnames(x)[reduced$columns])
  expect_identical(SummarizedExperiment::colData(reduced$data)$extra,
                   samples$extra[reduced$columns, , drop = FALSE])
})

test_that("checker calls are bounded, seeded and verified at the end", {
  skip_if_not_installed("SummarizedExperiment")
  x <- paired_reduction_fixture()
  withr::local_seed(123L)
  caller_seed <- .Random.seed
  state <- new.env()
  state$draws <- numeric()
  check <- function(data) {
    state$draws <- c(state$draws, stats::runif(1L))
    "target failure"
  }
  reduced <- reduce_paired_failure(x, "subject", "visit", ~ subject + visit, check, max_checks = 3L)
  expect_true(reduced$budget_exhausted)
  expect_identical(reduced$checks, 3L)
  expect_length(unique(state$draws), 1L)
  expect_identical(.Random.seed, caller_seed)

  state$draws <- numeric()
  unstable <- function(data) {
    check(data)
    if (length(state$draws) == 1L) "target failure"
  }
  expect_error(reduce_paired_failure(x, "subject", "visit", ~ subject + visit, unstable, max_checks = 2L),
               "final subset did not reproduce")
  expect_error(reduce_paired_failure(x, "subject", "visit", ~ subject + visit, function(data) NULL),
               "did not reproduce a failure")
  expect_error(reduce_paired_failure(x, "subject", "visit", ~ subject + visit, function(data) stop("unexpected")),
               "unexpected")
})

test_that("reduction retains HDF5 backing", {
  skip_if_not_installed("SummarizedExperiment")
  skip_if_not_installed("HDF5Array")
  x <- paired_reduction_fixture()
  file <- withr::local_tempfile(fileext = ".h5")
  disk <- HDF5Array::writeHDF5Array(
    SummarizedExperiment::assay(x), filepath = file, name = "counts", with.dimnames = TRUE
  )
  SummarizedExperiment::assay(x, "counts") <- disk
  check <- function(data) {
    if (any(SummarizedExperiment::assay(data, "counts") == 96L)) "target failure"
  }
  reduced <- reduce_paired_failure(x, "subject", "visit", ~ subject + visit, check)
  stored <- SummarizedExperiment::assay(reduced$data, "counts", withDimnames = FALSE)
  expect_s4_class(DelayedArray::seed(stored), "HDF5ArraySeed")
  expect_identical(as.integer(as.matrix(stored)[, 4L]), 96L)
  expect_identical(check(reduced$data), "target failure")
})
