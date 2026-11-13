write_alignment_fixture <- function(pkg) {
  dir.create(file.path(pkg, "R"), recursive = TRUE)
  dir.create(file.path(pkg, "tests", "testthat"), recursive = TRUE)
  dir.create(file.path(pkg, "longtests", "testthat"), recursive = TRUE)
  writeLines(c(
    "Package: samplealignmentfixture",
    "Version: 0.0.1",
    "Encoding: UTF-8",
    'Roxygen: list(roclets = "biocroxytest::tests_roclet")'
  ), file.path(pkg, "DESCRIPTION"))
  writeLines("export(align_samples)", file.path(pkg, "NAMESPACE"))
  writeLines(c(
    "make_experiment <- function(backend) {",
    "  counts <- matrix(c(11L, 12L, 21L, 22L, 31L, 32L), nrow = 2L,",
    "                   dimnames = list(c('g1', 'g2'), c('s1', 's2', 's3')))",
    "  if (backend == 'hdf5') {",
    "    path <- withr::local_tempfile(pattern = 'alignment-', tmpdir = '.', fileext = '.h5',",
    "                                    .local_envir = parent.frame())",
    "    counts <- HDF5Array::writeHDF5Array(counts, path, name = 'counts',",
    "                                       chunkdim = c(2L, 1L), with.dimnames = TRUE)",
    "  }",
    "  col_data <- data.frame(condition = c('control', 'control', 'treated'),",
    "                         row.names = c('s1', 's2', 's3'))",
    "  SummarizedExperiment::SummarizedExperiment(assays = list(counts = counts),",
    "                                            colData = col_data)",
    "}",
    "#' @testCases sample-alignment",
    "#' list(",
    "#'   cases = list(",
    "#'     memory = list(suite = 'tests', setup = {",
    "#'       x <- make_experiment('memory')",
    "#'       expect_true(is.matrix(SummarizedExperiment::assay(x, 'counts')))",
    "#'     }),",
    "#'     hdf5 = list(suite = 'longtests', setup = {",
    "#'       x <- make_experiment('hdf5')",
    "#'       stored <- SummarizedExperiment::assay(x, 'counts', withDimnames = FALSE)",
    "#'       expect_s4_class(stored, 'HDF5Matrix')",
    "#'       expect_identical(DelayedArray::chunkdim(stored), c(2L, 1L))",
    "#'     })",
    "#'   ),",
    "#'   test = {",
    "#'     aligned <- align_samples(x, c('s3', 's1'))",
    "#'     expect_s4_class(aligned, 'SummarizedExperiment')",
    "#'     counts <- SummarizedExperiment::assay(aligned, 'counts', withDimnames = FALSE)",
    "#'     if (methods::is(x |> SummarizedExperiment::assay('counts', withDimnames = FALSE),",
    "#'                     'HDF5Matrix')) {",
    "#'       expect_s4_class(DelayedArray::seed(counts), 'HDF5ArraySeed')",
    "#'     }",
    "#'     expect_identical(as.matrix(counts), matrix(c(31L, 32L, 11L, 12L), nrow = 2L,",
    "#'                      dimnames = list(c('g1', 'g2'), c('s3', 's1'))))",
    "#'     expect_identical(rownames(SummarizedExperiment::colData(aligned)), c('s3', 's1'))",
    "#'     expect_identical(aligned$condition, c('treated', 'control'))",
    "#'   }",
    "#' )",
    "align_samples <- function(x, samples) x[, samples]"
  ), file.path(pkg, "R", "align-samples.R"))
}

alignment_suite_results <- function(pkg, suite) {
  as.data.frame(testthat::test_dir(
    file.path(pkg, suite, "testthat"),
    package = "samplealignmentfixture",
    load_package = "source",
    reporter = "silent",
    stop_on_failure = FALSE
  ))
}

test_that("named cases preserve sample alignment in memory and on HDF5", {
  skip_if_not_installed("SummarizedExperiment")
  skip_if_not_installed("DelayedArray")
  skip_if_not_installed("HDF5Array")
  pkg <- tempfile("sample-alignment-")
  on.exit({
    if (isNamespaceLoaded("samplealignmentfixture")) {
      unloadNamespace("samplealignmentfixture")
    }
    unlink(pkg, recursive = TRUE)
  }, add = TRUE)
  write_alignment_fixture(pkg)

  plan <- test_case_plan(pkg)
  expect_identical(plan$id, c("sample-alignment/hdf5", "sample-alignment/memory"))
  expect_identical(plan$suite, c("longtests", "tests"))
  expect_false(any(file.exists(file.path(pkg, plan$path))))
  roxygen2::roxygenise(pkg, load_code = roxygen2::load_source)
  expect_true(all(file.exists(file.path(pkg, plan$path))))
  expect_length(list.files(pkg, pattern = "[.]h5$", recursive = TRUE), 0L)

  fields <- c("test", "failed", "skipped", "error", "warning", "passed")
  for (i in seq_len(nrow(plan))) {
    selected <- as.data.frame(run_test_case(plan$id[i], pkg, reporter = "silent"))
    suite <- alignment_suite_results(pkg, plan$suite[i])
    expect_identical(selected[fields], suite[fields])
    expect_identical(selected$test, plan$id[i])
    expect_identical(selected$failed, 0L)
    expect_false(selected$skipped)
    expect_false(selected$error)
    expect_identical(selected$warning, 0L)
    expect_gt(selected$passed, 0L)
  }

  # Keep sample IDs and assay values intact, but misalign the sample metadata.
  source <- file.path(pkg, "R", "align-samples.R")
  code <- readLines(source)
  writeLines(c(head(code, -1L),
    "align_samples <- function(x, samples) {",
    "  aligned <- x[, samples]",
    "  stored <- SummarizedExperiment::assay(x, 'counts', withDimnames = FALSE)",
    "  if (methods::is(stored, 'HDF5Matrix')) aligned$condition <- rev(aligned$condition)",
    "  aligned",
    "}"
  ), source)

  hdf5 <- as.data.frame(run_test_case("sample-alignment/hdf5", pkg, reporter = "silent"))
  expect_identical(hdf5$test, "sample-alignment/hdf5")
  expect_identical(hdf5$failed, 1L)
  expect_false(hdf5$skipped)
  expect_false(hdf5$error)
  expect_identical(hdf5[fields], alignment_suite_results(pkg, "longtests")[fields])
  memory <- as.data.frame(run_test_case("sample-alignment/memory", pkg, reporter = "silent"))
  expect_identical(memory$test, "sample-alignment/memory")
  expect_identical(memory$failed, 0L)
  expect_false(memory$skipped)
  expect_false(memory$error)
  expect_length(list.files(pkg, pattern = "[.]h5$", recursive = TRUE), 0L)
})
