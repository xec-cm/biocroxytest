evidence_package_contents <- function(pkg) {
  files <- list.files(pkg, all.files = TRUE, recursive = TRUE, no.. = TRUE)
  stats::setNames(lapply(file.path(pkg, files), function(file) {
    readBin(file, "raw", n = file.size(file))
  }), files)
}

evidence_status_source <- function() {
  c(
    "#' @testCases evidence",
    "#' list(cases = list(",
    "#'   a = list(suite = 'tests', setup = { mode <- 'complete'; value <- case_value() }),",
    "#'   ab = list(suite = 'longtests', setup = { mode <- 'fail'; value <- 41L }),",
    "#'   skipped = list(suite = 'tests', setup = { mode <- 'skip'; value <- case_value() }),",
    "#'   partial = list(suite = 'longtests', setup = { mode <- 'partial'; value <- case_value() }),",
    "#'   returned = list(suite = 'tests', setup = { mode <- 'return'; value <- case_value() }),",
    "#'   empty = list(suite = 'tests', setup = { mode <- 'empty' }),",
    "#'   error = list(suite = 'tests', setup = { mode <- 'error' }),",
    "#'   nested = list(suite = 'tests', setup = { mode <- 'nested'; value <- 41L }),",
    "#'   exited = list(suite = 'tests', setup = { mode <- 'exit' })",
    "#' ), test = {",
    paste0("#'   if (Sys.getpid() == ", Sys.getpid(), "L) stop('case executed in parent session')"),
    "#'   if (!is.null(getOption('biocroxytest.evidence.dirty'))) stop('options leaked between cases')",
    "#'   options(biocroxytest.evidence.dirty = TRUE)",
    "#'   writeLines(mode, 'fixture.txt')",
    "#'   writeLines(mode, 'created-during-case.txt')",
    "#'   if (mode == 'skip') skip('fixture unavailable before assertions')",
    "#'   if (mode == 'empty') return(invisible(NULL))",
    "#'   if (mode == 'error') stop('deliberate case error')",
    "#'   if (mode == 'exit') quit(save = 'no', status = 0L)",
    "#'   expect_identical(value, 42L)",
    "#'   if (mode == 'return') return(invisible(NULL))",
    "#'   if (mode == 'nested') test_that('child', { expect_true(TRUE) })",
    "#'   if (mode == 'partial') skip('backend unavailable after preliminary assertion')",
    "#' })",
    "answer <- function() 42L"
  )
}

evidence_revision_source <- function(answer = "42L") {
  c(
    "#' @testCases revision",
    "#' list(cases = list(",
    "#'   selected = list(suite = 'tests', setup = { value <- case_value() }),",
    "#'   'selected-extra' = list(suite = 'tests', setup = { stop('unselected case executed') })",
    "#' ), test = { expect_type(value, 'integer') })",
    paste0("answer <- function() ", answer)
  )
}

test_that("execution evidence distinguishes full, failed and interrupted cases", {
  withr::local_options(biocroxytest.evidence.dirty = NULL)
  pkg <- new_evidence_package(evidence_status_source())
  on.exit(unlink(pkg, recursive = TRUE, expand = FALSE), add = TRUE)
  before <- evidence_package_contents(pkg)
  result <- run_test_plan(pkg)
  cases <- result$cases
  expected <- c(
    "evidence/a" = "completed", "evidence/ab" = "failed", "evidence/empty" = "incomplete",
    "evidence/error" = "failed", "evidence/exited" = "incomplete", "evidence/nested" = "failed",
    "evidence/partial" = "incomplete", "evidence/returned" = "incomplete",
    "evidence/skipped" = "skipped"
  )
  expect_identical(sort(cases$id), names(expected))
  expect_identical(cases$status[match(names(expected), cases$id)], unname(expected))
  expect_identical(anyDuplicated(cases$id), 0L)
  expect_true(all(c("id", "suite", "source", "path", "status", "duration", "detail") %in%
                    names(cases)))
  expect_type(cases$duration, "double")
  expect_true(all(is.finite(cases$duration) & cases$duration >= 0))
  expect_match(cases$detail[match("evidence/skipped", cases$id)], "fixture unavailable")
  expect_match(cases$detail[match("evidence/partial", cases$id)], "backend unavailable")
  expect_match(cases$detail[match("evidence/error", cases$id)], "deliberate case error")
  expect_match(cases$detail[match("evidence/exited", cases$id)], "exited without returning")
  expect_match(cases$detail[match("evidence/nested", cases$id)], "Differences")
  expect_identical(sort(names(result$environments)), setdiff(names(expected), "evidence/exited"))
  environment <- result$environments[["evidence/a"]]
  expect_true(all(c("r_version", "platform", "os", "locale", "packages") %in%
                    names(environment)))
  expect_true(is.character(environment$r_version) && length(environment$r_version) == 1L &&
                nzchar(environment$r_version))
  expect_true(is.character(environment$platform) && any(nzchar(environment$platform)))
  expect_true(is.character(environment$os) && any(nzchar(environment$os)))
  expect_true(is.character(environment$locale) && any(nzchar(environment$locale)))
  expect_type(environment$packages, "character")
  expect_true("testthat" %in% names(environment$packages))
  expect_identical(unname(environment$packages["testthat"]),
                   as.character(utils::packageVersion("testthat")))
  expect_identical(evidence_package_contents(pkg), before)
  expect_null(getOption("biocroxytest.evidence.dirty"))
})

test_that("evidence selects exact IDs and fingerprints source and helper changes", {
  pkg <- new_evidence_package(evidence_revision_source())
  on.exit(unlink(pkg, recursive = TRUE, expand = FALSE), add = TRUE)
  before <- evidence_package_contents(pkg)
  first <- run_test_plan(pkg, ids = "revision/selected")
  second <- run_test_plan(pkg, ids = "revision/selected")
  expect_identical(first$cases$id, "revision/selected")
  expect_identical(first$cases$status, "completed")
  expect_identical(names(first$environments), "revision/selected")
  expect_type(first$revision, "character")
  expect_length(first$revision, 1L)
  expect_true(nzchar(first$revision))
  expect_identical(second$revision, first$revision)
  expect_identical(second$cases$status, "completed")
  expect_identical(evidence_package_contents(pkg), before)

  writeLines(evidence_revision_source("43L"), file.path(pkg, "R", "answer.R"))
  source_changed <- run_test_plan(pkg, ids = "revision/selected")
  expect_identical(source_changed$cases$status, "completed")
  expect_false(identical(source_changed$revision, first$revision))
  writeLines("case_value <- function() answer() + 1L",
             file.path(pkg, "tests", "testthat", "helper-value.R"))
  helper_changed <- run_test_plan(pkg, ids = "revision/selected")
  expect_identical(helper_changed$cases$status, "completed")
  expect_false(identical(helper_changed$revision, source_changed$revision))
})

test_that("missing, stale and setup failures cannot satisfy a planned case", {
  source <- evidence_revision_source()
  pkg <- new_evidence_package(source)
  on.exit(unlink(pkg, recursive = TRUE, expand = FALSE), add = TRUE)
  plan <- test_case_plan(pkg)
  generated <- file.path(pkg, plan$path[match("revision/selected", plan$id)])
  contents <- readLines(generated)
  unlink(generated)
  missing <- run_test_plan(pkg, ids = "revision/selected")
  expect_identical(missing$cases$id, "revision/selected")
  expect_identical(missing$cases$status, "missing")
  expect_length(missing$environments, 0L)

  writeLines(contents, generated)
  writeLines(sub("expect_type(value, 'integer')", "expect_type(value, 'double')",
                 source, fixed = TRUE), file.path(pkg, "R", "answer.R"))
  stale <- run_test_plan(pkg, ids = "revision/selected")
  expect_identical(stale$cases$status, "incomplete")
  expect_length(stale$environments, 0L)

  writeLines(source, file.path(pkg, "R", "answer.R"))
  writeLines("stop('deliberate helper setup failure')",
             file.path(pkg, "tests", "testthat", "helper-value.R"))
  before <- evidence_package_contents(pkg)
  setup <- run_test_plan(pkg, ids = "revision/selected")
  expect_identical(setup$cases$status, "incomplete")
  expect_match(setup$cases$detail, "deliberate helper setup failure")
  expect_identical(evidence_package_contents(pkg), before)
})

test_that("suite hooks see the original generated file and cannot change the recorded case", {
  pkg <- new_evidence_package(evidence_revision_source())
  on.exit(unlink(pkg, recursive = TRUE, expand = FALSE), add = TRUE)
  plan <- test_case_plan(pkg)
  file <- basename(plan$path[match("revision/selected", plan$id)])
  setup <- file.path(pkg, "tests", "testthat", "setup-evidence.R")
  teardown <- file.path(dirname(setup), "teardown-evidence.R")
  generated <- file.path(dirname(setup), file)
  checksum <- unname(tools::md5sum(generated))
  original_file_check <- sprintf("stopifnot(unname(tools::md5sum('%s')) == '%s')", file, checksum)
  writeLines(original_file_check, setup)
  writeLines(original_file_check, teardown)

  expect_identical(run_test_plan(pkg, ids = "revision/selected")$cases$status, "completed")
  unlink(teardown)

  writeLines(c(
    sprintf("file <- '%s'", file),
    "lines <- readLines(file)",
    "writeLines(sub('expect_type', 'expect_false', lines, fixed = TRUE), file)"
  ), setup)
  modified <- run_test_plan(pkg, ids = "revision/selected")
  expect_identical(modified$cases$status, "incomplete")
  expect_match(modified$cases$detail, "generated file changed during suite setup")
  expect_identical(unname(tools::md5sum(generated)), checksum)

  writeLines("testthat::skip('suite setup unavailable')", setup)
  skipped <- run_test_plan(pkg, ids = "revision/selected")
  expect_identical(skipped$cases$status, "incomplete")
  expect_match(skipped$cases$detail, "suite setup unavailable")
  expect_named(skipped$environments, "revision/selected")
})
