fixture_declaration <- function(source, checksum, required = TRUE, suite = "tests") {
  sprintf(
    "#'   sample = list(suite = '%s', fixtures = list(data = list(source = '%s', md5 = '%s', required = %s)))",
    suite, source, checksum, required
  )
}

fixture_case_source <- function(declaration, assertion) {
  c(
    "#' @testCases inputs",
    "#' list(cases = list(",
    declaration,
    sprintf("#' ), test = { %s })", assertion),
    "answer <- function() 42L"
  )
}

test_that("required local fixtures are checked before loading the case", {
  fixture <- withr::local_tempfile(lines = "verified data")
  checksum <- unname(tools::md5sum(fixture))
  declaration <- fixture_declaration("data.tsv", toupper(checksum), suite = "longtests")
  pkg <- new_evidence_package(fixture_case_source(
    declaration, "expect_identical(readLines(fixture_paths[['data']]), 'verified data')"
  ))
  on.exit(unlink(pkg, recursive = TRUE, expand = FALSE), add = TRUE)
  local_file <- file.path(pkg, "longtests", "testthat", "data.tsv")

  plan <- test_case_plan(pkg)
  expect_identical(plan$fixture[[1]]$data$md5, checksum)
  missing <- run_test_plan(pkg)
  expect_identical(missing$cases$status, "incomplete")
  expect_match(missing$cases$detail, "Fixture data")
  expect_length(missing$environments, 0L)
  expect_identical(missing$fixtures[["inputs/sample"]]$status, "missing")
  expect_error(run_test_case("inputs/sample", pkg), class = "biocroxytest_fixture_unavailable")
  expect_error(run_test_plan(pkg, fixture_cache = file.path(pkg, "cache")), "outside the source")

  file.copy(fixture, local_file)
  verified <- run_test_plan(pkg)
  expect_identical(verified$cases$status, "completed")
  expect_identical(verified$fixtures[["inputs/sample"]]$observed_md5, checksum)
  expect_identical(suppressMessages(as.data.frame(run_test_case(
    "inputs/sample", pkg, reporter = "silent"
  )))$passed, 1L)

  writeLines("different data", local_file)
  mismatch <- run_test_plan(pkg)
  expect_identical(mismatch$cases$status, "incomplete")
  expect_identical(mismatch$fixtures[["inputs/sample"]]$status, "mismatch")
  expect_identical(mismatch$fixtures[["inputs/sample"]]$observed_md5, unname(tools::md5sum(local_file)))
})

test_that("optional fixtures are reported and unavailable paths are character NA", {
  declaration <- fixture_declaration("missing.tsv", strrep("0", 32), required = FALSE)
  assertion <- "expect_identical(fixture_paths[['data']], NA_character_)"
  pkg <- new_evidence_package(fixture_case_source(declaration, assertion))
  on.exit(unlink(pkg, recursive = TRUE, expand = FALSE), add = TRUE)

  report <- run_test_plan(pkg)
  expect_identical(report$cases$status, "completed")
  expect_false(report$fixtures[["inputs/sample"]]$required)
  expect_identical(report$fixtures[["inputs/sample"]]$status, "missing")

  file <- file.path(pkg, "R", "answer.R")
  before <- test_case_plan(pkg)
  changed <- fixture_declaration("different.tsv", strrep("0", 32), required = FALSE)
  writeLines(fixture_case_source(changed, assertion), file)
  changes <- compare_test_plans(before, test_case_plan(pkg))
  expect_identical(changes$status, "changed")
  expect_true("fixture" %in% changes$fields[[1L]])
})

test_that("remote fixtures reuse only verified cached bytes by default", {
  skip_if_not_installed("BiocFileCache")
  fixture <- withr::local_tempfile(lines = "remote fixture")
  cache <- withr::local_tempdir()
  checksum <- unname(tools::md5sum(fixture))
  url <- "https://example.invalid/data.tsv"
  pkg <- new_evidence_package(fixture_case_source(
    fixture_declaration(url, checksum),
    "expect_identical(readLines(fixture_paths[['data']]), 'remote fixture')"
  ))
  on.exit(unlink(pkg, recursive = TRUE, expand = FALSE), add = TRUE)
  testthat::local_mocked_bindings(download.file = function(...) stop("Unexpected download"), .package = "utils")

  report <- run_test_plan(pkg, fixture_cache = cache)
  expect_identical(report$cases$status, "incomplete")
  expect_match(report$cases$detail, "not cached")

  bfc <- BiocFileCache::BiocFileCache(cache, ask = FALSE)
  cached <- BiocFileCache::bfcadd(bfc, paste(checksum, url), fixture, rtype = "local")
  report <- run_test_plan(pkg, fixture_cache = cache)
  expect_identical(report$cases$status, "completed")
  expect_identical(report$fixtures[["inputs/sample"]]$source, url)
  expect_identical(report$fixtures[["inputs/sample"]]$observed_md5, checksum)

  writeLines("corrupt cached data", unname(cached))
  report <- run_test_plan(pkg, fixture_cache = cache)
  expect_identical(report$cases$status, "incomplete")
  expect_identical(report$fixtures[["inputs/sample"]]$status, "mismatch")
})

test_that("opt-in downloads cache verified bytes and can retry after a mismatch", {
  skip_if_not_installed("BiocFileCache")
  fixture <- withr::local_tempfile(lines = "verified download")
  cache <- withr::local_tempdir()
  checksum <- unname(tools::md5sum(fixture))
  url <- "https://example.invalid/download.tsv"
  pkg <- new_evidence_package(fixture_case_source(
    fixture_declaration(url, checksum),
    "expect_identical(readLines(fixture_paths[['data']]), 'verified download')"
  ))
  on.exit(unlink(pkg, recursive = TRUE, expand = FALSE), add = TRUE)

  contents <- "wrong bytes"
  testthat::local_mocked_bindings(
    download.file = function(url, destfile, mode, quiet) {
      expect_identical(mode, "wb")
      writeLines(contents, destfile)
      0L
    },
    .package = "utils"
  )
  mismatch <- run_test_plan(pkg, download_fixtures = TRUE, fixture_cache = cache)
  expect_identical(mismatch$fixtures[["inputs/sample"]]$status, "mismatch")
  bfc <- BiocFileCache::BiocFileCache(cache, ask = FALSE)
  expect_identical(nrow(BiocFileCache::bfcinfo(bfc)), 0L)

  contents <- "verified download"
  verified <- run_test_plan(pkg, download_fixtures = TRUE, fixture_cache = cache)
  expect_identical(verified$cases$status, "completed")
  expect_identical(nrow(BiocFileCache::bfcinfo(bfc)), 1L)
  expect_identical(run_test_plan(pkg, fixture_cache = cache)$cases$status, "completed")
})

test_that("fixture declarations are static and reject invalid sources", {
  declaration <- fixture_declaration("data.txt", strrep("0", 32))
  pkg <- new_evidence_package(fixture_case_source(declaration, "expect_true(TRUE)"))
  on.exit(unlink(pkg, recursive = TRUE, expand = FALSE), add = TRUE)
  file <- file.path(pkg, "R", "answer.R")
  for (source in c("../outside", "/absolute", "~/fixture", "~user/fixture",
                   "https://user@example.com/file", "http://example.com/file")) {
    writeLines(fixture_case_source(fixture_declaration(source, strrep("0", 32)), "expect_true(TRUE)"), file)
    expect_error(test_case_plan(pkg), class = "biocroxytest_invalid_test_case")
  }
  declaration <- sub("'data.txt'", "stop('must not run')", declaration, fixed = TRUE)
  writeLines(fixture_case_source(declaration, "expect_true(TRUE)"), file)
  expect_error(test_case_plan(pkg), class = "biocroxytest_invalid_test_case")
})

test_that("fixture sources survive locale changes during generation", {
  source <- paste0("caf", intToUtf8(233L), ".tsv")
  declarations <- list(data = list(source = source, md5 = strrep("0", 32), required = FALSE))
  original <- fixture_test_code(declarations)
  withr::local_locale(c(LC_CTYPE = "C"))

  code <- fixture_test_code(declarations)
  expect_identical(code, original)
  expression <- parse(text = code)[[1L]]
  expect_identical(eval(expression[[3L]][[2L]])$data$source, source)
})

test_that("remote fixture staging does not depend on the ID's spelling", {
  skip_if_not_installed("BiocFileCache")
  cache <- withr::local_tempdir()
  bfc <- BiocFileCache::BiocFileCache(cache, ask = FALSE)
  lower <- withr::local_tempfile(lines = "lower")
  upper <- withr::local_tempfile(lines = "upper")
  lower_url <- "https://example.invalid/lower.tsv"
  upper_url <- "https://example.invalid/upper.tsv"
  checksums <- unname(tools::md5sum(c(lower, upper)))
  BiocFileCache::bfcadd(bfc, paste(checksums, c(lower_url, upper_url)),
                       c(lower, upper), rtype = "local")

  declaration <- sprintf(
    "#'   sample = list(suite = 'tests', fixtures = list(a = list(source = '%s', md5 = '%s'), A = list(source = '%s', md5 = '%s')))",
    lower_url, checksums[1L], upper_url, checksums[2L]
  )
  assertion <- "expect_identical(vapply(fixture_paths, readLines, character(1)), c(a = 'lower', A = 'upper'))"
  pkg <- new_evidence_package(fixture_case_source(declaration, assertion))
  on.exit(unlink(pkg, recursive = TRUE, expand = FALSE), add = TRUE)

  report <- run_test_plan(pkg, fixture_cache = cache)
  expect_identical(report$cases$status, "completed")
  expect_identical(report$fixtures[["inputs/sample"]]$observed_md5, checksums)
})

test_that("runner keeps fixture errors containing braces", {
  pkg <- new_evidence_package(fixture_case_source(
    fixture_declaration("https://example.invalid/data.tsv", strrep("0", 32)),
    "expect_true(TRUE)"
  ))
  on.exit(unlink(pkg, recursive = TRUE, expand = FALSE), add = TRUE)
  cache <- withr::local_tempdir()
  testthat::local_mocked_bindings(
    resolve_remote_fixture = function(...) stop("Cannot open cache-{unknown_object}"),
    .package = "biocroxytest"
  )

  error <- expect_error(run_test_case("inputs/sample", pkg, fixture_cache = cache),
                        class = "biocroxytest_fixture_unavailable")
  expect_match(conditionMessage(error), "cache-{unknown_object}", fixed = TRUE)
})
