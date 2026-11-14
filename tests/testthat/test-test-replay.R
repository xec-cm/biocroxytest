test_that("a replay restores the seed before suite setup and compares the failure", {
  source <- c(
    "#' @testCases simulation",
    "#' list(cases = list(draw = list(suite = 'tests')), test = {",
    "#'   expect_identical(c(setup_draw, runif(1)), c(-1, -1))",
    "#' })",
    "answer <- function() 42L"
  )
  pkg <- new_evidence_package(source)
  on.exit(unlink(pkg, recursive = TRUE, expand = FALSE), add = TRUE)
  writeLines("setup_draw <- runif(1)", file.path(pkg, "tests", "testthat", "setup-replay.R"))
  file <- withr::local_tempfile(fileext = ".rds")
  id <- "simulation/draw"

  withr::local_envvar(c(CI = NA, NOT_CRAN = "true", TZ = "UTC"))
  withr::local_seed(123L)
  caller_seed <- .Random.seed
  evidence <- run_test_plan(pkg, ids = id, seed = 98L)
  expect_identical(.Random.seed, caller_seed)
  expect_identical(evidence$cases$status, "failed")
  expect_identical(evidence$replay[[id]]$seed, 98L)

  record <- withr::with_dir(dirname(file), write_test_replay(evidence, id, basename(file)))
  expect_identical(readRDS(file), record)
  expect_identical(record$revision, evidence$revision)
  expect_identical(record$format, 2L)
  expect_match(record$command, "biocroxytest::replay_test_case", fixed = TRUE)
  from_command <- withr::with_dir(pkg, eval(parse(text = record$command)))
  expect_true(from_command$comparison$same_failure)
  expect_error(write_test_replay(evidence, id, file))
  expect_identical(readRDS(file), record)

  Sys.setenv(CI = "true", TZ = "Etc/GMT+2")
  replayed <- replay_test_case(file, pkg)
  expect_true(replayed$comparison$same_failure)
  expect_identical(replayed$replay[[id]]$settings, record$context$settings)
  expect_identical(replayed$cases$detail, evidence$cases$detail)
  expect_identical(.Random.seed, caller_seed)
  expect_identical(Sys.getenv("CI"), "true")
  writeLines("unrelated notes", file.path(pkg, "notes.txt"))
  expect_true(replay_test_case(file, pkg)$comparison$same_failure)

  other_seed <- record
  other_seed$context$seed <- 99L
  other_seed$environment$r_version <- "0.0.0"
  alternate <- withr::local_tempfile(fileext = ".rds")
  saveRDS(other_seed, alternate)
  different <- replay_test_case(alternate, pkg)
  expect_identical(different$cases$status, "failed")
  expect_false(different$comparison$same_failure)
  expect_true("r_version" %in% different$comparison$environment_changes)

  wrong_case <- record
  wrong_case$case$suite <- "longtests"
  saveRDS(wrong_case, alternate)
  expect_error(replay_test_case(alternate, pkg), class = "biocroxytest_invalid_replay")

  missing_environment <- record
  missing_environment$environment <- list()
  saveRDS(missing_environment, alternate)
  expect_error(replay_test_case(alternate, pkg), class = "biocroxytest_invalid_replay")

  writeLines("answer <- function() 43L", file.path(pkg, "R", "answer.R"))
  expect_error(replay_test_case(file, pkg), class = "biocroxytest_replay_revision")
})

test_that("identical messages from different assertions are different failures", {
  source <- c(
    "#' @testCases assertions",
    "#' list(cases = list(two = list(suite = 'tests')), test = {",
    "#'   values <- if (Sys.getenv('BIOCROXYTEST_REPLAY_PROBE') == 'first') c(2L, 1L) else c(1L, 2L)",
    "#'   validate(values[1L])",
    "#'   validate(values[2L])",
    "#' })",
    "answer <- function() 42L"
  )
  pkg <- new_evidence_package(source)
  on.exit(unlink(pkg, recursive = TRUE, expand = FALSE), add = TRUE)
  writeLines("validate <- function(value) expect_identical(value, 1L)",
             file.path(pkg, "tests", "testthat", "helper-validate.R"))
  file <- withr::local_tempfile(fileext = ".rds")
  withr::local_envvar(BIOCROXYTEST_REPLAY_PROBE = "first")
  evidence <- run_test_plan(pkg)
  write_test_replay(evidence, "assertions/two", file)

  Sys.setenv(BIOCROXYTEST_REPLAY_PROBE = "second")
  replayed <- replay_test_case(file, pkg)
  expect_identical(replayed$cases$detail, evidence$cases$detail)
  expect_false(replayed$comparison$same_failure)
})

test_that("a replay uses the recorded fixture availability", {
  skip_if_not_installed("BiocFileCache")
  fixture <- withr::local_tempfile(lines = "fixture bytes")
  cache <- withr::local_tempdir()
  file <- withr::local_tempfile(fileext = ".rds")
  checksum <- unname(tools::md5sum(fixture))
  url <- "https://example.invalid/replay.tsv"
  source <- c(
    "#' @testCases fixture",
    "#' list(cases = list(external = list(suite = 'longtests', fixtures = list(",
    sprintf("#'   data = list(source = '%s', md5 = '%s', required = FALSE)", url, checksum),
    "#' ))), test = {",
    "#'   expect_identical(readLines(fixture_paths[['data']]), 'wrong expected value')",
    "#'   stop(paste('Fixture used:', fixture_paths[['data']]))",
    "#' })",
    "answer <- function() 42L"
  )
  pkg <- new_evidence_package(source)
  on.exit(unlink(pkg, recursive = TRUE, expand = FALSE), add = TRUE)
  bfc <- BiocFileCache::BiocFileCache(cache, ask = FALSE)
  cached <- BiocFileCache::bfcadd(bfc, paste(checksum, url), fixture, rtype = "local")

  evidence <- run_test_plan(pkg, fixture_cache = cache)
  record <- write_test_replay(evidence, "fixture/external", file)
  expect_identical(record$fixtures$observed_md5, checksum)
  expect_match(record$case$detail, "<fixture:data>", fixed = TRUE)
  expect_true(replay_test_case(file, pkg, fixture_cache = cache)$comparison$same_failure)

  writeLines("corrupted fixture", unname(cached))
  replayed <- replay_test_case(file, pkg, fixture_cache = cache)
  expect_identical(replayed$cases$status, "incomplete")
  expect_match(replayed$cases$detail, "Fixture availability or checksum differs")
  expect_false(replayed$comparison$same_failure)
})

test_that("only failed cases with replay context can be exported", {
  source <- c(
    "#' @testCases arithmetic",
    "#' list(cases = list(one = list(suite = 'tests')), test = { expect_true(TRUE) })",
    "answer <- function() 42L"
  )
  pkg <- new_evidence_package(source)
  on.exit(unlink(pkg, recursive = TRUE, expand = FALSE), add = TRUE)
  file <- withr::local_tempfile(fileext = ".rds")
  expect_error(run_test_plan(pkg, seed = 1.5), "one integer")
  evidence <- run_test_plan(pkg)
  expect_error(write_test_replay(evidence, "arithmetic/one", file), "no failed case")
  expect_false(file.exists(file))
  saveRDS(list(format = 999L), file)
  expect_error(replay_test_case(file, pkg), class = "biocroxytest_invalid_replay")
})
