new_testexamples_package <- function() {
  pkg <- withr::local_tempdir("testexamples-", .local_envir = parent.frame())
  dir.create(file.path(pkg, "R"))
  dir.create(file.path(pkg, "tests", "testthat"), recursive = TRUE)
  writeLines(c(
    "Package: testexamplesfixture",
    "Version: 0.0.1",
    "Encoding: UTF-8",
    'Roxygen: list(roclets = "biocroxytest::tests_roclet")'
  ), file.path(pkg, "DESCRIPTION"))
  writeLines(character(), file.path(pkg, "NAMESPACE"))
  writeLines(c("library(testthat)", 'test_check("testexamplesfixture")'),
             file.path(pkg, "tests", "testthat.R"))
  pkg
}

generate_testexamples <- function(pkg, clean = FALSE) {
  roxygenise_fixture(pkg, clean = clean)
}

testexamples_results <- function(pkg) {
  as.data.frame(testthat::test_dir(
    file.path(pkg, "tests", "testthat"),
    filter = "^roxytest-testexamples-",
    reporter = "silent",
    stop_on_failure = FALSE
  ))
}

test_that("each testexamples tag runs ordered examples in its own test environment", {
  pkg <- new_testexamples_package()
  dir.create(file.path(pkg, "inst"))
  writeLines(c("steps <- c(steps, 'file')", "value <- value + 3L"),
             file.path(pkg, "inst", "step.R"))
  marker <- file.path(pkg, "condition.log")
  condition <- sprintf(
    "{ cat('checked\\n', file = %s, append = TRUE); identical((value %%%% 4L), 1L) }",
    encodeString(marker, quote = '"')
  )
  writeLines(c(
    "#' Ordered examples",
    "#' @examples",
    "#' steps <- 'inline'",
    "#' value <- 2L",
    "#' @example inst/step.R",
    "#' @examples",
    "#' \\dontrun{stop('Do not run this example')}",
    "#' \\dontshow{steps <- c(steps, 'dontshow')}",
    "#' \\donttest{steps <- c(steps, 'donttest')}",
    paste("#' @examplesIf", condition),
    "#' if (value == 5L) {",
    "#'   steps <- c(steps, 'conditional')",
    "#'   \\donttest{value <- 2L * value}",
    "#' }",
    "#' @examplesIf 'TRUE'",
    "#' steps <- c(steps, 'last')",
    "#' @testexamples",
    "#' expect_identical(steps, c('inline', 'file', 'dontshow', 'donttest', 'conditional', 'last'))",
    "#' expect_identical(value, 10L)",
    "#' value <- -1L",
    "#' @testexamples expect_identical(value, 10L)",
    "ordered <- function() NULL"
  ), file.path(pkg, "R", "ordered.R"))

  generate_testexamples(pkg)
  expect_false(file.exists(marker))
  results <- testexamples_results(pkg)
  expect_equal(nrow(results), 2L)
  expect_identical(anyDuplicated(results$test), 0L)
  expect_identical(results$passed, c(2L, 1L))
  expect_false(any(results$skipped | results$error | results$failed > 0L | results$warning > 0L))
  expect_identical(readLines(marker), c("checked", "checked"))
})

test_that("a false examplesIf condition skips its testexamples assertions", {
  pkg <- new_testexamples_package()
  writeLines(c(
    "#' Conditional examples",
    "#' @examples available <- 'FALSE'",
    "#' @examplesIf available",
    "#' stop('The conditional example should be skipped')",
    "#' @examples stop('The remaining examples should be skipped')",
    "#' @testexamples stop('The assertions should be skipped')",
    "guarded <- function() NULL"
  ), file.path(pkg, "R", "guarded.R"))

  generate_testexamples(pkg)
  results <- testexamples_results(pkg)
  expect_equal(nrow(results), 1L)
  expect_identical(results$skipped, TRUE)
  expect_identical(results$error, FALSE)
  expect_identical(results$failed, 0L)
})

test_that("freshness notices a changed example file without modifying generated tests", {
  pkg <- new_testexamples_package()
  dir.create(file.path(pkg, "inst"))
  example <- file.path(pkg, "inst", "values.R")
  writeLines("value <- 1L", example)
  writeLines(c(
    "#' External example",
    "#' @example inst/values.R",
    "#' @testexamples expect_identical(value, 1L)",
    "external <- function() NULL"
  ), file.path(pkg, "R", "external.R"))

  generate_testexamples(pkg)
  path <- "tests/testthat/test-roxytest-testexamples-external.R"
  generated <- file.path(pkg, path)
  before <- readBin(generated, "raw", n = file.size(generated))
  expect_true(check_generated_fixture(pkg))

  writeLines("value <- 2L", example)
  stale <- expect_error(check_generated_fixture(pkg), class = "biocroxytest_stale_tests")
  expect_identical(stale$changes, data.frame(
    status = "changed", suite = "tests", source = "R/external.R", path = path
  ))
  expect_identical(readBin(generated, "raw", n = file.size(generated)), before)
  expect_identical(readLines(example), "value <- 2L")
})

test_that("testexamples share a topic without losing the test's source", {
  pkg <- new_testexamples_package()
  writeLines(c(
    "#' Shared topic",
    "#' @examples order <- 'primary'",
    "#' @testexamples expect_identical(order, c('primary', 'related'))",
    "shared <- function() NULL"
  ), file.path(pkg, "R", "a.R"))
  writeLines(c(
    "#' @rdname shared",
    "#' @examples order <- c(order, 'related')",
    "#' @testexamples expect_identical(order, c('primary', 'related'))",
    "related <- function() NULL"
  ), file.path(pkg, "R", "b.R"))

  generate_testexamples(pkg)
  results <- testexamples_results(pkg)
  expect_identical(results$file, paste0("test-roxytest-testexamples-", c("a.R", "b.R")))
  expect_match(results$test[1L], "shared", fixed = TRUE)
  expect_match(results$test[2L], "related", fixed = TRUE)
  expect_identical(results$passed, c(1L, 1L))
  expect_false(any(results$skipped | results$error | results$failed > 0L | results$warning > 0L))
})

test_that("example grouping uses the Rd filename and describeIn target", {
  pkg <- new_testexamples_package()
  writeLines(c(
    "#' Operator topic",
    "#' @name a+b",
    "#' @examples steps <- 'primary'",
    "#' @testexamples expect_identical(steps, c('primary', 'shared'))",
    "primary <- function() NULL"
  ), file.path(pkg, "R", "a.R"))
  writeLines(c(
    "#' @rdname a-plus-b",
    "#' @examples steps <- c(steps, 'shared')",
    "shared <- function() NULL"
  ), file.path(pkg, "R", "b.R"))
  writeLines(c(
    "#' @describeIn primary Related function.",
    "#' @examples steps <- c(steps, 'described')",
    "described <- function() NULL"
  ), file.path(pkg, "R", "c.R"))
  writeLines(c(
    "#' @noRd",
    "#' @rdname primary",
    "#' @examples stop('This example is outside the Rd topic')",
    "NULL"
  ), file.path(pkg, "R", "hidden.R"))
  roxygenise_fixture(pkg, roclets = c("rd", "biocroxytest::tests_roclet"))
  expect_identical(testexamples_results(pkg)$passed, 1L)
  # @describeIn resolves the object topic, independently of the @name above.
  writeLines(c(
    "#' Primary topic",
    "#' @examples steps <- 'primary'",
    "#' @testexamples expect_identical(steps, c('primary', 'described'))",
    "primary <- function() NULL"
  ), file.path(pkg, "R", "a.R"))
  generate_testexamples(pkg)
  expect_identical(testexamples_results(pkg)$passed, 1L)
})

test_that("a platform directive can render an empty example", {
  pkg <- new_testexamples_package()
  inactive <- if (.Platform$OS.type == "windows") "unix" else "windows"
  writeLines(c(
    "#' Platform example",
    "#' @examples",
    paste("#' #ifdef", inactive),
    "#' stop('wrong platform')",
    "#' #endif",
    "#' @testexamples expect_true(TRUE)",
    "platform_example <- function() NULL"
  ), file.path(pkg, "R", "platform.R"))
  generate_testexamples(pkg)
  expect_identical(testexamples_results(pkg)$passed, 1L)
})

test_that("a migrated testexamples file keeps roxytest's path and title", {
  suppressPackageStartupMessages(skip_if_not_installed("roxytest"))
  pkg <- new_testexamples_package()
  withr::local_dir(pkg)
  writeLines(c(
    "#' Simple example",
    "#' @examples value <- 1L",
    "#' @testexamples expect_identical(value, 1L)",
    "simple <- function() NULL"
  ), "R/simple.R")
  roxygenise_fixture(roclets = "roxytest::testthat_roclet")
  path <- "tests/testthat/test-roxytest-testexamples-simple.R"
  expect_identical(readLines(path, n = 1L), roxytest_header)
  legacy <- parse(path)[[1L]]
  dir.create("tests/testthat/_snaps")
  snapshot <- "tests/testthat/_snaps/roxytest-testexamples-simple.md"
  writeLines("A saved snapshot", snapshot)
  stale <- "tests/testthat/test-roxytest-testexamples-obsolete.R"
  writeLines(generated_test_header, stale)

  generate_testexamples(pkg)
  expect_identical(readLines(path, n = 1L), generated_test_header)
  expect_identical(parse(path)[[1L]][[2L]], legacy[[2L]])
  expect_identical(readLines(snapshot), "A saved snapshot")
  expect_false(file.exists(stale))
  inventory <- generated_test_inventory(pkg)
  expect_identical(inventory, data.frame(suite = "tests", source = "R/simple.R", path = path))
})

test_that("migrating an S4 example test preserves its roxytest title", {
  suppressPackageStartupMessages(skip_if_not_installed("roxytest"))
  pkg <- new_testexamples_package()
  withr::local_dir(pkg)
  writeLines(c(
    "setGeneric('example_method', function(x) standardGeneric('example_method'))",
    "#' An example method",
    "#' @examples value <- 1L",
    "#' @testexamples expect_identical(value, 1L)",
    "setMethod('example_method', 'numeric', function(x) x)"
  ), "R/method.R")
  roxygenise_fixture(roclets = "roxytest::testthat_roclet")
  path <- "tests/testthat/test-roxytest-testexamples-method.R"
  legacy_title <- parse(path)[[1L]][[2L]]
  generate_testexamples(pkg)
  expect_identical(parse(path)[[1L]][[2L]], legacy_title)
})

test_that("a manual testexamples collision preserves the existing outputs of both suites", {
  pkg <- new_testexamples_package()
  dir.create(file.path(pkg, "longtests", "testthat"), recursive = TRUE)
  paths <- file.path(pkg, c(
    "tests/testthat/test-roxytest-testexamples-a.R",
    "tests/testthat/test-roxytest-testexamples-obsolete.R",
    "tests/testthat/test-roxytest-tests-a.R",
    "longtests/testthat/test-biocroxytest-longtests-a.R"
  ))
  writeLines("# Hand-written test", paths[1L])
  for (path in paths[-1L]) writeLines(c(generated_test_header, "# Existing test"), path)
  writeLines(c(
    "#' @examples value <- 1L",
    "#' @testexamples expect_identical(value, 1L)",
    "#' @tests expect_true(TRUE)",
    "#' @longtests expect_true(TRUE)",
    "a <- function() NULL"
  ), file.path(pkg, "R", "a.R"))
  before <- lapply(paths, readLines)

  expect_error(generate_testexamples(pkg, clean = TRUE), "collides.*test-roxytest-testexamples-a[.]R")
  expect_identical(lapply(paths, readLines), before)
})

test_that("Rd expressions cannot run while generating example tests", {
  pkg <- new_testexamples_package()
  marker <- file.path(pkg, "evaluated")
  writeLines(c(
    "#' Dynamic example",
    "#' @examples",
    sprintf("#' \\Sexpr{file.create(%s)}", encodeString(marker, quote = '"')),
    "#' @testexamples expect_true(TRUE)",
    "example_function <- function() NULL"
  ), file.path(pkg, "R", "dynamic.R"))

  expect_error(generate_testexamples(pkg), "Sexpr",
               class = "biocroxytest_invalid_test_example")
  expect_false(file.exists(marker))
  expect_length(list.files(file.path(pkg, "tests", "testthat")), 0L)
})
