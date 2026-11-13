new_named_case_package <- function() {
  pkg <- tempfile("named cases ")
  dir.create(file.path(pkg, "R"), recursive = TRUE)
  for (suite in c("tests", "longtests")) {
    dir.create(file.path(pkg, suite, "testthat"), recursive = TRUE)
  }
  writeLines(c(
    "Package: namedcasefixture", "Version: 0.0.1", "Encoding: UTF-8",
    "Suggests: testthat", "Config/testthat/edition: 3",
    'Roxygen: list(roclets = "biocroxytest::tests_roclet")'
  ), file.path(pkg, "DESCRIPTION"))
  writeLines(character(), file.path(pkg, "NAMESPACE"))
  pkg
}

named_case_source <- function(suite = "longtests", expected = "42L") {
  c(
    "#' @tests expect_identical(answer(), 42L)",
    "#' @testCases alignment",
    "#' list(",
    "#'   cases = list(",
    "#'     memory = list(suite = 'tests', setup = { value <- ordinary_value() }),",
    paste0("#'     disk = list(suite = '", suite, "', setup = { value <- disk_value() })"),
    "#'   ),",
    paste0("#'   test = { expect_identical(value, ", expected, ") }"),
    "#' )",
    "answer <- function() 42L"
  )
}

test_that("named cases share a body and select helpers from their own suite", {
  pkg <- new_named_case_package()
  on.exit(unlink(pkg, recursive = TRUE, expand = FALSE), add = TRUE)
  source <- file.path(pkg, "R/answer.R")
  writeLines(named_case_source(), source)
  writeLines(c(
    "ordinary_value <- function() answer()",
    "disk_value <- function() stop('ordinary suite leaked into long tests')"
  ), file.path(pkg, "tests/testthat/helper-values.R"))
  writeLines("disk_value <- function() answer()",
             file.path(pkg, "longtests/testthat/helper-values.R"))

  plan <- test_case_plan(pkg)
  expect_identical(plan$id, c("alignment/disk", "alignment/memory"))
  expect_identical(plan$suite, c("longtests", "tests"))
  expect_identical(plan$source, rep("R/answer.R", 2L))
  expect_identical(plan$description, plan$id)
  expect_false(any(file.exists(file.path(pkg, plan$path))))
  expect_error(run_test_case("alignment/disk", pkg, "silent"), "missing or out of date")
  roxygen2::roxygenise(pkg, load_code = roxygen2::load_source)
  expect_true(all(file.exists(file.path(pkg, plan$path))))
  for (id in plan$id) {
    result <- as.data.frame(run_test_case(id, pkg, "silent"))
    expect_identical(result$test, id)
    expect_identical(result$failed, 0L)
    expect_false(result$error)
    expect_identical(result$passed, 1L)
  }
  ordinary <- parse(file.path(pkg, plan$path[2L]))
  expect_match(ordinary[[1L]][[2L]], "^Function answer\\(\\) @ L")
  expect_identical(ordinary[[2L]][[2L]], "alignment/memory")

  writeLines(named_case_source(expected = "99L"), source)
  expect_error(run_test_case("alignment/disk", pkg, "silent"), "missing or out of date")
  expect_error(run_test_case("alignment/unknown", pkg, "silent"), "No test case has ID")
})

test_that("case IDs survive source and suite moves and expose plan changes", {
  pkg <- new_named_case_package()
  on.exit(unlink(pkg, recursive = TRUE, expand = FALSE), add = TRUE)
  source <- file.path(pkg, "R/answer.R")
  writeLines(named_case_source(), source)
  cat("Collate: 'answer.R'\n", file = file.path(pkg, "DESCRIPTION"), append = TRUE)
  writeLines("^R/ignored[.]R$", file.path(pkg, ".Rbuildignore"))
  writeLines(named_case_source(), file.path(pkg, "R/ignored.R"))
  before <- test_case_plan(pkg)
  unlink(source)
  writeLines(c("", named_case_source("tests", "99L")), file.path(pkg, "R/renamed.R"))
  after <- test_case_plan(pkg)
  expect_identical(after$id, before$id)
  expect_identical(after$source, rep("R/renamed.R", 2L))
  changes <- compare_test_plans(before, after)
  expect_identical(changes$id, c("alignment/disk", "alignment/disk", "alignment/memory"))
  expect_identical(changes$status, c("moved", "changed", "changed"))
})

test_that("invalid declarations fail before replacing generated output", {
  pkg <- new_named_case_package()
  on.exit(unlink(pkg, recursive = TRUE, expand = FALSE), add = TRUE)
  source <- file.path(pkg, "R/answer.R")
  writeLines(named_case_source(), source)
  roxygen2::roxygenise(pkg, load_code = roxygen2::load_source)
  paths <- file.path(pkg, test_case_plan(pkg)$path)
  before <- lapply(paths, readLines)

  for (declaration in c(
    "list(cases = stop('generator evaluated cases'), test = {})",
    "list(cases = list(x = list(suite = 'invalid')), test = {})",
    "list(cases = list(x = list(suite = 'tests'), x = list(suite = 'longtests')), test = {})",
    "list(cases = list(x = list(suite = 'tests')), test = stop('evaluated'))"
  )) {
    writeLines(c("#' @testCases alignment", paste("#'", declaration), "NULL"), source)
    expect_error(test_case_plan(pkg), class = "biocroxytest_invalid_test_case")
    expect_error(roxygen2::roxygenise(pkg, clean = TRUE, load_code = roxygen2::load_source),
                 class = "biocroxytest_invalid_test_case")
    expect_identical(lapply(paths, readLines), before)
  }
  writeLines(named_case_source(), source)
  writeLines(named_case_source(), file.path(pkg, "R/duplicate.R"))
  expect_error(test_case_plan(pkg), "Duplicate test case group")
  expect_identical(lapply(paths, readLines), before)
})

test_that("named cases preserve the source spelling and values of literals", {
  source <- paste(c(
    "#' @testCases literals",
    "#' list(cases = list(one = list(suite = 'tests', setup = {",
    "#'   unicode <- '\\u00e9'",
    "#'   multiline <- 'first",
    "#' second'",
    "#'   precise <- 0x1.fffffffffffffp-1",
    "#' })), test = {",
    "#'   expect_identical(unicode, intToUtf8(233L))",
    "#'   expect_identical(multiline, 'first\\nsecond')",
    "#'   expect_identical(precise, 1 - 2^-53)",
    "#' })",
    "NULL"
  ), collapse = "\n")
  output <- roxygen2::roc_proc_text(tests_roclet(), source)$tests[[1L]]
  expect_match(output, "unicode <- '\\u00e9'", fixed = TRUE)
  case <- parse(text = output)[[1L]]
  eval(case[[3L]], envir = new.env())
})

test_that("local selection accepts literal UTF-8 from generated tests", {
  pkg <- new_named_case_package()
  on.exit(unlink(pkg, recursive = TRUE, expand = FALSE), add = TRUE)
  code <- c(
    "#' @testCases unicode",
    "#' list(cases = list(one = list(suite = 'tests')), test = {",
    paste0("#'   expect_identical('", intToUtf8(233L), "', intToUtf8(233L))"),
    "#' })",
    "NULL"
  )
  writeLines(code, file.path(pkg, "R/utf8.R"), useBytes = TRUE)
  roxygen2::roxygenise(pkg, load_code = roxygen2::load_source)
  result <- as.data.frame(run_test_case("unicode/one", pkg, "silent"))
  expect_identical(result$passed, 1L)
  expect_false(result$error)
})

test_that("named cases must be declared directly for source-only planning", {
  source <- paste(c(
    "#' @eval c('@testCases generated',",
    "#'          'list(cases = list(one = list(suite = \"tests\")), test = {})')",
    "NULL"
  ), collapse = "\n")
  expect_error(roxygen2::roc_proc_text(tests_roclet(), source),
               "directly in the source", class = "biocroxytest_invalid_test_case")
})

test_that("selecting a case preserves snapshots for other cases with the same prefix", {
  pkg <- new_named_case_package()
  on.exit(unlink(pkg, recursive = TRUE, expand = FALSE), add = TRUE)
  withr::local_envvar(c(CI = "false", NOT_CRAN = "true"))
  writeLines(c(
    "#' @testCases snapshots",
    "#' list(cases = list(",
    "#'   a = list(suite = 'tests', setup = { value <- 'first' }),",
    "#'   ab = list(suite = 'tests', setup = { value <- 'second' })",
    "#' ), test = { expect_snapshot(cat(value)) })",
    "NULL"
  ), file.path(pkg, "R/snapshots.R"))
  roxygen2::roxygenise(pkg, load_code = roxygen2::load_source)
  plan <- test_case_plan(pkg)
  roxygen2::load_pkgload(pkg)
  suppressWarnings(testthat::test_file(
    file.path(pkg, plan$path[1L]), reporter = "silent", package = "namedcasefixture"
  ))
  snapshot <- file.path(pkg, "tests/testthat/_snaps/roxytest-tests-snapshots.md")
  before <- readLines(snapshot)
  expect_true(all(c("first", "second") %in% trimws(before)))
  result <- as.data.frame(run_test_case("snapshots/a", pkg, "silent"))
  expect_identical(result$test, "snapshots/a")
  expect_identical(result$passed, 1L)
  expect_identical(readLines(snapshot), before)
})
