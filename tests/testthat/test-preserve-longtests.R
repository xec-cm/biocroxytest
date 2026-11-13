test_that("repeated long test tags generate separate tests", {
  out <- roxygen2::roc_proc_text(longtests_roclet(), "
    #' @longtests
    #' expect_identical(1L, 1L)
    #' @longtests expect_identical(2L, 2L)
    f <- function() NULL
  ")
  cases <- parse(text = out$longtests[[1L]])

  expect_length(cases, 2L)
  expect_identical(cases[[1L]][[2L]], "Function f() @ L5")
  expect_identical(anyDuplicated(vapply(cases, `[[`, character(1), 2L)), 0L)
  expect_identical(
    lapply(cases, function(case) case[[3L]][[2L]]),
    list(quote(expect_identical(1L, 1L)), quote(expect_identical(2L, 2L)))
  )
})

test_that("long tests sharing an Rd topic stay with their source methods", {
  pkg <- tempfile("longtests-package-")
  on.exit(unlink(pkg, recursive = TRUE), add = TRUE)
  dir.create(file.path(pkg, "R"), recursive = TRUE)
  dir.create(file.path(pkg, "longtests", "testthat"), recursive = TRUE)
  writeLines(
    c("Package: longtestfixture", "Version: 0.0.1", "Encoding: UTF-8"),
    file.path(pkg, "DESCRIPTION")
  )
  writeLines(character(), file.path(pkg, "NAMESPACE"))
  writeLines(c(
    "setGeneric('bcx_identity', function(x) standardGeneric('bcx_identity'))",
    "#' @rdname bcx_identity",
    "#' @longtests expect_identical(bcx_identity(1L), 1L)",
    "setMethod('bcx_identity', 'integer', function(x) x)"
  ), file.path(pkg, "R", "a.R"))
  writeLines(c(
    "#' @rdname bcx_identity",
    "#' @longtests expect_identical(bcx_identity('x'), 'x')",
    "setMethod('bcx_identity', 'character', function(x) x)"
  ), file.path(pkg, "R", "b.R"))

  env <- roxygen2::load_source(pkg)
  roxygenise_fixture(
    pkg,
    roclets = "biocroxytest::longtests_roclet",
    load_code = function(path) env
  )
  files <- list.files(file.path(pkg, "longtests", "testthat"), full.names = TRUE)
  expect_identical(basename(files), paste0("test-biocroxytest-longtests-", c("a.R", "b.R")))
  cases <- lapply(files, function(file) parse(file)[[1L]])
  expect_identical(vapply(cases, `[[`, character(1), 2L), c(
    "Method bcx_identity,integer-method @ L4",
    "Method bcx_identity,character-method @ L3"
  ))
  for (case in cases) eval(case[[3L]], envir = env)
})

test_that("generated tests preserve multiline literals", {
  out <- roxygen2::roc_proc_text(longtests_roclet(), paste(c(
    "#' @longtests",
    "#' expect_identical(\"a",
    "#'   b\", \"a\\n  b\")",
    "#' expect_identical(r\"(c",
    "#' d)\", \"c\\nd\")",
    "f <- function() NULL"
  ), collapse = "\n"))
  case <- parse(text = out$longtests[[1L]])[[1L]]
  eval(case[[3L]])
})

test_that("generated tests quote titles for non-syntactic names", {
  out <- roxygen2::roc_proc_text(longtests_roclet(), paste(c(
    "#' @longtests expect_identical(`has \"quotes\"`(), 1L)",
    "`has \"quotes\"` <- function() 1L"
  ), collapse = "\n"))
  case <- parse(text = out$longtests[[1L]])[[1L]]
  expect_match(case[[2L]], 'has "quotes"', fixed = TRUE)
  env <- new.env()
  env[["has \"quotes\""]] <- function() 1L
  eval(case[[3L]], envir = env)
})
