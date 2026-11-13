new_generated_test_fixture <- function(
    roclets = 'c("rd", "biocroxytest::tests_roclet")') {
  pkg <- tempfile("generated tests ")
  dir.create(file.path(pkg, "R"), recursive = TRUE)
  dir.create(file.path(pkg, "tests", "testthat"), recursive = TRUE)
  dir.create(file.path(pkg, "longtests", "testthat"), recursive = TRUE)
  writeLines(c(
    "Package: generatedtestfixture",
    "Version: 0.0.1",
    "Encoding: UTF-8",
    paste0("Roxygen: list(roclets = ", roclets, ")")
  ), file.path(pkg, "DESCRIPTION"))
  writeLines(character(), file.path(pkg, "NAMESPACE"))
  pkg
}

generated_test_bytes <- function(path) {
  readBin(path, "raw", n = file.size(path))
}

generated_test_fixture_state <- function(pkg) {
  entries <- list.files(pkg, all.files = TRUE, no.. = TRUE, recursive = TRUE,
                        include.dirs = TRUE)
  paths <- file.path(pkg, entries)
  setNames(lapply(paths, function(path) {
    if (dir.exists(path)) NULL else generated_test_bytes(path)
  }), entries)
}

test_that("the inventory lists owned tests by suite and source", {
  pkg <- new_generated_test_fixture()
  on.exit(unlink(pkg, recursive = TRUE, expand = FALSE), add = TRUE)
  expected <- data.frame(
    suite = c("tests", "tests", "longtests"),
    source = c("R/a.R", "R/z.r", "R/a.R"),
    path = c("tests/testthat/test-roxytest-tests-a.R",
             "tests/testthat/test-roxytest-tests-z.r",
             "longtests/testthat/test-biocroxytest-longtests-a.R")
  )
  expect_identical(generated_test_inventory(pkg), expected[0, ])

  writeLines(roxytest_header, file.path(pkg, expected$path[2L]))
  writeLines(generated_test_header, file.path(pkg, expected$path[3L]))
  writeLines(generated_test_header, file.path(pkg, expected$path[1L]))
  writeLines(paste(roxytest_header, "(manual)"),
             file.path(pkg, "tests/testthat/test-roxytest-tests-manual.R"))
  writeLines(roxytest_header, file.path(pkg, "tests/testthat/test-roxytest-testexamples-a.R"))
  writeLines(generated_test_header, file.path(pkg, "tests/testthat/helper-a.R"))
  manual <- file.path(pkg, "tests/testthat/test-roxytest-tests-manual.R")
  for (suffix in list(as.raw(c(195L, 169L)), as.raw(0L), as.raw(255L))) {
    writeBin(c(charToRaw(generated_test_header), suffix, as.raw(10L)), manual)
    expect_false(made_by_biocroxytest(manual, "tests"))
  }
  dir.create(file.path(pkg, "tests/testthat/_snaps"))
  writeLines(generated_test_header,
             file.path(pkg, "tests/testthat/_snaps/test-roxytest-tests-a.R"))
  writeLines(roxytest_header,
             file.path(pkg, "longtests/testthat/test-biocroxytest-longtests-manual.R"))

  before <- generated_test_fixture_state(pkg)
  expect_identical(generated_test_inventory(pkg), expected)
  expect_identical(generated_test_fixture_state(pkg), before)
})

test_that("freshness checks regenerate in a fresh process and leave the source untouched", {
  pkg <- new_generated_test_fixture()
  on.exit(unlink(pkg, recursive = TRUE, expand = FALSE), add = TRUE)
  a <- file.path(pkg, "R/a.R")
  a_code <- c("#' Fixture A", "#' @tests stop('The checker must not run test bodies')",
              "#' @longtests expect_identical(a(), 1L)", "a <- function() 1L")
  writeLines(a_code, a)
  old <- file.path(pkg, "R/old.R")
  writeLines(c("#' @longtests expect_true(old())", "old <- function() TRUE"), old)
  roxygen2::roxygenise(pkg, load_code = roxygen2::load_source)

  # Fail if the checker loads the original package or copies its Git metadata.
  source_dir <- encodeString(normalizePath(pkg, winslash = "/"), quote = '"')
  writeLines(c(
    sprintf("stopifnot(Sys.getpid() != %dL)", Sys.getpid()),
    sprintf('stopifnot(normalizePath(".", winslash = "/") != %s)', source_dir),
    'stopifnot(!file.exists(".git"))'
  ), file.path(pkg, "R/process-check.R"))
  writeLines("gitdir: fake-worktree", file.path(pkg, ".git"))
  writeLines("# Manual test", file.path(pkg, "tests/testthat/test-manual.R"))
  writeLines(roxytest_header, file.path(pkg, "tests/testthat/test-roxytest-testexamples-a.R"))

  cwd <- getwd()
  before <- generated_test_fixture_state(pkg)
  expect_identical(withVisible(check_generated_tests(pkg)), list(value = TRUE, visible = FALSE))
  expect_identical(generated_test_fixture_state(pkg), before)
  expect_identical(getwd(), cwd)

  a_code[1:2] <- c("#' Updated fixture A", "#' @tests expect_identical(a(), 1L)")
  writeLines(a_code, a)
  writeLines(c("#' @longtests expect_true(b())", "b <- function() TRUE"), file.path(pkg, "R/b.R"))
  unlink(old)
  before <- generated_test_fixture_state(pkg)
  stale <- expect_error(check_generated_tests(pkg), class = "biocroxytest_stale_tests")
  expect_identical(stale$changes, data.frame(
    status = c("missing", "changed", "obsolete"),
    suite = c("longtests", "tests", "longtests"),
    source = c("R/b.R", "R/a.R", "R/old.R"),
    path = c("longtests/testthat/test-biocroxytest-longtests-b.R",
             "tests/testthat/test-roxytest-tests-a.R",
             "longtests/testthat/test-biocroxytest-longtests-old.R")
  ))
  expect_identical(generated_test_fixture_state(pkg), before)
  expect_identical(getwd(), cwd)
})

test_that("freshness normalizes line endings and detects other byte differences", {
  pkg <- new_generated_test_fixture(
    roclets = 'deparse(quote(getExportedValue("biocroxytest", "longtests_roclet")))'
  )
  on.exit(unlink(pkg, recursive = TRUE, expand = FALSE), add = TRUE)
  writeLines(c("#' @longtests expect_true(a())", "a <- function() TRUE"), file.path(pkg, "R/a.R"))
  ordinary <- file.path(pkg, "tests/testthat/test-roxytest-tests-retain.R")
  writeLines(roxytest_header, ordinary)
  roxygen2::roxygenise(pkg, load_code = roxygen2::load_source)
  relative <- "longtests/testthat/test-biocroxytest-longtests-a.R"
  path <- file.path(pkg, relative)
  original <- rawToChar(generated_test_bytes(path))
  lf <- gsub("\r\n", "\n", original, fixed = TRUE)
  converted <- if (identical(original, lf)) gsub("\n", "\r\n", lf, fixed = TRUE) else lf
  writeBin(charToRaw(converted), path)
  expect_true(check_generated_tests(pkg))
  expect_identical(readLines(ordinary), roxytest_header)
  expect_identical(generated_test_bytes(path), charToRaw(converted))

  without_final_newline <- charToRaw(sub("\n+$", "", lf))
  with_bom <- c(as.raw(c(0xef, 0xbb, 0xbf)), charToRaw(lf))
  for (bytes in list(without_final_newline, with_bom)) {
    writeBin(bytes, path)
    stale <- expect_error(check_generated_tests(pkg), class = "biocroxytest_stale_tests")
    expect_identical(stale$changes$status, "changed")
    expect_identical(stale$changes$path, relative)
    expect_identical(generated_test_bytes(path), bytes)
  }
})

test_that("the inventory and checker do not follow linked test files", {
  skip_on_os("windows")
  pkg <- new_generated_test_fixture()
  on.exit(unlink(pkg, recursive = TRUE, expand = FALSE), add = TRUE)
  target <- tempfile("linked test ", fileext = ".R")
  on.exit(unlink(target), add = TRUE)
  writeLines(generated_test_header, target)
  linked <- file.path(pkg, "tests/testthat/test-roxytest-tests-a.R")
  if (!isTRUE(suppressWarnings(file.symlink(target, linked)))) {
    skip("Cannot create a file link")
  }

  expect_true(file.exists(linked))
  expect_equal(nrow(generated_test_inventory(pkg)), 0L)
  expect_error(check_generated_tests(pkg), class = "biocroxytest_test_check_input")
  expect_identical(readLines(target), generated_test_header)
})

test_that("the inventory and checker do not follow linked test directories", {
  pkg <- new_generated_test_fixture()
  on.exit(unlink(pkg, recursive = TRUE, expand = FALSE), add = TRUE)
  target <- tempfile("linked tests ")
  dir.create(target)
  on.exit(unlink(target, recursive = TRUE, expand = FALSE), add = TRUE)
  filename <- "test-roxytest-tests-a.R"
  writeLines(generated_test_header, file.path(target, filename))
  linked <- file.path(pkg, "tests", "testthat")
  unlink(linked, recursive = TRUE)
  link_dir <- if (.Platform$OS.type == "windows") get("Sys.junction", baseenv()) else file.symlink
  if (!isTRUE(suppressWarnings(link_dir(normalizePath(target), linked)))) {
    skip("Cannot create a directory link")
  }

  expect_true(file.exists(file.path(linked, filename)))
  expect_equal(nrow(generated_test_inventory(pkg)), 0L)
  expect_error(check_generated_tests(pkg), class = "biocroxytest_test_check_input")
  expect_identical(readLines(file.path(target, filename)), generated_test_header)
})

test_that("freshness requires a configured biocroxytest roclet", {
  pkg <- new_generated_test_fixture(roclets = '"rd"')
  on.exit(unlink(pkg, recursive = TRUE, expand = FALSE), add = TRUE)
  expect_error(check_generated_tests(pkg), class = "biocroxytest_test_check_input")
})

test_that("regeneration failures retain their cause and leave the source untouched", {
  pkg <- new_generated_test_fixture()
  on.exit(unlink(pkg, recursive = TRUE, expand = FALSE), add = TRUE)
  writeLines(c("#' @tests expect_true(", "a <- function() TRUE"), file.path(pkg, "R/a.R"))
  before <- generated_test_fixture_state(pkg)

  failure <- expect_error(check_generated_tests(pkg), class = "biocroxytest_test_regeneration_error")
  expect_s3_class(failure$parent, "error")
  expect_match(conditionMessage(failure$parent), "Invalid generated tests for .*a[.]R")
  expect_identical(generated_test_fixture_state(pkg), before)
})
