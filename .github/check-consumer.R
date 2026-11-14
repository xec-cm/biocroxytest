check_consumer <- function() {
  scratch <- withr::local_tempdir("biocroxytest-consumer-")
  package <- "biocroxytestconsumer"
  consumer <- file.path(scratch, package)
  dir.create(file.path(consumer, "R"), recursive = TRUE)
  dir.create(file.path(consumer, "tests", "testthat"), recursive = TRUE)
  dir.create(file.path(consumer, "inst", "extdata"), recursive = TRUE)
  writeBin(charToRaw("3\n6\n"), file.path(consumer, "inst", "extdata", "expected.txt"))
  source <- file.path(consumer, "R", "increment.R")
  stopifnot(file.copy(".github/consumer/increment.R", source))
  writeLines(c(
    "Package: biocroxytestconsumer",
    "Title: Check Generated Tests in an Installed Package",
    "Version: 0.0.1",
    'Authors@R: person("CI", "Tester", email = "ci@example.org", role = c("aut", "cre"))',
    "Description: A small fixture for checking generated ordinary and long tests.",
    "License: GPL (>= 3)",
    "Encoding: UTF-8",
    "Suggests: fs, testthat (>= 3.3.0)",
    "Config/testthat/edition: 3",
    'Roxygen: list(roclets = c("namespace", "rd", "biocroxytest::tests_roclet"))'
  ), file.path(consumer, "DESCRIPTION"))
  writeLines(c("^\\.BBSoptions$", "^r/EXCLUDED[.]r$"), file.path(consumer, ".Rbuildignore"))
  writeLines(c(
    "library(testthat)",
    "library(biocroxytestconsumer)",
    'stopifnot(length(find.package(c("biocroxytest", "roxytest"), quiet = TRUE)) == 0L)',
    'test_check("biocroxytestconsumer")'
  ), file.path(consumer, "tests", "testthat.R"))
  pkgload::load_all(quiet = TRUE, helpers = FALSE, export_all = FALSE)
  withr::with_dir(consumer, biocroxytest::use_longtests())
  writeLines("suite_input <- function() 2L",
             file.path(consumer, "tests", "testthat", "helper-input.R"))
  writeLines("suite_input <- function() 5L",
             file.path(consumer, "longtests", "testthat", "helper-input.R"))
  writeLines(c(
    "#' @tests stop('Excluded code reached the ordinary suite')",
    "#' @longtests stop('Excluded code reached the long suite')",
    "excluded <- function() TRUE"
  ), file.path(consumer, "R", "excluded.R"))
  roxygen2::roxygenise(consumer)
  ids <- c("increment/ordinary", "increment/long")
  evidence <- biocroxytest::run_test_plan(consumer, ids = ids, seed = 42L)
  biocroxytest::check_test_plan(evidence, required = ids)
  fixture <- file.path(consumer, "inst", "extdata", "expected.txt")
  writeBin(charToRaw("3\n6\n7\n"), fixture)
  changed <- biocroxytest::run_test_plan(consumer, ids = ids, seed = 42L)
  stopifnot(all(changed$cases$status == "failed"), !identical(changed$revision, evidence$revision))
  writeBin(charToRaw("3\n6\n"), fixture)

  # Stage only the consumer's test dependency and its required dependencies.
  consumer_lib <- file.path(scratch, "library")
  dir.create(consumer_lib)
  dependencies <- tools::package_dependencies(
    c("fs", "testthat"), db = utils::installed.packages(),
    which = c("Depends", "Imports", "LinkingTo"), recursive = TRUE
  )
  for (dependency in unique(c(names(dependencies), unlist(dependencies, use.names = FALSE)))) {
    installed <- find.package(dependency)
    if (dirname(installed) == .Library) { next }
    stopifnot(file.copy(installed, consumer_lib, recursive = TRUE))
  }
  empty <- file.path(scratch, "empty-startup")
  file.create(empty)
  env <- c(callr::rcmd_safe_env(),
           R_ENVIRON = empty, R_ENVIRON_USER = empty,
           R_PROFILE = empty, R_PROFILE_USER = empty,
           R_LIBS_SITE = "NULL", "_R_CHECK_CRAN_INCOMING_" = "false")

  for (broken in c(FALSE, TRUE)) {
    if (broken) {
      lines <- readLines(source, warn = FALSE)
      original <- "increment <- function(x) x + 1L"
      stopifnot(sum(lines == original) == 1L)
      lines[lines == original] <- "increment <- function(x) x + 2L"
      writeLines(lines, source)
    }
    build <- callr::rcmd_safe("build", consumer, wd = scratch, show = TRUE,
                              fail_on_status = TRUE)
    tarball <- file.path(scratch, "biocroxytestconsumer_0.0.1.tar.gz")
    stopifnot(build$status == 0L, file.exists(tarball))
    contents <- utils::untar(tarball, list = TRUE)
    stopifnot(!any(endsWith(contents, "excluded.R")))

    for (suite in c("tests", "longtests")) {
      work <- file.path(scratch, paste(suite, broken, sep = "-"))
      dir.create(work)
      test_dir <- if (suite == "longtests") "--test-dir=longtests" else character()
      checked <- callr::rcmd_safe(
        "check", c("--no-manual", test_dir, tarball),
        wd = work, libpath = consumer_lib, env = env, show = TRUE
      )
      check_dir <- file.path(work, paste0(package, ".Rcheck"))
      log_path <- file.path(check_dir, "tests", "testthat.Rout")
      if (broken) { log_path <- paste0(log_path, ".fail") }
      if (!file.exists(log_path)) {
        stop("Missing testthat log for ", suite, " (broken = ", broken, ").")
      }
      log <- readLines(log_path, warn = FALSE)
      writeLines(log)
      expected <- if (suite == "tests") 3L else 2L
      result <- if (broken) {
        sprintf("FAIL %d | WARN 0 | SKIP 0 | PASS 2", expected)
      } else {
        sprintf("FAIL 0 | WARN 0 | SKIP 0 | PASS %d", expected + 2L)
      }
      stopifnot((checked$status != 0L) == broken,
                any(grepl(result, log, fixed = TRUE)))
    }
    unlink(tarball)
  }
}

check_consumer()
