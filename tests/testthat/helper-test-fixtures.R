roxygenise_fixture <- function(..., load_code = roxygen2::load_source) {
  suppressMessages(roxygen2::roxygenise(..., load_code = load_code))
}

new_evidence_package <- function(source) {
  pkg <- tempfile("case evidence ")
  dir.create(file.path(pkg, "R"), recursive = TRUE)
  for (suite in c("tests", "longtests")) {
    dir.create(file.path(pkg, suite, "testthat"), recursive = TRUE)
    writeLines(c("library(testthat)", "library(evidencefixture)",
                 "test_check('evidencefixture')"), file.path(pkg, suite, "testthat.R"))
    writeLines("case_value <- function() answer()",
               file.path(pkg, suite, "testthat", "helper-value.R"))
    writeLines("original fixture", file.path(pkg, suite, "testthat", "fixture.txt"))
  }
  writeLines(c(
    "Package: evidencefixture", "Version: 0.0.1", "Encoding: UTF-8",
    "Suggests: fs, testthat", "Config/testthat/edition: 3",
    'Roxygen: list(roclets = "biocroxytest::tests_roclet")'
  ), file.path(pkg, "DESCRIPTION"))
  writeLines(character(), file.path(pkg, "NAMESPACE"))
  writeLines(source, file.path(pkg, "R", "answer.R"))
  roxygenise_fixture(pkg)
  pkg
}

check_generated_fixture <- function(path) {
  output <- withr::local_tempfile()
  on.exit({
    lines <- readLines(output, warn = FALSE)
    plain <- cli::ansi_strip(lines)
    chatter <- grepl(
      paste0(
        "^(First time using roxygen2[.] Upgrading automatically[.]{3}|",
        ". Setting Config/roxygen2/version to |. Loading [[:alnum:].]+$|",
        "Writing '[^']+[.]Rd'$)"
      ),
      plain
    )
    writeLines(lines[!chatter])
  }, add = TRUE, after = FALSE)
  withr::with_output_sink(output, check_generated_tests(path))
}
