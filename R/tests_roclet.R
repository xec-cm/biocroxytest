#' Generate ordinary and long testthat tests
#'
#' Add `"biocroxytest::tests_roclet"` to the roxygen roclets in DESCRIPTION.
#' Write test bodies in `@tests` for ordinary tests or `@longtests` for
#' Bioconductor long tests. Each tag becomes a separate testthat test. Use
#' `usethis::use_testthat()` and [use_longtests()] to set up the suites you use.
#' Use `@testCases` to share a test body across named cases. See [test_case_plan()].
#' Use `@testexamples` to run documented examples before testing their results.
#'
#' To migrate `@tests` or `@testexamples` from roxytest, replace its testthat roclet with this
#' roclet. It retains the roxytest filename prefix and updates ownership
#' headers. Tests in shared `@rdname` topics follow their original source files,
#' so review any changed test names and snapshot paths after regenerating.
#' Use a single generator for each suite. [longtests_roclet()] remains available
#' for projects that want only the long test generator.
#'
#' With custom roclets, use the default `clean = FALSE`. The generator
#' removes obsolete tests after validating both suites on every run.
#' Test output follows the case-insensitive `.Rbuildignore` rules. A direct
#' roxygen2 run still uses its configured package loader and may load ignored
#' sources before the test roclet runs.
#'
#' @return A roxygen2 roclet.
#' @export
#' @examples
#' tests_roclet()
tests_roclet <- function() {
  check_test_roclets(resolved = FALSE)
  ensure_test_tag_parser("roxy_tag_tests", roxy_tag_parse.roxy_tag_tests)
  ensure_test_tag_parser("roxy_tag_testexamples", roxy_tag_parse.roxy_tag_testexamples)
  roxygen2::roclet("biocroxytest_tests")
}

#' @importFrom roxygen2 roxy_tag_parse
#' @export
roxy_tag_parse.roxy_tag_tests <- function(x) {
  if (!nzchar(trimws(x$raw))) {
    return(roxygen2::roxy_tag_warning(x, "requires a value"))
  }
  # Keep the value roxytest expects if its roclet is used separately.
  x$val <- sub("^\n", "", x$raw)
  x
}

ensure_test_tag_parser <- function(class, parser) {
  # pkgload can remove the shared entry when unloading roxytest. Look at the
  # generic's registration table, since the method is still visible here.
  registry <- get(".__S3MethodsTable__.", asNamespace("roxygen2"), inherits = FALSE)
  if (!exists(paste0("roxy_tag_parse.", class), envir = registry, inherits = FALSE)) {
    registerS3method("roxy_tag_parse", class, parser, envir = environment())
  }
}

#' @importFrom roxygen2 roclet_process
#' @export
roclet_process.roclet_biocroxytest_tests <- function(x, blocks, env, base_path) {
  check_test_roclets(resolved = TRUE)
  blocks <- select_test_blocks(blocks, base_path, env)
  cases <- collect_test_cases(blocks, check_origin = TRUE)
  results <- process_test_suites(blocks, c("tests", "longtests"), cases)
  results$testexamples <- process_test_examples(blocks, env, base_path)
  results
}

#' @importFrom roxygen2 roclet_output
#' @export
roclet_output.roclet_biocroxytest_tests <- function(x, results, base_path, ...) {
  write_test_suites(results, base_path)
}

#' @importFrom roxygen2 roclet_clean
#' @export
roclet_clean.roclet_biocroxytest_tests <- function(x, base_path) {
  invisible(TRUE)
}

check_test_roclets <- function(resolved) {
  # Roxygen's metadata reflects DESCRIPTION even when roclets are overridden.
  # Read the active call so explicit overrides work and clean = TRUE can be
  # rejected before a competing roclet's cleanup touches either suite.
  active <- sys.nframe()
  while (active > 0L && !identical(sys.function(active), roxygen2::roxygenise)) {
    active <- active - 1L
  }
  if (!active) { return(invisible(NULL)) }
  frame <- sys.frame(active)
  roclets <- get("roclets", envir = frame, inherits = FALSE)
  if (resolved) {
    unified <- vapply(roclets, inherits, logical(1), "roclet_biocroxytest_tests")
    legacy <- vapply(roclets, function(x) {
      inherits(x, c("roclet_longtests", "roclet_testthat"))
    }, logical(1))
    conflict <- sum(unified) > 1L || (any(unified) && any(legacy))
  } else {
    names <- vapply(roclets, roclet_reference, character(1))
    legacy <- c("biocroxytest::longtests_roclet", "longtests_roclet",
                "roxytest::testthat_roclet", "testthat_roclet")
    conflict <- any(names %in% legacy)
    if (!conflict && isTRUE(get("clean", envir = frame, inherits = FALSE))) {
      safe <- c("rd", "namespace", "vignette", "roxygen2::rd_roclet",
                "roxygen2::namespace_roclet", "roxygen2::vignette_roclet",
                "biocroxytest::tests_roclet")
      if (any(!names %in% safe)) {
        cli::cli_abort(c(
          "Use {.code clean = FALSE} with custom roclets.",
          i = "biocroxytest removes obsolete generated tests after validation."
        ), call = NULL)
      }
    }
  }
  if (conflict) {
    cli::cli_abort(c(
      "Use {.code biocroxytest::tests_roclet} alone for ordinary and long tests.",
      i = "Remove {.code roxytest::testthat_roclet} and {.code biocroxytest::longtests_roclet} from the same roxygen run."
    ), call = NULL)
  }
  invisible(NULL)
}

roclet_reference <- function(spec) {
  expr <- parse(text = spec, keep.source = FALSE)
  if (length(expr) != 1L) { return(NA_character_) }
  ref <- expr[[1L]]
  while (is.call(ref) && identical(ref[[1L]], as.name("("))) { ref <- ref[[2L]] }
  if (is.call(ref) && length(ref) == 1L) { ref <- ref[[1L]] }
  while (is.call(ref) && identical(ref[[1L]], as.name("("))) { ref <- ref[[2L]] }
  if (is.name(ref)) { return(as.character(ref)) }
  if (is.call(ref) && length(ref) == 3L &&
      (identical(ref[[1L]], as.name("::")) || identical(ref[[1L]], as.name(":::")))) {
    return(paste(as.character(ref[[2L]]), as.character(ref[[3L]]), sep = "::"))
  }
  NA_character_
}
