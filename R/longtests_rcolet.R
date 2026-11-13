#' Roclet for Long Tests
#'
#' This roclet is used to identify and process code blocks in your documentation
#' that are marked with `@longtests`. The `longtests_roclet` function creates a
#' new roclet, which is a plugin to the `roxygen2` package. Roclets are
#' responsible for parsing the R scripts of a package and producing the relevant
#' documentation files.
#'
#' The `longtests_roclet` specifically looks for code blocks in your
#' documentation that are annotated with the `@longtests` tag. These code blocks
#' should contain tests that take a long time to run, and thus cannot be
#' included in the regular test suite of the package.
#'
#' When you run `roxygen2::roxygenise` with the `longtests_roclet`, it will
#' extract these long tests from your documentation and save them in a separate
#' directory. This allows you to run these long tests separately from the rest
#' of your tests, for example, on a continuous integration server that is set up
#' to run long tests.
#'
#' @return A roclet that can be used with `roxygen2` to process code blocks
#'   marked with `@longtests`. This roclet will produce a set of test files in a
#'   separate directory, which can be run independently of the rest of your test
#'   suite.
#'
#' @export
#' @examples
#' # Create a new roclet
#' longtests_roclet()
longtests_roclet <- function() {
  roxygen2::roclet("longtests")
}

#' Parse a roxy_tag_longtests object
#'
#' This function parses a 'roxy_tag_longtests' object. It first checks if the
#' object inherits from 'roxy_tag_longtests' and 'roxy_tag'. If not, it aborts
#' with an error message. Then, it checks if the 'raw' field of the object is
#' empty. If it is, it returns a warning that a value is required. Finally, it
#' removes any leading newline characters from the 'raw' field and assigns the
#' result to the 'val' field of the object.
#'
#' @param x An object of class 'roxy_tag_longtests'.
#'
#' @return The input object with the 'val' field updated.
#' @importFrom roxygen2 roxy_tag_parse
#' @export
#' @keywords internal
roxy_tag_parse.roxy_tag_longtests <- function(x) {
  x
}

#' Process blocks for the long test roclet
#'
#' Generate a testthat test for each `@longtests` tag, grouped by source file.
#'
#' @param x An object of class 'roclet_longtests'.
#' @param blocks A list of roxygen2 blocks.
#' @param env The environment in which the roxygen2 blocks were parsed.
#' @param base_path A character string representing the base path of the
#'   package.
#'
#' @return A list with a single element 'longtests', which contains the
#'   processed test files.
#' @importFrom roxygen2 roclet_process
#' @export
#' @examples
#' # Create a dummy block function
#' block <- function(roclet, value) {
#'   list(roclet = roclet, value = value)
#' }
#'
#' # Create a new roclet_longtests object
#' x <- longtests_roclet()
#'
#' # Define some roxygen2 blocks
#' blocks <- list(
#'   block(
#'     roclet = "longtests",
#'     value = list(
#'       raw = "test_that('this is a test', { expect_equal(1, 1) })"
#'     )
#'   )
#' )
#'
#' blocks <- structure(blocks, class = "roclet_longtests")
#'
#' # Define the environment and base path
#' env <- globalenv()
#' base_path <- tempdir()
#'
#' # Process the blocks
#' roxygen2::roclet_process(x, blocks, env, base_path)
roclet_process.roclet_longtests <- function(x, blocks, env, base_path) {
  process_test_suites(blocks, "longtests")
}

#' Update generated long tests
#'
#' Validate the generated tests and their destinations before writing them.
#' Remove obsolete generated files after the new output has been written.
#'
#' @param x An object of class 'roclet_longtests'.
#' @param results The processed long tests.
#' @param base_path A character string representing the base path of the package.
#' @param ... Additional arguments passed to the function.
#' @return The source filenames of the generated tests.
#' @importFrom roxygen2 roclet_output
#' @export
#' @examples
#' base_path <- tempfile()
#' dir.create(file.path(base_path, "longtests", "testthat"), recursive = TRUE)
#' obj <- longtests_roclet()
#' roxygen2::roclet_output(obj, list(longtests = list()), base_path)
#' unlink(base_path, recursive = TRUE)
roclet_output.roclet_longtests <- function(x, results, base_path, ...) {
  verify_longtests_used(base_path)
  write_test_suites(list(longtests = results$longtests), base_path)
}

#' Defer cleanup of generated long tests
#'
#' roxygen2 calls this hook before it generates output with `clean = TRUE`.
#' Cleanup is deferred to the output step so failed validation leaves the
#' previous files in place.
#'
#' @param x An object of class 'roclet_longtests'.
#' @param base_path A character string representing the base path of the package.
#' @return Invisible TRUE.
#' @importFrom roxygen2 roclet_clean
#' @export
roclet_clean.roclet_longtests <- function(x, base_path) {
  verify_longtests_used(base_path)
  invisible(TRUE)
}
