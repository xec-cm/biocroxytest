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

#' Process blocks for 'longtests' roclet
#'
#' This function processes a list of blocks for the 'longtests' roclet. It calls
#' the 'internal_longtests_roclet_process' function with the specified blocks
#' and additional arguments. The code in the tests is indented, a 'test_that'
#' boilerplate is added to the tests, and a 'context' line is not added to the
#' header.
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
  internal_longtests_roclet_process(
    blocks = blocks,
    indent_code = TRUE,
    add_testthat_boilerplate = TRUE,
    add_context_header = FALSE
  )
}

#' Process blocks for 'longtests' roclet
#'
#' This function processes a list of blocks for the 'longtests' roclet. It calls
#' the 'internal_longtests_roclet_process' function with the specified blocks
#' and additional arguments. The code in the tests is indented, a 'test_that'
#' boilerplate is added to the tests, and a 'context' line is not added to the
#' header.
#'
#' @param x An object of class 'roclet_longtests'.
#'   package.
#' @param results A list with a single element 'longtests', which contains the
#' @param base_path A character string representing the base path of the
#' @param ... Additional arguments passed to the function.
#'
#' @return A list with a single element 'longtests', which contains the
#'   processed test files.
#' @importFrom roxygen2 roclet_output
#' @export
#' @examples
#' # Set up a temporary directory
#' base_path <- tempdir()
#' longtests_path <- file.path(base_path, "longtests")
#' unlink(longtests_path, recursive = TRUE, force = TRUE)
#' dir.create(longtests_path, recursive = TRUE, showWarnings = FALSE)
#'
#' # Create dummy inputs
#' obj <- longtests_roclet()
#' results <- list(longtests = list())
#'
#' # Run the roclet_output function
#' result <- roxygen2::roclet_output(obj, results, base_path)
roclet_output.roclet_longtests <- function(x, results, base_path, ...) {
  verify_longtests_used(base_path)

  roclet_clean.roclet_longtests(x, base_path)

  # Has side-effects: writes files to disk
  paths_longtests <- internal_longtests_roclet_output(
    results = results$longtests,
    base_path = normalizePath(
      file.path(base_path, "longtests", "testthat"),
      mustWork = FALSE
    ),
    prefix = "test-biocroxytest-longtests-"
  )

  paths <- c(paths_longtests)

  paths
}

#' Clean up test files in the 'longtests' directory
#'
#' This function cleans up test files in the 'longtests' directory of a given
#' base path. It first verifies if 'longtests' is used in the package by calling
#' `verify_longtests_used()`. Then, it finds all test files in the 'longtests'
#' directory that match the pattern "test-biocroxytest-.*\\.R$". Finally, it
#' calls `internal_longtests_roclet_clean(testfiles)` to remove the test files
#' generated by biocroxytest.
#'
#' @param x An object of class 'roclet_longtests'.
#' @param base_path A character string representing the base path of the
#'   package.
#'
#' @return Invisible NULL.
#' @importFrom roxygen2 roclet_clean
#' @export
#' @examples
#' # Set up a temporary directory
#' base_path <- tempdir()
#' longtests_path <- file.path(base_path, "longtests")
#' unlink(longtests_path, recursive = TRUE, force = TRUE)
#' dir.create(longtests_path, recursive = TRUE, showWarnings = FALSE)
#'
#' # Create dummy inputs
#' obj <- longtests_roclet()
#'
#' # Run the roclet_output function
#' result <- roxygen2::roclet_clean(obj, base_path)
roclet_clean.roclet_longtests <- function(x, base_path) {
  verify_longtests_used(base_path)

  testfiles <- dir(
    path = file.path(base_path, "longtests", "testthat"),
    pattern = "^test-biocroxytest-.*\\.[Rr]$",
    full.names = TRUE
  )

  # Has side-effects: deletes files on disk
  internal_longtests_roclet_clean(testfiles)

  invisible(TRUE)
}
