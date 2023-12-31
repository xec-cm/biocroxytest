% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/longtests_rcolet.R
\name{roclet_process.roclet_longtests}
\alias{roclet_process.roclet_longtests}
\title{Process blocks for 'longtests' roclet}
\usage{
\method{roclet_process}{roclet_longtests}(x, blocks, env, base_path)
}
\arguments{
\item{x}{An object of class 'roclet_longtests'.}

\item{blocks}{A list of roxygen2 blocks.}

\item{env}{The environment in which the roxygen2 blocks were parsed.}

\item{base_path}{A character string representing the base path of the
package.}
}
\value{
A list with a single element 'longtests', which contains the
processed test files.
}
\description{
This function processes a list of blocks for the 'longtests' roclet. It calls
the 'internal_longtests_roclet_process' function with the specified blocks
and additional arguments. The code in the tests is indented, a 'test_that'
boilerplate is added to the tests, and a 'context' line is not added to the
header.
}
\examples{
# Create a dummy block function
block <- function(roclet, value) {
  list(roclet = roclet, value = value)
}

# Create a new roclet_longtests object
x <- longtests_roclet()

# Define some roxygen2 blocks
blocks <- list(
  block(
    roclet = "longtests",
    value = list(
      raw = "test_that('this is a test', { expect_equal(1, 1) })"
    )
  )
)

blocks <- structure(blocks, class = "roclet_longtests")

# Define the environment and base path
env <- globalenv()
base_path <- tempdir()

# Process the blocks
roxygen2::roclet_process(x, blocks, env, base_path)
}
