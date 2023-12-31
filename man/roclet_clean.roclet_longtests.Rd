% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/longtests_rcolet.R
\name{roclet_clean.roclet_longtests}
\alias{roclet_clean.roclet_longtests}
\title{Clean up test files in the 'longtests' directory}
\usage{
\method{roclet_clean}{roclet_longtests}(x, base_path)
}
\arguments{
\item{x}{An object of class 'roclet_longtests'.}

\item{base_path}{A character string representing the base path of the
package.}
}
\value{
Invisible NULL.
}
\description{
This function cleans up test files in the 'longtests' directory of a given
base path. It first verifies if 'longtests' is used in the package by calling
\code{verify_longtests_used()}. Then, it finds all test files in the 'longtests'
directory that match the pattern "test-biocroxytest-.*\\.R$". Finally, it
calls \code{internal_longtests_roclet_clean(testfiles)} to remove the test files
generated by biocroxytest.
}
\examples{
# Set up a temporary directory
base_path <- tempdir()
longtests_path <- file.path(base_path, "longtests")
unlink(longtests_path, recursive = TRUE, force = TRUE)
dir.create(longtests_path, recursive = TRUE, showWarnings = FALSE)

# Create dummy inputs
obj <- longtests_roclet()

# Run the roclet_output function
result <- roxygen2::roclet_clean(obj, base_path)
}
