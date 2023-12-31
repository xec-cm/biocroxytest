% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_longtests_rcolet.R
\name{internal_longtests_roclet_process}
\alias{internal_longtests_roclet_process}
\title{Process blocks for 'longtests' roclet}
\usage{
internal_longtests_roclet_process(blocks, ...)
}
\arguments{
\item{blocks}{A list of roxygen2 blocks.}

\item{...}{Additional arguments.}
}
\value{
A list with a single element 'longtests', which contains the
processed test files.
}
\description{
This is an internal function that processes a list of blocks for the
'longtests' roclet. It first collects and annotates the 'rdname' of the
blocks. Then, it loops over each block and converts it to a testthat file
using the 'block_to_testthat' function. If the resulting testthat file has a
filename and contains 'longtests', it is added to the list of test files.
Finally, it processes the test files using the 'process_testfiles' function.
}
\keyword{internal}
