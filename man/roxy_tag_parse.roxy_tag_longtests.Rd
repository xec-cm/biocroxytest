% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/longtests_rcolet.R
\name{roxy_tag_parse.roxy_tag_longtests}
\alias{roxy_tag_parse.roxy_tag_longtests}
\title{Parse a roxy_tag_longtests object}
\usage{
\method{roxy_tag_parse}{roxy_tag_longtests}(x)
}
\arguments{
\item{x}{An object of class 'roxy_tag_longtests'.}
}
\value{
The input object with the 'val' field updated.
}
\description{
This function parses a 'roxy_tag_longtests' object. It first checks if the
object inherits from 'roxy_tag_longtests' and 'roxy_tag'. If not, it aborts
with an error message. Then, it checks if the 'raw' field of the object is
empty. If it is, it returns a warning that a value is required. Finally, it
removes any leading newline characters from the 'raw' field and assigns the
result to the 'val' field of the object.
}
\keyword{internal}
