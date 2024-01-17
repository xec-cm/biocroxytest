
<!-- README.md is generated from README.Rmd. Please edit that file -->

# biocroxytest <a href="https://xec-cm.github.io/biocroxytest/"><img src="man/figures/logo.png" align="right" height="133" alt="biocroxytest website" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/xec-cm/biocroxytest/workflows/R-CMD-check-bioc/badge.svg)](https://github.com/xec-cm/biocroxytest/actions)
[![Codecov test
coverage](https://codecov.io/gh/xec-cm/biocroxytest/branch/devel/graph/badge.svg)](https://app.codecov.io/gh/xec-cm/biocroxytest?branch=devel)
[![PRs
Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg?style=flat-square)](https://makeapullrequest.com)
[![GitHub
issues](https://img.shields.io/github/issues/xec-cm/biocroxytest)](https://github.com/xec-cm/biocroxytest/issues)
[![GitHub
pulls](https://img.shields.io/github/issues-pr/xec-cm/biocroxytest)](https://github.com/xec-cm/biocroxytest/pulls)
<!-- badges: end -->

<br>

*[biocroxytest](https://github.com/xec-cm/biocroxytest)* is an R package
inspired by *[roxytest](https://github.com/mikldk/roxytest)*. It is
specifically designed for the development of Bioconductor packages that
require tests with high execution times.

This package extends the functionality of `roxytest` by introducing a
new roclet, `@longtests`. This innovative feature allows developers to
document and store these long tests directly in their `roxygen2`
comments.

With *[biocroxytest](https://github.com/xec-cm/biocroxytest)*,
developers can now write comprehensive tests without worrying about
slowing down the daily build process. The `@longtests` roclet provides a
dedicated space for these extensive tests, ensuring they are easily
accessible and well-documented. This approach not only improves the
reliability of the package but also enhances its maintainability.
Developers can easily locate, understand, and update these long tests as
needed, leading to more robust and efficient code.

<br>

## Installation instructions

Get the latest stable `R` release from
[CRAN](http://cran.r-project.org/). Then install `biocroxytest` from
[Bioconductor](http://bioconductor.org/) using the following code:

``` r
# Not yet on Bioconductor
# if (!requireNamespace("BiocManager", quietly = TRUE)) {
#     install.packages("BiocManager")
# }
# 
# BiocManager::install("biocroxytest")
```

And the development version from
[GitHub](https://github.com/xec-cm/biocroxytest) with:

``` r
BiocManager::install("xec-cm/biocroxytest")
```

<br>

## Example

Here is how you can use
*[biocroxytest](https://github.com/xec-cm/biocroxytest)* to create a new
package with long tests:

To use the package in your own package you do not need to add any
additional dependencies in your package’s DESCRIPTION file. Simply add
the following lines to your package’s DESCRIPTION file (along with
Suggests: testthat):

    Roxygen: list(roclets = c("namespace", "rd", "biocroxytest::longtests_roclet"))

Then add `@longtests` to the roxygen comments of the functions you want
to test. For example, if the file `R/functions.R` contains this code:

``` r
#' A function to do x
#' 
#' @param x A number
#' 
#' @longtests 
#' expect_equal(foo(2), sqrt(2))
#' expect_error(foo("a string"))
#' 
#' @return something
foo <- function(x) {
  return(sqrt(x))
}

#' A function to do y
#' 
#' @param x Character vector
#' @param y Character vector
#' 
#' @longtests 
#' expect_equal(bar("A", "B"), paste("A", "B", sep = "/"))
#' 
#' @export
bar <- function(x, y) {
  paste0(x, "/", y)
}
```

Then `roxygen2::roxygenise()` will generate (with the `longtests_roclet`
roclet) the file `longtests/test-biocroxytest-tests-functions.R` with
this content:

``` r
# Generated by biocroxytest: do not edit by hand!

# File R/functions.R: @longtests

test_that("Function foo() @ L11", {
  expect_equal(foo(2), sqrt(2))
  expect_error(foo("a string"))
})


test_that("Function bar() @ L27", {
  expect_equal(bar("A", "B"), paste("A", "B", sep = "/"))
})
```

<br>

## Contributing

- If you think you have encountered a bug, please [submit an
  issue](https://github.com/xec-cm/biocroxytest/issues).

- Either way, learn how to create and share a
  [reprex](https://reprex.tidyverse.org/articles/articles/learn-reprex.html)
  (a minimal, reproducible example), to clearly communicate about your
  code.

- Working on your first Pull Request? You can learn how from this *free*
  series [How to Contribute to an Open Source Project on
  GitHub](https://kcd.im/pull-request)

<br>

## Code of Conduct

Please note that the
*[biocroxytest](https://github.com/xec-cm/biocroxytest)* project is
released with a [Contributor Code of
Conduct](http://bioconductor.org/about/code-of-conduct/). By
contributing to this project, you agree to abide by its terms.
