
<!-- README.md is generated from README.Rmd. Please edit that file -->

# biocroxytest <a href="https://xec-cm.github.io/biocroxytest/"><img src="man/figures/logo.png" align="right" height="138" alt="biocroxytest website" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/xec-cm/biocroxytest/workflows/R-CMD-check-bioc/badge.svg)](https://github.com/xec-cm/biocroxytest/actions)
[![PRs
Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg?style=flat-square)](https://makeapullrequest.com)
[![GitHub
issues](https://img.shields.io/github/issues/xec-cm/biocroxytest)](https://github.com/xec-cm/biocroxytest/issues)
[![GitHub
pulls](https://img.shields.io/github/issues-pr/xec-cm/biocroxytest)](https://github.com/xec-cm/biocroxytest/pulls)
<!-- badges: end -->

Write ordinary and Bioconductor long tests alongside the code they
cover. `biocroxytest` turns `@tests` and `@longtests` tags into standard
testthat files. The approach builds on
*[roxytest](https://github.com/mikldk/roxytest)* and
*[roxygen2](https://github.com/r-lib/roxygen2)*.

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

## Example

Set up ordinary tests with `usethis::use_testthat()` and long tests with
`biocroxytest::use_longtests()`. Run the setup for the suites you use
and list `testthat` in Suggests. Add the generator to your existing
roxygen configuration in DESCRIPTION.

    Roxygen: list(roclets = c("namespace", "rd", "biocroxytest::tests_roclet"))

Write the body of each test in a tag. For example, in `R/functions.R`:

``` r
#' A function to do x
#' 
#' @param x A number
#' 
#' @tests
#' expect_equal(foo(0), 0)
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

Run `roxygen2::roxygenise()`. The ordinary tests go to
`tests/testthat/test-roxytest-tests-functions.R` and the long tests go
to `longtests/testthat/test-biocroxytest-longtests-functions.R`. Each
suite uses its own testthat runner and helpers. The package only needs
testthat to run the generated tests.

For a long-test-only configuration, continue using
`biocroxytest::longtests_roclet`.

## Migrate from roxytest

Preview the migration in a clean branch or disposable copy. Replace
`roxytest::testthat_roclet` and, if present,
`biocroxytest::longtests_roclet` with `biocroxytest::tests_roclet`.
Regenerate and inspect the diff, then run your test suites. The
generator retains the `test-roxytest-tests-` prefix and updates the
ownership header. Use the default `clean = FALSE` with custom roclets.
The unified roclet cleans up obsolete generated tests after validating
both suites on every run.

For shared `@rdname` topics, each tag follows its own source file. If
roxytest previously merged those blocks, a test can move or change
description. Review and migrate any affected snapshots before adopting
the generated files. To revert a migration, restore both the previous
configuration and its generated files. Packages using roxytest’s
`@testexamples` can keep its roclet alongside `longtests_roclet` until
those tests have been migrated.

## Share a test across named cases

With `biocroxytest::tests_roclet`, put a literal list in an `@testCases`
tag. Each case chooses a suite and can prepare its own inputs. The
shared body runs once per case.

``` r
#' @testCases arithmetic
#' list(
#'   cases = list(
#'     small = list(suite = "tests", setup = { x <- 2L }),
#'     large = list(suite = "longtests", setup = { x <- 1000L })
#'   ),
#'   test = { expect_identical(increment(x), x + 1L) }
#' )
increment <- function(x) x + 1L
```

``` r
before <- biocroxytest::test_case_plan()
roxygen2::roxygenise()
biocroxytest::run_test_case("arithmetic/large")

# After editing the declarations:
biocroxytest::compare_test_plans(before, biocroxytest::test_case_plan())
```

IDs use the group and case names and survive changes of file or suite.
The planner reads declarations without loading the consumer package. The
generator leaves fixture creation to test execution. The local runner
uses the selected suite’s testthat helpers, setup and teardown in a
temporary copy to protect existing snapshots. Run the full suite to
review snapshot changes and exercise any custom suite runner.

## Check generated tests in CI

List the generated files, their source files, and their suites with
`biocroxytest::generated_test_inventory()`. To check that those files
match the source, run this after installing the package’s development
dependencies:

``` sh
Rscript --vanilla -e 'biocroxytest::check_generated_tests()'
```

Run the command from the consumer package root, or pass its path to the
function. The check uses the configured biocroxytest roclet in a
temporary copy. It exits with an error if generated files are missing,
changed, or obsolete. Regenerate with `roxygen2::roxygenise()` and
commit the resulting changes. Manual tests and snapshots are excluded
from the comparison.

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

## Code of Conduct

Please note that the
*[biocroxytest](https://github.com/xec-cm/biocroxytest)* project is
released with a [Contributor Code of
Conduct](http://bioconductor.org/about/code-of-conduct/). By
contributing to this project, you agree to abide by its terms.
