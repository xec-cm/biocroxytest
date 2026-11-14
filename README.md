
<!-- README.md is generated from README.Rmd. Please edit that file. -->

# biocroxytest <a href="https://xec-cm.github.io/biocroxytest/"><img src="man/figures/logo.png" align="right" height="138" alt="biocroxytest logo" /></a>

<!-- badges: start -->

[![Bioconductor](https://img.shields.io/badge/Bioconductor-biocroxytest-blue)](https://www.bioconductor.org/packages/release/bioc/html/biocroxytest.html)
[![R-CMD-check](https://github.com/xec-cm/biocroxytest/workflows/R-CMD-check-bioc/badge.svg)](https://github.com/xec-cm/biocroxytest/actions)
<!-- badges: end -->

Write testthat checks next to the R functions they cover. biocroxytest
generates ordinary test files and a separate long-test suite for
Bioconductor packages. It builds on
[roxytest](https://github.com/mikldk/roxytest) and uses testthat to run
the generated tests.

**Start with one test.** Our [first
tutorial](https://xec-cm.github.io/biocroxytest/articles/biocroxytest.html)
uses a complete small package: select samples in a requested order,
generate a test, see it pass, then introduce a bug and see it fail. You
can copy the example without changing your own project.

## Choose your next step

| I want to… | Guide |
|----|----|
| Generate and run my first test | [First test](https://xec-cm.github.io/biocroxytest/articles/biocroxytest.html) |
| Apply the same assertions to memory and HDF5 inputs | [Named cases](https://xec-cm.github.io/biocroxytest/articles/named-cases.html) |
| Record a failed case and reproduce it | [Failure replay](https://xec-cm.github.io/biocroxytest/articles/replay-walkthrough.html) |
| Use checked input files | [Fixtures](https://xec-cm.github.io/biocroxytest/articles/test-fixtures.html) |
| Require cases in CI, compare runs or migrate from roxytest | [Workflow recipes](https://xec-cm.github.io/biocroxytest/articles/test-workflows.html) |
| Find smaller paired data that retain a specific failure | [Failure reduction](https://xec-cm.github.io/biocroxytest/articles/reduce-failures.html) |

The first three guides form a learning path. Named cases, execution
reports and replay are optional: ordinary `@tests` and `@longtests`
remain useful on their own. Generated tests run with standard testthat;
install biocroxytest where you generate them or use its optional
runners.

## Installation

### GitHub development build

**This site documents the [GitHub devel
branch](https://github.com/xec-cm/biocroxytest/tree/devel).** Use this
build for the tutorials and the unified ordinary/long-test workflow. It
requires R 4.4 or newer. The first tutorial includes installation
commands for its Bioconductor example dependencies.

``` r
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
BiocManager::install("xec-cm/biocroxytest@devel", update = FALSE)
```

### Bioconductor release

biocroxytest has been available through Bioconductor since 3.19. Install
the release compatible with your R version using BiocManager:

``` r
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
BiocManager::install("biocroxytest")
```

Use the [Bioconductor package
page](https://www.bioconductor.org/packages/release/bioc/html/biocroxytest.html)
and [release
vignette](https://www.bioconductor.org/packages/release/bioc/vignettes/biocroxytest/inst/doc/biocroxytest.html)
for that build; development features may not yet be available there.

## How it fits into a package

Add `@tests` or `@longtests` to a function’s roxygen comments. Configure
`biocroxytest::tests_roclet` alongside your existing roclets in
`DESCRIPTION`, then run `roxygen2::roxygenise()`. Commit the generated
tests with the source and run the appropriate suite.

| Declaration | Destination | Execution |
|----|----|----|
| `@tests` | `tests/testthat/` | `devtools::test()` or the ordinary package check |
| `@longtests` | `longtests/testthat/` | Separate long-test check |
| `@testCases` | The suite chosen by each named case | Standard testthat or selection by case ID |

The [first
tutorial](https://xec-cm.github.io/biocroxytest/articles/biocroxytest.html)
shows the full setup, generated file and passing/failing output. For
complete arguments and return values, use the [function
reference](https://xec-cm.github.io/biocroxytest/reference/index.html).

## Contributing

Report bugs or propose changes through [GitHub
Issues](https://github.com/xec-cm/biocroxytest/issues). Include a
[minimal reproducible
example](https://reprex.tidyverse.org/articles/articles/learn-reprex.html)
when reporting a failure. Contributions follow the [Bioconductor Code of
Conduct](https://bioconductor.org/about/code-of-conduct/).
