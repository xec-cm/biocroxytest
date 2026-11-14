#' Increment an integer
#'
#' @param x An integer.
#' @return An integer.
#' @export
#' @examples
#' value <- increment(2L)
#' @examplesIf is.integer(value)
#' value <- increment(value)
#' @testexamples expect_identical(value, 4L)
#' @tests expect_identical(increment(suite_input()), 3L, label = "ordinary regression")
#' @longtests expect_identical(increment(suite_input()), 6L, label = "long regression")
#' @testCases increment
#' list(
#'   cases = list(
#'     ordinary = list(suite = "tests", setup = { expected <- 3L }),
#'     long = list(suite = "longtests", setup = { expected <- 6L })
#'   ),
#'   test = { expect_identical(increment(suite_input()), expected) }
#' )
increment <- function(x) x + 1L
