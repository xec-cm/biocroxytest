#' Require named cases to complete
#'
#' Check an execution report in CI. Every required ID must be present and
#' have status `completed`. By default, each required case must also have
#' recorded zero warnings. Warnings handled by the test, such as those
#' checked with [testthat::expect_warning()], do not count.
#'
#' @param evidence A result from [run_test_plan()].
#' @param required A nonempty character vector of unique case IDs.
#' @param allow_warnings Allow required cases to complete with warnings.
#'   The default, `FALSE`, requires warning counts for completed cases.
#'   For reports saved before warning counts were recorded, rerun the plan
#'   or explicitly set `allow_warnings = TRUE`.
#' @return Invisibly returns `TRUE` on success. Otherwise signals a
#'   `biocroxytest_incomplete_test_plan` error listing the required cases that
#'   did not meet the policy.
#' @export
check_test_plan <- function(evidence, required, allow_warnings = FALSE) {
  if (!is.character(required) || !is.null(dim(required)) || !length(required) ||
      anyNA(required) || any(!nzchar(required)) || anyDuplicated(required)) {
    cli::cli_abort("{.arg required} must contain at least one unique case ID.", call = NULL)
  }
  if (!identical(allow_warnings, TRUE) && !identical(allow_warnings, FALSE)) {
    cli::cli_abort("{.arg allow_warnings} must be TRUE or FALSE.", call = NULL)
  }
  cases <- if (is.list(evidence)) evidence[["cases"]] else NULL
  if (!is.data.frame(cases) || !is.character(cases[["id"]]) ||
      anyNA(cases$id) || anyDuplicated(cases$id) ||
      !is.character(cases[["status"]]) || anyNA(cases$status)) {
    cli::cli_abort("Supply execution evidence from {.fn run_test_plan}.", call = NULL)
  }

  selected <- match(required, cases$id)
  statuses <- cases$status[selected]
  reason <- ifelse(is.na(selected), "no result", statuses)
  completed <- !is.na(statuses) & statuses == "completed"
  reason[completed] <- NA_character_
  if (!allow_warnings) {
    counts <- cases[["warnings"]]
    if (!is.numeric(counts) || !is.null(dim(counts))) {
      counts <- rep(NA_integer_, nrow(cases))
    }
    counts <- counts[selected]
    reason[completed & is.na(counts)] <- "warning count unavailable"
    warned <- which(completed & !is.na(counts) & counts != 0)
    reason[warned] <- paste(counts[warned], "distinct warning message(s)")
  }
  problems <- !is.na(reason)
  if (any(problems)) {
    details <- sprintf("{required[%d]}: {reason[%d]}", which(problems), which(problems))
    cli::cli_abort(
      c("Required test cases did not meet the execution policy.",
        stats::setNames(details, rep("x", length(details)))),
      class = "biocroxytest_incomplete_test_plan", call = NULL
    )
  }
  invisible(TRUE)
}
