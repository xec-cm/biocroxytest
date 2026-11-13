#' Compare named test plans
#'
#' Compare cases by their explicit IDs. A case is `added` or `removed` when
#' its ID occurs in only one plan, `moved` when its suite changes, and
#' `changed` when its description or generated test code changes. Changes to
#' `fixture` and `required` are also compared when the column occurs in both
#' plans. Other columns are ignored.
#'
#' A case that changes both suite and content produces two rows, one for
#' each status. Source and output paths provide context but do not determine
#' case identity or content changes. Moving a case to another source file
#' alone therefore produces no row. An ID rename appears as a removal and
#' an addition. Neither plan is modified, and no tests are run.
#'
#' @param before,after Data frames containing `id`, `suite`, `source`, `path`,
#'   `description`, and `code` character columns without missing values. IDs
#'   must be nonempty and unique within each plan. Suites must be `tests` or
#'   `longtests`. The `code` column contains the generated setup and test body.
#'   Optional `fixture` and `required` columns may contain atomic values or
#'   lists, with one value per case. These values are compared with [identical()].
#' @return A data frame with `id`, `status`, `before_suite`, `after_suite`,
#'   `before_source`, `after_source`, `before_path`, and `after_path` columns.
#'   Missing sides of additions and removals contain `NA`. The `fields` list
#'   column contains the affected field names: `suite` for moves, changed
#'   content or metadata fields for changes, and an empty character vector
#'   for additions and removals. Rows are sorted by ID, with a move before
#'   a content change for the same ID. Identical plans return zero rows.
#' @export
#' @examples
#' before <- data.frame(
#'   id = "alignment/memory", suite = "tests", source = "R/select.R",
#'   path = "tests/testthat/test-roxytest-tests-select.R",
#'   description = "Sample identifiers stay aligned",
#'   code = "expect_identical(actual, expected)"
#' )
#' after <- before
#' after$suite <- "longtests"
#' after$path <- "longtests/testthat/test-biocroxytest-longtests-select.R"
#' after$code <- "expect_identical(actual, expected); expect_s4_class(result, 'S4')"
#' compare_test_plans(before, after)
compare_test_plans <- function(before, after) {
  validate_test_plan(before, "before")
  validate_test_plan(after, "after")
  fields <- c("description", "code", "fixture", "required")
  fields <- fields[fields %in% intersect(names(before), names(after))]
  ids <- sort(union(before$id, after$id), method = "radix")
  rows <- list()
  for (id in ids) {
    old <- match(id, before$id)
    new <- match(id, after$id)
    if (is.na(old) || is.na(new)) {
      status <- if (is.na(old)) "added" else "removed"
      rows[[length(rows) + 1L]] <- test_plan_change(
        id, status, character(), before, old, after, new
      )
      next
    }
    if (!identical(test_plan_value(before, old, "suite"),
                   test_plan_value(after, new, "suite"))) {
      rows[[length(rows) + 1L]] <- test_plan_change(
        id, "moved", "suite", before, old, after, new
      )
    }
    changed <- fields[!vapply(fields, function(field) {
      identical(test_plan_value(before, old, field), test_plan_value(after, new, field))
    }, logical(1))]
    if (length(changed)) {
      rows[[length(rows) + 1L]] <- test_plan_change(
        id, "changed", changed, before, old, after, new
      )
    }
  }
  empty <- data.frame(
    id = character(), status = character(), before_suite = character(),
    after_suite = character(), before_source = character(), after_source = character(),
    before_path = character(), after_path = character()
  )
  empty$fields <- list()
  do.call(rbind, c(list(empty), rows))
}

validate_test_plan <- function(plan, label) {
  abort <- function(message) {
    cli::cli_abort(message, class = "biocroxytest_invalid_test_plan", call = NULL)
  }
  if (!is.data.frame(plan)) { abort("{label} must be a data frame.") }
  if (anyNA(names(plan)) || any(!nzchar(names(plan))) || anyDuplicated(names(plan))) {
    abort("{label} must have nonempty, unique column names.")
  }
  required <- c("id", "suite", "source", "path", "description", "code")
  missing <- setdiff(required, names(plan))
  if (length(missing)) {
    abort("{label} is missing required columns: {.val {missing}}.")
  }
  for (field in required) {
    if (!is.character(plan[[field]]) || !is.null(dim(plan[[field]])) ||
        anyNA(plan[[field]])) {
      abort("{label}${field} must be a character vector without missing values.")
    }
  }
  if (any(!nzchar(trimws(plan$id))) || anyDuplicated(plan$id)) {
    abort("{label}$id must contain nonempty, unique case IDs.")
  }
  if (any(!plan$suite %in% c("tests", "longtests"))) {
    abort("{label}$suite must contain only {.val tests} or {.val longtests}.")
  }
  for (field in intersect(c("fixture", "required"), names(plan))) {
    if (!is.null(dim(plan[[field]]))) {
      abort("{label}${field} must have one value per case; use a list column for structured values.")
    }
  }
  invisible(NULL)
}

test_plan_value <- function(plan, row, field) {
  value <- plan[row, field, drop = FALSE][[field]]
  if (is.list(value)) value[[1L]] else unname(value)
}

test_plan_change <- function(id, status, fields, before, old, after, new) {
  row <- data.frame(
    id, status, before_suite = before$suite[old], after_suite = after$suite[new],
    before_source = before$source[old], after_source = after$source[new],
    before_path = before$path[old], after_path = after$path[new], row.names = NULL
  )
  row$fields <- list(fields)
  row
}
