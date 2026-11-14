#' List named test cases
#'
#' Declare a group with `@testCases group-name` and a literal R list containing
#' `cases` and a shared `test` block. Each named case has a `suite` of
#' `"tests"` or `"longtests"` and can have a `setup` block. The generator
#' puts the setup and shared body in the same `test_that()` call for each
#' case. Neither block runs when planning or generating tests.
#' Cases may also declare \link{test_fixtures}.
#'
#' Group names are unique in the package, and case names are unique within
#' their group. Names start with an ASCII letter or digit and may also
#' contain dots, underscores and hyphens. IDs have the form `group/case` and
#' stay the same when a declaration moves to another source file or suite.
#' Write the tag directly in the source. Use [tests_roclet()] to generate both
#' suites.
#'
#' @param path Path to the source package root.
#' @return A data frame with case `id`, `suite`, relative `source` and output
#'   `path`, testthat `description`, generated `code`, and a `fixture` list
#'   column with the fixture declarations. Each row describes
#'   one named case declared directly in the package's roxygen source.
#' @export
#' @examples
#' pkg <- tempfile("test-case-example-")
#' dir.create(file.path(pkg, "R"), recursive = TRUE)
#' writeLines("Package: examplepkg", file.path(pkg, "DESCRIPTION"))
#' writeLines(c(
#'   "#' @testCases arithmetic",
#'   "#' list(",
#'   "#'   cases = list(small = list(suite = 'tests', setup = { x <- 2L })),",
#'   "#'   test = { expect_identical(x + 1L, 3L) }",
#'   "#' )",
#'   "NULL"
#' ), file.path(pkg, "R", "example.R"))
#' test_case_plan(pkg)
#' unlink(pkg, recursive = TRUE)
test_case_plan <- function(path = ".") {
  path <- test_package_path(path)
  files <- list.files(file.path(path, "R"), pattern = "[.][Rr]$")
  sources <- paste0("R/", files, recycle0 = TRUE)
  ignore <- file.path(path, ".Rbuildignore")
  if (file.exists(ignore)) {
    patterns <- readLines(ignore, warn = FALSE)
    for (pattern in patterns[nzchar(patterns)]) {
      sources <- sources[!grepl(pattern, sources, perl = TRUE)]
    }
  }
  blocks <- lapply(file.path(path, sources), roxygen2::parse_file, env = NULL)
  collect_test_cases(unlist(blocks, recursive = FALSE))
}

#' Run one generated test case by ID
#'
#' Load the source package and run the selected generated test with testthat.
#' Helpers, setup and teardown are loaded from that case's suite. Regenerate
#' with `roxygen2::roxygenise()` after changing its declaration.
#' Selecting a case requires testthat 3.3.0 or newer.
#' Testthat runs in a temporary package copy so the original snapshots stay
#' intact. Use a full suite run to review or update snapshots and exercise
#' custom suite runners. The copy requires ordinary files and directories,
#' as in [check_generated_tests()].
#' The package is loaded from `path` in the current R session. To run a single
#' case in an isolated process with a timeout, use [run_test_plan()] with its ID.
#'
#' @param id An exact case ID from [test_case_plan()].
#' @param path Path to the source package root.
#' @param reporter A testthat reporter name or object.
#' @inheritParams run_test_plan
#' @return The testthat results, invisibly. Inspect these results for failures.
#' @export
#' @examples
#' if (interactive()) run_test_case("arithmetic/small")
run_test_case <- function(id, path = ".", reporter = "summary",
                          download_fixtures = FALSE, fixture_cache = NULL) {
  if (!is.character(id) || length(id) != 1L || is.na(id)) {
    cli::cli_abort("Supply one test case ID.", call = NULL)
  }
  path <- test_package_path(path)
  plan <- test_case_plan(path)
  index <- match(id, plan$id)
  if (is.na(index)) {
    cli::cli_abort("No test case has ID {.str {id}}.", call = NULL)
  }
  case <- plan[index, ]
  file <- file.path(path, case$path)
  verify_generated_case(file, case)
  require_case_testthat()
  validate_fixture_run_options(download_fixtures, fixture_cache)
  if (length(case$fixture[[1L]]) || !is.null(fixture_cache)) {
    fixture_cache <- resolve_fixture_cache(fixture_cache, path)
  }

  prepared <- local_test_case(path, case, download_fixtures, fixture_cache)
  fixtures <- prepared$fixtures
  if (any(fixtures$required & fixtures$status != "verified")) {
    details <- fixture_diagnostics(fixtures)
    cli::cli_abort("{details}", class = "biocroxytest_fixture_unavailable", call = NULL)
  }
  execute_test_case(path, prepared$file, case$description, fixture_paths(fixtures), reporter)
}

local_test_case <- function(path, case, download_fixtures, fixture_cache,
                            .local_envir = parent.frame()) {
  copy <- test_package_copy_path(path)
  withr::defer(unlink(copy, recursive = TRUE, expand = FALSE), envir = .local_envir)
  copy_test_package(path, copy)
  staging <- withr::local_tempdir("biocroxytest-fixtures-", .local_envir = .local_envir)
  fixtures <- resolve_case_fixtures(copy, case, staging, download_fixtures, fixture_cache)
  list(copy = copy, file = file.path(copy, case$path), fixtures = fixtures)
}

require_case_testthat <- function() {
  if (!requireNamespace("testthat", quietly = TRUE) ||
      utils::packageVersion("testthat") < "3.3.0") {
    cli::cli_abort("Running a named case requires testthat 3.3.0 or newer.", call = NULL)
  }
}

verify_generated_case <- function(file, case) {
  if (made_by_biocroxytest(file, case$suite)) {
    code <- parse(file, encoding = "UTF-8", keep.source = TRUE)
    expected <- parse(text = case$code, encoding = "UTF-8", keep.source = FALSE)[[1L]]
    matches <- vapply(code, function(expr) {
      is.call(expr) && identical(expr[[1L]], quote(test_that)) &&
        length(expr) >= 2L && identical(expr[[2L]], case$description)
    }, logical(1))
    if (sum(matches) == 1L) {
      index <- which(matches)
      selected <- code[[index]]
      if (identical(utils::removeSource(selected), expected)) {
        return(invisible(as.integer(attr(code, "srcref")[[index]])))
      }
    }
  }
  cli::cli_abort(c(
    "Generated test case {.str {case$id}} is missing or out of date.",
    i = "Run {.code roxygen2::roxygenise()} and retry."
  ), call = NULL)
}

#' @importFrom roxygen2 roxy_tag_parse
#' @export
roxy_tag_parse.roxy_tag_testCases <- function(x) {
  lines <- strsplit(x$raw, "\n", fixed = TRUE)[[1L]]
  group <- if (length(lines)) trimws(lines[[1L]]) else ""
  if (!valid_case_name(group)) {
    abort_case_tag(x, "Supply a group name on the same line as @testCases.")
  }
  expression <- tryCatch(
    parse(text = lines[-1L], keep.source = TRUE, encoding = "UTF-8"),
    error = function(e) abort_case_tag(x, "Cannot parse the test case declaration.", parent = e)
  )
  if (length(expression) != 1L) {
    abort_case_tag(x, "Supply one literal list with cases and a shared test block.")
  }
  fields <- case_list(expression[[1L]])
  if (!valid_case_fields(fields, c("cases", "test")) ||
      !is_case_block(fields$test)) {
    abort_case_tag(x, "Supply cases = list(...) and test = {...}.")
  }
  cases <- case_list(fields$cases)
  if (!length(cases) || is.null(names(cases)) || any(!valid_case_name(names(cases))) ||
      anyDuplicated(names(cases))) {
    abort_case_tag(x, "Case names must be valid, nonempty and unique within their group.")
  }
  cases <- lapply(cases, function(case) {
    values <- case_list(case)
    if (!valid_case_fields(values, "suite", c("setup", "fixtures")) ||
        !is.character(values$suite) || length(values$suite) != 1L ||
        !values$suite %in% c("tests", "longtests") ||
        ("setup" %in% names(values) && !is_case_block(values$setup))) {
      abort_case_tag(x, "Each case needs suite = \"tests\" or \"longtests\" and an optional setup = {...}.")
    }
    values$fixtures <- parse_case_fixtures(x, values)
    values
  })
  x$val <- list(group = group, cases = cases, test = fields$test)
  x
}

valid_case_name <- function(x) {
  !is.na(x) & grepl("^[A-Za-z0-9][A-Za-z0-9_.-]*$", x)
}

case_list <- function(expr) {
  if (is.call(expr) && identical(expr[[1L]], quote(list))) as.list(expr)[-1L] else NULL
}

valid_case_fields <- function(fields, required, optional = character()) {
  keys <- names(fields)
  length(keys) == length(fields) && !anyDuplicated(keys) &&
    all(required %in% keys) && all(keys %in% c(required, optional))
}

is_case_block <- function(expr) {
  is.call(expr) && identical(expr[[1L]], quote(`{`))
}

abort_case_tag <- function(tag, message, parent = NULL) {
  source <- basename(tag$file)
  line <- tag$line
  cli::cli_abort(c(
    "Invalid {.code @testCases} at {.file {source}} line {line}.",
    i = "{message}"
  ), class = "biocroxytest_invalid_test_case", parent = parent, call = NULL)
}

collect_test_cases <- function(blocks, check_origin = FALSE) {
  groups <- lapply(blocks, function(block) {
    tags <- roxygen2::block_get_tags(block, "testCases")
    if (check_origin) {
      for (tag in tags) {
        # Roxygen puts tags emitted by @eval at or after the object's line.
        if (tag$line >= block$line) {
          abort_case_tag(tag, "Write @testCases directly in the source rather than using @eval.")
        }
      }
    }
    tags
  })
  groups <- unlist(groups, recursive = FALSE)
  group_names <- vapply(groups, function(tag) tag$val$group, character(1))
  duplicate <- anyDuplicated(group_names)
  if (duplicate) {
    abort_case_tag(groups[[duplicate]], paste("Duplicate test case group", group_names[[duplicate]]))
  }
  rows <- lapply(groups, function(tag) {
    values <- tag$val
    source <- basename(tag$file)
    ids <- paste(values$group, names(values$cases), sep = "/")
    suites <- vapply(values$cases, `[[`, character(1), "suite", USE.NAMES = FALSE)
    code <- vapply(seq_along(ids), function(i) {
      case <- values$cases[[i]]
      body <- c(fixture_test_code(case$fixtures), case_block_code(case$setup), case_block_code(values$test))
      paste(c(paste0("test_that(", encodeString(ids[[i]], quote = '"'), ", {"),
              body, "})"), collapse = "\n")
    }, character(1))
    result <- data.frame(
      id = ids, suite = suites, source = paste0("R/", source),
      path = paste0(suites, "/testthat/", vapply(suites, test_prefix, character(1)), source),
      description = ids, code = code
    )
    result$fixture <- lapply(values$cases, "[[", "fixtures")
    result
  })
  empty <- data.frame(id = character(), suite = character(), source = character(),
                      path = character(), description = character(), code = character())
  empty$fixture <- list()
  plan <- do.call(rbind, c(list(empty), rows))
  plan <- plan[order(plan$id, method = "radix"), , drop = FALSE]
  rownames(plan) <- NULL
  plan
}

case_block_code <- function(block) {
  # Keep source spellings for literals that deparse() can alter, for example
  # Unicode strings under a non-UTF-8 locale.
  refs <- attr(block, "srcref")
  vapply(refs[-1L], function(ref) {
    indent_text(paste(as.character(ref), collapse = "\n"))
  }, character(1))
}
