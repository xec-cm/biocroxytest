#' Record execution evidence for named test cases
#'
#' Run the selected [test_case_plan()] cases using their generated files and
#' each suite's testthat helpers, setup and teardown. Each case starts in a
#' fresh R session and package copy. The package source and test code run
#' with your permissions, so use this with packages you trust.
#' The selected body is instrumented in the copy to observe completion.
#' Use a full suite run to exercise custom runners or dependencies on
#' other cases in the same session.
#'
#' Results are tied to a revision fingerprint of the copied paths and file
#' contents. Git metadata is excluded. Package code, helpers and generated
#' tests are part of the fingerprint. The checkout must meet the same copy
#' requirements as [check_generated_tests()].
#'
#' A case is `completed` when it reaches the end and records a passing
#' expectation without skips. A failed expectation or test error gives
#' `failed`. An explicit skip from the case body before any passes gives
#' `skipped`. An early return, nested or partial skip, stale generated
#' case or interrupted suite setup or teardown gives
#' `incomplete`. Absent generated files and absent case results give
#' `missing`. The case file and its declared code must agree before it runs.
#'
#' @param path Path to the source package root.
#' @param ids Exact case IDs to run. `NULL` runs the entire named plan.
#' @param download_fixtures Allow HTTPS downloads of declared fixtures when
#'   a verified copy is unavailable in the cache. Defaults to `FALSE`.
#' @param fixture_cache BiocFileCache directory. `NULL` uses the package's
#'   user cache directory, `tools::R_user_dir("biocroxytest", "cache")`.
#' @return A list with `revision`, a content fingerprint, `cases`, a data
#'   frame containing the selected case IDs, suites, paths, statuses,
#'   durations in seconds and diagnostics, and `environments`, the R
#'   version, platform, operating system, locale and loaded package versions
#'   observed at the end of execution, by case ID. The duration is elapsed
#'   time for the case, or elapsed runner time when execution is interrupted.
#'   `fixtures` contains the preflight fixture sources, expected and observed
#'   checksums, availability and diagnostics, keyed by case ID. A case with
#'   an unavailable required fixture is `incomplete` and does not start.
#' @export
#' @examples
#' pkg <- tempfile("test-plan-example-")
#' dir.create(pkg)
#' writeLines("Package: examplepkg", file.path(pkg, "DESCRIPTION"))
#' run_test_plan(pkg)
#' unlink(pkg, recursive = TRUE)
run_test_plan <- function(path = ".", ids = NULL, download_fixtures = FALSE, fixture_cache = NULL) {
  validate_fixture_run_options(download_fixtures, fixture_cache)
  path <- test_package_path(path)
  snapshot <- test_package_copy_path(path)
  on.exit(unlink(snapshot, recursive = TRUE, expand = FALSE), add = TRUE)
  copy_test_package(path, snapshot)

  plan <- test_case_plan(snapshot)
  if (!is.null(ids)) {
    if (!is.character(ids) || anyNA(ids) || anyDuplicated(ids)) {
      cli::cli_abort("{.arg ids} must contain unique case IDs.", call = NULL)
    }
    unknown <- setdiff(ids, plan$id)
    if (length(unknown)) {
      cli::cli_abort("Unknown test case IDs: {.val {unknown}}.", call = NULL)
    }
    plan <- plan[match(ids, plan$id), , drop = FALSE]
  }
  if (any(lengths(plan$fixture)) || !is.null(fixture_cache)) {
    fixture_cache <- resolve_fixture_cache(fixture_cache, path)
  }

  revision <- test_plan_revision(snapshot)
  cases_df <- plan[c("id", "suite", "source", "path")]
  cases_df$status <- rep("missing", nrow(cases_df))
  cases_df$duration <- rep(NA_real_, nrow(cases_df))
  cases_df$detail <- rep("", nrow(cases_df))
  environments <- list()
  fixtures <- list()

  if (nrow(plan)) {
    require_case_testthat()
  }
  for (i in seq_len(nrow(plan))) {
    evidence <- record_test_case(snapshot, plan[i, ], download_fixtures, fixture_cache)
    cases_df$status[i] <- evidence$status
    cases_df$duration[i] <- evidence$duration
    cases_df$detail[i] <- evidence$detail
    if (!is.null(evidence$environment)) {
      environments[[cases_df$id[i]]] <- evidence$environment
    }
    if (NROW(evidence$fixtures)) {
      fixtures[[cases_df$id[i]]] <- evidence$fixtures
    }
  }

  rownames(cases_df) <- NULL
  list(revision = revision, cases = cases_df, environments = environments, fixtures = fixtures)
}

test_plan_revision <- function(path) {
  files <- sort(as.character(fs::dir_ls(path, all = TRUE, recurse = TRUE)), method = "radix")
  regular <- !dir.exists(files)
  manifest <- list(
    paths = as.character(fs::path_rel(files, start = path)),
    contents = unname(tools::md5sum(files[regular])),
    regular = regular
  )
  if (anyNA(manifest$contents)) {
    cli::cli_abort("Could not fingerprint the package files.", call = NULL)
  }

  manifest_file <- tempfile("biocroxytest-manifest-")
  on.exit(unlink(manifest_file, expand = FALSE), add = TRUE)
  writeBin(serialize(manifest, NULL, version = 2L), manifest_file)
  unname(tools::md5sum(manifest_file))
}

record_test_case <- function(snapshot, case, download_fixtures, fixture_cache) {
  file <- file.path(snapshot, case$path)
  if (!file.exists(file)) {
    return(list(status = "missing", duration = NA_real_, detail = "Generated file is absent."))
  }
  location <- tryCatch(verify_generated_case(file, case), error = conditionMessage)
  if (is.character(location)) {
    return(list(status = "incomplete", duration = NA_real_, detail = location))
  }

  copy <- test_package_copy_path(snapshot)
  on.exit(unlink(copy, recursive = TRUE, expand = FALSE), add = TRUE)
  copy_test_package(snapshot, copy)
  staging <- tempfile("biocroxytest-fixtures-")
  on.exit(unlink(staging, recursive = TRUE), add = TRUE)
  fixtures <- resolve_case_fixtures(copy, case, staging, download_fixtures, fixture_cache)
  fixture_evidence <- fixtures[, names(fixtures) != "path"]
  if (any(fixtures$required & fixtures$status != "verified")) {
    return(list(status = "incomplete", duration = NA_real_,
                detail = fixture_diagnostics(fixtures), fixtures = fixture_evidence))
  }

  file <- file.path(copy, case$path)
  started <- proc.time()[["elapsed"]]
  run <- tryCatch(
    callr::r(
      execute_evidence_case,
      args = list(
        path = copy, file = file, description = case$description,
        code = case$code, location = location, checksum = unname(tools::md5sum(file)),
        paths = fixture_paths(fixtures)
      ),
      user_profile = FALSE,
      system_profile = FALSE,
      error = "error"
    ),
    error = function(error) list(problem = conditionMessage(error))
  )
  if (is.null(run)) {
    run <- list(problem = "The R process exited without returning test results.")
  }
  if (!is.null(run$problem)) {
    return(list(status = "incomplete", duration = proc.time()[["elapsed"]] - started,
                detail = run$problem, fixtures = fixture_evidence))
  }
  if (run$status == "incomplete" && is.na(run$duration)) {
    run$duration <- proc.time()[["elapsed"]] - started
  }
  run$fixtures <- fixture_evidence
  run
}

execute_evidence_case <- function(path, file, description, code, location, checksum, paths) {
  setwd(path)
  state <- new.env(parent = emptyenv())
  state$finished <- FALSE
  state$skipped <- FALSE
  state$started <- NA_real_
  state$duration <- NA_real_
  state$suite_warnings <- character()
  temporary <- tempfile(".biocroxytest-", tmpdir = dirname(file))
  on.exit(unlink(temporary, expand = FALSE), add = TRUE)

  # The reporter sees every expectation, including those in nested tests.
  # start_file runs after suite setup and just before testthat reads the file.
  reporter <- R6::R6Class(
    "CaseEvidenceReporter",
    inherit = testthat::SilentReporter,
    public = list(
      start_file = function(filename) {
        if (!identical(unname(tools::md5sum(file)), checksum)) {
          stop("The generated file changed during suite setup.", call. = FALSE)
        }
        state$original <- readBin(file, "raw", n = file.size(file))
        lines <- readLines(file, encoding = "UTF-8", warn = FALSE)
        start <- location[1L]
        end <- location[3L]
        prefix <- rawToChar(utils::head(charToRaw(lines[start]), location[2L] - 1L))
        suffix <- rawToChar(utils::tail(charToRaw(lines[end]), nchar(lines[end], type = "bytes") - location[4L]))
        selected <- strsplit(code, "\n", fixed = TRUE)[[1L]]

        # Only the selected expression changes. Observe a real case-body
        # skip separately from testthat's synthetic skip for an empty test.
        instrumented <- c(
          paste0(prefix, selected[1L]),
          "  base::withCallingHandlers({",
          selected[-c(1L, length(selected))],
          "    base::getOption('biocroxytest.case_event')('finished')",
          "  }, skip = function(cnd) base::getOption('biocroxytest.case_event')('skipped'))",
          paste0(selected[length(selected)], suffix)
        )
        lines <- c(utils::head(lines, start - 1L), instrumented, utils::tail(lines, length(lines) - end))
        writeLines(lines, temporary, useBytes = TRUE)
        fs::file_move(temporary, file)
        state$checksum <- unname(tools::md5sum(file))
      },
      start_test = function(context, test) {
        if (identical(test, description)) {
          state$started <- proc.time()[["elapsed"]]
        }
      },
      end_test = function(context, test) {
        if (identical(test, description)) {
          state$duration <- proc.time()[["elapsed"]] - state$started
        }
      },
      end_file = function() {
        if (!identical(unname(tools::md5sum(file)), state$checksum)) {
          stop("The generated file changed during case execution.", call. = FALSE)
        }
        writeBin(state$original, temporary)
        fs::file_move(temporary, file)
      }
    )
  )$new()

  problem <- tryCatch(withCallingHandlers({
    package <- read.dcf("DESCRIPTION", fields = "Package")[[1L]]
    roxygen2::load_pkgload(path)
    if (utils::packageVersion("testthat") < "3.3.0") {
      stop("Running a named case requires testthat 3.3.0 or newer.", call. = FALSE)
    }
    options(
      biocroxytest.case_event = function(event) { state[[event]] <- TRUE },
      biocroxytest.fixture_paths = paths
    )

    testthat::test_file(file, reporter = reporter, desc = description, package = package)
    if (!identical(unname(tools::md5sum(file)), checksum)) {
      stop("The generated file changed during suite teardown.", call. = FALSE)
    }
    NULL
  }, warning = function(cnd) {
    state$suite_warnings <- c(state$suite_warnings, conditionMessage(cnd))
  }), error = conditionMessage, skip = conditionMessage)

  expectations <- reporter$expectations()
  passed <- any(vapply(expectations, inherits, logical(1), "expectation_success"))
  failed <- any(vapply(expectations, inherits, logical(1), c("expectation_failure", "expectation_error")))
  skipped <- any(vapply(expectations, inherits, logical(1), "expectation_skip"))
  diagnostics <- Filter(function(cnd) !inherits(cnd, "expectation_success"), expectations)
  messages <- c(problem, state$suite_warnings, vapply(diagnostics, conditionMessage, character(1)))
  detail <- paste(unique(messages), collapse = "\n")

  status <- if (!is.null(problem)) {
    "incomplete"
  } else if (is.na(state$started)) {
    "missing"
  } else if (failed) {
    "failed"
  } else if (state$skipped && skipped && !passed && !state$finished) {
    "skipped"
  } else if (state$finished && passed && !skipped && !is.na(state$duration)) {
    "completed"
  } else {
    "incomplete"
  }
  if (!nzchar(detail)) {
    if (status == "missing") {
      detail <- "Testthat did not report the case."
    } else if (status == "incomplete") {
      detail <- "The case did not reach the end with a passing expectation."
    }
  }

  packages <- sort(loadedNamespaces(), method = "radix")
  environment <- list(
    r_version = as.character(getRversion()),
    platform = R.version$platform,
    os = Sys.info()[c("sysname", "release", "machine")],
    locale = Sys.getlocale(),
    packages = stats::setNames(vapply(packages, function(package) {
      as.character(getNamespaceVersion(package))
    }, character(1)), packages)
  )
  list(status = status, duration = if (is.null(problem)) state$duration else NA_real_,
       detail = detail, environment = environment)
}
