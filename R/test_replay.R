#' Save and replay a failed named test case
#'
#' Export a failed case from [run_test_plan()] to an RDS file. The record
#' contains the case, replay command, package revision, fixture evidence,
#' initial R seed and RNG kind, and the observed R environment. It includes
#' the failure diagnostic. Review these fields before sharing the file.
#'
#' Replay from the matching package source with the recorded fixture inputs.
#' Replay checks the package revision and fixture availability and checksums
#' before running the test. It restores the initial R seed and the recorded
#' values of `CI`, `NOT_CRAN` and `TZ` in the test process.
#' New records use the package input manifest to verify the revision.
#' Older records still verify the full checkout using their original rules.
#' The report records environment differences. Install matching R and
#' package versions yourself. Control other environment settings and any
#' random streams maintained outside base R in your test setup.
#' Diagnostics containing changing values, such as randomly named files,
#' can prevent a match.
#'
#' Use a replay file and source package you trust.
#' Save the record outside the source tree so the export does not change the
#' package revision. Run the recorded command from the source root. Update
#' its file path if the RDS file moves and pass `fixture_cache` if needed.
#'
#' @param evidence A result from [run_test_plan()].
#' @param id Exact ID of a failed named case in `evidence`.
#' @param file Path to the RDS file. Writing requires a new file in an
#'   existing directory. Replay reads the file without changing it.
#' @param path Path to the matching source package root.
#' @inheritParams run_test_plan
#' @return `write_test_replay()` returns the saved record invisibly.
#'   `replay_test_case()` returns a [run_test_plan()] result with a
#'   `comparison` list. It contains `same_failure` (whether the case
#'   failed with matching condition types, messages and available source
#'   locations), `environment_changes` (names of
#'   changed environment fields), and `original_detail`.
#' @examples
#' if (interactive() && file.exists("../case-failure.rds")) {
#'   replay_test_case("../case-failure.rds")
#' }
#' @export
write_test_replay <- function(evidence, id, file) {
  if (!is.character(id) || length(id) != 1L || is.na(id) || !nzchar(id)) {
    cli::cli_abort("Supply one failed case ID.", call = NULL)
  }
  validate_replay_file(file)
  file <- fs::path_abs(file)
  if (file.exists(file) || fs::is_link(file)) {
    cli::cli_abort("Replay output already exists: {.file {file}}.", call = NULL)
  }
  if (!dir.exists(dirname(file))) {
    cli::cli_abort("The replay output directory must already exist.", call = NULL)
  }
  command <- paste0("biocroxytest::replay_test_case(", encodeString(file, quote = '"'), ")")
  if (!is.list(evidence) || !is.data.frame(evidence$cases)) {
    cli::cli_abort("Supply evidence from {.fn run_test_plan}.", call = NULL)
  }
  cases_df <- evidence$cases
  case <- cases_df[which(cases_df$id == id & cases_df$status == "failed"), , drop = FALSE]
  if (nrow(case) != 1L) {
    cli::cli_abort("Evidence has no failed case with ID {.val {id}}.", call = NULL)
  }
  record <- list(
    format = if (is.null(evidence$revision_format)) 1L else evidence$revision_format,
    case = case,
    command = command,
    revision = evidence$revision,
    fixtures = evidence$fixtures[[id]],
    context = evidence$replay[[id]],
    environment = evidence$environments[[id]]
  )
  validate_replay_record(record)

  temporary <- tempfile(".biocroxytest-replay-", tmpdir = dirname(file))
  on.exit(unlink(temporary, expand = FALSE), add = TRUE)
  saveRDS(record, temporary, version = 2L)
  written <- fs::file_copy(temporary, file)
  if (!identical(written, file)) {
    unlink(written, expand = FALSE)
    cli::cli_abort("Replay output already exists: {.file {file}}.", call = NULL)
  }
  invisible(record)
}

#' @rdname write_test_replay
#' @export
replay_test_case <- function(file, path = ".", download_fixtures = FALSE,
                             fixture_cache = NULL, timeout = Inf) {
  validate_replay_file(file)
  record <- readRDS(file)
  validate_replay_record(record)

  result <- run_case_evidence(
    path, record$case$id, download_fixtures, fixture_cache,
    record$context$seed, timeout, replay = record
  )
  current <- result$environments[[record$case$id]]
  fields <- names(record$environment)
  changes <- fields[!vapply(fields, function(field) {
    identical(record$environment[[field]], current[[field]])
  }, logical(1))]

  result$comparison <- list(
    same_failure = identical(result$cases$status, "failed") &&
      identical(result$replay[[record$case$id]]$failures, record$context$failures),
    environment_changes = changes,
    original_detail = record$case$detail
  )
  result
}

valid_scalar_integer <- function(value) {
  is.numeric(value) && length(value) == 1L && is.null(dim(value)) &&
    is.finite(value) && abs(value) <= .Machine$integer.max && value == trunc(value)
}

verify_replay_revision <- function(revision, replay) {
  if (!identical(revision, replay$revision)) {
    cli::cli_abort(
      "Package revision differs from the replay record.",
      class = "biocroxytest_replay_revision", call = NULL
    )
  }
}

validate_replay_file <- function(file) {
  if (!is.character(file) || length(file) != 1L || is.na(file) || !nzchar(file)) {
    cli::cli_abort("{.arg file} must be one file path.", call = NULL)
  }
}

validate_replay_record <- function(record) {
  fields <- c("id", "suite", "source", "path", "status", "detail")
  if (!is.list(record) || !is.integer(record$format) || length(record$format) != 1L ||
      !record$format %in% c(1L, 2L) ||
      !is.data.frame(record$case) || nrow(record$case) != 1L ||
      !all(fields %in% names(record$case)) ||
      !all(vapply(record$case[fields], function(value) {
        is.character(value) && length(value) == 1L && is.null(dim(value)) && !is.na(value)
      }, logical(1))) ||
      !nzchar(record$case$id) ||
      !identical(record$case$status, "failed") ||
      !is.character(record$revision) || length(record$revision) != 1L ||
      is.na(record$revision) || !grepl("^[a-f0-9]{32}$", record$revision) ||
      !valid_replay_environment(record$environment) || !is.list(record$context) ||
      !valid_scalar_integer(record$context$seed) ||
      !identical(record$context$rng, c("Mersenne-Twister", "Inversion", "Rejection")) ||
      !is.character(record$context$settings) ||
      !identical(names(record$context$settings), c("CI", "NOT_CRAN", "TZ")) ||
      !is.character(record$context$failures) || !length(record$context$failures) ||
      anyNA(record$context$failures)) {
    cli::cli_abort(
      "Invalid replay record. Export a failed case with {.fn write_test_replay}.",
      class = "biocroxytest_invalid_replay", call = NULL
    )
  }
}

valid_replay_environment <- function(environment) {
  fields <- c("r_version", "platform", "os", "locale", "packages")
  is.list(environment) && identical(names(environment), fields) &&
    all(vapply(environment, is.character, logical(1))) &&
    all(lengths(environment[c("r_version", "platform", "locale")]) == 1L) &&
    identical(names(environment$os), c("sysname", "release", "machine")) &&
    length(environment$packages) > 0L && !is.null(names(environment$packages))
}

same_replay_fixtures <- function(current, recorded) {
  if (is.null(recorded)) {
    return(nrow(current) == 0L)
  }
  fields <- c("id", "source", "required", "expected_md5", "observed_md5", "status")
  is.data.frame(recorded) && all(fields %in% names(recorded)) &&
    identical(current[fields], recorded[fields])
}
