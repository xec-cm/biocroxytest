#' Declare fixtures for named test cases
#'
#' In a [test_case_plan()] declaration, add a named literal
#' `fixtures = list(...)` to a case. Each fixture has a literal
#' `source` and `md5`, and may specify `required = FALSE`.
#' Use a file path relative to the case's suite directory
#' (`tests/testthat` or `longtests/testthat`), or an HTTPS URL.
#' Use forward slashes and keep local fixtures inside the suite.
#' Use ordinary files. Local fixture declarations should not point to
#' special files or links outside the suite directory. Add `fs` to
#' the consumer package's Suggests when declaring fixtures.
#' Compute the checksum with [tools::md5sum()].
#'
#' The generated test provides a named character vector `fixture_paths`
#' to the case setup and body. Unavailable optional fixtures have
#' `NA_character_` paths. A required local fixture that is absent or has
#' changed fails the generated test. Standard testthat runs do not resolve
#' remote fixtures and skip cases that require them.
#'
#' Use [run_test_plan()] or [run_test_case()] to check all declared fixtures
#' before a case starts. The runners check local files and look for remote
#' files in BiocFileCache. Enable `download_fixtures` explicitly to fill the
#' cache. Only downloads that match the declared checksum are cached.
#' Keep a custom cache outside the package source tree.
#' Planning and generation do not access the files or the network.
#' @return In each generated test, `fixture_paths` is a named character vector
#'   with one path or `NA_character_` per fixture.
#' @name test_fixtures
NULL

parse_case_fixtures <- function(tag, values) {
  if (!"fixtures" %in% names(values)) {
    return(list())
  }
  fixtures <- case_list(values$fixtures)
  ids <- names(fixtures)
  if (is.null(fixtures) ||
      (length(fixtures) && (is.null(ids) || any(!valid_case_name(ids)) || anyDuplicated(ids)))) {
    abort_case_tag(tag, "Supply fixtures as a literal list with unique fixture names.")
  }

  lapply(fixtures, function(fixture) {
    fields <- case_list(fixture)
    if (!valid_case_fields(fields, c("source", "md5"), "required") ||
        !is.character(fields$source) || length(fields$source) != 1L ||
        is.na(fields$source) || !valid_fixture_source(fields$source) ||
        !is.character(fields$md5) || length(fields$md5) != 1L ||
        is.na(fields$md5) || !grepl("^[0-9A-Fa-f]{32}$", fields$md5) ||
        ("required" %in% names(fields) && !identical(fields$required, TRUE) &&
           !identical(fields$required, FALSE))) {
      abort_case_tag(tag, paste(
        "Each fixture needs a suite-relative file path or HTTPS URL as source,",
        "a 32-digit hexadecimal md5, and optionally required = TRUE or FALSE."
      ))
    }
    list(source = fields$source, md5 = tolower(fields$md5),
         required = !identical(fields$required, FALSE))
  })
}

remote_fixture <- function(source) startsWith(source, "https://")

valid_fixture_source <- function(source) {
  if (remote_fixture(source)) {
    return(grepl("^https://[^/@[:space:]?#]+/[^[:space:]#]+$", source))
  }
  nzchar(source) && !startsWith(source, "~") && !grepl("[:\\\\[:cntrl:]]", source) &&
    !grepl("(^/|/$|//|(^|/)[.]{1,2}(/|$))", source)
}

fixture_test_code <- function(fixtures) {
  if (!length(fixtures)) {
    return(character())
  }
  # Escape Unicode explicitly so generation agrees across locale settings.
  declarations <- vapply(names(fixtures), function(id) {
    fixture <- fixtures[[id]]
    points <- utf8ToInt(enc2utf8(fixture$source))
    source <- if (any(points > 127L)) {
      paste0('"', paste0(sprintf("\\U%08x", points), collapse = ""), '"')
    } else {
      encodeString(fixture$source, quote = '"')
    }
    sprintf('    "%s" = list(source = %s, md5 = "%s", required = %s)',
            id, source, fixture$md5, fixture$required)
  }, character(1))
  lookup <- deparse(generated_fixture_paths, control = c("keepInteger", "keepNA"))
  c(
    "  fixture_paths <- (",
    paste0("    ", lookup),
    "  )(list(",
    paste0(declarations, c(rep(",", length(declarations) - 1L), "")),
    "  ))"
  )
}

# Embed the lookup so generated tests do not depend on biocroxytest.
generated_fixture_paths <- function(fixtures) {
  prepared <- getOption("biocroxytest.fixture_paths")
  paths <- vapply(names(fixtures), function(id) {
    fixture <- fixtures[[id]]
    remote <- startsWith(fixture$source, "https://")
    path <- if (is.null(prepared)) {
      if (remote) NA_character_ else fixture$source
    } else {
      unname(prepared[id])
    }
    available <- !is.na(path) &&
      fs::file_info(path, fail = FALSE, follow = FALSE)$type %in% "file"
    if (available && !remote && is.null(prepared)) {
      suite <- normalizePath(".", winslash = "/", mustWork = TRUE)
      target <- normalizePath(path, winslash = "/", mustWork = TRUE)
      available <- startsWith(target, paste0(suite, "/"))
    }
    available <- available && identical(unname(tools::md5sum(path)), fixture$md5)
    if (available) {
      return(path)
    }
    if (!is.null(prepared) && !is.na(path)) {
      stop(sprintf("Verified fixture changed before case execution: %s", id), call. = FALSE)
    }
    if (fixture$required) {
      if (remote && is.null(prepared)) {
        testthat::skip(sprintf("Remote fixture requires the named case runner: %s", id))
      }
      stop(sprintf("Required fixture missing or checksum differs: %s", id), call. = FALSE)
    }
    NA_character_
  }, character(1))
  paths
}

validate_fixture_run_options <- function(download_fixtures, fixture_cache) {
  if (!identical(download_fixtures, TRUE) && !identical(download_fixtures, FALSE)) {
    cli::cli_abort("{.arg download_fixtures} must be TRUE or FALSE.", call = NULL)
  }
  if (!is.null(fixture_cache) && (!is.character(fixture_cache) ||
      length(fixture_cache) != 1L || is.na(fixture_cache) || !nzchar(fixture_cache))) {
    cli::cli_abort("{.arg fixture_cache} must be a directory path.", call = NULL)
  }
}

resolve_fixture_cache <- function(fixture_cache, path) {
  cache <- if (is.null(fixture_cache)) tools::R_user_dir("biocroxytest", "cache") else fixture_cache
  cache <- fs::path_abs(cache)
  parent <- cache
  while (!file.exists(parent) && !identical(parent, fs::path_dir(parent))) {
    parent <- fs::path_dir(parent)
  }
  resolved <- fs::path_real(parent)
  if (!identical(cache, parent)) {
    resolved <- fs::path(resolved, fs::path_rel(cache, start = parent))
  }
  resolved <- as.character(resolved)
  package <- as.character(path)
  # Also compare case-folded paths for case-insensitive macOS and Windows volumes.
  if (identical(tolower(resolved), tolower(package)) ||
      startsWith(tolower(resolved), paste0(tolower(package), "/"))) {
    cli::cli_abort("{.arg fixture_cache} must be outside the source package.", call = NULL)
  }
  resolved
}

resolve_case_fixtures <- function(path, case, staging, download, cache) {
  declarations <- case$fixture[[1L]]
  rows <- lapply(names(declarations), function(id) {
    fixture <- declarations[[id]]
    result <- tryCatch({
      if (remote_fixture(fixture$source)) {
        staged <- tempfile("fixture-", tmpdir = staging)
        resolve_remote_fixture(fixture, staged, download, cache)
      } else {
        local <- file.path(path, dirname(case$path), fixture$source)
        fixture_checksum(local, fixture$md5)
      }
    }, error = function(cnd) {
      list(path = NA_character_, md5 = NA_character_, status = "unavailable", detail = conditionMessage(cnd))
    })

    data.frame(
      id = id, source = fixture$source, required = fixture$required,
      expected_md5 = fixture$md5, observed_md5 = result$md5,
      status = result$status, detail = result$detail, path = result$path
    )
  })
  empty <- data.frame(
    id = character(), source = character(), required = logical(), expected_md5 = character(),
    observed_md5 = character(), status = character(), detail = character(), path = character()
  )
  do.call(rbind, c(list(empty), rows))
}

fixture_checksum <- function(path, expected) {
  type <- fs::file_info(path, fail = FALSE, follow = FALSE)$type
  md5 <- if (type %in% "file") unname(tools::md5sum(path)) else NA_character_
  if (is.na(md5)) {
    return(list(path = NA_character_, md5 = md5, status = "missing", detail = "Fixture file is unavailable."))
  }
  if (!identical(md5, expected)) {
    return(list(path = NA_character_, md5 = md5, status = "mismatch", detail = "Fixture checksum differs."))
  }
  list(path = path, md5 = md5, status = "verified", detail = "")
}

fixture_paths <- function(fixtures) stats::setNames(fixtures$path, fixtures$id)

fixture_diagnostics <- function(fixtures) {
  blocked <- fixtures$required & fixtures$status != "verified"
  paste(paste0("Fixture ", fixtures$id[blocked], ": ", fixtures$detail[blocked]), collapse = "\n")
}

resolve_remote_fixture <- function(fixture, staged, download, cache) {
  if (!download && !dir.exists(cache)) {
    return(list(path = NA_character_, md5 = NA_character_, status = "missing",
                detail = "Fixture is not cached. Enable download_fixtures to fetch it."))
  }
  if (!requireNamespace("BiocFileCache", quietly = TRUE)) {
    stop("Install BiocFileCache to resolve remote fixtures.", call. = FALSE)
  }
  bfc <- BiocFileCache::BiocFileCache(cache, ask = FALSE)
  key <- paste(fixture$md5, fixture$source)
  cached <- BiocFileCache::bfcquery(bfc, key, field = "rname", exact = TRUE)
  fs::dir_create(dirname(staged))

  result <- fixture_checksum(staged, fixture$md5)
  for (file in cached$rpath) {
    if (fs::file_info(file, fail = FALSE, follow = FALSE)$type %in% "file" &&
        file.copy(file, staged, overwrite = TRUE)) {
      result <- fixture_checksum(staged, fixture$md5)
      if (result$status == "verified") {
        return(result)
      }
    }
  }
  if (!download) {
    if (result$status == "missing") {
      result$detail <- "Fixture is not cached. Enable download_fixtures to fetch it."
    }
    return(result)
  }

  unlink(staged)
  status <- utils::download.file(fixture$source, staged, mode = "wb", quiet = TRUE)
  if (status != 0L) {
    stop("Fixture download failed.", call. = FALSE)
  }
  result <- fixture_checksum(staged, fixture$md5)
  if (result$status == "verified") {
    cached_path <- BiocFileCache::bfcadd(
      bfc, rname = key, fpath = staged, rtype = "local", action = "copy"
    )
    if (fixture_checksum(unname(cached_path), fixture$md5)$status != "verified") {
      result$detail <- "Downloaded fixture verified, but the cache copy could not be verified."
    }
  }
  result
}
