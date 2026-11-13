#' Set up long tests for a package
#'
#' Run this function from the package root. It enables long tests in
#' `.BBSoptions` and creates `longtests/testthat` and a testthat runner.
#' An existing long test runner is preserved. If there is no long test runner,
#' the function copies `tests/testthat.R` when available or creates a runner
#' for the package named in `DESCRIPTION`.
#'
#' @return Invisibly returns NULL. The function is called for its side effects.
#' @export
#' @examples
#' pkg <- tempfile("longtests-example-")
#' dir.create(pkg)
#' old <- setwd(pkg)
#' tryCatch({
#'   writeLines("Package: examplepkg", "DESCRIPTION")
#'   use_longtests()
#' }, finally = {
#'   setwd(old)
#'   unlink(pkg, recursive = TRUE, expand = FALSE)
#' })
use_longtests <- function() {
  if (!file.exists("DESCRIPTION")) {
    stop("Run use_longtests() from a package directory with a DESCRIPTION file.",
         call. = FALSE)
  }
  description <- read.dcf("DESCRIPTION", fields = "Package")
  package <- description[, "Package"]
  if (length(package) != 1L || is.na(package) ||
      !grepl("^[A-Za-z][A-Za-z0-9.]+$", package) || endsWith(package, ".")) {
    stop("DESCRIPTION must contain a valid Package field.", call. = FALSE)
  }

  setup_longtests_runner(package)
  setup_bbs_options()
  invisible(NULL)
}

setup_longtests_runner <- function(package) {
  path <- file.path("longtests", "testthat")
  if (!dir.exists(path) && !dir.create(path, recursive = TRUE)) {
    stop("Could not create ", path, call. = FALSE)
  }

  runner <- file.path("longtests", "testthat.R")
  if (file.exists(runner) || is_symlink(runner)) {
    if (!file.exists(runner) || dir.exists(runner) || file.access(runner, 4L) != 0L) {
      stop("Cannot use the existing long test runner: ", runner, call. = FALSE)
    }
    return(invisible(NULL))
  }

  ordinary <- file.path("tests", "testthat.R")
  staged <- tempfile(".testthat-", tmpdir = dirname(runner))
  on.exit(unlink(staged, expand = FALSE), add = TRUE)
  if (file.exists(ordinary) || is_symlink(ordinary)) {
    if (!file.exists(ordinary) || dir.exists(ordinary) ||
        file.access(ordinary, 4L) != 0L || !file.copy(ordinary, staged)) {
      stop("Could not copy ", ordinary, " to ", runner, call. = FALSE)
    }
  } else {
    name <- encodeString(package, quote = '"')
    writeLines(c(
      "library(testthat)",
      paste0("library(", name, ")"),
      "",
      paste0("test_check(", name, ")")
    ), staged)
  }

  if (file.exists(runner) || is_symlink(runner) || !file.rename(staged, runner)) {
    stop("Could not install the long test runner: ", runner, call. = FALSE)
  }
  cli::cli_inform(c("v" = "Created {runner}."))
}

setup_bbs_options <- function() {
  path <- ".BBSoptions"
  lines <- if (file.exists(path)) readLines(path, warn = FALSE) else character()
  updated <- enable_longtests_option(lines)
  # readLines drops a UTF-8 BOM, but BBS treats it as part of the first field.
  bom <- file.exists(path) && identical(
    readBin(path, "raw", n = 3L), as.raw(c(0xef, 0xbb, 0xbf))
  )
  if (!bom && identical(lines, updated)) { return(invisible(NULL)) }
  if (is_symlink(path)) {
    stop("Update RunLongTests in the linked .BBSoptions file directly.",
         call. = FALSE)
  }

  # Stage alongside the destination so a write error leaves the original intact.
  staged <- tempfile(".BBSoptions-", tmpdir = ".")
  on.exit(unlink(staged, expand = FALSE), add = TRUE)
  writeLines(updated, staged)
  if (file.exists(path)) { Sys.chmod(staged, file.info(path)$mode, use_umask = FALSE) }
  if (!file.rename(staged, path)) { stop("Could not update ", path, call. = FALSE) }
  cli::cli_inform(c("v" = "Enabled long tests in {path}."))
}

enable_longtests_option <- function(lines) {
  fields <- which(startsWith(lines, "RunLongTests:"))
  if (!length(fields)) { return(c(lines, "RunLongTests: TRUE")) }

  # BBS merges records, so the last occurrence is the effective field.
  field <- fields[length(fields)]
  continuation <- integer()
  for (i in seq.int(field + 1L, length(lines) + 1L)) {
    if (i > length(lines) || !nzchar(trimws(lines[i]))) { break }
    if (startsWith(lines[i], "#")) { next }
    if (!grepl("^[ \t]", lines[i])) { break }
    continuation <- c(continuation, i)
  }
  value <- paste(trimws(c(sub("^RunLongTests:", "", lines[field]),
                          lines[continuation])), collapse = " ")
  value <- trimws(value)
  if (tolower(value) == "true") { return(lines) }

  lines[field] <- "RunLongTests: TRUE"
  if (length(continuation)) { lines <- lines[-continuation] }
  lines
}
