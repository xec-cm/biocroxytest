#' @param x A character scalar containing R code
#'
#' @noRd
#' @keywords internal
indent_text <- function(x) {
  # Indenting continuation lines inside a literal changes its value.
  parsed <- tryCatch(parse(text = x, keep.source = TRUE), error = function(e) NULL)
  if (is.null(parsed)) { return(x) }
  tokens <- utils::getParseData(parsed)
  if (is.null(tokens) || any(tokens$terminal & tokens$line1 != tokens$line2)) { return(x) }
  paste0("  ", gsub("\n", "\n  ", x, fixed = TRUE))
}

#' @param x A character vector`
#'
#' @noRd
#' @keywords internal
prepare_tests <- function(x) { gsub("^\\s*(.*?)\\s*$", "\\1", x) }

#' @param x A character vector
#'
#' @noRd
#' @keywords internal
auto_quote <- function(x) {
  needs_quotes <- !grepl("^('|\").*\\1$", x) & !make.names(x) == x
  x[needs_quotes] <- encodeString(x[needs_quotes], quote = "\"")
  x
}

#' Convert a roxygen2 block to a testthat file
#'
#' This is an internal function that converts a roxygen2 block to a testthat
#' file. It first checks if the block has any 'longtests' tags. If not, it
#' returns NULL. Then, it extracts the 'longtests' and 'examples' tags from the
#' block and assigns them to the 'longtests' and 'examples' fields of the
#' testthat file, respectively. It also assigns the basename of the block's file
#' to the 'filename' field and builds a test title from the object's alias or
#' S4 method topic and its source line.
#'
#' @param block A roxygen2 block.
#'
#' @return A list representing a testthat file, or NULL if the block has no
#'   'longtests' tags.
#' @keywords internal
block_to_testthat <- function(block) {
  if (length(roxygen2::block_get_tags(block, "rdname")) > 1L) {
    stop("Did not expect two @rdname's in a block")
  }
  testthat_file <- list()
  longtests <- roxygen2::block_get_tags(block, "longtests")
  if (length(longtests) == 0L) { return(NULL) }
  testthat_file$longtests <- longtests
  testthat_file$examples <- roxygen2::block_get_tags(block, "examples")
  testthat_file$filename <- basename(block$file)
  object <- block$object
  testthat_file$functionname <- if (inherits(object, "s4method") && length(object$topic) > 0L) {
    paste("Method", object$topic[[1L]])
  } else if (length(object$alias) > 0L) { paste0("Function ", object$alias[[1L]], "()")
  } else { "[unknown alias]" }

  if (!is.null(block$line)) {
    testthat_file$functionname <- paste0(
      testthat_file$functionname, ' @ L', block$line
    )
  }

  testthat_file
}

#' Process test files for 'longtests'
#'
#' This is an internal function that processes a list of test files for
#' 'longtests'. It loops over each test file and extracts the 'longtests' and
#' 'functionname' fields. It then prepares the tests, optionally indents the
#' code, and formats the tests into a string with an optional 'test_that'
#' boilerplate. The function also constructs a header for the content, including
#' a 'context' line if specified. The resulting content strings are returned in
#' a named list, with the names being the paths of the test files.
#'
#' @param testfiles A named list of test files, where the names are the paths of
#'   the files.
#' @param indent_code A logical value indicating whether to indent the code in
#'   the tests.
#' @param add_testthat_boilerplate A logical value indicating whether to add a
#'   'test_that' boilerplate to the tests.
#' @param add_context_header A logical value indicating whether to add a
#'   'context' line to the header.
#'
#' @return A named list of content strings for the test files.
#' @keywords internal
process_testfiles <- function(testfiles,
                              indent_code = FALSE,
                              add_testthat_boilerplate = FALSE,
                              add_context_header = FALSE) {

  if (length(testfiles) == 0L) { return(list()) }

  paths <- names(testfiles)
  results <- lapply(seq_along(testfiles), function(i) {
    testfile <- testfiles[[i]]
    content <- lapply(testfile, function(x) {
      longtests <- vapply(x$longtests, `[[`, character(1), "raw")
      longtests <- prepare_tests(longtests)
      if (indent_code) {
        longtests <- vapply(longtests, indent_text, character(1), USE.NAMES = FALSE)
      }
      tests_name <- x$functionname
      if (length(longtests) > 1L) {
        tests_name <- c(tests_name, paste0(tests_name, " [", seq.int(2L, length(longtests)), "]"))
      }
      if (add_testthat_boilerplate) {
        titles <- encodeString(tests_name, quote = '"')
        paste0("test_that(", titles, ", {\n", longtests, "\n})\n")
      } else {
        paste0("## ", tests_name, "\n", longtests, "\n")
      }
    })

    content <- paste(unlist(content, use.names = FALSE), collapse = "\n\n")

    path_quoted <- ifelse(
      paths[i] == "<text>",
      paths[i],
      paste0('File R/', auto_quote(paths[i]), ': @longtests')
    )

    content_header <- paste0(generated_test_header, "\n\n")

    content_header <- ifelse(
      add_context_header,
      paste0(content_header, 'context("', path_quoted, '")', "\n\n"),
      paste0(content_header, "# ", path_quoted, "\n\n")
    )

    paste0(content_header, content)
  })

  names(results) <- paths

  results
}

#' Process blocks for 'longtests' roclet
#'
#' Convert each roxygen block independently and group the tests by source file.
#' A shared documentation topic does not change the tests' source.
#'
#' @param blocks A list of roxygen2 blocks.
#' @param ... Additional arguments.
#'
#' @return A list with a single element 'longtests', which contains the
#'   processed test files.
#' @keywords internal
internal_longtests_roclet_process <- function(blocks, ...) {
  longtestfiles <- list()

  for (block in blocks) {
    testthat <- block_to_testthat(block)
    if (is.null(testthat$filename)) { next }
    if (length(testthat$longtests) > 0L) {
      longtestfiles[[testthat$filename]] <- c(
        longtestfiles[[testthat$filename]],
        list(testthat)
      )
    }
  }

  list(longtests = process_testfiles(
    longtestfiles, indent_code = TRUE,
    add_testthat_boilerplate = TRUE,
    add_context_header = FALSE
  ))
}

#' Update generated long test files
#'
#' Check every destination and parse every test before changing the output.
#' Stage new files in the destination directory, then replace each file and
#' remove obsolete generated files. Manual files are left in place.
#'
#' @param results A named list of generated R code.
#' @param base_path The destination directory.
#' @param prefix The generated filename prefix.
#' @return The source filenames of the generated tests.
#' @keywords internal
internal_longtests_roclet_output <- function(results, base_path,
                                             prefix = "test-biocroxytest") {
  paths <- names(results)
  filenames <- paste0(prefix, paths, recycle0 = TRUE)
  if (length(paths) != length(results) || anyNA(paths) ||
      any(!nzchar(paths)) || any(grepl("[/\\\\]", filenames)) ||
      any(!grepl("^test-biocroxytest-.*[.][Rr]$", filenames))) {
    stop("Invalid generated long test filename.", call. = FALSE)
  }

  keys <- tolower(filenames)
  if (anyDuplicated(keys)) {
    stop("Duplicate generated long test destination: ",
         filenames[anyDuplicated(keys)], call. = FALSE)
  }

  if (length(results) && !dir.exists(base_path)) {
    stop("Long test output directory does not exist: ", base_path, call. = FALSE)
  }

  existing <- list.files(base_path, all.files = TRUE, no.. = TRUE)
  destinations <- file.path(base_path, filenames)
  for (i in seq_along(results)) {
    aliases <- existing[tolower(existing) == keys[i]]
    path <- destinations[i]
    if (any(aliases != filenames[i]) ||
        ((file.exists(path) || is_symlink(path)) &&
         (!filenames[i] %in% existing || !made_by_biocroxytest(path)))) {
      stop("Long test destination collides with an existing file: ", path,
           call. = FALSE)
    }
    tryCatch(
      parse(text = results[[i]], srcfile = srcfilecopy(filenames[i], results[[i]])),
      error = function(e) {
        stop("Invalid generated long tests for ", paths[i], ": ",
             conditionMessage(e), call. = FALSE)
      }
    )
  }

  obsolete <- file.path(base_path, setdiff(existing, filenames))
  obsolete <- obsolete[vapply(obsolete, made_by_biocroxytest, logical(1))]
  if (length(results)) {
    # Use the real basenames to catch aliases and invalid names on this volume.
    stage <- tempfile(".biocroxytest-", tmpdir = base_path)
    if (!dir.create(stage, mode = "0700")) {
      stop("Could not stage generated long tests in ", base_path, call. = FALSE)
    }
    on.exit(unlink(stage, recursive = TRUE, expand = FALSE), add = TRUE)
    staged <- file.path(stage, filenames)
    for (i in seq_along(results)) {
      if (file.exists(staged[i])) {
        stop("Duplicate generated long test destination: ", filenames[i],
             call. = FALSE)
      }
      tryCatch(
        writeLines(enc2utf8(results[[i]]), staged[i], useBytes = TRUE),
        error = function(e) {
          stop("Could not stage generated long test: ", filenames[i], ": ",
               conditionMessage(e), call. = FALSE)
        }
      )
    }

    for (i in seq_along(destinations)) {
      if (!file.rename(staged[i], destinations[i])) {
        stop("Could not update generated long test: ", destinations[i],
             call. = FALSE)
      }
    }
  }

  if (length(obsolete) && unlink(obsolete, expand = FALSE) != 0L) {
    stop("Could not remove obsolete generated long tests in ", base_path,
         call. = FALSE)
  }
  paths
}
