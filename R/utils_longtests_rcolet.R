indent_text <- function(x) {
  # Indenting continuation lines inside a literal changes its value.
  parsed <- tryCatch(parse(text = x, keep.source = TRUE), error = function(e) NULL)
  if (is.null(parsed)) { return(x) }
  tokens <- utils::getParseData(parsed)
  if (is.null(tokens) || any(tokens$terminal & tokens$line1 != tokens$line2)) { return(x) }
  paste0("  ", gsub("\n", "\n  ", x, fixed = TRUE))
}

prepare_tests <- function(x) { gsub("^\\s*(.*?)\\s*$", "\\1", x) }

auto_quote <- function(x) {
  needs_quotes <- !grepl("^('|\").*\\1$", x) & !make.names(x) == x
  x[needs_quotes] <- encodeString(x[needs_quotes], quote = "\"")
  x
}

test_prefix <- function(suite) {
  if (suite == "tests") "test-roxytest-tests-" else "test-biocroxytest-longtests-"
}

test_title <- function(block, suite) {
  object <- block$object
  title <- if (inherits(object, "s4method") && length(object$topic)) {
    paste("Method", object$topic[[1L]])
  } else if (length(object$alias)) {
    name <- object$alias[[1L]]
    if (suite == "tests") { name <- auto_quote(name) }
    paste0("Function ", name, "()")
  } else {
    "[unknown alias]"
  }
  if (!is.null(block$line)) { title <- paste0(title, " @ L", block$line) }
  title
}

process_test_suites <- function(blocks, suites, cases = NULL) {
  for (block in blocks) {
    if (length(roxygen2::block_get_tags(block, "rdname")) > 1L) {
      cli::cli_abort("Did not expect two {.code @rdname} tags in a block.", call = NULL)
    }
  }
  stats::setNames(lapply(suites, function(suite) {
    process_test_suite(blocks, suite, cases)
  }), suites)
}

process_test_suite <- function(blocks, suite, cases) {
  tests <- list()
  for (block in blocks) {
    tags <- roxygen2::block_get_tags(block, suite)
    if (!length(tags)) { next }

    path <- basename(block$file)
    code <- vapply(tags, function(tag) indent_text(prepare_tests(tag$raw)), character(1))
    title <- test_title(block, suite)
    titles <- rep(title, length(code))
    if (length(code) > 1L) {
      titles[-1L] <- paste0(title, " [", seq.int(2L, length(code)), "]")
    }
    tests[[path]] <- c(tests[[path]], paste0(
      "test_that(", encodeString(titles, quote = '"'), ", {\n", code, "\n})\n"
    ))
  }

  rows <- cases[cases$suite == suite, , drop = FALSE]
  for (i in seq_along(rows$id)) {
    path <- basename(rows$source[i])
    tests[[path]] <- c(tests[[path]], paste0(rows$code[i], "\n"))
  }
  paths <- names(tests)
  stats::setNames(lapply(paths, function(path) {
    source <- if (path == "<text>") path else paste0("File R/", auto_quote(path), ": @", suite)
    paste0(generated_test_header, "\n\n# ", source, "\n\n",
           paste(tests[[path]], collapse = "\n\n"))
  }), paths)
}

plan_test_suite <- function(results, base_path, suite) {
  path <- file.path(base_path, suite, "testthat")
  sources <- names(results)
  filenames <- paste0(test_prefix(suite), sources, recycle0 = TRUE)
  if (length(sources) != length(results) || anyNA(sources) ||
      any(!nzchar(sources)) || any(grepl("[/\\\\]", filenames)) ||
      any(!grepl("[.][Rr]$", filenames))) {
    cli::cli_abort("Invalid generated test filename for {.code {suite}}.", call = NULL)
  }

  keys <- tolower(filenames)
  duplicate <- anyDuplicated(keys)
  if (duplicate) {
    cli::cli_abort("Duplicate generated test destination: {.file {filenames[duplicate]}}.",
                   call = NULL)
  }

  if (length(results) && !dir.exists(path)) {
    setup <- if (suite == "tests") "usethis::use_testthat()" else "biocroxytest::use_longtests()"
    cli::cli_abort(c(
      "Test output directory does not exist: {.path {path}}.",
      i = "Run {.code {setup}} first."
    ), call = NULL)
  }

  existing <- list.files(path, all.files = TRUE, no.. = TRUE)
  destinations <- file.path(path, filenames)
  for (i in seq_along(results)) {
    aliases <- existing[tolower(existing) == keys[i]]
    destination <- destinations[i]
    if (any(aliases != filenames[i]) ||
        ((file.exists(destination) || is_symlink(destination)) &&
         (!filenames[i] %in% existing || !made_by_biocroxytest(destination, suite)))) {
      cli::cli_abort("Test destination collides with an existing file: {.file {destination}}.",
                     call = NULL)
    }
    tryCatch(
      parse(text = results[[i]], srcfile = srcfilecopy(filenames[i], results[[i]])),
      error = function(e) {
        cli::cli_abort("Invalid generated {suite} for {.file {sources[i]}}.",
                       parent = e, call = NULL)
      }
    )
  }

  obsolete <- file.path(path, setdiff(existing, filenames))
  obsolete <- obsolete[vapply(obsolete, made_by_biocroxytest, logical(1), suite = suite)]
  list(path = path, filenames = filenames, destinations = destinations,
       obsolete = obsolete, results = results)
}

stage_test_suite <- function(plan, stage) {
  # Use the real basenames to catch aliases and invalid names on this volume.
  if (!dir.create(stage, mode = "0700")) {
    cli::cli_abort("Could not stage generated tests in {.path {plan$path}}.", call = NULL)
  }
  staged <- file.path(stage, plan$filenames)
  for (i in seq_along(staged)) {
    if (file.exists(staged[i])) {
      cli::cli_abort("Duplicate generated test destination: {.file {plan$filenames[i]}}.",
                     call = NULL)
    }
    tryCatch(
      writeLines(enc2utf8(plan$results[[i]]), staged[i], useBytes = TRUE),
      error = function(e) {
        cli::cli_abort("Could not stage generated test: {.file {plan$filenames[i]}}.",
                       parent = e, call = NULL)
      }
    )
  }
  staged
}

write_test_suites <- function(results, base_path) {
  paths <- file.path(base_path, names(results), "testthat")
  if (anyDuplicated(normalizePath(paths[dir.exists(paths)]))) {
    cli::cli_abort("Ordinary and long tests need separate {.path testthat} directories.",
                   call = NULL)
  }
  plans <- Map(plan_test_suite, results, suite = names(results),
               MoreArgs = list(base_path = base_path))
  stages <- character()
  on.exit(unlink(stages, recursive = TRUE, expand = FALSE), add = TRUE)
  for (i in seq_along(plans)) {
    plan <- plans[[i]]
    if (!length(plan$results)) { next }
    stage <- tempfile(".biocroxytest-", tmpdir = plan$path)
    stages <- c(stages, stage)
    plans[[i]]$staged <- stage_test_suite(plan, stage)
  }

  for (plan in plans) {
    for (i in seq_along(plan$destinations)) {
      if (!file.rename(plan$staged[i], plan$destinations[i])) {
        cli::cli_abort("Could not update generated test: {.file {plan$destinations[i]}}.",
                       call = NULL)
      }
    }
  }
  for (plan in plans) {
    if (length(plan$obsolete) && unlink(plan$obsolete, expand = FALSE) != 0L) {
      cli::cli_abort("Could not remove obsolete generated tests in {.path {plan$path}}.",
                     call = NULL)
    }
  }
  record_suites <- getOption("biocroxytest.record_test_suites")
  if (is.function(record_suites)) { record_suites(names(results)) }
  unlist(lapply(results, names), use.names = FALSE)
}
