#' @importFrom roxygen2 roxy_tag_parse
#' @export
roxy_tag_parse.roxy_tag_testexamples <- function(x) {
  if (!nzchar(trimws(x$raw))) {
    return(roxygen2::roxy_tag_warning(x, "requires a value"))
  }
  # roxytest may also use the registered parser in a separate run.
  x$val <- sub("^\n", "", x$raw)
  x
}

example_test_prefix <- "test-roxytest-testexamples-"

process_test_examples <- function(blocks, env, base_path) {
  examples <- lapply(blocks, function(block) {
    Filter(function(tag) tag$tag %in% c("examples", "example", "examplesIf"), block$tags)
  })
  assertions <- lapply(blocks, roxygen2::block_get_tags, "testexamples")
  if (!any(lengths(assertions))) { return(list()) }

  relevant <- lengths(examples) > 0L | lengths(assertions) > 0L
  topics <- rep(NA_character_, length(blocks))
  topics[relevant] <- vapply(blocks[relevant], example_topic, character(1),
                             env = env, base_path = base_path)
  bodies <- list()
  for (i in which(lengths(assertions) > 0L)) {
    block <- blocks[[i]]
    peers <- if (is.na(topics[i])) i else which(topics == topics[i])
    tags <- unlist(examples[peers], recursive = FALSE)
    if (!length(tags)) {
      abort_test_example(assertions[[i]][[1L]], "Add an example to the documented topic.")
    }
    setup <- render_test_examples(tags, base_path, env)
    title <- test_title(block, "testexamples")
    names <- rep(title, length(assertions[[i]]))
    if (length(names) > 1L) {
      names[-1L] <- paste0(title, " [", seq.int(2L, length(names)), "]")
    }
    checks <- vapply(assertions[[i]], function(tag) {
      indent_text(prepare_tests(tag$raw))
    }, character(1))
    source <- basename(block$file)
    bodies[[source]] <- c(bodies[[source]], paste0(
      "test_that(", encodeString(names, quote = '"'), ", {\n", setup, "\n", checks, "\n})\n"
    ))
  }
  paths <- names(bodies)
  stats::setNames(lapply(paths, function(path) {
    source <- if (path == "<text>") path else paste0("File R/", auto_quote(path), ": @testexamples")
    paste0(generated_test_header, "\n\n# ", source, "\n\n",
           paste(bodies[[path]], collapse = "\n\n"))
  }), paths)
}

example_topic <- function(block, env, base_path) {
  # Ask the Rd roclet for the filename rather than reproducing its mapping
  # of object names, operators and @describeIn destinations to Rd pages.
  if (length(roxygen2::block_get_tags(block, "noRd"))) { return(NA_character_) }
  identity <- block
  identity$tags <- Filter(function(tag) {
    tag$tag %in% c("name", "rdname", "describeIn")
  }, block$tags)
  if (!length(identity$object$topic) &&
      !length(roxygen2::block_get_tags(identity, "name"))) {
    return(NA_character_)
  }
  # A title makes the identity block a documentable topic. Omit all executable
  # tags and templates from this probe.
  title <- roxygen2::roxy_tag("title", "Example topic", val = "Example topic")
  identity$tags <- c(identity$tags, list(title))
  topics <- roxygen2::roclet_process(roxygen2::rd_roclet(), list(identity), env, base_path)
  if (length(topics)) names(topics)[[1L]] else NA_character_
}

render_test_examples <- function(tags, base_path, env) {
  code <- lapply(tags, function(tag) {
    section <- roxygen2::roxy_tag_rd(tag, base_path, env)
    if (is.null(section)) {
      abort_test_example(tag, "Could not read the documented example.")
    }
    if (tag$tag == "examplesIf") {
      conditional_example(section$value, tag)
    } else {
      as.character(section$value)
    }
  })
  code <- render_rd_example(unlist(code, use.names = FALSE), tags[[1L]])
  indent_text(code)
}

conditional_example <- function(value, tag) {
  lines <- strsplit(as.character(value), "\n", fixed = TRUE)[[1L]]
  # Roxygen wraps @examplesIf in two hidden Rd markers. Keep its condition
  # at the same point in the example sequence and skip assertions if false.
  open <- "\\dontshow{if ("
  suffixes <- c(
    ") withAutoprint(\\{ # examplesIf}",
    ') (if (getRversion() >= "3.4") withAutoprint else force)(\\{ # examplesIf}'
  )
  close <- "\\dontshow{\\}) # examplesIf}"
  suffix <- suffixes[endsWith(lines[1L], suffixes)]
  if (length(lines) < 3L || !startsWith(lines[1L], open) ||
      length(suffix) != 1L || lines[length(lines)] != close) {
    abort_test_example(tag, "Could not interpret the roxygen @examplesIf wrapper.")
  }
  condition <- substr(lines[1L], nchar(open) + 1L, nchar(lines[1L]) - nchar(suffix))
  c(paste0("if (", condition, ') {} else testthat::skip("@examplesIf condition is false")'),
    lines[-c(1L, length(lines))])
}

render_rd_example <- function(lines, tag) {
  if (!length(lines)) { return(character()) }
  input <- paste(c("\\name{example-test}", "\\title{Example test}",
                   "\\encoding{UTF-8}", "\\examples{", lines, "}"), collapse = "\n")
  rd_input <- rawConnection(charToRaw(enc2utf8(input)))
  on.exit(close(rd_input), add = TRUE)
  output <- rawConnection(raw(), "w")
  on.exit(close(output), add = TRUE)
  withCallingHandlers({
    rd <- tools::parse_Rd(rd_input, encoding = "UTF-8")
    if (rd_has_dynamic_example(rd)) {
      abort_test_example(tag, "Rd \\Sexpr expressions are not supported in tested examples.")
    }
    tools::Rd2ex(rd, out = output, commentDontrun = TRUE,
                 commentDonttest = FALSE, stages = character())
  }, warning = function(w) {
    abort_test_example(tag, "Could not render the documented Rd example.", parent = w)
  })
  rendered <- rawToChar(rawConnectionValue(output))
  if (!nzchar(rendered)) { return("") }
  Encoding(rendered) <- "UTF-8"
  marker <- "### ** Examples\n"
  start <- regexpr(marker, rendered, fixed = TRUE)[1L]
  if (start < 0L) { abort_test_example(tag, "Could not extract the rendered examples.") }
  sub("\n+$", "", sub("^\n", "", substring(rendered, start + nchar(marker))))
}

rd_has_dynamic_example <- function(node) {
  identical(attr(node, "Rd_tag"), "\\Sexpr") ||
    (is.list(node) && any(vapply(node, rd_has_dynamic_example, logical(1))))
}

abort_test_example <- function(tag, message, parent = NULL) {
  cli::cli_abort(c(
    "Invalid {.code @testexamples} at {.file {basename(tag$file)}} line {tag$line}.",
    i = "{message}"
  ), class = "biocroxytest_invalid_test_example", parent = parent, call = NULL)
}
