roxygenise_fixture <- function(..., load_code = roxygen2::load_source) {
  suppressMessages(roxygen2::roxygenise(..., load_code = load_code))
}

check_generated_fixture <- function(path) {
  output <- withr::local_tempfile()
  on.exit({
    lines <- readLines(output, warn = FALSE)
    plain <- cli::ansi_strip(lines)
    chatter <- grepl(
      paste0(
        "^(First time using roxygen2[.] Upgrading automatically[.]{3}|",
        ". Setting Config/roxygen2/version to |. Loading [[:alnum:].]+$|",
        "Writing '[^']+[.]Rd'$)"
      ),
      plain
    )
    writeLines(lines[!chatter])
  }, add = TRUE, after = FALSE)
  withr::with_output_sink(output, check_generated_tests(path))
}
