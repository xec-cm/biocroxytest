#' Check if a file is generated by biocroxytest
#'
#' This is an internal function that checks if a file at a given path is
#' generated by biocroxytest. It checks if the file exists, if the name matches
#' the pattern "test-biocroxytest-.*\\.R$", and if the header of the file starts
#' with "Generated by biocroxytest". If the file exists and the name matches the
#' pattern but the header does not start with "Generated by biocroxytest", the
#' function will return FALSE.
#'
#' @param path A character string representing the path of the file to check.
#'
#' @return TRUE if all checks pass, FALSE otherwise.
#' @keywords internal
made_by_biocroxytest <- function(path) {
  check_files <- file.exists(path)
  check_name <- stringr::str_detect(path, "test-biocroxytest-.*[.]R$")
  check_header <- if (check_files) {
    res <-
      readLines(path, n = 1, encoding = "UTF-8", warn = FALSE) |>
      stringr::str_detect("^. Generated by biocroxytest")
    if (length(res) == 0) { res <- FALSE }
    res
  }

  if (is.null(check_header)) { check_header <- TRUE }

  result <- TRUE
  if (all(check_files, check_name) & !check_header) {
    result <- FALSE
  }

  result
}
