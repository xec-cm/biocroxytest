#' Check if longtests are used in the package
#'
#' This function checks if the directory "longtests" exists in the current
#' package. If it does not exist, the function will throw an error message and
#' suggest setting it up using the `biocroxytest::use_longtests()` function.
#'
#' @param base_path The base path of the package.
#'
#' @return TRUE if "longtests" directory exists, FALSE otherwise.
#' @keywords internal
verify_longtests_used <- function(base_path = ".") {
  if (!dir.exists(file.path(base_path, "longtests"))) {
    cli::cli_abort(c(
      "The package has no {.path longtests} directory.",
      i = "Set it up with {.code biocroxytest::use_longtests()}."
    ), call = NULL)
  }
  TRUE
}
