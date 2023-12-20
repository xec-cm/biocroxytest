test_that("roclet_output.roclet_longtests returns correct paths", {

  # Setup:
  base_path <- tempdir()
  longtests_path <- file.path(base_path, "longtests", "testthat")
  unlink(longtests_path, recursive = TRUE, force = TRUE)
  results <- roxygen2::roc_proc_text(longtests_roclet(), "
    #' Summing two numbers
    #'
    #' @param x A number
    #' @param y Another number
    #'
    #' @longtests
    #' expect_equal(f(0, 0), 0)
    #' expect_equal(f(2, 3), 5)
    f <- function(x, y) {
      x + y
    }")

  # Check error when longtests directory does not exist
  expect_error(roclet_output.roclet_longtests(NULL, results, base_path))

  # Act: Create a dummy longtests directory
  dir.create(longtests_path, recursive = TRUE, showWarnings = FALSE)

  # Check that the function returns the correct paths
  roclet_output.roclet_longtests(NULL, results, base_path) |>
    expect_equal("<text>")
})
