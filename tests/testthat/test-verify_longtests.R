# Unit tests
test_that("verify_longtests_used returns TRUE when 'longtests' directory exists", {
  # Create a temporary directory for testing
  old <- setwd(tempdir())
  dir.create("longtests/testhat", recursive = TRUE, showWarnings = FALSE)

  # Test the function
  expect_true(verify_longtests_used())

  # Clean up
  unlink("longtests", recursive = TRUE)
  setwd(old)
})

test_that("verify_longtests_used throws an error when 'longtests' directory does not exist", {
  # Create a temporary directory for testing
  old <- setwd(tempdir())

  # Test the function
  expect_error(verify_longtests_used())

  # Clean up
  setwd(old)
})
