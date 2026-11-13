test_that("indent_text correctly indents text", {
  # Test input and expected output
  test_input <- "Hello\nWorld"
  expected_output <- "  Hello\n  World"

  # Check if the function output matches the expected output
  expect_equal(indent_text(test_input), expected_output)
})

test_that("prepare_tests correctly trims whitespace", {
  # Test input and expected output
  test_input <- "  Hello World  "
  expected_output <- "Hello World"

  # Check if the function output matches the expected output
  expect_equal(prepare_tests(test_input), expected_output)
})

test_that("auto_quote correctly adds quotes", {
  # Test input and expected output
  test_strings <- c("hello", "'hello'", "\"hello\"", "hello world", "1_hello")
  expected_output <-
    c("hello", "'hello'", "\"hello\"", "\"hello world\"", "\"1_hello\"")

  # Check if the result matches the expected output
  expect_equal(auto_quote(test_strings), expected_output)
})
