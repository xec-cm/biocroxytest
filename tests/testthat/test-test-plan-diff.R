example_test_plan <- function(ids = c("alignment/memory", "alignment/hdf5")) {
  n <- length(ids)
  data.frame(
    id = ids, suite = rep("tests", n), source = rep("R/select.R", n),
    path = rep("tests/testthat/test-roxytest-tests-select.R", n),
    description = rep("Sample identifiers stay aligned", n),
    code = rep("expect_identical(actual, expected)", n)
  )
}

test_that("plan comparison matches stable IDs independently of row order", {
  before <- example_test_plan()
  after <- before[2:1, ]
  names(after$suite) <- c("second", "first")
  before$fixture <- list(list(accession = "first", hash = "a"), NULL)
  after$fixture <- list(NULL, list(accession = "first", hash = "a"))
  original_before <- before
  original_after <- after
  result <- compare_test_plans(before, after)
  expect_s3_class(result, "data.frame")
  expect_identical(nrow(result), 0L)
  expect_type(result$fields, "list")
  expect_identical(before, original_before)
  expect_identical(after, original_after)
})

test_that("plan comparison reports additions and removals with their origins", {
  before <- example_test_plan(c("alignment/old", "alignment/keep"))
  after <- example_test_plan(c("alignment/new", "alignment/keep"))
  after$source[1] <- "R/new.R"
  result <- compare_test_plans(before, after)
  expect_identical(result$id, c("alignment/new", "alignment/old"))
  expect_identical(result$status, c("added", "removed"))
  expect_identical(result$before_source, c(NA_character_, "R/select.R"))
  expect_identical(result$after_source, c("R/new.R", NA_character_))
  expect_identical(result$before_suite, c(NA_character_, "tests"))
  expect_identical(result$after_suite, c("tests", NA_character_))
  expect_identical(result$fields, list(character(), character()))
})

test_that("suite moves and content changes remain separate events", {
  before <- example_test_plan("alignment/hdf5")
  after <- before
  after$suite <- "longtests"
  after$path <- "longtests/testthat/test-biocroxytest-longtests-select.R"
  after$description <- "Identifiers and data stay aligned"
  after$code <- "expect_identical(actual, expected); expect_identical(data, reference)"
  result <- compare_test_plans(before, after)
  expect_identical(result$id, rep("alignment/hdf5", 2L))
  expect_identical(result$status, c("moved", "changed"))
  expect_identical(result$fields, list("suite", c("description", "code")))
  expect_identical(result$before_path, rep(before$path, 2L))
  expect_identical(result$after_path, rep(after$path, 2L))

  after$description <- before$description
  after$code <- before$code
  expect_identical(compare_test_plans(before, after)$status, "moved")
})

test_that("source paths and unrelated metadata do not change case content", {
  before <- example_test_plan()
  after <- before
  after$source <- "R/renamed.R"
  after$path <- "tests/testthat/test-roxytest-tests-renamed.R"
  before$line <- c(2L, 4L)
  after$line <- c(100L, 102L)
  expect_identical(nrow(compare_test_plans(before, after)), 0L)
})

test_that("optional fixture and required changes retain structured cell values", {
  before <- example_test_plan()
  before$fixture <- I(list(list(hash = "a", dimensions = c(3L, 2L)), NULL))
  before$required <- list(c("linux", "mac"), "linux")
  after <- before[2:1, ]
  after$fixture[[2]]$hash <- "b"
  after$required[[2]] <- "linux"
  result <- compare_test_plans(before, after)
  expect_identical(result$id, "alignment/memory")
  expect_identical(result$status, "changed")
  expect_identical(result$fields, list(c("fixture", "required")))

  before$required <- c(TRUE, FALSE)
  after <- before
  after$required[2] <- TRUE
  result <- compare_test_plans(before, after)
  expect_identical(result$id, "alignment/hdf5")
  expect_identical(result$fields, list("required"))
})

test_that("optional metadata is compared only when present in both plans", {
  before <- example_test_plan()
  after <- before
  after$fixture <- c("a", "b")
  before$required <- c(TRUE, FALSE)
  expect_identical(nrow(compare_test_plans(before, after)), 0L)
  expect_identical(nrow(compare_test_plans(after, before)), 0L)
})

test_that("empty plans preserve the result schema", {
  empty <- example_test_plan(character())
  plan <- example_test_plan("alignment/memory")
  unchanged <- compare_test_plans(empty, empty)
  added <- compare_test_plans(empty, plan)
  removed <- compare_test_plans(plan, empty)
  expect_identical(names(unchanged), names(added))
  expect_identical(names(unchanged), names(removed))
  expect_identical(nrow(unchanged), 0L)
  expect_identical(added$status, "added")
  expect_identical(removed$status, "removed")
})

test_that("invalid plans fail with specific input errors", {
  plan <- example_test_plan()
  invalid <- function(x) {
    expect_error(compare_test_plans(x, plan), class = "biocroxytest_invalid_test_plan")
    expect_error(compare_test_plans(plan, x), class = "biocroxytest_invalid_test_plan")
  }
  invalid(as.list(plan))
  invalid(plan[, names(plan) != "code"])
  invalid(plan[c(1, 1), ])
  bad <- plan
  bad$id[1] <- "  "
  invalid(bad)
  bad$id[1] <- NA_character_
  invalid(bad)
  bad <- plan
  bad$suite[1] <- "weekly"
  invalid(bad)
  bad <- plan
  bad$code <- factor(bad$code)
  invalid(bad)
  bad <- plan
  names(bad)[2] <- "id"
  invalid(bad)
  bad <- plan
  bad$fixture <- I(matrix(1:4, nrow = 2))
  invalid(bad)
})
