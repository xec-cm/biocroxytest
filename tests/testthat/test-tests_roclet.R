with_test_suites <- function(code) {
  pkg <- tempfile("test-suites-")
  dir.create(file.path(pkg, "R"), recursive = TRUE)
  dir.create(file.path(pkg, "tests", "testthat"), recursive = TRUE)
  dir.create(file.path(pkg, "longtests", "testthat"), recursive = TRUE)
  old <- setwd(pkg)
  on.exit({
    setwd(old)
    unlink(pkg, recursive = TRUE, expand = FALSE)
  }, add = TRUE)
  writeLines(c("Package: testfixture", "Version: 0.0.1", "Encoding: UTF-8"),
             "DESCRIPTION")
  writeLines(character(), "NAMESPACE")
  writeLines("# Existing ordinary runner", "tests/testthat.R")
  writeLines("# Existing long runner", "longtests/testthat.R")
  force(code)
}

generate_test_suites <- function(roclets = "biocroxytest::tests_roclet", clean = FALSE) {
  roxygen2::roxygenise(roclets = roclets, load_code = roxygen2::load_source, clean = clean)
}

test_that("the unified roclet keeps test bodies with their own file and suite", {
  with_test_suites({
    writeLines(c(
      "#' @rdname shared",
      "#' @tests expect_true(TRUE)",
      "#' @tests expect_false(FALSE)",
      "#' @longtests expect_equal(foo(), 1)",
      "foo <- function() 1"
    ), "R/a.R")
    writeLines(c(
      "#' @rdname shared",
      "#' @tests expect_identical(\"first",
      "#' second\", paste(\"first\", \"second\", sep = \"\\n\"))",
      "#' @longtests expect_equal(`a-b`(), 2)",
      "`a-b` <- function() 2"
    ), "R/b.R")

    generate_test_suites()
    ordinary <- file.path("tests/testthat", paste0("test-roxytest-tests-", c("a.R", "b.R")))
    long <- file.path("longtests/testthat", paste0("test-biocroxytest-longtests-", c("a.R", "b.R")))
    expect_true(all(file.exists(c(ordinary, long))))
    tests <- lapply(c(ordinary, long), parse)
    expect_identical(lengths(tests), c(2L, 1L, 1L, 1L))
    expect_identical(tests[[1]][[1]][[2]], "Function foo() @ L5")
    expect_identical(tests[[1]][[2]][[2]], "Function foo() @ L5 [2]")
    expect_identical(tests[[2]][[1]][[2]], 'Function "a-b"() @ L5')
    expect_identical(tests[[2]][[1]][[3]][[2]][[2]], "first\nsecond")
    expect_match(tests[[3]][[1]][[2]], "Function foo()", fixed = TRUE)
    expect_match(tests[[4]][[1]][[2]], "Function a-b()", fixed = TRUE)
    expect_identical(readLines("tests/testthat.R"), "# Existing ordinary runner")
    expect_identical(readLines("longtests/testthat.R"), "# Existing long runner")
  })
})

test_that("unified validation precedes replacement or cleanup in either suite", {
  with_test_suites({
    old <- c("tests/testthat/test-roxytest-tests-a.R",
             "tests/testthat/test-roxytest-tests-stale.R",
             "longtests/testthat/test-biocroxytest-longtests-a.R")
    manual <- c("tests/testthat/test-roxytest-tests-manual.R",
                "tests/testthat/test-roxytest-testexamples-keep.R")
    before <- c(roxytest_header, "# ordinary test")
    for (path in old[1:2]) { writeLines(before, path) }
    writeLines(c(generated_test_header, "# long test"), old[3])
    writeLines("# manual test", manual[1])
    writeLines(roxytest_header, manual[2])
    dir.create("tests/testthat/_snaps")
    writeLines("Snapshot", "tests/testthat/_snaps/roxytest-tests-a.md")
    files <- c(old, manual, "tests/testthat/_snaps/roxytest-tests-a.md")
    contents <- lapply(files, readLines)

    writeLines(c("#' @tests expect_true(TRUE)", "#' @longtests expect_true(",
                 "a <- function() TRUE"), "R/a.R")
    for (clean in c(FALSE, TRUE)) {
      expect_error(generate_test_suites(clean = clean), "Invalid generated longtests")
      expect_identical(lapply(files, readLines), contents)
    }

    writeLines(c("#' @tests expect_true(TRUE)", "#' @longtests expect_true(TRUE)",
                 "a <- function() TRUE"), "R/a.R")
    writeLines(c("#' @tests expect_true(TRUE)", "manual <- function() TRUE"), "R/manual.R")
    expect_error(generate_test_suites(clean = TRUE), "collides.*manual[.]R")
    expect_identical(lapply(files, readLines), contents)

    unlink("R/manual.R")
    generate_test_suites(clean = TRUE)
    expect_identical(readLines(old[1], n = 1), generated_test_header)
    expect_false(file.exists(old[2]))
    expect_identical(lapply(files[4:6], readLines), contents[4:6])
    first <- lapply(files[-2], readLines)
    generate_test_suites(clean = TRUE)
    expect_identical(lapply(files[-2], readLines), first)
  })
})

test_that("an unused suite does not require setup", {
  with_test_suites({
    unlink("longtests", recursive = TRUE)
    writeLines(c("#' @tests expect_true(TRUE)", "a <- function() TRUE"), "R/a.R")
    generate_test_suites()
    expect_true(file.exists("tests/testthat/test-roxytest-tests-a.R"))
    expect_false(dir.exists("longtests"))
  })
})

test_that("the suites cannot share one output directory", {
  with_test_suites({
    unlink("longtests/testthat", recursive = TRUE)
    if (!file.symlink("../tests/testthat", "longtests/testthat")) {
      skip("Cannot create a directory symlink")
    }
    writeLines(c("#' @tests expect_true(TRUE)", "#' @longtests expect_false(FALSE)",
                 "a <- function() TRUE"), "R/a.R")
    expect_error(generate_test_suites(clean = TRUE), "separate testthat directories")
    expect_length(list.files("tests/testthat"), 0L)
  })
})

test_that("competing roclets are rejected before clean can delete tests", {
  skip_if_not_installed("roxytest")
  with_test_suites({
    path <- "tests/testthat/test-roxytest-tests-a.R"
    writeLines(roxytest_header, path)
    writeLines(c("#' @tests expect_true(TRUE)", "a <- function() TRUE"), "R/a.R")
    unified <- "biocroxytest::tests_roclet"
    for (legacy in c("roxytest::testthat_roclet", "(roxytest:::testthat_roclet)()",
                     "biocroxytest::longtests_roclet")) {
      for (roclets in list(c(unified, legacy), c(legacy, unified))) {
        expect_error(generate_test_suites(roclets, clean = TRUE), "Use .* alone")
        expect_identical(readLines(path), roxytest_header)
      }
    }

    dynamic <- 'getExportedValue("roxytest", "testthat_roclet")'
    for (roclets in list(c(unified, dynamic), c(dynamic, unified))) {
      expect_error(generate_test_suites(roclets, clean = TRUE), "Use clean = FALSE")
      expect_identical(readLines(path), roxytest_header)
      expect_error(generate_test_suites(roclets), "Use .* alone")
      expect_identical(readLines(path), roxytest_header)
    }

    alias <- "biocroxytest_test_roclet_alias"
    assign(alias, roxytest::testthat_roclet, envir = globalenv())
    on.exit(rm(list = alias, envir = globalenv()), add = TRUE)
    for (roclets in list(c(unified, alias), c(alias, unified))) {
      expect_error(generate_test_suites(roclets, clean = TRUE), "Use clean = FALSE")
      expect_identical(readLines(path), roxytest_header)
      expect_error(generate_test_suites(roclets), "Use .* alone")
      expect_identical(readLines(path), roxytest_header)
    }

    # Explicit overrides take precedence over a package's configured roclets.
    cat('Roxygen: list(roclets = "roxytest::testthat_roclet")\n',
        file = "DESCRIPTION", append = TRUE)
    generate_test_suites(unified, clean = TRUE)
    expect_identical(readLines(path, n = 1), generated_test_header)
  })
})

test_that("the unified roclet restores the ordinary tag if its parser is removed", {
  with_test_suites({
    writeLines(c("#' @tests expect_true(TRUE)", "a <- function() TRUE"), "R/a.R")
    generate_test_suites()
    path <- "tests/testthat/test-roxytest-tests-a.R"
    before <- readLines(path)

    # pkgload's unload removes the shared S3 entry, even with biocroxytest loaded.
    methods <- get(".__S3MethodsTable__.", asNamespace("roxygen2"), inherits = FALSE)
    name <- "roxy_tag_parse.roxy_tag_tests"
    previous <- get(name, envir = methods, inherits = FALSE)
    on.exit(assign(name, previous, envir = methods), add = TRUE)
    rm(list = name, envir = methods)
    expect_no_warning(generate_test_suites())
    expect_identical(readLines(path), before)
  })
})
