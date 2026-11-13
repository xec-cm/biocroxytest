with_setup_package <- function(code) {
  pkg <- tempfile("longtests-setup-")
  dir.create(pkg)
  old <- setwd(pkg)
  on.exit({
    setwd(old)
    unlink(pkg, recursive = TRUE, expand = FALSE)
  }, add = TRUE)
  writeLines(c("Package: setupfixture", "Version: 0.0.1"), "DESCRIPTION")
  force(code)
}

test_that("use_longtests creates a runner for the current package", {
  with_setup_package({
    use_longtests()
    runner <- readLines("longtests/testthat.R")
    expect_true(dir.exists("longtests/testthat"))
    expect_true('library("setupfixture")' %in% runner)
    expect_true('test_check("setupfixture")' %in% runner)
    expect_true("RunLongTests: TRUE" %in% readLines(".BBSoptions"))
  })
})

test_that("use_longtests copies a runner once and preserves it on reruns", {
  with_setup_package({
    dir.create("tests")
    original <- c("# Ordinary runner", "testthat::test_check('setupfixture')")
    writeLines(original, "tests/testthat.R")
    use_longtests()
    expect_identical(readLines("longtests/testthat.R"), original)

    custom <- c("# Custom long test runner", "options(my_test_mode = TRUE)")
    writeLines(custom, "longtests/testthat.R")
    writeLines("# A manual long test", "longtests/testthat/test-manual.R")
    use_longtests()
    expect_identical(readLines("longtests/testthat.R"), custom)
    expect_identical(readLines("longtests/testthat/test-manual.R"), "# A manual long test")
  })
})

test_that("use_longtests ignores commented options and updates only the active field", {
  with_setup_package({
    preserved <- c("# RunLongTests: FALSE", "UnsupportedPlatforms: win", "  linux")
    writeLines(preserved, ".BBSoptions")
    use_longtests()
    expect_identical(readLines(".BBSoptions"), c(preserved, "RunLongTests: TRUE"))

    writeLines(c(preserved, "RunLongTests:\tfalse", "GPU_reliance: optional"), ".BBSoptions")
    use_longtests()
    expect_identical(
      readLines(".BBSoptions"),
      c(preserved, "RunLongTests: TRUE", "GPU_reliance: optional")
    )

    current <- c(preserved, "RunLongTests: true")
    writeLines(current, ".BBSoptions")
    use_longtests()
    expect_identical(readLines(".BBSoptions"), current)

    # BBS merges records and recognizes continuations after a comment.
    writeLines(c("RunLongTests: TRUE", "", "RunLongTests:", "# keep", "  false",
                 "GPU_reliance: optional"), ".BBSoptions")
    use_longtests()
    expect_identical(
      readLines(".BBSoptions"),
      c("RunLongTests: TRUE", "", "RunLongTests: TRUE", "# keep",
        "GPU_reliance: optional")
    )
  })
})

test_that("use_longtests requires DESCRIPTION before changing files", {
  with_setup_package({
    unlink("DESCRIPTION")
    expect_error(use_longtests(), "DESCRIPTION")
    expect_false(file.exists(".BBSoptions"))
    expect_false(dir.exists("longtests"))
  })
})

test_that("use_longtests removes a BOM that hides the first field from BBS", {
  with_setup_package({
    bom <- as.raw(c(0xef, 0xbb, 0xbf))
    expected <- c("RunLongTests: TRUE", "GPU_reliance: optional")
    writeBin(c(bom, charToRaw(paste0(expected, "\n", collapse = ""))), ".BBSoptions")
    use_longtests()
    expect_false(identical(readBin(".BBSoptions", "raw", n = 3L), bom))
    expect_identical(readLines(".BBSoptions"), expected)
  })
})

test_that("use_longtests does not enable BBS when a runner cannot be created", {
  with_setup_package({
    dir.create("longtests/testthat.R", recursive = TRUE)
    writeLines("RunLongTests: FALSE", ".BBSoptions")
    expect_error(use_longtests(), "existing long test runner")
    expect_identical(readLines(".BBSoptions"), "RunLongTests: FALSE")
    expect_true(dir.exists("longtests/testthat.R"))
  })
})

test_that("a failed ordinary runner copy leaves setup safe to retry", {
  with_setup_package({
    dir.create("tests")
    ordinary <- "tests/testthat.R"
    runner <- "longtests/testthat.R"
    writeLines("# Ordinary runner", ordinary)
    Sys.chmod(ordinary, "0000")
    if (file.access(ordinary, 4L) == 0L) { skip("Cannot make the fixture unreadable") }
    expect_error(use_longtests(), "Could not copy")
    expect_false(file.exists(runner))
    expect_false(file.exists(".BBSoptions"))
    Sys.chmod(ordinary, "0600")
    use_longtests()
    expect_identical(readLines(runner), "# Ordinary runner")
    expect_identical(readLines(".BBSoptions"), "RunLongTests: TRUE")
  })
})

