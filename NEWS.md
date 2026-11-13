# biocroxytest (development version)

* Add `generated_test_inventory()` and `check_generated_tests()`. The
  CI check reports missing, changed, and obsolete generated tests after
  regenerating in a temporary copy (#7).
* Use cli for setup and generation diagnostics.

* Add `tests_roclet()` to generate ordinary `@tests` and Bioconductor
  `@longtests` together. Migrating dar in a temporary checkout preserved
  its ordinary test paths, descriptions and snapshots (#5, #6).

* Make `use_longtests()` safe to rerun. Create a runner for the package in
  `DESCRIPTION`, preserve existing runners, and update the active BBS option
  while retaining comments and unrelated settings (#4).

* Validate all generated long tests before replacing files, including with
  `clean = TRUE`. Preserve hand-written tests and report name collisions (#3).

* Generate a separate test for every `@longtests` tag, keeping tests with their
  source files even when they share an `@rdname`. Preserve multiline literals
  and generate valid test titles for S4 methods and non-syntactic names (#2).

# biocroxytest 0.99.3


# biocroxytest 0.99.2

### Bug fixes

* Remove .Rproj file

# biocroxytest 0.99.1

### Bug fixes

* Add files to .gitignore

# biocroxytest 0.99.0

### New features

* Initial Bioconductor submission

