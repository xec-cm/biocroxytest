# biocroxytest (development version)

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

