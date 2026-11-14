# biocroxytest (development version)

* Copy and fingerprint package and test inputs through a manifest, with
  opt-in paths for extra inputs and support for older replay records (#37).
* Match build exclusions case-insensitively for test plans and roclets (#38).

* Add `check_test_plan()` to require completed cases in CI, with a policy
  for recorded warnings (#35).
* Share named-case execution and support time limits for isolated runners and
  replay (#36).

* Verify installed consumer tests in CI and require roxygen2 7.1.2 or newer
  for conditional examples (#34).

* Pilot `reduce_paired_failure()` for finding a smaller SummarizedExperiment
  that retains whole pairs, an estimable design and a specific failure (#15).

* Export a failed named case and replay it with the recorded R seed, source
  revision and verified fixture identities (#14).

* Declare checksummed fixtures for named cases. Check required files before
  execution, record fixture evidence and reuse optional HTTPS downloads
  through BiocFileCache (#13).

* Add `run_test_plan()` to report which named cases ran, how long they
  took and the package revision and R environment used (#12).
* Keep test fixture output out of the progress reporter.

* Support `@testexamples` in the unified roclet. Run documented examples
  before their assertions with Rd execution rules and runtime conditions (#11).

* Add `@testCases` for named cases that share expectations, with a plan and
  local selection by stable ID. Exercise S4 sample alignment in memory and
  on HDF5 (#8, #9).
* Add `compare_test_plans()` to detect additions, removals, suite moves,
  and content changes by case ID (#10).

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

