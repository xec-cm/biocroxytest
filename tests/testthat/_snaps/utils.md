# internal_longtests_roclet_clean returns a message for files not created by biocroxytest

    Code
      expect_equal(internal_longtests_roclet_output(results, longtests_path), "-1.R")
    Message
      ! Clean-up: Some files in longtests/ with the file name pattern test-biocroxytest-*.R was not created by biocroxytest (missing header), and hence was not removed!
      * Please be sure that this is intended.

