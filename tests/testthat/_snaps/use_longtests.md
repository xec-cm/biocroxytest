# use_longtests works as expected

    Code
      use_longtests()
    Message
      v The .BBSoptions file has been created.
      v Adding RunLongTests: TRUE to the .BBSoptions file.
      v The longtests directory has been created.
      v Adding testthat.R to the longtests directory.

# setup_bboptions works as expected

    Code
      use_longtests()
    Message
      v The .BBSoptions file has been created.
      v Adding RunLongTests: TRUE to the .BBSoptions file.
      v The longtests directory has been created.
      v Adding testthat.R to the longtests directory.

# setup_bboptions handles existing .BBSoptions with RunLongTests: FALSE

    Code
      use_longtests()
    Message
      i The .BBSoptions file contains RunLongTests: FALSE
      v Changing RunLongTests: FALSE to RunLongTests: TRUE
      v The longtests directory has been created.
      v Adding testthat.R to the longtests directory.

# setup_bboptions handles existing .BBSoptions with RunLongTests: TRUE

    Code
      use_longtests()
    Message
      i The .BBSoptions file contains RunLongTests: TRUE
      v Nothing to do.
      v The longtests directory has been created.
      v Adding testthat.R to the longtests directory.

# setup_bboptions handles existing .BBSoptions with no RunLongTests correctly

    Code
      use_longtests()
    Message
      i The .BBSoptions file does not contain RunLongTests
      v Adding RunLongTests: TRUE to the .BBSoptions file
      v The longtests directory has been created.
      v Adding testthat.R to the longtests directory.

# use_longtests works as expected when test folder exists

    Code
      use_longtests()
    Message
      v The .BBSoptions file has been created.
      v Adding RunLongTests: TRUE to the .BBSoptions file.
      v The longtests directory has been created.
      v Adding testthat.R to the longtests directory.

