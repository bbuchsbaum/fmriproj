# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#testing-design-tests-files

library(testthat)
library(fmriproj)

# Enable testthat edition 3 for snapshot testing
local_edition(3)

test_check("fmriproj") 