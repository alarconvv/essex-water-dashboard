# tests/testthat.R
#
# Standard testthat runner. This project is not an installable package, so
# there is no library() call for the app itself -- helper-*.R files in
# tests/testthat/ source the R/ (and etl/) files the tests need.
#
# Run from the project root:
#   Rscript tests/testthat.R
library(testthat)

testthat::test_dir("tests/testthat")
