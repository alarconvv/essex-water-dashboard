# tests/testthat/helper-load.R
#
# testthat sources every helper-*.R file in this directory before running
# any test-*.R file. The project isn't an installable package, so we source
# the app-layer files directly.
#
# testthat::test_path() resolves relative to tests/testthat/, so these paths
# work regardless of the working directory the suite was launched from.
#
# Only files that exist are sourced, so this helper stays safe before
# R/data_access.R has been written.

# components.R uses unqualified shiny/htmltools tag builders (tags, div,
# span, icon), so shiny must be attached.
suppressPackageStartupMessages({
  library(htmltools)
  library(shiny)
})

.app_r_files <- c("helpers.R", "components.R", "data_access.R")

for (.f in .app_r_files) {
  .p <- testthat::test_path("..", "..", "R", .f)
  if (file.exists(.p)) {
    source(.p, local = FALSE)
  }
}
rm(.f, .p, .app_r_files)
