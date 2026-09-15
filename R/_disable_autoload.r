# R/_disable_autoload.r
#
# Empty marker file. shiny::loadSupport() looks for a file named
# "_disable_autoload.r" (case-insensitive) directly under R/. When it is
# present, Shiny does not auto-source every R/*.R file into its own internal
# environment.
#
# Without this marker, Shiny would source R/helpers.R, R/components.R,
# R/data_access.R and R/server.R a second time, in addition to the explicit
# source("global.R") / source("R/server.R") calls at the top of app.R. That
# second copy is harmless today but confusing to reason about. With autoload
# off, app.R's explicit sourcing is the single, predictable place where the
# app layer is loaded, exactly as tests/testthat/helper-load.R and
# tests/testthat/test-integration-server.R load it for the test suite.
