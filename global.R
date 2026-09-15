# global.R
#
# App startup: packages, the app-layer sources, the read-only store
# connection, and the manual content from config.yml.
#
# app.R sources this file explicitly. Shiny does not source global.R on its
# own for single-file app.R apps (shiny:::shinyAppDir_appR() calls
# loadSupport() with globalrenv = NULL), and R/_disable_autoload.r turns off
# the automatic R/*.R sourcing, so this file is the one place the app layer is
# loaded.
#
# This file must also be safe to source outside a running app (tests, a plain
# Rscript). The onStop() registration below is guarded for that reason.
#
# No network code lives in the app layer. Every number the app shows comes
# from data/essexwater.sqlite, which etl/run_etl.R fills.

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(shinyWidgets)
  library(htmltools)
  library(plotly)
  library(leaflet)
  library(DBI)
  library(RSQLite)
})

source("R/helpers.R")
source("R/components.R")
source("R/data_access.R")
source("etl/constants.R")

#' Open the store read-only.
#'
#' A missing store file must not stop the app: every data_access.R function
#' returns its empty shape when a table is missing, so an empty in-memory
#' database gives the documented "No data" / "Unavailable" degraded mode
#' instead of a crash. The in-memory fallback is also what happens when the
#' read-only open fails for any other reason.
#'
#' @param path Store path (defaults to resolve_db_path()).
#' @return An open DBI connection.
open_store <- function(path = resolve_db_path()) {
  if (!file.exists(path)) {
    message("Store not found at ", path, "; starting with an empty store. Run etl/run_etl.R.")
    return(DBI::dbConnect(RSQLite::SQLite(), ":memory:"))
  }
  tryCatch(
    DBI::dbConnect(RSQLite::SQLite(), path, flags = RSQLite::SQLITE_RO),
    error = function(e) {
      message("Could not open ", path, " read-only (", conditionMessage(e), "); starting with an empty store.")
      DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    }
  )
}

con <- open_store()
manual <- load_manual_content()

# Close the store when the app stops. Outside a running app there is no stop
# event to register against, so a failure here is ignored.
tryCatch(
  shiny::onStop(function() {
    store <- get0("con", envir = globalenv(), inherits = FALSE)
    if (inherits(store, "DBIConnection") && DBI::dbIsValid(store)) {
      DBI::dbDisconnect(store)
    }
  }),
  error = function(e) NULL
)
