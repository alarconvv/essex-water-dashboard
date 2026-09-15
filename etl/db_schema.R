# etl/db_schema.R
#
# SQLite schema for the Essex County Water Dashboard store
# (data/essexwater.sqlite). ensure_schema(con) is idempotent
# (CREATE TABLE IF NOT EXISTS) and safe on every ETL run and app start.
#
# Conventions (shared by the ETL writers and the app's data-access layer):
#   - site_no   full OGC monitoring_location_id, e.g. "USGS-01101000"
#   - datetime  UTC, "YYYY-MM-DD HH:MM:SS" (same text format SQLite's own
#               datetime() returns, so string comparison == time comparison)
#   - date      "YYYY-MM-DD" (the USGS daily-value date, local station day)
#   - approval_status "Provisional" / "Approved" as reported by USGS
#   - qualifier comma-joined USGS qualifier codes, or NULL
#   - percentile / typical tables hold 366 rows per site (month_nu, day_nu,
#     including Feb 29); a row with years_used = 0 has NULL statistics.
#
# Requires etl/constants.R (SITES) for seed_sites().

#' Create every table in the store if it does not already exist.
#'
#' @param con A DBI connection.
#' @return invisible(TRUE)
ensure_schema <- function(con) {
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS sites (
      site_no TEXT PRIMARY KEY,
      kind TEXT NOT NULL CHECK (kind IN ('flow', 'groundwater', 'precip')),
      parameter_code TEXT,
      name TEXT,
      watershed TEXT,
      lat REAL,
      lon REAL
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS flow_instantaneous (
      site_no TEXT NOT NULL,
      datetime TEXT NOT NULL,
      discharge_cfs REAL,
      approval_status TEXT,
      qualifier TEXT,
      PRIMARY KEY (site_no, datetime)
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS flow_daily (
      site_no TEXT NOT NULL,
      date TEXT NOT NULL,
      discharge_cfs REAL,
      approval_status TEXT,
      qualifier TEXT,
      PRIMARY KEY (site_no, date)
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS flow_percentiles (
      site_no TEXT NOT NULL,
      month_nu INTEGER NOT NULL,
      day_nu INTEGER NOT NULL,
      p10 REAL,
      p25 REAL,
      p50 REAL,
      p75 REAL,
      p90 REAL,
      years_used INTEGER NOT NULL DEFAULT 0,
      PRIMARY KEY (site_no, month_nu, day_nu)
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS groundwater_instantaneous (
      site_no TEXT NOT NULL,
      datetime TEXT NOT NULL,
      depth_ft REAL,
      approval_status TEXT,
      qualifier TEXT,
      PRIMARY KEY (site_no, datetime)
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS groundwater_daily (
      site_no TEXT NOT NULL,
      date TEXT NOT NULL,
      depth_ft REAL,
      approval_status TEXT,
      qualifier TEXT,
      PRIMARY KEY (site_no, date)
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS groundwater_percentiles (
      site_no TEXT NOT NULL,
      month_nu INTEGER NOT NULL,
      day_nu INTEGER NOT NULL,
      p10 REAL,
      p25 REAL,
      p50 REAL,
      p75 REAL,
      p90 REAL,
      years_used INTEGER NOT NULL DEFAULT 0,
      PRIMARY KEY (site_no, month_nu, day_nu)
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS precip_instantaneous (
      site_no TEXT NOT NULL,
      datetime TEXT NOT NULL,
      precip_in REAL,
      approval_status TEXT,
      qualifier TEXT,
      PRIMARY KEY (site_no, datetime)
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS precip_daily (
      site_no TEXT NOT NULL,
      date TEXT NOT NULL,
      precip_in REAL,
      approval_status TEXT,
      qualifier TEXT,
      PRIMARY KEY (site_no, date)
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS precip_typical (
      site_no TEXT NOT NULL,
      month_nu INTEGER NOT NULL,
      day_nu INTEGER NOT NULL,
      median_in REAL,
      years_used INTEGER NOT NULL DEFAULT 0,
      PRIMARY KEY (site_no, month_nu, day_nu)
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS drought_status (
      fips TEXT NOT NULL,
      map_date TEXT NOT NULL,
      d0 REAL,
      d1 REAL,
      d2 REAL,
      d3 REAL,
      d4 REAL,
      PRIMARY KEY (fips, map_date)
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS etl_runs (
      run_id INTEGER PRIMARY KEY AUTOINCREMENT,
      source TEXT NOT NULL,
      started_at TEXT,
      finished_at TEXT,
      status TEXT CHECK (status IN ('success', 'failure')),
      rows_written INTEGER,
      error_message TEXT
    )
  ")

  DBI::dbExecute(
    con,
    "CREATE INDEX IF NOT EXISTS idx_etl_runs_source_status ON etl_runs (source, status, finished_at)"
  )

  invisible(TRUE)
}

#' Seed (or refresh) the sites table from etl/constants.R::SITES.
#'
#' Idempotent: INSERT OR REPLACE keyed on site_no.
#'
#' @param con A DBI connection.
#' @param sites List of site descriptors (defaults to SITES).
#' @return invisible(TRUE)
seed_sites <- function(con, sites = SITES) {
  for (s in sites) {
    DBI::dbExecute(
      con,
      "INSERT OR REPLACE INTO sites (site_no, kind, parameter_code, name, watershed, lat, lon) VALUES (?, ?, ?, ?, ?, ?, ?)", # nolint: line_length_linter.
      params = list(s$site_no, s$kind, s$parameter_code, s$name, s$watershed, s$lat, s$lon)
    )
  }
  invisible(TRUE)
}
