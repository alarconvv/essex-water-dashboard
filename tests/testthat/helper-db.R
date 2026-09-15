# tests/testthat/helper-db.R
#
# Temp SQLite stores for ETL and data-access tests. testthat sources this
# before every test file. Paths use test_path("..", "..") so they resolve to
# the project root regardless of where the suite is launched.
#
#   build_empty_db()   schema only, zero rows anywhere (degraded-mode tests)
#   build_fixture_db() schema + sites + the deterministic rows below
#
# Both return an open DBI connection to a fresh temp file; callers
# disconnect (e.g. on.exit(DBI::dbDisconnect(con))).
#
# ---------------------------------------------------------------------------
# SEEDED VALUES (build_fixture_db) -- later rounds assert against these; keep
# this block in sync with the code below.
#
# `now` = Sys.time(), `today` = Sys.Date() at build time. datetime values
# are UTC "YYYY-MM-DD HH:MM:SS"; dates "YYYY-MM-DD".
#
# sites: all four from etl/constants.R::SITES
#   USGS-01101000 (flow), USGS-01102000 (flow),
#   USGS-424520070562401 (groundwater), USGS-424510070564401 (precip)
#
# flow_instantaneous (FIX_FLOW_SITE = SITE_PARKER):
#   now - 1 h     12.5 cfs  Provisional   <- latest Parker reading
#   now - 25 h    15.0 cfs  Provisional
#   now - 400 d   99.0 cfs  Approved      <- outside any 30-day window
#   SITE_IPSWICH now - 1 h  30.0 cfs  Provisional
#
# flow_daily (Parker):
#   2022-07-01 0.005 | 2022-07-02 0.5 | 2022-07-03 5.0
#   2023-07-01 0.5   | 2023-07-02 0.5 | 2023-07-03 5.0
#     below 1 cfs:   2022 -> 2 days, 2023 -> 2 days
#     below 0.1 cfs: 2022 -> 1 day,  2023 -> 0 days
#     below 0.01:    2022 -> 1 day,  2023 -> 0 days
#   plus today-1 .. today-3 (current year): 10.0, 11.0, 12.0 (today-1 = 10.0)
#
# flow_percentiles (Parker), two rows:
#   (6, 15)                      p10 1, p25 2, p50 5, p75 10, p90 20, years_used 30
#   (month(today), day(today))   p10 4, p25 6, p50 9, p75 14, p90 22, years_used 25
#
# groundwater_instantaneous (GW_SITE): now - 1 h 10.33 ft; now - 2 h 10.30 ft
# groundwater_daily (GW_SITE): today-1 10.2 ft; today-2 10.1 ft
# groundwater_percentiles (GW_SITE) at (month(today), day(today)):
#   p10 6.0, p25 7.0, p50 8.0, p75 9.0, p90 10.0, years_used 40
#   (depth to water: larger = deeper = drier)
#
# precip_instantaneous (PRECIP_SITE): now - 1 h 0.10 in; now - 2 h 0.25 in
# precip_daily (PRECIP_SITE), today-k for k = 1..7:
#   0.00, 0.50, 0.00, 0.25, 0.00, 0.00, 1.00   -> 7-day total 1.75 in
#   plus today-8 = 2.00 (outside a 7-day window ending today-1)
# precip_typical (PRECIP_SITE) at (month(today), day(today)): median_in 0.05, years_used 1
#
# drought_status (ESSEX_COUNTY_FIPS "25009"):
#   2024-01-02 d0 50 d1 20 d2 0  d3 0 d4 0
#   2024-01-09 d0 60 d1 30 d2 10 d3 0 d4 0   <- latest -> "D2 active"
#
# etl_runs (run order = insertion order):
#   flow_latest:USGS-01101000     success rows 3  finished 2024-01-09 08:00:05
#   flow_daily:USGS-01101000      success rows 9  finished 2024-01-09 08:01:05
#   precip_daily:USGS-424510070564401 success rows 8 finished 2024-01-09 08:01:30
#   drought:25009                 success rows 2  finished 2024-01-09 08:02:05
#   flow_latest:USGS-01101000     failure rows 0  finished 2024-01-10 08:00:05 "timeout"
#     (the later failure must not replace the last *successful* update time)
# ---------------------------------------------------------------------------

source(testthat::test_path("..", "..", "etl", "constants.R"))
source(testthat::test_path("..", "..", "etl", "db_schema.R"))

FIX_FLOW_SITE <- SITE_PARKER

build_empty_db <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), tempfile(fileext = ".sqlite"))
  ensure_schema(con)
  con
}

build_fixture_db <- function() {
  con <- build_empty_db()
  seed_sites(con)

  now <- Sys.time()
  today <- Sys.Date()
  ts <- function(t) format(t, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  ds <- function(d) format(d, "%Y-%m-%d")
  m_today <- as.integer(format(today, "%m"))
  d_today <- as.integer(format(today, "%d"))

  ins <- function(sql, ...) DBI::dbExecute(con, sql, params = list(...))

  # flow_instantaneous
  sql_fi <- "INSERT INTO flow_instantaneous (site_no, datetime, discharge_cfs, approval_status, qualifier) VALUES (?, ?, ?, ?, ?)" # nolint: line_length_linter.
  ins(sql_fi, SITE_PARKER, ts(now - 3600), 12.5, "Provisional", NA_character_)
  ins(sql_fi, SITE_PARKER, ts(now - 25 * 3600), 15.0, "Provisional", NA_character_)
  ins(sql_fi, SITE_PARKER, ts(now - 400 * 86400), 99.0, "Approved", NA_character_)
  ins(sql_fi, SITE_IPSWICH, ts(now - 3600), 30.0, "Provisional", NA_character_)

  # flow_daily
  sql_fd <- "INSERT INTO flow_daily (site_no, date, discharge_cfs, approval_status, qualifier) VALUES (?, ?, ?, ?, ?)" # nolint: line_length_linter.
  hist_dates <- c("2022-07-01", "2022-07-02", "2022-07-03", "2023-07-01", "2023-07-02", "2023-07-03")
  hist_vals <- c(0.005, 0.5, 5.0, 0.5, 0.5, 5.0)
  for (i in seq_along(hist_dates)) ins(sql_fd, SITE_PARKER, hist_dates[i], hist_vals[i], "Approved", NA_character_)
  recent_vals <- c(10.0, 11.0, 12.0)
  for (k in 1:3) ins(sql_fd, SITE_PARKER, ds(today - k), recent_vals[k], "Provisional", NA_character_)

  # flow_percentiles
  sql_fp <- "INSERT INTO flow_percentiles (site_no, month_nu, day_nu, p10, p25, p50, p75, p90, years_used) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)" # nolint: line_length_linter.
  if (!(m_today == 6L && d_today == 15L)) {
    ins(sql_fp, SITE_PARKER, 6L, 15L, 1.0, 2.0, 5.0, 10.0, 20.0, 30L)
  }
  ins(sql_fp, SITE_PARKER, m_today, d_today, 4.0, 6.0, 9.0, 14.0, 22.0, 25L)

  # groundwater
  sql_gi <- "INSERT INTO groundwater_instantaneous (site_no, datetime, depth_ft, approval_status, qualifier) VALUES (?, ?, ?, ?, ?)" # nolint: line_length_linter.
  ins(sql_gi, GW_SITE, ts(now - 3600), 10.33, "Provisional", NA_character_)
  ins(sql_gi, GW_SITE, ts(now - 2 * 3600), 10.30, "Provisional", NA_character_)
  sql_gd <- "INSERT INTO groundwater_daily (site_no, date, depth_ft, approval_status, qualifier) VALUES (?, ?, ?, ?, ?)" # nolint: line_length_linter.
  ins(sql_gd, GW_SITE, ds(today - 1), 10.2, "Provisional", NA_character_)
  ins(sql_gd, GW_SITE, ds(today - 2), 10.1, "Provisional", NA_character_)
  sql_gp <- "INSERT INTO groundwater_percentiles (site_no, month_nu, day_nu, p10, p25, p50, p75, p90, years_used) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)" # nolint: line_length_linter.
  ins(sql_gp, GW_SITE, m_today, d_today, 6.0, 7.0, 8.0, 9.0, 10.0, 40L)

  # precipitation
  sql_pi <- "INSERT INTO precip_instantaneous (site_no, datetime, precip_in, approval_status, qualifier) VALUES (?, ?, ?, ?, ?)" # nolint: line_length_linter.
  ins(sql_pi, PRECIP_SITE, ts(now - 3600), 0.10, "Provisional", NA_character_)
  ins(sql_pi, PRECIP_SITE, ts(now - 2 * 3600), 0.25, "Provisional", NA_character_)
  sql_pd <- "INSERT INTO precip_daily (site_no, date, precip_in, approval_status, qualifier) VALUES (?, ?, ?, ?, ?)" # nolint: line_length_linter.
  p_vals <- c(0.00, 0.50, 0.00, 0.25, 0.00, 0.00, 1.00, 2.00)
  for (k in seq_along(p_vals)) ins(sql_pd, PRECIP_SITE, ds(today - k), p_vals[k], "Provisional", NA_character_)
  ins(
    "INSERT INTO precip_typical (site_no, month_nu, day_nu, median_in, years_used) VALUES (?, ?, ?, ?, ?)",
    PRECIP_SITE, m_today, d_today, 0.05, 1L
  )

  # drought_status
  sql_dr <- "INSERT INTO drought_status (fips, map_date, d0, d1, d2, d3, d4) VALUES (?, ?, ?, ?, ?, ?, ?)"
  ins(sql_dr, ESSEX_COUNTY_FIPS, "2024-01-02", 50.0, 20.0, 0.0, 0.0, 0.0)
  ins(sql_dr, ESSEX_COUNTY_FIPS, "2024-01-09", 60.0, 30.0, 10.0, 0.0, 0.0)

  # etl_runs
  sql_er <- "INSERT INTO etl_runs (source, started_at, finished_at, status, rows_written, error_message) VALUES (?, ?, ?, ?, ?, ?)" # nolint: line_length_linter.
  run_row <- function(src, start, end, status, rows, err = NA_character_) {
    ins(sql_er, src, start, end, status, rows, err)
  }
  run_row(paste0("flow_latest:", SITE_PARKER), "2024-01-09 08:00:00", "2024-01-09 08:00:05", "success", 3L)
  run_row(paste0("flow_daily:", SITE_PARKER), "2024-01-09 08:01:00", "2024-01-09 08:01:05", "success", 9L)
  run_row(paste0("precip_daily:", PRECIP_SITE), "2024-01-09 08:01:10", "2024-01-09 08:01:30", "success", 8L)
  run_row(paste0("drought:", ESSEX_COUNTY_FIPS), "2024-01-09 08:02:00", "2024-01-09 08:02:05", "success", 2L)
  run_row(paste0("flow_latest:", SITE_PARKER), "2024-01-10 08:00:00", "2024-01-10 08:00:05", "failure", 0L, "timeout")

  con
}
