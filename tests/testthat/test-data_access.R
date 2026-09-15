# tests/testthat/test-data_access.R
#
# Gate 1 (unit). Every function in R/data_access.R against:
#   1. build_fixture_db()  -- exact seeded values (documented in helper-db.R)
#   2. build_empty_db()    -- documented empty shapes, no errors
#   3. schema drift        -- the function's table dropped (or recreated
#                             without its value column) -> empty shape
#   4. a closed connection / NULL connection -> empty shape
# Hostile-input coverage lives in test-security-sql_injection.R.
#
# R/data_access.R is sourced by helper-load.R; constants (SITE_PARKER, ...)
# and build_*_db() come from helper-db.R. Expected empty shapes are written
# out literally here, independent of the implementation's own templates.

EMPTY_SITES <- data.frame(
  site_no = character(0), kind = character(0), parameter_code = character(0),
  name = character(0), watershed = character(0), lat = numeric(0), lon = numeric(0),
  stringsAsFactors = FALSE
)
EMPTY_LATEST <- list(value = NA_real_, datetime = NA_character_)
EMPTY_INST <- data.frame(datetime = as.POSIXct(character(0), tz = "UTC"), value = numeric(0))
EMPTY_DAILY <- data.frame(date = as.Date(character(0)), value = numeric(0))
EMPTY_PCT <- data.frame(
  month_nu = integer(0), day_nu = integer(0),
  p10 = numeric(0), p25 = numeric(0), p50 = numeric(0), p75 = numeric(0), p90 = numeric(0),
  years_used = integer(0)
)
EMPTY_WINDOW <- list(total_in = NA_real_, start_date = as.Date(NA), end_date = as.Date(NA), days_with_data = 0L)
EMPTY_TYPICAL <- list(typical_in = NA_real_, years_used = 0L)
EMPTY_LOW_FLOW <- data.frame(yr = integer(0), threshold = numeric(0), days = integer(0))
EMPTY_DROUGHT <- data.frame(
  fips = character(0), map_date = as.Date(character(0)),
  d0 = numeric(0), d1 = numeric(0), d2 = numeric(0), d3 = numeric(0), d4 = numeric(0),
  stringsAsFactors = FALSE
)
EMPTY_STATUS <- data.frame(
  source = character(0), last_success = character(0),
  last_status = character(0), last_error = character(0),
  stringsAsFactors = FALSE
)

TODAY <- Sys.Date()
M_TODAY <- as.integer(format(TODAY, "%m"))
D_TODAY <- as.integer(format(TODAY, "%d"))
TODAY_IS_JUNE_15 <- M_TODAY == 6L && D_TODAY == 15L

# Fixture DB with one table dropped (schema drift).
fixture_without <- function(table) {
  con <- build_fixture_db()
  DBI::dbRemoveTable(con, table)
  con
}

closed_con <- function() {
  con <- build_fixture_db()
  DBI::dbDisconnect(con)
  con
}

# Run `f(con)` against the empty DB, the fixture DB minus each table in
# `tables`, a closed connection and NULL; every result must be `expected`.
expect_degrades_to <- function(f, expected, tables) {
  con <- build_empty_db()
  expect_identical(f(con), expected, info = "empty store")
  DBI::dbDisconnect(con)

  for (tbl in tables) {
    con <- fixture_without(tbl)
    expect_identical(f(con), expected, info = paste("dropped table", tbl))
    DBI::dbDisconnect(con)
  }

  expect_identical(f(closed_con()), expected, info = "closed connection")
  expect_identical(f(NULL), expected, info = "NULL connection")
}

seed_precip <- function(con, dates, values, site = PRECIP_SITE) {
  for (i in seq_along(dates)) {
    DBI::dbExecute(
      con,
      "INSERT OR REPLACE INTO precip_daily (site_no, date, precip_in, approval_status, qualifier) VALUES (?, ?, ?, ?, ?)", # nolint: line_length_linter.
      params = list(site, format(as.Date(dates[i]), "%Y-%m-%d"), values[i], "Approved", NA_character_)
    )
  }
}

# A complete `days`-day window ending on `end` with a constant daily value.
seed_window <- function(con, end, value, days = 7) {
  end <- as.Date(end)
  seed_precip(con, seq(end - (days - 1), end, by = "day"), rep(value, days))
}

# ---- get_sites -------------------------------------------------------------------

test_that("get_sites returns every seeded site with contract columns and types", {
  con <- build_fixture_db()
  on.exit(DBI::dbDisconnect(con))

  res <- get_sites(con)
  expect_s3_class(res, "data.frame")
  expect_identical(names(res), names(EMPTY_SITES))
  expect_identical(vapply(res, class, character(1)), vapply(EMPTY_SITES, class, character(1)))
  expect_identical(res$site_no, c(SITE_PARKER, SITE_IPSWICH, GW_SITE, PRECIP_SITE))
  expect_identical(res$kind, c("flow", "flow", "groundwater", "precip"))

  parker <- res[res$site_no == SITE_PARKER, ]
  expect_identical(parker$name, "Parker River at Byfield, MA")
  expect_identical(parker$watershed, "Parker River")
  expect_identical(parker$parameter_code, "00060")
  expect_equal(parker$lat, 42.752869)
  expect_equal(parker$lon, -70.945610)
})

test_that("get_sites filters by kind", {
  con <- build_fixture_db()
  on.exit(DBI::dbDisconnect(con))

  expect_identical(get_sites(con, "flow")$site_no, c(SITE_PARKER, SITE_IPSWICH))
  expect_identical(get_sites(con, "groundwater")$site_no, GW_SITE)
  expect_identical(get_sites(con, "precip")$parameter_code, "00045")
  expect_identical(get_sites(con, "lake"), EMPTY_SITES)
  expect_identical(get_sites(con, NA_character_), EMPTY_SITES)
  expect_identical(get_sites(con, c("flow", "precip")), EMPTY_SITES)
})

test_that("get_sites degrades to the empty shape", {
  expect_degrades_to(function(con) get_sites(con), EMPTY_SITES, "sites")
})

# ---- get_latest_reading ----------------------------------------------------------

test_that("get_latest_reading returns the newest value and its UTC timestamp per kind", {
  con <- build_fixture_db()
  on.exit(DBI::dbDisconnect(con))

  flow <- get_latest_reading(con, "flow", SITE_PARKER)
  expect_identical(names(flow), c("value", "datetime"))
  expect_identical(flow$value, 12.5)
  expect_type(flow$datetime, "character")
  expect_match(flow$datetime, "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$")
  age_h <- as.numeric(difftime(Sys.time(), as.POSIXct(flow$datetime, tz = "UTC"), units = "hours"))
  expect_gt(age_h, 0.9)
  expect_lt(age_h, 1.5)

  expect_identical(get_latest_reading(con, "flow", SITE_IPSWICH)$value, 30.0)
  expect_identical(get_latest_reading(con, "groundwater", GW_SITE)$value, 10.33)
  expect_identical(get_latest_reading(con, "precip", PRECIP_SITE)$value, 0.10)
  # Default kind is "flow".
  expect_identical(get_latest_reading(con, site_no = SITE_PARKER)$value, 12.5)
})

test_that("get_latest_reading skips a newer row whose value is missing", {
  con <- build_fixture_db()
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(
    con,
    "INSERT INTO flow_instantaneous (site_no, datetime, discharge_cfs) VALUES (?, ?, NULL)",
    params = list(SITE_PARKER, format(Sys.time() + 60, "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  )
  expect_identical(get_latest_reading(con, "flow", SITE_PARKER)$value, 12.5)
})

test_that("get_latest_reading returns the empty shape for unknown sites and invalid kinds", {
  con <- build_fixture_db()
  on.exit(DBI::dbDisconnect(con))

  expect_identical(get_latest_reading(con, "flow", "USGS-00000000"), EMPTY_LATEST)
  expect_identical(get_latest_reading(con, "groundwater", SITE_PARKER), EMPTY_LATEST)
  expect_identical(get_latest_reading(con, "lake", SITE_PARKER), EMPTY_LATEST)
  expect_identical(get_latest_reading(con, NA_character_, SITE_PARKER), EMPTY_LATEST)
  expect_identical(get_latest_reading(con, NULL, SITE_PARKER), EMPTY_LATEST)
  expect_identical(get_latest_reading(con, c("flow", "precip"), SITE_PARKER), EMPTY_LATEST)
  expect_identical(get_latest_reading(con, "flow", NA_character_), EMPTY_LATEST)
  expect_identical(get_latest_reading(con, "flow"), EMPTY_LATEST)
})

test_that("get_latest_reading degrades to the empty shape", {
  expect_degrades_to(
    function(con) get_latest_reading(con, "flow", SITE_PARKER), EMPTY_LATEST, "flow_instantaneous"
  )
  expect_degrades_to(
    function(con) get_latest_reading(con, "groundwater", GW_SITE), EMPTY_LATEST, "groundwater_instantaneous"
  )
  expect_degrades_to(
    function(con) get_latest_reading(con, "precip", PRECIP_SITE), EMPTY_LATEST, "precip_instantaneous"
  )
})

test_that("get_latest_reading degrades when the value column is missing (column drift)", {
  con <- build_fixture_db()
  on.exit(DBI::dbDisconnect(con))
  DBI::dbRemoveTable(con, "flow_instantaneous")
  DBI::dbExecute(con, "CREATE TABLE flow_instantaneous (site_no TEXT, datetime TEXT)")
  DBI::dbExecute(
    con, "INSERT INTO flow_instantaneous VALUES (?, ?)",
    params = list(SITE_PARKER, "2026-01-01 00:00:00")
  )
  expect_identical(get_latest_reading(con, "flow", SITE_PARKER), EMPTY_LATEST)
  expect_identical(get_instantaneous_series(con, "flow", SITE_PARKER, 10000), EMPTY_INST)
})

# ---- get_instantaneous_series ----------------------------------------------------

test_that("get_instantaneous_series returns POSIXct UTC rows inside the window, ascending", {
  con <- build_fixture_db()
  on.exit(DBI::dbDisconnect(con))

  res <- get_instantaneous_series(con, "flow", SITE_PARKER, days = 30)
  expect_identical(names(res), c("datetime", "value"))
  expect_s3_class(res$datetime, "POSIXct")
  expect_identical(attr(res$datetime, "tzone"), "UTC")
  expect_identical(res$value, c(15.0, 12.5))
  expect_equal(as.numeric(difftime(res$datetime[2], res$datetime[1], units = "secs")), 86400)
  expect_identical(rownames(res), c("1", "2"))

  expect_identical(get_instantaneous_series(con, "flow", SITE_PARKER, days = 500)$value, c(99.0, 15.0, 12.5))
  expect_identical(get_instantaneous_series(con, "flow", SITE_IPSWICH, days = 1)$value, 30.0)
  expect_identical(get_instantaneous_series(con, "groundwater", GW_SITE, days = 1)$value, c(10.30, 10.33))
  expect_identical(get_instantaneous_series(con, "precip", PRECIP_SITE, days = 1)$value, c(0.25, 0.10))
})

test_that("get_instantaneous_series honours `now` as the upper bound", {
  con <- build_fixture_db()
  on.exit(DBI::dbDisconnect(con))

  # Window [now - 36 h, now - 12 h] contains only the reading at now - 25 h.
  res <- get_instantaneous_series(con, "flow", SITE_PARKER, days = 1, now = Sys.time() - 12 * 3600)
  expect_identical(res$value, 15.0)

  # A `now` in another time zone is compared in UTC.
  eastern <- as.POSIXct(format(Sys.time(), tz = "America/New_York"), tz = "America/New_York")
  expect_identical(get_instantaneous_series(con, "flow", SITE_PARKER, days = 2, now = eastern + 1)$value, c(15.0, 12.5))
})

test_that("get_instantaneous_series rejects invalid arguments with the empty shape", {
  con <- build_fixture_db()
  on.exit(DBI::dbDisconnect(con))

  expect_identical(get_instantaneous_series(con, "flow", SITE_PARKER, days = -1), EMPTY_INST)
  expect_identical(get_instantaneous_series(con, "flow", SITE_PARKER, days = NA_real_), EMPTY_INST)
  expect_identical(get_instantaneous_series(con, "flow", SITE_PARKER, days = "7"), EMPTY_INST)
  expect_identical(get_instantaneous_series(con, "flow", SITE_PARKER, days = 7, now = "today"), EMPTY_INST)
  expect_identical(get_instantaneous_series(con, "lake", SITE_PARKER, days = 7), EMPTY_INST)
  expect_identical(get_instantaneous_series(con, site_no = SITE_PARKER, days = 7), EMPTY_INST)
  expect_identical(get_instantaneous_series(con, "flow", "USGS-00000000", days = 7), EMPTY_INST)
})

test_that("get_instantaneous_series degrades to the empty shape", {
  expect_degrades_to(
    function(con) get_instantaneous_series(con, "flow", SITE_PARKER, 30), EMPTY_INST, "flow_instantaneous"
  )
  expect_degrades_to(
    function(con) get_instantaneous_series(con, "precip", PRECIP_SITE, 30), EMPTY_INST, "precip_instantaneous"
  )
})

# ---- get_daily_series ------------------------------------------------------------

test_that("get_daily_series returns Date rows between inclusive bounds, ascending", {
  con <- build_fixture_db()
  on.exit(DBI::dbDisconnect(con))

  res <- get_daily_series(con, "flow", SITE_PARKER, as.Date("2022-07-01"), as.Date("2023-12-31"))
  expect_identical(names(res), c("date", "value"))
  expect_s3_class(res$date, "Date")
  expect_identical(
    format(res$date),
    c("2022-07-01", "2022-07-02", "2022-07-03", "2023-07-01", "2023-07-02", "2023-07-03")
  )
  expect_identical(res$value, c(0.005, 0.5, 5.0, 0.5, 0.5, 5.0))

  # Character dates work and both ends are inclusive.
  expect_identical(get_daily_series(con, "flow", SITE_PARKER, "2022-07-02", "2022-07-03")$value, c(0.5, 5.0))

  recent <- get_daily_series(con, "flow", SITE_PARKER, TODAY - 3, TODAY - 1)
  expect_identical(recent$date, TODAY - 3:1)
  expect_identical(recent$value, c(12.0, 11.0, 10.0))

  expect_identical(get_daily_series(con, "groundwater", GW_SITE, TODAY - 2, TODAY)$value, c(10.1, 10.2))
  precip <- get_daily_series(con, "precip", PRECIP_SITE, TODAY - 8, TODAY - 1)
  expect_identical(nrow(precip), 8L)
  expect_equal(sum(precip$value), 3.75)
})

test_that("get_daily_series rejects invalid ranges with the empty shape", {
  con <- build_fixture_db()
  on.exit(DBI::dbDisconnect(con))

  expect_identical(get_daily_series(con, "flow", SITE_PARKER, "2023-12-31", "2022-01-01"), EMPTY_DAILY)
  expect_identical(get_daily_series(con, "flow", SITE_PARKER, "not a date", "2023-12-31"), EMPTY_DAILY)
  expect_identical(get_daily_series(con, "flow", SITE_PARKER, NA, "2023-12-31"), EMPTY_DAILY)
  expect_identical(get_daily_series(con, "flow", SITE_PARKER, "2022-01-01", "2022-06-30"), EMPTY_DAILY)
  expect_identical(get_daily_series(con, "lake", SITE_PARKER, "2022-01-01", "2023-12-31"), EMPTY_DAILY)
})

test_that("get_daily_series degrades to the empty shape", {
  for (k in c("flow", "groundwater", "precip")) {
    expect_degrades_to(
      function(con) get_daily_series(con, k, SITE_PARKER, "1990-01-01", "2100-01-01"),
      EMPTY_DAILY, paste0(k, "_daily")
    )
  }
})

# ---- get_percentiles -------------------------------------------------------------

test_that("get_percentiles returns all stored month-days with contract types", {
  con <- build_fixture_db()
  on.exit(DBI::dbDisconnect(con))

  res <- get_percentiles(con, "flow", SITE_PARKER)
  expect_identical(names(res), names(EMPTY_PCT))
  expect_identical(vapply(res, class, character(1)), vapply(EMPTY_PCT, class, character(1)))
  expect_identical(nrow(res), if (TODAY_IS_JUNE_15) 1L else 2L)
  expect_false(is.unsorted(res$month_nu * 100L + res$day_nu))

  today_row <- get_percentiles(con, "flow", SITE_PARKER, month_nu = M_TODAY, day_nu = D_TODAY)
  expect_identical(nrow(today_row), 1L)
  expect_identical(unlist(today_row[, c("p10", "p25", "p50", "p75", "p90")], use.names = FALSE), c(4, 6, 9, 14, 22))
  expect_identical(today_row$years_used, 25L)

  # Default kind is "flow".
  expect_identical(get_percentiles(con, site_no = SITE_PARKER), res)

  gw <- get_percentiles(con, "groundwater", GW_SITE, M_TODAY, D_TODAY)
  expect_identical(unlist(gw[, c("p10", "p25", "p50", "p75", "p90")], use.names = FALSE), c(6, 7, 8, 9, 10))
  expect_identical(gw$years_used, 40L)
})

test_that("get_percentiles filters on month only and on the June 15 row", {
  skip_if(TODAY_IS_JUNE_15, "fixture omits the 6/15 row when today is June 15")
  con <- build_fixture_db()
  on.exit(DBI::dbDisconnect(con))

  jun15 <- get_percentiles(con, "flow", SITE_PARKER, month_nu = 6, day_nu = 15L)
  expect_identical(jun15$p50, 5)
  expect_identical(jun15$years_used, 30L)

  june <- get_percentiles(con, "flow", SITE_PARKER, month_nu = 6)
  expect_true(15L %in% june$day_nu)
  expect_true(all(june$month_nu == 6L))
})

test_that("get_percentiles rejects precip and malformed filters (never widens to all rows)", {
  con <- build_fixture_db()
  on.exit(DBI::dbDisconnect(con))

  expect_identical(get_percentiles(con, "precip", PRECIP_SITE), EMPTY_PCT)
  expect_identical(get_percentiles(con, "flow", SITE_PARKER, month_nu = 13), EMPTY_PCT)
  expect_identical(get_percentiles(con, "flow", SITE_PARKER, month_nu = "6"), EMPTY_PCT)
  expect_identical(get_percentiles(con, "flow", SITE_PARKER, month_nu = NA), EMPTY_PCT)
  expect_identical(get_percentiles(con, "flow", SITE_PARKER, month_nu = c(6, 7)), EMPTY_PCT)
  expect_identical(get_percentiles(con, "flow", SITE_PARKER, day_nu = 1.5), EMPTY_PCT)
  expect_identical(get_percentiles(con, "flow", "USGS-00000000"), EMPTY_PCT)
})

test_that("get_percentiles degrades to the empty shape", {
  expect_degrades_to(function(con) get_percentiles(con, "flow", SITE_PARKER), EMPTY_PCT, "flow_percentiles")
  expect_degrades_to(
    function(con) get_percentiles(con, "groundwater", GW_SITE), EMPTY_PCT, "groundwater_percentiles"
  )
})

# ---- get_precip_window_total -----------------------------------------------------

test_that("get_precip_window_total defaults to the 7 days ending on the latest stored date", {
  con <- build_fixture_db()
  on.exit(DBI::dbDisconnect(con))

  res <- get_precip_window_total(con, PRECIP_SITE)
  expect_identical(names(res), names(EMPTY_WINDOW))
  expect_equal(res$total_in, 1.75)
  expect_identical(res$start_date, TODAY - 7)
  expect_identical(res$end_date, TODAY - 1)
  expect_identical(res$days_with_data, 7L)
})

test_that("get_precip_window_total honours explicit end_date and days", {
  con <- build_fixture_db()
  on.exit(DBI::dbDisconnect(con))

  shifted <- get_precip_window_total(con, PRECIP_SITE, days = 7, end_date = TODAY - 2)
  expect_equal(shifted$total_in, 3.75)
  expect_identical(shifted$start_date, TODAY - 8)

  expect_equal(get_precip_window_total(con, PRECIP_SITE, days = 1, end_date = format(TODAY - 7))$total_in, 1.00)

  month <- get_precip_window_total(con, PRECIP_SITE, days = 30)
  expect_equal(month$total_in, 3.75)
  expect_identical(month$days_with_data, 8L)

  # A known window with no data: dates filled, no total.
  future <- get_precip_window_total(con, PRECIP_SITE, days = 7, end_date = TODAY + 30)
  expect_identical(future$total_in, NA_real_)
  expect_identical(future$days_with_data, 0L)
  expect_identical(future$end_date, TODAY + 30)
})

test_that("get_precip_window_total returns the empty shape for unknown sites and invalid days", {
  con <- build_fixture_db()
  on.exit(DBI::dbDisconnect(con))

  expect_identical(get_precip_window_total(con, "USGS-00000000"), EMPTY_WINDOW)
  expect_identical(get_precip_window_total(con, PRECIP_SITE, days = 0), EMPTY_WINDOW)
  expect_identical(get_precip_window_total(con, PRECIP_SITE, days = 2.5), EMPTY_WINDOW)
  expect_identical(get_precip_window_total(con, PRECIP_SITE, days = "7"), EMPTY_WINDOW)
  expect_identical(get_precip_window_total(con, PRECIP_SITE, end_date = "garbage"), EMPTY_WINDOW)
})

test_that("get_precip_window_total degrades to the empty shape", {
  expect_degrades_to(function(con) get_precip_window_total(con, PRECIP_SITE), EMPTY_WINDOW, "precip_daily")
})

# ---- get_precip_typical_window ---------------------------------------------------

test_that("get_precip_typical_window is NA with the short fixture record", {
  con <- build_fixture_db()
  on.exit(DBI::dbDisconnect(con))

  expect_identical(get_precip_typical_window(con, PRECIP_SITE, end_date = TODAY - 1), EMPTY_TYPICAL)
})

test_that("get_precip_typical_window is NA when years_used < min_years", {
  con <- build_fixture_db()
  on.exit(DBI::dbDisconnect(con))
  seed_window(con, "2019-07-10", 0.1)
  seed_window(con, "2020-07-10", 0.2)

  res <- get_precip_typical_window(con, PRECIP_SITE, days = 7, end_date = "2024-07-10", min_years = 3)
  expect_identical(res, list(typical_in = NA_real_, years_used = 2L))

  # The same two years are enough when the caller lowers the bar.
  expect_equal(
    get_precip_typical_window(con, PRECIP_SITE, days = 7, end_date = "2024-07-10", min_years = 2)$typical_in,
    1.05
  )
})

test_that("get_precip_typical_window takes the median of complete prior-year windows only", {
  con <- build_fixture_db()
  on.exit(DBI::dbDisconnect(con))
  seed_window(con, "2019-07-10", 0.1) # 0.7
  seed_window(con, "2020-07-10", 0.2) # 1.4
  seed_window(con, "2021-07-10", 0.5) # 3.5
  # 2018: only 6 of 7 days -> incomplete, excluded.
  seed_precip(con, seq(as.Date("2018-07-05"), as.Date("2018-07-10"), by = "day"), rep(1, 6))
  # 2017: complete dates, but one stored value is NULL -> incomplete.
  seed_window(con, "2017-07-10", 1)
  DBI::dbExecute(
    con, "UPDATE precip_daily SET precip_in = NULL WHERE site_no = ? AND date = ?",
    params = list(PRECIP_SITE, "2017-07-07")
  )
  # The current year's own window must not count toward "typical".
  seed_window(con, "2024-07-10", 9)

  res <- get_precip_typical_window(con, PRECIP_SITE, days = 7, end_date = as.Date("2024-07-10"))
  expect_identical(res$years_used, 3L)
  expect_equal(res$typical_in, 1.4)

  seed_window(con, "2022-07-10", 1) # 7.0 -> median of 0.7, 1.4, 3.5, 7.0
  res4 <- get_precip_typical_window(con, PRECIP_SITE, days = 7, end_date = "2024-07-10")
  expect_identical(res4$years_used, 4L)
  expect_equal(res4$typical_in, 2.45)
})

test_that("get_precip_typical_window handles year-crossing windows and Feb 29", {
  con <- build_fixture_db()
  on.exit(DBI::dbDisconnect(con))

  # 7 days ending Jan 3 start on Dec 28 of the previous year.
  seed_window(con, "2021-01-03", 0.1)
  seed_window(con, "2022-01-03", 0.2)
  seed_window(con, "2023-01-03", 0.3)
  jan <- get_precip_typical_window(con, PRECIP_SITE, days = 7, end_date = "2024-01-03")
  expect_identical(jan$years_used, 3L)
  expect_equal(jan$typical_in, 1.4)

  # Feb 29 (leap year) compares against windows ending Feb 28 in 2021-2023.
  seed_window(con, "2021-02-28", 0.1)
  seed_window(con, "2022-02-28", 0.2)
  seed_window(con, "2023-02-28", 0.3)
  leap <- get_precip_typical_window(con, PRECIP_SITE, days = 7, end_date = "2024-02-29")
  expect_identical(leap$years_used, 3L)
  expect_equal(leap$typical_in, 1.4)
})

test_that("get_precip_typical_window returns the empty shape on invalid input", {
  con <- build_fixture_db()
  on.exit(DBI::dbDisconnect(con))

  expect_identical(get_precip_typical_window(con, PRECIP_SITE), EMPTY_TYPICAL)
  expect_identical(get_precip_typical_window(con, PRECIP_SITE, end_date = "garbage"), EMPTY_TYPICAL)
  expect_identical(get_precip_typical_window(con, PRECIP_SITE, days = 0, end_date = TODAY), EMPTY_TYPICAL)
  expect_identical(get_precip_typical_window(con, PRECIP_SITE, end_date = TODAY, min_years = NA), EMPTY_TYPICAL)
  expect_identical(get_precip_typical_window(con, "USGS-00000000", end_date = NULL), EMPTY_TYPICAL)
})

test_that("get_precip_typical_window degrades to the empty shape", {
  expect_degrades_to(
    function(con) get_precip_typical_window(con, PRECIP_SITE, end_date = TODAY - 1), EMPTY_TYPICAL, "precip_daily"
  )
})

# ---- get_low_flow_days -----------------------------------------------------------

test_that("get_low_flow_days counts days below each threshold per year", {
  con <- build_fixture_db()
  on.exit(DBI::dbDisconnect(con))

  res <- get_low_flow_days(con, SITE_PARKER, years = 2022:2023)
  expected <- data.frame(
    yr = rep(c(2022L, 2023L), 3),
    threshold = rep(c(1, 0.1, 0.01), each = 2),
    days = c(2L, 2L, 1L, 0L, 1L, 0L)
  )
  expect_identical(res, expected)
})

test_that("get_low_flow_days zero-fills requested years inside a site with data", {
  con <- build_fixture_db()
  on.exit(DBI::dbDisconnect(con))

  res <- get_low_flow_days(con, SITE_PARKER, years = c(2023, 2021, 2022), thresholds = 6)
  expect_identical(res$yr, c(2021L, 2022L, 2023L))
  expect_identical(res$days, c(0L, 3L, 3L))

  # Current-year rows (10, 11, 12 cfs) are never below 1 cfs.
  this_year <- as.integer(format(TODAY - 1, "%Y"))
  expect_identical(get_low_flow_days(con, SITE_PARKER, years = this_year, thresholds = 1)$days, 0L)
})

test_that("get_low_flow_days returns the empty shape without data or with invalid input", {
  con <- build_fixture_db()
  on.exit(DBI::dbDisconnect(con))

  expect_identical(get_low_flow_days(con, SITE_PARKER, years = 2010), EMPTY_LOW_FLOW)
  expect_identical(get_low_flow_days(con, "USGS-00000000", years = 2022:2023), EMPTY_LOW_FLOW)
  expect_identical(get_low_flow_days(con, SITE_PARKER, years = "2022"), EMPTY_LOW_FLOW)
  expect_identical(get_low_flow_days(con, SITE_PARKER, years = c(2022, NA)), EMPTY_LOW_FLOW)
  expect_identical(get_low_flow_days(con, SITE_PARKER, years = 2022.5), EMPTY_LOW_FLOW)
  expect_identical(get_low_flow_days(con, SITE_PARKER, years = integer(0)), EMPTY_LOW_FLOW)
  expect_identical(get_low_flow_days(con, SITE_PARKER, years = 2022, thresholds = NA_real_), EMPTY_LOW_FLOW)
})

test_that("get_low_flow_days degrades to the empty shape", {
  expect_degrades_to(function(con) get_low_flow_days(con, SITE_PARKER, 2022:2023), EMPTY_LOW_FLOW, "flow_daily")
})

# ---- get_drought_status ----------------------------------------------------------

test_that("get_drought_status returns weekly rows ascending with Date map_date", {
  con <- build_fixture_db()
  on.exit(DBI::dbDisconnect(con))

  res <- get_drought_status(con, ESSEX_COUNTY_FIPS)
  expect_identical(names(res), names(EMPTY_DROUGHT))
  expect_identical(vapply(res, class, character(1)), vapply(EMPTY_DROUGHT, class, character(1)))
  expect_identical(res$map_date, as.Date(c("2024-01-02", "2024-01-09")))
  expect_identical(res$fips, c("25009", "25009"))
  expect_identical(res$d0, c(50, 60))
  expect_identical(res$d2, c(0, 10))
  # Contract with R/helpers.R: the result feeds summarize_drought() directly.
  expect_identical(summarize_drought(res), "D2 active")
  expect_identical(summarize_drought(EMPTY_DROUGHT), "Unavailable")
})

test_that("get_drought_status returns the empty shape for unknown or invalid fips", {
  con <- build_fixture_db()
  on.exit(DBI::dbDisconnect(con))

  expect_identical(get_drought_status(con, "99999"), EMPTY_DROUGHT)
  expect_identical(get_drought_status(con, 25009), EMPTY_DROUGHT)
  expect_identical(get_drought_status(con, NA_character_), EMPTY_DROUGHT)
})

test_that("get_drought_status degrades to the empty shape", {
  expect_degrades_to(function(con) get_drought_status(con, ESSEX_COUNTY_FIPS), EMPTY_DROUGHT, "drought_status")
})

# ---- get_last_updated ------------------------------------------------------------

test_that("get_last_updated returns the latest successful finish for a source prefix", {
  con <- build_fixture_db()
  on.exit(DBI::dbDisconnect(con))

  # The later 2024-01-10 failure must not replace the last success.
  expect_identical(get_last_updated(con, "flow_latest:"), "2024-01-09 08:00:05")
  expect_identical(get_last_updated(con, paste0("flow_latest:", SITE_PARKER)), "2024-01-09 08:00:05")
  expect_identical(get_last_updated(con, "flow_daily:"), "2024-01-09 08:01:05")
  expect_identical(get_last_updated(con, "flow"), "2024-01-09 08:01:05")
  expect_identical(get_last_updated(con, "precip"), "2024-01-09 08:01:30")
  expect_identical(get_last_updated(con, "drought:25009"), "2024-01-09 08:02:05")
  # The result feeds format_last_updated() directly.
  expect_match(format_last_updated(get_last_updated(con, "drought:")), "^Updated Jan 9, 2024")
})

test_that("get_last_updated returns NA_character_ when nothing matches", {
  con <- build_fixture_db()
  on.exit(DBI::dbDisconnect(con))

  expect_identical(get_last_updated(con, "groundwater"), NA_character_)
  expect_identical(get_last_updated(con, "FLOW_LATEST:"), NA_character_)
  expect_identical(get_last_updated(con, "latest"), NA_character_)
  expect_identical(get_last_updated(con, ""), NA_character_)
  expect_identical(get_last_updated(con, NA_character_), NA_character_)
  expect_identical(get_last_updated(con, c("flow", "drought")), NA_character_)
  expect_identical(format_last_updated(NA_character_), "Not yet updated")
})

test_that("get_last_updated degrades to NA_character_", {
  expect_degrades_to(function(con) get_last_updated(con, "flow"), NA_character_, "etl_runs")
})

# ---- get_source_status -----------------------------------------------------------

test_that("get_source_status summarises one row per source", {
  con <- build_fixture_db()
  on.exit(DBI::dbDisconnect(con))

  res <- get_source_status(con)
  expect_identical(names(res), names(EMPTY_STATUS))
  expect_identical(vapply(res, class, character(1)), vapply(EMPTY_STATUS, class, character(1)))
  expect_identical(res$source, c(
    "drought:25009", paste0("flow_daily:", SITE_PARKER),
    paste0("flow_latest:", SITE_PARKER), paste0("precip_daily:", PRECIP_SITE)
  ))

  latest <- res[res$source == paste0("flow_latest:", SITE_PARKER), ]
  expect_identical(latest$last_success, "2024-01-09 08:00:05")
  expect_identical(latest$last_status, "failure")
  expect_identical(latest$last_error, "timeout")

  drought <- res[res$source == "drought:25009", ]
  expect_identical(drought$last_success, "2024-01-09 08:02:05")
  expect_identical(drought$last_status, "success")
  expect_identical(drought$last_error, NA_character_)
})

test_that("get_source_status reports a never-successful source with NA last_success", {
  con <- build_fixture_db()
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(
    con,
    "INSERT INTO etl_runs (source, started_at, finished_at, status, rows_written, error_message) VALUES (?, ?, ?, ?, ?, ?)", # nolint: line_length_linter.
    params = list(paste0("groundwater_daily:", GW_SITE), "2024-01-09 09:00:00", "2024-01-09 09:00:30", "failure", 0L, "HTTP 503") # nolint: line_length_linter.
  )

  res <- get_source_status(con)
  gw <- res[res$source == paste0("groundwater_daily:", GW_SITE), ]
  expect_identical(nrow(res), 5L)
  expect_identical(gw$last_success, NA_character_)
  expect_identical(gw$last_status, "failure")
  expect_identical(gw$last_error, "HTTP 503")
})

test_that("get_source_status degrades to the empty shape", {
  expect_degrades_to(get_source_status, EMPTY_STATUS, "etl_runs")
})
