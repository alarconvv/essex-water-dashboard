# tests/testthat/test-etl-write_store.R
#
# Parameterized delete-then-insert upserts (etl/write_store.R): correct
# rows, idempotent re-writes, no duplicates, scoped replacement, atomic
# transactions. constants.R / db_schema.R come from helper-db.R.
source(testthat::test_path("..", "..", "etl", "write_store.R"))
source(testthat::test_path("..", "..", "etl", "stats.R"))

site <- "USGS-01101000"
count_rows <- function(con, sql) DBI::dbGetQuery(con, sql)$n

flow_daily_df <- function(dates, vals, status = "Provisional") {
  data.frame(
    site_no = site, date = dates, discharge_cfs = vals,
    approval_status = status, qualifier = NA_character_, stringsAsFactors = FALSE
  )
}

iv_df <- function(site_no, value_name, values) {
  df <- data.frame(
    site_no = site_no, datetime = c("2026-09-14 00:00:00", "2026-09-14 00:15:00"),
    v = values, approval_status = "Provisional", qualifier = NA
  )
  names(df)[3] <- value_name
  df
}

drought_df <- function(map_date, d0, d1 = 0, d2 = 0) {
  data.frame(fips = "25009", map_date = map_date, d0 = d0, d1 = d1, d2 = d2, d3 = 0, d4 = 0)
}

test_that("upsert_flow_daily writes rows and is idempotent on re-write", {
  con <- build_empty_db()
  on.exit(DBI::dbDisconnect(con))
  dates <- c("2024-01-01", "2024-01-02", "2024-01-03")

  expect_equal(upsert_flow_daily(con, flow_daily_df(dates, c(10, 11, 12))), 3L)
  expect_equal(upsert_flow_daily(con, flow_daily_df(dates, c(10, 11, 12))), 3L)
  expect_equal(upsert_flow_daily(con, flow_daily_df(dates, c(50, 51, 52), "Approved")), 3L)

  rows <- DBI::dbGetQuery(con, "SELECT * FROM flow_daily WHERE site_no = ? ORDER BY date", params = list(site))
  expect_equal(nrow(rows), 3)
  expect_equal(rows$discharge_cfs, c(50, 51, 52))
  expect_equal(rows$approval_status, rep("Approved", 3))
})

test_that("upsert_flow_daily only replaces rows inside the written range and site", {
  con <- build_empty_db()
  on.exit(DBI::dbDisconnect(con))
  upsert_flow_daily(con, flow_daily_df(c("2024-01-01", "2024-01-02", "2024-01-03"), c(10, 11, 12)))
  other <- flow_daily_df("2024-01-02", 77)
  other$site_no <- "USGS-01102000"
  upsert_flow_daily(con, other)

  upsert_flow_daily(con, flow_daily_df(c("2024-01-02", "2024-01-03", "2024-01-04"), c(99, 98, 97)))

  rows <- DBI::dbGetQuery(con, "SELECT * FROM flow_daily WHERE site_no = ? ORDER BY date", params = list(site))
  expect_equal(rows$date, c("2024-01-01", "2024-01-02", "2024-01-03", "2024-01-04"))
  expect_equal(rows$discharge_cfs, c(10, 99, 98, 97))
  ipswich <- DBI::dbGetQuery(
    con, "SELECT discharge_cfs FROM flow_daily WHERE site_no = ?",
    params = list("USGS-01102000")
  )
  expect_equal(ipswich$discharge_cfs, 77)
})

test_that("duplicate keys inside one input are collapsed (last wins), not a PK error", {
  con <- build_empty_db()
  on.exit(DBI::dbDisconnect(con))
  n <- upsert_flow_daily(con, flow_daily_df(c("2024-01-01", "2024-01-01", "2024-01-02"), c(1, 2, 3)))
  expect_equal(n, 2L)
  rows <- DBI::dbGetQuery(con, "SELECT * FROM flow_daily ORDER BY date")
  expect_equal(rows$discharge_cfs, c(2, 3))
})

test_that("NULL or empty input is a no-op returning 0; missing columns error", {
  con <- build_empty_db()
  on.exit(DBI::dbDisconnect(con))
  expect_equal(upsert_flow_daily(con, NULL), 0L)
  expect_equal(upsert_flow_daily(con, data.frame()), 0L)
  expect_equal(upsert_precip_instantaneous(con, NULL), 0L)
  expect_equal(upsert_flow_percentiles(con, site, NULL), 0L)
  expect_equal(upsert_drought_status(con, data.frame()), 0L)
  expect_error(upsert_flow_daily(con, data.frame(site_no = site, date = "2024-01-01")), "discharge_cfs")
  expect_equal(count_rows(con, "SELECT COUNT(*) AS n FROM flow_daily"), 0)
})

test_that("instantaneous upserts (flow, groundwater, precip) are idempotent", {
  con <- build_empty_db()
  on.exit(DBI::dbDisconnect(con))
  flow <- iv_df(site, "discharge_cfs", c(1, 2))
  gw <- iv_df(GW_SITE, "depth_ft", c(10.1, 10.2))
  pr <- iv_df(PRECIP_SITE, "precip_in", c(0, 0.01))
  for (i in 1:2) {
    expect_equal(upsert_flow_instantaneous(con, flow), 2L)
    expect_equal(upsert_gw_instantaneous(con, gw), 2L)
    expect_equal(upsert_precip_instantaneous(con, pr), 2L)
  }
  expect_equal(count_rows(con, "SELECT COUNT(*) AS n FROM flow_instantaneous"), 2)
  expect_equal(count_rows(con, "SELECT COUNT(*) AS n FROM groundwater_instantaneous"), 2)
  expect_equal(count_rows(con, "SELECT COUNT(*) AS n FROM precip_instantaneous"), 2)
  depths <- DBI::dbGetQuery(con, "SELECT depth_ft FROM groundwater_instantaneous ORDER BY datetime")$depth_ft
  expect_equal(depths, c(10.1, 10.2))
})

test_that("groundwater and precip daily upserts are idempotent", {
  con <- build_empty_db()
  on.exit(DBI::dbDisconnect(con))
  gw <- data.frame(site_no = GW_SITE, date = c("2026-09-01", "2026-09-02"), depth_ft = c(9, 9.1))
  pr <- data.frame(site_no = PRECIP_SITE, date = c("2026-09-01", "2026-09-02"), precip_in = c(0.33, 0))
  for (i in 1:2) {
    expect_equal(upsert_groundwater_daily(con, gw), 2L)
    expect_equal(upsert_precip_daily(con, pr), 2L)
  }
  expect_equal(count_rows(con, "SELECT COUNT(*) AS n FROM groundwater_daily"), 2)
  expect_equal(DBI::dbGetQuery(con, "SELECT precip_in FROM precip_daily ORDER BY date")$precip_in, c(0.33, 0))
})

test_that("percentile and typical tables are fully replaced per site", {
  con <- build_empty_db()
  on.exit(DBI::dbDisconnect(con))
  hist <- data.frame(date = c("2020-06-15", "2021-06-15"), value = c(1, 3))

  full <- build_percentile_table(hist, reference_year = 2026)
  expect_equal(upsert_flow_percentiles(con, site, full), 366L)
  expect_equal(upsert_flow_percentiles(con, site, full), 366L)
  expect_equal(count_rows(con, "SELECT COUNT(*) AS n FROM flow_percentiles"), 366)
  jun15 <- DBI::dbGetQuery(
    con, "SELECT p50, years_used FROM flow_percentiles WHERE site_no = ? AND month_nu = ? AND day_nu = ?",
    params = list(site, 6L, 15L)
  )
  expect_equal(jun15$p50, 2)
  expect_equal(jun15$years_used, 2L)

  expect_equal(upsert_flow_percentiles(con, site, full[1:2, ]), 2L)
  expect_equal(count_rows(con, "SELECT COUNT(*) AS n FROM flow_percentiles"), 2)

  expect_equal(upsert_groundwater_percentiles(con, GW_SITE, full), 366L)
  expect_equal(upsert_groundwater_percentiles(con, GW_SITE, full), 366L)
  expect_equal(count_rows(con, "SELECT COUNT(*) AS n FROM groundwater_percentiles"), 366)

  typ <- build_precip_typical_table(hist, reference_year = 2026)
  expect_equal(upsert_precip_typical(con, PRECIP_SITE, typ), 366L)
  expect_equal(upsert_precip_typical(con, PRECIP_SITE, typ), 366L)
  expect_equal(count_rows(con, "SELECT COUNT(*) AS n FROM precip_typical"), 366)
})

test_that("upsert_drought_status is idempotent and keeps other map dates", {
  con <- build_empty_db()
  on.exit(DBI::dbDisconnect(con))
  df1 <- drought_df(c("2024-01-02", "2024-01-09"), d0 = c(50, 60), d1 = c(20, 30), d2 = c(0, 10))
  expect_equal(upsert_drought_status(con, df1), 2L)
  df2 <- df1
  df2$d0 <- c(70, 80)
  expect_equal(upsert_drought_status(con, df2), 2L)
  upsert_drought_status(con, drought_df("2024-01-16", d0 = 1))

  rows <- DBI::dbGetQuery(con, "SELECT * FROM drought_status ORDER BY map_date")
  expect_equal(nrow(rows), 3)
  expect_equal(rows$d0, c(70, 80, 1))
})

test_that("a failed insert rolls back the delete (transaction)", {
  con <- build_empty_db()
  on.exit(DBI::dbDisconnect(con))
  upsert_flow_daily(con, flow_daily_df(c("2024-01-01", "2024-01-02"), c(10, 11)))

  mockery::stub(.replace_rows, "DBI::dbAppendTable", function(...) stop("simulated insert failure"))
  out <- data.frame(site_no = site, date = "2024-01-01", discharge_cfs = 1, approval_status = NA, qualifier = NA)
  expect_error(
    .replace_rows(
      con, "flow_daily",
      "DELETE FROM flow_daily WHERE site_no = ? AND date >= ? AND date <= ?",
      list(site, "2024-01-01", "2024-01-02"), out
    ),
    "simulated insert failure"
  )
  rows <- DBI::dbGetQuery(con, "SELECT discharge_cfs FROM flow_daily ORDER BY date")
  expect_equal(rows$discharge_cfs, c(10, 11))
})

test_that("SQL-looking values are stored as data, not executed", {
  con <- build_empty_db()
  on.exit(DBI::dbDisconnect(con))
  evil <- "x'); DROP TABLE flow_daily; --"
  df <- flow_daily_df("2024-01-01", 1)
  df$site_no <- evil
  expect_equal(upsert_flow_daily(con, df), 1L)
  expect_true(DBI::dbExistsTable(con, "flow_daily"))
  expect_equal(read_daily_series(con, "flow", evil)$value, 1)
})

test_that("log_etl_run inserts one row with the given fields", {
  con <- build_empty_db()
  on.exit(DBI::dbDisconnect(con))
  log_etl_run(con, "flow_latest:USGS-01101000", "2026-01-01 00:00:00", "2026-01-01 00:00:05", "success", 42L)
  rows <- DBI::dbGetQuery(con, "SELECT * FROM etl_runs")
  expect_equal(nrow(rows), 1)
  expect_equal(rows$source, "flow_latest:USGS-01101000")
  expect_equal(rows$rows_written, 42L)
  expect_true(is.na(rows$error_message))
})

test_that("read_daily_series and latest_daily_date read the stored series", {
  con <- build_empty_db()
  on.exit(DBI::dbDisconnect(con))
  expect_true(is.na(latest_daily_date(con, "flow", site)))
  expect_equal(nrow(read_daily_series(con, "precip", PRECIP_SITE)), 0)
  upsert_flow_daily(con, flow_daily_df(c("2024-01-03", "2024-01-01"), c(3, 1)))
  expect_equal(latest_daily_date(con, "flow", site), as.Date("2024-01-03"))
  s <- read_daily_series(con, "flow", site)
  expect_named(s, c("date", "value"))
  expect_equal(s$date, c("2024-01-01", "2024-01-03"))
  expect_error(read_daily_series(con, "flow_daily; DROP TABLE sites", site), "unknown kind")
})

test_that("latest_drought_date returns NA on an empty store and the max map_date per county", {
  con <- build_empty_db()
  on.exit(DBI::dbDisconnect(con))
  expect_true(is.na(latest_drought_date(con, "25009")))
  upsert_drought_status(con, drought_df(c("2026-09-01", "2026-09-08"), d0 = c(100, 100)))
  other <- drought_df("2026-09-15", d0 = 5)
  other$fips <- "25017"
  upsert_drought_status(con, other)
  expect_equal(latest_drought_date(con, "25009"), as.Date("2026-09-08"))
  expect_equal(latest_drought_date(con, "25017"), as.Date("2026-09-15"))
  expect_true(is.na(earliest_drought_date(con, "99999")))
  expect_equal(earliest_drought_date(con, "25009"), as.Date("2026-09-01"))
  expect_equal(earliest_drought_date(con, "25017"), as.Date("2026-09-15"))
})

test_that("ensure_schema and seed_sites are idempotent", {
  con <- build_empty_db()
  on.exit(DBI::dbDisconnect(con))
  ensure_schema(con)
  seed_sites(con)
  seed_sites(con)
  s <- DBI::dbGetQuery(con, "SELECT site_no, kind FROM sites ORDER BY site_no")
  expect_equal(nrow(s), 4)
  expect_setequal(s$kind, c("flow", "flow", "groundwater", "precip"))
  expect_error(DBI::dbExecute(con, "INSERT INTO sites (site_no, kind) VALUES ('x', 'bogus')"))
})
