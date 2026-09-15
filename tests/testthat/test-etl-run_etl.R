# tests/testthat/test-etl-run_etl.R
#
# Orchestrator (etl/run_etl.R). Sourcing run_etl.R defines run_etl() without
# side effects (is_main_script is FALSE here). Fake fetchers are injected
# through run_etl(fetchers = ...), so no function environments are patched;
# the store is a temp SQLite DB from helper-db.R.
#
# Offline guard: every httr2 request made anywhere in this file is answered
# by a mock that throws, so an accidental live call can never reach the
# network and shows up as a failed assertion instead.
source(testthat::test_path("..", "..", "etl", "run_etl.R"))

httr2::local_mocked_responses(function(req) stop("live network call attempted in test: ", req$url))

TODAY <- as.Date("2026-09-15")
FLOW <- "USGS-01101000"
GW <- "USGS-424520070562401"
PR <- "USGS-424510070564401"
FIPS <- "25009"

series <- function(site, times, value_name, values) {
  time_col <- if (grepl(" ", times[1], fixed = TRUE)) "datetime" else "date"
  df <- data.frame(site_no = site, t = times, v = values, approval_status = "Approved", qualifier = NA_character_)
  names(df)[2:3] <- c(time_col, value_name)
  df
}
hist_dates <- c("2023-09-15", "2024-09-15", "2025-09-15", "2026-09-14")
flow_daily <- series(FLOW, hist_dates, "discharge_cfs", c(5, 7, 9, 0.1))
gw_daily <- series(GW, hist_dates, "depth_ft", c(8, 9, 10, 10.3))
pr_daily <- series(PR, c("2025-09-15", "2026-09-13"), "precip_in", c(0.4, 1.07))
flow_iv <- series(FLOW, c("2026-09-15 00:00:00", "2026-09-15 00:15:00"), "discharge_cfs", c(0.2, 0.17))
gw_iv <- series(GW, "2026-09-15 00:00:00", "depth_ft", 10.33)
pr_iv <- series(PR, "2026-09-15 00:00:00", "precip_in", 0)
drought <- data.frame(fips = FIPS, map_date = "2026-09-08", d0 = 100, d1 = 1.77, d2 = 0, d3 = 0, d4 = 0)

# All fetchers succeed with the small frames above; override any by name.
fake_fetchers <- function(...) {
  base <- list(
    flow_latest = function(...) flow_iv,
    flow_daily = function(...) flow_daily,
    groundwater_latest = function(...) gw_iv,
    groundwater_daily = function(...) gw_daily,
    precip_latest = function(...) pr_iv,
    precip_daily = function(...) pr_daily,
    drought = function(...) drought
  )
  utils::modifyList(base, list(...))
}
returns_null <- function(...) NULL

call_etl <- function(con, fetchers = fake_fetchers(), ...) {
  run_etl(
    con,
    flow_sites = FLOW, groundwater_sites = GW, precip_sites = PR, county_fips = FIPS,
    today = TODAY, fetchers = fetchers, ...
  )
}
n_rows <- function(con, table_sql) DBI::dbGetQuery(con, table_sql)$n

expected_sources <- c(
  paste0(c("flow_latest:", "flow_daily:", "flow_percentiles:"), FLOW),
  paste0(c("groundwater_latest:", "groundwater_daily:", "groundwater_percentiles:"), GW),
  paste0(c("precip_latest:", "precip_daily:", "precip_typical:"), PR),
  paste0("drought:", FIPS)
)

test_that("one failing source is logged while every other source still writes", {
  con <- build_empty_db()
  on.exit(DBI::dbDisconnect(con))
  fetchers <- fake_fetchers(flow_latest = function(...) stop("simulated flow outage"))

  result <- NULL
  expect_no_error(result <- call_etl(con, fetchers))

  expect_equal(names(result), expected_sources)
  runs <- DBI::dbGetQuery(con, "SELECT * FROM etl_runs ORDER BY run_id")
  expect_equal(runs$source, expected_sources)

  failed <- runs[runs$source == paste0("flow_latest:", FLOW), ]
  expect_equal(failed$status, "failure")
  expect_equal(failed$rows_written, 0L)
  expect_match(failed$error_message, "simulated flow outage")
  expect_true(all(runs$status[runs$source != paste0("flow_latest:", FLOW)] == "success"))

  expect_equal(n_rows(con, "SELECT COUNT(*) AS n FROM flow_instantaneous"), 0)
  expect_equal(n_rows(con, "SELECT COUNT(*) AS n FROM flow_daily"), 4)
  expect_equal(n_rows(con, "SELECT COUNT(*) AS n FROM groundwater_instantaneous"), 1)
  expect_equal(n_rows(con, "SELECT COUNT(*) AS n FROM precip_daily"), 2)
  expect_equal(n_rows(con, "SELECT COUNT(*) AS n FROM drought_status"), 1)

  # Percentiles come from stored prior-year history (2023-2025 -> 5, 7, 9).
  p <- DBI::dbGetQuery(
    con, "SELECT p50, years_used FROM flow_percentiles WHERE site_no = ? AND month_nu = 9 AND day_nu = 15",
    params = list(FLOW)
  )
  expect_equal(p$p50, 7)
  expect_equal(p$years_used, 3L)
  expect_equal(n_rows(con, "SELECT COUNT(*) AS n FROM groundwater_percentiles"), 366)
  typ <- DBI::dbGetQuery(
    con, "SELECT median_in, years_used FROM precip_typical WHERE site_no = ? AND month_nu = 9 AND day_nu = 15",
    params = list(PR)
  )
  expect_equal(typ$median_in, 0.4)
  expect_equal(typ$years_used, 1L)
  expect_equal(result[[paste0("flow_daily:", FLOW)]]$rows_written, 4L)
})

test_that("all fetchers returning NULL are logged as failures, never a crash", {
  con <- build_empty_db()
  on.exit(DBI::dbDisconnect(con))
  fetchers <- fake_fetchers(
    flow_latest = returns_null, flow_daily = returns_null,
    groundwater_latest = returns_null, groundwater_daily = returns_null,
    precip_latest = returns_null, precip_daily = returns_null, drought = returns_null
  )
  expect_no_error(result <- call_etl(con, fetchers))
  expect_true(all(vapply(result, function(r) r$status == "failure", logical(1))))
  runs <- DBI::dbGetQuery(con, "SELECT * FROM etl_runs")
  expect_equal(nrow(runs), length(expected_sources))
  expect_true(all(runs$status == "failure"))
  expect_match(runs$error_message[runs$source == paste0("flow_daily:", FLOW)], "NULL or empty")
  expect_match(runs$error_message[runs$source == paste0("flow_percentiles:", FLOW)], "no daily history")
})

test_that("a fetcher's recorded failure reason reaches etl_runs.error_message", {
  con <- build_empty_db()
  on.exit(DBI::dbDisconnect(con))
  fetchers <- fake_fetchers(drought = function(...) {
    etl_note_failure("drought 25009: unexpected content type text/csv")
    NULL
  })
  result <- call_etl(con, fetchers)
  expect_equal(result[[paste0("drought:", FIPS)]]$status, "failure")
  expect_match(result[[paste0("drought:", FIPS)]]$error_message, "text/csv")
  expect_true(is.na(result[[paste0("flow_daily:", FLOW)]]$error_message))
})

test_that("a writer error (malformed fetch result) is a logged failure, not a crash", {
  con <- build_empty_db()
  on.exit(DBI::dbDisconnect(con))
  fetchers <- fake_fetchers(groundwater_latest = function(...) data.frame(site_no = GW, nonsense = 1))
  expect_no_error(result <- call_etl(con, fetchers))
  r <- result[[paste0("groundwater_latest:", GW)]]
  expect_equal(r$status, "failure")
  expect_match(r$error_message, "missing column")
  expect_equal(result[[paste0("precip_daily:", PR)]]$status, "success")
})

test_that("percentiles are rebuilt from stored history even when the daily fetch fails", {
  con <- build_empty_db()
  on.exit(DBI::dbDisconnect(con))
  call_etl(con)
  result <- call_etl(con, fake_fetchers(flow_daily = function(...) stop("outage")))
  expect_equal(result[[paste0("flow_daily:", FLOW)]]$status, "failure")
  expect_equal(result[[paste0("flow_percentiles:", FLOW)]]$status, "success")
  expect_equal(n_rows(con, "SELECT COUNT(*) AS n FROM flow_daily"), 4) # nothing deleted
})

test_that("re-running is idempotent (no duplicate data rows)", {
  con <- build_empty_db()
  on.exit(DBI::dbDisconnect(con))
  call_etl(con)
  call_etl(con)
  expect_equal(n_rows(con, "SELECT COUNT(*) AS n FROM flow_daily"), 4)
  expect_equal(n_rows(con, "SELECT COUNT(*) AS n FROM flow_instantaneous"), 2)
  expect_equal(n_rows(con, "SELECT COUNT(*) AS n FROM flow_percentiles"), 366)
  expect_equal(n_rows(con, "SELECT COUNT(*) AS n FROM drought_status"), 1)
  expect_equal(n_rows(con, "SELECT COUNT(*) AS n FROM etl_runs"), 2 * length(expected_sources))
})

test_that("daily fetches backfill on an empty store, then fetch incrementally", {
  con <- build_empty_db()
  on.exit(DBI::dbDisconnect(con))
  calls <- list()
  fetchers <- fake_fetchers(flow_daily = function(site_id, start_date, end_date) {
    calls[[length(calls) + 1]] <<- list(start = as.Date(start_date), end = as.Date(end_date))
    flow_daily
  })

  call_etl(con, fetchers) # empty store -> full history
  call_etl(con, fetchers, daily_lookback_days = 45) # latest stored date 2026-09-14
  call_etl(con, fetchers, full_refresh = TRUE)

  expect_length(calls, 3)
  expect_equal(calls[[1]]$start, FLOW_HISTORY_START)
  expect_equal(calls[[1]]$end, TODAY)
  expect_equal(calls[[2]]$start, as.Date("2026-09-14") - 45)
  expect_equal(calls[[3]]$start, FLOW_HISTORY_START)
})

test_that("an incomplete fetchers list is rejected before anything runs", {
  con <- build_empty_db()
  on.exit(DBI::dbDisconnect(con))
  expect_error(call_etl(con, list(flow_latest = function(...) flow_iv)), "fetchers must supply")
  expect_equal(n_rows(con, "SELECT COUNT(*) AS n FROM etl_runs"), 0)
})

test_that("the offline guard intercepts real fetchers: failures are logged, no network is reached", {
  con <- build_empty_db()
  on.exit(DBI::dbDisconnect(con))
  httr2::local_mocked_responses(function(req) stop("live network call attempted in test: ", req$url))
  result <- run_etl(
    con,
    flow_sites = FLOW, groundwater_sites = character(0), precip_sites = character(0),
    county_fips = FIPS, today = TODAY
  )
  expect_equal(result[[paste0("flow_latest:", FLOW)]]$status, "failure")
  expect_match(result[[paste0("flow_latest:", FLOW)]]$error_message, "live network call attempted")
  expect_match(result[[paste0("drought:", FIPS)]]$error_message, "live network call attempted")
  expect_equal(n_rows(con, "SELECT COUNT(*) AS n FROM flow_daily"), 0)
})

drought_weeks <- function(map_dates) {
  data.frame(fips = FIPS, map_date = map_dates, d0 = 50, d1 = 0, d2 = 0, d3 = 0, d4 = 0)
}

test_that("drought backfills 3 years on an empty store, then fetches from latest map_date - 14 days", {
  con <- build_empty_db()
  on.exit(DBI::dbDisconnect(con))
  three_years <- drought_weeks(c("2023-09-12", "2025-03-04", "2026-09-08"))
  calls <- list()
  fetchers <- fake_fetchers(drought = function(fips, days_back = 14, today = Sys.Date(), start_date = NULL) {
    calls[[length(calls) + 1]] <<- list(fips = fips, start = as.Date(start_date), today = as.Date(today))
    three_years
  })

  call_etl(con, fetchers) # empty store -> 3-year backfill
  call_etl(con, fetchers) # history reaches 2023-09-12; latest 2026-09-08 -> start 2026-08-25
  call_etl(con, fetchers, full_refresh = TRUE, drought_backfill_years = 1)

  expect_length(calls, 3)
  expect_equal(calls[[1]]$fips, FIPS)
  expect_equal(calls[[1]]$start, as.Date("2023-09-15"))
  expect_equal(calls[[1]]$today, TODAY)
  expect_equal(calls[[2]]$start, as.Date("2026-08-25"))
  expect_equal(calls[[3]]$start, as.Date("2025-09-15"))
})

test_that("a store whose drought history does not reach the backfill start is backfilled", {
  con <- build_empty_db()
  on.exit(DBI::dbDisconnect(con))
  # Mirrors the real store before this change: only the last two weeks.
  call_etl(con, fake_fetchers(drought = function(...) drought_weeks(c("2026-09-01", "2026-09-08"))))
  starts <- as.Date(character(0))
  call_etl(con, fake_fetchers(drought = function(fips, today, start_date, ...) {
    starts <<- c(starts, as.Date(start_date))
    drought
  }))
  expect_equal(starts, as.Date("2023-09-15"))
})

test_that("the incremental drought window never starts after today", {
  con <- build_empty_db()
  on.exit(DBI::dbDisconnect(con))
  future <- drought_weeks(c("2023-09-12", "2026-10-06"))
  call_etl(con, fake_fetchers(drought = function(...) future))
  starts <- as.Date(character(0))
  call_etl(con, fake_fetchers(drought = function(fips, today, start_date, ...) {
    starts <<- c(starts, as.Date(start_date))
    drought
  }))
  expect_equal(starts, TODAY)
})

test_that("an incremental drought fetch keeps older stored weeks", {
  con <- build_empty_db()
  on.exit(DBI::dbDisconnect(con))
  history <- data.frame(
    fips = FIPS, map_date = c("2024-01-02", "2025-06-03", "2026-09-01"),
    d0 = c(10, 20, 100), d1 = 0, d2 = 0, d3 = 0, d4 = 0
  )
  call_etl(con, fake_fetchers(drought = function(...) history))
  result <- call_etl(con, fake_fetchers(drought = function(...) drought)) # one new week
  expect_equal(result[[paste0("drought:", FIPS)]]$status, "success")
  stored <- DBI::dbGetQuery(con, "SELECT map_date FROM drought_status ORDER BY map_date")$map_date
  expect_equal(stored, c("2024-01-02", "2025-06-03", "2026-09-01", "2026-09-08"))
})
