# tests/testthat/test-security-sql_injection.R
#
# Gate 4 (security). Hostile strings go into every caller-supplied argument
# of every R/data_access.R function that reaches SQL (site_no, fips,
# source_prefix, kind, dates). For each payload we assert:
#   - no error,
#   - no unrelated rows (empty shape / NA),
#   - afterwards every table still exists with its row count unchanged, and
#     the real seeded values are still readable.
# The last point is the structural proof: a query that merely "matched
# nothing" is also what a truncated/extended statement could look like;
# intact tables and counts show the payload was bound as a value.
#
# Also covered:
#   - get_last_updated() compares its prefix literally, so LIKE wildcards
#     ("%", "_") are ordinary characters.
#   - An invalid `kind` never reaches the database: with DBI::dbGetQuery
#     mocked to count calls, an invalid kind makes zero calls (a valid kind
#     makes one, proving the mock is live).
#
# Uses a real on-disk SQLite fixture (helper-db.R), so RSQLite's actual
# parameter binding is exercised. Row counts use DBI::dbReadTable(), so this
# file builds no SQL strings itself.

PAYLOADS <- c(
  tautology = "x' OR '1'='1",
  drop_sites = "'; DROP TABLE sites; --",
  like_percent = "%",
  like_underscore = "_",
  union_select = "' UNION SELECT source, finished_at FROM etl_runs --",
  comment_suffix = "USGS-01101000'--",
  or_true = "USGS-01101000' OR 1=1 --",
  stacked_delete = "25009'; DELETE FROM drought_status; --"
)

snapshot_counts <- function(con) {
  tables <- sort(DBI::dbListTables(con))
  stats::setNames(vapply(tables, function(t) nrow(DBI::dbReadTable(con, t)), integer(1)), tables)
}

expect_db_intact <- function(con, baseline) {
  for (tbl in names(baseline)) {
    expect_true(DBI::dbExistsTable(con, tbl), info = paste("table missing:", tbl))
  }
  expect_identical(snapshot_counts(con), baseline)
}

expect_real_values_readable <- function(con) {
  expect_identical(nrow(get_sites(con)), 4L)
  expect_identical(get_latest_reading(con, "flow", SITE_PARKER)$value, 12.5)
  expect_identical(nrow(get_drought_status(con, ESSEX_COUNTY_FIPS)), 2L)
  expect_identical(get_last_updated(con, "drought:"), "2024-01-09 08:02:05")
}

with_fixture <- function(code) {
  con <- build_fixture_db()
  on.exit(DBI::dbDisconnect(con))
  baseline <- snapshot_counts(con)
  code(con)
  expect_db_intact(con, baseline)
  expect_real_values_readable(con)
}

# ---- site_no / fips / kind-filter payloads ------------------------------------

test_that("get_sites binds a hostile kind filter as a value", {
  with_fixture(function(con) {
    for (p in PAYLOADS) {
      res <- NULL
      expect_no_error(res <- get_sites(con, kind = p))
      expect_identical(nrow(res), 0L, info = p)
    }
  })
})

test_that("get_latest_reading binds a hostile site_no for every kind", {
  with_fixture(function(con) {
    for (k in c("flow", "groundwater", "precip")) {
      for (p in PAYLOADS) {
        res <- NULL
        expect_no_error(res <- get_latest_reading(con, k, p))
        expect_identical(res, list(value = NA_real_, datetime = NA_character_), info = paste(k, p))
      }
    }
  })
})

test_that("get_instantaneous_series binds a hostile site_no for every kind", {
  with_fixture(function(con) {
    for (k in c("flow", "groundwater", "precip")) {
      for (p in PAYLOADS) {
        res <- NULL
        expect_no_error(res <- get_instantaneous_series(con, k, p, days = 100000))
        expect_identical(nrow(res), 0L, info = paste(k, p))
      }
    }
  })
})

test_that("get_daily_series binds hostile site_no and date strings", {
  with_fixture(function(con) {
    for (k in c("flow", "groundwater", "precip")) {
      for (p in PAYLOADS) {
        res <- NULL
        expect_no_error(res <- get_daily_series(con, k, p, "1900-01-01", "2100-12-31"))
        expect_identical(nrow(res), 0L, info = paste(k, p))
        expect_no_error(res <- get_daily_series(con, k, SITE_PARKER, p, "2100-12-31"))
        expect_identical(nrow(res), 0L, info = paste(k, "start_date", p))
        expect_no_error(res <- get_daily_series(con, k, SITE_PARKER, "1900-01-01", p))
        expect_identical(nrow(res), 0L, info = paste(k, "end_date", p))
      }
    }
  })
})

test_that("get_percentiles binds hostile site_no and rejects hostile month/day filters", {
  with_fixture(function(con) {
    for (k in c("flow", "groundwater")) {
      for (p in PAYLOADS) {
        res <- NULL
        expect_no_error(res <- get_percentiles(con, k, p))
        expect_identical(nrow(res), 0L, info = paste(k, p))
        # A hostile filter must not be treated as "no filter" (all rows).
        expect_no_error(res <- get_percentiles(con, k, SITE_PARKER, month_nu = p))
        expect_identical(nrow(res), 0L, info = paste(k, "month_nu", p))
        expect_no_error(res <- get_percentiles(con, k, SITE_PARKER, day_nu = p))
        expect_identical(nrow(res), 0L, info = paste(k, "day_nu", p))
      }
    }
  })
})

test_that("precipitation window functions bind a hostile site_no and end_date", {
  with_fixture(function(con) {
    for (p in PAYLOADS) {
      total <- typical <- NULL
      expect_no_error(total <- get_precip_window_total(con, p))
      expect_identical(total$days_with_data, 0L, info = p)
      expect_identical(total$total_in, NA_real_, info = p)
      expect_no_error(total <- get_precip_window_total(con, PRECIP_SITE, end_date = p))
      expect_identical(total$days_with_data, 0L, info = paste("end_date", p))

      expect_no_error(typical <- get_precip_typical_window(con, p, end_date = Sys.Date()))
      expect_identical(typical, list(typical_in = NA_real_, years_used = 0L), info = p)
      expect_no_error(typical <- get_precip_typical_window(con, PRECIP_SITE, end_date = p))
      expect_identical(typical$years_used, 0L, info = paste("end_date", p))
    }
  })
})

test_that("get_low_flow_days binds a hostile site_no", {
  with_fixture(function(con) {
    for (p in PAYLOADS) {
      res <- NULL
      expect_no_error(res <- get_low_flow_days(con, p, years = 2022:2023))
      expect_identical(nrow(res), 0L, info = p)
      expect_no_error(res <- get_low_flow_days(con, SITE_PARKER, years = p))
      expect_identical(nrow(res), 0L, info = paste("years", p))
    }
  })
})

test_that("get_drought_status binds a hostile fips", {
  with_fixture(function(con) {
    for (p in PAYLOADS) {
      res <- NULL
      expect_no_error(res <- get_drought_status(con, p))
      expect_identical(nrow(res), 0L, info = p)
    }
  })
})

# ---- get_last_updated / get_source_status ------------------------------------------

test_that("get_last_updated binds a hostile prefix and treats LIKE wildcards literally", {
  with_fixture(function(con) {
    for (p in PAYLOADS) {
      res <- "unset"
      expect_no_error(res <- get_last_updated(con, p))
      # "%" and "_" included: neither may widen the match to every source.
      expect_identical(res, NA_character_, info = p)
    }
    expect_identical(get_last_updated(con, "flow%"), NA_character_)
    expect_identical(get_last_updated(con, "flow_latest_"), NA_character_)
    expect_identical(get_last_updated(con, "fl_w"), NA_character_)
    expect_identical(get_last_updated(con, "flow_latest:"), "2024-01-09 08:00:05")
  })
})

test_that("get_last_updated matches a source that really contains wildcard characters", {
  con <- build_fixture_db()
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(
    con,
    "INSERT INTO etl_runs (source, started_at, finished_at, status, rows_written) VALUES (?, ?, ?, ?, ?)",
    params = list("odd%_source:x", "2025-01-01 00:00:00", "2025-01-01 00:00:09", "success", 1L)
  )
  expect_identical(get_last_updated(con, "odd%_"), "2025-01-01 00:00:09")
  expect_identical(get_last_updated(con, "odd%"), "2025-01-01 00:00:09")
  expect_identical(get_last_updated(con, "oddX_"), NA_character_)
  expect_identical(get_last_updated(con, "odd__"), NA_character_)
})

test_that("get_source_status stays correct after hostile calls elsewhere", {
  with_fixture(function(con) {
    for (p in PAYLOADS) {
      get_last_updated(con, p)
      get_latest_reading(con, "flow", p)
    }
    res <- NULL
    expect_no_error(res <- get_source_status(con))
    expect_identical(nrow(res), 4L)
  })
})

# ---- kind whitelist ------------------------------------------------------------------

test_that("a hostile or unsupported kind returns the empty shape for every kind-taking function", {
  with_fixture(function(con) {
    bad_kinds <- c(PAYLOADS, sql_table = "flow_instantaneous", upper = "FLOW", blank = "")
    for (k in bad_kinds) {
      expect_identical(get_latest_reading(con, k, SITE_PARKER), list(value = NA_real_, datetime = NA_character_))
      expect_identical(nrow(get_instantaneous_series(con, k, SITE_PARKER, days = 100000)), 0L)
      expect_identical(nrow(get_daily_series(con, k, SITE_PARKER, "1900-01-01", "2100-12-31")), 0L)
      expect_identical(nrow(get_percentiles(con, k, SITE_PARKER)), 0L)
    }
    expect_identical(nrow(get_percentiles(con, "precip", PRECIP_SITE)), 0L)
  })
})

test_that("an invalid kind never reaches the database", {
  con <- build_fixture_db()
  on.exit(DBI::dbDisconnect(con))

  calls <- 0L
  local_mocked_bindings(
    dbGetQuery = function(...) {
      calls <<- calls + 1L
      data.frame()
    },
    .package = "DBI"
  )

  # Control: a valid kind issues exactly one query, so the mock is live.
  get_latest_reading(con, "flow", SITE_PARKER)
  expect_identical(calls, 1L)

  calls <- 0L
  hostile_kind <- "flow_instantaneous; DROP TABLE sites; --"
  get_latest_reading(con, hostile_kind, SITE_PARKER)
  get_instantaneous_series(con, hostile_kind, SITE_PARKER, days = 7)
  get_daily_series(con, hostile_kind, SITE_PARKER, "2020-01-01", "2020-12-31")
  get_percentiles(con, hostile_kind, SITE_PARKER)
  get_percentiles(con, "precip", PRECIP_SITE)
  get_latest_reading(con, NULL, SITE_PARKER)
  expect_identical(calls, 0L)
})
