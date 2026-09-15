# tests/testthat/test-etl-fetch_flow.R
#
# Streamflow fetchers plus the shared OGC client they exercise
# (etl/http_common.R): success from captured fixtures, empty features,
# HTTP 500, network error, malformed JSON, missing fields, wrong content
# type, cursor pagination, page cap, foreign next-link host, chunking, and
# timestamp / qualifier normalization. httr2 is mocked with
# local_mocked_responses(); zero live network calls.
source(testthat::test_path("..", "..", "etl", "http_common.R"))
source(testthat::test_path("..", "..", "etl", "fetch_flow.R"))

fx <- function(name) paste(readLines(testthat::test_path("fixture-data", name), warn = FALSE), collapse = "\n")
json_response <- function(txt, status = 200L, ctype = "application/json; charset=utf-8") {
  httr2::response(status_code = status, headers = list(`Content-Type` = ctype), body = charToRaw(txt))
}
fixture_props <- function(name) {
  p <- jsonlite::fromJSON(fx(name))$features$properties
  p[order(p$time), , drop = FALSE]
}
query_of <- function(req) httr2::url_parse(req$url)$query

latest_txt <- fx("usgs_flow_latest.json")
daily_txt <- fx("usgs_flow_daily.json")
page1_txt <- fx("usgs_flow_daily_page1.json")
empty_txt <- fx("usgs_empty.json")

# ---- success -------------------------------------------------------------------

test_that("fetch_flow_latest parses the continuous fixture into the documented shape", {
  seen <- new.env()
  httr2::local_mocked_responses(function(req) {
    seen$req <- req
    json_response(latest_txt)
  })
  res <- fetch_flow_latest(SITE_PARKER, days = 1, end_time = as.POSIXct("2026-09-15 04:00:00", tz = "UTC"))

  expected <- fixture_props("usgs_flow_latest.json")
  expect_s3_class(res, "data.frame")
  expect_named(res, c("site_no", "datetime", "discharge_cfs", "approval_status", "qualifier"))
  expect_equal(nrow(res), nrow(expected))
  expect_true(all(res$site_no == SITE_PARKER))
  expect_true(all(grepl("^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$", res$datetime)))
  expect_false(is.unsorted(res$datetime))
  expect_equal(res$discharge_cfs, as.numeric(expected$value))
  expect_type(res$discharge_cfs, "double")

  q <- query_of(seen$req)
  expect_match(seen$req$url, "/collections/continuous/items", fixed = TRUE)
  expect_equal(q$monitoring_location_id, SITE_PARKER)
  expect_equal(q$parameter_code, "00060")
  expect_equal(q$datetime, "2026-09-14T04:00:00Z/2026-09-15T04:00:00Z")
  expect_equal(seen$req$options$timeout_ms, HTTP_TIMEOUT_LATEST * 1000)
})

test_that("fetch_flow_daily parses the daily fixture and requests statistic 00003", {
  seen <- new.env()
  httr2::local_mocked_responses(function(req) {
    seen$req <- req
    json_response(daily_txt)
  })
  res <- fetch_flow_daily(SITE_PARKER, "2026-09-01", "2026-09-15")

  expected <- fixture_props("usgs_flow_daily.json")
  expect_named(res, c("site_no", "date", "discharge_cfs", "approval_status", "qualifier"))
  expect_equal(res$date, expected$time)
  expect_equal(res$discharge_cfs, as.numeric(expected$value))

  q <- query_of(seen$req)
  expect_match(seen$req$url, "/collections/daily/items", fixed = TRUE)
  expect_equal(q$statistic_id, "00003")
  expect_equal(q$datetime, "2026-09-01/2026-09-15")
  expect_equal(seen$req$options$timeout_ms, HTTP_TIMEOUT_DAILY * 1000)
})

# ---- failure modes --------------------------------------------------------------

test_that("an empty FeatureCollection returns NULL with a recorded reason", {
  httr2::local_mocked_responses(function(req) json_response(empty_txt))
  expect_null(fetch_flow_latest(SITE_PARKER, days = 1))
  expect_match(etl_last_failure(), "no valid rows")
  expect_null(fetch_flow_daily(SITE_PARKER, "2026-09-01", "2026-09-15"))
})

test_that("HTTP 500 returns NULL", {
  httr2::local_mocked_responses(function(req) json_response('{"code":"err"}', status = 500L))
  expect_null(fetch_flow_latest(SITE_PARKER, days = 1))
  expect_match(etl_last_failure(), "HTTP 500")
  expect_null(fetch_flow_daily(SITE_PARKER, "2026-09-01", "2026-09-15"))
})

test_that("a network error returns NULL and does not propagate", {
  httr2::local_mocked_responses(function(req) stop("simulated network error"))
  expect_no_error(res <- fetch_flow_latest(SITE_PARKER, days = 1))
  expect_null(res)
  expect_match(etl_last_failure(), "simulated network error")
  expect_null(fetch_flow_daily(SITE_PARKER, "2026-09-01", "2026-09-15"))
})

test_that("malformed JSON returns NULL", {
  httr2::local_mocked_responses(function(req) json_response("{not valid json"))
  expect_null(fetch_flow_latest(SITE_PARKER, days = 1))
  expect_null(fetch_flow_daily(SITE_PARKER, "2026-09-01", "2026-09-15"))
})

test_that("missing fields (no features member, no value field) return NULL", {
  httr2::local_mocked_responses(function(req) json_response('{"type":"FeatureCollection"}'))
  expect_null(fetch_flow_daily(SITE_PARKER, "2026-09-01", "2026-09-15"))
  expect_match(etl_last_failure(), "features")

  no_value <- '{"type":"FeatureCollection","features":[{"type":"Feature","properties":{"time":"2026-09-01"}}]}'
  httr2::local_mocked_responses(function(req) json_response(no_value))
  expect_null(fetch_flow_daily(SITE_PARKER, "2026-09-01", "2026-09-15"))
  expect_match(etl_last_failure(), "missing required fields")
})

test_that("a non-JSON content type (e.g. an HTML error page) returns NULL", {
  httr2::local_mocked_responses(function(req) json_response(daily_txt, ctype = "text/html"))
  expect_null(fetch_flow_daily(SITE_PARKER, "2026-09-01", "2026-09-15"))
  expect_match(etl_last_failure(), "content type")
})

test_that("an invalid days argument returns NULL without a request", {
  called <- FALSE
  httr2::local_mocked_responses(function(req) {
    called <<- TRUE
    json_response(latest_txt)
  })
  expect_null(fetch_flow_latest(SITE_PARKER, days = -1))
  expect_false(called)
})

# ---- pagination -----------------------------------------------------------------

test_that("rel=next cursor links are followed until the last page", {
  urls <- character(0)
  httr2::local_mocked_responses(function(req) {
    urls <<- c(urls, req$url)
    if (grepl("cursor=", req$url, fixed = TRUE)) json_response(daily_txt) else json_response(page1_txt)
  })
  res <- fetch_flow_daily(SITE_PARKER, "2026-09-01", "2026-09-15")

  expect_length(urls, 2)
  expect_match(urls[2], "cursor=", fixed = TRUE)
  all_dates <- unique(c(fixture_props("usgs_flow_daily_page1.json")$time, fixture_props("usgs_flow_daily.json")$time))
  expect_equal(res$date, sort(all_dates))
  expect_false(anyDuplicated(res$date) > 0)
})

test_that("an endless next chain hits the page cap and returns NULL (never truncated data)", {
  n <- 0L
  httr2::local_mocked_responses(function(req) {
    n <<- n + 1L
    json_response(page1_txt)
  })
  expect_null(fetch_flow_daily(SITE_PARKER, "2026-09-01", "2026-09-15"))
  expect_equal(n, OGC_MAX_PAGES)
  expect_match(etl_last_failure(), "page cap")
})

test_that("a next link pointing at another host is refused", {
  evil <- sub("https://api.waterdata.usgs.gov/", "https://evil.example.com/", page1_txt, fixed = TRUE)
  urls <- character(0)
  httr2::local_mocked_responses(function(req) {
    urls <<- c(urls, req$url)
    json_response(evil)
  })
  expect_null(fetch_flow_daily(SITE_PARKER, "2026-09-01", "2026-09-15"))
  expect_length(urls, 1)
  expect_match(etl_last_failure(), "unexpected host")
})

# ---- chunking -------------------------------------------------------------------

test_that("long daily ranges are requested in DAILY_CHUNK_YEARS windows", {
  windows <- character(0)
  httr2::local_mocked_responses(function(req) {
    windows <<- c(windows, query_of(req)$datetime)
    json_response(daily_txt)
  })
  res <- fetch_flow_daily(SITE_PARKER, "1990-01-01", "2004-06-30")
  expect_equal(windows, c("1990-01-01/1994-12-31", "1995-01-01/1999-12-31", "2000-01-01/2004-06-30"))
  expect_equal(nrow(res), nrow(fixture_props("usgs_flow_daily.json"))) # duplicates across chunks collapsed
})

test_that("an empty chunk (before a record starts) does not fail the others", {
  httr2::local_mocked_responses(function(req) {
    if (startsWith(query_of(req)$datetime, "1990")) json_response(empty_txt) else json_response(daily_txt)
  })
  res <- fetch_flow_daily(SITE_PARKER, "1990-01-01", "1999-12-31")
  expect_equal(nrow(res), nrow(fixture_props("usgs_flow_daily.json")))
})

test_that("one failing chunk fails the whole daily fetch (no partial history)", {
  httr2::local_mocked_responses(function(req) {
    if (startsWith(query_of(req)$datetime, "1995")) json_response("{}", status = 503L) else json_response(daily_txt)
  })
  withr::local_options(essexwater.http_max_tries = 1L)
  expect_null(fetch_flow_daily(SITE_PARKER, "1990-01-01", "1999-12-31"))
})

test_that("date_chunks covers the range exactly and rejects inverted ranges", {
  ch <- date_chunks("2020-03-01", "2020-03-01", 5)
  expect_equal(nrow(ch), 1)
  expect_equal(ch$start, as.Date("2020-03-01"))
  expect_equal(ch$end, as.Date("2020-03-01"))
  expect_error(date_chunks("2021-01-01", "2020-01-01"), "invalid date range")
})

# ---- normalization ----------------------------------------------------------------

test_that(".to_utc_text converts offsets, Z and fractional seconds to UTC text", {
  expect_equal(
    .to_utc_text(c(
      "2026-09-12T00:00:00+00:00", "2026-09-12T00:00:00-05:00",
      "2026-09-12T23:30:00Z", "2026-09-12T01:00:00.000001+01:00", "garbage"
    )),
    c("2026-09-12 00:00:00", "2026-09-12 05:00:00", "2026-09-12 23:30:00", "2026-09-12 00:00:00", NA)
  )
})

test_that("qualifier arrays are collapsed, null values and non-numeric values dropped", {
  body <- paste0(
    '{"type":"FeatureCollection","features":[',
    '{"type":"Feature","properties":{"time":"2026-01-02","value":"1.5","approval_status":"Approved","qualifier":["ICE","ESTIMATED"]}},', # nolint: line_length_linter.
    '{"type":"Feature","properties":{"time":"2026-01-01","value":"2.5",',
    '"approval_status":"Approved","qualifier":null}},',
    '{"type":"Feature","properties":{"time":"2026-01-03","value":null,"approval_status":"Approved","qualifier":null}},',
    '{"type":"Feature","properties":{"time":"2026-01-04","value":"Eqp","approval_status":"Approved","qualifier":null}}',
    '],"links":[]}'
  )
  httr2::local_mocked_responses(function(req) json_response(body))
  res <- fetch_flow_daily(SITE_PARKER, "2026-01-01", "2026-01-04")
  expect_equal(res$date, c("2026-01-01", "2026-01-02"))
  expect_equal(res$discharge_cfs, c(2.5, 1.5))
  expect_equal(res$qualifier, c(NA, "ICE,ESTIMATED"))
})
