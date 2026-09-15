# tests/testthat/test-etl-fetch_precip.R
#
# Precipitation (00045) fetchers against captured OGC fixtures, httr2 mocked.
# The daily fetcher must request statistic 00006 (sum), verified live.
source(testthat::test_path("..", "..", "etl", "http_common.R"))
source(testthat::test_path("..", "..", "etl", "fetch_precip.R"))

fx <- function(name) paste(readLines(testthat::test_path("fixture-data", name), warn = FALSE), collapse = "\n")
json_response <- function(txt, status = 200L, ctype = "application/json; charset=utf-8") {
  httr2::response(status_code = status, headers = list(`Content-Type` = ctype), body = charToRaw(txt))
}
fixture_props <- function(name) {
  p <- jsonlite::fromJSON(fx(name))$features$properties
  p[order(p$time), , drop = FALSE]
}
query_of <- function(req) httr2::url_parse(req$url)$query

test_that("fetch_precip_latest parses the continuous fixture", {
  seen <- new.env()
  httr2::local_mocked_responses(function(req) {
    seen$req <- req
    json_response(fx("usgs_precip_latest.json"))
  })
  res <- fetch_precip_latest(days = 1)
  expected <- fixture_props("usgs_precip_latest.json")

  expect_named(res, c("site_no", "datetime", "precip_in", "approval_status", "qualifier"))
  expect_equal(nrow(res), nrow(expected))
  expect_true(all(res$site_no == PRECIP_SITE))
  expect_equal(res$precip_in, as.numeric(expected$value))
  expect_true(all(res$precip_in >= 0))
  expect_equal(query_of(seen$req)$parameter_code, "00045")
  expect_match(seen$req$url, "/collections/continuous/items", fixed = TRUE)
  expect_equal(seen$req$options$timeout_ms, HTTP_TIMEOUT_LATEST * 1000)
})

test_that("fetch_precip_daily parses daily totals and requests statistic 00006", {
  seen <- new.env()
  httr2::local_mocked_responses(function(req) {
    seen$req <- req
    json_response(fx("usgs_precip_daily.json"))
  })
  res <- fetch_precip_daily(start_date = "2026-09-01", end_date = "2026-09-15")
  expected <- fixture_props("usgs_precip_daily.json")

  expect_named(res, c("site_no", "date", "precip_in", "approval_status", "qualifier"))
  expect_equal(res$date, expected$time)
  expect_equal(res$precip_in, as.numeric(expected$value))
  expect_true(all(res$precip_in >= 0 & res$precip_in < 15)) # plausible daily totals (in)
  q <- query_of(seen$req)
  expect_equal(q$statistic_id, "00006")
  expect_equal(q$parameter_code, "00045")
  expect_equal(q$monitoring_location_id, PRECIP_SITE)
})

test_that("empty features (e.g. before the gauge record starts) return NULL", {
  httr2::local_mocked_responses(function(req) json_response(fx("usgs_empty.json")))
  expect_null(fetch_precip_latest(days = 1))
  expect_null(fetch_precip_daily(start_date = "1990-01-01", end_date = "1990-01-31"))
})

test_that("HTTP 500 returns NULL", {
  httr2::local_mocked_responses(function(req) json_response("{}", status = 500L))
  expect_null(fetch_precip_latest(days = 1))
  expect_null(fetch_precip_daily(start_date = "2026-09-01", end_date = "2026-09-15"))
})

test_that("a network error returns NULL", {
  httr2::local_mocked_responses(function(req) stop("simulated network error"))
  expect_null(fetch_precip_latest(days = 1))
  expect_null(fetch_precip_daily(start_date = "2026-09-01", end_date = "2026-09-15"))
})

test_that("malformed JSON or missing fields return NULL", {
  httr2::local_mocked_responses(function(req) json_response("<<<"))
  expect_null(fetch_precip_daily(start_date = "2026-09-01", end_date = "2026-09-15"))
  httr2::local_mocked_responses(function(req) json_response('{"features":[{"properties":{"time":"2026-09-01"}}]}'))
  expect_null(fetch_precip_latest(days = 1))
})
