# tests/testthat/test-etl-fetch_groundwater.R
#
# Groundwater (72019) fetchers against captured OGC fixtures, httr2 mocked.
source(testthat::test_path("..", "..", "etl", "http_common.R"))
source(testthat::test_path("..", "..", "etl", "fetch_groundwater.R"))

fx <- function(name) paste(readLines(testthat::test_path("fixture-data", name), warn = FALSE), collapse = "\n")
json_response <- function(txt, status = 200L, ctype = "application/json; charset=utf-8") {
  httr2::response(status_code = status, headers = list(`Content-Type` = ctype), body = charToRaw(txt))
}
fixture_props <- function(name) {
  p <- jsonlite::fromJSON(fx(name))$features$properties
  p[order(p$time), , drop = FALSE]
}
query_of <- function(req) httr2::url_parse(req$url)$query

test_that("fetch_groundwater_latest parses the continuous fixture", {
  seen <- new.env()
  httr2::local_mocked_responses(function(req) {
    seen$req <- req
    json_response(fx("usgs_groundwater_latest.json"))
  })
  res <- fetch_groundwater_latest(days = 1)
  expected <- fixture_props("usgs_groundwater_latest.json")

  expect_named(res, c("site_no", "datetime", "depth_ft", "approval_status", "qualifier"))
  expect_equal(nrow(res), nrow(expected))
  expect_true(all(res$site_no == GW_SITE))
  expect_equal(res$depth_ft, as.numeric(expected$value))
  expect_equal(query_of(seen$req)$parameter_code, "72019")
  expect_equal(query_of(seen$req)$monitoring_location_id, GW_SITE)
  expect_match(seen$req$url, "/collections/continuous/items", fixed = TRUE)
  expect_equal(seen$req$options$timeout_ms, HTTP_TIMEOUT_LATEST * 1000)
})

test_that("fetch_groundwater_daily parses the daily fixture with statistic 00003", {
  seen <- new.env()
  httr2::local_mocked_responses(function(req) {
    seen$req <- req
    json_response(fx("usgs_groundwater_daily.json"))
  })
  res <- fetch_groundwater_daily(start_date = "2026-09-01", end_date = "2026-09-15")
  expected <- fixture_props("usgs_groundwater_daily.json")

  expect_named(res, c("site_no", "date", "depth_ft", "approval_status", "qualifier"))
  expect_equal(res$date, expected$time)
  expect_equal(res$depth_ft, as.numeric(expected$value))
  expect_true(all(res$depth_ft > 0 & res$depth_ft < 100)) # plausible depth to water in ft
  expect_equal(query_of(seen$req)$statistic_id, "00003")
  expect_equal(query_of(seen$req)$parameter_code, "72019")
})

test_that("empty features return NULL", {
  httr2::local_mocked_responses(function(req) json_response(fx("usgs_empty.json")))
  expect_null(fetch_groundwater_latest(days = 1))
  expect_null(fetch_groundwater_daily(start_date = "2026-09-01", end_date = "2026-09-15"))
})

test_that("HTTP 500 returns NULL", {
  httr2::local_mocked_responses(function(req) json_response("{}", status = 500L))
  expect_null(fetch_groundwater_latest(days = 1))
  expect_null(fetch_groundwater_daily(start_date = "2026-09-01", end_date = "2026-09-15"))
})

test_that("a network error returns NULL", {
  httr2::local_mocked_responses(function(req) stop("simulated network error"))
  expect_null(fetch_groundwater_latest(days = 1))
  expect_null(fetch_groundwater_daily(start_date = "2026-09-01", end_date = "2026-09-15"))
})

test_that("malformed JSON or missing fields return NULL", {
  httr2::local_mocked_responses(function(req) json_response("[1, 2"))
  expect_null(fetch_groundwater_latest(days = 1))
  httr2::local_mocked_responses(function(req) {
    json_response('{"features":[{"properties":{"value":"10.2"}}]}')
  })
  expect_null(fetch_groundwater_daily(start_date = "2026-09-01", end_date = "2026-09-15"))
  expect_match(etl_last_failure(), "missing required fields")
})
