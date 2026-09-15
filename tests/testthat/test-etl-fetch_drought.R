# tests/testthat/test-etl-fetch_drought.R
#
# U.S. Drought Monitor fetcher (httr2). Regression suite for the real bug:
# without `Accept: application/json` the endpoint answers 200 text/csv, and
# parsing that as JSON throws. httr2 is mocked; zero live network calls.
source(testthat::test_path("..", "..", "etl", "http_common.R"))
source(testthat::test_path("..", "..", "etl", "fetch_drought.R"))

fx <- function(name) paste(readLines(testthat::test_path("fixture-data", name), warn = FALSE), collapse = "\n")
text_response <- function(txt, status = 200L, ctype = "application/json; charset=utf-8") {
  httr2::response(status_code = status, headers = list(`Content-Type` = ctype), body = charToRaw(txt))
}

json_txt <- fx("usdm_drought_success.json")
csv_txt <- fx("usdm_drought_csv_default.txt")
raw_json <- jsonlite::fromJSON(json_txt)

test_that("fetch_drought_status parses the JSON fixture (lowercase d0-d4, camelCase mapDate)", {
  httr2::local_mocked_responses(function(req) text_response(json_txt))
  res <- fetch_drought_status("25009", days_back = 14, today = as.Date("2026-09-15"))

  expect_s3_class(res, "data.frame")
  expect_named(res, c("fips", "map_date", "d0", "d1", "d2", "d3", "d4"))
  expect_equal(nrow(res), nrow(raw_json))
  expect_true(all(res$fips == "25009"))
  expect_true(all(grepl("^\\d{4}-\\d{2}-\\d{2}$", res$map_date)))
  expect_false(is.unsorted(res$map_date))
  ord <- order(substr(raw_json$mapDate, 1, 10))
  expect_equal(res$d0, as.numeric(raw_json$d0[ord]))
  expect_equal(res$d1, as.numeric(raw_json$d1[ord]))
  expect_equal(res$d4, as.numeric(raw_json$d4[ord]))
})

test_that("the request sends Accept: application/json, a 10 s timeout, and the county/date query", {
  seen <- new.env()
  httr2::local_mocked_responses(function(req) {
    seen$req <- req
    text_response(json_txt)
  })
  fetch_drought_status("25009", days_back = 14, today = as.Date("2026-09-15"))

  expect_equal(seen$req$headers$Accept, "application/json")
  expect_equal(seen$req$options$timeout_ms, 10000)
  q <- httr2::url_parse(seen$req$url)$query
  expect_equal(q$aoi, "25009")
  expect_equal(q$startdate, "09/01/2026")
  expect_equal(q$enddate, "09/15/2026")
  expect_equal(q$statisticsType, "1")
})

test_that("start_date overrides days_back for a multi-year backfill window", {
  seen <- new.env()
  httr2::local_mocked_responses(function(req) {
    seen$req <- req
    text_response(json_txt)
  })
  res <- fetch_drought_status("25009", today = as.Date("2026-09-15"), start_date = "2023-09-15")
  expect_s3_class(res, "data.frame")
  q <- httr2::url_parse(seen$req$url)$query
  expect_equal(q$startdate, "09/15/2023")
  expect_equal(q$enddate, "09/15/2026")
  expect_equal(seen$req$options$timeout_ms, 10000)
})

test_that("a start_date after today returns NULL without making a request", {
  called <- FALSE
  httr2::local_mocked_responses(function(req) {
    called <<- TRUE
    text_response(json_txt)
  })
  expect_null(fetch_drought_status("25009", today = as.Date("2026-09-15"), start_date = "2026-09-16"))
  expect_false(called)
  expect_match(etl_last_failure(), "invalid drought window")
})

test_that("the API's HTTP 400 for an inverted range returns NULL", {
  httr2::local_mocked_responses(function(req) {
    text_response('"end date is greater than start date."', status = 400L)
  })
  expect_null(fetch_drought_status("25009", start_date = "2026-01-01", today = as.Date("2026-09-15")))
  expect_match(etl_last_failure(), "HTTP 400")
})

test_that("a CSV response (content-type text/csv) returns NULL instead of a fromJSON crash", {
  # The fixture is the endpoint's real default response without the Accept
  # header; prove it really would crash a naive parser.
  expect_error(jsonlite::fromJSON(csv_txt))
  httr2::local_mocked_responses(function(req) text_response(csv_txt, ctype = "text/csv; charset=utf-8"))
  expect_no_error(res <- fetch_drought_status("25009"))
  expect_null(res)
  expect_match(etl_last_failure(), "content type")
})

test_that("an empty JSON array returns NULL", {
  httr2::local_mocked_responses(function(req) text_response("[]"))
  expect_null(fetch_drought_status("25009"))
})

test_that("HTTP 500 returns NULL", {
  httr2::local_mocked_responses(function(req) text_response("oops", status = 500L, ctype = "text/plain"))
  expect_null(fetch_drought_status("25009"))
  expect_match(etl_last_failure(), "HTTP 500")
})

test_that("a network error returns NULL and does not propagate", {
  httr2::local_mocked_responses(function(req) stop("simulated network error"))
  expect_no_error(res <- fetch_drought_status("25009"))
  expect_null(res)
})

test_that("malformed JSON or missing fields return NULL", {
  httr2::local_mocked_responses(function(req) text_response('[{"mapDate": '))
  expect_null(fetch_drought_status("25009"))

  httr2::local_mocked_responses(function(req) {
    text_response('[{"MapDate":"2026-09-08T00:00:00","FIPS":"25009","D0":100}]')
  })
  expect_null(fetch_drought_status("25009"))
  expect_match(etl_last_failure(), "missing expected fields")
})
