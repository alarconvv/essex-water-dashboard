# tests/testthat/test-helpers-format_last_updated.R

test_that("POSIXct is formatted in the display time zone", {
  ts <- as.POSIXct("2026-09-14 19:05:00", tz = "UTC")
  expect_equal(format_last_updated(ts), "Updated Sep 14, 2026 3:05 PM")
  expect_equal(format_last_updated(ts, tz = "UTC"), "Updated Sep 14, 2026 7:05 PM")
})

test_that("ISO-8601 strings with Z or offsets are parsed", {
  expect_equal(
    format_last_updated("2026-09-14T19:05:00Z"),
    "Updated Sep 14, 2026 3:05 PM"
  )
  expect_equal(
    format_last_updated("2026-09-14T15:05:00-04:00"),
    "Updated Sep 14, 2026 3:05 PM"
  )
  expect_equal(
    format_last_updated("2026-09-14T21:05:00+0200", tz = "UTC"),
    "Updated Sep 14, 2026 7:05 PM"
  )
  expect_equal(
    format_last_updated("2026-09-14T19:05:00.123Z", tz = "UTC"),
    "Updated Sep 14, 2026 7:05 PM"
  )
})

test_that("strings without an offset are interpreted as UTC", {
  expect_equal(
    format_last_updated("2026-09-14 19:05:00"),
    "Updated Sep 14, 2026 3:05 PM"
  )
  expect_equal(
    format_last_updated("2026-09-14 19:05", tz = "UTC"),
    "Updated Sep 14, 2026 7:05 PM"
  )
})

test_that("midnight and noon use 12-hour clock correctly", {
  expect_equal(
    format_last_updated("2026-01-05T00:07:00Z", tz = "UTC"),
    "Updated Jan 5, 2026 12:07 AM"
  )
  expect_equal(
    format_last_updated("2026-01-05T12:00:00Z", tz = "UTC"),
    "Updated Jan 5, 2026 12:00 PM"
  )
})

test_that("numeric epoch seconds are accepted", {
  ts <- as.numeric(as.POSIXct("2026-09-14 19:05:00", tz = "UTC"))
  expect_equal(format_last_updated(ts, tz = "UTC"), "Updated Sep 14, 2026 7:05 PM")
})

test_that("Date and date-only strings omit the time", {
  expect_equal(format_last_updated(as.Date("2026-09-04")), "Updated Sep 4, 2026")
  expect_equal(format_last_updated("2026-09-04"), "Updated Sep 4, 2026")
})

test_that("missing or unparseable input -> 'Not yet updated', never throws", {
  bad <- list(
    NULL, NA, NA_character_, NA_real_, character(0), "", "   ",
    "not a date", "2026-13-45T99:99:99Z", "14/09/2026", Inf,
    as.POSIXct(NA), as.Date(NA), list("2026-09-14"), TRUE
  )
  for (x in bad) {
    expect_no_error(out <- format_last_updated(x))
    expect_equal(out, "Not yet updated")
  }
})

test_that("an invalid display time zone degrades instead of throwing", {
  expect_no_error(out <- format_last_updated("2026-09-14T19:05:00Z", tz = "Not/AZone"))
  expect_type(out, "character")
  expect_length(out, 1)
})

test_that("only the first element of a vector is used", {
  expect_equal(
    format_last_updated(c("2026-09-14T19:05:00Z", "2020-01-01T00:00:00Z"), tz = "UTC"),
    "Updated Sep 14, 2026 7:05 PM"
  )
})
