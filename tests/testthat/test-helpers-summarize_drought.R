# tests/testthat/test-helpers-summarize_drought.R
#
# summarize_drought() operates on a data.frame shaped like the
# drought_status table: fips, map_date, d0, d1, d2, d3, d4.

drought_row <- function(map_date = "2026-09-01", d0 = 0, d1 = 0, d2 = 0,
                        d3 = 0, d4 = 0) {
  data.frame(
    fips = "25009", map_date = map_date,
    d0 = d0, d1 = d1, d2 = d2, d3 = d3, d4 = d4,
    stringsAsFactors = FALSE
  )
}

test_that("summarize_drought returns Unavailable for NULL input", {
  expect_equal(summarize_drought(NULL), "Unavailable")
})

test_that("summarize_drought returns Unavailable for zero-row data frame", {
  empty <- data.frame(
    fips = character(0), map_date = character(0),
    d0 = numeric(0), d1 = numeric(0), d2 = numeric(0),
    d3 = numeric(0), d4 = numeric(0),
    stringsAsFactors = FALSE
  )
  expect_equal(summarize_drought(empty), "Unavailable")
})

test_that("summarize_drought returns No drought when all categories are zero", {
  expect_equal(summarize_drought(drought_row()), "No drought (D0-D4)")
})

test_that("summarize_drought returns No drought when all categories are NA", {
  dr <- drought_row(d0 = NA_real_, d1 = NA_real_, d2 = NA_real_,
                    d3 = NA_real_, d4 = NA_real_)
  expect_equal(summarize_drought(dr), "No drought (D0-D4)")
})

test_that("summarize_drought prioritizes D4 over lower categories", {
  dr <- drought_row(d0 = 100, d1 = 80, d2 = 50, d3 = 20, d4 = 5)
  expect_equal(summarize_drought(dr), "D4 active")
})

test_that("summarize_drought falls through to the highest active category", {
  expect_equal(summarize_drought(drought_row(d0 = 60, d1 = 15)), "D1 active")
  expect_equal(summarize_drought(drought_row(d0 = 60, d2 = 1)), "D2 active")
  expect_equal(summarize_drought(drought_row(d3 = 0.1)), "D3 active")
  expect_equal(summarize_drought(drought_row(d0 = 25)), "D0 active")
})

test_that("summarize_drought treats NA cells as not-active and skips them", {
  dr <- drought_row(d0 = 10, d1 = NA_real_, d2 = NA_real_, d3 = NA_real_,
                    d4 = NA_real_)
  expect_equal(summarize_drought(dr), "D0 active")
})

test_that("summarize_drought uses the LAST row by map_date, even if unsorted", {
  dr <- rbind(
    drought_row("2026-08-15", 90, 70, 40, 20, 10),
    drought_row("2026-09-01")
  )
  expect_equal(summarize_drought(dr), "No drought (D0-D4)")
  expect_equal(summarize_drought(dr[2:1, ]), "No drought (D0-D4)")
})

test_that("summarize_drought picks up an increase in the most recent row", {
  dr <- rbind(
    drought_row("2026-08-15"),
    drought_row("2026-09-01", 90, 70, 40, 20, 10)
  )
  expect_equal(summarize_drought(dr), "D4 active")
})

test_that("summarize_drought sorts real Date columns correctly", {
  dr <- rbind(
    drought_row(as.Date("2026-09-08"), d0 = 30),
    drought_row(as.Date("2026-08-25"), d2 = 50)
  )
  expect_equal(summarize_drought(dr), "D0 active")
})
