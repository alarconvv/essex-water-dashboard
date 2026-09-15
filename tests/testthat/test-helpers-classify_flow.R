# tests/testthat/test-helpers-classify_flow.R
#
# Boundary conditions, NA handling, and NULL/zero-length-arg safety for
# classify_flow(). Percentile thresholds: p10=10, p25=25, p75=75, p90=90.

test_that("classify_flow buckets exact boundary values correctly", {
  expect_equal(classify_flow(5, 10, 25, 75, 90), "Much below normal")
  expect_equal(classify_flow(10, 10, 25, 75, 90), "Below normal")
  expect_equal(classify_flow(20, 10, 25, 75, 90), "Below normal")
  expect_equal(classify_flow(25, 10, 25, 75, 90), "Normal")
  expect_equal(classify_flow(50, 10, 25, 75, 90), "Normal")
  expect_equal(classify_flow(75, 10, 25, 75, 90), "Normal")
  expect_equal(classify_flow(80, 10, 25, 75, 90), "Above normal")
  expect_equal(classify_flow(90, 10, 25, 75, 90), "Above normal")
  expect_equal(classify_flow(95, 10, 25, 75, 90), "Much above normal")
})

test_that("classify_flow handles values just either side of each boundary", {
  expect_equal(classify_flow(9.999, 10, 25, 75, 90), "Much below normal")
  expect_equal(classify_flow(24.999, 10, 25, 75, 90), "Below normal")
  expect_equal(classify_flow(75.001, 10, 25, 75, 90), "Above normal")
  expect_equal(classify_flow(90.001, 10, 25, 75, 90), "Much above normal")
})

test_that("classify_flow returns Unknown when any argument is NA", {
  expect_equal(classify_flow(NA, 10, 25, 75, 90), "Unknown")
  expect_equal(classify_flow(50, NA, 25, 75, 90), "Unknown")
  expect_equal(classify_flow(50, 10, NA, 75, 90), "Unknown")
  expect_equal(classify_flow(50, 10, 25, NA, 90), "Unknown")
  expect_equal(classify_flow(50, 10, 25, 75, NA), "Unknown")
  expect_equal(classify_flow(NA, NA, NA, NA, NA), "Unknown")
})

test_that("classify_flow is safe against NULL arguments", {
  expect_equal(classify_flow(5, NULL, 20, 50, 80), "Unknown")
  expect_equal(classify_flow(NULL, 10, 25, 75, 90), "Unknown")
  expect_equal(classify_flow(5, 10, 25, 75, NULL), "Unknown")
})

test_that("classify_flow is safe against zero-length vector arguments", {
  expect_equal(classify_flow(5, numeric(0), 20, 50, 80), "Unknown")
  expect_equal(classify_flow(numeric(0), 10, 25, 75, 90), "Unknown")
  expect_equal(classify_flow(5, 10, 25, 75, numeric(0)), "Unknown")
  expect_equal(
    classify_flow(numeric(0), numeric(0), numeric(0), numeric(0), numeric(0)),
    "Unknown"
  )
})

test_that("classify_flow never throws on NULL/zero-length input", {
  expect_no_error(classify_flow(5, NULL, 20, 50, 80))
  expect_no_error(classify_flow(5, numeric(0), 20, 50, 80))
  expect_no_error(classify_flow(NULL, NULL, NULL, NULL, NULL))
})

test_that("classify_flow handles typical Parker River values (cfs)", {
  expect_equal(classify_flow(2.1, 3.5, 5.0, 15.0, 25.0), "Much below normal")
  expect_equal(classify_flow(30.0, 3.5, 5.0, 15.0, 25.0), "Much above normal")
  expect_equal(classify_flow(10.0, 3.5, 5.0, 15.0, 25.0), "Normal")
})
