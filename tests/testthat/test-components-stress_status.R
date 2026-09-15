# tests/testthat/test-components-stress_status.R
#
# stress_status(score) thresholds from Source A's R/components.R:
#   score < 35 -> low, < 55 -> moderate, < 72 -> high, else critical.
#
# NA behavior (documented, not changed): stress_status() uses a bare
# `if (score < 35)`, so NA / NULL / zero-length input throws an error
# ("missing value where TRUE/FALSE needed" / "argument is of length zero").
# Callers must guard before calling it. The stress score itself is a
# disclosed placeholder (TODO(stress-score)).

expect_stress <- function(score, label, class) {
  st <- stress_status(score)
  expect_type(st, "list")
  expect_named(st, c("label", "class"))
  expect_equal(st$label, label, info = paste("score", score))
  expect_equal(st$class, class, info = paste("score", score))
}

test_that("low stress below 35", {
  expect_stress(0, "Low Watershed Stress", "stress-low")
  expect_stress(34, "Low Watershed Stress", "stress-low")
  expect_stress(34.999, "Low Watershed Stress", "stress-low")
  expect_stress(-5, "Low Watershed Stress", "stress-low")
})

test_that("35 is the moderate boundary", {
  expect_stress(35, "Moderate Watershed Stress", "stress-moderate")
  expect_stress(35.001, "Moderate Watershed Stress", "stress-moderate")
  expect_stress(54.999, "Moderate Watershed Stress", "stress-moderate")
})

test_that("55 is the high boundary", {
  expect_stress(55, "High Watershed Stress", "stress-high")
  expect_stress(55.001, "High Watershed Stress", "stress-high")
  expect_stress(71.999, "High Watershed Stress", "stress-high")
})

test_that("72 is the critical boundary", {
  expect_stress(72, "Critical Watershed Stress", "stress-critical")
  expect_stress(72.001, "Critical Watershed Stress", "stress-critical")
  expect_stress(100, "Critical Watershed Stress", "stress-critical")
})

test_that("integer scores behave like doubles", {
  expect_stress(35L, "Moderate Watershed Stress", "stress-moderate")
  expect_stress(72L, "Critical Watershed Stress", "stress-critical")
})

test_that("NA / NULL / zero-length scores throw (current, documented behavior)", {
  expect_error(stress_status(NA), "missing value")
  expect_error(stress_status(NA_real_), "missing value")
  expect_error(stress_status(NULL), "length zero")
  expect_error(stress_status(numeric(0)), "length zero")
})

# Note: the stress-* classes returned here have no rule in www/styles.css or
# www/dashboard_new.css, and Source A's app.R never applies status$class
# (only status$label is rendered). This is inherited Source A behavior, so no
# CSS assertion is made; the four classes are pinned as a stable contract.
test_that("stress classes are the four documented values", {
  classes <- vapply(c(0, 40, 60, 90), function(s) stress_status(s)$class, "")
  expect_equal(
    unname(classes),
    c("stress-low", "stress-moderate", "stress-high", "stress-critical")
  )
})
