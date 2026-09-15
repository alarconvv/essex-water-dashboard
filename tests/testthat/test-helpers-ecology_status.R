# tests/testthat/test-helpers-ecology_status.R
#
# ecology_status(flow_cfs, threshold_cfs) drives the ecology condition card:
# latest Parker River flow vs the provisional eco_flow_threshold_cfs.

expect_shape <- function(st) {
  expect_type(st, "list")
  expect_named(st, c("state", "label", "badge_class", "comparison"))
  for (nm in names(st)) {
    expect_type(st[[nm]], "character")
    expect_length(st[[nm]], 1)
  }
}

test_that("flow above threshold -> above / badge-good", {
  st <- ecology_status(12.4, 8)
  expect_shape(st)
  expect_equal(st$state, "above")
  expect_equal(st$label, "Above eco-flow threshold")
  expect_equal(st$badge_class, "badge-good")
  expect_equal(st$comparison, "4.4 cfs above 8 cfs threshold (provisional)")
})

test_that("flow below threshold -> at_or_below / badge-critical", {
  st <- ecology_status(5, 8)
  expect_shape(st)
  expect_equal(st$state, "at_or_below")
  expect_equal(st$label, "At or below eco-flow threshold")
  expect_equal(st$badge_class, "badge-critical")
  expect_equal(st$comparison, "3 cfs below 8 cfs threshold (provisional)")
})

test_that("flow exactly at threshold counts as at_or_below", {
  st <- ecology_status(8, 8)
  expect_shape(st)
  expect_equal(st$state, "at_or_below")
  expect_equal(st$badge_class, "badge-critical")
  expect_equal(st$comparison, "At the 8 cfs threshold (provisional)")
  # Integer threshold (as yaml parses `8`) vs double flow.
  expect_equal(ecology_status(8.0, 8L)$state, "at_or_below")
})

test_that("values just either side of the threshold", {
  expect_equal(ecology_status(8.01, 8)$state, "above")
  expect_equal(ecology_status(7.99, 8)$state, "at_or_below")
})

test_that("zero flow is a real reading, not missing", {
  expect_equal(ecology_status(0, 8)$state, "at_or_below")
  expect_equal(ecology_status(1, 0)$state, "above")
})

test_that("large values are formatted with a thousands separator", {
  expect_equal(
    ecology_status(1508, 8)$comparison,
    "1,500 cfs above 8 cfs threshold (provisional)"
  )
})

test_that("NA / NULL / zero-length flow -> unavailable / badge-warning", {
  for (flow in list(NA, NA_real_, NULL, numeric(0))) {
    st <- ecology_status(flow, 8)
    expect_shape(st)
    expect_equal(st$state, "unavailable")
    expect_equal(st$label, "Comparison unavailable")
    expect_equal(st$badge_class, "badge-warning")
    expect_equal(st$comparison, "Live flow unavailable")
  }
})

test_that("NA / NULL / zero-length threshold -> unavailable / badge-warning", {
  for (thr in list(NA, NA_real_, NULL, numeric(0))) {
    st <- ecology_status(10, thr)
    expect_shape(st)
    expect_equal(st$state, "unavailable")
    expect_equal(st$badge_class, "badge-warning")
    expect_equal(st$comparison, "Eco-flow threshold not configured")
  }
})

test_that("both missing -> unavailable with combined message", {
  st <- ecology_status(NULL, NA)
  expect_equal(st$state, "unavailable")
  expect_equal(st$comparison, "Live flow and eco-flow threshold unavailable")
})

test_that("non-numeric / non-finite inputs are treated as missing, never thrown", {
  expect_no_warning(st <- ecology_status(10, "eight"))
  expect_equal(st$state, "unavailable")
  expect_equal(ecology_status(Inf, 8)$state, "unavailable")
  expect_equal(ecology_status(NaN, 8)$state, "unavailable")
  expect_equal(ecology_status(10, list())$state, "unavailable")
  expect_equal(ecology_status(10, list("eight"))$state, "unavailable")
  expect_equal(ecology_status(TRUE, 8)$state, "unavailable")
  # A numeric string (hand-edited config) is still usable.
  expect_equal(ecology_status(10, "8")$state, "above")
})

test_that("the default config threshold (8 cfs) and a NA default both work", {
  expect_equal(ecology_status(9, .manual_content_defaults$eco_flow_threshold_cfs)$state,
               "unavailable")
  cfg <- load_manual_content(testthat::test_path("fixture-data", "config_yml_normal.yml"))
  expect_equal(ecology_status(9, cfg$eco_flow_threshold_cfs)$state, "above")
})

test_that("every badge class ecology_status can return exists in Source A CSS", {
  css_path <- testthat::test_path("..", "..", "www", "styles.css")
  skip_if_not(file.exists(css_path), "www/styles.css not present")
  css <- paste(readLines(css_path, warn = FALSE), collapse = "\n")
  classes <- unique(c(
    ecology_status(10, 8)$badge_class,
    ecology_status(5, 8)$badge_class,
    ecology_status(NA, 8)$badge_class
  ))
  expect_setequal(classes, c("badge-good", "badge-critical", "badge-warning"))
  for (cls in classes) {
    expect_true(
      grepl(paste0(".condition-badge.", cls), css, fixed = TRUE),
      info = cls
    )
  }
})
