# tests/testthat/test-etl-stats.R
#
# Pure same-day statistics (etl/stats.R). Expected quantiles are computed by
# hand with R's default type-7 rule: for sorted x[1..n], h = (n - 1) * p + 1,
# q = x[floor(h)] + (h - floor(h)) * (x[floor(h) + 1] - x[floor(h)]).
source(testthat::test_path("..", "..", "etl", "stats.R"))

# 5 usable prior years on 06-15: 1, 2, 3, 4, 5 (after the rules below).
synthetic <- data.frame(
  date = c(
    "2016-06-15", "2017-06-15", "2017-06-15", "2018-06-15", "2018-06-15",
    "2019-06-15", "2020-06-15", "2021-06-15", "2016-06-16"
  ),
  value = c(1, 2, 50, NA, 3, 4, 5, 100, 999),
  stringsAsFactors = FALSE
)

test_that("compute_same_day_stats matches hand-computed type-7 quantiles", {
  r <- compute_same_day_stats(synthetic, as.Date("2021-06-15"))
  expect_named(r, c("p10", "p25", "p50", "p75", "p90", "years_used"))
  expect_equal(nrow(r), 1)
  # n = 5: p10 h = 1.4 -> 1 + 0.4 * 1 = 1.4; p90 h = 4.6 -> 4 + 0.6 * 1 = 4.6
  expect_equal(r$p10, 1.4)
  expect_equal(r$p25, 2)
  expect_equal(r$p50, 3)
  expect_equal(r$p75, 4)
  expect_equal(r$p90, 4.6)
  expect_equal(r$years_used, 5L)
})

test_that("the reference year and later years are excluded", {
  # 100 is the 2021 value; it must not appear.
  r <- compute_same_day_stats(synthetic, as.Date("2021-06-15"))
  expect_lt(r$p90, 100)
  # A reference in 2019 only sees 2016-2018 -> 1, 2, 3.
  r2019 <- compute_same_day_stats(synthetic, "2019-06-15")
  expect_equal(r2019$years_used, 3L)
  expect_equal(r2019$p50, 2)
})

test_that("one value per year per month-day: the first finite value after sorting wins", {
  # 2017 has 2 then 50 -> 2 is used; 2018 has NA then 3 -> 3 is used.
  r <- compute_same_day_stats(synthetic, "2021-06-15", probs = c(0, 1))
  expect_equal(r$p0, 1)
  expect_equal(r$p100, 5)
  expect_equal(r$years_used, 5L)
})

test_that("NA, NaN, Inf and unparseable values/dates are ignored", {
  df <- data.frame(
    date = c("2010-01-05", "2011-01-05", "2012-01-05", "2013-01-05", "not-a-date", "2014-01-05"),
    value = c("2", NA, "NaN", "Inf", "7", "4"),
    stringsAsFactors = FALSE
  )
  r <- compute_same_day_stats(df, as.Date("2020-01-05"))
  expect_equal(r$years_used, 2L)
  expect_equal(r$p50, 3)
})

test_that("Feb 29 uses only leap-year Feb 29 values", {
  df <- data.frame(
    date = as.Date(c("2016-02-29", "2020-02-29", "2019-02-28", "2019-03-01", "2021-02-28")),
    value = c(10, 20, 99, 99, 99)
  )
  r <- compute_same_day_stats(df, as.Date("2024-02-29"))
  expect_equal(r$years_used, 2L)
  expect_equal(r$p50, 15)
  expect_equal(r$p10, 11) # h = 1.1 -> 10 + 0.1 * 10
})

test_that("no prior years, empty input, or a bad reference date give NA with years_used 0", {
  none <- compute_same_day_stats(synthetic, as.Date("2016-06-15"))
  expect_true(all(is.na(unlist(none[, c("p10", "p25", "p50", "p75", "p90")]))))
  expect_equal(none$years_used, 0L)

  expect_equal(compute_same_day_stats(NULL, Sys.Date())$years_used, 0L)
  expect_equal(compute_same_day_stats(data.frame(), Sys.Date())$years_used, 0L)
  expect_equal(compute_same_day_stats(data.frame(x = 1), Sys.Date())$years_used, 0L)
  bad_ref <- compute_same_day_stats(synthetic, NA)
  expect_true(is.na(bad_ref$p50))
  expect_equal(bad_ref$years_used, 0L)
})

test_that("build_percentile_table returns 366 calendar-ordered rows consistent with compute_same_day_stats", {
  df <- rbind(
    synthetic,
    data.frame(date = c("2016-02-29", "2020-02-29"), value = c(10, 20), stringsAsFactors = FALSE)
  )
  tab <- build_percentile_table(df, reference_year = 2021)
  expect_equal(nrow(tab), 366)
  expect_named(tab, c("month_nu", "day_nu", "p10", "p25", "p50", "p75", "p90", "years_used"))
  expect_equal(unlist(tab[1, c("month_nu", "day_nu")]), c(month_nu = 1, day_nu = 1))
  expect_equal(unlist(tab[366, c("month_nu", "day_nu")]), c(month_nu = 12, day_nu = 31))
  expect_false(anyDuplicated(tab[, c("month_nu", "day_nu")]) > 0)

  jun15 <- tab[tab$month_nu == 6 & tab$day_nu == 15, ]
  single <- compute_same_day_stats(df, as.Date("2021-06-15"))
  expect_equal(jun15$p10, single$p10)
  expect_equal(jun15$p90, single$p90)
  expect_equal(jun15$years_used, single$years_used)

  feb29 <- tab[tab$month_nu == 2 & tab$day_nu == 29, ]
  expect_equal(feb29$years_used, 2L)
  expect_equal(feb29$p50, 15)

  jun16 <- tab[tab$month_nu == 6 & tab$day_nu == 16, ]
  expect_equal(jun16$years_used, 1L)
  expect_equal(jun16$p50, 999)

  empty_day <- tab[tab$month_nu == 1 & tab$day_nu == 1, ]
  expect_equal(empty_day$years_used, 0L)
  expect_true(is.na(empty_day$p50))
})

test_that("build_percentile_table on empty input is 366 NA rows", {
  tab <- build_percentile_table(NULL, reference_year = 2026)
  expect_equal(nrow(tab), 366)
  expect_true(all(tab$years_used == 0L))
  expect_true(all(is.na(tab$p50)))
})

test_that("compute_precip_typical is the median same-day total", {
  df <- data.frame(
    date = c("2022-09-13", "2023-09-13", "2024-09-13", "2025-09-13", "2026-09-13"),
    value = c(0, 0.5, 0, 1.2, 3)
  )
  r <- compute_precip_typical(df, as.Date("2026-09-13"))
  expect_named(r, c("median_in", "years_used"))
  expect_equal(r$median_in, 0.25) # median of 0, 0, 0.5, 1.2
  expect_equal(r$years_used, 4L)

  none <- compute_precip_typical(df, as.Date("2022-09-13"))
  expect_true(is.na(none$median_in))
  expect_equal(none$years_used, 0L)
})

test_that("build_precip_typical_table has 366 rows and matches compute_precip_typical", {
  df <- data.frame(date = c("2024-07-04", "2025-07-04"), value = c(0.2, 0.6))
  tab <- build_precip_typical_table(df, reference_year = 2026)
  expect_equal(nrow(tab), 366)
  expect_named(tab, c("month_nu", "day_nu", "median_in", "years_used"))
  jul4 <- tab[tab$month_nu == 7 & tab$day_nu == 4, ]
  expect_equal(jul4$median_in, compute_precip_typical(df, as.Date("2026-07-04"))$median_in)
  expect_equal(jul4$median_in, 0.4)
  expect_equal(jul4$years_used, 2L)
})
