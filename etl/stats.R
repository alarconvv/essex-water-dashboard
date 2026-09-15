# etl/stats.R
#
# Pure statistics (no I/O, no network, no database) used by the ETL to turn
# stored daily values into same-day-of-year reference statistics.
#
# Replaces Source A's heuristic band (p10 = median * 0.52, p90 = median *
# 1.55) with real empirical quantiles of the same month-day across prior
# calendar years.
#
# Rules shared by every function here:
#   - "Prior complete years" = calendar years strictly before the reference
#     year. The reference year itself (partial) is never used.
#   - One value per year per month-day. If a year has several rows for the
#     same month-day (e.g. overlapping page fetches), the first finite value
#     after sorting by date is used.
#   - Non-finite values (NA, NaN, Inf) are dropped before anything else.
#   - Feb 29 uses only values observed on Feb 29 (leap years); it is not
#     padded with Feb 28 or Mar 1.
#   - Quantiles use stats::quantile(type = 7), R's default (linear
#     interpolation between order statistics).
#   - With zero usable years every statistic is NA and years_used is 0.

#' Normalize a (date, value) data.frame: parse dates, coerce values, drop
#' non-finite values, add year and month-day keys.
#' @keywords internal
.prepare_daily_series <- function(df) {
  usable <- is.data.frame(df) && nrow(df) > 0 && all(c("date", "value") %in% names(df))
  if (!usable) {
    return(data.frame(
      date = as.Date(character(0)), value = numeric(0),
      year = integer(0), md = character(0), stringsAsFactors = FALSE
    ))
  }
  date <- suppressWarnings(as.Date(substr(as.character(df$date), 1, 10)))
  value <- suppressWarnings(as.numeric(df$value))
  keep <- !is.na(date) & is.finite(value)
  out <- data.frame(date = date[keep], value = value[keep], stringsAsFactors = FALSE)
  out <- out[order(out$date), , drop = FALSE]
  out$year <- as.integer(format(out$date, "%Y"))
  out$md <- format(out$date, "%m-%d")
  # One value per (year, month-day): keep the first after sorting by date.
  out[!duplicated(out[, c("year", "md")]), , drop = FALSE]
}

#' Column names for a probs vector, e.g. 0.1 -> "p10".
#' @keywords internal
.prob_names <- function(probs) {
  paste0("p", as.character(round(probs * 100, 4)))
}

#' Quantiles of a value vector as a one-row data.frame (NA when empty).
#' @keywords internal
.quantile_row <- function(values, probs) {
  if (length(values) == 0) {
    q <- rep(NA_real_, length(probs))
  } else {
    q <- unname(stats::quantile(values, probs = probs, type = 7, names = FALSE))
  }
  out <- as.data.frame(as.list(stats::setNames(q, .prob_names(probs))))
  out$years_used <- length(values)
  out
}

#' Same-day-of-year percentiles from prior complete years.
#'
#' @param df data.frame with columns `date` (Date or "YYYY-MM-DD") and
#'   `value` (numeric or numeric-like character).
#' @param reference_date Date (or "YYYY-MM-DD"); its month-day selects the
#'   values and its year excludes the current and later years.
#' @param probs Probabilities for stats::quantile().
#' @return One-row data.frame with columns p10, p25, p50, p75, p90 (named
#'   from `probs`) and years_used (integer). All statistics are NA and
#'   years_used is 0 when no prior-year value exists.
compute_same_day_stats <- function(df, reference_date,
                                   probs = c(0.1, 0.25, 0.5, 0.75, 0.9)) {
  ref <- suppressWarnings(as.Date(reference_date))
  if (length(ref) != 1 || is.na(ref)) {
    return(.quantile_row(numeric(0), probs))
  }
  s <- .prepare_daily_series(df)
  ref_year <- as.integer(format(ref, "%Y"))
  sel <- s$year < ref_year & s$md == format(ref, "%m-%d")
  .quantile_row(s$value[sel], probs)
}

#' All 366 month-day keys in calendar order (a leap-year calendar).
#' @keywords internal
.all_month_days <- function() {
  d <- seq(as.Date("2000-01-01"), as.Date("2000-12-31"), by = "day")
  data.frame(
    month_nu = as.integer(format(d, "%m")),
    day_nu = as.integer(format(d, "%d")),
    md = format(d, "%m-%d"),
    stringsAsFactors = FALSE
  )
}

#' Build the full 366-row month-day percentile table for one series.
#'
#' Equivalent to calling compute_same_day_stats() for every month-day with a
#' reference date in `reference_year`, but computed in one pass.
#'
#' @param df data.frame(date, value) of daily values for one site.
#' @param reference_year Integer; only years strictly before it are used.
#' @param probs Probabilities for stats::quantile().
#' @return data.frame with 366 rows and columns month_nu, day_nu, p10, p25,
#'   p50, p75, p90, years_used, ordered Jan 1 .. Dec 31 (Feb 29 included).
build_percentile_table <- function(df,
                                   reference_year = as.integer(format(Sys.Date(), "%Y")),
                                   probs = c(0.1, 0.25, 0.5, 0.75, 0.9)) {
  s <- .prepare_daily_series(df)
  s <- s[s$year < as.integer(reference_year), , drop = FALSE]
  by_md <- split(s$value, s$md)
  cal <- .all_month_days()
  rows <- lapply(cal$md, function(md) {
    v <- by_md[[md]]
    .quantile_row(if (is.null(v)) numeric(0) else v, probs)
  })
  cbind(cal[, c("month_nu", "day_nu")], do.call(rbind, rows))
}

#' Typical (median) same-day precipitation total from prior complete years.
#'
#' The input values are daily totals (USGS statistic 00006, inches). Note
#' that the median of a single day's total is 0 for most days of the year
#' in this climate; the value is honest but callers comparing multi-day
#' totals should aggregate the daily table rather than compare against it.
#'
#' @param df data.frame(date, value) of daily precipitation totals.
#' @param reference_date Date; month-day to evaluate, year to exclude.
#' @return One-row data.frame(median_in, years_used); median_in is NA and
#'   years_used 0 when no prior-year value exists.
compute_precip_typical <- function(df, reference_date) {
  r <- compute_same_day_stats(df, reference_date, probs = 0.5)
  data.frame(median_in = r$p50, years_used = r$years_used)
}

#' Build the 366-row typical-precipitation table for one gauge.
#'
#' @param df data.frame(date, value) of daily precipitation totals.
#' @param reference_year Integer; only years strictly before it are used.
#' @return data.frame(month_nu, day_nu, median_in, years_used), 366 rows.
build_precip_typical_table <- function(df,
                                       reference_year = as.integer(format(Sys.Date(), "%Y"))) {
  t <- build_percentile_table(df, reference_year = reference_year, probs = 0.5)
  data.frame(
    month_nu = t$month_nu, day_nu = t$day_nu,
    median_in = t$p50, years_used = t$years_used
  )
}
