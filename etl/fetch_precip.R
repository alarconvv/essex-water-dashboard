# etl/fetch_precip.R
#
# Precipitation (parameter 00045, inches) for the co-located Byfield gauge
# USGS-424510070564401 from the USGS OGC API via httr2. No API key.
#
# statistic_id verification (live, 2026-09-14/15):
#   - `daily` with statistic_id=00006 (sum) returns data; a daily request
#     with no statistic filter returns only 00006 rows, so 00006 is the one
#     statistic served for this gauge.
#   - Values are plausible daily totals in inches, not mean-style numbers:
#     2026-09-13 = 1.07, 2026-05-30 = 1.90, 2026-07-07 = 1.12; Jan 1 - Sep 13
#     2026 sums to 25.24 in over 252 reported days; most days are 0.00.
#   - Record starts 2025-06-28 (daily) / 2025-06-27 (continuous). Requests
#     before that return a valid, empty FeatureCollection.
#   - `continuous` values are 15-minute increments (statistic_id null).
#
# Requires etl/constants.R and etl/http_common.R.

#' Recent 15-minute precipitation increments for the gauge.
#'
#' @param site_id OGC monitoring_location_id (default PRECIP_SITE).
#' @param days Trailing days to request.
#' @param end_time End of the window (POSIXct; default now).
#' @return data.frame(site_no, datetime chr UTC, precip_in num (increment
#'   for that interval), approval_status, qualifier), sorted, or NULL.
fetch_precip_latest <- function(site_id = PRECIP_SITE, days = 30, end_time = Sys.time()) {
  .null_on_failure(
    paste0("precip_latest ", site_id),
    ogc_fetch_continuous(site_id, PARAM_PRECIP, days, end_time, "precip_in")
  )
}

#' Daily precipitation totals (statistic 00006, sum) over a date range.
#'
#' @param site_id OGC monitoring_location_id (default PRECIP_SITE).
#' @param start_date,end_date Date or "YYYY-MM-DD" (inclusive).
#' @return data.frame(site_no, date chr "YYYY-MM-DD", precip_in num (daily
#'   total), approval_status, qualifier), sorted, or NULL.
fetch_precip_daily <- function(site_id = PRECIP_SITE, start_date, end_date) {
  .null_on_failure(
    paste0("precip_daily ", site_id),
    ogc_fetch_daily(site_id, PARAM_PRECIP, STAT_SUM, start_date, end_date, "precip_in")
  )
}
