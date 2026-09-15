# etl/fetch_groundwater.R
#
# Groundwater depth to water (parameter 72019, ft below land surface) for
# observation well USGS-424520070562401 from the USGS OGC API via httr2.
# Replaces Source A's get_usgs_groundwater() and the data pull inside
# get_groundwater_seasonal_median().
#
# Live check (2026-09-14): the `daily` collection serves only statistic
# 00003 (daily mean) for this well, record from 1984-10-17. Source A's
# request omitted statistic_id; we pin 00003 explicitly so a future extra
# statistic (e.g. daily max) can never be silently mixed in.
#
# Requires etl/constants.R and etl/http_common.R.

#' Recent 15-minute depth-to-water readings for a well.
#'
#' @param site_id OGC monitoring_location_id (default GW_SITE).
#' @param days Trailing days to request.
#' @param end_time End of the window (POSIXct; default now).
#' @return data.frame(site_no, datetime chr UTC, depth_ft num,
#'   approval_status, qualifier), sorted, or NULL.
fetch_groundwater_latest <- function(site_id = GW_SITE, days = 30, end_time = Sys.time()) {
  .null_on_failure(
    paste0("groundwater_latest ", site_id),
    ogc_fetch_continuous(site_id, PARAM_GW_DEPTH, days, end_time, "depth_ft")
  )
}

#' Daily mean depth to water (statistic 00003) over a date range.
#'
#' @param site_id OGC monitoring_location_id (default GW_SITE).
#' @param start_date,end_date Date or "YYYY-MM-DD" (inclusive).
#' @return data.frame(site_no, date chr "YYYY-MM-DD", depth_ft num,
#'   approval_status, qualifier), sorted, or NULL.
fetch_groundwater_daily <- function(site_id = GW_SITE, start_date, end_date) {
  .null_on_failure(
    paste0("groundwater_daily ", site_id),
    ogc_fetch_daily(site_id, PARAM_GW_DEPTH, STAT_MEAN, start_date, end_date, "depth_ft")
  )
}
