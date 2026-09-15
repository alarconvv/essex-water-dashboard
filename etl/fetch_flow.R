# etl/fetch_flow.R
#
# Streamflow (parameter 00060, ft^3/s) from the USGS OGC API via httr2.
# Replaces Source A's get_usgs_flow() (latest-continuous, one value) and the
# data pull inside get_seasonal_median() (daily, statistic 00003).
#
# Both fetchers return NULL on any failure or empty result (the reason is
# recorded via etl_note_failure()); they never throw.
#
# Requires etl/constants.R and etl/http_common.R.

#' Recent 15-minute discharge for a gauge.
#'
#' Uses the `continuous` collection (not `latest-continuous`, which returns
#' only a single most-recent value) so the app can chart a recent series.
#'
#' @param site_id OGC monitoring_location_id, e.g. "USGS-01101000".
#' @param days Trailing days to request (positive number).
#' @param end_time End of the window (POSIXct; default now).
#' @return data.frame(site_no chr, datetime chr UTC "YYYY-MM-DD HH:MM:SS",
#'   discharge_cfs num, approval_status chr, qualifier chr), sorted by
#'   datetime, or NULL.
fetch_flow_latest <- function(site_id, days = 30, end_time = Sys.time()) {
  .null_on_failure(
    paste0("flow_latest ", site_id),
    ogc_fetch_continuous(site_id, PARAM_DISCHARGE, days, end_time, "discharge_cfs")
  )
}

#' Daily mean discharge (statistic 00003) for a gauge over a date range.
#'
#' @param site_id OGC monitoring_location_id.
#' @param start_date,end_date Date or "YYYY-MM-DD" (inclusive).
#' @return data.frame(site_no chr, date chr "YYYY-MM-DD", discharge_cfs num,
#'   approval_status chr, qualifier chr), sorted by date, or NULL.
fetch_flow_daily <- function(site_id, start_date, end_date) {
  .null_on_failure(
    paste0("flow_daily ", site_id),
    ogc_fetch_daily(site_id, PARAM_DISCHARGE, STAT_MEAN, start_date, end_date, "discharge_cfs")
  )
}
