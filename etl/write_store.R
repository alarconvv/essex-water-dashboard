# etl/write_store.R
#
# Store I/O for the ETL: parameterized delete-then-insert upserts (one per
# table), the etl_runs logger, and the two small reads the ETL needs
# (daily history for percentile computation, latest stored date for
# incremental fetches).
#
# Every write runs inside a transaction, so a failed insert never leaves a
# deleted-but-not-replaced range behind. Time-series upserts replace only
# the (site, date range) actually being written; percentile/typical tables
# are replaced in full per site.
#
# CRITICAL: every SQL string here is a literal with `?` placeholders and
# values passed via `params = list(...)`. Never build SQL with sprintf() or
# paste0() -- the security gate greps for exactly that.

#' Delete a scoped range and append replacement rows atomically.
#' `table` is always a literal table name from a caller in this file.
#' @keywords internal
.replace_rows <- function(con, table, delete_sql, delete_params, out) {
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, delete_sql, params = delete_params)
    DBI::dbAppendTable(con, table, out)
  })
  nrow(out)
}

#' Keep the last row per key (later rows win) and drop rows with NA keys.
#' @keywords internal
.dedupe_on <- function(out, keys) {
  out <- out[stats::complete.cases(out[, keys, drop = FALSE]), , drop = FALSE]
  out[!duplicated(out[, keys, drop = FALSE], fromLast = TRUE), , drop = FALSE]
}

#' Shape a fetcher's time-series output into table columns.
#' @keywords internal
.ts_frame <- function(df, time_col, value_col) {
  missing_cols <- setdiff(c("site_no", time_col, value_col), names(df))
  if (length(missing_cols) > 0) {
    stop("missing column(s): ", paste(missing_cols, collapse = ", "))
  }
  out <- data.frame(
    site_no = as.character(df$site_no),
    time = as.character(df[[time_col]]),
    value = suppressWarnings(as.numeric(df[[value_col]])),
    approval_status = if ("approval_status" %in% names(df)) as.character(df$approval_status) else NA_character_,
    qualifier = if ("qualifier" %in% names(df)) as.character(df$qualifier) else NA_character_,
    stringsAsFactors = FALSE
  )
  names(out)[2:3] <- c(time_col, value_col)
  .dedupe_on(out, c("site_no", time_col))
}

.is_empty <- function(df) is.null(df) || !is.data.frame(df) || nrow(df) == 0

# ---- Instantaneous (continuous) tables -------------------------------------

#' Upsert instantaneous flow rows.
#' @param con DBI connection.
#' @param df data.frame(site_no, datetime, discharge_cfs, approval_status,
#'   qualifier) as returned by fetch_flow_latest().
#' @return Integer rows written (0 for NULL/empty input).
upsert_flow_instantaneous <- function(con, df) {
  if (.is_empty(df)) return(0L)
  out <- .ts_frame(df, "datetime", "discharge_cfs")
  n <- 0L
  for (site in unique(out$site_no)) {
    part <- out[out$site_no == site, , drop = FALSE]
    rng <- range(part$datetime)
    n <- n + .replace_rows(
      con, "flow_instantaneous",
      "DELETE FROM flow_instantaneous WHERE site_no = ? AND datetime >= ? AND datetime <= ?",
      list(site, rng[1], rng[2]), part
    )
  }
  as.integer(n)
}

#' Upsert instantaneous groundwater rows (table groundwater_instantaneous;
#' "gw" keeps the name within the 30-character lint limit).
#' @param df data.frame(site_no, datetime, depth_ft, approval_status, qualifier).
#' @return Integer rows written.
upsert_gw_instantaneous <- function(con, df) {
  if (.is_empty(df)) return(0L)
  out <- .ts_frame(df, "datetime", "depth_ft")
  n <- 0L
  for (site in unique(out$site_no)) {
    part <- out[out$site_no == site, , drop = FALSE]
    rng <- range(part$datetime)
    n <- n + .replace_rows(
      con, "groundwater_instantaneous",
      "DELETE FROM groundwater_instantaneous WHERE site_no = ? AND datetime >= ? AND datetime <= ?",
      list(site, rng[1], rng[2]), part
    )
  }
  as.integer(n)
}

#' Upsert instantaneous precipitation rows.
#' @param df data.frame(site_no, datetime, precip_in, approval_status, qualifier).
#' @return Integer rows written.
upsert_precip_instantaneous <- function(con, df) {
  if (.is_empty(df)) return(0L)
  out <- .ts_frame(df, "datetime", "precip_in")
  n <- 0L
  for (site in unique(out$site_no)) {
    part <- out[out$site_no == site, , drop = FALSE]
    rng <- range(part$datetime)
    n <- n + .replace_rows(
      con, "precip_instantaneous",
      "DELETE FROM precip_instantaneous WHERE site_no = ? AND datetime >= ? AND datetime <= ?",
      list(site, rng[1], rng[2]), part
    )
  }
  as.integer(n)
}

# ---- Daily tables ------------------------------------------------------------

#' Upsert daily flow rows.
#' @param df data.frame(site_no, date, discharge_cfs, approval_status, qualifier).
#' @return Integer rows written.
upsert_flow_daily <- function(con, df) {
  if (.is_empty(df)) return(0L)
  out <- .ts_frame(df, "date", "discharge_cfs")
  n <- 0L
  for (site in unique(out$site_no)) {
    part <- out[out$site_no == site, , drop = FALSE]
    rng <- range(part$date)
    n <- n + .replace_rows(
      con, "flow_daily",
      "DELETE FROM flow_daily WHERE site_no = ? AND date >= ? AND date <= ?",
      list(site, rng[1], rng[2]), part
    )
  }
  as.integer(n)
}

#' Upsert daily groundwater rows.
#' @param df data.frame(site_no, date, depth_ft, approval_status, qualifier).
#' @return Integer rows written.
upsert_groundwater_daily <- function(con, df) {
  if (.is_empty(df)) return(0L)
  out <- .ts_frame(df, "date", "depth_ft")
  n <- 0L
  for (site in unique(out$site_no)) {
    part <- out[out$site_no == site, , drop = FALSE]
    rng <- range(part$date)
    n <- n + .replace_rows(
      con, "groundwater_daily",
      "DELETE FROM groundwater_daily WHERE site_no = ? AND date >= ? AND date <= ?",
      list(site, rng[1], rng[2]), part
    )
  }
  as.integer(n)
}

#' Upsert daily precipitation rows.
#' @param df data.frame(site_no, date, precip_in, approval_status, qualifier).
#' @return Integer rows written.
upsert_precip_daily <- function(con, df) {
  if (.is_empty(df)) return(0L)
  out <- .ts_frame(df, "date", "precip_in")
  n <- 0L
  for (site in unique(out$site_no)) {
    part <- out[out$site_no == site, , drop = FALSE]
    rng <- range(part$date)
    n <- n + .replace_rows(
      con, "precip_daily",
      "DELETE FROM precip_daily WHERE site_no = ? AND date >= ? AND date <= ?",
      list(site, rng[1], rng[2]), part
    )
  }
  as.integer(n)
}

# ---- Percentile / typical tables (full replace per site) ----------------------

.percentile_frame <- function(site_no, table_df) {
  need <- c("month_nu", "day_nu", "p10", "p25", "p50", "p75", "p90", "years_used")
  missing_cols <- setdiff(need, names(table_df))
  if (length(missing_cols) > 0) stop("missing column(s): ", paste(missing_cols, collapse = ", "))
  out <- data.frame(
    site_no = as.character(site_no),
    month_nu = as.integer(table_df$month_nu), day_nu = as.integer(table_df$day_nu),
    p10 = as.numeric(table_df$p10), p25 = as.numeric(table_df$p25),
    p50 = as.numeric(table_df$p50), p75 = as.numeric(table_df$p75),
    p90 = as.numeric(table_df$p90),
    years_used = as.integer(table_df$years_used),
    stringsAsFactors = FALSE
  )
  .dedupe_on(out, c("site_no", "month_nu", "day_nu"))
}

#' Replace all flow percentile rows for one site.
#' @param site_no Site id.
#' @param table_df Output of build_percentile_table().
#' @return Integer rows written.
upsert_flow_percentiles <- function(con, site_no, table_df) {
  if (.is_empty(table_df)) return(0L)
  out <- .percentile_frame(site_no, table_df)
  as.integer(.replace_rows(
    con, "flow_percentiles",
    "DELETE FROM flow_percentiles WHERE site_no = ?", list(site_no), out
  ))
}

#' Replace all groundwater percentile rows for one site.
#' @return Integer rows written.
upsert_groundwater_percentiles <- function(con, site_no, table_df) {
  if (.is_empty(table_df)) return(0L)
  out <- .percentile_frame(site_no, table_df)
  as.integer(.replace_rows(
    con, "groundwater_percentiles",
    "DELETE FROM groundwater_percentiles WHERE site_no = ?", list(site_no), out
  ))
}

#' Replace all typical-precipitation rows for one gauge.
#' @param table_df Output of build_precip_typical_table().
#' @return Integer rows written.
upsert_precip_typical <- function(con, site_no, table_df) {
  if (.is_empty(table_df)) return(0L)
  need <- c("month_nu", "day_nu", "median_in", "years_used")
  missing_cols <- setdiff(need, names(table_df))
  if (length(missing_cols) > 0) stop("missing column(s): ", paste(missing_cols, collapse = ", "))
  out <- data.frame(
    site_no = as.character(site_no),
    month_nu = as.integer(table_df$month_nu), day_nu = as.integer(table_df$day_nu),
    median_in = as.numeric(table_df$median_in),
    years_used = as.integer(table_df$years_used),
    stringsAsFactors = FALSE
  )
  out <- .dedupe_on(out, c("site_no", "month_nu", "day_nu"))
  as.integer(.replace_rows(
    con, "precip_typical",
    "DELETE FROM precip_typical WHERE site_no = ?", list(site_no), out
  ))
}

# ---- Drought ----------------------------------------------------------------------

#' Upsert drought status rows.
#' @param df data.frame(fips, map_date, d0, d1, d2, d3, d4) from
#'   fetch_drought_status().
#' @return Integer rows written.
upsert_drought_status <- function(con, df) {
  if (.is_empty(df)) return(0L)
  need <- c("fips", "map_date", "d0", "d1", "d2", "d3", "d4")
  missing_cols <- setdiff(need, names(df))
  if (length(missing_cols) > 0) stop("missing column(s): ", paste(missing_cols, collapse = ", "))
  out <- data.frame(
    fips = as.character(df$fips), map_date = as.character(df$map_date),
    d0 = as.numeric(df$d0), d1 = as.numeric(df$d1), d2 = as.numeric(df$d2),
    d3 = as.numeric(df$d3), d4 = as.numeric(df$d4),
    stringsAsFactors = FALSE
  )
  out <- .dedupe_on(out, c("fips", "map_date"))
  n <- 0L
  for (f in unique(out$fips)) {
    part <- out[out$fips == f, , drop = FALSE]
    rng <- range(part$map_date)
    n <- n + .replace_rows(
      con, "drought_status",
      "DELETE FROM drought_status WHERE fips = ? AND map_date >= ? AND map_date <= ?",
      list(f, rng[1], rng[2]), part
    )
  }
  as.integer(n)
}

# ---- etl_runs ----------------------------------------------------------------------

#' Log one ETL source outcome.
#' @param status "success" or "failure".
#' @return invisible(TRUE)
log_etl_run <- function(con, source, started_at, finished_at, status,
                        rows_written, error_message = NA_character_) {
  DBI::dbExecute(
    con,
    "INSERT INTO etl_runs (source, started_at, finished_at, status, rows_written, error_message) VALUES (?, ?, ?, ?, ?, ?)", # nolint: line_length_linter.
    params = list(source, started_at, finished_at, status, as.integer(rows_written), error_message)
  )
  invisible(TRUE)
}

# ---- Reads the ETL needs -----------------------------------------------------------

#' Read one site's stored daily series as data.frame(date, value).
#'
#' @param kind "flow", "groundwater" or "precip" (selects a literal query).
#' @return data.frame(date = chr, value = num), possibly zero rows.
read_daily_series <- function(con, kind, site_no) {
  sql <- switch(kind,
    flow = "SELECT date, discharge_cfs AS value FROM flow_daily WHERE site_no = ? ORDER BY date",
    groundwater = "SELECT date, depth_ft AS value FROM groundwater_daily WHERE site_no = ? ORDER BY date",
    precip = "SELECT date, precip_in AS value FROM precip_daily WHERE site_no = ? ORDER BY date",
    stop("unknown kind: ", kind)
  )
  DBI::dbGetQuery(con, sql, params = list(site_no))
}

#' Latest stored daily date for a site, or NA if none.
#' @return Date (length 1, possibly NA).
latest_daily_date <- function(con, kind, site_no) {
  sql <- switch(kind,
    flow = "SELECT MAX(date) AS d FROM flow_daily WHERE site_no = ?",
    groundwater = "SELECT MAX(date) AS d FROM groundwater_daily WHERE site_no = ?",
    precip = "SELECT MAX(date) AS d FROM precip_daily WHERE site_no = ?",
    stop("unknown kind: ", kind)
  )
  d <- DBI::dbGetQuery(con, sql, params = list(site_no))$d
  if (length(d) == 0 || is.na(d[1])) as.Date(NA) else as.Date(d[1])
}

#' Latest stored USDM map_date for a county, or NA if none.
#' @return Date (length 1, possibly NA).
latest_drought_date <- function(con, fips) {
  d <- DBI::dbGetQuery(
    con, "SELECT MAX(map_date) AS d FROM drought_status WHERE fips = ?",
    params = list(fips)
  )$d
  if (length(d) == 0 || is.na(d[1])) as.Date(NA) else as.Date(d[1])
}

#' Earliest stored USDM map_date for a county, or NA if none.
#' @return Date (length 1, possibly NA).
earliest_drought_date <- function(con, fips) {
  d <- DBI::dbGetQuery(
    con, "SELECT MIN(map_date) AS d FROM drought_status WHERE fips = ?",
    params = list(fips)
  )$d
  if (length(d) == 0 || is.na(d[1])) as.Date(NA) else as.Date(d[1])
}
