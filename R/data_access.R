# R/data_access.R
#
# The only layer through which the Shiny app reads the SQLite store
# (data/essexwater.sqlite, schema in etl/db_schema.R). No network code here;
# all HTTP lives in etl/.
#
# Rules every function follows:
#   - `con` (a DBI connection) is the first argument.
#   - SQL text is always a fixed string literal. Caller values are bound with
#     `params = list(...)`. A `kind` argument never reaches SQL as text: it is
#     validated against a whitelist (match.arg) and then used as a key into a
#     named vector of complete, literal SQL statements (.SQL_* below).
#   - Nothing throws. A missing table, a missing column (schema drift), a
#     closed connection, invalid arguments, or zero rows all return the
#     documented empty shape, with correct column names and types.
#   - Aggregation (window totals, medians, low-flow counts, per-source status)
#     happens in R, not SQL.
#   - Time: `datetime` columns are stored as UTC "YYYY-MM-DD HH:MM:SS" and are
#     returned as POSIXct (tz "UTC") in series; `date` columns become Date.
#     get_latest_reading(), get_last_updated() and get_source_status() return
#     timestamps as the stored UTC character string (format_last_updated()
#     in R/helpers.R parses it for display).
#
# Invalid `kind` (unknown name, NA, NULL, length != 1, or a kind the function
# does not support, e.g. "precip" for get_percentiles()): the function returns
# its empty shape. It never builds or runs SQL for it. This keeps the
# "never throws" contract the server relies on.

# ---- Whitelists and literal SQL ------------------------------------------------

.SERIES_KINDS <- c("flow", "groundwater", "precip")
.PERCENTILE_KINDS <- c("flow", "groundwater")

.SQL_LATEST <- c(
  flow = "SELECT datetime, discharge_cfs AS value FROM flow_instantaneous WHERE site_no = ? AND discharge_cfs IS NOT NULL ORDER BY datetime DESC LIMIT 1", # nolint: line_length_linter.
  groundwater = "SELECT datetime, depth_ft AS value FROM groundwater_instantaneous WHERE site_no = ? AND depth_ft IS NOT NULL ORDER BY datetime DESC LIMIT 1", # nolint: line_length_linter.
  precip = "SELECT datetime, precip_in AS value FROM precip_instantaneous WHERE site_no = ? AND precip_in IS NOT NULL ORDER BY datetime DESC LIMIT 1" # nolint: line_length_linter.
)

.SQL_INSTANTANEOUS <- c(
  flow = "SELECT datetime, discharge_cfs AS value FROM flow_instantaneous WHERE site_no = ? AND datetime >= ? AND datetime <= ? ORDER BY datetime ASC", # nolint: line_length_linter.
  groundwater = "SELECT datetime, depth_ft AS value FROM groundwater_instantaneous WHERE site_no = ? AND datetime >= ? AND datetime <= ? ORDER BY datetime ASC", # nolint: line_length_linter.
  precip = "SELECT datetime, precip_in AS value FROM precip_instantaneous WHERE site_no = ? AND datetime >= ? AND datetime <= ? ORDER BY datetime ASC" # nolint: line_length_linter.
)

.SQL_DAILY <- c(
  flow = "SELECT date, discharge_cfs AS value FROM flow_daily WHERE site_no = ? AND date >= ? AND date <= ? ORDER BY date ASC", # nolint: line_length_linter.
  groundwater = "SELECT date, depth_ft AS value FROM groundwater_daily WHERE site_no = ? AND date >= ? AND date <= ? ORDER BY date ASC", # nolint: line_length_linter.
  precip = "SELECT date, precip_in AS value FROM precip_daily WHERE site_no = ? AND date >= ? AND date <= ? ORDER BY date ASC" # nolint: line_length_linter.
)

# `(? IS NULL OR col = ?)`: an NA parameter binds as SQL NULL and disables
# that filter. Only a NULL R argument becomes NA; any other non-integer
# argument is rejected before the query (see get_percentiles()).
.SQL_PERCENTILES <- c(
  flow = "SELECT month_nu, day_nu, p10, p25, p50, p75, p90, years_used FROM flow_percentiles WHERE site_no = ? AND (? IS NULL OR month_nu = ?) AND (? IS NULL OR day_nu = ?) ORDER BY month_nu ASC, day_nu ASC", # nolint: line_length_linter.
  groundwater = "SELECT month_nu, day_nu, p10, p25, p50, p75, p90, years_used FROM groundwater_percentiles WHERE site_no = ? AND (? IS NULL OR month_nu = ?) AND (? IS NULL OR day_nu = ?) ORDER BY month_nu ASC, day_nu ASC" # nolint: line_length_linter.
)

# ---- Empty shapes ----------------------------------------------------------------

.empty_sites <- function() {
  data.frame(
    site_no = character(0), kind = character(0), parameter_code = character(0),
    name = character(0), watershed = character(0), lat = numeric(0), lon = numeric(0),
    stringsAsFactors = FALSE
  )
}

.empty_latest <- function() list(value = NA_real_, datetime = NA_character_)

.empty_instantaneous <- function() {
  data.frame(datetime = as.POSIXct(character(0), tz = "UTC"), value = numeric(0))
}

.empty_daily <- function() data.frame(date = as.Date(character(0)), value = numeric(0))

.empty_percentiles <- function() {
  data.frame(
    month_nu = integer(0), day_nu = integer(0),
    p10 = numeric(0), p25 = numeric(0), p50 = numeric(0), p75 = numeric(0), p90 = numeric(0),
    years_used = integer(0)
  )
}

.empty_precip_window <- function() {
  list(total_in = NA_real_, start_date = as.Date(NA), end_date = as.Date(NA), days_with_data = 0L)
}

.empty_precip_typical <- function() list(typical_in = NA_real_, years_used = 0L)

.empty_low_flow <- function() data.frame(yr = integer(0), threshold = numeric(0), days = integer(0))

.empty_drought <- function() {
  data.frame(
    fips = character(0), map_date = as.Date(character(0)),
    d0 = numeric(0), d1 = numeric(0), d2 = numeric(0), d3 = numeric(0), d4 = numeric(0),
    stringsAsFactors = FALSE
  )
}

.empty_source_status <- function() {
  data.frame(
    source = character(0), last_success = character(0),
    last_status = character(0), last_error = character(0),
    stringsAsFactors = FALSE
  )
}

# ---- Internal validation helpers ---------------------------------------------------

#' Resolve `kind` against a whitelist. Returns the matched name, or stops
#' (callers catch it and return their empty shape).
#'
#' Unlike bare match.arg(), NULL is rejected rather than silently becoming
#' the first choice; the untouched default vector still selects the first
#' choice (the usual `kind = c(...)` idiom).
#' @keywords internal
.match_kind <- function(kind, choices) {
  if (identical(kind, choices)) {
    return(choices[[1]])
  }
  if (!is.character(kind) || length(kind) != 1L || is.na(kind) || !nzchar(kind)) {
    stop("invalid kind")
  }
  match.arg(kind, choices)
}

#' A single non-NA, non-empty character value (site_no / fips / prefix).
#' @keywords internal
.is_scalar_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
}

#' A single finite number, optionally with a lower bound.
#' @keywords internal
.is_scalar_number <- function(x, min = -Inf) {
  (is.numeric(x) || is.integer(x)) && length(x) == 1L && is.finite(x) && x >= min
}

#' Coerce a Date / "YYYY-MM-DD" scalar to Date; NA when not parseable.
#' @keywords internal
.as_scalar_date <- function(x) {
  if (length(x) != 1L) {
    return(as.Date(NA))
  }
  if (inherits(x, "Date")) {
    return(x)
  }
  if (inherits(x, "POSIXt")) {
    return(as.Date(format(x, "%Y-%m-%d")))
  }
  if (!is.character(x)) {
    return(as.Date(NA))
  }
  suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
}

#' Vectorised "YYYY-MM-DD" -> Date (NA when unparseable).
#' @keywords internal
.as_date_vec <- function(x) {
  as.Date(substr(as.character(x), 1, 10), format = "%Y-%m-%d")
}

#' Parse stored UTC "YYYY-MM-DD HH:MM:SS" text into POSIXct UTC.
#' @keywords internal
.parse_utc <- function(x) {
  as.POSIXct(as.character(x), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
}

#' Format a POSIXct as stored UTC text.
#' @keywords internal
.format_utc <- function(x) format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC")

#' Latest precip_daily date for a site, or NA Date.
#' @keywords internal
.latest_precip_date <- function(con, site_no) {
  res <- DBI::dbGetQuery(
    con,
    "SELECT MAX(date) AS max_date FROM precip_daily WHERE site_no = ? AND precip_in IS NOT NULL",
    params = list(site_no)
  )
  if (nrow(res) == 0) {
    return(as.Date(NA))
  }
  .as_scalar_date(as.character(res$max_date[[1]]))
}

# ---- Public API ------------------------------------------------------------------------

#' Monitoring sites.
#'
#' @param con A DBI connection.
#' @param kind NULL for all sites, or a single kind string ("flow",
#'   "groundwater", "precip") bound as a query parameter. An unknown kind
#'   matches no rows.
#' @return data.frame(site_no, kind, parameter_code, name, watershed, lat, lon)
#'   ordered by kind then site_no; zero rows (same columns) when empty.
get_sites <- function(con, kind = NULL) {
  tryCatch(
    {
      if (is.null(kind)) {
        res <- DBI::dbGetQuery(
          con,
          "SELECT site_no, kind, parameter_code, name, watershed, lat, lon FROM sites ORDER BY kind ASC, site_no ASC"
        )
      } else {
        if (!.is_scalar_string(kind)) {
          return(.empty_sites())
        }
        res <- DBI::dbGetQuery(
          con,
          "SELECT site_no, kind, parameter_code, name, watershed, lat, lon FROM sites WHERE kind = ? ORDER BY site_no ASC", # nolint: line_length_linter.
          params = list(kind)
        )
      }
      if (nrow(res) == 0) {
        return(.empty_sites())
      }
      data.frame(
        site_no = as.character(res$site_no), kind = as.character(res$kind),
        parameter_code = as.character(res$parameter_code), name = as.character(res$name),
        watershed = as.character(res$watershed),
        lat = as.numeric(res$lat), lon = as.numeric(res$lon),
        stringsAsFactors = FALSE
      )
    },
    error = function(e) .empty_sites()
  )
}

#' Most recent non-missing instantaneous reading for a site.
#'
#' @param con A DBI connection.
#' @param kind "flow" (discharge_cfs), "groundwater" (depth_ft), or "precip"
#'   (precip_in, a 15-minute increment).
#' @param site_no Full OGC id, e.g. "USGS-01101000".
#' @return list(value = numeric, datetime = UTC "YYYY-MM-DD HH:MM:SS"), or
#'   list(value = NA_real_, datetime = NA_character_) when unavailable.
get_latest_reading <- function(con, kind = c("flow", "groundwater", "precip"), site_no) {
  tryCatch(
    {
      kind <- .match_kind(kind, .SERIES_KINDS)
      if (!.is_scalar_string(site_no)) {
        return(.empty_latest())
      }
      res <- DBI::dbGetQuery(con, .SQL_LATEST[[kind]], params = list(site_no))
      if (nrow(res) == 0) {
        return(.empty_latest())
      }
      list(value = as.numeric(res$value[[1]]), datetime = as.character(res$datetime[[1]]))
    },
    error = function(e) .empty_latest()
  )
}

#' Instantaneous readings in the `days` before `now` (inclusive bounds).
#'
#' @param con A DBI connection.
#' @param kind "flow", "groundwater", or "precip".
#' @param site_no Full OGC id.
#' @param days Positive number of days back from `now`.
#' @param now POSIXct upper bound (any tz; compared in UTC).
#' @return data.frame(datetime POSIXct UTC, value numeric), ascending by
#'   datetime. Rows whose stored value is NULL are kept with value NA (a real
#'   gap). Zero rows when empty.
get_instantaneous_series <- function(con, kind, site_no, days, now = Sys.time()) {
  tryCatch(
    {
      kind <- .match_kind(kind, .SERIES_KINDS)
      if (!.is_scalar_string(site_no) || !.is_scalar_number(days, min = 0) ||
            !inherits(now, "POSIXt") || length(now) != 1L || is.na(now)) {
        return(.empty_instantaneous())
      }
      upper <- as.POSIXct(now)
      lower <- upper - as.numeric(days) * 86400
      res <- DBI::dbGetQuery(
        con, .SQL_INSTANTANEOUS[[kind]],
        params = list(site_no, .format_utc(lower), .format_utc(upper))
      )
      if (nrow(res) == 0) {
        return(.empty_instantaneous())
      }
      out <- data.frame(datetime = .parse_utc(res$datetime), value = as.numeric(res$value))
      out <- out[!is.na(out$datetime), , drop = FALSE]
      rownames(out) <- NULL
      out
    },
    error = function(e) .empty_instantaneous()
  )
}

#' Daily values between two dates (inclusive).
#'
#' @param con A DBI connection.
#' @param kind "flow", "groundwater", or "precip".
#' @param site_no Full OGC id.
#' @param start_date,end_date Date or "YYYY-MM-DD".
#' @return data.frame(date Date, value numeric), ascending. Zero rows when
#'   empty, when a date is invalid, or when start_date > end_date.
get_daily_series <- function(con, kind, site_no, start_date, end_date) {
  tryCatch(
    {
      kind <- .match_kind(kind, .SERIES_KINDS)
      start <- .as_scalar_date(start_date)
      end <- .as_scalar_date(end_date)
      if (!.is_scalar_string(site_no) || is.na(start) || is.na(end) || start > end) {
        return(.empty_daily())
      }
      res <- DBI::dbGetQuery(
        con, .SQL_DAILY[[kind]],
        params = list(site_no, format(start, "%Y-%m-%d"), format(end, "%Y-%m-%d"))
      )
      if (nrow(res) == 0) {
        return(.empty_daily())
      }
      out <- data.frame(date = .as_date_vec(res$date), value = as.numeric(res$value))
      out <- out[!is.na(out$date), , drop = FALSE]
      rownames(out) <- NULL
      out
    },
    error = function(e) .empty_daily()
  )
}

#' Stored same-month-day percentiles.
#'
#' @param con A DBI connection.
#' @param kind "flow" or "groundwater" (groundwater values are depth to
#'   water: larger = drier).
#' @param site_no Full OGC id.
#' @param month_nu,day_nu NULL for no filter, or a single whole number.
#'   Any other value (NA, text, a vector) returns zero rows.
#' @return data.frame(month_nu, day_nu, p10, p25, p50, p75, p90, years_used)
#'   ordered by month_nu, day_nu. Statistics may be NA where years_used = 0.
get_percentiles <- function(con, kind = c("flow", "groundwater"), site_no, month_nu = NULL, day_nu = NULL) {
  tryCatch(
    {
      kind <- .match_kind(kind, .PERCENTILE_KINDS)
      if (!.is_scalar_string(site_no)) {
        return(.empty_percentiles())
      }
      as_filter <- function(x, max) {
        if (is.null(x)) {
          return(NA_integer_)
        }
        if (!.is_scalar_number(x, min = 1) || x > max || x != round(x)) {
          stop("invalid month/day filter")
        }
        as.integer(x)
      }
      m <- as_filter(month_nu, 12)
      d <- as_filter(day_nu, 31)
      res <- DBI::dbGetQuery(con, .SQL_PERCENTILES[[kind]], params = list(site_no, m, m, d, d))
      if (nrow(res) == 0) {
        return(.empty_percentiles())
      }
      data.frame(
        month_nu = as.integer(res$month_nu), day_nu = as.integer(res$day_nu),
        p10 = as.numeric(res$p10), p25 = as.numeric(res$p25), p50 = as.numeric(res$p50),
        p75 = as.numeric(res$p75), p90 = as.numeric(res$p90),
        years_used = as.integer(res$years_used)
      )
    },
    error = function(e) .empty_percentiles()
  )
}

#' Precipitation total over a `days`-day window ending on `end_date`.
#'
#' @param con A DBI connection.
#' @param site_no Precipitation gauge OGC id.
#' @param days Window length in days (whole number >= 1).
#' @param end_date Date / "YYYY-MM-DD"; NULL = the latest date with a value in
#'   precip_daily for this site.
#' @return list(total_in, start_date, end_date, days_with_data). total_in is
#'   the sum of the non-missing daily totals in [end_date - days + 1,
#'   end_date], NA when days_with_data is 0. The two dates are filled
#'   whenever the window is known (an explicit end_date, or a site with
#'   data); otherwise they are NA.
get_precip_window_total <- function(con, site_no, days = 7, end_date = NULL) {
  tryCatch(
    {
      if (!.is_scalar_string(site_no) || !.is_scalar_number(days, min = 1) || days != round(days)) {
        return(.empty_precip_window())
      }
      end <- if (is.null(end_date)) .latest_precip_date(con, site_no) else .as_scalar_date(end_date)
      if (is.na(end)) {
        return(.empty_precip_window())
      }
      start <- end - (as.integer(days) - 1L)
      res <- DBI::dbGetQuery(
        con,
        "SELECT date, precip_in FROM precip_daily WHERE site_no = ? AND date >= ? AND date <= ?",
        params = list(site_no, format(start, "%Y-%m-%d"), format(end, "%Y-%m-%d"))
      )
      vals <- as.numeric(res$precip_in)
      vals <- vals[is.finite(vals)]
      list(
        total_in = if (length(vals) == 0) NA_real_ else sum(vals),
        start_date = start,
        end_date = end,
        days_with_data = length(vals)
      )
    },
    error = function(e) .empty_precip_window()
  )
}

#' Typical precipitation for the same `days`-day window in prior years.
#'
#' For every calendar year before `end_date`'s year, the window ending on the
#' same month-day (Feb 29 -> Feb 28 in non-leap years) is totalled, but only
#' if all `days` days have a stored value. typical_in is the median of those
#' complete-year totals.
#'
#' @param con A DBI connection.
#' @param site_no Precipitation gauge OGC id.
#' @param days Window length in days (whole number >= 1).
#' @param end_date Date / "YYYY-MM-DD". NULL is also accepted and means the
#'   latest stored date for the site (same default as
#'   get_precip_window_total()).
#' @param min_years Minimum number of complete prior years required.
#' @return list(typical_in, years_used). years_used is the number of complete
#'   prior-year windows found; typical_in is NA when years_used < min_years
#'   (or years_used is 0).
get_precip_typical_window <- function(con, site_no, days = 7, end_date, min_years = 3) {
  tryCatch(
    {
      if (!.is_scalar_string(site_no) || !.is_scalar_number(days, min = 1) || days != round(days) ||
            !.is_scalar_number(min_years, min = 0)) {
        return(.empty_precip_typical())
      }
      end <- if (is.null(end_date)) .latest_precip_date(con, site_no) else .as_scalar_date(end_date)
      if (is.na(end)) {
        return(.empty_precip_typical())
      }
      days <- as.integer(days)
      current_start <- end - (days - 1L)
      res <- DBI::dbGetQuery(
        con,
        "SELECT date, precip_in FROM precip_daily WHERE site_no = ? AND date < ? AND precip_in IS NOT NULL",
        params = list(site_no, format(current_start, "%Y-%m-%d"))
      )
      vals <- as.numeric(res$precip_in)
      keep <- is.finite(vals)
      if (!any(keep)) {
        return(.empty_precip_typical())
      }
      by_date <- stats::setNames(vals[keep], substr(as.character(res$date[keep]), 1, 10))

      end_year <- as.integer(format(end, "%Y"))
      first_year <- as.integer(substr(min(names(by_date)), 1, 4))
      month_day <- format(end, "%m-%d")
      totals <- numeric(0)
      for (yr in seq.int(first_year, length.out = max(0L, end_year - first_year))) {
        yr_end <- as.Date(paste0(yr, "-", month_day), format = "%Y-%m-%d")
        if (is.na(yr_end)) {
          yr_end <- as.Date(paste0(yr, "-02-28"))
        }
        window <- format(seq(yr_end - (days - 1L), yr_end, by = "day"), "%Y-%m-%d")
        window_vals <- by_date[window]
        if (!anyNA(window_vals)) {
          totals <- c(totals, sum(window_vals))
        }
      }
      n <- length(totals)
      list(
        typical_in = if (n == 0L || n < min_years) NA_real_ else stats::median(totals),
        years_used = n
      )
    },
    error = function(e) .empty_precip_typical()
  )
}

#' Days per year with daily mean flow strictly below each threshold.
#'
#' @param con A DBI connection.
#' @param site_no Flow gauge OGC id.
#' @param years Whole-number vector of calendar years.
#' @param thresholds Numeric vector of discharge thresholds (cfs).
#' @return data.frame(yr integer, threshold numeric, days integer): the full
#'   years x thresholds grid (thresholds in the given order, years ascending
#'   within each), zero-filled. Days with a missing value are not counted.
#'   Zero rows when the site has no daily values at all in `years` (no data
#'   is not the same as zero low-flow days), or on invalid input/error.
get_low_flow_days <- function(con, site_no, years, thresholds = c(1, 0.1, 0.01)) {
  tryCatch(
    {
      if (!.is_scalar_string(site_no) || !is.numeric(years) || length(years) == 0L ||
            anyNA(years) || any(years != round(years)) ||
            !is.numeric(thresholds) || length(thresholds) == 0L || any(!is.finite(thresholds))) {
        return(.empty_low_flow())
      }
      years <- sort(unique(as.integer(years)))
      thresholds <- unique(as.numeric(thresholds))
      res <- DBI::dbGetQuery(
        con,
        "SELECT date, discharge_cfs FROM flow_daily WHERE site_no = ? AND date >= ? AND date <= ? AND discharge_cfs IS NOT NULL", # nolint: line_length_linter.
        params = list(site_no, sprintf("%04d-01-01", min(years)), sprintf("%04d-12-31", max(years)))
      )
      if (nrow(res) == 0) {
        return(.empty_low_flow())
      }
      obs_year <- as.integer(substr(as.character(res$date), 1, 4))
      flow <- as.numeric(res$discharge_cfs)
      in_years <- obs_year %in% years & is.finite(flow)
      if (!any(in_years)) {
        return(.empty_low_flow())
      }
      obs_year <- obs_year[in_years]
      flow <- flow[in_years]

      grid <- expand.grid(yr = years, threshold = thresholds, KEEP.OUT.ATTRS = FALSE)
      grid$days <- vapply(
        seq_len(nrow(grid)),
        function(i) sum(obs_year == grid$yr[i] & flow < grid$threshold[i]),
        integer(1)
      )
      grid <- grid[order(match(grid$threshold, thresholds), grid$yr), c("yr", "threshold", "days")]
      rownames(grid) <- NULL
      grid
    },
    error = function(e) .empty_low_flow()
  )
}

#' U.S. Drought Monitor weekly area percentages for a county.
#'
#' @param con A DBI connection.
#' @param fips County FIPS code, e.g. "25009".
#' @return data.frame(fips, map_date Date, d0, d1, d2, d3, d4), ascending by
#'   map_date. Zero rows when empty.
get_drought_status <- function(con, fips) {
  tryCatch(
    {
      if (!.is_scalar_string(fips)) {
        return(.empty_drought())
      }
      res <- DBI::dbGetQuery(
        con,
        "SELECT fips, map_date, d0, d1, d2, d3, d4 FROM drought_status WHERE fips = ? ORDER BY map_date ASC",
        params = list(fips)
      )
      if (nrow(res) == 0) {
        return(.empty_drought())
      }
      out <- data.frame(
        fips = as.character(res$fips), map_date = .as_date_vec(res$map_date),
        d0 = as.numeric(res$d0), d1 = as.numeric(res$d1), d2 = as.numeric(res$d2),
        d3 = as.numeric(res$d3), d4 = as.numeric(res$d4),
        stringsAsFactors = FALSE
      )
      out <- out[!is.na(out$map_date), , drop = FALSE]
      rownames(out) <- NULL
      out
    },
    error = function(e) .empty_drought()
  )
}

#' Latest successful ETL finish time for sources starting with a prefix.
#'
#' The prefix is compared literally (`substr(source, 1, n) = prefix`), not
#' with LIKE, so "%" and "_" are ordinary characters and match nothing
#' unless a source name really starts with them.
#'
#' @param con A DBI connection.
#' @param source_prefix e.g. "flow_latest:" or "flow_daily:USGS-01101000".
#'   An empty or NA prefix matches nothing.
#' @return finished_at (UTC "YYYY-MM-DD HH:MM:SS") of the most recent
#'   successful run, or NA_character_.
get_last_updated <- function(con, source_prefix) {
  tryCatch(
    {
      if (!.is_scalar_string(source_prefix)) {
        return(NA_character_)
      }
      res <- DBI::dbGetQuery(
        con,
        "SELECT MAX(finished_at) AS finished_at FROM etl_runs WHERE status = 'success' AND finished_at IS NOT NULL AND substr(source, 1, ?) = ?", # nolint: line_length_linter.
        params = list(nchar(source_prefix, type = "chars"), source_prefix)
      )
      if (nrow(res) == 0 || is.na(res$finished_at[[1]])) {
        return(NA_character_)
      }
      as.character(res$finished_at[[1]])
    },
    error = function(e) NA_character_
  )
}

#' Per-source ETL status for the Evidence "data sources" panel.
#'
#' @param con A DBI connection.
#' @return data.frame(source, last_success, last_status, last_error), one row
#'   per distinct source, ordered by source. last_success is the latest
#'   successful finished_at (NA if the source never succeeded); last_status
#'   and last_error describe the most recent run (highest run_id), so
#'   last_error is NA when that run succeeded. Zero rows when empty.
get_source_status <- function(con) {
  tryCatch(
    {
      res <- DBI::dbGetQuery(
        con,
        "SELECT run_id, source, finished_at, status, error_message FROM etl_runs ORDER BY run_id ASC"
      )
      if (nrow(res) == 0) {
        return(.empty_source_status())
      }
      sources <- sort(unique(as.character(res$source)))
      rows <- lapply(sources, function(src) {
        runs <- res[res$source == src, , drop = FALSE]
        ok <- runs$status %in% "success" & !is.na(runs$finished_at)
        latest <- runs[which.max(runs$run_id), , drop = FALSE]
        data.frame(
          source = src,
          last_success = if (any(ok)) max(as.character(runs$finished_at[ok])) else NA_character_,
          last_status = as.character(latest$status),
          last_error = as.character(latest$error_message),
          stringsAsFactors = FALSE
        )
      })
      out <- do.call(rbind, rows)
      rownames(out) <- NULL
      out
    },
    error = function(e) .empty_source_status()
  )
}
