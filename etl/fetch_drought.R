# etl/fetch_drought.R
#
# U.S. Drought Monitor county statistics (percent area in D0-D4) via
# usdmdataservices.unl.edu, ported from Source B's httr version to httr2.
#
# Live behavior (re-confirmed 2026-09-14):
#   - Without `Accept: application/json` the endpoint returns 200 with
#     content-type text/csv (upper-case MapDate,FIPS,...,D0..D4 columns).
#     Parsing that as JSON throws, so we send the Accept header AND require
#     status 200 plus a JSON content type before parsing.
#   - JSON fields: mapDate (camelCase, "2026-09-08T00:00:00"), fips, county,
#     state, none, d0..d4 (lowercase), validStart, validEnd,
#     statisticFormatID. One element per weekly map in the window.
#
# Requires etl/constants.R and etl/http_common.R.

#' Drought severity (percent of county area in D0-D4) for USDM weekly maps.
#'
#' Range limits (live, 2026-09-15): a single request handles multi-year
#' windows quickly -- 1 y = 53 weeks in 0.5 s, 3 y = 157 in 0.6 s, 10 y = 522
#' in 1.4 s, 2000-01-01..2026-09-15 = 1,393 in 2.4 s, no duplicate weeks --
#' so no chunking is needed. An inverted range returns HTTP 400.
#'
#' @param fips County FIPS code, e.g. "25009".
#' @param days_back Trailing days to request when start_date is NULL.
#' @param today End date of the window (Date; default today).
#' @param start_date Optional explicit start (Date or "YYYY-MM-DD"); overrides
#'   days_back. Used by run_etl() for the multi-year backfill.
#' @return data.frame(fips chr, map_date chr "YYYY-MM-DD", d0 num, d1 num,
#'   d2 num, d3 num, d4 num), sorted by map_date, or NULL on any failure
#'   (network error, non-200, non-JSON content type, empty or malformed body,
#'   or a start date after today).
fetch_drought_status <- function(fips, days_back = 14, today = Sys.Date(), start_date = NULL) {
  label <- paste0("drought ", fips)
  tryCatch(
    {
      today <- as.Date(today)
      start <- if (is.null(start_date)) today - days_back else as.Date(start_date)
      if (length(start) != 1 || is.na(start) || start > today) {
        stop("invalid drought window start: ", format(start))
      }
      resp <- httr2::request(USDM_COUNTY_STATS_URL) |>
        httr2::req_url_query(
          aoi = fips,
          startdate = format(start, "%m/%d/%Y"),
          enddate = format(today, "%m/%d/%Y"),
          statisticsType = 1
        ) |>
        httr2::req_headers(Accept = "application/json") |>
        httr2::req_user_agent("essex-water-dashboard-etl") |>
        httr2::req_timeout(HTTP_TIMEOUT_DROUGHT) |>
        httr2::req_error(is_error = function(resp) FALSE) |>
        httr2::req_perform()

      status <- httr2::resp_status(resp)
      if (status != 200) {
        etl_note_failure(paste0(label, ": HTTP ", status))
        return(NULL)
      }
      ctype <- tryCatch(httr2::resp_content_type(resp), error = function(e) NA_character_)
      if (length(ctype) == 0 || is.na(ctype) || !grepl("json", ctype, ignore.case = TRUE)) {
        etl_note_failure(paste0(label, ": unexpected content type ", ctype))
        return(NULL)
      }

      parsed <- jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = TRUE)
      if (!is.data.frame(parsed) || nrow(parsed) == 0) {
        etl_note_failure(paste0(label, ": no drought rows returned"))
        return(NULL)
      }
      required <- c("mapDate", "fips", "d0", "d1", "d2", "d3", "d4")
      if (!all(required %in% names(parsed))) {
        etl_note_failure(paste0(label, ": response missing expected fields"))
        return(NULL)
      }

      map_date <- suppressWarnings(as.Date(substr(as.character(parsed$mapDate), 1, 10)))
      out <- data.frame(
        fips = as.character(parsed$fips),
        map_date = format(map_date, "%Y-%m-%d"),
        d0 = suppressWarnings(as.numeric(parsed$d0)),
        d1 = suppressWarnings(as.numeric(parsed$d1)),
        d2 = suppressWarnings(as.numeric(parsed$d2)),
        d3 = suppressWarnings(as.numeric(parsed$d3)),
        d4 = suppressWarnings(as.numeric(parsed$d4)),
        stringsAsFactors = FALSE
      )
      out <- out[!is.na(map_date) & !is.na(out$fips), , drop = FALSE]
      if (nrow(out) == 0) {
        etl_note_failure(paste0(label, ": no rows with a valid mapDate"))
        return(NULL)
      }
      out <- out[order(out$map_date), , drop = FALSE]
      rownames(out) <- NULL
      out
    },
    error = function(e) {
      etl_note_failure(paste0(label, ": ", conditionMessage(e)))
      NULL
    }
  )
}
