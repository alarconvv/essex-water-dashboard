# etl/http_common.R
#
# Shared HTTP plumbing for the ETL fetchers (etl/fetch_*.R):
#   1. A tiny "last failure" note so a fetcher that returns NULL can still
#      explain why in etl_runs.error_message.
#   2. A USGS OGC API client (httr2) that follows cursor pagination.
#   3. Generic continuous / daily series fetchers that the per-source
#      fetchers wrap.
#
# Live-verified API behavior (2026-09-14/15):
#   - Pagination is cursor-based: when more rows exist, `links` contains
#     rel = "next" whose href carries `cursor=` plus the original query
#     (limit, properties, skipGeometry, f are preserved). No next link means
#     the last page. `numberMatched` is NOT reported, so the next link is the
#     only reliable signal; we follow it and never stop early.
#   - `limit` maximum is 50000 (larger -> HTTP 400 "Limit of 50000 exceeded").
#   - A single page of 13,149 daily rows (Parker 1990-2025) returned with no
#     next link, so large pages do arrive complete -- but slowly (11 s for
#     flow, 49 s for 1984-2025 groundwater). Daily history is therefore
#     requested in DAILY_CHUNK_YEARS windows with a per-request timeout.
#   - Rows are NOT returned in time order; callers sort.
#   - `continuous` requires an explicit ISO-8601 interval: ISO durations
#     such as "P3D" are rejected with HTTP 400.
#   - Values arrive as strings ("0.10"); qualifier is null or an array.
#
# Requires etl/constants.R.

# ---- Failure note --------------------------------------------------------------

.etl_failure_env <- new.env(parent = emptyenv())

#' Record why the most recent fetch failed (read by run_etl()).
etl_note_failure <- function(msg) {
  assign("last", as.character(msg)[1], envir = .etl_failure_env)
  invisible(NULL)
}

#' Most recent recorded fetch failure, or NA.
etl_last_failure <- function() {
  get0("last", envir = .etl_failure_env, inherits = FALSE, ifnotfound = NA_character_)
}

#' Clear the recorded failure (called before each fetch).
etl_clear_failure <- function() {
  assign("last", NA_character_, envir = .etl_failure_env)
  invisible(NULL)
}

.etl_verbose <- function() isTRUE(getOption("essexwater.etl_verbose", FALSE))

# ---- OGC client -------------------------------------------------------------------

#' Apply the standard options every ETL request gets.
#' @keywords internal
.ogc_req_options <- function(req, timeout_sec) {
  req |>
    httr2::req_headers(Accept = "application/geo+json, application/json") |>
    httr2::req_user_agent("essex-water-dashboard-etl") |>
    httr2::req_timeout(timeout_sec) |>
    httr2::req_retry(
      max_tries = getOption("essexwater.http_max_tries", 3L),
      retry_on_failure = TRUE,
      backoff = function(i) min(2^i, 10)
    ) |>
    httr2::req_error(is_error = function(resp) FALSE)
}

#' Parse one OGC items page. Throws on anything unexpected.
#' @return list(props = data.frame or NULL (no features), next_href = chr or NA)
#' @keywords internal
.ogc_parse_page <- function(resp) {
  status <- httr2::resp_status(resp)
  if (status != 200) stop("HTTP ", status)
  ctype <- tryCatch(httr2::resp_content_type(resp), error = function(e) NA_character_)
  if (length(ctype) == 0 || is.na(ctype) || !grepl("json", ctype, ignore.case = TRUE)) {
    stop("unexpected content type: ", if (length(ctype) == 0) "none" else ctype)
  }
  parsed <- jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = TRUE)
  if (!is.list(parsed) || !("features" %in% names(parsed))) {
    stop("response has no 'features' member")
  }
  feats <- parsed$features
  props <- NULL
  if (length(feats) > 0) {
    if (!is.data.frame(feats) || !is.data.frame(feats$properties)) {
      stop("features lack a 'properties' object")
    }
    props <- feats$properties
  }
  next_href <- NA_character_
  links <- parsed$links
  if (is.data.frame(links) && all(c("rel", "href") %in% names(links))) {
    h <- links$href[!is.na(links$rel) & links$rel == "next"]
    if (length(h) > 0) next_href <- h[1]
  }
  list(props = props, next_href = next_href)
}

#' Fetch every page of an OGC items query.
#'
#' @param collection e.g. "continuous", "daily", "latest-continuous".
#' @param query Named list of query parameters.
#' @param timeout_sec Per-request timeout.
#' @param max_pages Page cap; exceeding it throws (never silently truncates).
#' @param base_url API root.
#' @return data.frame of feature properties (zero rows when the query
#'   matched nothing). Throws on any HTTP/parse failure.
ogc_get_items <- function(collection, query, timeout_sec = HTTP_TIMEOUT_DAILY,
                          max_pages = OGC_MAX_PAGES, base_url = USGS_OGC_BASE_URL) {
  allowed_prefix <- sub("^(https://[^/]+/).*$", "\\1", base_url)
  req <- httr2::request(base_url) |>
    httr2::req_url_path_append("collections", collection, "items") |>
    httr2::req_url_query(!!!query) |>
    .ogc_req_options(timeout_sec)

  pages <- list()
  for (page in seq_len(max_pages)) {
    parsed <- .ogc_parse_page(httr2::req_perform(req))
    if (!is.null(parsed$props) && nrow(parsed$props) > 0) {
      pages[[length(pages) + 1]] <- parsed$props
    }
    if (is.na(parsed$next_href)) {
      if (length(pages) == 0) {
        return(data.frame(time = character(0), value = character(0), stringsAsFactors = FALSE))
      }
      return(.rbind_fill(pages))
    }
    # Only follow next links back to the same API host.
    if (!startsWith(parsed$next_href, allowed_prefix)) {
      stop("refusing to follow next link to unexpected host")
    }
    req <- httr2::request(parsed$next_href) |> .ogc_req_options(timeout_sec)
  }
  stop("page cap (", max_pages, ") exceeded; refusing to return truncated data")
}

#' rbind data.frames whose column sets may differ (missing -> NA).
#' @keywords internal
.rbind_fill <- function(dfs) {
  cols <- unique(unlist(lapply(dfs, names)))
  dfs <- lapply(dfs, function(d) {
    for (cn in setdiff(cols, names(d))) d[[cn]] <- NA
    d[, cols, drop = FALSE]
  })
  do.call(rbind, dfs)
}

# ---- Normalization --------------------------------------------------------------

#' Parse OGC timestamps ("2026-09-12T00:00:00+00:00", "...Z", with or
#' without fractional seconds) to UTC "YYYY-MM-DD HH:MM:SS".
#' @keywords internal
.to_utc_text <- function(x) {
  x <- as.character(x)
  base <- as.POSIXct(substr(x, 1, 19), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  off <- sub("^.{19}(\\.[0-9]+)?", "", x)
  secs <- rep(0, length(x))
  has_off <- grepl("^[+-][0-9]{2}:?[0-9]{2}$", off)
  if (any(has_off)) {
    o <- gsub(":", "", off[has_off])
    sgn <- ifelse(substr(o, 1, 1) == "-", -1, 1)
    secs[has_off] <- sgn * (as.numeric(substr(o, 2, 3)) * 3600 + as.numeric(substr(o, 4, 5)) * 60)
  }
  bad <- !(off %in% c("", "Z") | has_off)
  out <- format(base - secs, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  out[is.na(base) | bad] <- NA_character_
  out
}

#' Collapse the qualifier field (NULL / NA / character array) to text.
#' @keywords internal
.collapse_qualifier <- function(q, n) {
  if (is.null(q)) return(rep(NA_character_, n))
  if (is.list(q)) {
    return(vapply(q, function(e) {
      e <- e[!is.na(e)]
      if (length(e) == 0) NA_character_ else paste(as.character(e), collapse = ",")
    }, character(1)))
  }
  as.character(q)
}

#' Convert OGC properties into a validated, sorted, de-duplicated series.
#'
#' @param props data.frame of feature properties (must have time, value).
#' @param site_id Site id written to every row.
#' @param time_kind "datetime" (continuous) or "date" (daily).
#' @param value_name Output value column name, e.g. "discharge_cfs".
#' @return data.frame(site_no, <time_kind>, <value_name>, approval_status,
#'   qualifier); zero rows if nothing valid. Throws if time/value missing.
ogc_to_series <- function(props, site_id, time_kind, value_name) {
  if (!is.data.frame(props) || !all(c("time", "value") %in% names(props))) {
    stop("response is missing required fields 'time' and/or 'value'")
  }
  n <- nrow(props)
  t <- if (time_kind == "datetime") {
    .to_utc_text(props$time)
  } else {
    d <- suppressWarnings(as.Date(substr(as.character(props$time), 1, 10), format = "%Y-%m-%d"))
    ifelse(is.na(d), NA_character_, format(d, "%Y-%m-%d"))
  }
  out <- data.frame(
    site_no = rep(as.character(site_id), n),
    time = as.character(t),
    value = suppressWarnings(as.numeric(props$value)),
    # rep_len keeps zero-row pages (e.g. a chunk before a gauge's record
    # starts) valid instead of erroring on a length-1 NA fallback.
    approval_status = if ("approval_status" %in% names(props)) {
      as.character(props$approval_status)
    } else {
      rep_len(NA_character_, n)
    },
    qualifier = rep_len(.collapse_qualifier(props$qualifier, n), n),
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$time) & is.finite(out$value), , drop = FALSE]
  out <- out[order(out$time), , drop = FALSE]
  out <- out[!duplicated(out$time, fromLast = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  names(out)[names(out) == "time"] <- time_kind
  names(out)[names(out) == "value"] <- value_name
  out
}

# ---- Generic series fetchers ---------------------------------------------------

#' Recent continuous (15-minute) values for one site/parameter.
#'
#' Throws on failure; the public fetchers wrap this in tryCatch.
#' @return Series data.frame (see ogc_to_series), possibly zero rows.
ogc_fetch_continuous <- function(site_id, parameter_code, days, end_time,
                                 value_name, timeout_sec = HTTP_TIMEOUT_LATEST) {
  days <- as.numeric(days)
  if (length(days) != 1 || !is.finite(days) || days <= 0) stop("days must be a positive number")
  end_time <- as.POSIXct(end_time, tz = "UTC")
  start_time <- end_time - days * 86400
  fmt <- function(x) format(x, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  props <- ogc_get_items(
    "continuous",
    list(
      f = "json",
      monitoring_location_id = site_id,
      parameter_code = parameter_code,
      datetime = paste0(fmt(start_time), "/", fmt(end_time)),
      properties = "time,value,approval_status,qualifier",
      skipGeometry = "true",
      limit = OGC_PAGE_LIMIT
    ),
    timeout_sec = timeout_sec
  )
  ogc_to_series(props, site_id, "datetime", value_name)
}

#' Split [start, end] into consecutive windows of `years` years.
#' @return data.frame(start = Date, end = Date)
date_chunks <- function(start_date, end_date, years = DAILY_CHUNK_YEARS) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  if (is.na(start_date) || is.na(end_date) || start_date > end_date) {
    stop("invalid date range")
  }
  starts <- seq(start_date, end_date, by = paste(years, "years"))
  ends <- c(starts[-1] - 1, end_date)
  data.frame(start = starts, end = pmin(ends, end_date))
}

#' Daily values for one site/parameter/statistic over a date range,
#' fetched in bounded chunks. Any failing chunk fails the whole call (a
#' partial history is never returned as if complete).
#'
#' Throws on failure; the public fetchers wrap this in tryCatch.
#' @return Series data.frame (see ogc_to_series), possibly zero rows.
ogc_fetch_daily <- function(site_id, parameter_code, statistic_id, start_date, end_date,
                            value_name, timeout_sec = HTTP_TIMEOUT_DAILY,
                            chunk_years = DAILY_CHUNK_YEARS) {
  chunks <- date_chunks(start_date, end_date, chunk_years)
  parts <- vector("list", nrow(chunks))
  for (i in seq_len(nrow(chunks))) {
    if (.etl_verbose() && nrow(chunks) > 1) {
      message(sprintf(
        "    %s %s daily chunk %d/%d: %s..%s", site_id, parameter_code, i, nrow(chunks),
        chunks$start[i], chunks$end[i]
      ))
    }
    props <- ogc_get_items(
      "daily",
      list(
        f = "json",
        monitoring_location_id = site_id,
        parameter_code = parameter_code,
        statistic_id = statistic_id,
        datetime = paste0(format(chunks$start[i]), "/", format(chunks$end[i])),
        properties = "time,value,approval_status,qualifier",
        skipGeometry = "true",
        limit = OGC_PAGE_LIMIT
      ),
      timeout_sec = timeout_sec
    )
    parts[[i]] <- ogc_to_series(props, site_id, "date", value_name)
  }
  out <- do.call(rbind, parts)
  out <- out[order(out$date), , drop = FALSE]
  out <- out[!duplicated(out$date, fromLast = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Wrap a throwing fetch so it returns NULL on error or zero rows, noting why.
#' @keywords internal
.null_on_failure <- function(label, expr) {
  tryCatch(
    {
      out <- expr
      if (is.null(out) || !is.data.frame(out) || nrow(out) == 0) {
        etl_note_failure(paste0(label, ": no valid rows returned"))
        NULL
      } else {
        out
      }
    },
    error = function(e) {
      etl_note_failure(paste0(label, ": ", conditionMessage(e)))
      NULL
    }
  )
}
