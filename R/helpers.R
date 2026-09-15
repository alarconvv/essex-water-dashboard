# R/helpers.R
#
# Pure helper functions for the Essex County Water Dashboard app layer.
# Zero DB access, zero network access -- everything here is a deterministic
# function of its arguments, which is what makes it unit-testable in
# complete isolation (see tests/testthat/test-helpers-*.R and
# tests/testthat/test-security-*.R).
#
# Ported from the architecture reference project (classify_flow,
# summarize_drought, is_safe_url, evidence_row, load_manual_content) and
# extended with small UI helpers the Source A layout needs (ecology_status,
# format_last_updated, resolve_config_path, resolve_db_path).
#
# Exceptions to "no I/O":
#   - load_manual_content() reads a YAML file, but never throws; it always
#     returns a well-formed list even when the file is missing or malformed.
#   - resolve_config_path() / resolve_db_path() read environment variables
#     (no filesystem access).

#' Normalize a scalar-ish argument to a single NA-safe value.
#'
#' Collapses NULL or zero-length vectors to NA_real_ so callers can safely
#' run is.na() on the result without hitting "argument is of length zero"
#' errors from if()/comparison operators.
#'
#' @param x Any value (NULL, numeric(0), a scalar, or a longer vector).
#' @return A single value: NA_real_ if x was NULL/zero-length, otherwise the
#'   first element of x.
.na_safe_scalar <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_real_)
  }
  x[[1]]
}

#' Coerce any value to a single finite numeric, or NA_real_.
#'
#' NULL, zero-length, NA, non-numeric strings (e.g. a hand-edited
#' config.yml value like "eight"), Inf/NaN, lists, and anything that fails
#' coercion all become NA_real_. Never throws or warns.
#'
#' @param x Any value.
#' @return A numeric scalar (finite) or NA_real_.
.as_finite_number <- function(x) {
  tryCatch(
    {
      x <- .na_safe_scalar(x)
      if (is.list(x) || is.factor(x)) {
        return(NA_real_)
      }
      if (is.logical(x)) {
        return(NA_real_)
      }
      num <- suppressWarnings(as.numeric(x))
      if (length(num) != 1 || !is.finite(num)) NA_real_ else num
    },
    error = function(e) NA_real_
  )
}

#' Classify a current flow reading against its historical percentiles.
#'
#' Five-bucket classification used for the "is this normal?" advisory:
#'   - current < p10                  -> "Much below normal"
#'   - p10 <= current < p25           -> "Below normal"
#'   - p25 <= current <= p75          -> "Normal"
#'   - p75 < current <= p90           -> "Above normal"
#'   - current > p90                  -> "Much above normal"
#' Safe against NA, NULL, and zero-length arguments in any position -- any
#' of those normalizes to NA and yields "Unknown" rather than throwing.
#'
#' @param current Current discharge value (numeric scalar).
#' @param p10,p25,p75,p90 Historical percentile thresholds (numeric scalars).
#' @return A single character string: one of "Much below normal",
#'   "Below normal", "Normal", "Above normal", "Much above normal", or
#'   "Unknown".
classify_flow <- function(current, p10, p25, p75, p90) {
  current <- .na_safe_scalar(current)
  p10 <- .na_safe_scalar(p10)
  p25 <- .na_safe_scalar(p25)
  p75 <- .na_safe_scalar(p75)
  p90 <- .na_safe_scalar(p90)

  if (is.na(current) || is.na(p10) || is.na(p25) || is.na(p75) || is.na(p90)) {
    return("Unknown")
  }

  if (current < p10) {
    "Much below normal"
  } else if (current < p25) {
    "Below normal"
  } else if (current <= p75) {
    "Normal"
  } else if (current <= p90) {
    "Above normal"
  } else {
    "Much above normal"
  }
}

#' Summarize a drought_status data frame into a single human-readable string.
#'
#' Looks at the most recent row (by map_date) and scans severity columns in
#' priority order D4 -> D0, returning the first one that is actively > 0
#' (NA is treated as not-active, never a match).
#'
#' @param dr A data.frame shaped like the drought_status table: columns
#'   fips, map_date, d0, d1, d2, d3, d4 (percent area in that drought
#'   category), one row per reporting date. Rows need not be pre-sorted.
#' @return A character string: "Unavailable" if dr is NULL or has 0 rows;
#'   "D4 active" / "D3 active" / ... / "D0 active" for the highest-priority
#'   active category; "No drought (D0-D4)" if all categories are zero/NA.
summarize_drought <- function(dr) {
  if (is.null(dr) || nrow(dr) == 0) {
    return("Unavailable")
  }

  # Defensive sort by map_date so we reliably read the latest report even
  # if the caller didn't pre-sort (e.g. rows came back in insertion order).
  ord <- order(dr$map_date)
  dr <- dr[ord, , drop = FALSE]
  last_row <- dr[nrow(dr), , drop = FALSE]

  cols <- c("d4", "d3", "d2", "d1", "d0")
  for (col in cols) {
    val <- last_row[[col]]
    val <- .na_safe_scalar(val)
    if (!is.na(val) && val > 0) {
      return(paste0(toupper(col), " active"))
    }
  }

  "No drought (D0-D4)"
}

#' Check whether a URL uses a safe (http/https) scheme.
#'
#' Used to gate any user-facing hyperlink (e.g. config.yml-sourced
#' learn_more_url / comment_url) against scheme-based XSS vectors like
#' `javascript:` or `data:` URIs. Never throws, regardless of input.
#'
#' @param url Any value; normally a character scalar.
#' @return TRUE if url is a non-NA, non-empty string starting with
#'   "http://" or "https://" (case-insensitive), FALSE otherwise.
is_safe_url <- function(url) {
  tryCatch(
    {
      if (is.null(url) || length(url) == 0) {
        return(FALSE)
      }
      url <- url[[1]]
      if (is.na(url) || !is.character(url)) {
        return(FALSE)
      }
      grepl("^https?://", url, ignore.case = TRUE)
    },
    error = function(e) FALSE
  )
}

#' Build one escaped label/value table row for the Evidence panel.
#'
#' Thin wrapper around htmltools tag builders so that automatic HTML
#' escaping of `value` (and `label`) can be unit-tested in isolation.
#' Deliberately does NOT wrap value in HTML() -- doing so would defeat
#' htmltools' escaping and reopen the XSS hole this function exists to close.
#'
#' @param label Row label (character scalar); rendered inside <b>.
#' @param value Row value (character scalar); rendered as an escaped text node.
#' @return An htmltools tag equivalent to
#'   tags$tr(tags$td(tags$b(label)), tags$td(value)).
evidence_row <- function(label, value) {
  htmltools::tags$tr(
    htmltools::tags$td(htmltools::tags$b(label)),
    htmltools::tags$td(value)
  )
}

#' Documented default content for load_manual_content() fallback cases.
.manual_content_defaults <- list(
  permit_status = "Not configured",
  next_hearing = "Not configured",
  eco_flow_threshold_cfs = NA_real_,
  learn_more_url = "",
  comment_url = "",
  note = "Not configured"
)

#' Resolve the manual-content config path.
#'
#' @param default Path used when ESSEXWATER_CONFIG_PATH is unset or empty.
#' @return Character scalar: the env var value if set and non-empty,
#'   otherwise `default`.
resolve_config_path <- function(default = "config.yml") {
  value <- Sys.getenv("ESSEXWATER_CONFIG_PATH", unset = "")
  if (nzchar(value)) value else default
}

#' Resolve the SQLite store path.
#'
#' @param default Path used when ESSEXWATER_DB_PATH is unset or empty.
#' @return Character scalar: the env var value if set and non-empty,
#'   otherwise `default`.
resolve_db_path <- function(default = "data/essexwater.sqlite") {
  value <- Sys.getenv("ESSEXWATER_DB_PATH", unset = "")
  if (nzchar(value)) value else default
}

#' Load manually-maintained Evidence/Local Water Management content.
#'
#' Reads a YAML file expected to have fields: permit_status, next_hearing,
#' eco_flow_threshold_cfs, learn_more_url, comment_url, note. Must NEVER
#' throw -- a bad config file degrades to placeholder content, not a crash.
#'
#' @param path Path to a YAML file. Defaults to resolve_config_path(), so
#'   ESSEXWATER_CONFIG_PATH overrides "config.yml".
#' @return A named list with all six fields present. Missing or unparseable
#'   file -> `.manual_content_defaults`. Otherwise the parsed YAML merged
#'   over the defaults via modifyList(), so omitted keys fall back to their
#'   documented placeholder instead of NULL.
load_manual_content <- function(path = resolve_config_path()) {
  if (is.null(path) || length(path) == 0 || is.na(path[[1]]) ||
        !is.character(path) || !file.exists(path[[1]])) {
    return(.manual_content_defaults)
  }

  tryCatch(
    {
      parsed <- yaml::read_yaml(path[[1]])
      if (is.null(parsed) || !is.list(parsed)) {
        return(.manual_content_defaults)
      }
      utils::modifyList(.manual_content_defaults, parsed)
    },
    error = function(e) .manual_content_defaults,
    warning = function(w) .manual_content_defaults
  )
}

#' Format a cfs value for card text: at most one decimal, thousands mark.
.format_cfs <- function(x) {
  format(round(x, 1), big.mark = ",", trim = TRUE, scientific = FALSE)
}

#' Compare live flow against the ecological flow threshold.
#'
#' Drives the "ecology" condition card: latest Parker River flow vs
#' `eco_flow_threshold_cfs` from config.yml (a provisional placeholder).
#' Badge classes reuse Source A's existing condition-badge classes
#' (www/styles.css): badge-good, badge-critical, badge-warning. The
#' unavailable state uses badge-warning, matching Source A's own
#' "Comparison unavailable" flow badge.
#'
#' @param flow_cfs Latest flow (numeric scalar; NULL/NA/non-numeric ok).
#' @param threshold_cfs Ecological flow threshold (numeric scalar; NULL/NA/
#'   non-numeric ok).
#' @return list(state, label, badge_class, comparison), all character
#'   scalars:
#'   - flow > threshold:  state "above", label "Above eco-flow threshold",
#'     badge_class "badge-good"
#'   - flow <= threshold: state "at_or_below", label
#'     "At or below eco-flow threshold", badge_class "badge-critical"
#'   - either missing:    state "unavailable", label
#'     "Comparison unavailable", badge_class "badge-warning"
ecology_status <- function(flow_cfs, threshold_cfs) {
  flow <- .as_finite_number(flow_cfs)
  threshold <- .as_finite_number(threshold_cfs)

  if (is.na(flow) || is.na(threshold)) {
    comparison <- if (is.na(flow) && is.na(threshold)) {
      "Live flow and eco-flow threshold unavailable"
    } else if (is.na(flow)) {
      "Live flow unavailable"
    } else {
      "Eco-flow threshold not configured"
    }
    return(list(
      state = "unavailable",
      label = "Comparison unavailable",
      badge_class = "badge-warning",
      comparison = comparison
    ))
  }

  thr_txt <- paste0(.format_cfs(threshold), " cfs")

  if (flow > threshold) {
    list(
      state = "above",
      label = "Above eco-flow threshold",
      badge_class = "badge-good",
      comparison = paste0(
        .format_cfs(flow - threshold), " cfs above ",
        thr_txt, " threshold (provisional)"
      )
    )
  } else if (flow == threshold) {
    list(
      state = "at_or_below",
      label = "At or below eco-flow threshold",
      badge_class = "badge-critical",
      comparison = paste0("At the ", thr_txt, " threshold (provisional)")
    )
  } else {
    list(
      state = "at_or_below",
      label = "At or below eco-flow threshold",
      badge_class = "badge-critical",
      comparison = paste0(
        .format_cfs(threshold - flow), " cfs below ",
        thr_txt, " threshold (provisional)"
      )
    )
  }
}

#' Parse a timestamp-ish value into POSIXct (UTC) or Date; NULL on failure.
#'
#' Accepts POSIXct/POSIXlt, Date, numeric epoch seconds, or character in
#' ISO-8601-like forms: "YYYY-MM-DD", "YYYY-MM-DD HH:MM[:SS[.fff]]",
#' "YYYY-MM-DDTHH:MM[:SS[.fff]]" with optional "Z" or "+HH:MM"/"-HHMM"
#' offset. Character values without an offset are interpreted as UTC (the
#' ETL writes UTC).
.parse_timestamp <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NULL)
  }
  x <- x[1]

  if (inherits(x, "Date")) {
    return(if (is.na(x)) NULL else x)
  }
  if (inherits(x, "POSIXt")) {
    x <- as.POSIXct(x)
    return(if (is.na(x)) NULL else x)
  }
  if (is.factor(x)) {
    x <- as.character(x)
  }
  if (is.numeric(x)) {
    if (!is.finite(x)) {
      return(NULL)
    }
    return(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"))
  }
  if (!is.character(x) || is.na(x)) {
    return(NULL)
  }

  s <- trimws(x)
  if (!nzchar(s)) {
    return(NULL)
  }

  if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", s)) {
    d <- as.Date(s, format = "%Y-%m-%d")
    return(if (is.na(d)) NULL else d)
  }

  offset_secs <- 0
  m <- regmatches(s, regexec("^(.*[0-9])(Z|[+-][0-9]{2}:?[0-9]{2})$", s))[[1]]
  if (length(m) == 3) {
    s <- m[2]
    off <- m[3]
    if (off != "Z") {
      digits <- gsub(":", "", substring(off, 2))
      sign <- if (substr(off, 1, 1) == "-") -1 else 1
      offset_secs <- sign * (as.integer(substr(digits, 1, 2)) * 3600 +
                               as.integer(substr(digits, 3, 4)) * 60)
    }
  }

  s <- sub("T", " ", s, fixed = TRUE)
  if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{1,2}:[0-9]{2}(:[0-9]{2}(\\.[0-9]+)?)?$", s)) {
    return(NULL)
  }
  fmt <- if (grepl("^[^:]*:[^:]*$", s)) "%Y-%m-%d %H:%M" else "%Y-%m-%d %H:%M:%OS"
  p <- as.POSIXct(strptime(s, format = fmt, tz = "UTC"))
  if (is.na(p)) {
    return(NULL)
  }
  p - offset_secs
}

#' Human-readable "last updated" text for store-sourced numbers.
#'
#' Locale-independent (month abbreviations and AM/PM are built manually).
#'
#' @param timestamp POSIXct/POSIXlt, Date, numeric epoch seconds, or an
#'   ISO-8601-like character string (see .parse_timestamp). NULL, NA,
#'   empty, or unparseable input is allowed.
#' @param tz Display time zone for date-times (default "America/New_York").
#'   Ignored for Date / date-only input.
#' @return Character scalar: "Updated Sep 14, 2026 3:05 PM" for date-times,
#'   "Updated Sep 14, 2026" for dates, or "Not yet updated" when the input
#'   is missing or cannot be parsed. Never throws.
format_last_updated <- function(timestamp, tz = "America/New_York") {
  fallback <- "Not yet updated"
  tryCatch(
    {
      p <- .parse_timestamp(timestamp)
      if (is.null(p)) {
        return(fallback)
      }

      if (inherits(p, "Date")) {
        lt <- as.POSIXlt(p, tz = "UTC")
        return(sprintf(
          "Updated %s %d, %d",
          month.abb[lt$mon + 1], lt$mday, lt$year + 1900L
        ))
      }

      lt <- as.POSIXlt(p, tz = tz)
      hour12 <- lt$hour %% 12
      if (hour12 == 0) hour12 <- 12
      ampm <- if (lt$hour < 12) "AM" else "PM"
      sprintf(
        "Updated %s %d, %d %d:%02d %s",
        month.abb[lt$mon + 1], lt$mday, lt$year + 1900L,
        hour12, lt$min, ampm
      )
    },
    error = function(e) fallback,
    warning = function(w) fallback
  )
}
