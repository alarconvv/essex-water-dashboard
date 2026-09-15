# R/server.R
#
# Server logic for the Essex County Water Dashboard: Source A's UI, rewired
# from live API calls to the SQLite store.
#
# Why this file lives in R/ and not the app root: shiny:::shinyAppDir()
# checks for a root-level "server.R" first and, when one exists, switches to
# the legacy ui.R + server.R pattern and ignores app.R entirely (the app would
# serve "No UI defined"). app.R loads this file with source("R/server.R"),
# and R/_disable_autoload.r stops Shiny from sourcing it a second time.
# Keeping `server` in its own file also lets shiny::testServer() load it
# standalone (tests/testthat/test-integration-server.R).
#
# Globals this file relies on, set up by global.R (or by the integration test
# against a fixture store): `con`, `manual`, the R/helpers.R, R/components.R
# and R/data_access.R functions, and etl/constants.R.
#
# Data rules:
#   - Every number shown as data comes from R/data_access.R. Nothing here
#     synthesizes, randomizes, or hard-codes a measurement. The water budget
#     and the municipal-use card are the only numbers not from the store, and
#     both say "Illustrative" in text.
#   - Every output degrades to "No data" / "Unavailable" / an empty chart
#     with a message when the store is empty.
#   - Chart and map colors come only from CHART_COLORS (the design tokens in
#     www/dashboard_new.css and the neutrals in AESTHETIC_GUIDE.md §2.3/§7).
#   - Output ids are unique. Shiny binds only the first element with a given
#     id, so a repeated id would leave the second copy blank.

# ---- Constants -----------------------------------------------------------------

LOCAL_TZ <- "America/New_York"

CHART_COLORS <- c(
  ink = "#172533",
  water = "#336891",
  water_light = "#C6D5E0",
  ecology = "#C1DB70",
  municipal = "#F45932",
  warning = "#E75B52",
  secondary_text = "#687987",
  control_border = "#D8DEE3",
  typical_line = "#94A3B8",
  axis_line = "#E5E7EB",
  grid = "#F0F0F0",
  tick = "#6B7280",
  axis_title = "#9CA3AF",
  annotation = "#64748B",
  ecology_text = "#7A9430",
  ribbon = "rgba(198,213,224,0.40)",
  ribbon_edge = "rgba(198,213,224,0)",
  today_marker = "rgba(23,37,51,0.35)",
  municipal_light = "rgba(244,89,50,0.55)"
)

# USDM categories are an ordered severity scale: neutral for D0, stepping
# toward municipal and warning for D3-D4 (AESTHETIC_GUIDE.md §7).
DROUGHT_LEVELS <- c(
  d0 = "Abnormally Dry",
  d1 = "Moderate Drought",
  d2 = "Severe Drought",
  d3 = "Extreme Drought",
  d4 = "Exceptional Drought"
)
DROUGHT_COLORS <- c(
  d0 = CHART_COLORS[["control_border"]],
  d1 = CHART_COLORS[["typical_line"]],
  d2 = CHART_COLORS[["municipal_light"]],
  d3 = CHART_COLORS[["municipal"]],
  d4 = CHART_COLORS[["warning"]]
)

PERIOD_DAYS <- c(today = 1, "7d" = 7, "30d" = 30, "90d" = 90, "6m" = 180, "1y" = 365)
DEFAULT_PERIOD_DAYS <- 30
INSTANTANEOUS_MAX_DAYS <- 7
LOW_FLOW_THRESHOLDS <- c(1, 0.1, 0.01)
LOW_FLOW_YEARS <- 10
PRECIP_WINDOW_DAYS <- 7
PRECIP_MIN_TYPICAL_YEARS <- 3
PRECIP_SHORT_RECORD_NOTE <- "Comparison unavailable — gauge record began June 2025"
STALE_PRECIP_DAYS <- 2
STALE_GROUNDWATER_HOURS <- 48
HERO_RIBBON_NAME <- "Typical range (p10–p90)"
STORE_POLL_MS <- 10 * 60 * 1000

# Illustrative water budget, kept from Source A. These are NOT measurements:
# no live evapotranspiration, boundary inflow, or municipal-use source exists,
# so every element that shows them says "Illustrative" in text.
ILLUSTRATIVE_WATER_BUDGET <- list(
  "2023 Annual" = list(period = "2023 Annual", precip = 43.8, qin = 1.2, et = 23.4, qout = 18.9, human_use = 1.8)
)

# Endpoint text for the Evidence "data sources" panel, keyed by the part of
# etl_runs.source before the colon (see etl/run_etl.R).
SOURCE_CATALOG <- list(
  flow_latest = list(
    label = "River flow, 15-minute readings",
    endpoint = paste0(USGS_OGC_BASE_URL, "/collections/continuous/items (parameter 00060)")
  ),
  flow_daily = list(
    label = "River flow, daily means",
    endpoint = paste0(USGS_OGC_BASE_URL, "/collections/daily/items (00060, statistic 00003)")
  ),
  flow_percentiles = list(
    label = "River flow percentiles",
    endpoint = "Computed in R from stored daily means (same calendar day, prior complete years)"
  ),
  groundwater_latest = list(
    label = "Groundwater depth, 15-minute readings",
    endpoint = paste0(USGS_OGC_BASE_URL, "/collections/continuous/items (parameter 72019)")
  ),
  groundwater_daily = list(
    label = "Groundwater depth, daily means",
    endpoint = paste0(USGS_OGC_BASE_URL, "/collections/daily/items (72019, statistic 00003)")
  ),
  groundwater_percentiles = list(
    label = "Groundwater depth percentiles",
    endpoint = "Computed in R from stored daily means (same calendar day, prior complete years)"
  ),
  precip_latest = list(
    label = "Precipitation, 15-minute increments",
    endpoint = paste0(USGS_OGC_BASE_URL, "/collections/continuous/items (parameter 00045)")
  ),
  precip_daily = list(
    label = "Precipitation, daily totals",
    endpoint = paste0(USGS_OGC_BASE_URL, "/collections/daily/items (00045, statistic 00006)")
  ),
  precip_typical = list(
    label = "Typical daily precipitation",
    endpoint = "Computed in R from stored daily totals (short record; not used for comparisons)"
  ),
  drought = list(
    label = "Drought status (U.S. Drought Monitor)",
    endpoint = paste0(USDM_COUNTY_STATS_URL, " (county, cumulative percent area)")
  )
)

# ---- Pure helpers (no reactivity) -----------------------------------------------

#' Named vector of flow-gauge choices for the WATERSHED selector.
#' @return c("Parker River" = "USGS-01101000", ...), from etl/constants.R.
flow_site_choices <- function(sites = SITES) {
  flow <- Filter(function(s) identical(s$kind, "flow"), sites)
  stats::setNames(
    vapply(flow, function(s) s$site_no, character(1)),
    vapply(flow, function(s) s$watershed, character(1))
  )
}

#' Site descriptor from etl/constants.R, or a minimal stand-in.
site_meta <- function(site_no, sites = SITES) {
  hit <- Filter(function(s) identical(s$site_no, site_no), sites)
  if (length(hit) == 0) {
    return(list(site_no = site_no, name = site_no, watershed = site_no))
  }
  hit[[1]]
}

#' "USGS-01101000" -> "USGS 01101000" (Source A's source-line style).
usgs_label <- function(site_no) sub("^USGS-", "USGS ", site_no)

#' Days in a TIME PERIOD choice; unknown or missing -> 30.
period_days <- function(period) {
  if (is.null(period) || length(period) != 1 || is.na(period) || !period %in% names(PERIOD_DAYS)) {
    return(DEFAULT_PERIOD_DAYS)
  }
  unname(PERIOD_DAYS[[period]])
}

#' Locale-independent "Sep 15".
format_month_day <- function(date) {
  paste(month.abb[as.integer(format(date, "%m"))], as.integer(format(date, "%d")))
}

#' Locale-independent "Sep 15, 2026".
format_long_date <- function(date) paste0(format_month_day(date), ", ", format(date, "%Y"))

#' "Aug 17 – Sep 15, 2026" (same year) or "Sep 16, 2025 – Sep 15, 2026".
format_date_range <- function(start, end) {
  if (identical(format(start, "%Y"), format(end, "%Y"))) {
    paste0(format_month_day(start), " – ", format_month_day(end), ", ", format(end, "%Y"))
  } else {
    paste0(format_long_date(start), " – ", format_long_date(end))
  }
}

#' Fixed-decimal number text with a thousands mark.
format_number <- function(x, digits = 2) {
  format(round(x, digits), nsmall = digits, big.mark = ",", trim = TRUE, scientific = FALSE)
}

#' Percent text: whole numbers without decimals, otherwise one decimal.
format_pct <- function(x) {
  paste0(formatC(x, format = "f", digits = if (x == round(x)) 0 else 1), "%")
}

#' Stored UTC "YYYY-MM-DD HH:MM:SS" -> POSIXct UTC (NA when missing).
parse_utc <- function(utc_text) {
  as.POSIXct(as.character(utc_text), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
}

#' Stored UTC text -> the local (station) calendar date.
utc_to_local_date <- function(utc_text) {
  as.Date(format(parse_utc(utc_text), "%Y-%m-%d", tz = LOCAL_TZ))
}

#' Stored same-day percentiles for each date (NA where none is stored).
#'
#' @param perc get_percentiles() output.
#' @param dates Date vector.
#' @return data.frame(p10, p25, p50, p75, p90, years_used), one row per date.
percentiles_for_dates <- function(perc, dates) {
  key <- paste(as.integer(format(dates, "%m")), as.integer(format(dates, "%d")))
  idx <- match(key, paste(perc$month_nu, perc$day_nu))
  data.frame(
    p10 = perc$p10[idx], p25 = perc$p25[idx], p50 = perc$p50[idx],
    p75 = perc$p75[idx], p90 = perc$p90[idx], years_used = perc$years_used[idx]
  )
}

#' value / median * 100, NA where either is missing or the median is not > 0.
percent_of_median <- function(value, median) {
  ok <- is.finite(value) & is.finite(median) & median > 0
  out <- rep(NA_real_, length(value))
  out[ok] <- value[ok] / median[ok] * 100
  out
}

#' Source A's arrow + direction + reference text for a percent of typical.
median_comparison <- function(pct, reference = "seasonal median") {
  if (length(pct) != 1 || !is.finite(pct)) {
    return("Historical comparison unavailable")
  }
  diff <- round(pct - 100)
  if (diff < 0) {
    paste0("↓ ", abs(diff), "% below ", reference)
  } else if (diff > 0) {
    paste0("↑ ", diff, "% above ", reference)
  } else {
    paste0("At ", reference)
  }
}

#' Flow badge from classify_flow() (stored p10/p25/p75/p90), using Source
#' A's badge wording and classes.
flow_badge <- function(classification) {
  switch(
    classification,
    "Much below normal" = list(label = "Unusually low", class = "badge-critical"),
    "Below normal" = list(label = "Below typical", class = "badge-warning"),
    "Normal" = list(label = "Near typical", class = "badge-good"),
    "Above normal" = list(label = "Above typical", class = "badge-good"),
    "Much above normal" = list(label = "Well above typical", class = "badge-good"),
    list(label = "Comparison unavailable", class = "badge-warning")
  )
}

#' Plain-language percentile position for a classify_flow() bucket.
percentile_phrase <- function(classification) {
  switch(
    classification,
    "Much below normal" = "below the 10th percentile",
    "Below normal" = "between the 10th and 25th percentiles",
    "Normal" = "between the 25th and 75th percentiles",
    "Above normal" = "between the 75th and 90th percentiles",
    "Much above normal" = "above the 90th percentile",
    "outside the stored percentile record"
  )
}

#' Latest drought headline, e.g. "D2 active — Severe Drought".
drought_headline <- function(rows) {
  summary <- summarize_drought(rows)
  if (identical(summary, "Unavailable")) {
    return("Unavailable")
  }
  if (startsWith(summary, "No drought")) {
    return("No drought (D0–D4)")
  }
  code <- tolower(substr(summary, 1, 2))
  paste0(toupper(code), " active — ", DROUGHT_LEVELS[[code]])
}

#' Area text for the latest USDM row (percentages are cumulative).
drought_area_text <- function(latest) {
  codes <- c("d4", "d3", "d2", "d1", "d0")
  vals <- vapply(codes, function(cd) .as_finite_number(latest[[cd]]), numeric(1))
  active <- codes[!is.na(vals) & vals > 0]
  if (length(active) == 0) {
    return("No part of the county is in D0–D4 on the latest map")
  }
  top <- active[[1]]
  text <- paste0(
    format_pct(vals[[top]]), " of county area in ", toupper(top),
    " (", DROUGHT_LEVELS[[top]], ") or worse"
  )
  if (top != "d0" && is.finite(vals[["d0"]])) {
    text <- paste0(text, " · ", format_pct(vals[["d0"]]), " at least abnormally dry (D0)")
  }
  text
}

#' River flow over the selected period, joined to stored percentiles.
#'
#' Periods of 7 days or less use the stored 15-minute readings; longer
#' periods use daily means over every date in the window (dates without a
#' stored value keep flow = NA, so gaps stay visible). Percentiles are looked
#' up by each point's local calendar day. x is character (local time) so
#' plotly draws it without a time-zone shift.
#'
#' @return list(data = data.frame(x, date, flow, p10, p25, p50, p75, p90,
#'   years_used), resolution, source_prefix)
flow_period_frame <- function(con, site_no, days, end_date = Sys.Date(), now = Sys.time()) {
  perc <- get_percentiles(con, "flow", site_no)
  if (days <= INSTANTANEOUS_MAX_DAYS) {
    series <- get_instantaneous_series(con, "flow", site_no, days, now)
    dates <- as.Date(format(series$datetime, "%Y-%m-%d", tz = LOCAL_TZ))
    x <- format(series$datetime, "%Y-%m-%d %H:%M", tz = LOCAL_TZ)
    flow <- series$value
    resolution <- "15-minute readings"
    prefix <- "flow_latest:"
  } else {
    dates <- seq(end_date - (days - 1), end_date, by = "day")
    series <- get_daily_series(con, "flow", site_no, dates[[1]], end_date)
    x <- format(dates, "%Y-%m-%d")
    flow <- series$value[match(dates, series$date)]
    resolution <- "daily mean"
    prefix <- "flow_daily:"
  }
  data <- cbind(
    data.frame(x = x, date = dates, flow = flow, stringsAsFactors = FALSE),
    percentiles_for_dates(perc, dates)
  )
  list(data = data, resolution = resolution, source_prefix = paste0(prefix, site_no))
}

# ---- Chart builders (Source A / AESTHETIC_GUIDE.md §7 Plotly style) -------------

chart_xaxis <- function(...) {
  utils::modifyList(
    list(
      title = "", showgrid = FALSE, showline = TRUE, linecolor = CHART_COLORS[["axis_line"]],
      tickfont = list(size = 11, color = CHART_COLORS[["tick"]])
    ),
    list(...)
  )
}

chart_yaxis <- function(title, ...) {
  utils::modifyList(
    list(
      title = list(text = title, font = list(size = 10, color = CHART_COLORS[["axis_title"]])),
      showgrid = TRUE, gridcolor = CHART_COLORS[["grid"]], zeroline = FALSE,
      tickfont = list(size = 11, color = CHART_COLORS[["tick"]])
    ),
    list(...)
  )
}

finish_chart <- function(p, ..., showlegend = FALSE, margin = list(t = 10, r = 24, l = 0, b = 10)) {
  plotly::layout(
    p, ...,
    showlegend = showlegend, hovermode = "x unified", margin = margin,
    paper_bgcolor = "white", plot_bgcolor = "white"
  ) |>
    plotly::config(displayModeBar = FALSE)
}

#' Blank chart that says why it is blank.
empty_chart <- function(message) {
  plotly::plot_ly(
    x = 0, y = 0, type = "scatter", mode = "markers",
    marker = list(opacity = 0), hoverinfo = "skip"
  ) |>
    finish_chart(
      xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
      annotations = list(list(
        text = message, x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE,
        font = list(size = 12, color = CHART_COLORS[["secondary_text"]])
      ))
    )
}

#' Hero chart: "Compared with typical" (flow as % of the stored median) or
#' "Actual values" (stored p10-p90 ribbon, dashed stored median, solid flow).
build_hero_chart <- function(hero, mode, eco_threshold = NA_real_) {
  df <- hero$data
  if (!any(is.finite(df$flow)) && !any(is.finite(df$p50))) {
    return(empty_chart("No river flow in the store for this period"))
  }
  x_range <- c(min(df$x), max(df$x))

  if (identical(mode, "normalized")) {
    df$pct <- percent_of_median(df$flow, df$p50)
    if (!any(is.finite(df$pct))) {
      return(empty_chart("No stored flow and median for the same dates, so no comparison with typical"))
    }
    y_max <- max(150, ceiling(max(df$pct, na.rm = TRUE) * 1.1 / 25) * 25)
    yaxis <- chart_yaxis("Percent of seasonal median (%)", range = c(0, y_max), ticksuffix = "%")
    if (y_max <= 150) {
      yaxis$tickvals <- seq(0, 150, by = 25)
    }
    p <- plotly::plot_ly(df, x = ~x) |>
      plotly::add_trace(
        y = ~pct, type = "scatter", mode = "lines",
        name = "River flow (% of seasonal median)",
        line = list(color = CHART_COLORS[["water"]], width = 2.5),
        hovertemplate = "%{x}<br>River flow: %{y:.0f}% of seasonal median<extra></extra>"
      )
    return(finish_chart(
      p,
      xaxis = chart_xaxis(range = x_range),
      yaxis = yaxis,
      shapes = list(list(
        type = "line", x0 = x_range[[1]], x1 = x_range[[2]], y0 = 100, y1 = 100,
        line = list(color = CHART_COLORS[["typical_line"]], width = 1.5, dash = "dash")
      )),
      annotations = list(list(
        x = x_range[[2]], y = 100, text = "100% = Seasonal median", showarrow = FALSE,
        xanchor = "right", yanchor = "bottom", font = list(size = 10, color = CHART_COLORS[["annotation"]])
      ))
    ))
  }

  p <- plotly::plot_ly()
  band <- df[is.finite(df$p10) & is.finite(df$p90), , drop = FALSE]
  if (nrow(band) > 0) {
    p <- plotly::add_ribbons(
      p,
      data = band, x = ~x, ymin = ~p10, ymax = ~p90,
      name = HERO_RIBBON_NAME,
      fillcolor = CHART_COLORS[["ribbon"]],
      line = list(color = CHART_COLORS[["ribbon_edge"]]),
      hoverinfo = "skip"
    )
  }
  # Days without a stored value stay NA: plotly draws interior NAs as gaps
  # (no line bridges missing days) and trims leading/trailing empty days, so
  # the x-axis range is pinned to the full selected window below.
  if (any(is.finite(df$p50))) {
    p <- plotly::add_trace(
      p,
      data = df, x = ~x, y = ~p50, type = "scatter", mode = "lines",
      name = "Historical median flow",
      line = list(color = CHART_COLORS[["secondary_text"]], width = 1.75, dash = "dash"),
      hoverinfo = "skip"
    )
  }
  if (any(is.finite(df$flow))) {
    p <- plotly::add_trace(
      p,
      data = df, x = ~x, y = ~flow, type = "scatter", mode = "lines",
      name = "River flow (cfs)",
      line = list(color = CHART_COLORS[["water"]], width = 2.5),
      customdata = ~p50,
      hovertemplate = paste0(
        "<b>%{x}</b><br>",
        "River flow: <b>%{y:.2f} cfs</b><br>",
        "Median for this date: %{customdata:.2f} cfs",
        "<extra></extra>"
      )
    )
  }

  shapes <- list()
  annotations <- list()
  if (is.finite(eco_threshold)) {
    shapes <- list(list(
      type = "line", x0 = x_range[[1]], x1 = x_range[[2]], y0 = eco_threshold, y1 = eco_threshold,
      line = list(color = CHART_COLORS[["ecology"]], width = 1.5, dash = "dot")
    ))
    annotations <- list(list(
      x = x_range[[2]], y = eco_threshold,
      text = paste0("Ecological ref. (~", format_number(eco_threshold, 0), " cfs, provisional)"),
      showarrow = FALSE, xanchor = "right", yanchor = "bottom",
      font = list(size = 10, color = CHART_COLORS[["ecology_text"]])
    ))
  }
  finish_chart(
    p,
    xaxis = chart_xaxis(range = x_range),
    yaxis = chart_yaxis("Flow (cfs)", rangemode = "tozero"),
    shapes = shapes,
    annotations = annotations
  )
}

#' "Is this normal?" chart: this year's daily means over the stored p10-p90
#' ribbon and dashed median for every calendar day, with a "Today" marker.
build_seasonal_chart <- function(perc, daily, today = Sys.Date()) {
  year <- as.integer(format(today, "%Y"))
  dates <- as.Date(sprintf("%04d-%02d-%02d", year, perc$month_nu, perc$day_nu), format = "%Y-%m-%d")
  keep <- !is.na(dates)
  band <- data.frame(
    x = format(dates[keep], "%Y-%m-%d"),
    p10 = perc$p10[keep], p50 = perc$p50[keep], p90 = perc$p90[keep],
    stringsAsFactors = FALSE
  )
  band <- band[order(band$x), , drop = FALSE]
  daily <- daily[is.finite(daily$value), , drop = FALSE]
  has_band <- any(is.finite(band$p10) & is.finite(band$p90))
  if (!has_band && nrow(daily) == 0) {
    return(empty_chart("No stored percentiles or daily flow for this gauge yet"))
  }

  p <- plotly::plot_ly()
  if (has_band) {
    ribbon <- band[is.finite(band$p10) & is.finite(band$p90), , drop = FALSE]
    p <- plotly::add_ribbons(
      p,
      data = ribbon, x = ~x, ymin = ~p10, ymax = ~p90,
      name = "Prior years p10–p90",
      fillcolor = CHART_COLORS[["ribbon"]],
      line = list(color = CHART_COLORS[["ribbon_edge"]]),
      hoverinfo = "skip"
    )
  }
  if (any(is.finite(band$p50))) {
    p <- plotly::add_lines(
      p,
      data = band, x = ~x, y = ~p50,
      name = "Historical median",
      line = list(color = CHART_COLORS[["secondary_text"]], width = 1.75, dash = "dash"),
      hovertemplate = "Median: %{y:.2f} cfs<extra></extra>"
    )
  }
  if (nrow(daily) > 0) {
    current <- data.frame(x = format(daily$date, "%Y-%m-%d"), flow = daily$value, stringsAsFactors = FALSE)
    p <- plotly::add_lines(
      p,
      data = current, x = ~x, y = ~flow,
      name = paste(year, "daily mean flow"),
      line = list(color = CHART_COLORS[["water"]], width = 2.5),
      hovertemplate = "%{x}<br>Daily mean flow: <b>%{y:.2f} cfs</b><extra></extra>"
    )
    p <- plotly::add_markers(
      p,
      data = current[nrow(current), , drop = FALSE], x = ~x, y = ~flow,
      name = "Latest daily mean",
      marker = list(color = CHART_COLORS[["water"]], size = 8),
      hoverinfo = "skip"
    )
  }

  today_x <- format(today, "%Y-%m-%d")
  finish_chart(
    p,
    xaxis = chart_xaxis(range = c(sprintf("%04d-01-01", year), sprintf("%04d-12-31", year))),
    yaxis = chart_yaxis("Daily mean flow (cfs)", rangemode = "tozero"),
    shapes = list(list(
      type = "line", x0 = today_x, x1 = today_x, y0 = 0, y1 = 1, yref = "paper",
      line = list(color = CHART_COLORS[["today_marker"]], width = 1, dash = "dash")
    )),
    annotations = list(list(
      x = today_x, y = 1, yref = "paper", text = "Today", showarrow = FALSE,
      xanchor = "left", yanchor = "top", font = list(size = 10, color = CHART_COLORS[["ink"]])
    ))
  )
}

#' Plain-language takeaway for the seasonal chart.
#' @return list(headline, detail)
seasonal_takeaway <- function(perc, daily) {
  daily <- daily[is.finite(daily$value), , drop = FALSE]
  if (nrow(daily) == 0) {
    return(list(
      headline = "No daily flow stored for this year yet.",
      detail = "The shaded band still shows the prior-year range for each calendar day."
    ))
  }
  latest <- daily[nrow(daily), , drop = FALSE]
  p <- percentiles_for_dates(perc, latest$date)
  cls <- classify_flow(latest$value, p$p10, p$p25, p$p75, p$p90)
  when <- paste0(format_number(latest$value, 2), " cfs on ", format_month_day(latest$date))
  if (identical(cls, "Unknown")) {
    return(list(
      headline = paste0("Latest daily mean flow: ", when, "."),
      detail = "No stored percentiles for that calendar day, so it cannot be compared with prior years."
    ))
  }
  list(
    headline = paste0("Latest daily mean flow: ", when, " — ", tolower(cls), " for the date."),
    detail = paste0(
      "That is ", percentile_phrase(cls), " of daily means for the same calendar day in ",
      p$years_used, " prior years. Shaded band = 10th–90th percentile; dashed line = median."
    )
  )
}

#' Low-flow days: three small multiples (one per threshold), one color.
build_low_flow_chart <- function(lf, thresholds = LOW_FLOW_THRESHOLDS) {
  if (nrow(lf) == 0) {
    return(empty_chart("No daily river flow in the store for the last 10 years"))
  }
  panels <- lapply(thresholds, function(th) {
    d <- lf[lf$threshold == th, , drop = FALSE]
    plotly::plot_ly(
      d,
      x = ~yr, y = ~days, type = "bar",
      name = paste("Below", format(th), "cfs"),
      marker = list(color = CHART_COLORS[["water"]]),
      hovertemplate = paste0("%{x}: %{y} days below ", format(th), " cfs<extra></extra>")
    )
  })
  n <- length(panels)
  axes <- list()
  for (i in seq_len(n)) {
    axes[[if (i == 1) "xaxis" else paste0("xaxis", i)]] <- chart_xaxis(dtick = 2, tickformat = "d")
  }
  titles <- lapply(seq_len(n), function(i) {
    list(
      text = paste0("<b>Below ", format(thresholds[[i]]), " cfs</b>"),
      x = (i - 0.5) / n, y = 1.02, xref = "paper", yref = "paper",
      xanchor = "center", yanchor = "bottom", showarrow = FALSE,
      font = list(size = 11, color = CHART_COLORS[["ink"]])
    )
  })
  do.call(
    finish_chart,
    c(
      list(
        plotly::subplot(panels, nrows = 1, shareY = TRUE, margin = 0.03),
        yaxis = chart_yaxis("Days per year", rangemode = "tozero"),
        annotations = titles,
        bargap = 0.25,
        margin = list(t = 28, r = 8, l = 0, b = 10)
      ),
      axes
    )
  )
}

#' Plain-language takeaway for the low-flow chart.
low_flow_takeaway <- function(lf, this_year) {
  if (nrow(lf) == 0) {
    return(list(headline = "No low-flow history yet.", detail = "No daily river flow is stored for these years."))
  }
  main <- lf[lf$threshold == LOW_FLOW_THRESHOLDS[[1]], , drop = FALSE]
  worst <- main[which.max(main$days), , drop = FALSE]
  current <- main$days[main$yr == this_year]
  headline <- if (worst$days == 0) {
    "No days below 1 cfs in the last 10 years."
  } else {
    paste0("Most days below 1 cfs: ", worst$yr, " (", worst$days, " days).")
  }
  list(
    headline = headline,
    detail = paste0(
      this_year, " so far: ", if (length(current) == 1) current else 0,
      " days below 1 cfs (partial year). Each panel counts days whose daily mean flow was below ",
      "the threshold; a year without stored daily values counts as 0."
    )
  )
}

#' Non-overlapping category bands from cumulative USDM percentages.
#'
#' USDM county statistics are cumulative (d1 = percent of area in D1 or
#' worse). band_k = d_k - d_(k+1), clamped at 0, so the bands stack back up
#' to d0 and each color covers only its own category.
drought_bands <- function(rows) {
  codes <- names(DROUGHT_LEVELS)
  cum <- vapply(codes, function(cd) {
    v <- suppressWarnings(as.numeric(rows[[cd]]))
    ifelse(is.finite(v), v, 0)
  }, numeric(nrow(rows)))
  cum <- matrix(cum, nrow = nrow(rows), dimnames = list(NULL, codes))
  bands <- cum
  for (i in seq_len(length(codes) - 1)) {
    bands[, i] <- pmax(0, cum[, i] - cum[, i + 1])
  }
  list(cumulative = cum, bands = bands)
}

#' USDM D0-D4 history: weekly stepped, stacked area of category bands.
#'
#' ~3 years of weekly maps (150+ points) is too dense for bars. The stack's
#' top edge is the D0-or-worse share; D4 sits at the bottom so the severe
#' categories stay anchored to the axis. Hover shows the cumulative share.
build_drought_chart <- function(rows) {
  if (nrow(rows) == 0) {
    return(empty_chart("No U.S. Drought Monitor maps in the store yet"))
  }
  x <- format(rows$map_date, "%Y-%m-%d")
  split <- drought_bands(rows)
  p <- plotly::plot_ly()
  for (code in rev(names(DROUGHT_LEVELS))) {
    p <- plotly::add_trace(
      p,
      x = x, y = split$bands[, code],
      type = "scatter", mode = "lines", stackgroup = "drought",
      name = paste(toupper(code), DROUGHT_LEVELS[[code]]),
      line = list(width = 0, shape = "hv"),
      fillcolor = DROUGHT_COLORS[[code]],
      customdata = split$cumulative[, code],
      hovertemplate = paste0(toupper(code), " or worse: %{customdata:.1f}%<extra></extra>")
    )
  }
  finish_chart(
    p,
    showlegend = TRUE,
    legend = list(
      orientation = "h", x = 0, y = -0.18, traceorder = "reversed",
      font = list(size = 10, color = CHART_COLORS[["secondary_text"]])
    ),
    xaxis = chart_xaxis(type = "date"),
    yaxis = chart_yaxis("% of county area", range = c(0, 100), ticksuffix = "%", tickvals = seq(0, 100, by = 25)),
    margin = list(t = 10, r = 24, l = 0, b = 40)
  )
}

#' Plain-language summary of the whole stored drought record.
drought_history_summary <- function(rows) {
  n <- nrow(rows)
  cum <- drought_bands(rows)$cumulative
  since <- format_long_date(rows$map_date[[1]])
  weeks_d1 <- sum(cum[, "d1"] > 0)
  codes <- names(DROUGHT_LEVELS)
  seen <- codes[vapply(codes, function(cd) any(cum[, cd] > 0), logical(1))]
  worst_text <- if (length(seen) == 0) {
    "No week recorded any D0–D4 area."
  } else {
    worst <- seen[[length(seen)]]
    last_seen <- rows$map_date[[max(which(cum[, worst] > 0))]]
    paste0(
      "Most severe category recorded: ", toupper(worst), " (", DROUGHT_LEVELS[[worst]], "), last seen ",
      format_long_date(last_seen), "."
    )
  }
  paste0(
    "Across ", n, " weekly map", if (n == 1) "" else "s", " since ", since, ", part of the county was in D1 ",
    "(Moderate Drought) or worse in ", weeks_d1, " week", if (weeks_d1 == 1) "" else "s", ". ", worst_text
  )
}

#' Text for a possibly non-character manual-content field.
manual_text <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return("Not configured")
  }
  paste(as.character(unlist(x)), collapse = " ")
}

#' Learn-more / public-comment links. Only http(s) URLs become links; an
#' empty or unsafe URL renders no link at all.
management_links <- function(manual) {
  links <- list(
    if (is_safe_url(manual$learn_more_url)) {
      tags$a(
        href = manual$learn_more_url, class = "local-management-link",
        target = "_blank", rel = "noopener noreferrer", "Learn more"
      )
    },
    if (is_safe_url(manual$comment_url)) {
      tags$a(
        href = manual$comment_url, class = "local-management-link",
        target = "_blank", rel = "noopener noreferrer", "Public comment"
      )
    }
  )
  links <- Filter(Negate(is.null), links)
  if (length(links) == 0) {
    return(div(class = "local-management-meta", "Learn-more and public-comment links are not configured yet."))
  }
  div(class = "local-management-links", links)
}

#' One Source A category header (label + rule + question).
category_header <- function(class, label, question) {
  div(
    class = paste("condition-category", class),
    div(class = "category-header-row", span(class = "category-label", label), span(class = "category-rule")),
    div(class = "category-question", question)
  )
}

# ---- Server ------------------------------------------------------------------------

server <- function(input, output, session) {

  # --- Store refresh and selections ------------------------------------------
  # The ETL writes on its own schedule. etl_runs is checked every 10 minutes
  # and every data reactive depends on store_version(), so new runs show up
  # without a restart.
  store_version <- reactivePoll(
    intervalMillis = STORE_POLL_MS,
    session = session,
    checkFunc = function() {
      status <- get_source_status(con)
      paste(status$source, status$last_success, status$last_status, collapse = "|")
    },
    valueFunc = function() Sys.time()
  )

  selected_site <- reactive({
    site <- input$watershed
    if (is.null(site) || length(site) != 1 || !site %in% FLOW_SITES) SITE_PARKER else site
  })
  selected_days <- reactive(period_days(input$time_period))
  chart_mode <- reactive(if (identical(input$chart_mode, "actual")) "actual" else "normalized")
  site_info <- reactive(site_meta(selected_site()))

  updated_text <- function(prefix) format_last_updated(get_last_updated(con, prefix))
  eco_threshold <- function() .as_finite_number(manual$eco_flow_threshold_cfs)

  #' config.yml's eco_flow_threshold_cfs is a single provisional value that
  #' has only ever been confirmed against the Parker River gauge (see
  #' DEVELOPMENT_GUIDE.md Known limitations). Applying it to the Ipswich
  #' River gauge as well would silently compare Ipswich flow against a
  #' threshold nobody has validated for that river, so the comparison (card,
  #' chart reference line, and chart note) is shown only when Parker is
  #' selected; NA here means "no comparison", never "Ipswich's threshold".
  eco_threshold_active <- reactive({
    if (identical(selected_site(), SITE_PARKER)) eco_threshold() else NA_real_
  })

  flow_updated <- reactive({
    store_version()
    updated_text(paste0("flow_latest:", selected_site()))
  })
  flow_latest <- reactive({
    store_version()
    get_latest_reading(con, "flow", selected_site())
  })
  flow_perc <- reactive({
    store_version()
    get_percentiles(con, "flow", selected_site())
  })
  groundwater_latest <- reactive({
    store_version()
    get_latest_reading(con, "groundwater", GW_SITE)
  })
  groundwater_perc <- reactive({
    store_version()
    get_percentiles(con, "groundwater", GW_SITE)
  })
  precip_window <- reactive({
    store_version()
    get_precip_window_total(con, PRECIP_SITE, days = PRECIP_WINDOW_DAYS)
  })
  precip_typical <- reactive({
    window <- precip_window()
    if (is.na(window$end_date)) {
      return(list(typical_in = NA_real_, years_used = 0L))
    }
    get_precip_typical_window(
      con, PRECIP_SITE,
      days = PRECIP_WINDOW_DAYS, end_date = window$end_date, min_years = PRECIP_MIN_TYPICAL_YEARS
    )
  })
  drought_rows <- reactive({
    store_version()
    get_drought_status(con, ESSEX_COUNTY_FIPS)
  })
  drought_updated <- reactive({
    store_version()
    updated_text(paste0("drought:", ESSEX_COUNTY_FIPS))
  })
  ecology <- reactive({
    if (identical(selected_site(), SITE_PARKER)) {
      return(ecology_status(flow_latest()$value, eco_threshold()))
    }
    # Decision (pass 2 review): pin the ecology comparison to Parker River,
    # the only gauge the configured threshold has been confirmed for, rather
    # than silently reusing it for Ipswich.
    list(
      state = "unavailable",
      label = "Comparison unavailable",
      badge_class = "badge-warning",
      comparison = "Comparison unavailable — threshold set for Parker River only"
    )
  })
  hero <- reactive({
    store_version()
    flow_period_frame(con, selected_site(), selected_days())
  })
  current_year_daily <- reactive({
    store_version()
    today <- Sys.Date()
    get_daily_series(con, "flow", selected_site(), as.Date(paste0(format(today, "%Y"), "-01-01")), today)
  })
  low_flow <- reactive({
    store_version()
    this_year <- as.integer(format(Sys.Date(), "%Y"))
    get_low_flow_days(con, selected_site(), (this_year - LOW_FLOW_YEARS + 1):this_year, LOW_FLOW_THRESHOLDS)
  })

  # --- Header and view level ---------------------------------------------------

  output$header_date_range <- renderUI({
    end_date <- Sys.Date()
    start_date <- end_date - (selected_days() - 1)
    div(class = "dashboard-date-range", format_date_range(start_date, end_date))
  })

  output$header_updated <- renderText(flow_updated())

  output$view_level_dots <- renderUI({
    selected <- suppressWarnings(as.numeric(input$view_level))
    if (length(selected) != 1 || is.na(selected)) {
      selected <- 1
    }
    div(
      class = "view-level-dots",
      span(class = if (selected >= 1) "view-dot active" else "view-dot"),
      span(class = if (selected >= 2) "view-dot active" else "view-dot"),
      span(class = if (selected >= 3) "view-dot active" else "view-dot")
    )
  })

  output$conditions_subtitle <- renderText(
    paste0("Latest available observations for the ", site_info()$watershed, " watershed")
  )

  # --- Stress banner ---------------------------------------------------------------
  # TODO(stress-score): no validated stress formula exists. Source A showed a
  # hard-coded 62; the banner now shows the unavailable state and no number.
  # stress_status() throws on NA, so it is never called here.
  output$stress_banner <- renderUI({
    div(
      class = "current-conditions-section",
      div(
        class = "current-conditions-header",
        div(class = "current-conditions-label", "CURRENT CONDITIONS"),
        div(class = "current-conditions-updated", flow_updated())
      ),
      div(
        class = "current-conditions-card",
        div(
          class = "current-conditions-inner",
          div(
            class = "current-conditions-top",
            div(
              class = "current-conditions-copy",
              div(class = "stress-status-badge", "Watershed stress score — not yet available"),
              div(
                class = "stress-description",
                paste(
                  "Methodology pending; not derived from live data.",
                  "Preliminary — no score is shown until a validated method exists."
                )
              )
            )
          ),
          div(
            class = "stress-bar-section",
            div(class = "stress-gradient-track", `aria-hidden` = "true"),
            div(
              class = "stress-axis-labels",
              span("Typical"),
              span("Moderate Stress"),
              span("High Stress"),
              span("Critical Stress")
            )
          ),
          div(
            class = "stress-source-line",
            "Planned inputs: USGS streamflow · USGS groundwater · USGS precipitation · Municipal data"
          )
        )
      )
    )
  })

  # --- Five condition cards ------------------------------------------------------------

  output$condition_cards <- renderUI({
    site <- selected_site()

    # The "flow" and "ecology" card tooltips (R/components.R) describe a
    # specific gauge/river by name, so they are built here per selected
    # watershed instead of using components.R's Parker-only defaults.
    site_watershed <- site_info()$watershed
    site_desc <- paste0(sub(", MA$", ", Massachusetts", site_info()$name), ", station ", sub("^USGS-", "", site))
    flow_tooltip <- paste0(
      "The amount of water flowing through the ", site_watershed, " at the monitoring station.\n\n",
      "Data source: U.S. Geological Survey (USGS), ", site_desc, ".\n\n",
      "Update status: The dashboard retrieves the latest available ",
      "USGS measurement. This is near-real-time monitoring data, ",
      "not a guaranteed live reading. The observation time should ",
      "be displayed so users can see how recent the value is.\n\n",
      "Historical comparison: USGS historical discharge records ",
      "are used to compare current flow with typical conditions ",
      "for the same time of year."
    )
    eco_tooltip <- if (identical(site, SITE_PARKER)) {
      paste0(
        "Compares the latest Parker River flow at the USGS Byfield ",
        "gauge (station 01101000) with the ecological flow threshold ",
        "set in the dashboard's config.yml.\n\n",
        "Provisional: the threshold is a placeholder pending ",
        "confirmation from IRWA/PRCWA staff, not a regulatory standard. ",
        "USGS data are provisional and may be revised.\n\n",
        "Sustained flow at or below the threshold may stress fish and ",
        "other aquatic life."
      )
    } else {
      paste0(
        "The dashboard's config.yml holds one ecological flow threshold, ",
        "set for the Parker River gauge at Byfield (station 01101000). ",
        "It has not been confirmed for the ", site_watershed, ", so no ",
        "ecological comparison is shown while it is selected.\n\n",
        "Switch WATERSHED back to Parker River to see the ecological flow comparison."
      )
    }

    # River flow: latest stored reading vs stored percentiles for its local day.
    flow_now <- flow_latest()
    flow_value <- flow_now$value
    flow_pct <- percentiles_for_dates(flow_perc(), utc_to_local_date(flow_now$datetime))
    flow_class <- flow_badge(classify_flow(flow_value, flow_pct$p10, flow_pct$p25, flow_pct$p75, flow_pct$p90))
    if (!is.finite(flow_value)) {
      flow_class <- list(label = "Data unavailable", class = "badge-critical")
    }

    # Rainfall: real 7-day total; typical comparison only with >= 3 prior years.
    window <- precip_window()
    total <- window$total_in
    typical <- precip_typical()
    rain_stale_days <- if (is.na(window$end_date)) NA_integer_ else as.integer(Sys.Date() - window$end_date)
    rain_badge <- if (!is.finite(total)) {
      list(label = "Data unavailable", class = "badge-critical")
    } else if (!is.na(rain_stale_days) && rain_stale_days > STALE_PRECIP_DAYS) {
      list(label = "Data delayed", class = "badge-warning")
    } else {
      list(label = "7-day total", class = "badge-good")
    }
    rain_comparison <- if (!is.finite(total)) {
      "No rainfall totals in the store"
    } else if (typical$years_used >= PRECIP_MIN_TYPICAL_YEARS && is.finite(typical$typical_in)) {
      diff <- total - typical$typical_in
      reference <- paste0(" the ", format_number(typical$typical_in, 2), " in typical for these dates")
      if (diff < 0) {
        paste0("↓ ", format_number(abs(diff), 2), " in below", reference)
      } else if (diff > 0) {
        paste0("↑ ", format_number(diff, 2), " in above", reference)
      } else {
        paste0("At", reference)
      }
    } else {
      PRECIP_SHORT_RECORD_NOTE
    }
    rain_dates <- if (is.na(window$start_date)) {
      "Observation dates unavailable"
    } else {
      format_date_range(window$start_date, window$end_date)
    }

    # Groundwater: latest depth vs stored median depth for its local day.
    gw_now <- groundwater_latest()
    depth <- gw_now$value
    gw_date <- utc_to_local_date(gw_now$datetime)
    median_depth <- percentiles_for_dates(groundwater_perc(), gw_date)$p50
    if (!is.finite(depth) || !is.finite(median_depth)) {
      gw_comparison <- "Historical comparison unavailable"
      gw_badge <- list(label = "Comparison unavailable", class = "badge-warning")
    } else {
      gw_diff <- depth - median_depth
      if (gw_diff > 0) {
        gw_comparison <- paste0(format_number(gw_diff, 2), " ft deeper than seasonal median")
        gw_badge <- list(label = "Lower than seasonal median", class = "badge-warning")
      } else if (gw_diff < 0) {
        gw_comparison <- paste0(format_number(abs(gw_diff), 2), " ft shallower than seasonal median")
        gw_badge <- list(label = "Higher than seasonal median", class = "badge-good")
      } else {
        gw_comparison <- "At seasonal median"
        gw_badge <- list(label = "At seasonal median", class = "badge-good")
      }
    }
    gw_age_hours <- as.numeric(difftime(Sys.time(), parse_utc(gw_now$datetime), units = "hours"))
    if (!is.finite(depth)) {
      gw_badge <- list(label = "Data unavailable", class = "badge-critical")
    } else if (!is.finite(gw_age_hours)) {
      gw_badge <- list(label = "Date unavailable", class = "badge-warning")
    } else if (gw_age_hours > STALE_GROUNDWATER_HOURS) {
      gw_badge <- list(label = "Data delayed", class = "badge-warning")
    }
    gw_date_text <- if (is.na(gw_date)) "Observation date unavailable" else format_long_date(gw_date)

    eco <- ecology()
    flow_text <- if (is.finite(flow_value)) format_number(flow_value, 2) else "N/A"

    div(
      class = "conditions-scroll",
      div(
        class = "conditions-scroll-inner",
        div(
          class = "conditions-category-grid",
          category_header("category-availability", "Water Availability", "How much water is available?"),
          category_header("category-demand", "Water Demand", "How much water are we using?"),
          category_header("category-context", "Water Context", "What does this mean for the ecosystem?")
        ),
        div(
          class = "conditions-grid",
          condition_card(
            id = "flow",
            title = "River Flow",
            value = flow_text,
            unit = "cfs",
            badge = flow_class$label,
            badge_class = flow_class$class,
            source = paste0(usgs_label(site), " · Live · ", flow_updated()),
            comparison = median_comparison(percent_of_median(flow_value, flow_pct$p50)),
            tooltip_body_override = flow_tooltip
          ),
          condition_card(
            id = "rain",
            title = "Rainfall",
            value = if (is.finite(total)) format_number(total, 2) else "N/A",
            unit = "in",
            badge = rain_badge$label,
            badge_class = rain_badge$class,
            source = paste0("USGS Byfield gauge (single site) · ", rain_dates),
            comparison = rain_comparison
          ),
          condition_card(
            id = "groundwater",
            title = "Groundwater Level",
            value = if (is.finite(depth)) format_number(depth, 2) else "N/A",
            unit = "ft below land surface",
            badge = gw_badge$label,
            badge_class = gw_badge$class,
            source = paste0(usgs_label(GW_SITE), " (single well) · ", gw_date_text),
            comparison = gw_comparison
          ),
          condition_card(
            "pumping",
            "Municipal Use",
            "2.8",
            "MGD",
            "Illustrative",
            "badge-warning",
            "Municipal data · Illustrative",
            "↑ 18% above seasonal average"
          ),
          condition_card(
            id = "ecology",
            title = "Ecological Flow",
            value = flow_text,
            unit = "cfs",
            badge = eco$label,
            badge_class = eco$badge_class,
            source = if (identical(site, SITE_PARKER)) {
              paste0(usgs_label(site), " vs config.yml threshold (provisional)")
            } else {
              paste0(usgs_label(site), " · threshold configured for Parker River only")
            },
            comparison = eco$comparison,
            tooltip_body_override = eco_tooltip
          )
        )
      )
    )
  })

  # --- Drought status card -----------------------------------------------------------

  output$drought_status_card <- renderUI({
    rows <- drought_rows()
    latest <- if (nrow(rows) > 0) rows[nrow(rows), , drop = FALSE] else NULL
    meta <- c(
      "U.S. Drought Monitor · county-level · weekly",
      if (!is.null(latest)) paste("Map date", format_long_date(latest$map_date)),
      drought_updated()
    )
    div(
      class = "local-management-grid",
      div(
        class = "local-management-card",
        div(class = "local-management-label", "DROUGHT STATUS · ESSEX COUNTY"),
        div(class = "local-management-status", drought_headline(rows)),
        div(
          class = "local-management-meta",
          if (is.null(latest)) "No U.S. Drought Monitor maps in the store yet" else drought_area_text(latest)
        ),
        div(class = "local-management-meta", paste(meta, collapse = " · "))
      ),
      div(
        class = "local-management-explainer",
        div(class = "local-management-explainer-title", "Reading the drought categories"),
        div(
          class = "local-management-explainer-text",
          paste(
            "D0 Abnormally Dry · D1 Moderate · D2 Severe · D3 Extreme · D4 Exceptional.",
            "Percentages are cumulative: the share of the county in a category or any worse one.",
            "County-level status is broader than a single river gauge."
          )
        )
      )
    )
  })

  # --- Water budget (Illustrative, Source A) --------------------------------------------

  output$water_budget_ui <- renderUI({
    period <- input$water_budget_period
    if (is.null(period) || !period %in% names(ILLUSTRATIVE_WATER_BUDGET)) {
      period <- names(ILLUSTRATIVE_WATER_BUDGET)[[1]]
    }
    d <- ILLUSTRATIVE_WATER_BUDGET[[period]]
    delta_s <- (d$precip + d$qin) - (d$et + d$qout + d$human_use)
    marker_left <- min(100, max(0, (delta_s + 5) * 10))
    source_text <- paste("Illustrative ·", d$period)
    equation <- HTML("ΔS = P + Q<sub>in</sub> − ET − Q<sub>out</sub> − U<sub>net</sub>")

    tagList(
      div(
        class = "wb-hero",
        role = "status",
        `aria-label` = paste0(
          d$period, " illustrative water budget: ", sprintf("%+.1f", delta_s), " inches change in storage"
        ),
        div(
          class = "wb-hero-top-row",
          div(
            class = "wb-hero-left",
            div(
              class = "wb-hero-badge-row",
              span(class = "wb-year-badge", "WATER BUDGET · 2023"),
              span(class = "wb-provenance-badge", "◇ Illustrative")
            ),
            div(class = "wb-hero-title", "Change in watershed storage (ΔS)"),
            div(class = "wb-hero-equation", equation),
            div(class = "wb-hero-note", "Upper Parker River watershed · USGS 01101000 drainage area")
          ),
          div(
            class = "wb-hero-right",
            div(
              class = "wb-hero-value-row",
              span(class = "wb-hero-value", sprintf("%+.1f", delta_s)),
              span(class = "wb-hero-unit", "in")
            ),
            div(class = "wb-hero-desc", "Slight storage gain · Illustrative annual example")
          )
        ),
        div(
          class = "wb-balance",
          div(
            class = "wb-balance-track",
            div(class = "wb-balance-center"),
            div(class = "wb-balance-marker", style = paste0("left: ", marker_left, "%;"))
          ),
          div(
            class = "wb-balance-labels",
            span(class = "wb-balance-label-left", "Storage loss (ΔS < 0)"),
            span(class = "wb-balance-label-center", "Balance"),
            span(class = "wb-balance-label-right", "Storage gain (ΔS > 0)")
          )
        ),
        div(
          class = "wb-hero-disclosure",
          "Illustrative example — values demonstrate the method and are not current watershed measurements."
        )
      ),
      div(
        class = "conditions-scroll",
        div(
          class = "conditions-scroll-inner",
          div(
            class = "conditions-category-grid",
            category_header("category-availability", "Water Inputs", "What water is entering the watershed?"),
            category_header("category-demand", "Water Outputs", "What water is leaving the watershed?")
          ),
          div(
            class = "conditions-grid",
            condition_card(
              id = "wb_precip",
              title = "Precipitation (P)",
              value = sprintf("%.1f", d$precip),
              unit = "in",
              badge = "Annual total",
              badge_class = "badge-good",
              source = source_text,
              comparison = "Illustrative value — not a measured total",
              icon = tags$img(src = "icons/Precipitation.svg", alt = "", class = "condition-icon-img")
            ),
            condition_card(
              id = "wb_qin",
              title = "Water Inflow",
              value = sprintf("%.1f", d$qin),
              unit = "in",
              badge = "Annual total",
              badge_class = "badge-good",
              source = source_text,
              comparison = "Boundary inflow"
            ),
            condition_card(
              id = "wb_et",
              title = "Evapotranspiration",
              value = sprintf("%.1f", d$et),
              unit = "in",
              badge = "Annual total",
              badge_class = "badge-warning",
              source = source_text,
              comparison = "Water lost to atmosphere"
            ),
            condition_card(
              id = "wb_qout",
              title = "Water Outflow",
              value = sprintf("%.1f", d$qout),
              unit = "in",
              badge = "Annual total",
              badge_class = "badge-warning",
              source = source_text,
              comparison = "Water leaving the watershed"
            ),
            condition_card(
              id = "wb_human",
              title = "Human Water Use",
              value = sprintf("%.1f", d$human_use),
              unit = "in",
              badge = "Annual total",
              badge_class = "badge-warning",
              source = source_text,
              comparison = "Net human water use"
            )
          )
        )
      ),
      div(
        class = "wb-result-summary",
        div(
          class = "wb-result-main",
          div(
            class = "wb-result-value-row",
            span(class = "wb-result-delta", "ΔS"),
            span(class = "wb-result-value", sprintf("%+.1f", delta_s)),
            span(class = "wb-result-unit", "in")
          ),
          div(
            class = "wb-result-status",
            if (delta_s > 0) "Slight storage gain" else if (delta_s < 0) "Storage loss" else "Approximately balanced"
          ),
          div(
            class = "wb-result-description",
            if (delta_s > 0) {
              "Inputs slightly exceeded outputs during this illustrative year."
            } else if (delta_s < 0) {
              "Outputs slightly exceeded inputs during this illustrative year."
            } else {
              "Inputs and outputs were approximately equal during this illustrative year."
            }
          )
        ),
        div(
          class = "wb-result-signs",
          span(class = "wb-result-sign-item", strong("+"), " Storage gain"),
          span(class = "wb-result-sign-item", strong("−"), " Storage loss")
        )
      ),
      div(
        class = "wb-about-estimate",
        div(
          class = "wb-about-header",
          span(class = "wb-about-icon", "ⓘ"),
          span(class = "wb-about-title", "ABOUT THIS ESTIMATE")
        ),
        div(class = "wb-about-text", "All components use the same watershed boundary, year, and compatible units."),
        div(
          class = "wb-about-text",
          "Groundwater helps interpret storage trends, but does not directly measure watershed-wide ΔS."
        )
        # No "View methodology" link: config.yml (out of scope for this file)
        # has no dedicated methodology URL, and a link pointing at "#" is a
        # dead link. Re-add this once a real URL exists, gated by
        # is_safe_url() like management_links() above.
      )
    )
  })

  # --- Local water management (Summary, from config.yml) --------------------------------

  output$local_management <- renderUI({
    div(
      class = "local-management-grid",
      div(
        class = "local-management-card",
        # config.yml's permit_status/next_hearing/note carry no watershed
        # field (they are one manually maintained record, not one per
        # gauge), so this label neither names a specific river nor follows
        # the WATERSHED selector above -- doing either would claim
        # specificity the manual content does not have. Source A always
        # said "IPSWICH WITHDRAWAL PERMIT" here regardless of any selector;
        # pass 2 review found nothing in config.yml or the docs confirming
        # that scope, so the label was generalized (Known limitations,
        # DEVELOPMENT_GUIDE.md).
        div(class = "local-management-label", "WATER WITHDRAWAL PERMIT"),
        div(class = "local-management-status", manual_text(manual$permit_status)),
        div(class = "local-management-meta", paste("Next hearing:", manual_text(manual$next_hearing))),
        div(
          class = "local-management-meta",
          span(class = "wb-provenance-badge", "◇ Illustrative"),
          " ",
          manual_text(manual$note)
        ),
        management_links(manual)
      ),
      div(
        class = "local-management-explainer",
        div(class = "local-management-explainer-title", "Authorized withdrawal vs. safe yield"),
        div(
          class = "local-management-explainer-text",
          paste(
            "A withdrawal permit defines how much water a supplier is authorized to withdraw.",
            "Safe yield refers to the amount of water the system can reliably provide while accounting",
            "for hydrologic and environmental constraints."
          )
        )
      )
    )
  })

  # --- Hero chart: water availability ------------------------------------------------------

  output$water_avail_subtitle <- renderUI({
    h <- hero()
    end_date <- Sys.Date()
    start_date <- end_date - (selected_days() - 1)
    div(
      class = "chart-subtitle",
      paste0(
        site_info()$watershed, " · ", format_date_range(start_date, end_date), " · ",
        usgs_label(selected_site()), " ", h$resolution, " · ", updated_text(h$source_prefix)
      )
    )
  })

  output$hero_chart <- renderPlotly(build_hero_chart(hero(), chart_mode(), eco_threshold_active()))

  output$chart_html_legend <- renderUI({
    labels <- if (identical(chart_mode(), "normalized")) {
      c("River flow (% of seasonal median)", "100% = seasonal median")
    } else {
      c("River flow", "Historical median flow")
    }
    div(
      class = "chart-html-legend",
      div(class = "chart-legend-item", span(class = "chart-legend-line chart-legend-river"), span(labels[[1]])),
      div(class = "chart-legend-item", span(class = "chart-legend-line chart-legend-typical"), span(labels[[2]]))
    )
  })

  output$chart_conversion_note <- renderUI({
    years <- hero()$data$years_used
    years <- years[is.finite(years)]
    years_text <- if (length(years) > 0) paste0(" across ", max(years), " prior years") else ""
    municipal <- "Municipal water use is not shown: no live municipal data source is connected yet."
    eco_line <- if (is.finite(eco_threshold_active())) {
      "Dotted green line = provisional ecological reference from config.yml (Parker River only). "
    } else {
      "No ecological reference line: the configured threshold is Parker River only and is not shown for Ipswich. "
    }
    text <- if (identical(chart_mode(), "actual")) {
      paste0(
        "Shaded band = 10th–90th percentile of daily mean flow for the same calendar day", years_text,
        " (stored USGS daily values). ", eco_line,
        municipal
      )
    } else {
      paste0("100% = median daily mean flow for the same calendar day", years_text, ". ", municipal)
    }
    div(class = "chart-conversion-note", text)
  })

  output$chart_stats_bar <- renderUI({
    df <- hero()$data
    has_flow <- which(is.finite(df$flow))
    latest <- if (length(has_flow) > 0) df[max(has_flow), , drop = FALSE] else NULL

    if (identical(chart_mode(), "actual")) {
      river_value <- if (is.null(latest)) "No data" else paste0(format_number(latest$flow, 2), " cfs")
      comparison <- if (is.null(latest) || !is.finite(latest$p50)) {
        "Typical: unavailable"
      } else {
        paste0("Typical: ~", format_number(latest$p50, 2), " cfs")
      }
      comparison_class <- "stat-block-comparison"
      note <- "Median = prior years, same day"
    } else {
      pct <- if (is.null(latest)) NA_real_ else percent_of_median(latest$flow, latest$p50)
      river_value <- if (is.finite(pct)) paste0(round(pct), "%") else "No data"
      comparison <- median_comparison(pct, "seasonal median")
      direction <- if (!is.finite(pct)) NULL else if (pct < 100) "stat-below" else "stat-above"
      comparison_class <- paste(c("stat-block-comparison", direction), collapse = " ")
      note <- "100% = seasonal median"
    }

    div(
      class = "chart-stats-bar",
      div(
        class = "chart-stat-block",
        div(class = "stat-block-label", "RIVER FLOW"),
        div(class = "stat-block-value stat-water", span(river_value), span(class = "stat-provenance", "●")),
        div(class = comparison_class, comparison)
      ),
      div(class = "stat-divider", `aria-hidden` = "true"),
      div(
        class = "chart-stat-block",
        div(class = "stat-block-label", "MUNICIPAL WATER USE"),
        div(class = "stat-block-value", span("Not available")),
        div(class = "stat-block-comparison", "No live municipal data connected")
      ),
      div(class = "stat-note", note)
    )
  })

  # --- Details: is this normal? ------------------------------------------------------------

  output$seasonal_flow_subtitle <- renderText({
    store_version()
    paste0(
      site_info()$watershed, " · ", format(Sys.Date(), "%Y"), " daily mean flow vs prior years · ",
      usgs_label(selected_site()), " · ", updated_text(paste0("flow_daily:", selected_site()))
    )
  })

  output$seasonal_flow <- renderPlotly(build_seasonal_chart(flow_perc(), current_year_daily()))

  output$seasonal_flow_takeaway <- renderUI({
    t <- seasonal_takeaway(flow_perc(), current_year_daily())
    div(class = "chart-conversion-note", tags$b(t$headline), " ", t$detail)
  })

  # --- Details: low-flow days ------------------------------------------------------------------

  output$low_flow_subtitle <- renderText({
    this_year <- as.integer(format(Sys.Date(), "%Y"))
    paste0(
      site_info()$watershed, " · ", this_year - LOW_FLOW_YEARS + 1, "–", this_year,
      " · days with daily mean flow below each threshold · ", usgs_label(selected_site())
    )
  })

  output$low_flow_days <- renderPlotly(build_low_flow_chart(low_flow()))

  output$low_flow_takeaway <- renderUI({
    t <- low_flow_takeaway(low_flow(), as.integer(format(Sys.Date(), "%Y")))
    div(class = "chart-conversion-note", tags$b(t$headline), " ", t$detail)
  })

  # --- Details: drought history ----------------------------------------------------------------

  output$drought_history_subtitle <- renderText({
    rows <- drought_rows()
    span_text <- if (nrow(rows) > 0) {
      paste0(format_date_range(rows$map_date[[1]], rows$map_date[[nrow(rows)]]), " · ")
    } else {
      ""
    }
    paste0(
      "Essex County (FIPS ", ESSEX_COUNTY_FIPS, ") · U.S. Drought Monitor · county-level · weekly · ",
      span_text, drought_updated()
    )
  })

  output$drought_history <- renderPlotly(build_drought_chart(drought_rows()))

  output$drought_history_takeaway <- renderUI({
    rows <- drought_rows()
    if (nrow(rows) == 0) {
      return(div(
        class = "chart-conversion-note",
        tags$b("No drought maps stored yet."), " Weekly maps appear after the ETL runs."
      ))
    }
    latest <- rows[nrow(rows), , drop = FALSE]
    div(
      class = "chart-conversion-note",
      tags$b(paste0("Latest map (", format_long_date(latest$map_date), "): ", drought_headline(rows), ".")),
      " ",
      paste0(
        drought_area_text(latest), ". ", drought_history_summary(rows),
        " Each color is one category; the top edge is the share of the county at least abnormally dry."
      )
    )
  })

  # --- Details: evidence table -------------------------------------------------------------------

  output$evidence_table <- renderUI({
    site <- selected_site()
    flow_now <- flow_latest()
    today_pct <- percentiles_for_dates(flow_perc(), Sys.Date())
    window <- precip_window()
    gw_now <- groundwater_latest()
    rows <- drought_rows()
    eco <- ecology()

    flow_row <- if (is.finite(flow_now$value)) {
      paste0(format_number(flow_now$value, 2), " cfs (latest 15-minute reading) · USGS OGC API · ", flow_updated())
    } else {
      "No data"
    }
    pct_row <- if (is.finite(today_pct$p10) && is.finite(today_pct$p90)) {
      paste0(
        format_number(today_pct$p10, 2), "–", format_number(today_pct$p90, 2), " cfs (p10–p90) for today from ",
        today_pct$years_used, " prior years · computed from stored USGS daily means · ",
        updated_text(paste0("flow_percentiles:", site))
      )
    } else {
      "Unavailable"
    }
    rain_row <- if (is.finite(window$total_in)) {
      paste0(
        format_number(window$total_in, 2), " in (", format_date_range(window$start_date, window$end_date),
        ") · USGS OGC API · ", updated_text(paste0("precip_daily:", PRECIP_SITE))
      )
    } else {
      "No data"
    }
    gw_row <- if (is.finite(gw_now$value)) {
      paste0(
        format_number(gw_now$value, 2), " ft below land surface · USGS OGC API · ",
        updated_text(paste0("groundwater_latest:", GW_SITE))
      )
    } else {
      "No data"
    }
    drought_row <- if (nrow(rows) > 0) {
      paste0(
        drought_headline(rows), " · map date ", format_long_date(rows$map_date[[nrow(rows)]]),
        " · U.S. Drought Monitor · ", drought_updated()
      )
    } else {
      "Unavailable"
    }

    div(
      class = "local-management-card",
      div(class = "local-management-label", "EVIDENCE TABLE · LIVE METRICS SHOWN ON THIS PAGE"),
      tags$table(
        class = "evidence-rows",
        tags$tbody(
          evidence_row(paste("River flow ·", usgs_label(site)), flow_row),
          evidence_row(paste("Flow percentiles ·", usgs_label(site)), pct_row),
          evidence_row(paste("Rainfall, 7-day total ·", usgs_label(PRECIP_SITE)), rain_row),
          evidence_row(paste("Groundwater depth ·", usgs_label(GW_SITE)), gw_row),
          evidence_row(
            "Ecological flow comparison",
            paste(eco$comparison, "· threshold from config.yml (provisional)")
          ),
          evidence_row(paste("Drought status · FIPS", ESSEX_COUNTY_FIPS), drought_row)
        )
      )
    )
  })

  # --- Evidence: data sources ------------------------------------------------------------------------

  output$data_sources <- renderUI({
    store_version()
    status <- get_source_status(con)
    expected <- c(
      as.vector(outer(c("flow_latest:", "flow_daily:", "flow_percentiles:"), FLOW_SITES, paste0)),
      paste0(c("groundwater_latest:", "groundwater_daily:", "groundwater_percentiles:"), GW_SITE),
      paste0(c("precip_latest:", "precip_daily:", "precip_typical:"), PRECIP_SITE),
      paste0("drought:", ESSEX_COUNTY_FIPS)
    )
    sources <- c(expected, setdiff(status$source, expected))

    rows <- lapply(sources, function(src) {
      entry <- SOURCE_CATALOG[[sub(":.*$", "", src)]]
      label <- if (is.null(entry)) sub(":.*$", "", src) else entry$label
      endpoint <- if (is.null(entry)) "Unknown endpoint" else entry$endpoint
      hit <- status[status$source == src, , drop = FALSE]
      last_success <- if (nrow(hit) == 1 && !is.na(hit$last_success)) {
        sub("^Updated ", "", format_last_updated(hit$last_success))
      } else {
        "none recorded"
      }
      last_run <- if (nrow(hit) == 1) {
        error <- if (!is.na(hit$last_error) && nzchar(hit$last_error)) paste0(" (", hit$last_error, ")") else ""
        paste0(hit$last_status, error)
      } else {
        "never run"
      }
      evidence_row(
        paste0(label, " · ", sub("^[^:]*:", "", src)),
        paste0("Endpoint: ", endpoint, " · Last success: ", last_success, " · Last run: ", last_run)
      )
    })

    div(
      class = "local-management-card",
      div(class = "local-management-label", "DATA SOURCES · ENDPOINTS AND LAST SUCCESSFUL UPDATE"),
      div(
        class = "local-management-meta",
        paste(
          "The dashboard never calls these services while you browse. A scheduled ETL job (etl/run_etl.R)",
          "stores their data locally; the times below come from its run log. USGS data are provisional."
        )
      ),
      tags$table(class = "evidence-rows", tags$tbody(rows)),
      div(
        class = "local-management-meta",
        paste(
          "Any official water-use restrictions must come from your municipality, not this dashboard.",
          "This tool supports informed discussion; it does not replace official notices."
        )
      )
    )
  })

  # --- Evidence: gauge map -----------------------------------------------------------------------------

  output$gauge_map <- renderLeaflet({
    store_version()
    sites <- get_sites(con)
    sites <- sites[is.finite(sites$lat) & is.finite(sites$lon), , drop = FALSE]

    m <- leaflet::leaflet(options = leaflet::leafletOptions(scrollWheelZoom = FALSE)) |>
      leaflet::addTiles(options = leaflet::tileOptions(opacity = 0.55)) |>
      leaflet::setView(lng = -70.92, lat = 42.71, zoom = 11)

    if (nrow(sites) == 0) {
      return(m)
    }
    kind_label <- c(flow = "Stream gauge", groundwater = "Groundwater well", precip = "Precipitation gauge")
    is_well <- sites$kind == "groundwater"
    popup <- paste0(
      "<strong>", htmltools::htmlEscape(sites$name), "</strong><br>",
      htmltools::htmlEscape(unname(kind_label[sites$kind])), " · ", htmltools::htmlEscape(sites$site_no), "<br>",
      htmltools::htmlEscape(sites$watershed), " watershed"
    )
    m |>
      leaflet::addCircleMarkers(
        lng = sites$lon, lat = sites$lat,
        radius = ifelse(sites$kind == "flow", 9, 7),
        color = ifelse(is_well, CHART_COLORS[["ink"]], CHART_COLORS[["water"]]),
        weight = ifelse(is_well, 3, 1.5),
        fillColor = ifelse(is_well, CHART_COLORS[["water_light"]], CHART_COLORS[["water"]]),
        fillOpacity = 0.9,
        popup = popup,
        label = sites$name
      ) |>
      leaflet::addLegend(
        position = "bottomright",
        colors = c(CHART_COLORS[["water"]], CHART_COLORS[["water_light"]]),
        labels = c("Stream or precipitation gauge", "Groundwater well"),
        opacity = 1
      )
  })

  # --- Evidence: local water management (config.yml) ---------------------------------------------------

  output$local_management_evidence <- renderUI({
    threshold <- eco_threshold()
    div(
      class = "local-management-card",
      div(
        class = "local-management-label",
        "LOCAL WATER MANAGEMENT ", span(class = "wb-provenance-badge", "◇ Illustrative")
      ),
      div(
        class = "local-management-meta",
        "Manually maintained in config.yml; not verified against current permit records."
      ),
      tags$table(
        class = "evidence-rows",
        tags$tbody(
          evidence_row("Permit status", manual_text(manual$permit_status)),
          evidence_row("Next hearing", manual_text(manual$next_hearing)),
          evidence_row(
            "Ecological flow threshold",
            if (is.finite(threshold)) paste0(format_number(threshold, 1), " cfs (provisional)") else "Not configured"
          ),
          evidence_row("Note", manual_text(manual$note))
        )
      ),
      management_links(manual)
    )
  })

  # --- Evidence: water 101 ------------------------------------------------------------------------------

  output$water_101 <- renderUI({
    div(
      class = "local-management-explainer",
      div(class = "local-management-explainer-title", "Water 101 — how the pieces fit together"),
      div(
        class = "local-management-explainer-text",
        paste(
          "River flow reflects the balance between what falls as precipitation, what leaves through",
          "evapotranspiration and human withdrawals, and what is held in storage (soil, groundwater, wetlands).",
          "Conceptually:"
        )
      ),
      div(class = "wb-hero-equation", HTML("ΔS = P + Q<sub>in</sub> − ET − Q<sub>out</sub> − U<sub>net</sub>")),
      div(
        class = "local-management-explainer-text",
        paste(
          "This dashboard does not compute a live water-budget total. Evapotranspiration, boundary inflow,",
          "and municipal use have no free, real-time public source here, so a computed total would mean",
          "inventing at least one input. That is why the water budget above is labeled Illustrative."
        )
      ),
      div(
        class = "local-management-explainer-text",
        paste(
          "Reading the charts: solid lines are current measurements, dashed lines are historical medians,",
          "and shaded bands are the 10th–90th percentile range for the same calendar day in prior years."
        )
      )
    )
  })
}
