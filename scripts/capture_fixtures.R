#!/usr/bin/env Rscript
# scripts/capture_fixtures.R
#
# Calls each live endpoint the ETL uses once and saves the raw response
# bodies under tests/testthat/fixture-data/ for the mocked unit tests (the
# test suite itself makes zero network calls). Large responses are trimmed
# to a few features (the most recent by time) and their rel="next" links are
# removed so each trimmed fixture is a valid final page. One deliberately
# untrimmed 2-row page keeps its real next link for pagination tests.
#
# Every request has a 30 s timeout. No API keys are involved.
# Re-run only when upstream response shapes are suspected to have changed:
#   Rscript scripts/capture_fixtures.R

script_args <- commandArgs(trailingOnly = FALSE)
script_path_arg <- sub("^--file=", "", script_args[grep("^--file=", script_args)])
scripts_dir <- if (length(script_path_arg) > 0) {
  dirname(normalizePath(script_path_arg))
} else {
  file.path(getwd(), "scripts")
}
app_dir <- dirname(scripts_dir)
source(file.path(app_dir, "etl", "constants.R"))

fixture_dir <- file.path(app_dir, "tests", "testthat", "fixture-data")
dir.create(fixture_dir, showWarnings = FALSE, recursive = TRUE)

TIMEOUT <- 30
KEEP <- 6
today <- Sys.Date()

fetch_body <- function(req, accept = NULL) {
  if (!is.null(accept)) req <- httr2::req_headers(req, Accept = accept)
  resp <- req |>
    httr2::req_timeout(TIMEOUT) |>
    httr2::req_user_agent("essex-water-dashboard-etl fixture capture") |>
    httr2::req_perform()
  list(
    body = httr2::resp_body_string(resp),
    ctype = httr2::resp_header(resp, "content-type")
  )
}

ogc_req <- function(collection, query) {
  httr2::request(USGS_OGC_BASE_URL) |>
    httr2::req_url_path_append("collections", collection, "items") |>
    httr2::req_url_query(!!!query)
}

save_trimmed <- function(body, name, keep = KEEP, drop_next = TRUE) {
  x <- jsonlite::fromJSON(body, simplifyVector = FALSE)
  feats <- x$features
  if (length(feats) > keep) {
    times <- vapply(feats, function(f) as.character(f$properties$time), character(1))
    feats <- feats[order(times)]
    feats <- feats[seq(length(feats) - keep + 1, length(feats))]
  }
  x$features <- feats
  x$numberReturned <- length(feats)
  if (drop_next && !is.null(x$links)) {
    x$links <- Filter(function(l) !identical(l$rel, "next"), x$links)
  }
  txt <- jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", pretty = TRUE, digits = NA)
  writeLines(txt, file.path(fixture_dir, name))
  cat(sprintf("  %-34s %d feature(s)\n", name, length(feats)))
}

iso <- function(t) format(t, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
recent_window <- paste0(iso(Sys.time() - 86400), "/", iso(Sys.time()))
daily_window <- paste0(format(today - 14), "/", format(today))

continuous_query <- function(site, param) {
  list(
    f = "json", monitoring_location_id = site, parameter_code = param,
    datetime = recent_window, properties = "time,value,approval_status,qualifier",
    skipGeometry = "true", limit = 200
  )
}
daily_query <- function(site, param, stat, window = daily_window, limit = 200) {
  list(
    f = "json", monitoring_location_id = site, parameter_code = param,
    statistic_id = stat, datetime = window,
    properties = "time,value,approval_status,qualifier",
    skipGeometry = "true", limit = limit
  )
}

capture <- function(collection, query, name, ...) {
  save_trimmed(fetch_body(ogc_req(collection, query))$body, name, ...)
}

cat("USGS OGC fixtures:\n")
capture("continuous", continuous_query(SITE_PARKER, PARAM_DISCHARGE), "usgs_flow_latest.json")
capture("daily", daily_query(SITE_PARKER, PARAM_DISCHARGE, STAT_MEAN), "usgs_flow_daily.json")
capture(
  "daily", daily_query(SITE_PARKER, PARAM_DISCHARGE, STAT_MEAN, limit = 2),
  "usgs_flow_daily_page1.json",
  keep = 2, drop_next = FALSE
)
capture("continuous", continuous_query(GW_SITE, PARAM_GW_DEPTH), "usgs_groundwater_latest.json")
capture("daily", daily_query(GW_SITE, PARAM_GW_DEPTH, STAT_MEAN), "usgs_groundwater_daily.json")
capture("continuous", continuous_query(PRECIP_SITE, PARAM_PRECIP), "usgs_precip_latest.json")
capture("daily", daily_query(PRECIP_SITE, PARAM_PRECIP, STAT_SUM), "usgs_precip_daily.json")
# A valid FeatureCollection with zero features: precipitation before the
# gauge's record begins (2025-06-28).
save_trimmed(
  fetch_body(ogc_req("daily", daily_query(PRECIP_SITE, PARAM_PRECIP, STAT_SUM, window = "1990-01-01/1990-01-31")))$body,
  "usgs_empty.json"
)

cat("USDM drought fixtures:\n")
usdm_req <- httr2::request(USDM_COUNTY_STATS_URL) |>
  httr2::req_url_query(
    aoi = ESSEX_COUNTY_FIPS,
    startdate = format(today - 14, "%m/%d/%Y"),
    enddate = format(today, "%m/%d/%Y"),
    statisticsType = 1
  )
json <- fetch_body(usdm_req, accept = "application/json")
writeLines(json$body, file.path(fixture_dir, "usdm_drought_success.json"))
cat(sprintf("  %-34s content-type %s\n", "usdm_drought_success.json", json$ctype))
csv <- fetch_body(usdm_req) # no Accept header: reproduces the CSV default
writeLines(csv$body, file.path(fixture_dir, "usdm_drought_csv_default.txt"))
cat(sprintf("  %-34s content-type %s\n", "usdm_drought_csv_default.txt", csv$ctype))

cat("\nFixtures written to", fixture_dir, "\n")
