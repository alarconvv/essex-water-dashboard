#!/usr/bin/env Rscript
# etl/run_etl.R
#
# ETL orchestrator: USGS OGC API (flow, groundwater, precipitation) and the
# U.S. Drought Monitor -> data/essexwater.sqlite (override with
# ESSEXWATER_DB_PATH). The Shiny app only ever reads that store.
#
# Principle: a dead upstream source never breaks the others. Every source
# step runs in its own tryCatch and writes one etl_runs row, success or
# failure. Nothing is fabricated: a failed fetch writes no data.
#
# Sources per run (etl_runs.source):
#   flow_latest:<site>  flow_daily:<site>  flow_percentiles:<site>   (each flow gauge)
#   groundwater_latest:<site>  groundwater_daily:<site>  groundwater_percentiles:<site>
#   precip_latest:<site>  precip_daily:<site>  precip_typical:<site>
#   drought:<fips>
#
# Daily history is incremental: the first run backfills from the history
# start in constants.R; later runs re-fetch from (latest stored date -
# daily_lookback_days) so provisional revisions are picked up. Pass
# full_refresh = TRUE to re-pull everything. Percentile / typical tables are
# computed from the *stored* daily history (etl/stats.R), so they are
# refreshed even when that run's daily fetch fails.
#
# Fetchers are injected (`fetchers = default_etl_fetchers()`), so tests pass
# fakes explicitly instead of patching function environments.
#
# Usage: Rscript etl/run_etl.R   (from anywhere; paths resolve from this file)
#
# The sourcing-vs-main-script guard is Source B's approach: side effects only
# happen when Rscript was launched on this file itself.

resolve_own_path <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_flag <- sub("^--file=", "", cmd_args[grep("^--file=", cmd_args)])
  if (length(file_flag) > 0 && basename(file_flag) == "run_etl.R") {
    return(normalizePath(file_flag))
  }
  for (i in rev(seq_along(sys.frames()))) {
    ofile <- sys.frame(i)$ofile
    if (!is.null(ofile) && basename(ofile) == "run_etl.R") {
      return(normalizePath(ofile))
    }
  }
  NA_character_
}

this_file <- resolve_own_path()
etl_dir <- if (!is.na(this_file)) dirname(this_file) else file.path(getwd(), "etl")
app_dir <- dirname(etl_dir)

is_main_script <- {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_flag <- sub("^--file=", "", cmd_args[grep("^--file=", cmd_args)])
  length(file_flag) > 0 && basename(file_flag) == "run_etl.R"
}

source(file.path(etl_dir, "constants.R"))
source(file.path(etl_dir, "db_schema.R"))
source(file.path(etl_dir, "http_common.R"))
source(file.path(etl_dir, "fetch_flow.R"))
source(file.path(etl_dir, "fetch_groundwater.R"))
source(file.path(etl_dir, "fetch_precip.R"))
source(file.path(etl_dir, "fetch_drought.R"))
source(file.path(etl_dir, "stats.R"))
source(file.path(etl_dir, "write_store.R"))

#' The live fetchers run_etl() uses by default.
#'
#' @return Named list of functions: flow_latest, flow_daily,
#'   groundwater_latest, groundwater_daily, precip_latest, precip_daily,
#'   drought. Signatures match etl/fetch_*.R.
default_etl_fetchers <- function() {
  list(
    flow_latest = fetch_flow_latest,
    flow_daily = fetch_flow_daily,
    groundwater_latest = fetch_groundwater_latest,
    groundwater_daily = fetch_groundwater_daily,
    precip_latest = fetch_precip_latest,
    precip_daily = fetch_precip_daily,
    drought = fetch_drought_status
  )
}

#' Run the full ETL against an open connection whose schema already exists.
#'
#' @param con DBI connection (caller runs ensure_schema()/seed_sites()).
#' @param flow_sites,groundwater_sites,precip_sites Character vectors of OGC
#'   monitoring_location_ids.
#' @param county_fips County FIPS code(s) for drought.
#' @param latest_days Trailing days of 15-minute data per site.
#' @param daily_lookback_days Days re-fetched before the latest stored date.
#' @param full_refresh If TRUE, re-pull daily history from the history start.
#' @param drought_days_back Incremental drought window: days re-fetched
#'   before the latest stored map_date.
#' @param drought_backfill_years Years of weekly USDM maps requested when the
#'   store has no rows for the county, when its earliest stored map_date is
#'   more than a week after the backfill start, or on full_refresh (one
#'   request; a live 2000-2026 range of 1,393 weeks returned in ~2.4 s).
#' @param today Reference date (percentiles use years before its year).
#' @param verbose Print a line as each source starts.
#' @param fetchers Named list of fetch functions; must contain every name in
#'   default_etl_fetchers() (no silent fallback to live fetchers).
#' @return Named list keyed by etl_runs source, each
#'   list(status, rows_written, error_message).
run_etl <- function(con,
                    flow_sites = FLOW_SITES,
                    groundwater_sites = GW_SITE,
                    precip_sites = PRECIP_SITE,
                    county_fips = ESSEX_COUNTY_FIPS,
                    latest_days = 30,
                    daily_lookback_days = 45,
                    full_refresh = FALSE,
                    drought_days_back = 14,
                    drought_backfill_years = 3,
                    today = Sys.Date(),
                    verbose = FALSE,
                    fetchers = default_etl_fetchers()) {
  required <- names(default_etl_fetchers())
  missing_fetchers <- setdiff(required, names(fetchers))
  if (length(missing_fetchers) > 0 || !all(vapply(fetchers[required], is.function, logical(1)))) {
    stop("fetchers must supply functions named: ", paste(required, collapse = ", "))
  }

  today <- as.Date(today)
  ref_year <- as.integer(format(today, "%Y"))
  run_summary <- list()

  run_step <- function(source_name, fetch_fn, write_fn) {
    if (verbose) message(sprintf("[etl] %s ...", source_name))
    started_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC")
    etl_clear_failure()
    result <- tryCatch(
      {
        data <- fetch_fn()
        if (is.null(data) || nrow(data) == 0) {
          why <- etl_last_failure()
          list(
            ok = FALSE, rows = 0L,
            msg = if (is.na(why)) "fetch returned NULL or empty result" else why
          )
        } else {
          list(ok = TRUE, rows = as.integer(write_fn(data)), msg = NA_character_)
        }
      },
      error = function(e) list(ok = FALSE, rows = 0L, msg = conditionMessage(e))
    )
    status <- if (isTRUE(result$ok)) "success" else "failure"
    finished_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC")
    tryCatch(
      log_etl_run(con, source_name, started_at, finished_at, status, result$rows, result$msg),
      error = function(e) {
        message(sprintf("WARNING: failed to log etl_runs row for %s: %s", source_name, conditionMessage(e)))
      }
    )
    # `<<-` targets run_etl()'s own frame (run_step is nested), not globalenv.
    run_summary[[source_name]] <<- list( # nolint: assignment_linter.
      status = status, rows_written = result$rows, error_message = result$msg
    )
    invisible(NULL)
  }

  daily_start <- function(kind, site, history_start) {
    if (isTRUE(full_refresh)) return(history_start)
    last <- latest_daily_date(con, kind, site)
    if (is.na(last)) history_start else max(history_start, last - daily_lookback_days)
  }

  stats_table <- function(kind, site, builder) {
    hist <- read_daily_series(con, kind, site)
    if (nrow(hist) == 0) stop("no daily history in store")
    tab <- builder(hist, reference_year = ref_year)
    if (all(tab$years_used == 0)) stop("no prior-year daily values in store")
    tab
  }

  # --- Streamflow ---------------------------------------------------------------
  for (site in flow_sites) {
    run_step(
      paste0("flow_latest:", site),
      function() fetchers$flow_latest(site, days = latest_days),
      function(df) upsert_flow_instantaneous(con, df)
    )
    run_step(
      paste0("flow_daily:", site),
      function() fetchers$flow_daily(site, daily_start("flow", site, FLOW_HISTORY_START), today),
      function(df) upsert_flow_daily(con, df)
    )
    run_step(
      paste0("flow_percentiles:", site),
      function() stats_table("flow", site, build_percentile_table),
      function(tab) upsert_flow_percentiles(con, site, tab)
    )
  }

  # --- Groundwater -------------------------------------------------------------
  for (site in groundwater_sites) {
    run_step(
      paste0("groundwater_latest:", site),
      function() fetchers$groundwater_latest(site, days = latest_days),
      function(df) upsert_gw_instantaneous(con, df)
    )
    run_step(
      paste0("groundwater_daily:", site),
      function() fetchers$groundwater_daily(site, daily_start("groundwater", site, GW_HISTORY_START), today),
      function(df) upsert_groundwater_daily(con, df)
    )
    run_step(
      paste0("groundwater_percentiles:", site),
      function() stats_table("groundwater", site, build_percentile_table),
      function(tab) upsert_groundwater_percentiles(con, site, tab)
    )
  }

  # --- Precipitation -----------------------------------------------------------
  for (site in precip_sites) {
    run_step(
      paste0("precip_latest:", site),
      function() fetchers$precip_latest(site, days = latest_days),
      function(df) upsert_precip_instantaneous(con, df)
    )
    run_step(
      paste0("precip_daily:", site),
      function() fetchers$precip_daily(site, daily_start("precip", site, PRECIP_HISTORY_START), today),
      function(df) upsert_precip_daily(con, df)
    )
    run_step(
      paste0("precip_typical:", site),
      function() stats_table("precip", site, build_precip_typical_table),
      function(tab) upsert_precip_typical(con, site, tab)
    )
  }

  # --- Drought -------------------------------------------------------------------
  # Backfill drought_backfill_years of weekly maps when the county has no
  # stored rows, when the stored history does not reach back to the backfill
  # start (e.g. a store created before backfill existed -- found on the live
  # run, which otherwise stayed at 3 weeks), or on full_refresh. Otherwise
  # re-fetch from the latest stored map_date minus drought_days_back. One
  # request per county: USDM returns multi-year ranges in seconds.
  drought_start <- function(fips) {
    backfill <- seq(today, by = paste0("-", drought_backfill_years, " years"), length.out = 2)[2]
    if (isTRUE(full_refresh)) return(backfill)
    first <- earliest_drought_date(con, fips)
    last <- latest_drought_date(con, fips)
    # Maps are weekly: allow one week of slack before declaring a gap.
    if (is.na(first) || is.na(last) || first > backfill + 7) return(backfill)
    min(today, last - drought_days_back)
  }

  for (fips in county_fips) {
    run_step(
      paste0("drought:", fips),
      function() fetchers$drought(fips, today = today, start_date = drought_start(fips)),
      function(df) upsert_drought_status(con, df)
    )
  }

  run_summary
}

#' Print the per-source summary returned by run_etl().
print_run_summary <- function(run_summary) {
  cat("\n=== ETL run summary ===\n")
  for (name in names(run_summary)) {
    r <- run_summary[[name]]
    line <- sprintf("[%-7s] %-46s rows=%d", toupper(r$status), name, r$rows_written)
    if (!is.na(r$error_message)) line <- paste0(line, "  error=", r$error_message)
    cat(line, "\n")
  }
  n_ok <- sum(vapply(run_summary, function(r) r$status == "success", logical(1)))
  cat(sprintf("=======================\n%d/%d sources succeeded.\n", n_ok, length(run_summary)))
  invisible(NULL)
}

if (is_main_script) {
  db_path <- Sys.getenv(
    "ESSEXWATER_DB_PATH",
    unset = file.path(app_dir, "data", "essexwater.sqlite")
  )
  dir.create(dirname(db_path), showWarnings = FALSE, recursive = TRUE)
  options(essexwater.etl_verbose = TRUE)

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  ensure_schema(con)
  seed_sites(con)
  run_summary <- run_etl(con, verbose = TRUE)
  print_run_summary(run_summary)
  DBI::dbDisconnect(con)
  # Exit 0 even with partial failures: they are recorded in etl_runs.
  invisible(NULL)
}
