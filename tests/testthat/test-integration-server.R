# tests/testthat/test-integration-server.R
#
# Gate 2 -- integration. Drives the REAL R/server.R with shiny::testServer()
# against temp SQLite stores from helper-db.R (build_fixture_db() /
# build_empty_db()). No mocks of R/data_access.R, no network, and the real
# data/essexwater.sqlite is never opened.
#
# server() reads the globals `con` and `manual` (set by global.R in the real
# app). use_store() swaps both in the global environment for one test and
# restores them afterwards.
#
# Fixture values asserted below are documented at the top of helper-db.R.

suppressPackageStartupMessages({
  library(shiny)
  library(plotly)
  library(leaflet)
})

.root <- normalizePath(testthat::test_path("..", ".."))
source(file.path(.root, "etl", "constants.R"), local = FALSE)
source(file.path(.root, "R", "data_access.R"), local = FALSE)
source(file.path(.root, "R", "server.R"), local = FALSE)

# ---- Helpers -------------------------------------------------------------------

use_store <- function(build_fn, config_path = file.path(.root, "config.yml")) {
  store <- build_fn()
  genv <- globalenv()
  saved <- lapply(c(con = "con", manual = "manual"), function(nm) {
    if (exists(nm, envir = genv, inherits = FALSE)) list(get(nm, envir = genv)) else NULL
  })
  assign("con", store, envir = genv)
  assign("manual", load_manual_content(config_path), envir = genv)
  list(
    con = store,
    cleanup = function() {
      if (DBI::dbIsValid(store)) DBI::dbDisconnect(store)
      for (nm in names(saved)) {
        if (is.null(saved[[nm]])) {
          if (exists(nm, envir = genv, inherits = FALSE)) rm(list = nm, envir = genv)
        } else {
          assign(nm, saved[[nm]][[1]], envir = genv)
        }
      }
    }
  )
}

html_of <- function(x) paste(x$html, collapse = "")

# The HTML of one condition card (from its id up to the next card).
card_html <- function(cards, id) {
  rest <- substring(cards, regexpr(paste0('id="card-', id, '"'), cards, fixed = TRUE))
  nxt <- regexpr("<button", substring(rest, 2), fixed = TRUE)
  if (nxt > 0) substring(rest, 1, nxt) else rest
}

plot_traces <- function(json) jsonlite::fromJSON(json, simplifyVector = FALSE)$x$data
trace_named <- function(traces, name) Filter(function(t) identical(t$name, name), traces)
nums <- function(v) vapply(v, function(e) if (is.null(e)) NA_real_ else as.numeric(e), numeric(1))

all_outputs <- c(
  "header_date_range", "header_updated", "view_level_dots", "conditions_subtitle", "stress_banner",
  "condition_cards", "drought_status_card", "water_budget_ui", "local_management", "water_avail_subtitle",
  "hero_chart", "chart_html_legend", "chart_conversion_note", "chart_stats_bar", "seasonal_flow_subtitle",
  "seasonal_flow", "seasonal_flow_takeaway", "low_flow_subtitle", "low_flow_days", "low_flow_takeaway",
  "drought_history_subtitle", "drought_history", "drought_history_takeaway", "evidence_table",
  "data_sources", "gauge_map", "local_management_evidence", "water_101"
)

# ---- 1. Inputs re-fire outputs with the right per-site values -----------------

test_that("switching watershed re-fires cards and header with each site's seeded values", {
  fx <- use_store(build_fixture_db)
  on.exit(fx$cleanup(), add = TRUE)

  testServer(server, {
    session$setInputs(watershed = SITE_PARKER, time_period = "30d", chart_mode = "actual")
    parker <- html_of(output$condition_cards)
    # Parker latest reading: 12.5 cfs; flow_latest last success 2024-01-09 08:00:05 UTC.
    expect_match(card_html(parker, "flow"), "12.50", fixed = TRUE)
    expect_match(card_html(parker, "flow"), "USGS 01101000", fixed = TRUE)
    expect_equal(output$header_updated, "Updated Jan 9, 2024 3:00 AM")
    expect_match(output$conditions_subtitle, "Parker River watershed", fixed = TRUE)

    session$setInputs(watershed = SITE_IPSWICH)
    ipswich <- html_of(output$condition_cards)
    # Ipswich latest reading: 30.0 cfs; it has no etl_runs rows.
    expect_match(card_html(ipswich, "flow"), "30.00", fixed = TRUE)
    expect_match(card_html(ipswich, "flow"), "USGS 01102000", fixed = TRUE)
    expect_false(grepl("12.50", card_html(ipswich, "flow"), fixed = TRUE))
    expect_equal(output$header_updated, "Not yet updated")
    expect_match(output$conditions_subtitle, "Ipswich River watershed", fixed = TRUE)

    session$setInputs(watershed = SITE_PARKER)
    expect_match(card_html(html_of(output$condition_cards), "flow"), "12.50", fixed = TRUE)
  })
})

test_that("the flow card tooltip names the selected river, not always Parker", {
  fx <- use_store(build_fixture_db)
  on.exit(fx$cleanup(), add = TRUE)

  testServer(server, {
    session$setInputs(watershed = SITE_PARKER)
    parker_flow <- card_html(html_of(output$condition_cards), "flow")
    expect_match(parker_flow, "Parker River at the monitoring station", fixed = TRUE)
    expect_match(parker_flow, "Parker River at Byfield, Massachusetts, station 01101000", fixed = TRUE)

    session$setInputs(watershed = SITE_IPSWICH)
    ipswich_flow <- card_html(html_of(output$condition_cards), "flow")
    expect_match(ipswich_flow, "Ipswich River at the monitoring station", fixed = TRUE)
    expect_match(ipswich_flow, "station 01102000", fixed = TRUE)
    expect_false(grepl("Parker River at the monitoring station", ipswich_flow, fixed = TRUE))
  })
})

test_that("the local water management label does not falsely claim a watershed and has no dead link", {
  fx <- use_store(build_fixture_db)
  on.exit(fx$cleanup(), add = TRUE)

  testServer(server, {
    for (site in c(SITE_PARKER, SITE_IPSWICH)) {
      session$setInputs(watershed = site)
      management <- html_of(output$local_management)
      expect_match(management, "WATER WITHDRAWAL PERMIT", fixed = TRUE, info = site)
      expect_false(grepl("IPSWICH WITHDRAWAL PERMIT", management, fixed = TRUE), info = site)
    }

    # No dead "#" link anywhere in the rendered water budget or local
    # management markup (pass 2 review: "View methodology" pointed at "#").
    rendered <- paste(html_of(output$water_budget_ui), html_of(output$local_management))
    expect_false(grepl('href="#"', rendered, fixed = TRUE))
    expect_false(grepl("View methodology", rendered, fixed = TRUE))
  })
})

test_that("switching time period re-fires the header range and the hero chart window", {
  fx <- use_store(build_fixture_db)
  on.exit(fx$cleanup(), add = TRUE)
  today <- Sys.Date()

  testServer(server, {
    session$setInputs(watershed = SITE_PARKER, time_period = "30d", chart_mode = "actual")
    expect_match(html_of(output$header_date_range), format_date_range(today - 29, today), fixed = TRUE)
    expect_match(html_of(output$water_avail_subtitle), format_date_range(today - 29, today), fixed = TRUE)
    expect_equal(nrow(hero()$data), 30)
    hero_30 <- jsonlite::fromJSON(output$hero_chart, simplifyVector = FALSE)$x
    # The x-axis spans the whole window even though most days have no stored value.
    expect_equal(unlist(hero_30$layout$xaxis$range), format(c(today - 29, today)))
    flow_30 <- trace_named(hero_30$data, "River flow (cfs)")
    expect_length(flow_30, 1)
    # Daily means today-1..today-3 are 10, 11, 12 cfs.
    expect_true(all(c(10, 11, 12) %in% nums(flow_30[[1]]$y)))

    session$setInputs(time_period = "90d")
    expect_match(html_of(output$header_date_range), format_date_range(today - 89, today), fixed = TRUE)
    expect_equal(nrow(hero()$data), 90)
    hero_90 <- jsonlite::fromJSON(output$hero_chart, simplifyVector = FALSE)$x
    expect_equal(unlist(hero_90$layout$xaxis$range), format(c(today - 89, today)))
    expect_true(all(c(10, 11, 12) %in% nums(trace_named(hero_90$data, "River flow (cfs)")[[1]]$y)))

    session$setInputs(time_period = "today")
    # "today" uses 15-minute readings: only the now - 1 h reading (12.5 cfs) is in the last day.
    expect_match(html_of(output$water_avail_subtitle), "15-minute readings", fixed = TRUE)
    flow_today <- nums(trace_named(plot_traces(output$hero_chart), "River flow (cfs)")[[1]]$y)
    expect_equal(flow_today[is.finite(flow_today)], 12.5)

    session$setInputs(time_period = "7d")
    flow_week <- nums(trace_named(plot_traces(output$hero_chart), "River flow (cfs)")[[1]]$y)
    expect_setequal(flow_week[is.finite(flow_week)], c(15.0, 12.5))
  })
})

# ---- 2. Ecology responds to flow vs threshold ----------------------------------

test_that("ecology card reflects latest flow vs the config threshold", {
  fx <- use_store(build_fixture_db)
  on.exit(fx$cleanup(), add = TRUE)

  # config.yml threshold 8 cfs; Parker latest 12.5 cfs -> above.
  testServer(server, {
    session$setInputs(watershed = SITE_PARKER)
    eco <- card_html(html_of(output$condition_cards), "ecology")
    expect_match(eco, "Above eco-flow threshold", fixed = TRUE)
    expect_match(eco, "condition-badge badge-good", fixed = TRUE)
    expect_match(eco, "4.5 cfs above 8 cfs threshold", fixed = TRUE)
  })

  # Raise the threshold above the flow -> at or below.
  manual$eco_flow_threshold_cfs <<- 20
  testServer(server, {
    session$setInputs(watershed = SITE_PARKER)
    eco <- card_html(html_of(output$condition_cards), "ecology")
    expect_match(eco, "At or below eco-flow threshold", fixed = TRUE)
    expect_match(eco, "condition-badge badge-critical", fixed = TRUE)
    expect_match(eco, "7.5 cfs below 20 cfs threshold", fixed = TRUE)
  })

  # Back to 8 cfs, but a newer, lower reading arrives -> at or below.
  manual$eco_flow_threshold_cfs <<- 8
  DBI::dbExecute(
    fx$con,
    paste(
      "INSERT INTO flow_instantaneous (site_no, datetime, discharge_cfs, approval_status, qualifier)",
      "VALUES (?, ?, ?, ?, ?)"
    ),
    params = list(SITE_PARKER, format(Sys.time() - 60, "%Y-%m-%d %H:%M:%S", tz = "UTC"), 5.0, "Provisional", NA)
  )
  testServer(server, {
    session$setInputs(watershed = SITE_PARKER)
    eco <- card_html(html_of(output$condition_cards), "ecology")
    expect_match(eco, "At or below eco-flow threshold", fixed = TRUE)
    expect_match(eco, "3 cfs below 8 cfs threshold", fixed = TRUE)

    # Ipswich (30 cfs): the config.yml threshold is Parker-only, so the
    # comparison is unavailable rather than silently reused for Ipswich
    # (pass 2 review decision; see R/server.R's `ecology` reactive).
    session$setInputs(watershed = SITE_IPSWICH)
    ipswich_eco <- card_html(html_of(output$condition_cards), "ecology")
    expect_match(ipswich_eco, "Comparison unavailable — threshold set for Parker River only", fixed = TRUE)
    expect_match(ipswich_eco, "condition-badge badge-warning", fixed = TRUE)
    expect_false(grepl("Above eco-flow threshold", ipswich_eco, fixed = TRUE))

    # The hero chart's ecological reference line is Parker-only too.
    session$setInputs(time_period = "30d", chart_mode = "actual")
    expect_false(grepl("Ecological ref.", output$hero_chart, fixed = TRUE))
    expect_match(html_of(output$chart_conversion_note), "not shown for Ipswich", fixed = TRUE)

    # Switching back to Parker restores the live comparison and reference line.
    session$setInputs(watershed = SITE_PARKER)
    expect_match(card_html(html_of(output$condition_cards), "ecology"), "At or below eco-flow threshold", fixed = TRUE)
    expect_match(output$hero_chart, "Ecological ref.", fixed = TRUE)
  })
})

# ---- 3. Charts use stored percentiles, not the old heuristic ----------------------

test_that("hero and seasonal charts plot stored percentiles, not median * 0.52 / 1.55", {
  fx <- use_store(build_fixture_db)
  on.exit(fx$cleanup(), add = TRUE)
  today <- Sys.Date()
  heuristic <- c(9 * 0.52, 9 * 1.55)

  testServer(server, {
    session$setInputs(watershed = SITE_PARKER, time_period = "30d", chart_mode = "actual")
    traces <- plot_traces(output$hero_chart)
    ribbon <- trace_named(traces, HERO_RIBBON_NAME)
    expect_length(ribbon, 1)
    band <- nums(ribbon[[1]]$y)
    # Today's stored row: p10 4, p50 9, p90 22.
    expect_true(all(c(4, 22) %in% band))
    expect_false(any(abs(outer(band, heuristic, "-")) < 1e-9, na.rm = TRUE))
    expect_true(9 %in% nums(trace_named(traces, "Historical median flow")[[1]]$y))

    seasonal <- plot_traces(output$seasonal_flow)
    season_band <- nums(trace_named(seasonal, "Prior years p10–p90")[[1]]$y)
    expect_true(all(c(4, 22) %in% season_band))
    if (!(format(today, "%m-%d") == "06-15")) {
      # The (6, 15) row: p10 1, p90 20.
      expect_true(all(c(1, 20) %in% season_band))
    }
    expect_false(any(abs(outer(season_band, heuristic, "-")) < 1e-9, na.rm = TRUE))
    current <- trace_named(seasonal, paste(format(today, "%Y"), "daily mean flow"))
    expect_length(current, 1)
    expect_true(all(c(10, 11, 12) %in% nums(current[[1]]$y)))
  })
})

# ---- 4. Drought, evidence table, data sources, water 101 render -----------------

test_that("drought card and history render the seeded USDM maps", {
  fx <- use_store(build_fixture_db)
  on.exit(fx$cleanup(), add = TRUE)

  testServer(server, {
    session$setInputs(watershed = SITE_PARKER)
    card <- html_of(output$drought_status_card)
    expect_match(card, "D2 active — Severe Drought", fixed = TRUE)
    expect_match(card, "10% of county area in D2 (Severe Drought) or worse", fixed = TRUE)
    expect_match(card, "U.S. Drought Monitor · county-level · weekly", fixed = TRUE)
    expect_match(card, "Map date Jan 9, 2024", fixed = TRUE)
    expect_match(card, "Updated Jan 9, 2024 3:02 AM", fixed = TRUE)

    traces <- plot_traces(output$drought_history)
    expect_length(traces, 5)
    # Cumulative d0..d2 (50/20/0, 60/30/10) become non-overlapping bands.
    d2 <- trace_named(traces, "D2 Severe Drought")
    expect_equal(nums(d2[[1]]$y), c(0, 10))
    expect_equal(nums(d2[[1]]$customdata), c(0, 10))
    expect_equal(nums(trace_named(traces, "D1 Moderate Drought")[[1]]$y), c(20, 20))
    expect_equal(nums(trace_named(traces, "D0 Abnormally Dry")[[1]]$y), c(30, 30))
    takeaway <- html_of(output$drought_history_takeaway)
    expect_match(takeaway, "Latest map (Jan 9, 2024)", fixed = TRUE)
    expect_match(takeaway, "Across 2 weekly maps since Jan 2, 2024", fixed = TRUE)
    expect_match(takeaway, "or worse in 2 weeks", fixed = TRUE)
    expect_match(takeaway, "D2 (Severe Drought), last seen Jan 9, 2024", fixed = TRUE)
    expect_match(output$drought_history_subtitle, "U.S. Drought Monitor · county-level · weekly", fixed = TRUE)
  })
})

test_that("evidence table, data sources, map, low-flow chart and water 101 render", {
  fx <- use_store(build_fixture_db)
  on.exit(fx$cleanup(), add = TRUE)
  this_year <- as.integer(format(Sys.Date(), "%Y"))

  testServer(server, {
    session$setInputs(watershed = SITE_PARKER, time_period = "30d")

    evidence <- html_of(output$evidence_table)
    expect_match(evidence, "evidence-rows", fixed = TRUE)
    expect_match(evidence, "12.50 cfs", fixed = TRUE)
    expect_match(evidence, "4.00–22.00 cfs (p10–p90) for today from 25 prior years", fixed = TRUE)
    expect_match(evidence, "1.75 in", fixed = TRUE)
    expect_match(evidence, "10.33 ft below land surface", fixed = TRUE)
    expect_match(evidence, "D2 active", fixed = TRUE)

    sources <- html_of(output$data_sources)
    expect_match(sources, USGS_OGC_BASE_URL, fixed = TRUE)
    expect_match(sources, USDM_COUNTY_STATS_URL, fixed = TRUE)
    # Parker flow_latest: last success Jan 9 even though the latest run failed.
    expect_match(sources, "Last success: Jan 9, 2024 3:00 AM · Last run: failure (timeout)", fixed = TRUE)
    # Groundwater daily has no etl_runs rows in the fixture.
    expect_match(sources, "none recorded · Last run: never run", fixed = TRUE)

    water <- html_of(output$water_101)
    expect_match(water, "does not compute a live water-budget total", fixed = TRUE)
    expect_match(water, "Q<sub>in</sub>", fixed = TRUE)

    map <- jsonlite::fromJSON(output$gauge_map, simplifyVector = FALSE)
    methods <- vapply(map$x$calls, function(cl) cl$method, character(1))
    expect_true("addCircleMarkers" %in% methods)

    low <- plot_traces(output$low_flow_days)
    expect_length(low, 3)
    if (2022 >= this_year - LOW_FLOW_YEARS + 1) {
      below_1 <- low[[1]]
      expect_equal(nums(below_1$y)[nums(below_1$x) == 2022], 2)
      expect_equal(nums(low[[2]]$y)[nums(low[[2]]$x) == 2022], 1)
    }
  })
})

# ---- 5. Removed outputs are gone ---------------------------------------------------

test_that("seasonal_pumping and conservation_guidance are not defined anywhere", {
  fx <- use_store(build_fixture_db)
  on.exit(fx$cleanup(), add = TRUE)

  testServer(server, {
    session$setInputs(watershed = SITE_PARKER)
    expect_error(output$seasonal_pumping)
    expect_error(output$conservation_guidance)
  })
  for (f in c("app.R", file.path("R", "server.R"))) {
    code <- paste(readLines(file.path(.root, f), warn = FALSE), collapse = "\n")
    expect_false(grepl("seasonal_pumping", code, fixed = TRUE), info = f)
    expect_false(grepl("conservation_guidance", code, fixed = TRUE), info = f)
  }
})

# ---- 6. Stress banner shows no number ------------------------------------------------

test_that("stress banner renders the unavailable state with no score", {
  fx <- use_store(build_fixture_db)
  on.exit(fx$cleanup(), add = TRUE)

  testServer(server, {
    session$setInputs(watershed = SITE_PARKER)
    banner <- html_of(output$stress_banner)
    expect_match(banner, "Watershed stress score — not yet available", fixed = TRUE)
    expect_match(banner, "not derived from live data", fixed = TRUE)
    expect_false(grepl("stress-score-number", banner, fixed = TRUE))
    expect_false(grepl("stress-indicator", banner, fixed = TRUE))
    expect_false(grepl("/ 100", banner, fixed = TRUE))
    expect_false(grepl("8:15", banner, fixed = TRUE))
    # The only digits allowed are in the real "Updated ..." time.
    expect_match(banner, "Updated Jan 9, 2024 3:00 AM", fixed = TRUE)
    text <- gsub("<[^>]+>", " ", sub("Updated Jan 9, 2024 3:00 AM", "", banner, fixed = TRUE))
    expect_false(grepl("[0-9]", text))
  })
})

# ---- 7. Rain card with a short gauge record -------------------------------------------

test_that("rain card shows the real 7-day total and no typical comparison", {
  fx <- use_store(build_fixture_db)
  on.exit(fx$cleanup(), add = TRUE)
  today <- Sys.Date()

  testServer(server, {
    session$setInputs(watershed = SITE_PARKER)
    rain <- card_html(html_of(output$condition_cards), "rain")
    # precip_daily today-7..today-1 sums to 1.75 in; precip_typical has 1 year.
    expect_match(rain, "1.75", fixed = TRUE)
    expect_match(rain, "7-day total", fixed = TRUE)
    expect_match(rain, "Comparison unavailable — gauge record began June 2025", fixed = TRUE)
    expect_match(rain, format_date_range(today - 7, today - 1), fixed = TRUE)
    expect_match(rain, "single site", fixed = TRUE)
    expect_false(grepl("typical for these dates", rain, fixed = TRUE))
  })
})

# ---- 8. Empty store degrades without errors ----------------------------------------------

test_that("empty store: every output renders its unavailable state without error", {
  fx <- use_store(build_empty_db)
  on.exit(fx$cleanup(), add = TRUE)

  testServer(server, {
    for (mode in c("normalized", "actual")) {
      for (period in c("today", "30d", "1y")) {
        session$setInputs(watershed = SITE_PARKER, time_period = period, chart_mode = mode)
        for (id in all_outputs) {
          expect_no_error(value <- output[[id]])
          expect_false(is.null(value), info = id)
        }
      }
    }
    cards <- html_of(output$condition_cards)
    expect_match(card_html(cards, "flow"), "N/A", fixed = TRUE)
    expect_match(card_html(cards, "flow"), "Data unavailable", fixed = TRUE)
    expect_match(card_html(cards, "rain"), "No rainfall totals in the store", fixed = TRUE)
    expect_match(card_html(cards, "groundwater"), "Observation date unavailable", fixed = TRUE)
    expect_match(card_html(cards, "ecology"), "Comparison unavailable", fixed = TRUE)
    expect_match(card_html(cards, "pumping"), "Illustrative", fixed = TRUE)
    expect_equal(output$header_updated, "Not yet updated")
    expect_match(html_of(output$drought_status_card), "Unavailable", fixed = TRUE)
    expect_match(output$hero_chart, "No river flow in the store for this period", fixed = TRUE)
    expect_match(output$seasonal_flow, "No stored percentiles or daily flow", fixed = TRUE)
    expect_match(output$low_flow_days, "No daily river flow in the store", fixed = TRUE)
    expect_match(output$drought_history, "No U.S. Drought Monitor maps", fixed = TRUE)
    expect_match(html_of(output$chart_stats_bar), "No data", fixed = TRUE)
    expect_match(html_of(output$data_sources), "never run", fixed = TRUE)
  })
})

# ---- 9. Malicious config.yml renders escaped with no unsafe links ----------------------------

test_that("malicious config.yml renders escaped and produces no javascript:/data: links", {
  malicious <- file.path(.root, "tests", "testthat", "fixture-data", "config_yml_malicious.yml")
  withr::local_envvar(ESSEXWATER_CONFIG_PATH = malicious)
  fx <- use_store(build_fixture_db, config_path = resolve_config_path())
  on.exit(fx$cleanup(), add = TRUE)

  testServer(server, {
    session$setInputs(watershed = SITE_PARKER, chart_mode = "actual")
    rendered <- paste(
      html_of(output$local_management),
      html_of(output$local_management_evidence),
      html_of(output$condition_cards),
      html_of(output$evidence_table)
    )
    expect_false(grepl("<script", rendered, fixed = TRUE))
    expect_false(grepl("<img src=x", rendered, fixed = TRUE))
    expect_false(grepl("<b onmouseover", rendered, fixed = TRUE))
    expect_match(rendered, "&lt;script&gt;alert(1)&lt;/script&gt;", fixed = TRUE)
    expect_match(rendered, "&lt;img src=x onerror=alert(1)&gt;", fixed = TRUE)
    expect_false(grepl("javascript:", rendered, fixed = TRUE))
    expect_false(grepl('href="data:', rendered, fixed = TRUE))
    expect_false(grepl("local-management-link", rendered, fixed = TRUE))
    expect_match(rendered, "links are not configured yet", fixed = TRUE)

    # The script-laden threshold is not a number: no comparison, no reference line.
    eco <- card_html(html_of(output$condition_cards), "ecology")
    expect_match(eco, "Eco-flow threshold not configured", fixed = TRUE)
    expect_false(grepl("Ecological ref.", output$hero_chart, fixed = TRUE))
  })
})

# ---- 10. The real app.R UI builds, in Source A's structure --------------------------------------

test_that("app.R builds Source A's UI with unique ids and every server output mounted", {
  fx <- use_store(build_fixture_db)
  on.exit(fx$cleanup(), add = TRUE)
  withr::local_envvar(
    ESSEXWATER_DB_PATH = fx$con@dbname,
    ESSEXWATER_CONFIG_PATH = file.path(.root, "config.yml")
  )
  genv <- globalenv()
  app_con <- NULL
  withr::with_dir(.root, {
    source("app.R", local = genv)
    app_con <- get("con", envir = genv)
  })
  on.exit(if (DBI::dbIsValid(app_con)) DBI::dbDisconnect(app_con), add = TRUE)
  rendered <- htmltools::renderTags(get("ui", envir = genv))

  head <- paste(rendered$head, collapse = "\n")
  expect_lt(regexpr("styles.css", head, fixed = TRUE), regexpr("dashboard_new.css", head, fixed = TRUE))
  expect_gt(regexpr("styles.css", head, fixed = TRUE), 0)

  html <- rendered$html
  ids <- sub('^id="(.*)"$', "\\1", regmatches(html, gregexpr('id="[^"]+"', html))[[1]])
  expect_equal(ids[duplicated(ids)], character(0))
  for (id in c(all_outputs, "watershed", "time_period", "view_level", "chart_mode", "water_budget_period")) {
    expect_true(id %in% ids, info = id)
  }
  for (cls in c("dashboard-header", "icon-summary", "icon-details", "icon-evidence", "view-level-section",
                "dashboard-shell", "water-budget-section", "water-avail-card", "watershed-control")) {
    expect_match(html, cls, fixed = TRUE)
  }
  expect_match(html, "input.view_level &gt;= 2", fixed = TRUE)
  expect_match(html, "input.view_level &gt;= 3", fixed = TRUE)
  expect_false(grepl("8:15", html, fixed = TRUE))
  expect_false(grepl(paste0("no", "aa"), html, ignore.case = TRUE))
  expect_false(grepl("seasonal_pumping|conservation_guidance", html))
})
