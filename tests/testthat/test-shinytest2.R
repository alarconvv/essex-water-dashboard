# tests/testthat/test-shinytest2.R
#
# Gate 3 -- functional tests. Each test boots the REAL app (app.R + global.R +
# R/server.R, unmodified) in its own background R process with
# shinytest2::AppDriver and drives it with headless Chrome. Everything runs
# offline: the store is a temp SQLite file built by helper-db.R
# (build_fixture_db() / build_empty_db()) and handed to the app process via
# ESSEXWATER_DB_PATH, set in THIS process before AppDriver$new() (AppDriver
# has no env= argument; the child inherits the parent environment). Map tiles
# are requested by the browser only; nothing in the assertions depends on them.
#
# Fixture values asserted here are documented at the top of helper-db.R.
#
# --- Two environment settings required on this machine -----------------------
# 1. NOT_CRAN = "true": shinytest2 skips every AppDriver unless NOT_CRAN (or
#    SHINYTEST2_APP_DRIVER_TEST_ON_CRAN) is "true". A plain
#    `Rscript -e 'testthat::test_dir(...)'` sets neither.
# 2. TESTTHAT_IS_CHECKING = "true": DESCRIPTION sits at the app root, so
#    shinytest2 treats the app as a package under development and starts the
#    child with options(warn = 2). That turns the harmless "package was built
#    under R version 4.6.1" warning into a startup error inside library().
#    shinytest2 skips that dev-package branch when testthat::is_checking(),
#    which is a plain check of this env var. Proven in the sibling app/
#    project's test-shinytest2.R.
Sys.setenv(NOT_CRAN = "true", TESTTHAT_IS_CHECKING = "true")

app_root <- normalizePath(testthat::test_path("..", ".."))

CARDS_JS <- "Array.from(document.querySelectorAll('#condition_cards .conditions-grid > .condition-card'))"

# ---- Helpers -------------------------------------------------------------------

#' Build a helper-db.R store, close it, and return its file path (only a path
#' can cross into the app process). The file is deleted when `envir` exits.
store_path <- function(build_fn, envir = parent.frame()) {
  con <- build_fn()
  path <- DBI::dbGetInfo(con)$dbname
  DBI::dbDisconnect(con)
  withr::defer(unlink(path), envir = envir)
  path
}

#' Boot the app against `db_path` (and optionally a config.yml override).
#' The env vars are set in the caller's frame and the app is stopped when the
#' caller exits.
start_app <- function(db_path, name, config_path = NA_character_, envir = parent.frame()) {
  withr::local_envvar(c(ESSEXWATER_DB_PATH = db_path, ESSEXWATER_CONFIG_PATH = config_path), .local_envir = envir)
  app <- shinytest2::AppDriver$new(
    app_dir = app_root, name = name,
    height = 1000, width = 1440, load_timeout = 60000, timeout = 20000
  )
  withr::defer(app$stop(), envir = envir)
  app$wait_for_idle()
  # wait_for_idle() reports Shiny's busy/idle websocket state, which can flip
  # to idle a moment before the DOM text it produced is actually queryable
  # (observed as an intermittent empty ".dashboard-updated" on a fresh boot).
  # header_updated is present in every server state (fixture or empty store)
  # once the very first render has landed, so waiting on its non-empty text
  # is a reliable proxy that the initial DOM is fully settled.
  app$wait_for_js(
    "(() => { const el = document.querySelector('.dashboard-updated'); return !!(el && el.textContent.trim()); })()",
    timeout = 20000
  )
  app
}

#' Zero uncaught R errors in the app process. shinytest2 0.5.1 logs only
#' "info"/"stderr" levels, so errors are found by the word "Error" on the app's
#' own stderr stream (R prints "Error in ...:" / "Warning: Error in ..." for
#' render errors; package-attach notes and plain warnings never contain it).
expect_no_app_errors <- function(app) {
  logs <- app$get_logs()
  shiny_lines <- logs[logs$location == "shiny", , drop = FALSE]
  errors <- shiny_lines$message[grepl("Error", shiny_lines$message, fixed = TRUE)]
  expect_equal(length(errors), 0, label = paste("app stderr error lines:", paste(errors, collapse = "\n")))
}

squish <- function(x) gsub("\\s+", " ", trimws(paste(unlist(x), collapse = " ")))

js_string <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE)

#' Trimmed textContent of the first element matching a CSS selector ("" if none).
text_of <- function(app, selector) {
  squish(app$get_js(paste0(
    "(() => { const el = document.querySelector(", js_string(selector), "); ",
    "return el ? el.textContent : ''; })()"
  )))
}

count_of <- function(app, selector) {
  as.numeric(app$get_js(paste0("document.querySelectorAll(", js_string(selector), ").length")))
}

#' Computed display of the conditionalPanel wrapping a depth section.
section_display <- function(app, section_id) {
  app$get_js(paste0("getComputedStyle(document.getElementById('", section_id, "').parentElement).display"))
}

#' plotly marks the output element itself with .js-plotly-plot and attaches
#' the drawn traces/layout to it once it has rendered.
plot_js <- function(output_id, body) {
  paste0("(() => { const el = document.getElementById('", output_id, "'); ", body, " })()")
}

#' Wait until a plotly output has drawn at least one trace in the browser.
wait_for_plot <- function(app, output_id) {
  app$wait_for_js(plot_js(output_id, "return !!(el && el.data && el.data.length > 0);"), timeout = 20000)
}

plot_is_drawn <- function(app, output_id) {
  isTRUE(app$get_js(plot_js(output_id, "return !!(el && el.data && el.data.length > 0);")))
}

plot_trace_names <- function(app, output_id) {
  unlist(app$get_js(plot_js(output_id, "return el && el.data ? el.data.map(t => t.name || '') : [];")))
}

plot_annotations <- function(app, output_id) {
  unlist(app$get_js(plot_js(
    output_id,
    "return el && el.layout && el.layout.annotations ? el.layout.annotations.map(a => a.text) : [];"
  )))
}

wait_for_markers <- function(app, n) {
  app$wait_for_js(
    paste0("document.querySelectorAll('#gauge_map path.leaflet-interactive').length === ", n),
    timeout = 20000
  )
}

card_part <- function(app, card_id, part) text_of(app, paste0("#card-", card_id, " .", part))

set_level <- function(app, level) {
  app$set_inputs(view_level = as.character(level))
  app$wait_for_idle()
}

local_hours_date <- function(hours_ago) {
  as.Date(format(Sys.time() - hours_ago * 3600, "%Y-%m-%d", tz = "America/New_York"))
}

short_date <- function(date) paste(month.abb[as.integer(format(date, "%m"))], as.integer(format(date, "%d")))

# ---- 1. Boot on the fixture store ---------------------------------------------------------

test_that("1. boot: fixture store loads with zero errors, real updated time, no stress score", {
  app <- start_app(store_path(build_fixture_db), "boot")

  expect_no_app_errors(app)
  # flow_latest:USGS-01101000 last success 2024-01-09 08:00:05 UTC -> 3:00 AM Eastern.
  expect_equal(text_of(app, ".dashboard-updated"), "Updated Jan 9, 2024 3:00 AM")
  expect_equal(text_of(app, ".current-conditions-updated"), "Updated Jan 9, 2024 3:00 AM")
  expect_false(grepl("8:15", app$get_html("html", outer_html = TRUE), fixed = TRUE))

  today <- Sys.Date()
  start <- today - 29
  expected_range <- paste0(
    short_date(start), if (format(start, "%Y") == format(today, "%Y")) "" else paste0(", ", format(start, "%Y")),
    " – ", short_date(today), ", ", format(today, "%Y")
  )
  expect_equal(text_of(app, ".dashboard-date-range"), expected_range)

  expect_match(text_of(app, ".stress-status-badge"), "not yet available", fixed = TRUE)
  expect_equal(count_of(app, "#stress_banner .stress-score-number"), 0)
  expect_equal(count_of(app, "#stress_banner .stress-score-display"), 0)
  expect_equal(count_of(app, "#stress_banner .stress-indicator"), 0)
  score_area <- text_of(app, "#stress_banner .current-conditions-inner")
  expect_true(nzchar(score_area))
  expect_false(grepl("[0-9]", score_area), label = score_area)
})

# ---- 2. Removed precipitation vendor is absent --------------------------------------------

test_that("2. no reference to the removed precipitation vendor in the rendered page", {
  app <- start_app(store_path(build_fixture_db), "no-vendor")
  vendor <- paste0("no", "aa")

  expect_false(grepl(vendor, app$get_html("html", outer_html = TRUE), ignore.case = TRUE))
  # Deeper levels render more outputs; check the fully expanded page too.
  set_level(app, 3)
  wait_for_plot(app, "seasonal_flow")
  wait_for_markers(app, 4)
  full_html <- app$get_html("html", outer_html = TRUE)
  expect_false(grepl(vendor, full_html, ignore.case = TRUE))
  expect_match(full_html, "USGS Byfield gauge", fixed = TRUE)
  expect_no_app_errors(app)
})

# ---- 3. Icons ----------------------------------------------------------------------------------

test_that("3. view-level icons are served and CSS icon references match files case-sensitively", {
  app <- start_app(store_path(build_fixture_db), "icons")
  base <- sub("/$", "", app$get_url())

  for (icon in c("Summary", "Details", "Evidence")) {
    resp <- httr2::request(paste0(base, "/icons/", icon, ".svg")) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()
    expect_equal(httr2::resp_status(resp), 200L, info = icon)
    expect_match(httr2::resp_body_string(resp), "<svg", fixed = TRUE, info = icon)
  }

  # list.files() is case-exact even on case-insensitive macOS file systems,
  # which is what Linux/shinyapps.io will enforce.
  icon_files <- list.files(file.path(app_root, "www", "icons"))
  css <- paste(readLines(file.path(app_root, "www", "dashboard_new.css"), warn = FALSE), collapse = "\n")
  urls <- regmatches(css, gregexpr('url\\("icons/[^"]+"\\)', css))[[1]]
  referenced <- unique(sub('^url\\("icons/(.*)"\\)$', "\\1", urls))
  expect_setequal(referenced, c("Summary.svg", "Details.svg", "Evidence.svg"))
  for (ref in referenced) {
    expect_true(ref %in% icon_files, info = ref)
  }

  # The icons really paint: the mask image resolves to the served file.
  mask <- app$get_js("getComputedStyle(document.querySelector('.icon-summary')).webkitMaskImage")
  expect_match(mask, "icons/Summary.svg", fixed = TRUE)
})

# ---- 4. View levels --------------------------------------------------------------------------

test_that("4. view level toggles depth sections, dots follow the level, hidden outputs render when shown", {
  app <- start_app(store_path(build_fixture_db), "view-levels")
  active_pill <- ".view-level-left .btn-check:checked + .btn"

  # Summary. Depth sections are hidden (display:none via conditionalPanel),
  # but Shiny always computes every output's FIRST value on initial load
  # regardless of visibility -- only re-renders after invalidation are
  # suspended while hidden -- so plotly widgets already carry .data in the
  # DOM here (confirmed empirically). Leaflet is the one output that visibly
  # depends on layout: with zero pixel size under display:none it cannot
  # place markers, so it stays a meaningful "not yet drawn" signal.
  expect_equal(count_of(app, ".view-dot.active"), 1)
  expect_equal(count_of(app, ".view-dot"), 3)
  expect_match(text_of(app, active_pill), "Summary", fixed = TRUE)
  expect_equal(section_display(app, "details-section"), "none")
  expect_equal(section_display(app, "evidence-section"), "none")
  expect_equal(count_of(app, "#gauge_map path.leaflet-interactive"), 0)

  # Details.
  set_level(app, 2)
  expect_equal(count_of(app, ".view-dot.active"), 2)
  expect_match(text_of(app, active_pill), "Details", fixed = TRUE)
  expect_false(section_display(app, "details-section") == "none")
  expect_equal(section_display(app, "evidence-section"), "none")
  for (id in c("seasonal_flow", "low_flow_days", "drought_history")) {
    wait_for_plot(app, id)
    expect_true(plot_is_drawn(app, id), info = id)
  }
  expect_match(text_of(app, "#evidence_table"), "12.50 cfs", fixed = TRUE)

  # Evidence.
  set_level(app, 3)
  expect_equal(count_of(app, ".view-dot.active"), 3)
  expect_match(text_of(app, active_pill), "Evidence", fixed = TRUE)
  expect_false(section_display(app, "evidence-section") == "none")
  expect_false(section_display(app, "details-section") == "none")
  wait_for_markers(app, 4)
  expect_equal(count_of(app, "#gauge_map path.leaflet-interactive"), 4)
  expect_match(text_of(app, "#data_sources"), "DATA SOURCES", fixed = TRUE)

  # Back to Summary: depth sections hide again.
  set_level(app, 1)
  expect_equal(count_of(app, ".view-dot.active"), 1)
  expect_equal(section_display(app, "details-section"), "none")
  expect_equal(section_display(app, "evidence-section"), "none")
  expect_no_app_errors(app)
})

# ---- 5. Summary with fixture values -------------------------------------------------------------

test_that("5. Summary: five condition cards, ecology state, rain comparison, drought card", {
  app <- start_app(store_path(build_fixture_db), "summary-values")

  ids <- unlist(app$get_js(paste0(CARDS_JS, ".map(c => c.id)")))
  expect_equal(ids, c("card-flow", "card-rain", "card-groundwater", "card-pumping", "card-ecology"))
  # All five share one row under the 5-column category header (no wrapped card).
  tops <- unlist(app$get_js(paste0(CARDS_JS, ".map(c => c.offsetTop)")))
  expect_length(unique(tops), 1)

  # River flow: latest Parker reading 12.5 cfs.
  expect_equal(card_part(app, "flow", "value-number"), "12.50")
  expect_equal(card_part(app, "flow", "card-source"), "USGS 01101000 · Live · Updated Jan 9, 2024 3:00 AM")

  # Rainfall: today-7..today-1 sums to 1.75 in; only 1 prior year -> no comparison.
  expect_equal(card_part(app, "rain", "value-number"), "1.75")
  expect_equal(card_part(app, "rain", "condition-badge"), "7-day total")
  expect_equal(card_part(app, "rain", "card-comparison"), "Comparison unavailable — gauge record began June 2025")
  expect_match(card_part(app, "rain", "card-source"), "USGS Byfield gauge (single site)", fixed = TRUE)

  # Groundwater: latest 10.33 ft; stored median for today is 8.0 ft.
  expect_equal(card_part(app, "groundwater", "value-number"), "10.33")
  if (identical(local_hours_date(1), Sys.Date())) {
    expect_equal(card_part(app, "groundwater", "card-comparison"), "2.33 ft deeper than seasonal median")
    expect_equal(card_part(app, "groundwater", "condition-badge"), "Lower than seasonal median")
  }

  # Municipal use stays Illustrative.
  expect_equal(card_part(app, "pumping", "value-number"), "2.8")
  expect_equal(card_part(app, "pumping", "condition-badge"), "Illustrative")
  expect_equal(card_part(app, "pumping", "card-source"), "Municipal data · Illustrative")

  # Ecology: 12.5 cfs vs the 8 cfs config.yml threshold -> above.
  expect_equal(card_part(app, "ecology", "value-number"), "12.50")
  expect_equal(card_part(app, "ecology", "condition-badge"), "Above eco-flow threshold")
  expect_equal(count_of(app, "#card-ecology .condition-badge.badge-good"), 1)
  expect_equal(card_part(app, "ecology", "card-comparison"), "4.5 cfs above 8 cfs threshold (provisional)")

  # Drought card: latest USDM map 2024-01-09 has D2 = 10 %.
  drought <- text_of(app, "#drought_status_card")
  expect_match(drought, "DROUGHT STATUS · ESSEX COUNTY", fixed = TRUE)
  expect_equal(text_of(app, "#drought_status_card .local-management-status"), "D2 active — Severe Drought")
  expect_match(
    drought,
    "10% of county area in D2 (Severe Drought) or worse · 60% at least abnormally dry (D0)",
    fixed = TRUE
  )
  expect_match(
    drought,
    "U.S. Drought Monitor · county-level · weekly · Map date Jan 9, 2024 · Updated Jan 9, 2024 3:02 AM",
    fixed = TRUE
  )
  expect_no_app_errors(app)
})

# ---- 6. Details and Evidence render ----------------------------------------------------------------

test_that("6. Details and Evidence outputs render real content; removed outputs are absent", {
  app <- start_app(store_path(build_fixture_db), "details-evidence")
  set_level(app, 3)

  wait_for_plot(app, "seasonal_flow")
  seasonal <- plot_trace_names(app, "seasonal_flow")
  current_year <- paste(format(Sys.Date(), "%Y"), "daily mean flow")
  expect_true(all(c("Prior years p10–p90", "Historical median", current_year) %in% seasonal))

  wait_for_plot(app, "drought_history")
  expect_equal(
    plot_trace_names(app, "drought_history"),
    c("D4 Exceptional Drought", "D3 Extreme Drought", "D2 Severe Drought", "D1 Moderate Drought", "D0 Abnormally Dry")
  )
  expect_match(text_of(app, "#drought_history_takeaway"), "Across 2 weekly maps since Jan 2, 2024", fixed = TRUE)

  wait_for_plot(app, "low_flow_days")
  expect_length(plot_trace_names(app, "low_flow_days"), 3)
  expect_equal(
    plot_annotations(app, "low_flow_days"),
    c("<b>Below 1 cfs</b>", "<b>Below 0.1 cfs</b>", "<b>Below 0.01 cfs</b>")
  )

  evidence <- text_of(app, "#evidence_table")
  expect_match(evidence, "12.50 cfs (latest 15-minute reading)", fixed = TRUE)
  expect_match(evidence, "1.75 in", fixed = TRUE)
  expect_match(evidence, "10.33 ft below land surface", fixed = TRUE)
  expect_match(evidence, "D2 active — Severe Drought", fixed = TRUE)
  expect_equal(count_of(app, "#evidence_table .evidence-rows tr"), 6)

  sources <- text_of(app, "#data_sources")
  expect_match(sources, "https://api.waterdata.usgs.gov/ogcapi/v0/collections/continuous/items", fixed = TRUE)
  expect_match(sources, "Last success: Jan 9, 2024 3:00 AM · Last run: failure (timeout)", fixed = TRUE)
  expect_match(sources, "none recorded · Last run: never run", fixed = TRUE)

  wait_for_markers(app, 4)
  expect_equal(count_of(app, "#gauge_map path.leaflet-interactive"), 4)
  expect_equal(count_of(app, "#gauge_map .legend"), 1)

  expect_match(text_of(app, "#water_101"), "does not compute a live water-budget total", fixed = TRUE)
  management <- text_of(app, "#local_management_evidence")
  expect_match(management, "Status to be confirmed", fixed = TRUE)
  expect_match(management, "8.0 cfs (provisional)", fixed = TRUE)
  expect_match(management, "Illustrative", fixed = TRUE)

  page <- app$get_html("html", outer_html = TRUE)
  for (removed in c("seasonal_pumping", "conservation_guidance")) {
    expect_equal(count_of(app, paste0("#", removed)), 0, info = removed)
    expect_false(grepl(removed, page, fixed = TRUE), info = removed)
  }
  expect_no_app_errors(app)
})

# ---- 7. Watershed switch ------------------------------------------------------------------------

test_that("7. switching watershed updates the flow card to the other gauge's seeded value", {
  app <- start_app(store_path(build_fixture_db), "watershed")

  options <- squish(app$get_js("JSON.stringify(Object.values($('#watershed')[0].selectize.options))"))
  expect_match(options, '"label":"Parker River"', fixed = TRUE)
  expect_match(options, '"label":"Ipswich River"', fixed = TRUE)
  expect_equal(card_part(app, "flow", "value-number"), "12.50")
  # Ecology card is live for Parker: 12.5 cfs vs the 8 cfs threshold.
  expect_equal(card_part(app, "ecology", "condition-badge"), "Above eco-flow threshold")

  app$set_inputs(watershed = SITE_IPSWICH)
  app$wait_for_idle()
  expect_equal(card_part(app, "flow", "value-number"), "30.00")
  expect_equal(card_part(app, "flow", "card-source"), "USGS 01102000 · Live · Not yet updated")
  expect_equal(text_of(app, ".dashboard-updated"), "Not yet updated")
  expect_match(text_of(app, ".section-subtitle"), "Ipswich River watershed", fixed = TRUE)

  # The config.yml ecological threshold is Parker-only: Ipswich shows
  # "Comparison unavailable" rather than reusing Parker's threshold, and the
  # local water management label does not follow the selector (pass 2 review).
  expect_equal(card_part(app, "ecology", "condition-badge"), "Comparison unavailable")
  expect_match(
    card_part(app, "ecology", "card-comparison"),
    "Comparison unavailable — threshold set for Parker River only",
    fixed = TRUE
  )
  expect_match(text_of(app, "#local_management .local-management-label"), "WATER WITHDRAWAL PERMIT", fixed = TRUE)

  app$set_inputs(watershed = SITE_PARKER)
  app$wait_for_idle()
  expect_equal(card_part(app, "flow", "value-number"), "12.50")
  expect_equal(card_part(app, "ecology", "condition-badge"), "Above eco-flow threshold")
  expect_no_app_errors(app)
})

test_that("7b. no dead '#' links ship in the rendered page", {
  app <- start_app(store_path(build_fixture_db), "no-dead-links")
  set_level(app, 3)
  wait_for_markers(app, 4)
  app$wait_for_js("document.querySelector('#local_management_evidence .evidence-rows') !== null", timeout = 20000)

  # Leaflet's own zoom-in/zoom-out controls (inside #gauge_map) legitimately
  # use href="#" -- they are library chrome, not app-authored links. Every
  # other anchor must have a real target.
  app_hrefs <- unlist(app$get_js(paste0(
    "Array.from(document.querySelectorAll('a[href]'))",
    ".filter(a => !a.closest('#gauge_map'))",
    ".map(a => a.getAttribute('href'))"
  )))
  expect_false(any(app_hrefs == "#"), label = paste(app_hrefs, collapse = " "))
  expect_false(grepl("View methodology", app$get_html("html", outer_html = TRUE), fixed = TRUE))
  expect_no_app_errors(app)
})

# ---- 8. CSS cascade --------------------------------------------------------------------------------

test_that("8. computed styles follow dashboard_new.css tokens, not styles.css", {
  app <- start_app(store_path(build_fixture_db), "css-cascade")

  # Resolve each token in the browser on a probe element, so the comparison
  # uses the same serialization as getComputedStyle().
  resolved <- function(property, value) {
    app$get_js(paste0(
      "(() => { const p = document.createElement('div'); p.style.", property, " = '", value, "'; ",
      "document.body.appendChild(p); const v = getComputedStyle(p)['", property, "']; p.remove(); return v; })()"
    ))
  }
  card <- function(property) {
    app$get_js(paste0("getComputedStyle(document.querySelector('#card-flow'))['", property, "']"))
  }

  # dashboard_new.css: .condition-card { padding: 18px; border-radius: var(--radius-card);
  #   box-shadow: var(--shadow-card) }. styles.css declares padding 20px and no shadow.
  expect_equal(card("paddingTop"), "18px")
  expect_equal(card("paddingLeft"), "18px")
  expect_false(card("paddingTop") == "20px")
  expect_equal(card("borderTopLeftRadius"), resolved("borderTopLeftRadius", "var(--radius-card)"))
  expect_equal(card("borderTopLeftRadius"), "12px")
  expect_equal(card("boxShadow"), resolved("boxShadow", "var(--shadow-card)"))
  expect_false(card("boxShadow") == "none")

  # Page background: --color-page (#F0F0F1), not styles.css's #EAF1F6.
  rgb <- function(hex) {
    v <- grDevices::col2rgb(hex)
    sprintf("rgb(%d, %d, %d)", v[1], v[2], v[3])
  }
  body_bg <- app$get_js("getComputedStyle(document.body).backgroundColor")
  expect_equal(body_bg, rgb("#F0F0F1"))
  expect_equal(body_bg, resolved("backgroundColor", "var(--color-page)"))
  expect_false(body_bg == rgb("#EAF1F6"))

  # Header background: --color-ink (#172533).
  header_bg <- app$get_js("getComputedStyle(document.querySelector('.dashboard-header')).backgroundColor")
  expect_equal(header_bg, rgb("#172533"))
  expect_equal(header_bg, resolved("backgroundColor", "var(--color-ink)"))

  # The tokens themselves are the documented values.
  tokens <- unlist(app$get_js(paste0(
    "(() => { const s = getComputedStyle(document.documentElement); ",
    "return ['--color-page', '--color-ink', '--radius-card'].map(t => s.getPropertyValue(t).trim()); })()"
  )))
  expect_equal(tokens, c("#F0F0F1", "#172533", "12px"))
})

# ---- 9. Empty store --------------------------------------------------------------------------------

test_that("9. empty store: every section shows its fallback with zero errors and no fabricated values", {
  app <- start_app(store_path(build_empty_db), "empty-store")

  expect_no_app_errors(app)
  expect_equal(text_of(app, ".dashboard-updated"), "Not yet updated")
  expect_equal(count_of(app, "#stress_banner .stress-score-number"), 0)

  for (id in c("flow", "rain", "groundwater", "pumping", "ecology")) {
    value <- card_part(app, id, "value-number")
    badge <- card_part(app, id, "condition-badge")
    if (identical(badge, "Illustrative")) {
      # The only digits allowed are the explicitly Illustrative municipal value.
      expect_equal(id, "pumping")
      expect_equal(card_part(app, id, "card-source"), "Municipal data · Illustrative")
    } else {
      expect_false(grepl("[0-9]", value), info = paste(id, value))
      expect_equal(value, "N/A", info = id)
    }
  }
  expect_equal(card_part(app, "flow", "condition-badge"), "Data unavailable")
  expect_equal(card_part(app, "rain", "card-comparison"), "No rainfall totals in the store")
  expect_equal(card_part(app, "groundwater", "card-comparison"), "Historical comparison unavailable")
  expect_equal(card_part(app, "ecology", "card-comparison"), "Live flow unavailable")
  expect_equal(text_of(app, "#drought_status_card .local-management-status"), "Unavailable")
  expect_match(text_of(app, "#chart_stats_bar"), "No data", fixed = TRUE)
  wait_for_plot(app, "hero_chart")
  expect_equal(plot_annotations(app, "hero_chart"), "No river flow in the store for this period")

  set_level(app, 3)
  for (id in c("seasonal_flow", "low_flow_days", "drought_history")) {
    wait_for_plot(app, id)
  }
  expect_equal(plot_annotations(app, "seasonal_flow"), "No stored percentiles or daily flow for this gauge yet")
  expect_equal(plot_annotations(app, "low_flow_days"), "No daily river flow in the store for the last 10 years")
  expect_equal(plot_annotations(app, "drought_history"), "No U.S. Drought Monitor maps in the store yet")
  expect_match(text_of(app, "#seasonal_flow_takeaway"), "No daily flow stored for this year yet.", fixed = TRUE)
  expect_match(text_of(app, "#drought_history_takeaway"), "No drought maps stored yet.", fixed = TRUE)

  evidence <- text_of(app, "#evidence_table")
  expect_match(evidence, "River flow · USGS 01101000 No data", fixed = TRUE)
  expect_match(evidence, "Drought status · FIPS 25009 Unavailable", fixed = TRUE)
  sources <- text_of(app, "#data_sources")
  expect_match(sources, "none recorded · Last run: never run", fixed = TRUE)
  expect_false(grepl("Last run: success", sources, fixed = TRUE))
  app$wait_for_js("document.querySelector('#gauge_map.leaflet-container, #gauge_map .leaflet-container') !== null",
                  timeout = 20000)
  expect_equal(count_of(app, "#gauge_map path.leaflet-interactive"), 0)
  expect_match(text_of(app, "#water_101"), "Water 101", fixed = TRUE)
  expect_no_app_errors(app)
})

# ---- 10. Malicious config.yml ---------------------------------------------------------------------

test_that("10. malicious config.yml renders only as escaped text with no unsafe links or handlers", {
  malicious <- normalizePath(testthat::test_path("fixture-data", "config_yml_malicious.yml"), mustWork = TRUE)
  app <- start_app(store_path(build_fixture_db), "malicious-config", config_path = malicious)
  set_level(app, 3)
  app$wait_for_js("document.querySelector('#local_management_evidence .evidence-rows') !== null", timeout = 20000)

  html <- app$get_html("html", outer_html = TRUE)
  expect_false(grepl("<script>alert(1)</script>", html, fixed = TRUE))
  expect_false(grepl("<img src=x onerror", html, fixed = TRUE))
  expect_false(grepl("<b onmouseover", html, fixed = TRUE))
  expect_match(html, "&lt;script&gt;alert(1)&lt;/script&gt;", fixed = TRUE)
  expect_match(html, "&lt;img src=x onerror=alert(1)&gt;", fixed = TRUE)

  # The payloads are visible to the reader as inert text.
  expect_match(text_of(app, "#local_management .local-management-status"), "<script>alert(1)</script>", fixed = TRUE)
  expect_match(text_of(app, "#local_management_evidence"), "<img src=x onerror=alert(1)>", fixed = TRUE)

  expect_equal(count_of(app, "[onerror]"), 0)
  expect_equal(count_of(app, "[onmouseover]"), 0)
  expect_false(isTRUE(app$get_js(paste0(
    "Array.from(document.querySelectorAll('script')).some(s => ",
    "s.textContent.includes('alert(1)') || s.textContent.includes('alert(document.cookie)'))"
  ))))
  hrefs <- unlist(app$get_js("Array.from(document.querySelectorAll('[href]')).map(a => a.getAttribute('href'))"))
  expect_false(any(grepl("^\\s*(javascript|data):", hrefs, ignore.case = TRUE)), label = paste(hrefs, collapse = " "))
  expect_equal(count_of(app, ".local-management-link"), 0)
  expect_match(text_of(app, "#local_management"), "links are not configured yet", fixed = TRUE)

  # The script-laden threshold is not a number: ecology comparison is unavailable.
  expect_equal(card_part(app, "ecology", "card-comparison"), "Eco-flow threshold not configured")
  expect_no_app_errors(app)
})
