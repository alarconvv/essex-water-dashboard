# Development Guide: Essex County Water Dashboard

This guide is for anyone who maintains or extends the merged dashboard.
It covers what the dashboard is for, the rule that governs every number
on screen, how data moves from public APIs to the page, which prototype
features are built, and how to run, test, and deploy the project.

The merged project combines two earlier builds:

- **Source A**, the Figma-derived Shiny prototype. Its design and
  features are kept: layout, components, CSS, and icons.
- **Source B**, the architecture prototype. Its ETL → SQLite → Shiny
  data layer, helpers, and test gates are kept.

Visual rules live in [`AESTHETIC_GUIDE.md`](AESTHETIC_GUIDE.md). This
guide covers code, data, and operations.

---

## 1. Project objective

The dashboard gives residents, municipal, state, and federal staff, and
watershed advocates a **shared, credible view** of water availability
and use in the Parker and Ipswich River watersheds. With the same
evidence in front of everyone, drought-driven conflicts can be
discussed early instead of settled only after a crisis.

**Target users:**

- Residents, who need plain-language, actionable context.
- Municipal, state, and federal staff, who need sourced, defensible
  numbers.
- Watershed advocates at the Ipswich River Watershed Association (IRWA)
  and the Parker River Clean Water Association (PRCWA), who need
  evidence they can use at hearings.

---

## 2. Guiding principle: never fabricate data

This is the most important rule in the project. **Every number shown is
either (a) read from the local store, which the ETL filled from a free
public API, or (b) explicitly labeled "Illustrative" or manually
maintained.**

What this means in practice:

- When a fetch fails, the ETL writes nothing. It never writes a
  fallback value. The failure is recorded in `etl_runs`.
- When data is missing, the app shows "No data", "N/A", "Unavailable",
  "Data unavailable", or "Comparison unavailable" in the same
  component, or an empty chart that says why. It never shows a guessed
  number.
- Comparisons are shown only when enough history backs them. For
  example, the rainfall "typical" comparison needs at least 3 prior
  years (Section 12).
- Source A's hard-coded values are gone:
  - the stress score of 62
  - the "Updated 8:15 PM" text
  - the p10/p90 multipliers
  - the synthetic hero-chart series

  Each one was either replaced by stored data or removed.
- In `R/server.R`, only two sets of numbers do not come from the store:
  the water budget and the municipal-use card. Both say "Illustrative"
  in text and keep Source A's existing badge colors
  (AESTHETIC_GUIDE §6.4).

---

## 3. Prototype elements catalog

This is the Figma prototype's feature list (owl-cut-23629555.figma.site)
and its status in the merged build, grouped by view level. Levels add
depth to one scrolling page: Summary is always shown, Details appears at
`view_level >= 2`, and Evidence at `view_level >= 3`
(AESTHETIC_GUIDE §5). Output ids are shown in `code`.

Legend:

- `[x]` Built on stored live data.
- `[x] Illustrative` Built, but explicitly not live.
- `[-]` Removed on purpose.
- `[ ]` Not built yet.

### Summary level

- [x] **Header** (`dashboard_header(updated, watershed)`):
  - The **WATERSHED** control is a real `selectInput("watershed")` with
    the two flow gauges from `etl/constants.R`: Parker River
    (`USGS-01101000`, the default) and Ipswich River (`USGS-01102000`).
    It switches the river flow and ecology cards, the hero chart, the
    "Is this normal?" chart, the low-flow chart, the flow rows of the
    evidence table, and the subtitles.
  - The groundwater, rainfall, and drought figures come from single
    sites, so they do not change with the selection.
  - TIME PERIOD is `selectInput("time_period")`.
  - The date range (`header_date_range`) always ends today.
  - The "Updated" line (`header_updated`) is the last successful
    `flow_latest:<site>` ETL run for the selected gauge, formatted by
    `format_last_updated()`. No time is hard-coded.
- [x] **Stress banner, shown as unavailable** (`stress_banner`):
  - The badge reads "Watershed stress score — not yet available". The
    description reads "Methodology pending; not derived from live data.
    Preliminary — no score is shown until a validated method exists."
  - The gradient track and axis labels remain, with no indicator dot
    and no number.
  - The source line lists planned inputs.
  - `stress_status()` is never called, because it throws on `NA`
    (`# TODO(stress-score)`).
- [x] **River flow card** (in `condition_cards`): the latest stored
      15-minute reading for the selected gauge, classified by
      `classify_flow()` against the stored percentiles for its local
      calendar day. `flow_badge()` in `R/server.R` maps each bucket to
      Source A's badge wording:

      | `classify_flow()` result | Badge | Class |
      |---|---|---|
      | Much below normal | Unusually low | `badge-critical` |
      | Below normal | Below typical | `badge-warning` |
      | Normal | Near typical | `badge-good` |
      | Above normal | Above typical | `badge-good` |
      | Much above normal | Well above typical | `badge-good` |
      | Unknown (no percentiles) | Comparison unavailable | `badge-warning` |
      | No reading | Data unavailable | `badge-critical` |

      The source line reads, for example, "USGS 01101000 · Live ·
      Updated …". The comparison is a percent of the seasonal median.
- [x] **Rainfall card:** the real 7-day total from `precip_daily` at the
      Byfield gauge. Its source line reads "USGS Byfield gauge (single
      site) · <date window>".
  - Badge: "7-day total" (`badge-good`). It becomes "Data delayed"
    (`badge-warning`) when the latest stored day is more than 2 days
    old, and "Data unavailable" (`badge-critical`) with no data.
  - Comparison: a typical 7-day total is shown only when at least 3
    prior years have complete windows. Otherwise it reads "Comparison
    unavailable — gauge record began June 2025".
- [x] **Groundwater card:** the latest depth to water at well
      `USGS-424520070562401`, compared with the stored **median** (p50)
      depth for that calendar day. Depth is inverted, so a larger depth
      means a lower water table:
  - Deeper than the median: "Lower than seasonal median"
    (`badge-warning`), with "X ft deeper than seasonal median".
  - Shallower: "Higher than seasonal median" (`badge-good`).
  - A reading older than 48 hours shows "Data delayed". A missing
    reading shows "Data unavailable".
- [x] Illustrative **Municipal use (pumping) card:** Source A's values
      (2.8 MGD, "↑ 18% above seasonal average"), badged and sourced as
      Illustrative. No live municipal feed exists (Section 4).
- [x] **Ecology card** ("Ecological Flow", under "Water Context"): the
      Parker River gauge's latest flow compared with
      `eco_flow_threshold_cfs` from `config.yml` via `ecology_status()`.
      It shows "Above eco-flow threshold", "At or below eco-flow
      threshold", or "Comparison unavailable", and every comparison is
      marked provisional. **Pass 2 review decision:** the single
      configured threshold has only been confirmed for Parker River, so
      it is applied only when Parker is selected; selecting Ipswich shows
      "Comparison unavailable — threshold set for Parker River only"
      instead of silently reusing Parker's number (Section 12).
- [x] **Drought status card** (`drought_status_card`):
  - A headline built from `summarize_drought()`, for example "D2
    active — Severe Drought".
  - The cumulative county area for the latest week.
  - The line "U.S. Drought Monitor · county-level · weekly", the map
    date, and the last-updated time.
  - An explainer of the D0–D4 categories.
- [x] Illustrative **Water budget** (`water_budget_ui`):
  - Keeps Source A's example numbers for "2023 Annual": P 43.8, Qin 1.2,
    ET 23.4, Qout 18.9, and human use 1.8 in, giving ΔS +0.9 in.
  - Illustrative labeling appears in the hero badge ("◇ Illustrative"),
    the disclosure line, every component card's source line
    ("Illustrative · 2023 Annual"), and the section subtitle.
  - **Pass 2 review decision:** the "About this estimate" box no longer
    ends with a "View methodology →" link. It previously pointed at `#`
    (a dead link); `config.yml` has no dedicated methodology URL field,
    so the link was removed rather than left dead. Re-add it, gated by
    `is_safe_url()` like `management_links()`, once a real URL exists.
- [x] Illustrative **Local Water Management** (`local_management`):
      permit status, next hearing, an Illustrative badge with the note,
      and links. All of it comes from `config.yml`, and a link renders
      only when `is_safe_url()` is TRUE. **Pass 2 review decision:** the
      card's label reads "WATER WITHDRAWAL PERMIT", not "IPSWICH
      WITHDRAWAL PERMIT" as in Source A — `config.yml`'s
      `permit_status`/`next_hearing`/`note` are one manually maintained
      record with no watershed field, so the label neither names a
      specific river nor follows the WATERSHED selector (Section 12).
- [x] **"Water Availability vs Municipal Use" hero chart**
      (`hero_chart`, with `chart_stats_bar`, `chart_html_legend`,
      `chart_conversion_note`, `water_avail_subtitle`):
  - **"Compared with typical":** flow as a percent of the stored
    same-day median, with a dashed 100% line.
  - **"Actual values":** a stored p10–p90 ribbon, a dashed stored
    median, solid flow, and a dotted provisional ecological reference
    line.
  - Periods of 7 days or less plot the stored 15-minute readings;
    longer periods plot daily means.
  - **There is no municipal series.** The stats bar shows "MUNICIPAL
    WATER USE: Not available · No live municipal data connected". The
    note under the chart says municipal use is not shown because no
    live source is connected.
- [ ] "Current advisory" callout driven by flow status: not built.

### Details level (`view_level >= 2`)

- [x] **"Is this normal?" chart** (`seasonal_flow`, with
      `seasonal_flow_subtitle` and `seasonal_flow_takeaway`): this
      year's daily mean flow for the selected gauge, drawn as a solid
      water line with a marker on the latest day, over the stored
      p10–p90 ribbon and a dashed median for every calendar day. A
      dashed ink "Today" line marks the date. The takeaway states the
      latest value's percentile position and how many prior years back
      it.
- [x] **Low-flow days chart** (`low_flow_days`, with
      `low_flow_subtitle` and `low_flow_takeaway`): the selected gauge
      over the last 10 calendar years, the current partial year
      included. It has three small-multiple bar panels, one each for
      days below 1, 0.1, and 0.01 cfs, all in `--color-water`.
- [x] **Drought history** (`drought_history`, with
      `drought_history_subtitle` and `drought_history_takeaway`): a
      weekly, **stacked, stepped area chart**.
  - USDM county percentages are cumulative, so the server first
    converts them to non-overlapping category bands.
  - D4 sits at the bottom of the stack, so the top edge is the share of
    the county that is at least abnormally dry.
  - The colors step from neutral (D0, D1) through municipal (D2, D3) to
    warning (D4).
  - Hover shows the cumulative share, and a Plotly legend is shown.
- [x] **Evidence table** (`evidence_table`): `evidence_row()` rows for
      river flow, flow percentiles for today (p10–p90 and years used),
      the rainfall 7-day total, groundwater depth, the ecology
      comparison, and drought status. Each row carries its source and
      last-updated time.
- [-] **`seasonal_pumping`:** removed. No live municipal pumping data
      exists, so the monthly pumping chart is not built.
- [ ] "Summer creates a natural tension" narrative callout: not built.
- [ ] Monthly conditions comparison table (rainfall, flow, groundwater,
      municipal use vs. typical): not built.

### Evidence level (`view_level >= 3`)

- [x] **Data sources** (`data_sources`): one row per expected ETL source
      (the 13 listed in Section 9, plus any unexpected source found in
      `etl_runs`). Each row shows the endpoint, the last successful
      update, and the last run status with its error message. The panel
      also states that the dashboard never calls these services while
      you browse, and that official restrictions come from
      municipalities.
- [x] **Gauge map** (`gauge_map`, leaflet): OpenStreetMap tiles at
      reduced opacity with circle markers.
  - Stream gauges and the precipitation gauge are filled with
    `--color-water`. The groundwater well is filled with
    `--color-water-light` and has an ink outline.
  - Popups are escaped with `htmlEscape()`, and a legend sits
    bottom-right.
  - Watershed boundaries are not drawn yet.
- [x] Illustrative **Local Water Management**
      (`local_management_evidence`): an Illustrative badge, the note
      "Manually maintained in config.yml; not verified against current
      permit records", evidence rows (permit status, next hearing,
      ecological threshold, note), and safe links only.
- [x] **Water 101** (`water_101`): a static, conceptual water-budget
      explainer with no numbers. It explains why no live budget total
      is computed and how to read the charts.
- [-] **`conservation_guidance`:** removed. The "What can I do?" panel
      is not built.
- [ ] Map extras from the prototype (watershed boundaries, withdrawal
      layer, period toggle, conditions legend): not built. No live
      withdrawal data exists.
- [ ] Data download button: not built.

---

## 4. Data source feasibility matrix

| Data need | Source used | Live and free? | Notes |
|---|---|---|---|
| River discharge (00060) | USGS OGC API: `continuous` (15-min) and `daily` (statistic 00003, mean) | Yes, no key | `USGS-01101000` Parker River at Byfield; `USGS-01102000` Ipswich River near Ipswich. Daily history stored from 1990. |
| Same-day flow percentiles | Computed in the ETL from stored daily values (`etl/stats.R`) | Yes | Real quantiles of prior complete calendar years; replaces Source A's multiplier heuristic |
| Groundwater depth to water (72019) | USGS OGC API, well `USGS-424520070562401` (MA-NIW 27 Newbury) | Yes, no key | Daily record from 1984-10-17; only statistic 00003 is served |
| Precipitation (00045) | USGS OGC API, Byfield gauge `USGS-424510070564401`; daily statistic 00006 (sum) | Yes, no key | Co-located with the Parker gauge. **Record starts 2025-06-28.** |
| Drought severity (D0–D4) | U.S. Drought Monitor county statistics, FIPS `25009` (Essex County) | Yes, no key | County-level, weekly, **cumulative** percent area; the ETL keeps about 3 years |
| Precipitation from a NOAA weather station | — | **Removed** | Source A's NOAA integration was removed; the USGS gauge above replaces it and needs no API key |
| Evapotranspiration | PRISM / gridMET / OpenET | Not live | Gridded products that need geospatial processing; water budget stays Illustrative |
| Municipal withdrawals | MassDEP / Water Management Act reports | Not live | Annual reports, not a feed; pumping card stays Illustrative and the hero chart shows no municipal series |
| Permit status, next hearing, links | `config.yml` | Manual | Placeholders until IRWA/PRCWA confirm |
| Ecological flow threshold | `config.yml` (`eco_flow_threshold_cfs: 8`) | Manual | Provisional placeholder |

Site ids, parameter and statistic codes, history start dates, timeouts,
and endpoints all live in `etl/constants.R`, the single source of truth.

---

## 5. Architecture

```
USGS OGC API (api.waterdata.usgs.gov/ogcapi/v0)     U.S. Drought Monitor API
  flow · groundwater · precipitation                  county FIPS 25009
                 │                                          │
                 └──────────────┬───────────────────────────┘
                                ▼
          etl/run_etl.R  — fetch (httr2) → validate → write
          one tryCatch per source; a failure writes no data
                                │
                                ▼
          data/essexwater.sqlite  (gitignored; ESSEXWATER_DB_PATH)
          daily + 15-min series, percentile tables, drought weeks,
          etl_runs (one row per source per run)
                                │   read-only connection, parameterized DBI queries
                                ▼
          Shiny app: app.R → global.R → R/server.R
                     R/data_access.R · R/helpers.R · R/components.R
                     config.yml (manual content) · www/ (CSS, icons)
```

### Hard rules

1. **The app server never calls an external API.** All HTTP lives in
   `etl/`. Gate 4 checks that `app.R`, `global.R`, and `R/` make no
   network calls. The one outside request comes from the viewer's
   browser, which loads OpenStreetMap tiles for the gauge map.
2. **One failing source never stops the others.** Each source step runs
   in its own `tryCatch` and logs a `success` or `failure` row to
   `etl_runs`.
3. **All SQL is parameterized** (`params = list(...)`). Never build SQL
   with `sprintf()`, `paste()`, `paste0()`, or `glue()`. Gate 4 checks
   this statically, and SQL-injection tests cover `R/data_access.R`.
4. **No secrets anywhere.** Every source is keyless. `config.yml` holds
   only manual content.

### The ETL layer (`etl/`)

| File | Responsibility |
|---|---|
| `constants.R` | Sites, parameter and statistic codes, endpoints, history start dates, HTTP budgets (also sourced by the app for site ids) |
| `http_common.R` | Shared httr2 helpers for the OGC API: paging (`ogc_get_items`), 5-year date chunks, parsing to a series, failure notes |
| `fetch_flow.R`, `fetch_groundwater.R`, `fetch_precip.R` | `fetch_*_latest()` (15-min, trailing days) and `fetch_*_daily()` (date range). Each returns `NULL` on any failure and never throws. |
| `fetch_drought.R` | `fetch_drought_status(fips, days_back, today, start_date)`: sends `Accept: application/json` and requires HTTP 200 plus a JSON content type, because the endpoint returns CSV otherwise |
| `stats.R` | Pure statistics: `compute_same_day_stats()`, `build_percentile_table()`, `compute_precip_typical()`, `build_precip_typical_table()` |
| `db_schema.R` | `ensure_schema(con)` (idempotent `CREATE TABLE IF NOT EXISTS`), `seed_sites(con)` |
| `write_store.R` | `upsert_*()` writers, `log_etl_run()`, and incremental-run lookups (`read_daily_series()`, `latest_daily_date()`, `earliest_drought_date()`, `latest_drought_date()`) |
| `run_etl.R` | The orchestrator `run_etl(con, ...)`; fetchers are injectable for tests |

### The store (`data/essexwater.sqlite`)

| Table | Contents |
|---|---|
| `sites` | `site_no` (full OGC id, e.g. `USGS-01101000`), `kind` (`flow` / `groundwater` / `precip`), `parameter_code`, `name`, `watershed`, `lat`, `lon` |
| `flow_instantaneous`, `groundwater_instantaneous`, `precip_instantaneous` | 15-min values: `site_no`, `datetime` (UTC `YYYY-MM-DD HH:MM:SS`), value (`discharge_cfs` / `depth_ft` / `precip_in`), `approval_status`, `qualifier` |
| `flow_daily`, `groundwater_daily`, `precip_daily` | Daily values: `site_no`, `date` (`YYYY-MM-DD`), value, `approval_status`, `qualifier` |
| `flow_percentiles`, `groundwater_percentiles` | 366 rows per site: `month_nu`, `day_nu`, `p10`, `p25`, `p50`, `p75`, `p90`, `years_used` |
| `precip_typical` | 366 rows per site: `month_nu`, `day_nu`, `median_in`, `years_used` (stored, but the app does not use it for comparisons) |
| `drought_status` | `fips`, `map_date`, `d0`..`d4`: cumulative percent of county area (`d1` = D1 or worse) |
| `etl_runs` | `run_id`, `source`, `started_at`, `finished_at`, `status`, `rows_written`, `error_message` |

`etl_runs.source` names are `flow_latest:<site>`, `flow_daily:<site>`,
`flow_percentiles:<site>`, the matching `groundwater_*` and
`precip_latest` / `precip_daily` / `precip_typical` names, and
`drought:25009`.

### The app layer

Loading order: `shiny::runApp()` runs `app.R`. It sources `global.R`,
then `R/server.R`, then builds the UI and calls `shinyApp(ui, server)`.
`R/_disable_autoload.r` turns off Shiny's automatic sourcing of `R/`, so
these explicit calls are the only place app code is loaded.

| File | Responsibility |
|---|---|
| `app.R` | Source A's `page_fluid` UI: header, view-level pills and dots, the Summary flow, `conditionalPanel` sections for Details and Evidence, and a small `chart_card()` builder; loads `www/styles.css` then `www/dashboard_new.css` |
| `global.R` | Attaches shiny, bslib, shinyWidgets, htmltools, plotly, leaflet, DBI, and RSQLite. Sources `R/helpers.R`, `R/components.R`, `R/data_access.R`, and `etl/constants.R`. Opens the store with `open_store()`, loads `manual <- load_manual_content()`, and registers an `onStop()` disconnect. |
| `R/server.R` | `server()` plus its non-reactive helpers and chart builders: `CHART_COLORS` (tokens), `flow_badge()`, `build_hero_chart()`, `build_seasonal_chart()`, `build_low_flow_chart()`, `build_drought_chart()`, the illustrative water budget. It reads only through `R/data_access.R`. It lives in `R/`, because a root-level `server.R` would make Shiny ignore `app.R`. |
| `R/data_access.R` | Every store query (contract below) |
| `R/helpers.R` | Pure functions: `classify_flow()`, `summarize_drought()`, `is_safe_url()`, `evidence_row()`, `load_manual_content()`, `resolve_config_path()`, `resolve_db_path()`, `ecology_status()`, `format_last_updated()` |
| `R/components.R` | Source A's UI components: `stress_status()`, `condition_card()`, `dashboard_header(updated = NULL, watershed = NULL)`, `section_heading()` |
| `R/_disable_autoload.r` | Marker file that turns off Shiny's `R/` autoload |

**Opening the store.** `open_store()` opens the file at
`resolve_db_path()` read-only (`RSQLite::SQLITE_RO`). If the file does
not exist, or the read-only open fails, it logs a message and connects
to an **empty in-memory database** instead. Every `data_access.R`
function then returns its empty shape, and the page renders its
unavailable states. The connection is opened once at startup. An app
that started before the store existed keeps its empty in-memory store
until it is restarted.

**Refreshing.** `server()` runs a `reactivePoll` every **10 minutes**
that compares `get_source_status(con)` (source, last success, last
status). Every data reactive depends on that poll, so a new ETL run
shows up in open sessions within 10 minutes without a restart.

**Data-access contract (`R/data_access.R`).** Every function takes `con`
first, uses parameterized SQL, never throws, and returns the documented
empty shape when tables or rows are missing or a query errors.

| Function | Returns |
|---|---|
| `get_sites(con, kind = NULL)` | data.frame(`site_no`, `kind`, `parameter_code`, `name`, `watershed`, `lat`, `lon`) |
| `get_latest_reading(con, kind, site_no)` | list(`value`, `datetime`), both NA when empty |
| `get_instantaneous_series(con, kind, site_no, days, now = Sys.time())` | data.frame(`datetime` POSIXct UTC, `value`), ascending |
| `get_daily_series(con, kind, site_no, start_date, end_date)` | data.frame(`date`, `value`), ascending |
| `get_percentiles(con, kind, site_no, month_nu = NULL, day_nu = NULL)` | data.frame(`month_nu`, `day_nu`, `p10`..`p90`, `years_used`) |
| `get_precip_window_total(con, site_no, days = 7, end_date = NULL)` | list(`total_in`, `start_date`, `end_date`, `days_with_data`) |
| `get_precip_typical_window(con, site_no, days = 7, end_date, min_years = 3)` | list(`typical_in`, `years_used`): the median of prior years' complete same-window totals; `typical_in` is NA below `min_years` |
| `get_low_flow_days(con, site_no, years, thresholds = c(1, 0.1, 0.01))` | data.frame(`yr`, `threshold`, `days`), zero-filled grid |
| `get_drought_status(con, fips)` | data.frame(`fips`, `map_date`, `d0`..`d4`), ascending |
| `get_last_updated(con, source_prefix)` | Latest successful `finished_at`, or `NA_character_` |
| `get_source_status(con)` | data.frame(`source`, `last_success`, `last_status`, `last_error`) |

### Manual content (`config.yml`)

`load_manual_content()` reads `config.yml`, or the file named by
`ESSEXWATER_CONFIG_PATH`. It merges the file over documented defaults,
so a missing or malformed file degrades to "Not configured" text
instead of crashing. The file is read once at app startup, so restart
the app after editing it.

| Field | Current value | Used by |
|---|---|---|
| `permit_status` | "Status to be confirmed" | Local Water Management (Summary and Evidence) |
| `next_hearing` | "Date to be confirmed" | Local Water Management (Summary and Evidence) |
| `eco_flow_threshold_cfs` | `8` | Ecology card, hero chart ecological reference line, evidence rows — applied only when Parker River is selected (Section 12) |
| `learn_more_url`, `comment_url` | `""` (no link rendered) | Local Water Management |
| `note` | Placeholder pending IRWA/PRCWA confirmation | Local Water Management |

Never put API keys or tokens in this file. None are needed.

### Environment variables

| Variable | Default | Effect |
|---|---|---|
| `ESSEXWATER_DB_PATH` | `data/essexwater.sqlite` | Store location for both the ETL and the app |
| `ESSEXWATER_CONFIG_PATH` | `config.yml` | Manual-content file (tests point this at temporary files) |
| `NOT_CRAN` | unset | Set to `true` when running the gates, so skip-on-CRAN tests run |

---

## 6. Tech stack

- **R ≥ 4.1** (the code uses the native `|>` pipe).
- **Shiny** with `bslib::page_fluid()` and **no custom bslib theme**.
  All styling comes from `www/styles.css` and `www/dashboard_new.css`.
- **shinyWidgets** for the view-level and chart-mode
  `radioGroupButtons()`.
- **plotly** for charts and **leaflet** for the gauge map (OpenStreetMap
  tiles).
- **httr2** and **jsonlite** in the ETL, for the USGS OGC API and USDM.
  The legacy `dataRetrieval`/NWIS services are not used.
- **DBI** and **RSQLite** for the store, and **yaml** for `config.yml`.
- For testing: **testthat** (edition 3), **shinytest2** with
  **chromote** (headless Chrome), **mockery**, **httptest2**, **withr**,
  and **lintr**.

`DESCRIPTION` lists every dependency. `tests/security/dependency_notes.md`
is regenerated by Gate 4 and lists which ones the code references.

---

## 7. Design guidelines (condensed)

Full detail is in [`AESTHETIC_GUIDE.md`](AESTHETIC_GUIDE.md). Core
rules:

- Style stays exactly as Source A. The palette uses the design tokens
  in `dashboard_new.css`: ink `#172533` (header, text), water `#336891`
  (supply and normal), municipal `#F45932` (demand and caution), ecology
  `#C1DB70` (positive), and warning `#E75B52` (severe). The page is
  `#F0F0F1` with white 12px-radius cards. In R, chart and map colors
  come from `CHART_COLORS` in `R/server.R`.
- One scrolling page. The Summary / Details / Evidence pills set depth,
  not tabs. Never add `page_navbar`, `nav_panel`, or `value_box`, or a
  bslib theme or an extra stylesheet.
- New UI reuses `condition_card()`, `section_heading()`, `chart_card()`
  (`app.R`), and the existing classes. If a CSS rule is unavoidable,
  append it to the end of `dashboard_new.css` using existing tokens
  (AESTHETIC_GUIDE §10). The merged app's own additions sit under
  "MERGED APP ADDITIONS" at the end of that file: the stress banner
  padding, the 5-column Summary card grid, the watershed selector width,
  and `.evidence-rows`.
- In charts, dashed = historical, solid = current, and a shaded ribbon
  = percentile range. The "Today" marker is ink. Every chart gets a
  plain-language takeaway.
- Every illustrative element says "Illustrative" in text. Every live
  number has a source and a last-updated time or observation date
  beside it.
- Known CSS quirks are listed in AESTHETIC_GUIDE §11. Don't copy them,
  and don't "fix" them without a design decision.

---

## 8. Where the old code went

Source A's `R/data_connections.R` and `R/data_processing.R` no longer
exist. Their logic moved into the ETL, and the app now reads the store.

| Source A | Merged project |
|---|---|
| `R/data_connections.R` `get_usgs_flow()`: single latest value, fetched on page load | `etl/fetch_flow.R` `fetch_flow_latest()`: 30 days of 15-min data, stored in `flow_instantaneous` |
| `R/data_connections.R` `get_usgs_groundwater()` | `etl/fetch_groundwater.R` `fetch_groundwater_latest()` and `fetch_groundwater_daily()`, with statistic 00003 pinned explicitly |
| `R/data_connections.R` NOAA rainfall function | **Removed.** Replaced by `etl/fetch_precip.R` `fetch_precip_latest()` / `fetch_precip_daily()` on the USGS Byfield gauge (no API key) |
| `R/data_processing.R` `get_seasonal_median()`: daily pull plus `median()` | Daily pull → `fetch_flow_daily()`; statistics → `etl/stats.R` `build_percentile_table()` → `flow_percentiles` |
| `R/data_processing.R` `get_groundwater_seasonal_median()` | `fetch_groundwater_daily()` + `build_percentile_table()` → `groundwater_percentiles` |
| `R/data_processing.R` `get_rainfall_typical()` | `get_precip_typical_window()` in `R/data_access.R`: median of prior years' complete 7-day totals, needs ≥ 3 years. The ETL also stores `precip_typical`, but the app does not use it. |
| `app.R` percentile band `p10 = median * 0.52`, `p90 = median * 1.55` | **Replaced by real quantiles:** `stats::quantile(type = 7)` of the same month-day across prior complete calendar years (up to 36 years for flow) |
| `app.R` `seasonal_median_cfs()` / `generate_hero_data()` (synthetic series) | **Deleted.** The hero chart uses `flow_period_frame()` in `R/server.R`, which reads stored readings or daily means plus stored percentiles. |
| `app.R` `get_flow_status()` | **Deleted.** `R/helpers.R` `classify_flow()` plus `flow_badge()` in `R/server.R`, which keeps Source A's badge wording. |
| `app.R` hard-coded permit and hearing text | `config.yml` via `load_manual_content()` |
| `app.R` `stress_score` of 62; `output$stress_banner` never mounted | No score; `stress_banner` is mounted in its unavailable state (`# TODO(stress-score)`) |
| "Updated 8:15 PM" in `dashboard_header()` and the banner | `dashboard_header(updated = textOutput("header_updated"))` and the banner header, both from `format_last_updated()` on `etl_runs` |
| Static "Parker River" watershed display | `dashboard_header(watershed = selectInput("watershed", ...))`: Parker or Ipswich |
| `config.yml` | Not ported. The new `config.yml` is manual content only. |

From Source B (the architecture prototype), `summarize_drought()`,
`classify_flow()`, `is_safe_url()`, `evidence_row()`, and
`load_manual_content()` were ported into `R/helpers.R`.
`get_drought_status()` became `etl/fetch_drought.R` (httr → httr2, plus
a content-type check). `run_gates.R`, `.lintr`, `tests/security/`, and
the `global.R` / `R/server.R` / `R/_disable_autoload.r` loading pattern
were ported and adapted. Source B's teal bslib theme, `www/custom.css`,
`page_navbar`/`nav_panel`, and `value_box` were **not** ported.

---

## 9. ETL operations

### Running the ETL

From the project root (the script also resolves its own paths when run
from elsewhere):

```bash
Rscript etl/run_etl.R
```

The script creates the `data/` directory and schema if they are missing,
seeds `sites`, runs every source, and prints a per-source summary ending
in `N/13 sources succeeded.` (three steps each for Parker flow, Ipswich
flow, groundwater, and precipitation, plus drought).

**It exits 0 even when some sources fail.** Failures are recorded in
`etl_runs`, so read the summary or the table after each run.

To write the store somewhere else, set the path for both the ETL and the
app:

```bash
ESSEXWATER_DB_PATH=/srv/essexwater/essexwater.sqlite Rscript etl/run_etl.R
```

### First run vs. later runs

- **First run (backfill).**
  - Daily history is pulled from 1990-01-01 for flow and precipitation
    and from 1984-01-01 for groundwater, in 5-year chunks with 30-second
    request timeouts. It makes many requests and can take several
    minutes.
  - Precipitation returns data only from 2025-06-28.
  - Drought gets **3 years** of weekly USDM maps in one request
    (`drought_backfill_years = 3`).
- **Later runs (incremental).**
  - Each daily series is re-fetched from 45 days before its latest
    stored date, so provisional USGS revisions inside that window are
    picked up.
  - The 15-min tables take the trailing 30 days.
  - Drought re-fetches from 14 days before the latest stored map. It
    backfills the full 3 years again whenever the earliest stored map
    starts more than a week after the backfill start, for example in a
    store created before backfill existed.
- **Every run** rebuilds the percentile and typical tables from the
  *stored* daily history. They refresh even if that run's daily fetch
  failed.
- **Full refresh.** To re-pull all history (for example after older
  data was revised), call the orchestrator with `full_refresh = TRUE`:

  ```r
  # From the project root, in R
  source("etl/run_etl.R")  # defines functions only; does not run
  con <- DBI::dbConnect(RSQLite::SQLite(), Sys.getenv("ESSEXWATER_DB_PATH", "data/essexwater.sqlite"))
  ensure_schema(con)
  seed_sites(con)
  print_run_summary(run_etl(con, full_refresh = TRUE, verbose = TRUE))
  DBI::dbDisconnect(con)
  ```

  Pass `drought_backfill_years` to keep more or fewer years of drought
  maps.

### Checking ETL health

```r
con <- DBI::dbConnect(RSQLite::SQLite(), "data/essexwater.sqlite", flags = RSQLite::SQLITE_RO)
DBI::dbGetQuery(con, "
  SELECT source, status, finished_at, rows_written, error_message
  FROM etl_runs ORDER BY run_id DESC LIMIT 13")
DBI::dbDisconnect(con)
```

In the app, the Evidence level's data sources panel (`data_sources`)
shows the endpoint, the last success, and the last run status for every
source.

### Scheduling

No scheduler ships with this repository, and **no GitHub Actions
workflow is included yet**. Run the ETL on a schedule on a machine whose
store the app can read.

- **Cadence.** Once or twice a day is enough. USGS daily values update
  daily, and USDM maps are weekly.
- **cron example** (daily at 06:15). Use absolute paths, because cron
  has a minimal environment:

  ```cron
  15 6 * * * cd /path/to/essex-water-dashboard && /usr/local/bin/Rscript etl/run_etl.R >> "$HOME/essexwater-etl.log" 2>&1
  ```

- **Posit Connect (self-managed) or Workbench jobs.** Run the same
  script as a scheduled job. Point `ESSEXWATER_DB_PATH` at a persistent
  location that the deployed app also reads.
- **GitHub Actions (not included).** A workflow could run the ETL, but
  the store is gitignored (and Gate 4 checks it stays ignored). The
  workflow would have to publish the database somewhere the app can
  read, not commit it.

**Concurrency.** The app holds a read-only connection, and the ETL is
the only writer. `data_access.R` functions never throw: if a query fails
while the ETL holds a write lock, that render shows its empty or
unavailable state, and the next input change or the 10-minute poll
re-queries. Open sessions pick up a finished run through the poll.

---

## 10. Testing and quality gates

### Run all four gates

From the project root:

```bash
NOT_CRAN=true Rscript run_gates.R
```

The gates run fully offline: HTTP is mocked and the databases are
temporary fixtures. Each gate runs in a fresh `Rscript` subprocess. The
command exits 0 only if all four pass. A gate that matches no test
files, or runs zero expectations, counts as a **FAIL**.

| Gate | Test files | What it covers |
|---|---|---|
| 1. Unit | Every `tests/testthat/test-*.R` not claimed below | Helpers, `stress_status()` thresholds, `condition_card()` (including ecology), every ETL fetcher with mocked httr2 (success, empty, HTTP error, network error, malformed, drought CSV content type), drought backfill and incremental windows, `write_store` idempotency, `run_etl` partial failure, `stats.R`, and every `data_access` function against fixture and empty stores |
| 2. Integration | `test-integration-*.R` (currently `test-integration-server.R`) | `shiny::testServer()` on the real `R/server.R` with a seeded temporary SQLite: watershed and period switches re-fire cards, header, and charts; ecology follows flow vs. threshold; the hero and "Is this normal?" charts plot stored percentiles, not the old multipliers; drought card and history, evidence table, data sources, map, low-flow chart, and Water 101 render; `seasonal_pumping` and `conservation_guidance` are absent; the stress banner shows no score; the rain card shows the real 7-day total; empty-store degradation; malicious `config.yml` is escaped with no `javascript:` or `data:` links; `app.R` builds unique ids with every server output mounted |
| 3. Functional | `test-shinytest2*.R` | shinytest2 in headless Chrome against a fixture store. It is planned to cover: boot without errors, the removed vendor name never appearing, view-level icons loading, `view_level` toggling sections and dots, charts, map and evidence rendering, CSS resolving to `dashboard_new.css`, degraded mode, and escaped malicious config. |
| 4. Security | `test-security-*.R` **and** `tests/security/run_security_checks.R` | HTML escaping, URL safety, SQL injection, plus script checks: (a) secret scan, (b) lint, (c) no string-built SQL, (d) no HTTP in the app layer, (e) dependency audit (rewrites `dependency_notes.md`), (f) `.gitignore` effectiveness |

Gate 3 needs Chrome or Chromium available to **chromote**. Its test
files are being added. Until one exists, `run_gates.R` reports Gate 3
as FAIL ("no tests found").

Test helpers:

- `tests/testthat/helper-load.R` sources `R/helpers.R`,
  `R/components.R`, and `R/data_access.R`.
- `tests/testthat/helper-db.R` provides `build_fixture_db()` (seeded
  values documented at the top of the file) and `build_empty_db()`.

### Before every commit

```bash
Rscript tests/security/secret_scan.R
```

The scan must print `PASS`. It checks tracked and untracked
(non-ignored) files for credential-shaped values and secret-bearing
filenames (`.Renviron`, `.env`, `.Rhistory`, `.RData`, `rsconnect/`,
and others). It also fails if any code file (`.R`, `.yml`, `.css`,
`.js`, `.dcf`) references NOAA (removed), `config::get()`,
`library(config)`, or `.Renviron`. That includes test files.

Lint on its own:

```bash
Rscript -e 'lintr::lint_dir(".")'
```

### Refreshing HTTP fixtures

`scripts/capture_fixtures.R` calls each live endpoint once and saves
trimmed responses under `tests/testthat/fixture-data/`. Re-run it only
when upstream response shapes may have changed, then re-run the gates.

---

## 11. Deployment

**The ETL must run before the app has data.** If the store file is
missing when the app starts, `global.R` logs "Store not found … Run
etl/run_etl.R", connects to an empty in-memory database, and every
output shows its unavailable state. The app does **not** switch to the
file once the ETL creates it, so restart the app after the first ETL
run.

Deployment steps:

1. Install the packages from `DESCRIPTION` on the host.
2. Run `Rscript etl/run_etl.R` so the store exists at the path the app
   will read (`ESSEXWATER_DB_PATH`).
3. Start or deploy the app with that same path.
4. Schedule the ETL (Section 9). Later runs are picked up by the
   running app within 10 minutes.

**Posit Connect Cloud** is the intended public host: Git-backed
deployment, with a free tier. Two constraints matter for this
architecture:

- `data/*.sqlite` is gitignored, so a Git-backed deployment does **not**
  include the store. Either deploy in a way that bundles a freshly
  built store with the app and republish after each ETL run, or host
  the store where the running app can read it.
- A scheduled ETL must write to storage the app can see. Before relying
  on scheduled execution or persistent storage on Connect Cloud,
  confirm the platform's current support. On a server you control (Posit
  Connect, Shiny Server, a VM), cron or a scheduled job with a shared
  `ESSEXWATER_DB_PATH` is straightforward.

Keep deployment metadata out of Git. `rsconnect/` is gitignored, and the
secret scan rejects it. The app needs no environment secrets. Viewers'
browsers must be able to reach OpenStreetMap's tile servers for the map
background.

---

## 12. Known limitations to communicate to IRWA/PRCWA

- **Stress score not yet derived.** No validated formula exists, so the
  banner shows the score as unavailable and never shows a number
  (`# TODO(stress-score)`).
- **Precipitation history is short.** The Byfield gauge record begins
  2025-06-28. The rainfall card shows real 7-day totals, but the
  "typical" comparison needs at least 3 prior years with complete
  windows. Until then it reads "Comparison unavailable — gauge record
  began June 2025". By the prior-calendar-year rule, that is roughly
  mid-2028 for July–December windows and 2029 for January–June windows.
- **Ipswich has flow only.** The watershed selector switches the
  flow-based card, the ecology card, and the flow charts to the Ipswich
  gauge. The groundwater well and rain gauge are in the Parker River
  watershed, and their cards stay the same for either selection.
- **One ecological threshold, Parker only.** `config.yml` holds a single
  provisional `eco_flow_threshold_cfs` (8 cfs), confirmed only for the
  Parker River gauge. **Resolved in pass 2 review:** the ecology card,
  the hero chart's dotted reference line, and its caption apply the
  threshold only when Parker is selected. Selecting Ipswich shows
  "Comparison unavailable — threshold set for Parker River only", the
  reference line is not drawn, and the card tooltip explains why. A
  second, Ipswich-specific threshold would need its own confirmation and
  its own `config.yml` field before this could show a live comparison for
  Ipswich too.
- **One groundwater well.** MA-NIW 27 Newbury is a local indicator. It
  does not measure aquifer or watershed storage.
- **Drought is county-level and weekly.** USDM statistics cover all of
  Essex County (FIPS 25009), not the watershed. The ETL keeps about 3
  years of weekly maps (157 in the local store when this was written).
- **USGS data is provisional.** Recent values are marked Provisional and
  may be revised. Incremental runs re-fetch 45 days, so older revisions
  need a full refresh.
- **Municipal use and ET are not live.** The pumping card and the water
  budget are Illustrative. The hero chart, titled "Water Availability vs
  Municipal Use", shows no municipal series and says so.
- **The water budget shows Source A's example numbers.** They are
  labeled Illustrative throughout: the "◇ Illustrative" provenance
  badge, a `wb-hero-disclosure` line, "Illustrative value — not a
  measured total" on every input/output card, and the CSS scoping inside
  `.water-budget-section` that renders every badge in the quieter neutral
  style (`#EEF3F5` / `#687987`, 8px uppercase) instead of the colored
  live-data badges. **Resolved in pass 2 review:** the "View methodology
  →" link, which pointed at `#`, was removed rather than left dead —
  `config.yml` (out of scope for this pass) has no methodology URL field
  to gate it on.
- **The local water management label is not watershed-specific.**
  Source A always read "IPSWICH WITHDRAWAL PERMIT" here, regardless of
  any selector. `config.yml`'s `permit_status`/`next_hearing`/`note` are
  one manually maintained record with no watershed field, and nothing in
  the docs or config established that record as specifically about the
  Ipswich River, so pass 2 review generalized the label to "WATER
  WITHDRAWAL PERMIT" instead of either leaving an unverified river name
  or fabricating a second, watershed-reactive record `config.yml` does
  not have.
- **Manual content is placeholder.** Permit status, the next hearing,
  links, and the 8 cfs ecological threshold wait on IRWA/PRCWA
  confirmation. The app reads `config.yml` only at startup.
- **Map is basic.** The map shows OpenStreetMap tiles loaded by the
  viewer's browser and gauge markers. Watershed boundaries and a
  withdrawal layer are not drawn.
- **Percentile coverage varies.** Flow percentiles use up to 36 prior
  years (1990–2025) and groundwater up to 42. Feb 29 uses only leap
  years. A month-day with no prior data shows as unavailable.
- **The data is only as fresh as the last ETL run.** There is no
  scheduler in the repo yet, and no GitHub Actions workflow. An app
  started before the store existed needs a restart.
- **CSS quirks.** Several inherited cascade inconsistencies are
  documented and deliberately left in place. See AESTHETIC_GUIDE §11.
- **Extending to other watersheds** means re-verifying data availability
  for each new site (OGC collections, statistic codes, record length),
  not just editing `etl/constants.R`.

---

## 13. Adding a feature

1. **Decide: live or illustrative?** If no free, keyless source exists,
   the feature is Illustrative and must say so in text (Section 2).
2. **New live source.** Add its codes to `etl/constants.R`. Write a
   `fetch_*()` with httr2, `req_timeout()`, and `.null_on_failure()`
   (return `NULL`, never throw). Add its table to `ensure_schema()`, an
   `upsert_*()` to `write_store.R`, and a `run_step()` to `run_etl()`.
   `ensure_schema()` only creates missing tables. It does not alter
   existing ones, so a column change to an existing table needs a
   manual migration or a rebuilt store. Capture fixtures and add mocked
   tests (success, empty, HTTP error, network error, malformed). Add the
   source prefix to `SOURCE_CATALOG` in `R/server.R` so the data sources
   panel describes it.
3. **Data access.** Add a function to `R/data_access.R`: `con` first,
   parameterized SQL, never throws, documented empty shape. Test it
   against `build_fixture_db()` and `build_empty_db()`, plus an
   injection case.
4. **UI.** Build it from existing components, `chart_card()`,
   `CHART_COLORS`, and the tokens (AESTHETIC_GUIDE §10). Make its
   reactive depend on `store_version()` so it refreshes with the poll.
   Place it at the right view level with a unique output id. Give it a
   source line, a last-updated time, and a degraded state.
5. **Verify.** Add `testServer()` coverage in
   `test-integration-server.R`. Run `NOT_CRAN=true Rscript run_gates.R`
   and `Rscript tests/security/secret_scan.R`, then update this guide's
   Section 3 checklist and Section 12 limitations.
