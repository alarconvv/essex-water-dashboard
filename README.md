# Essex County Water Dashboard

A public-facing Shiny dashboard of water availability and use in the
**Parker River** and **Ipswich River** watersheds of Essex County,
Massachusetts. It is built for residents, public officials, and the
Ipswich River Watershed Association (IRWA) and Parker River Clean Water
Association (PRCWA). Its goal is that decisions about drought and water
use start from the same sourced evidence.

Every number is either read from public USGS and U.S. Drought Monitor
data, or explicitly labeled **Illustrative**. Nothing is fabricated.

<!-- Screenshot: add a capture of the Summary view here, e.g. ![Summary view](docs/screenshot-summary.png), once the merged UI is final. -->

> **Status:** working prototype. Some figures, such as municipal water
> use and the water budget, are illustrative placeholders, and the
> watershed stress score is not yet derived. See
> [Known limitations](#known-limitations).

## What it shows

The dashboard is one scrolling page. The **Summary / Details /
Evidence** buttons add depth to it; they are not separate tabs.

- **Summary:**
  - the stress banner (score not yet available)
  - current condition cards (river flow, 7-day rainfall, groundwater,
    illustrative municipal use, ecological flow check)
  - U.S. Drought Monitor status for Essex County
  - the water budget example (Illustrative)
  - local water-management information (manually maintained)
  - a river flow chart against real historical percentiles
- **Details:**
  - an "Is this normal?" chart of this year's flow against prior years
  - low-flow days per year
  - drought history since 2023
  - an evidence table with the source and update time of every live
    number
- **Evidence:**
  - data sources, each with its endpoint and last successful update
  - a gauge map
  - manually maintained local water-management details
  - a Water 101 explainer

The **WATERSHED** selector in the header switches the flow-based cards
and charts between the Parker River and Ipswich River gauges. The page
checks for new data every 10 minutes.

## Quick start

**Requirements:** R 4.1 or newer. Chrome or Chromium is needed only for
the functional test gate.

1. Install the R packages listed in `DESCRIPTION`:

   ```bash
   Rscript -e 'install.packages("remotes"); remotes::install_deps(dependencies = TRUE)'
   ```

2. Build the local data store. The first run downloads the full USGS
   history and can take several minutes. Later runs are incremental.

   ```bash
   Rscript etl/run_etl.R
   ```

   This writes `data/essexwater.sqlite` and prints how many of the 13
   sources succeeded. A failed source is logged and skipped. It never
   stops the others.

3. Run the app from the project root:

   ```bash
   Rscript -e 'shiny::runApp()'
   ```

   `app.R` loads `global.R` and `R/server.R` itself.

The app server reads only the local store and never calls an external
API. The only outside request is the map background: the viewer's
browser loads OpenStreetMap tiles.

If you skip step 2, the app starts with an empty store and shows
"No data" or "Unavailable" everywhere. Restart it after the first ETL
run. Later runs are picked up automatically.

### Configuration

| What | Where |
|---|---|
| Permit status, next hearing, links, ecological flow threshold | `config.yml` (manually maintained; no secrets) |
| Store location | `ESSEXWATER_DB_PATH` (default `data/essexwater.sqlite`) |
| Manual-content file location | `ESSEXWATER_CONFIG_PATH` (default `config.yml`) |

## Data sources and attribution

| Data | Provider | Details |
|---|---|---|
| Streamflow | U.S. Geological Survey, [Water Data for the Nation](https://waterdata.usgs.gov/) (OGC API) | Parker River at Byfield (`USGS-01101000`); Ipswich River near Ipswich (`USGS-01102000`) |
| Groundwater level | U.S. Geological Survey | Observation well MA-NIW 27 Newbury (`USGS-424520070562401`), depth to water |
| Precipitation | U.S. Geological Survey | Byfield precipitation gauge (`USGS-424510070564401`); record begins June 2025 |
| Drought severity | [U.S. Drought Monitor](https://droughtmonitor.unl.edu/), produced by the National Drought Mitigation Center at the University of Nebraska-Lincoln, USDA, and NOAA | Essex County (FIPS 25009), weekly; about 3 years stored |
| Permit and hearing information, ecological threshold | Manually maintained in `config.yml` | Placeholders pending IRWA/PRCWA confirmation |

USGS data are provisional and subject to revision. Percentile
comparisons are computed by this project from the USGS daily record.

## Architecture

```
USGS OGC API (flow, groundwater, precipitation)    U.S. Drought Monitor
                    │                                      │
                    └─────────────────┬────────────────────┘
                                      ▼
            etl/run_etl.R   fetch → validate → store (scheduled)
                                      │
                                      ▼
            data/essexwater.sqlite   (+ etl_runs log per source)
                                      │  read-only, parameterized queries
                                      ▼
            Shiny app   app.R · global.R · R/server.R · R/data_access.R
                        R/helpers.R · R/components.R · www/
```

Details (schema, data-access contract, ETL operations, and deployment)
are in [`DEVELOPMENT_GUIDE.md`](DEVELOPMENT_GUIDE.md). Visual design
rules are in [`AESTHETIC_GUIDE.md`](AESTHETIC_GUIDE.md).

```
essex-water-dashboard/
  app.R  global.R  config.yml  run_gates.R  DESCRIPTION
  R/        app layer: server, data access, helpers, UI components
  etl/      fetchers, statistics, schema, writers, orchestrator
  www/      styles.css, dashboard_new.css, icons/
  scripts/  capture_fixtures.R (refresh HTTP test fixtures)
  tests/    testthat suites and tests/security/ checks
  data/     local SQLite store (gitignored)
```

## Running tests

Run all four gates (unit, integration, functional, security) from the
project root. They run fully offline:

```bash
NOT_CRAN=true Rscript run_gates.R
```

Before every commit, run the secret scan. It must print `PASS`:

```bash
Rscript tests/security/secret_scan.R
```

## Known limitations

- The watershed stress score is not yet derived and is shown as
  unavailable.
- The precipitation record starts June 2025, so a "typical rainfall"
  comparison is unavailable until at least 3 prior years accumulate.
- The Ipswich River has streamflow only. The groundwater well and rain
  gauge are in the Parker River watershed, so those cards don't change
  with the watershed selector.
- The ecological flow check uses config.yml's one provisional threshold,
  which has only been confirmed for the Parker River gauge. Selecting
  Ipswich shows "Comparison unavailable — threshold set for Parker River
  only" (card, chart reference line, and chart note) instead of reusing
  Parker's threshold.
- A single groundwater well is a local indicator, not a measure of
  watershed storage.
- The "Local Water Management" permit card is one manually maintained
  record (`config.yml`), not one per watershed, so its label ("WATER
  WITHDRAWAL PERMIT") does not name a specific river and does not follow
  the WATERSHED selector.
- Drought status is county-level and weekly. The ETL keeps about 3
  years of weekly maps.
- USGS data are provisional and may be revised.
- Municipal water use and evapotranspiration have no free live source.
  Those figures are Illustrative.
- The data is only as fresh as the last ETL run. No scheduler or GitHub
  Actions workflow is included yet.

The full list is in [DEVELOPMENT_GUIDE.md §12](DEVELOPMENT_GUIDE.md#12-known-limitations-to-communicate-to-irwaprcwa).

## Security

The app uses no API keys or secrets: every data source is public and
keyless, and the former NOAA integration was removed.

## License

TBD — choose before publishing.
