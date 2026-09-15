# etl/constants.R
#
# Single source of truth for the monitoring sites, parameter codes, and
# endpoints used by the ETL (etl/*.R) and, later, the app's data-access
# layer. No API keys: every source here is a public, keyless endpoint.
#
# Site identifiers are stored exactly as the USGS OGC API names them
# (`monitoring_location_id`, e.g. "USGS-01101000"), and that full string is
# what goes into every `site_no` column in the SQLite store.
#
# Live verification (2026-09-14/15, api.waterdata.usgs.gov/ogcapi/v0):
#   - USGS-01101000 Parker River: latest-continuous, continuous (15-min,
#     statistic 00011) and daily (statistic 00003) all return 00060 data.
#   - USGS-01102000 Ipswich River: the same three collections DO return
#     00060 data (e.g. latest 1.57 ft^3/s at 2026-09-15T03:00Z; daily mean
#     2.30 ft^3/s on 2026-09-01), so Ipswich is kept.
#   - USGS-424520070562401 well (MA-NIW 27 NEWBURY): 72019 continuous
#     (statistic 00011) and daily (only statistic 00003 is served), daily
#     record 1984-10-17 onward.
#   - USGS-424510070564401 Byfield precipitation gauge: 00045 continuous
#     (15-min increments, no statistic) and daily statistic 00006 (sum).
#     00006 values are plausible daily totals in inches (1.07 in on
#     2026-09-13, 1.90 in on 2026-05-30; Jan-Sep 2026 total 25.24 in).
#     The record only begins 2025-06-27 (continuous) / 2025-06-28 (daily),
#     so "typical" precipitation has at most one prior year for Jul-Dec and
#     none for Jan-Jun until more history accrues.
#   Coordinates below are from the OGC monitoring-locations collection.

USGS_OGC_BASE_URL <- "https://api.waterdata.usgs.gov/ogcapi/v0"
USDM_COUNTY_STATS_URL <- paste0(
  "https://usdmdataservices.unl.edu/api/CountyStatistics/",
  "GetDroughtSeverityStatisticsByAreaPercent"
)

# Parameter / statistic codes ------------------------------------------------

PARAM_DISCHARGE <- "00060" # streamflow, ft^3/s
PARAM_GW_DEPTH <- "72019" # depth to water below land surface, ft
PARAM_PRECIP <- "00045" # precipitation, inches

STAT_MEAN <- "00003" # daily mean (flow, groundwater)
STAT_SUM <- "00006" # daily sum (precipitation) -- verified live, see header

# Sites ----------------------------------------------------------------------

SITE_PARKER <- "USGS-01101000"
SITE_IPSWICH <- "USGS-01102000"
GW_SITE <- "USGS-424520070562401"
PRECIP_SITE <- "USGS-424510070564401"

FLOW_SITES <- c(SITE_PARKER, SITE_IPSWICH)

ESSEX_COUNTY_FIPS <- "25009"

# One entry per row of the `sites` table. kind is one of
# "flow" / "groundwater" / "precip".
SITES <- list(
  list(
    site_no = SITE_PARKER, kind = "flow", parameter_code = PARAM_DISCHARGE,
    name = "Parker River at Byfield, MA", watershed = "Parker River",
    lat = 42.752869, lon = -70.945610
  ),
  list(
    site_no = SITE_IPSWICH, kind = "flow", parameter_code = PARAM_DISCHARGE,
    name = "Ipswich River near Ipswich, MA", watershed = "Ipswich River",
    lat = 42.659816, lon = -70.893662
  ),
  list(
    site_no = GW_SITE, kind = "groundwater", parameter_code = PARAM_GW_DEPTH,
    name = "MA-NIW 27 Newbury, MA (observation well)", watershed = "Parker River",
    lat = 42.755372, lon = -70.939489
  ),
  list(
    site_no = PRECIP_SITE, kind = "precip", parameter_code = PARAM_PRECIP,
    name = "Byfield precipitation at Byfield, MA", watershed = "Parker River",
    lat = 42.752822, lon = -70.945586
  )
)

# History windows ---------------------------------------------------------------
# Start dates for the first (backfill) pull of each daily series. Flow and
# precipitation follow Source A's 1990 start; groundwater follows Source A's
# 1984 start. Later runs fetch incrementally (see run_etl()).

FLOW_HISTORY_START <- as.Date("1990-01-01")
GW_HISTORY_START <- as.Date("1984-01-01")
PRECIP_HISTORY_START <- as.Date("1990-01-01")

# HTTP budgets ---------------------------------------------------------------------
# Per-request timeouts in seconds. Daily history is pulled in
# DAILY_CHUNK_YEARS-year windows so no single request approaches the
# timeout (a single 1984-2025 groundwater page took ~49 s live).

HTTP_TIMEOUT_LATEST <- 30
HTTP_TIMEOUT_DAILY <- 30
HTTP_TIMEOUT_DROUGHT <- 10
DAILY_CHUNK_YEARS <- 5
OGC_PAGE_LIMIT <- 10000 # API maximum is 50000
OGC_MAX_PAGES <- 20 # per request chain; exceeding it is a failure, never a silent truncation
