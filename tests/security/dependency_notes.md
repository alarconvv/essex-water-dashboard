# Dependency audit notes

Generated: 2026-09-15 by `tests/security/run_security_checks.R` (check e).

Result: **PASS** -- 18 package(s) installed and on CRAN; no undeclared usage

Every package in `DESCRIPTION` `Imports`/`Suggests` is checked for a local
installation and for a current CRAN listing (`available.packages()`), which
catches dependencies archived or removed from CRAN. This is a
maintenance-risk signal, not a vulnerability (CVE) scan.

CRAN index: reachable

| Package | Field | Installed | On CRAN |
|---|---|---|---|
| shiny | Imports | yes | yes |
| bslib | Imports | yes | yes |
| shinyWidgets | Imports | yes | yes |
| plotly | Imports | yes | yes |
| htmltools | Imports | yes | yes |
| httr2 | Imports | yes | yes |
| jsonlite | Imports | yes | yes |
| yaml | Imports | yes | yes |
| DBI | Imports | yes | yes |
| RSQLite | Imports | yes | yes |
| leaflet | Imports | yes | yes |
| testthat | Suggests | yes | yes |
| shinytest2 | Suggests | yes | yes |
| mockery | Suggests | yes | yes |
| httptest2 | Suggests | yes | yes |
| chromote | Suggests | yes | yes |
| lintr | Suggests | yes | yes |
| withr | Suggests | yes | yes |

## Packages used in code but missing from DESCRIPTION

Scanned `app.R`, `global.R`, `R/`, `etl/` for `pkg::` and `library()`/`require()`/`requireNamespace()`.
Base-R packages are exempt.

None.

## Imports not (yet) referenced by `pkg::` or `library()` in scanned code

Informational only (code may not be written yet, or may rely on attached packages).

None.

