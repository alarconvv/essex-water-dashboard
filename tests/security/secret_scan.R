#!/usr/bin/env Rscript
# Secret scan. Run from the project root before every commit:
#   Rscript tests/security/secret_scan.R
# Scans tracked and untracked-but-not-ignored files. Exits non-zero on any
# credential-shaped value, secret-bearing filename, or leftover NOAA/config
# reference in code. The app uses no API keys, so any hit is a regression.

list_files <- function() {
  in_git <- identical(
    suppressWarnings(system2("git", c("rev-parse", "--is-inside-work-tree"),
                             stdout = TRUE, stderr = FALSE)),
    "true"
  )
  if (in_git) {
    files <- system2("git", c("ls-files", "--cached", "--others", "--exclude-standard"),
                     stdout = TRUE)
  } else {
    files <- list.files(".", recursive = TRUE, all.files = TRUE, no.. = TRUE)
    files <- files[!grepl("^\\.git/", files)]
  }
  files[file.exists(files) & !dir.exists(files)]
}

read_lines_if_text <- function(path) {
  size <- file.info(path)$size
  if (is.na(size) || size == 0 || size > 5e6) return(NULL)
  raw <- readBin(path, "raw", n = size)
  if (any(raw == as.raw(0))) return(NULL)
  strsplit(rawToChar(raw), "\n", fixed = TRUE)[[1]]
}

any_file_patterns <- c(
  aws_access_key = "AKIA[0-9A-Z]{16}",
  credential_assignment = paste0(
    "(?i)(api[_-]?key|secret|password|passwd|token)[\"']?\\s*[:=]\\s*",
    "[\"']?[A-Za-z0-9_\\-]{16,}"
  ),
  bearer_token = "(?i)bearer\\s+[A-Za-z0-9._\\-]{20,}",
  private_key = "-----BEGIN [A-Z ]*PRIVATE KEY-----"
)
code_patterns <- c(
  noaa_reference = "(?i)noaa",
  config_package_lookup = "config::get\\(|library\\(config\\)",
  renviron_lookup = "(?i)\\.renviron"
)
code_ext <- "\\.(R|r|Rmd|rmd|yml|yaml|css|js|dcf)$"
forbidden_name <- paste0(
  "(^|/)(\\.Renviron|[^/]*\\.Renviron|\\.env|credentials\\.json|",
  "\\.Rhistory|\\.RData|\\.httr-oauth)$|(^|/)rsconnect/"
)
self_path <- file.path("tests", "security", "secret_scan.R")

files <- list_files()
findings <- character(0)

for (f in files) {
  if (grepl(forbidden_name, f, perl = TRUE)) {
    findings <- c(findings, sprintf("%s: secret-bearing file must not be in the repo", f))
  }
  if (f == self_path) next
  lines <- read_lines_if_text(f)
  if (is.null(lines)) next

  patterns <- any_file_patterns
  if (grepl(code_ext, f)) patterns <- c(patterns, code_patterns)
  for (nm in names(patterns)) {
    idx <- grep(patterns[[nm]], lines, perl = TRUE)
    if (length(idx) > 0) {
      findings <- c(findings, sprintf("%s:%d: %s", f, idx, nm))
    }
  }
}

cat(sprintf("Secret scan: %d file(s) scanned.\n", length(files)))
if (length(findings) > 0) {
  cat("FAIL\n", paste0("  ", findings, collapse = "\n"), "\n", sep = "")
  quit(status = 1)
}
cat("PASS: no secrets found.\n")
