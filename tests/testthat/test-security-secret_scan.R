# tests/testthat/test-security-secret_scan.R
#
# Gate 4 (security). tests/security/secret_scan.R is a top-level script, not
# a function library, so it is exercised here as a subprocess against a
# scratch git repo (never the real repo) -- the same technique
# run_security_checks.R's .gitignore check (f) uses. Proves the scanner
# actually fires on planted examples, in every extension it claims to cover.
#
# Regression: code_ext originally matched only R/yml/yaml/css/js/dcf, so an
# R Markdown report file mentioning the removed weather vendor or the old
# config-file lookup was invisible to the scanner. code_ext now includes
# Rmd/rmd.
#
# The planted fixture content below is built with paste0() rather than
# written as literal strings, the same trick run_security_checks.R's check
# (f) uses for its env-file names: this file itself must not contain the
# very patterns it is testing for, or the real secret scan (run over this
# repo, not the scratch one) would flag it as a false positive.
skip_if_not(nzchar(Sys.which("git")), "git not available")

vendor <- paste0("no", "aa") # the removed weather-data vendor's name
env_suffix <- paste0("Ren", "viron")
config_lookup <- paste0("config", "::", "get(\"x\")")

scan_script <- testthat::test_path("..", "..", "tests", "security", "secret_scan.R")
rscript_bin <- file.path(R.home("bin"), "Rscript")

run_scan_in_scratch <- function(files) {
  scratch <- tempfile("secret_scan_check_")
  dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE, force = TRUE), add = TRUE)
  for (rel in names(files)) {
    path <- file.path(scratch, rel)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines(files[[rel]], path)
  }
  git_base <- c("-C", shQuote(scratch), "-c", "core.excludesFile=/dev/null")
  system2("git", c(git_base, "init", "-q"), stdout = FALSE, stderr = FALSE)
  system2("git", c(git_base, "add", "-A"), stdout = FALSE, stderr = FALSE)
  # secret_scan.R always scans getwd() and takes no CLI args of its own, so
  # set the working directory with an -e wrapper instead. The R code is
  # R-quoted with deparse() (not shell-quoted) and the whole argument is
  # shell-quoted exactly once, so paths with spaces survive both layers.
  r_code <- sprintf("setwd(%s); source(%s)", deparse(scratch), deparse(normalizePath(scan_script)))
  out <- suppressWarnings(system2(
    rscript_bin, c("-e", shQuote(r_code)),
    stdout = TRUE, stderr = TRUE
  ))
  status <- attr(out, "status")
  list(status = if (is.null(status)) 0L else as.integer(status), out = paste(out, collapse = "\n"))
}

test_that("a clean scratch repo passes", {
  res <- run_scan_in_scratch(list("R/ok.R" = "f <- function(x) x + 1"))
  expect_equal(res$status, 0L)
  expect_match(res$out, "PASS")
})

test_that("a vendor-name reference inside an R Markdown file is caught (regression: Rmd was unscanned)", {
  res <- run_scan_in_scratch(list("report.Rmd" = c("---", "title: x", "---", "", paste0("Data from `r", vendor, "`."))))
  expect_false(identical(res$status, 0L))
  expect_match(res$out, paste0(vendor, "_reference"))
})

test_that("the removed config-file lookup inside an R Markdown file is caught", {
  res <- run_scan_in_scratch(list("notes.Rmd" = paste0("```{r}\n", config_lookup, "\n```")))
  expect_false(identical(res$status, 0L))
  expect_match(res$out, "config_package_lookup")
})

test_that("a credential-shaped assignment (using `=`, unrelated to the vendor name) in a plain .R file is caught", {
  res <- run_scan_in_scratch(list(
    "R/x.R" = "api_key = \"eMZMjUvifeZQTsjMeFgwzSNxMeQEppHE\""
  ))
  expect_false(identical(res$status, 0L))
  expect_match(res$out, "credential_assignment")
})

test_that("a forbidden environment-file name committed to the scratch repo is caught", {
  fname <- paste0(toupper(vendor), ".", env_suffix)
  res <- run_scan_in_scratch(stats::setNames(list(paste0(toupper(vendor), "_TOKEN=x")), fname))
  expect_false(identical(res$status, 0L))
  expect_match(res$out, "secret-bearing file")
})
