#!/usr/bin/env Rscript
# run_gates.R
#
# Single entrypoint for the four test gates. Run from the project root:
#   NOT_CRAN=true Rscript run_gates.R
# Exits 0 only if all four gates PASS.
#
# Gates are selected by test-file name (the part after "test-"), so every
# file under tests/testthat/ belongs to exactly one gate:
#   1. Unit        -- everything not claimed by gates 2-4
#   2. Integration -- test-integration-*.R   (shiny::testServer)
#   3. Functional  -- test-shinytest2*.R     (shinytest2, headless Chrome)
#   4. Security    -- test-security-*.R  AND  tests/security/run_security_checks.R
#
# Design rules:
#   * Every gate runs in a fresh Rscript subprocess, so state loaded by one
#     gate (a shinytest2 app, a DB connection, sourced globals) cannot leak
#     into another.
#   * No vacuous passes: a gate whose filter matches zero test files, or
#     whose files run zero expectations, is FAIL ("no tests found").
#   * A test file that errors while loading counts as a failure.

# --- Resolve the project root from this script's location ------------------
script_arg <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
project_root <- if (length(script_arg) > 0) dirname(normalizePath(script_arg[1])) else getwd()
setwd(project_root)

test_dir_path <- file.path("tests", "testthat")
security_script <- file.path("tests", "security", "run_security_checks.R")
rscript_bin <- file.path(R.home("bin"), "Rscript")

if (!dir.exists(test_dir_path)) {
  cat("run_gates.R: tests/testthat not found under", project_root, "\n")
  quit(status = 2)
}

# Gate filters are matched against the test name exactly the way testthat
# does it: basename without the "test-" prefix and the .R extension.
gate_specs <- list(
  unit = list(label = "1. Unit", filter = "^(integration-|shinytest2|security-)", invert = TRUE),
  integration = list(label = "2. Integration", filter = "^integration-", invert = FALSE),
  functional = list(label = "3. Functional", filter = "^shinytest2", invert = FALSE),
  security = list(label = "4. Security", filter = "^security-", invert = FALSE)
)

matching_test_files <- function(filter, invert) {
  files <- list.files(test_dir_path, pattern = "^test.*\\.[rR]$")
  test_names <- sub("^test[-_]?", "", sub("\\.[rR]$", "", files))
  hits <- grepl(filter, test_names, perl = TRUE)
  files[if (invert) !hits else hits]
}

# Child process body. It runs one gate's test files and saves a compact
# per-test result table (or the error that stopped test_dir) to an RDS file.
child_code <- c(
  "args <- commandArgs(trailingOnly = TRUE)",
  "filter <- args[1]; invert <- identical(args[2], 'TRUE'); out_path <- args[3]",
  "suppressPackageStartupMessages(library(testthat))",
  "res <- tryCatch(",
  "  testthat::test_dir('tests/testthat', filter = filter, invert = invert,",
  "    reporter = 'summary', stop_on_failure = FALSE, stop_on_warning = FALSE,",
  "    load_package = 'none'),",
  "  error = function(e) e",
  ")",
  "if (inherits(res, 'error')) {",
  "  saveRDS(list(ok = FALSE, error = conditionMessage(res)), out_path)",
  "  quit(status = 1)",
  "}",
  "df <- as.data.frame(res)",
  "keep <- intersect(c('file', 'test', 'nb', 'failed', 'skipped', 'error', 'passed'), names(df))",
  "saveRDS(list(ok = TRUE, df = df[, keep, drop = FALSE]), out_path)"
)
child_script <- tempfile("gate_child_", fileext = ".R")
writeLines(child_code, child_script)

elapsed_since <- function(start) {
  round(as.numeric(difftime(Sys.time(), start, units = "secs")), 1)
}

run_testthat_gate <- function(spec) {
  cat(sprintf("\n=== Gate %s (testthat) ===\n", spec$label))
  start <- Sys.time()
  files <- matching_test_files(spec$filter, spec$invert)
  if (length(files) == 0) {
    cat("  no test files match this gate\n")
    return(list(ok = FALSE, detail = "no tests found", duration = elapsed_since(start)))
  }
  cat(sprintf("  %d file(s): %s\n", length(files), paste(files, collapse = ", ")))

  out_path <- tempfile("gate_result_", fileext = ".rds")
  on.exit(unlink(out_path), add = TRUE)
  exit_status <- system2(
    rscript_bin,
    c(
      "--no-save", "--no-restore", shQuote(child_script),
      shQuote(spec$filter), as.character(spec$invert), shQuote(out_path)
    )
  )
  duration <- elapsed_since(start)

  if (!file.exists(out_path)) {
    return(list(
      ok = FALSE, duration = duration,
      detail = sprintf("test subprocess died (exit_status=%s) before reporting", exit_status)
    ))
  }
  child <- readRDS(out_path)
  if (!isTRUE(child$ok)) {
    return(list(ok = FALSE, duration = duration, detail = paste("test_dir error:", child$error)))
  }

  df <- child$df
  n_pass <- sum(df$passed, na.rm = TRUE)
  n_fail <- sum(df$failed, na.rm = TRUE)
  n_skip <- sum(df$skipped, na.rm = TRUE)
  errored <- df$error %in% TRUE
  n_err_files <- length(unique(df$file[errored]))
  n_err_tests <- sum(errored)

  if (n_pass + n_fail + n_err_tests == 0) {
    return(list(
      ok = FALSE, duration = duration,
      detail = sprintf("no tests found (0 expectations ran; %d skipped)", n_skip)
    ))
  }
  detail <- sprintf(
    "files=%d passed=%d failed=%d errors=%d (in %d file(s)) skipped=%d",
    length(files), n_pass, n_fail, n_err_tests, n_err_files, n_skip
  )
  ok <- n_fail == 0 && n_err_tests == 0 && identical(as.integer(exit_status), 0L)
  list(ok = ok, detail = detail, duration = duration)
}

run_script_check <- function(label, path) {
  cat(sprintf("\n=== Gate %s (script: %s) ===\n", label, path))
  start <- Sys.time()
  if (!file.exists(path)) {
    return(list(ok = FALSE, detail = sprintf("%s missing", path), duration = elapsed_since(start)))
  }
  exit_status <- system2(rscript_bin, c("--no-save", "--no-restore", shQuote(path)))
  list(
    ok = identical(as.integer(exit_status), 0L),
    detail = sprintf("exit_status=%s", exit_status),
    duration = elapsed_since(start)
  )
}

as_row <- function(label, ok, detail, duration) {
  list(gate = label, status = if (isTRUE(ok)) "PASS" else "FAIL", detail = detail, duration = duration)
}

rows <- list()
for (key in c("unit", "integration", "functional")) {
  spec <- gate_specs[[key]]
  r <- run_testthat_gate(spec)
  rows[[key]] <- as_row(spec$label, r$ok, r$detail, r$duration)
}

sec_tests <- run_testthat_gate(gate_specs$security)
sec_script <- run_script_check(gate_specs$security$label, security_script)
rows$security <- as_row(
  gate_specs$security$label,
  sec_tests$ok && sec_script$ok,
  sprintf("testthat: %s | script: %s", sec_tests$detail, sec_script$detail),
  sec_tests$duration + sec_script$duration
)

unlink(child_script)

cat("\n================================ GATE SUMMARY ================================\n")
cat(sprintf("%-16s %-6s %-9s %s\n", "GATE", "STATUS", "DURATION", "DETAIL"))
for (r in rows) {
  cat(sprintf("%-16s %-6s %-9s %s\n", r$gate, r$status, paste0(r$duration, "s"), r$detail))
}
cat("==============================================================================\n")

if (all(vapply(rows, function(r) identical(r$status, "PASS"), logical(1)))) {
  cat("\nALL GATES PASS.\n")
  quit(status = 0)
}
cat("\nONE OR MORE GATES FAILED.\n")
quit(status = 1)
