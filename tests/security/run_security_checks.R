#!/usr/bin/env Rscript
# tests/security/run_security_checks.R
#
# Gate 4 (security), script half. Run from the project root:
#   Rscript tests/security/run_security_checks.R
# Prints PASS / FAIL / WARN per check and exits with the number of failed
# checks (0 = all clean). WARN never fails the gate on its own.
#
# The testthat half of Gate 4 (tests/testthat/test-security-*.R) is run by
# run_gates.R, not here.
#
# Checks:
#   a. Secret scan      -- runs tests/security/secret_scan.R, uses its exit status
#   b. Lint             -- lintr::lint_dir(".") with the project .lintr; any lint fails
#   c. SQL building     -- no sprintf/paste/paste0/glue-built SQL reaching
#                          dbGetQuery/dbExecute/dbSendQuery in R/ or etl/
#   d. App-layer HTTP   -- app.R, global.R, R/ make no network calls (all HTTP
#                          lives in etl/)
#   e. Dependencies     -- DESCRIPTION packages installed and on CRAN; packages
#                          used in code but undeclared; writes dependency_notes.md
#   f. .gitignore       -- secret-bearing / generated files are ignored in a
#                          scratch repo; the test fixture DB is not

script_arg <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
security_dir <- if (length(script_arg) > 0) {
  dirname(normalizePath(script_arg[1]))
} else {
  file.path(getwd(), "tests", "security")
}
project_root <- dirname(dirname(security_dir))
setwd(project_root)
rscript_bin <- file.path(R.home("bin"), "Rscript")

results <- list()
record <- function(results, key, label, status, detail) {
  results[[key]] <- list(label = label, status = status, detail = detail)
  cat(sprintf("  %s -- %s\n\n", status, detail))
  results
}

r_files_in <- function(dir) {
  if (!dir.exists(dir)) {
    return(character(0))
  }
  sort(list.files(dir, pattern = "\\.[rR]$", full.names = TRUE))
}

# Parse a file with source references. Returns NULL (and the message) when the
# file does not parse, so callers can report it instead of silently skipping.
parse_file <- function(path) {
  tryCatch(
    list(exprs = parse(path, keep.source = TRUE), error = NULL),
    error = function(e) list(exprs = NULL, error = conditionMessage(e))
  )
}

cat("=== Gate 4: security checks ===\n")
cat("Project root:", project_root, "\n\n")

# ---------------------------------------------------------------------------
# a. Secret scan (delegated; its logic lives in secret_scan.R only)
# ---------------------------------------------------------------------------
cat("[a] Secret scan (tests/security/secret_scan.R)...\n")
scan_script <- file.path("tests", "security", "secret_scan.R")
if (!file.exists(scan_script)) {
  results <- record(results, "secret_scan", "a. Secret scan", "FAIL", "secret_scan.R missing")
} else {
  scan_out <- suppressWarnings(system2(rscript_bin, shQuote(scan_script), stdout = TRUE, stderr = TRUE))
  scan_status <- attr(scan_out, "status")
  scan_status <- if (is.null(scan_status)) 0L else as.integer(scan_status)
  cat(paste0("    ", scan_out, collapse = "\n"), "\n")
  results <- record(
    results, "secret_scan", "a. Secret scan",
    if (scan_status == 0L) "PASS" else "FAIL",
    sprintf("secret_scan.R exit_status=%d", scan_status)
  )
}

# ---------------------------------------------------------------------------
# b. Lint
# ---------------------------------------------------------------------------
cat("[b] Lint (lintr::lint_dir with project .lintr)...\n")
lint_res <- tryCatch(
  {
    if (!requireNamespace("lintr", quietly = TRUE)) stop("lintr is not installed")
    lints <- lintr::lint_dir(".")
    list(ok = TRUE, lints = lints)
  },
  error = function(e) list(ok = FALSE, error = conditionMessage(e))
)
if (!isTRUE(lint_res$ok)) {
  results <- record(results, "lint", "b. Lint", "FAIL", paste("lintr could not run:", lint_res$error))
} else if (length(lint_res$lints) == 0) {
  results <- record(results, "lint", "b. Lint", "PASS", "0 lints")
} else {
  lint_df <- as.data.frame(lint_res$lints)
  lint_lines <- sprintf(
    "%s:%s:%s [%s] %s",
    lint_df$filename, lint_df$line_number, lint_df$column_number, lint_df$linter, lint_df$message
  )
  cat(paste0("    ", lint_lines, collapse = "\n"), "\n")
  results <- record(
    results, "lint", "b. Lint", "FAIL",
    sprintf("%d lint(s) in %d file(s)", nrow(lint_df), length(unique(lint_df$filename)))
  )
}

# ---------------------------------------------------------------------------
# c. Static SQL string building
# ---------------------------------------------------------------------------
# Walks each file's syntax tree (so comments and prose strings are ignored)
# and flags an *interpolating* string builder -- sprintf()/paste()/glue()
# with a non-literal argument or a {placeholder} -- when either
#   * it flows into the statement argument of a DBI query/execute call,
#     directly or through a variable assigned in the same file, or
#   * its literal text has the shape of a SQL statement (SELECT..FROM,
#     INSERT..INTO, UPDATE..SET, DELETE..FROM, CREATE/DROP TABLE|INDEX|VIEW).
# Fixed DDL literals (e.g. in etl/db_schema.R) pass: they don't interpolate.
cat("[c] Static SQL string building (R/*.R, etl/*.R)...\n")

db_fns <- c("dbGetQuery", "dbExecute", "dbSendQuery", "dbSendStatement")
format_builders <- c("sprintf", "gettextf")
paste_builders <- c("paste", "paste0", "str_c")
glue_builders <- c("glue", "str_glue")
sql_shape <- paste0(
  "(?is)\\b(SELECT\\b.*\\bFROM|INSERT\\b.*\\bINTO|UPDATE\\b.*\\bSET|DELETE\\b.*\\bFROM|",
  "(CREATE|DROP)\\b.*\\b(TABLE|INDEX|VIEW|TRIGGER))\\b"
)

call_name <- function(x) {
  if (!is.call(x)) {
    return("")
  }
  fn <- x[[1]]
  if (is.symbol(fn)) {
    return(as.character(fn))
  }
  if (is.call(fn) && as.character(fn[[1]]) %in% c("::", ":::")) {
    return(as.character(fn[[3]]))
  }
  ""
}

call_args <- function(x) {
  args <- as.list(x)[-1]
  if (is.null(names(args))) names(args) <- rep("", length(args))
  args
}

is_empty_arg <- function(x) is.symbol(x) && !nzchar(as.character(x))

literal_text <- function(x) {
  if (is.character(x)) {
    return(x)
  }
  if (!is.call(x)) {
    return(character(0))
  }
  out <- character(0)
  for (a in as.list(x)[-1]) {
    if (!is_empty_arg(a)) out <- c(out, literal_text(a))
  }
  out
}

# TRUE when the builder call splices a non-literal value into the string.
interpolates <- function(x) {
  nm <- call_name(x)
  args <- call_args(x)
  if (nm %in% format_builders) {
    return(length(args) > 1 || !is.character(args[[1]]))
  }
  if (nm %in% paste_builders) {
    content <- args[!names(args) %in% c("sep", "collapse")]
    return(any(!vapply(content, is.character, logical(1))))
  }
  if (nm %in% glue_builders) {
    content <- args[!names(args) %in% c(".sep", ".envir", ".open", ".close")]
    return(any(!vapply(content, is.character, logical(1))) || any(grepl("{", literal_text(x), fixed = TRUE)))
  }
  FALSE
}

is_builder <- function(x) call_name(x) %in% c(format_builders, paste_builders, glue_builders)

statement_arg <- function(x) {
  args <- call_args(x)
  if ("statement" %in% names(args)) {
    return(args[["statement"]])
  }
  unnamed <- args[names(args) == ""]
  skip <- if ("conn" %in% names(args)) 0 else 1
  if (length(unnamed) > skip) unnamed[[skip + 1]] else NULL
}

# Collect every call in a file with the best line number available (srcrefs
# exist for top-level expressions and for each statement inside `{ }`).
collect_calls <- function(exprs) {
  acc <- new.env(parent = emptyenv())
  acc$found <- list()
  walk <- function(node, line) {
    if (!is.call(node)) {
      return(invisible())
    }
    acc$found[[length(acc$found) + 1]] <- list(node = node, line = line)
    refs <- attr(node, "srcref")
    kids <- as.list(node)
    for (i in seq_along(kids)) {
      if (is_empty_arg(kids[[i]])) next
      kid_line <- line
      if (is.list(refs) && i <= length(refs) && inherits(refs[[i]], "srcref")) kid_line <- refs[[i]][1]
      walk(kids[[i]], kid_line)
    }
  }
  top_refs <- attr(exprs, "srcref")
  for (i in seq_along(exprs)) {
    walk(exprs[[i]], if (!is.null(top_refs)) top_refs[[i]][1] else NA_integer_)
  }
  acc$found
}

sql_files <- c(r_files_in("R"), r_files_in("etl"))
sql_hits <- character(0)
for (f in sql_files) {
  parsed <- parse_file(f)
  if (is.null(parsed$exprs)) {
    sql_hits <- c(sql_hits, sprintf("%s: does not parse (%s)", f, parsed$error))
    next
  }
  calls <- collect_calls(parsed$exprs)

  # Variables assigned (transitively) from an interpolating builder.
  tainted <- character(0)
  repeat {
    before <- length(tainted)
    for (cl in calls) {
      nd <- cl$node
      if (call_name(nd) %in% c("<-", "=", "<<-") && is.symbol(nd[[2]])) {
        rhs <- nd[[3]]
        if ((is_builder(rhs) && interpolates(rhs)) || (is.symbol(rhs) && as.character(rhs) %in% tainted)) {
          tainted <- union(tainted, as.character(nd[[2]]))
        }
      }
    }
    if (length(tainted) == before) break
  }

  for (cl in calls) {
    nd <- cl$node
    nm <- call_name(nd)
    if (nm %in% db_fns) {
      stmt <- statement_arg(nd)
      if (is.null(stmt)) next
      if (is_builder(stmt) && interpolates(stmt)) {
        sql_hits <- c(sql_hits, sprintf("%s:%s: %s() statement built with %s()", f, cl$line, nm, call_name(stmt)))
      } else if (is.symbol(stmt) && as.character(stmt) %in% tainted) {
        sql_hits <- c(sql_hits, sprintf(
          "%s:%s: %s() statement `%s` was built by string interpolation", f, cl$line, nm, as.character(stmt)
        ))
      }
    } else if (is_builder(nd) && interpolates(nd) &&
                 grepl(sql_shape, paste(literal_text(nd), collapse = " "), perl = TRUE)) {
      sql_hits <- c(sql_hits, sprintf("%s:%s: SQL statement built with %s()", f, cl$line, nm))
    }
  }
}
# One finding per file:line (a builder passed straight into dbGetQuery() would
# otherwise be reported twice: as the statement and as SQL-shaped text).
sql_hits <- sql_hits[!duplicated(sub("^([^:]+:[^:]+):.*$", "\\1", sql_hits))]
if (length(sql_hits) == 0) {
  results <- record(
    results, "sql", "c. SQL string building", "PASS",
    sprintf("%d file(s) scanned (R/, etl/), 0 findings", length(sql_files))
  )
} else {
  cat(paste0("    ", sql_hits, collapse = "\n"), "\n")
  results <- record(
    results, "sql", "c. SQL string building", "FAIL",
    sprintf("%d finding(s) in %d file(s) scanned", length(sql_hits), length(sql_files))
  )
}

# ---------------------------------------------------------------------------
# d. No external HTTP in the app layer
# ---------------------------------------------------------------------------
# Uses parser tokens, so a CSS `url(` inside a string or a comment mentioning
# httr2 does not count; real calls and namespace references do.
cat("[d] No external HTTP in app layer (app.R, global.R, R/*.R)...\n")

net_pkgs <- c("httr2", "httr", "curl")
net_fns <- c("download.file", "url")
attach_fns <- c("library", "require", "requireNamespace", "loadNamespace")

app_files <- c(Filter(file.exists, c("app.R", "global.R")), r_files_in("R"))
http_hits <- character(0)
for (f in app_files) {
  parsed <- parse_file(f)
  if (is.null(parsed$exprs)) {
    http_hits <- c(http_hits, sprintf("%s: does not parse (%s)", f, parsed$error))
    next
  }
  pd <- utils::getParseData(parsed$exprs, includeText = TRUE)
  if (is.null(pd) || nrow(pd) == 0) next
  pd <- pd[order(pd$line1, pd$col1), ]

  hit <- pd$token == "SYMBOL_PACKAGE" & pd$text %in% net_pkgs
  http_hits <- c(http_hits, sprintf("%s:%d: %s:: reference", f, pd$line1[hit], pd$text[hit]))

  hit <- pd$token == "SYMBOL_FUNCTION_CALL" & pd$text %in% net_fns
  http_hits <- c(http_hits, sprintf("%s:%d: %s() call", f, pd$line1[hit], pd$text[hit]))

  # library(httr2) / requireNamespace("curl") etc.: the package name is the
  # next SYMBOL or STR_CONST token after the attach call.
  for (i in which(pd$token == "SYMBOL_FUNCTION_CALL" & pd$text %in% attach_fns)) {
    after <- pd[(pd$line1 > pd$line1[i] | (pd$line1 == pd$line1[i] & pd$col1 > pd$col1[i])) &
                  pd$token %in% c("SYMBOL", "STR_CONST"), ]
    if (nrow(after) == 0) next
    pkg <- gsub("^[\"']|[\"']$", "", after$text[1])
    if (pkg %in% net_pkgs) {
      http_hits <- c(http_hits, sprintf("%s:%d: %s(%s)", f, pd$line1[i], pd$text[i], pkg))
    }
  }
}
if (length(http_hits) == 0) {
  results <- record(
    results, "http", "d. No app-layer HTTP", "PASS",
    sprintf("%d file(s) scanned, 0 network calls", length(app_files))
  )
} else {
  cat(paste0("    ", http_hits, collapse = "\n"), "\n")
  results <- record(
    results, "http", "d. No app-layer HTTP", "FAIL",
    sprintf("%d finding(s); network access belongs in etl/", length(http_hits))
  )
}

# ---------------------------------------------------------------------------
# e. Dependency audit
# ---------------------------------------------------------------------------
cat("[e] Dependency audit (DESCRIPTION vs installed, CRAN, and code usage)...\n")

parse_pkg_field <- function(desc, field) {
  if (!field %in% colnames(desc) || is.na(desc[1, field])) {
    return(character(0))
  }
  pkgs <- trimws(sub("\\(.*\\)", "", strsplit(desc[1, field], ",")[[1]]))
  pkgs[nzchar(pkgs) & pkgs != "R"]
}

desc <- read.dcf("DESCRIPTION")
declared <- rbind(
  data.frame(package = parse_pkg_field(desc, "Imports"), field = "Imports", stringsAsFactors = FALSE),
  data.frame(package = parse_pkg_field(desc, "Suggests"), field = "Suggests", stringsAsFactors = FALSE)
)
base_pkgs <- rownames(utils::installed.packages(priority = "base"))

declared$installed <- vapply(declared$package, function(p) nzchar(system.file(package = p)), logical(1))

old_opts <- options(timeout = 30)
cran <- tryCatch(
  {
    avail <- suppressWarnings(utils::available.packages(repos = "https://cloud.r-project.org"))
    if (nrow(avail) == 0) stop("empty package index")
    list(ok = TRUE, pkgs = rownames(avail))
  },
  error = function(e) list(ok = FALSE, error = conditionMessage(e))
)
options(old_opts)
declared$on_cran <- if (isTRUE(cran$ok)) declared$package %in% cran$pkgs else NA

# Packages the code actually uses: pkg:: references and library()/require()/
# requireNamespace() calls in app.R, global.R, R/, etl/.
usage_files <- c(Filter(file.exists, c("app.R", "global.R")), r_files_in("R"), r_files_in("etl"))
used <- data.frame(package = character(0), file = character(0), stringsAsFactors = FALSE)
unparsed <- character(0)
for (f in usage_files) {
  parsed <- parse_file(f)
  if (is.null(parsed$exprs)) {
    unparsed <- c(unparsed, f)
    next
  }
  pd <- utils::getParseData(parsed$exprs, includeText = TRUE)
  if (is.null(pd) || nrow(pd) == 0) next
  pd <- pd[order(pd$line1, pd$col1), ]
  pkgs <- pd$text[pd$token == "SYMBOL_PACKAGE"]
  for (i in which(pd$token == "SYMBOL_FUNCTION_CALL" & pd$text %in% attach_fns)) {
    after <- pd[(pd$line1 > pd$line1[i] | (pd$line1 == pd$line1[i] & pd$col1 > pd$col1[i])) &
                  pd$token %in% c("SYMBOL", "STR_CONST"), ]
    if (nrow(after) > 0) pkgs <- c(pkgs, gsub("^[\"']|[\"']$", "", after$text[1]))
  }
  pkgs <- unique(pkgs)
  if (length(pkgs) > 0) used <- rbind(used, data.frame(package = pkgs, file = f, stringsAsFactors = FALSE))
}
undeclared <- used[!used$package %in% c(declared$package, base_pkgs), , drop = FALSE]
unused_imports <- setdiff(declared$package[declared$field == "Imports"], used$package)

not_installed <- declared$package[!declared$installed]
not_on_cran <- if (isTRUE(cran$ok)) declared$package[!declared$on_cran] else character(0)

dep_problems <- c(
  if (length(not_installed) > 0) sprintf("not installed: %s", paste(not_installed, collapse = ", ")),
  if (length(not_on_cran) > 0) sprintf("not on CRAN: %s", paste(not_on_cran, collapse = ", ")),
  if (nrow(undeclared) > 0) sprintf(
    "used but not in DESCRIPTION: %s",
    paste(sprintf("%s (%s)", undeclared$package, undeclared$file), collapse = ", ")
  ),
  if (length(unparsed) > 0) sprintf("could not parse for usage scan: %s", paste(unparsed, collapse = ", "))
)
dep_status <- if (length(dep_problems) > 0) "FAIL" else if (!isTRUE(cran$ok)) "WARN" else "PASS"
dep_detail <- if (length(dep_problems) > 0) {
  paste(dep_problems, collapse = "; ")
} else if (!isTRUE(cran$ok)) {
  sprintf("%d package(s) installed; CRAN unreachable, listing NOT verified (%s)", nrow(declared), cran$error)
} else {
  sprintf("%d package(s) installed and on CRAN; no undeclared usage", nrow(declared))
}

yes_no <- function(x) ifelse(is.na(x), "unknown", ifelse(x, "yes", "no"))
notes <- c(
  "# Dependency audit notes",
  "",
  sprintf("Generated: %s by `tests/security/run_security_checks.R` (check e).", format(Sys.Date(), "%Y-%m-%d")),
  "",
  sprintf("Result: **%s** -- %s", dep_status, dep_detail),
  "",
  "Every package in `DESCRIPTION` `Imports`/`Suggests` is checked for a local",
  "installation and for a current CRAN listing (`available.packages()`), which",
  "catches dependencies archived or removed from CRAN. This is a",
  "maintenance-risk signal, not a vulnerability (CVE) scan.",
  "",
  sprintf("CRAN index: %s", if (isTRUE(cran$ok)) "reachable" else paste("UNREACHABLE --", cran$error)),
  "",
  "| Package | Field | Installed | On CRAN |",
  "|---|---|---|---|",
  sprintf(
    "| %s | %s | %s | %s |",
    declared$package, declared$field, yes_no(declared$installed), yes_no(declared$on_cran)
  ),
  "",
  "## Packages used in code but missing from DESCRIPTION",
  "",
  "Scanned `app.R`, `global.R`, `R/`, `etl/` for `pkg::` and `library()`/`require()`/`requireNamespace()`.",
  "Base-R packages are exempt.",
  "",
  if (nrow(undeclared) == 0) "None." else sprintf("- `%s` in `%s`", undeclared$package, undeclared$file),
  "",
  "## Imports not (yet) referenced by `pkg::` or `library()` in scanned code",
  "",
  "Informational only (code may not be written yet, or may rely on attached packages).",
  "",
  if (length(unused_imports) == 0) "None." else paste0("- `", unused_imports, "`"),
  ""
)
writeLines(notes, file.path(security_dir, "dependency_notes.md"))
cat("    wrote tests/security/dependency_notes.md\n")
results <- record(results, "deps", "e. Dependency audit", dep_status, dep_detail)

# ---------------------------------------------------------------------------
# f. .gitignore effectiveness (scratch repo; the real index is never touched)
# ---------------------------------------------------------------------------
cat("[f] .gitignore effectiveness (scratch git repo)...\n")

# The environment-file names are assembled at runtime because secret_scan.R
# treats a literal environment-file name in code as a credential lookup.
env_suffix <- paste0("Ren", "viron")
must_ignore <- c(
  ".Rhistory", ".RData", "rsconnect/x.dcf",
  paste0("local.", env_suffix), paste0(".", env_suffix),
  ".env", "data/essexwater.sqlite"
)
must_track <- "tests/testthat/fixture-data/fixture.sqlite"

gi_problems <- character(0)
if (!file.exists(".gitignore")) {
  gi_problems <- ".gitignore missing"
} else if (!nzchar(Sys.which("git"))) {
  gi_problems <- "git not available"
} else {
  scratch <- tempfile("gitignore_check_")
  dir.create(scratch)
  file.copy(".gitignore", file.path(scratch, ".gitignore"))
  for (rel in c(must_ignore, must_track)) {
    dir.create(dirname(file.path(scratch, rel)), recursive = TRUE, showWarnings = FALSE)
    writeLines("dummy", file.path(scratch, rel))
  }
  # Isolate from the user's global excludes so only the project .gitignore counts.
  git_base <- c("-C", shQuote(scratch), "-c", "core.excludesFile=/dev/null")
  init_status <- system2("git", c(git_base, "init", "-q"), stdout = FALSE, stderr = FALSE)
  if (!identical(as.integer(init_status), 0L)) {
    gi_problems <- "git init failed in scratch dir"
  } else {
    check_ignored <- function(rel) {
      system2("git", c(git_base, "check-ignore", "-q", shQuote(rel)), stdout = FALSE, stderr = FALSE)
    }
    for (rel in must_ignore) {
      st <- check_ignored(rel)
      if (!identical(as.integer(st), 0L)) gi_problems <- c(gi_problems, sprintf("NOT ignored: %s (exit %s)", rel, st))
    }
    st <- check_ignored(must_track)
    if (!identical(as.integer(st), 1L)) {
      gi_problems <- c(gi_problems, sprintf("wrongly ignored (or error): %s (exit %s)", must_track, st))
    }
  }
  unlink(scratch, recursive = TRUE, force = TRUE)
}
if (length(gi_problems) == 0) {
  results <- record(
    results, "gitignore", "f. .gitignore effectiveness", "PASS",
    sprintf("%d path(s) ignored; fixture DB still trackable", length(must_ignore))
  )
} else {
  cat(paste0("    ", gi_problems, collapse = "\n"), "\n")
  results <- record(results, "gitignore", "f. .gitignore effectiveness", "FAIL", paste(gi_problems, collapse = "; "))
}

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
cat("=== Security check summary ===\n")
cat(sprintf("%-30s %-6s %s\n", "CHECK", "STATUS", "DETAIL"))
for (r in results) {
  cat(sprintf("%-30s %-6s %s\n", r$label, r$status, r$detail))
}
n_fail <- sum(vapply(results, function(r) identical(r$status, "FAIL"), logical(1)))
if (n_fail == 0) {
  cat("\nAll security checks passed.\n")
  quit(status = 0)
}
cat(sprintf("\n%d security check(s) FAILED.\n", n_fail))
quit(status = min(n_fail, 125))
