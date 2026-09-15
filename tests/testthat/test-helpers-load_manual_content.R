# tests/testthat/test-helpers-load_manual_content.R
#
# load_manual_content() must never throw: missing file, malformed YAML,
# partial YAML, and malicious-but-parseable YAML all come back as a
# well-formed six-field list.

fixture_path <- function(name) {
  testthat::test_path("fixture-data", name)
}

manual_fields <- c(
  "permit_status", "next_hearing", "eco_flow_threshold_cfs",
  "learn_more_url", "comment_url", "note"
)

test_that("load_manual_content returns documented defaults when file is missing", {
  result <- load_manual_content(fixture_path("does_not_exist.yml"))
  expect_type(result, "list")
  expect_true(all(manual_fields %in% names(result)))
  expect_equal(result, .manual_content_defaults)
  expect_equal(result$permit_status, "Not configured")
  expect_true(is.na(result$eco_flow_threshold_cfs))
})

test_that("load_manual_content never throws for a missing file", {
  expect_no_error(load_manual_content("/definitely/not/a/real/path/config.yml"))
})

test_that("load_manual_content parses the normal config (Step 3 content)", {
  result <- load_manual_content(fixture_path("config_yml_normal.yml"))
  expect_type(result, "list")
  expect_true(all(manual_fields %in% names(result)))
  expect_equal(result$permit_status, "Status to be confirmed")
  expect_equal(result$next_hearing, "Date to be confirmed")
  expect_equal(result$eco_flow_threshold_cfs, 8)
  expect_equal(result$learn_more_url, "")
  expect_equal(result$comment_url, "")
  # Empty URLs are placeholders: they must never render as a live link.
  expect_false(is_safe_url(result$learn_more_url))
  expect_false(is_safe_url(result$comment_url))
  expect_match(result$note, "Placeholder pending confirmation")
})

test_that("the normal fixture matches the shipped config.yml", {
  shipped <- testthat::test_path("..", "..", "config.yml")
  skip_if_not(file.exists(shipped), "config.yml not present")
  expect_equal(
    load_manual_content(shipped),
    load_manual_content(fixture_path("config_yml_normal.yml"))
  )
})

test_that("load_manual_content falls back to defaults on malformed YAML", {
  expect_no_error(load_manual_content(fixture_path("config_yml_malformed.yml")))
  result <- load_manual_content(fixture_path("config_yml_malformed.yml"))
  expect_equal(result, .manual_content_defaults)
})

test_that("load_manual_content passes malicious-but-valid YAML through raw", {
  # The loader does no escaping; evidence_row()/condition_card() escape at
  # render time (see test-security-html_escaping.R).
  result <- load_manual_content(fixture_path("config_yml_malicious.yml"))
  expect_type(result, "list")
  expect_equal(result$permit_status, "<script>alert(1)</script>")
  expect_false(is_safe_url(result$learn_more_url))
  expect_false(is_safe_url(result$comment_url))
})

test_that("load_manual_content fills missing fields from defaults on a partial config", {
  result <- load_manual_content(fixture_path("config_yml_partial.yml"))
  expect_true(all(manual_fields %in% names(result)))
  # Specified fields override defaults.
  expect_equal(result$permit_status, "Status to be confirmed")
  expect_equal(result$eco_flow_threshold_cfs, 12)
  # Omitted fields fall back to documented defaults, never NULL.
  expect_equal(result$next_hearing, "Not configured")
  expect_equal(result$note, "Not configured")
  expect_equal(result$learn_more_url, "")
  expect_equal(result$comment_url, "")
})

test_that("load_manual_content returns defaults for NULL/NA/non-character path", {
  expect_equal(load_manual_content(NULL), .manual_content_defaults)
  expect_equal(load_manual_content(NA), .manual_content_defaults)
  expect_equal(load_manual_content(character(0)), .manual_content_defaults)
  expect_equal(load_manual_content(42), .manual_content_defaults)
})

test_that("load_manual_content returns defaults for an empty YAML file", {
  empty <- withr::local_tempfile(fileext = ".yml")
  writeLines(character(0), empty)
  expect_equal(load_manual_content(empty), .manual_content_defaults)
})

test_that("load_manual_content() with no argument honors ESSEXWATER_CONFIG_PATH", {
  withr::local_envvar(
    ESSEXWATER_CONFIG_PATH = normalizePath(fixture_path("config_yml_partial.yml"))
  )
  result <- load_manual_content()
  expect_equal(result$eco_flow_threshold_cfs, 12)
  expect_equal(result$next_hearing, "Not configured")
})

test_that("load_manual_content() with env var pointing at a missing file degrades", {
  withr::local_envvar(ESSEXWATER_CONFIG_PATH = "/no/such/dir/config.yml")
  expect_equal(load_manual_content(), .manual_content_defaults)
})
