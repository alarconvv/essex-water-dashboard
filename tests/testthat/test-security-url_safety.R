# tests/testthat/test-security-url_safety.R
#
# Gate 4 (security): is_safe_url() accepts only http(s) URLs and rejects
# every other scheme (javascript:, data:, etc.) and malformed/NA/NULL input,
# without ever throwing.

test_that("is_safe_url accepts http and https URLs", {
  expect_true(is_safe_url("http://example.com"))
  expect_true(is_safe_url("https://example.com"))
  expect_true(is_safe_url("https://www.mass.gov/orgs/massachusetts-department-of-environmental-protection"))
})

test_that("is_safe_url is case-insensitive on scheme", {
  expect_true(is_safe_url("HTTP://example.com"))
  expect_true(is_safe_url("HTTPS://example.com"))
  expect_true(is_safe_url("HtTpS://example.com"))
})

test_that("is_safe_url rejects javascript: URLs", {
  expect_false(is_safe_url("javascript:alert(1)"))
  expect_false(is_safe_url("JavaScript:alert(1)"))
  expect_false(is_safe_url("javascript:alert(document.cookie)"))
  expect_false(is_safe_url(" javascript:alert(1)"))
})

test_that("is_safe_url rejects data: URLs", {
  expect_false(is_safe_url("data:text/html,<script>alert(1)</script>"))
  expect_false(is_safe_url("data:text/html;base64,PHNjcmlwdD5hbGVydCgxKTwvc2NyaXB0Pg=="))
})

test_that("is_safe_url rejects other non-http(s) schemes", {
  expect_false(is_safe_url("ftp://example.com"))
  expect_false(is_safe_url("file:///etc/passwd"))
  expect_false(is_safe_url("mailto:test@example.com"))
  expect_false(is_safe_url("vbscript:msgbox(1)"))
})

test_that("is_safe_url rejects empty string, NA, and NULL", {
  expect_false(is_safe_url(""))
  expect_false(is_safe_url(NA))
  expect_false(is_safe_url(NA_character_))
  expect_false(is_safe_url(NULL))
})

test_that("is_safe_url rejects malformed/non-URL input without throwing", {
  expect_false(is_safe_url("not a url at all"))
  expect_false(is_safe_url("   "))
  expect_false(is_safe_url("//example.com"))
  expect_false(is_safe_url("http:/example.com"))
  expect_no_error(is_safe_url(123))
  expect_false(is_safe_url(123))
  expect_no_error(is_safe_url(list("http://example.com")))
  expect_no_error(is_safe_url(character(0)))
  expect_false(is_safe_url(character(0)))
})

test_that("is_safe_url rejects the malicious config fixture's URLs", {
  manual <- load_manual_content(testthat::test_path("fixture-data", "config_yml_malicious.yml"))
  expect_match(manual$learn_more_url, "^javascript:")
  expect_match(manual$comment_url, "^data:")
  expect_false(is_safe_url(manual$learn_more_url))
  expect_false(is_safe_url(manual$comment_url))
})

test_that("the shipped config's empty placeholder URLs never become links", {
  manual <- load_manual_content(testthat::test_path("fixture-data", "config_yml_normal.yml"))
  expect_equal(manual$learn_more_url, "")
  expect_equal(manual$comment_url, "")
  expect_false(is_safe_url(manual$learn_more_url))
  expect_false(is_safe_url(manual$comment_url))
})

test_that("defaults (missing config) also produce no links", {
  expect_false(is_safe_url(.manual_content_defaults$learn_more_url))
  expect_false(is_safe_url(.manual_content_defaults$comment_url))
})
