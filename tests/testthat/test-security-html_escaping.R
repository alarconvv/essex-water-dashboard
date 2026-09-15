# tests/testthat/test-security-html_escaping.R
#
# Gate 4 (security): evidence_row() and condition_card() must always escape
# config/user-sourced text via htmltools' automatic text-node escaping.
# Neither may wrap such content in HTML().

malicious_config <- function() {
  load_manual_content(testthat::test_path("fixture-data", "config_yml_malicious.yml"))
}

test_that("evidence_row escapes a literal <script> tag in value", {
  rendered <- htmltools::doRenderTags(evidence_row("Status", "<script>alert(1)</script>"))
  expect_true(grepl("&lt;script&gt;", rendered, fixed = TRUE))
  expect_false(grepl("<script>alert(1)</script>", rendered, fixed = TRUE))
})

test_that("evidence_row escapes an onerror image payload in value", {
  rendered <- htmltools::doRenderTags(evidence_row("Next hearing", "<img src=x onerror=alert(1)>"))
  expect_true(grepl("&lt;img", rendered, fixed = TRUE))
  expect_false(grepl("<img src=x onerror=alert(1)>", rendered, fixed = TRUE))
})

test_that("evidence_row escapes malicious content in label too", {
  rendered <- htmltools::doRenderTags(evidence_row("<script>alert('label')</script>", "value"))
  expect_true(grepl("&lt;script&gt;", rendered, fixed = TRUE))
  expect_false(grepl("<script>alert('label')</script>", rendered, fixed = TRUE))
})

test_that("evidence_row escapes an inline event-handler payload", {
  rendered <- htmltools::doRenderTags(evidence_row("Note", "<b onmouseover=alert(1)>hover me</b>"))
  expect_true(grepl("&lt;b onmouseover=alert\\(1\\)&gt;", rendered))
  expect_false(grepl("<b onmouseover=alert(1)>", rendered, fixed = TRUE))
})

test_that("evidence_row produces the expected tr/td/b structure for benign input", {
  rendered <- htmltools::doRenderTags(evidence_row("Status", "Status to be confirmed"))
  expect_true(grepl("<tr>", rendered, fixed = TRUE))
  expect_true(grepl("<b>Status</b>", rendered, fixed = TRUE))
  expect_true(grepl("<td>Status to be confirmed</td>", rendered, fixed = TRUE))
  expect_true(grepl("<td>\\s*<b>Status</b>\\s*</td>", rendered))
})

test_that("evidence_row never throws for empty or NA value", {
  expect_no_error(htmltools::doRenderTags(evidence_row("Label", "")))
  expect_no_error(htmltools::doRenderTags(evidence_row("Label", NA_character_)))
})

test_that("every malicious config field renders escaped through evidence_row", {
  manual <- malicious_config()
  rows <- htmltools::tags$table(lapply(names(manual), function(nm) {
    evidence_row(nm, as.character(manual[[nm]]))
  }))
  rendered <- htmltools::doRenderTags(rows)

  expect_false(grepl("<script", rendered, fixed = TRUE))
  expect_false(grepl("<img", rendered, fixed = TRUE))
  expect_false(grepl("<b onmouseover", rendered, fixed = TRUE))
  expect_true(grepl("&lt;script&gt;alert(1)&lt;/script&gt;", rendered, fixed = TRUE))
  expect_true(grepl("&lt;img src=x onerror=alert(1)&gt;", rendered, fixed = TRUE))
  # URLs are rendered as inert text, never as href attributes.
  expect_false(grepl("href=", rendered, fixed = TRUE))
})

test_that("malicious config text renders escaped through condition_card", {
  manual <- malicious_config()
  st <- ecology_status(10, manual$eco_flow_threshold_cfs)
  # A script-laden threshold is not a number -> no comparison is fabricated.
  expect_equal(st$state, "unavailable")

  card <- condition_card(
    id = "ecology",
    title = manual$permit_status,
    value = manual$eco_flow_threshold_cfs,
    unit = manual$next_hearing,
    badge = manual$note,
    badge_class = st$badge_class,
    source = manual$learn_more_url,
    comparison = manual$comment_url
  )
  rendered <- htmltools::doRenderTags(card)

  expect_false(grepl("<script", rendered, fixed = TRUE))
  expect_false(grepl("<img", rendered, fixed = TRUE))
  expect_false(grepl("<b onmouseover", rendered, fixed = TRUE))
  expect_true(grepl("&lt;script&gt;alert(1)&lt;/script&gt;", rendered, fixed = TRUE))
  expect_true(grepl("&lt;img src=x onerror=alert(1)&gt;", rendered, fixed = TRUE))
  expect_true(grepl("&lt;b onmouseover=alert(1)&gt;", rendered, fixed = TRUE))
  # The javascript:/data: URLs appear only as text, never as a link target.
  expect_false(grepl("href=", rendered, fixed = TRUE))
  expect_false(grepl("src=\"data:", rendered, fixed = TRUE))
})
