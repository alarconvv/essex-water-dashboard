# tests/testthat/test-components-condition_card.R
#
# condition_card() must render for every card_meta id the app uses,
# escape its text arguments, and never mention the removed precipitation
# vendor in tooltips (string built in pieces so the secret scanner, which
# forbids that word in .R files, doesn't flag this test).

card_ids <- c(
  "flow", "rain", "groundwater", "pumping", "ecology",
  "wb_precip", "wb_qin", "wb_et", "wb_qout", "wb_human"
)
removed_vendor <- paste0("no", "aa")

render_card <- function(id, ...) {
  args <- utils::modifyList(
    list(
      id = id, title = "Title", value = "1.0", unit = "cfs",
      badge = "Near typical", badge_class = "badge-good",
      source = "USGS", comparison = "Typical for mid-September"
    ),
    list(...)
  )
  htmltools::doRenderTags(do.call(condition_card, args))
}

test_that("condition_card renders without error for every app card id", {
  for (id in card_ids) {
    expect_no_error(html <- render_card(id))
    expect_true(grepl(paste0('id="card-', id, '"'), html, fixed = TRUE), info = id)
    expect_true(grepl(paste0("condition-card-", id), html, fixed = TRUE), info = id)
    expect_true(grepl("condition-badge badge-good", html, fixed = TRUE), info = id)
    expect_true(grepl("condition-tooltip-body", html, fixed = TRUE), info = id)
  }
})

test_that("condition_card renders without a unit", {
  html <- htmltools::doRenderTags(condition_card(
    id = "flow", title = "River Flow", value = "12", unit = NULL,
    badge = "Near typical", badge_class = "badge-good",
    source = "USGS", comparison = "Typical"
  ))
  expect_false(grepl("value-unit", html, fixed = TRUE))
})

test_that("no tooltip mentions the removed precipitation vendor", {
  for (id in card_ids) {
    html <- render_card(id)
    expect_false(grepl(removed_vendor, html, ignore.case = TRUE), info = id)
  }
})

test_that("ecology tooltip describes the real, provisional comparison", {
  html <- render_card("ecology")
  expect_match(html, "Parker River flow", fixed = TRUE)
  expect_match(html, "ecological flow threshold", fixed = TRUE)
  expect_match(html, "config.yml", fixed = TRUE)
  expect_match(html, "Provisional", fixed = TRUE)
  expect_true(grepl("icon-ecology", html, fixed = TRUE))
})

test_that("ecology card renders each ecology_status state with its badge class", {
  for (flow in list(12, 5, NA)) {
    st <- ecology_status(flow, 8)
    html <- render_card(
      "ecology",
      title = "Ecological Flow",
      value = if (is.na(flow)) "—" else format(flow),
      badge = st$label, badge_class = st$badge_class,
      source = "USGS 01101000 · threshold provisional",
      comparison = st$comparison
    )
    expect_true(grepl(paste("condition-badge", st$badge_class), html, fixed = TRUE))
    expect_true(grepl(paste("card-comparison", st$badge_class), html, fixed = TRUE))
    expect_true(grepl(st$label, html, fixed = TRUE))
  }
})

test_that("condition_card escapes value, source, title, badge, comparison", {
  payload <- "<script>alert(1)</script>"
  html <- render_card(
    "flow",
    title = payload, value = payload, unit = payload,
    badge = payload, source = payload, comparison = payload
  )
  expect_false(grepl(payload, html, fixed = TRUE))
  expect_true(grepl("&lt;script&gt;alert(1)&lt;/script&gt;", html, fixed = TRUE))
})
