# app.R
#
# UI for the Essex County Water Dashboard. Visually this is Source A's page:
# page_fluid, dashboard_header(), the VIEW LEVEL pills and dot indicator,
# conditionalPanel depth sections, styles.css then dashboard_new.css, the
# card grids, water budget, and hero chart. The data behind it now comes
# from the SQLite store (R/data_access.R) instead of live API calls.
#
# Loading (reused from the architecture reference app):
#   - global.R is sourced explicitly. Shiny does not source global.R for
#     single-file app.R apps (shinyAppDir_appR() calls loadSupport() with
#     globalrenv = NULL), so without this call `con` and `manual` would not
#     exist.
#   - R/_disable_autoload.r turns off Shiny's automatic R/*.R sourcing, so
#     the two source() calls below are the only place the app layer loads.
#   - server() lives in R/server.R, not a root-level server.R: a root-level
#     server.R makes shiny::runApp(".") ignore app.R entirely.
source("global.R")
source("R/server.R")

#' A chart card in Source A's .water-avail-card pattern (AESTHETIC_GUIDE.md
#' §6.7): title + subtitle, chart, optional legend, takeaway note.
chart_card <- function(heading_id, title, subtitle, chart, legend = NULL, note = NULL) {
  tags$section(
    class = "water-avail-section",
    `aria-labelledby` = heading_id,
    div(
      class = "water-avail-card",
      div(
        class = "chart-header-row",
        div(
          class = "chart-title-block",
          tags$h2(id = heading_id, class = "chart-title", title),
          div(class = "chart-subtitle", subtitle)
        )
      ),
      div(class = "chart-container", chart),
      legend,
      note
    )
  )
}

ui <- page_fluid(

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "dashboard_new.css")
  ),

  dashboard_header(
    updated = textOutput("header_updated", inline = TRUE),
    watershed = selectInput(
      inputId = "watershed",
      label = NULL,
      choices = flow_site_choices(),
      selected = SITE_PARKER,
      width = "176px"
    )
  ),

  div(
    class = "view-level-section",

    div(
      class = "view-level-top-row",

      div(
        class = "view-level-left",

        span(class = "view-level-label", "VIEW LEVEL"),

        radioGroupButtons(
          inputId = "view_level",
          label = NULL,
          choiceNames = list(
            HTML('<span class="view-level-icon icon-summary"></span><span>Summary</span>'),
            HTML('<span class="view-level-icon icon-details"></span><span>Details</span>'),
            HTML('<span class="view-level-icon icon-evidence"></span><span>Evidence</span>')
          ),
          choiceValues = c("1", "2", "3"),
          selected = "1",
          justified = TRUE
        )
      ),

      div(
        class = "view-level-right",
        uiOutput("view_level_dots"),
        div(class = "view-level-helper", "Some data may be delayed depending on the source")
      )
    )
  ),

  div(
    class = "dashboard-shell",

    # ---- Summary (view level 1) ------------------------------------------------

    uiOutput("stress_banner"),

    section_heading(
      eyebrow = "CURRENT CONDITIONS",
      title = "Understanding Today's Conditions",
      subtitle = textOutput("conditions_subtitle", inline = TRUE)
    ),

    uiOutput("condition_cards"),

    uiOutput("drought_status_card"),

    tags$section(
      class = "water-budget-section",

      div(
        class = "water-budget-header-row",

        section_heading(
          eyebrow = "WATER BUDGET",
          title = "Where the watershed's water comes from — and where it goes",
          subtitle = "Illustrative accounting of water entering, leaving, and remaining in the watershed"
        ),

        div(
          class = "wb-period-control",
          div(class = "wb-period-label", "ACCOUNTING PERIOD"),
          div(class = "wb-period-note", "Independent of dashboard period"),
          selectInput(
            inputId = "water_budget_period",
            label = NULL,
            choices = c("2023 Annual"),
            selected = "2023 Annual",
            width = "140px"
          )
        )
      ),

      uiOutput("water_budget_ui")
    ),

    section_heading(
      eyebrow = "LOCAL WATER MANAGEMENT",
      title = "How local water use is managed",
      subtitle = "Permits, safe-yield context, and current municipal water-management information"
    ),

    uiOutput("local_management"),

    tags$section(
      class = "water-avail-section",
      `aria-labelledby` = "water-avail-heading",

      div(
        class = "water-avail-card",

        div(
          class = "chart-header-row",

          div(
            class = "chart-title-block",
            tags$h2(id = "water-avail-heading", class = "chart-title", "Water Availability vs Municipal Use"),
            div(class = "chart-subtitle", uiOutput("water_avail_subtitle"))
          ),

          div(
            class = "view-toggle",
            radioGroupButtons(
              inputId = "chart_mode",
              label = NULL,
              choices = c("Actual values" = "actual", "Compared with typical" = "normalized"),
              selected = "normalized",
              justified = FALSE
            )
          )
        ),

        uiOutput("chart_stats_bar"),

        div(class = "chart-container", plotlyOutput("hero_chart", height = "340px")),

        uiOutput("chart_html_legend"),

        uiOutput("chart_conversion_note")
      )
    ),

    # ---- Details (view level 2) ----------------------------------------------------

    conditionalPanel(
      condition = "input.view_level >= 2",
      div(
        id = "details-section",

        section_heading(
          eyebrow = "DETAILED ANALYSIS",
          title = "Is today's river flow normal?",
          subtitle = "This year's flow against prior years, low-flow history, and county drought status"
        ),

        chart_card(
          heading_id = "seasonal-flow-heading",
          title = "Is this normal?",
          subtitle = textOutput("seasonal_flow_subtitle", inline = TRUE),
          chart = plotlyOutput("seasonal_flow", height = "340px"),
          legend = div(
            class = "chart-html-legend",
            div(class = "chart-legend-item", span(class = "chart-legend-line chart-legend-river"), span("This year")),
            div(
              class = "chart-legend-item",
              span(class = "chart-legend-line chart-legend-typical"),
              span("Historical median")
            )
          ),
          note = uiOutput("seasonal_flow_takeaway")
        ),

        chart_card(
          heading_id = "low-flow-heading",
          title = "Extreme low-flow days per year",
          subtitle = textOutput("low_flow_subtitle", inline = TRUE),
          chart = plotlyOutput("low_flow_days", height = "300px"),
          note = uiOutput("low_flow_takeaway")
        ),

        chart_card(
          heading_id = "drought-history-heading",
          title = "Drought history",
          subtitle = textOutput("drought_history_subtitle", inline = TRUE),
          chart = plotlyOutput("drought_history", height = "300px"),
          note = uiOutput("drought_history_takeaway")
        ),

        tags$section(class = "water-avail-section", uiOutput("evidence_table"))
      )
    ),

    # ---- Evidence (view level 3) ----------------------------------------------------

    conditionalPanel(
      condition = "input.view_level >= 3",
      div(
        id = "evidence-section",

        section_heading(
          eyebrow = "EVIDENCE",
          title = "Show me the evidence",
          subtitle = "Where each live number comes from, when it was last updated, and what is still illustrative"
        ),

        uiOutput("data_sources"),

        chart_card(
          heading_id = "gauge-map-heading",
          title = "Monitoring locations",
          subtitle = "USGS stream gauges, groundwater well, and precipitation gauge used by this dashboard",
          chart = leafletOutput("gauge_map", height = "380px"),
          note = div(
            class = "chart-conversion-note",
            "Map tiles © OpenStreetMap contributors. Watershed boundaries are not drawn yet."
          )
        ),

        tags$section(
          class = "water-avail-section",
          div(
            class = "local-management-grid",
            uiOutput("local_management_evidence"),
            uiOutput("water_101")
          )
        )
      )
    )
  )
)

shinyApp(ui, server)
