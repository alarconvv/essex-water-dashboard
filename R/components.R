# ============================================================
# REUSABLE UI COMPONENTS
# Essex County Water Dashboard
# ============================================================

# -----------------------------
# 1. Stress status function
# -----------------------------

stress_status <- function(score) {

  if (score < 35) {
    list(
      label = "Low Watershed Stress",
      class = "stress-low"
    )

  } else if (score < 55) {
    list(
      label = "Moderate Watershed Stress",
      class = "stress-moderate"
    )

  } else if (score < 72) {
    list(
      label = "High Watershed Stress",
      class = "stress-high"
    )

  } else {
    list(
      label = "Critical Watershed Stress",
      class = "stress-critical"
    )
  }
}



# -----------------------------
# Condition card
# -----------------------------

condition_card <- function(
  id,
  title,
  value,
  unit = NULL,
  badge,
  badge_class,
  source,
  comparison,
  icon = NULL,
  tooltip_body_override = NULL
) {

  # UI metadata only — does not change dashboard data
  card_meta <- switch(
    id,

    "flow" = list(
      icon = "water",
      icon_class = "icon-flow",
      tooltip_title = "River flow",
      tooltip_body = paste0(
        "The amount of water flowing through the ",
        "Parker River at the monitoring station.\n\n",
        "Data source: U.S. Geological Survey (USGS), Parker River ",
        "at Byfield, Massachusetts, station 01101000.\n\n",
        "Update status: The dashboard retrieves the latest available ",
        "USGS measurement. This is near-real-time monitoring data, ",
        "not a guaranteed live reading. The observation time should ",
        "be displayed so users can see how recent the value is.\n\n",
        "Historical comparison: USGS historical discharge records ",
        "are used to compare current flow with typical conditions ",
        "for the same time of year."
      )
    ),

    "rain" = list(
      icon = "cloud-rain",
      icon_class = "icon-rain",
      tooltip_title = "Recent rainfall",
      tooltip_body = paste0(
        "Recent precipitation recorded at the USGS Byfield ",
        "precipitation gauge, co-located with the Parker River ",
        "streamflow gauge.\n\n",
        "Data source: U.S. Geological Survey (USGS), precipitation ",
        "station 424510070564401, via the USGS Water Data API.\n\n",
        "Update status: Values come from the dashboard's scheduled ",
        "refresh of the latest available USGS observations. USGS data ",
        "are provisional and may be revised.\n\n",
        "Historical comparison: Daily precipitation totals from prior ",
        "years at the same gauge are used to compare recent rainfall ",
        "with typical conditions for the same time of year. A single ",
        "gauge is a local indicator and does not represent ",
        "precipitation across the entire watershed."
      )
    ),

    "groundwater" = list(
      icon = "droplet",
      icon_class = "icon-groundwater",
      tooltip_title = "Groundwater level",
      tooltip_body = paste0(
        "Groundwater depth below the land surface ",
        "at a USGS monitoring well.\n\n",
        "Data source: U.S. Geological Survey (USGS), groundwater ",
        "monitoring station 424520070562401.\n\n",
        "Update status: The dashboard retrieves the latest available ",
        "USGS groundwater-level observation. The data may be updated ",
        "periodically and should not be described as continuously live. ",
        "The observation date indicates how recent the measurement is.\n\n",
        "Historical comparison: Historical groundwater levels are ",
        "used to evaluate whether current conditions are relatively ",
        "high or low for the season. A single well does not directly ",
        "measure total watershed groundwater storage."
      )
    ),

    "pumping" = list(
      icon = "faucet-drip",
      icon_class = "icon-pumping",
      tooltip_title = "Municipal water use",
      tooltip_body = paste0(
        "Current status: Illustrative prototype value; not live ",
        "municipal pumping data.\n\n",
        "Intended source: Massachusetts Water Management Act reports ",
        "and local public water supplier records.\n\n",
        "Planned use: Monthly withdrawals by source, converted to ",
        "consistent units and compared with historical demand. ",
        "Data availability and reporting delays will be documented."
      )
    ),

    "ecology" = list(
      icon = "leaf",
      icon_class = "icon-ecology",
      tooltip_title = "Ecological context",
      tooltip_body = paste0(
        "Compares the latest Parker River flow at the USGS Byfield ",
        "gauge (station 01101000) with the ecological flow threshold ",
        "set in the dashboard's config.yml.\n\n",
        "Provisional: the threshold is a placeholder pending ",
        "confirmation from IRWA/PRCWA staff, not a regulatory standard. ",
        "USGS data are provisional and may be revised.\n\n",
        "Sustained flow at or below the threshold may stress fish and ",
        "other aquatic life.\n\n",
        "This comparison is shown only for the Parker River gauge; ",
        "config.yml holds no separate threshold for the Ipswich River."
      )
    ),

    "wb_precip" = list(
      icon = "cloud-rain",
      icon_class = "icon-rain",
      tooltip_title = "Precipitation",
      tooltip_body = paste0(
        "Precipitation entering the watershed ",
        "during the selected period.\n\n",
        "Planned source: USGS precipitation gauges and PRISM gridded ",
        "precipitation data."
      )
    ),

    "wb_qin" = list(
      icon = "water",
      icon_class = "icon-flow",
      tooltip_title = "Water inflow",
      tooltip_body = paste0(
        "Water crossing into the defined watershed ",
        "boundary, including relevant surface and subsurface inflow.\n\n",
        "USGS streamflow records, StreamStats ",
        "watershed delineations, and available hydrogeologic studies. ",
        "Subsurface inflow may require an existing groundwater model ",
        "or additional estimation.\n\n"
      )
    ),

    "wb_et" = list(
      icon = "sun",
      icon_class = "icon-pumping",
      tooltip_title = "Evapotranspiration",
      tooltip_body = paste0(
        "Water returned to the atmosphere through ",
        "evaporation and plant transpiration.\n\n",
        "Planned source: OpenET, USGS water-budget datasets, or other ",
        "suitable modeled products, depending on coverage and ",
        "availability for the Parker River Watershed.\n\n."
      )
    ),

    "wb_qout" = list(
      icon = "arrow-right-from-bracket",
      icon_class = "icon-pumping",
      tooltip_title = "Water outflow",
      tooltip_body = paste0(
        "Water leaving the defined watershed ",
        "boundary through surface discharge and relevant subsurface ",
        "outflow.\n\n",
        "already retrieves USGS river-flow information",
        "monthly water-budget outflow.\n\n",
        "Planned source: USGS streamflow records, including the ",
        "Parker River gauge at Byfield where appropriate, together ",
        "with watershed boundary and hydrogeologic information.\n\n."
      )
    ),

    "wb_human" = list(
      icon = "faucet-drip",
      icon_class = "icon-pumping",
      tooltip_title = "Human water use",
      tooltip_body = paste0(
        "Net human water use associated with the ",
        "watershed, accounting for relevant withdrawals, return ",
        "flows, imports, and exports.\n\n",
        "Planned source: Massachusetts Water Management Act reports, ",
        "public water supplier records, municipal withdrawal data, ",
        "and available wastewater or return-flow information.\n\n",
        "Processing: Convert reported use to the selected monthly ",
        "period and estimate net water removal. Gross pumping will ",
        "not automatically be treated as water permanently lost ",
        "from the watershed."
      )
    )
  )

  # A caller with per-watershed context (server.R, for "flow" and "ecology")
  # may override the static tooltip text above -- e.g. naming the selected
  # gauge instead of always describing the Parker River, or explaining why
  # the ecological comparison is unavailable for a gauge with no configured
  # threshold. NULL (the default) keeps the static text.
  if (!is.null(tooltip_body_override)) {
    card_meta$tooltip_body <- tooltip_body_override
  }

  tags$button(
    type = "button",

    class = paste(
      "condition-card",
      "condition-card-interactive",
      paste0("condition-card-", id)
    ),

    id = paste0("card-", id),

    onclick = paste0(
      "document.querySelectorAll('.condition-card').forEach(",
      "function(card){card.classList.remove('selected');});",
      "this.classList.add('selected');",
      "Shiny.setInputValue('selected_condition','",
      id,
      "',{priority:'event'});"
    ),

    # -------------------------
    # Top row
    # -------------------------

    div(
      class = "condition-card-top",

      div(
        class = paste(
          "condition-icon",
          card_meta$icon_class
        ),

        if (!is.null(icon)) {
          icon
        } else {
          shiny::icon(card_meta$icon)
        }
      ),

      div(
        class = "condition-card-status",

        div(
          class = paste(
            "condition-badge",
            badge_class
          ),
          badge
        ),

        span(
          class = "condition-chevron",
          icon("chevron-right")
        )
      )
    ),

    # -------------------------
    # Title
    # -------------------------

    div(
      class = "condition-card-title",
      title
    ),

    # -------------------------
    # Measurement
    # -------------------------

    div(
      class = "condition-value",

      span(
        class = "value-number",
        value
      ),

      if (!is.null(unit)) {
        span(
          class = "value-unit",
          unit
        )
      }
    ),

    # -------------------------
    # Source
    # -------------------------

    div(
      class = "condition-source-area",

      div(
        class = "card-source",
        source
      )
    ),

    # -------------------------
    # Comparison
    # -------------------------

    div(
      class = paste(
        "card-comparison",
        badge_class
      ),
      comparison
    ),

    # -------------------------
    # Hover overlay
    # -------------------------

    div(
      class = "condition-tooltip",

      div(
        class = "condition-tooltip-heading",

        icon("circle-info"),

        span(
          card_meta$tooltip_title
        )
      ),

      div(
        class = "condition-tooltip-body",
        card_meta$tooltip_body
      ),

      div(
        class = "condition-tooltip-footer",

        span(
          "Click to explore"
        ),

        icon("chevron-right")
      )
    )
  )
}

# ============================================================
# DASHBOARD HEADER
# ============================================================

# `updated`: tag or text for the "Updated ..." line (e.g. a textOutput fed by
# format_last_updated()). NULL shows "Not yet updated"; the header never
# carries a hard-coded time.
# `watershed`: an input control (e.g. a selectInput) for the WATERSHED slot.
# NULL keeps the static "Parker River" display box.
dashboard_header <- function(updated = NULL, watershed = NULL) {

  watershed_control <- if (is.null(watershed)) {
    div(
      class = "dashboard-header-control",

      div(
        class = "dashboard-control-label",
        "WATERSHED"
      ),

      div(
        class = "dashboard-control-value",
        span("Parker River"),
        icon("chevron-down")
      )
    )
  } else {
    div(
      class = "dashboard-header-control time-period-control watershed-control",

      div(
        class = "dashboard-control-label",
        "WATERSHED"
      ),

      watershed
    )
  }

  div(
    class = "dashboard-header",

    div(
      class = "dashboard-header-inner",

      div(
        class = "dashboard-brand",

        div(
          class = "dashboard-brand-icon",
          icon("water")
        ),

        div(
          class = "dashboard-brand-text",

          div(
            class = "dashboard-brand-title",
            "Essex County Water"
          ),

          div(
            class = "dashboard-brand-subtitle",
            "Water availability & use"
          )
        )
      ),

      div(
        class = "dashboard-header-controls",

        watershed_control,

        div(
          class = "dashboard-header-control time-period-control",

          div(
            class = "dashboard-control-label",
            "TIME PERIOD"
          ),

          selectInput(
            inputId = "time_period",
            label = NULL,

            choices = c(
              "Today" = "today",
              "Past 7 days" = "7d",
              "Past 30 days" = "30d",
              "Past 90 days" = "90d",
              "Past 6 months" = "6m",
              "Past 12 months" = "1y"
            ),

            selected = "30d",
            width = "176px"
          )
        ),

        div(
          class = "dashboard-date-status",

          uiOutput("header_date_range"),

          div(
            class = "dashboard-updated",
            if (is.null(updated)) "Not yet updated" else updated
          )
        ),

        tags$button(
          type = "button",
          class = "dashboard-share-button",
          `aria-label` = "Share dashboard",
          icon("share-nodes")
        )
      )
    )
  )
}


# ============================================================
# SECTION HEADING
# ============================================================

section_heading <- function(eyebrow = NULL, title, subtitle = NULL) {

  div(
    class = "section-heading",

    if (!is.null(eyebrow)) {
      div(
        class = "section-eyebrow",
        eyebrow
      )
    },

    h2(
      class = "section-title",
      title
    ),

    if (!is.null(subtitle)) {
      div(
        class = "section-subtitle",
        subtitle
      )
    }
  )
}
