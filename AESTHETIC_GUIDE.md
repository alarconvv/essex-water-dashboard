# Aesthetic Style Guide: Essex County Water Dashboard

This guide describes the design as it is built in `www/dashboard_new.css`,
`www/styles.css`, and `R/components.R`. Use it whenever you build or
restyle any part of the dashboard. The design comes from the Figma
prototype (owl-cut-23629555.figma.site). Every color, size, and class
name below was checked against the stylesheets.

**Source of truth.** `app.R` loads `www/styles.css` first and
`www/dashboard_new.css` second. About 20 class names appear in both
files. `dashboard_new.css` is the authoritative design and holds the
design tokens. When the two files disagree, the normal CSS cascade
decides, and in a few places a more specific or `!important` rule in
`styles.css` still wins. Section 11 lists those places.

## 1. Overall tone

The style is clinical, scientific, and data-journalism: closer to a
NYT/FiveThirtyEight interactive explainer than a typical dashboard.
It is calm, low-saturation, and trustworthy, with generous whitespace.
It is data-dense but never cluttered. It should read as "credible
evidence for a public hearing," not "flashy consumer app."

## 2. Color system

Color carries **meaning**, not decoration. Keep saturation low except
for the semantic accents.

### 2.1 Design tokens

These are defined in `:root` at the top of `dashboard_new.css`. Always
reference them as `var(--token)` in CSS and use their hex values in R
(Plotly, leaflet).

| Token | Value | Used for |
|---|---|---|
| `--color-ink` | `#172533` | Header background, primary text, active view-level button |
| `--color-water` | `#336891` | Supply / normal accent: river flow line, "Water Inputs" label, links |
| `--color-water-light` | `#C6D5E0` | Water budget hero background, percentile ribbon fill |
| `--color-ecology` | `#C1DB70` | Positive / ecological accent: ecological reference line |
| `--color-ecology-light` | `#E1EEB9` | Light ecological tint (backgrounds only) |
| `--color-municipal` | `#F45932` | Demand / municipal accent: municipal use line, "Water Outputs" label |
| `--color-warning` | `#E75B52` | Severe / below-normal: `.badge-warning` text, `.stat-below` |
| `--color-page` | `#F0F0F1` | Page background (`body`) |
| `--color-card` | `#FFFFFF` | Card background |
| `--color-border` | `#F3F4F6` | 1px card borders and dividers |
| `--radius-card` | `12px` | Corner radius of every card |
| `--shadow-card` | `0 1px 3px rgba(0, 0, 0, 0.08)` | The only card shadow |
| `--content-max-width` | `1280px` | Width of the header row, view-level bar, and `.dashboard-shell` |

### 2.2 Semantic convention

Apply these meanings consistently in cards, badges, charts, and maps:

| Meaning | Token | Examples |
|---|---|---|
| Supply side, availability, normal conditions | `--color-water` | River flow, precipitation, groundwater, water inputs |
| Demand side, human use, caution | `--color-municipal` | Municipal pumping, water outputs, "above typical" demand |
| Positive, ecological | `--color-ecology` | Ecological flow threshold, ecology card, gains |
| Severe, negative, unusually low | `--color-warning` | Severe status, below-threshold comparisons |
| Illustrative, neutral, or manually maintained | Gray neutrals (2.3) | Typical/historical reference lines, placeholder content |

Gray is never used for real live data. It keeps "this is measured"
visually separate from "this is a placeholder or a reference."

### 2.3 Neutrals in use (not yet tokens)

`dashboard_new.css` uses a small set of neutral grays that are not
tokens. Reuse these exact values. Never invent new grays.

| Value | Used for |
|---|---|
| `#687987` | Secondary text: eyebrows, subtitles, labels, helper text, axis-adjacent captions |
| `#8B9AA5` / `#8A98A8` | Tertiary text: sources, period notes, stat labels |
| `#D8DEE3` | Control borders (view-level buttons, selects), category rules |
| `#EEF3F5` | Neutral fill: explainer callout, water-budget badges |
| `#94A3B8` | Dashed "typical" reference line and legend swatch |
| `#F7F8F8` | Stats bar fill inside the chart card |

### 2.4 Contrast rules

The lighter accents do not have enough contrast to carry small text on
white:

- Safe for body text: `--color-ink` (15.6:1), `--color-water` (6.0:1).
- `#687987` is 4.49:1 on white and 3.95:1 on `--color-page`. Use it
  only on white cards, never for text on the page background.
- `--color-municipal` (3.3:1) and `--color-warning` (3.5:1) work for
  lines, bold numbers, and short bold labels. Do not use them for body
  copy.
- `--color-water-light`, `--color-ecology`, and `--color-ecology-light`
  (all under 1.6:1) are for fills, lines, and markers only, never text.
  Ecological annotations in charts use the darker `#7A9430`.

## 3. Typography

- No `font-family` is declared, so text inherits the default sans-serif
  system stack from bslib/Bootstrap 5. Do not add display or serif
  fonts. The only other face is the monospace stack
  (`ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace`)
  used for the water-budget equation.
- **Big numbers are the focal point.** Key metrics render larger than
  any heading. Condition card values (`.condition-value`) are 28px,
  weight 650, in `--color-ink` (26px inside the water budget section).
  The stress score is 48px bold. Big numbers use
  `font-variant-numeric: tabular-nums` where they update.
- Units sit next to the number in smaller, lighter gray
  (`.value-unit`, `.wb-hero-unit`, `.stress-score-total`).
- **Section labels are uppercase, small, and letter-spaced.** Eyebrows
  and control labels are 10px, weight 700, letter-spacing 0.06–0.08em,
  in `#687987`. Examples: "CURRENT CONDITIONS", "VIEW LEVEL",
  "ACCOUNTING PERIOD". Write these labels in uppercase in the R source.
  `.section-eyebrow` does not apply `text-transform`.
- Type scale in use:

| Size | Weight | Element |
|---|---|---|
| 48px | 700 | Stress score (`.stress-score-number`) |
| 28px | 650–700 | Condition values, water budget result value |
| 24px | 650 | Section titles (`.section-title`) |
| 18px | 650–700 | Chart card titles, local management status |
| 15px | 600 | Header brand title |
| 13–14px | 400–600 | Section subtitles, header controls, stress description |
| 11–12px | 400–600 | Body and explanatory text, card titles, sources, comparisons, helper text |
| 10px | 700 | Eyebrows, control labels, badges, legend and axis captions |
| 8–9px | 700 | Stats bar labels, provenance glyphs (use sparingly) |

- Explanatory and caveat text stays small (11–13px) and gray. This
  de-emphasizes disclaimers without hiding them.

## 4. Layout

- **Page shell.** `app.R` uses `page_fluid()`. Content lives in
  `.dashboard-shell`: max width `--content-max-width` (1280px), centered,
  32px padding, and a vertical flex column with a 32px gap between
  sections.
- **Sections.** Every section starts with `section_heading()`
  (eyebrow, title, optional subtitle) and 16px below it.
- **Card-based modular sections.** Every distinct idea gets its own
  card: `--color-card` background, 1px `--color-border` border,
  `--radius-card` (12px) corners, and `--shadow-card`. Padding is 18px
  for condition cards and 24px for larger cards (`.water-avail-card`,
  `.local-management-card`, `.wb-hero`). Content never touches the card
  edge.
- **Condition card grids.** `.conditions-grid` is a CSS grid with a
  16px gap: 4 equal columns by default, 2 columns under 900px, and
  1 column under 560px. Inside `.water-budget-section` it becomes
  5 columns (fixed 190px columns with horizontal scroll under 1100px).
- **Category headers** above a card row (`.conditions-category-grid`)
  show an uppercase `.category-label`, a 1px `.category-rule` line, and
  a gray `.category-question`. Inside the water budget section,
  "Water Inputs" uses `--color-water` and "Water Outputs" uses
  `--color-municipal`. Water budget cards also get a 1px top border in
  the same color (first two cards water, the rest municipal).
- **Two-column content** (`.local-management-grid`) uses two equal
  columns that stack to one below 700px.
- **Alignment.** Supplementary controls (period selectors, view
  toggles) are right-aligned in a header row or card corner. Primary
  content and labels are left-aligned.
- **Responsive breakpoints in use:** 1100px, 900px, 800px, 700px,
  560px. Reuse these; do not add new ones.

## 5. Navigation and wayfinding

The dashboard is **one continuous, scrollable page**. There are no tabs,
no `page_navbar`, and no `nav_panel`s. The view-level control does not
switch between separate panels. It sets how deep the same page goes.

### 5.1 Top header (`dashboard_header()`)

- `.dashboard-header` is sticky (`position: sticky; top: 0;
  z-index: 1000`), full width, with a `--color-ink` background and white
  text. The inner row is at least 80px tall and constrained to
  `--content-max-width`.
- **Left (brand):** a 36px rounded-square mark (10px radius,
  `rgba(255,255,255,0.12)` fill) holding the Font Awesome `water`
  glyph. Next to it, "Essex County Water" (15px, weight 600) sits above
  the subtitle "Water availability & use" (11px, 72% opacity).
- **Right (controls):** each control has a 10px uppercase label at 70%
  opacity above a 40px-tall box with a translucent white fill
  (`rgba(255,255,255,0.08)`) and a translucent white border. The
  controls are:
  - WATERSHED: a display box with a `chevron-down` glyph.
  - TIME PERIOD: `selectInput("time_period")`, restyled via
    `.time-period-control`. It is 176px wide with a 10px radius and
    opens a white dropdown with a 12px radius.
  - A date-range and "Updated …" status block.
  - A 44px square share button (`share-nodes` glyph), hidden under
    900px.
- Under 900px the header stacks: the brand sits on top and the controls
  wrap below it.

### 5.2 View level: depth markers, not tabs

A white bar (`.view-level-section`, 1px `--color-border` bottom border)
sits directly under the header.

- **Left:** the label "VIEW LEVEL" and the pill buttons. They are a
  `shinyWidgets::radioGroupButtons()` input with
  `inputId = "view_level"`, `choiceValues = c("1", "2", "3")`,
  `selected = "1"`. Each choice name is HTML: an icon span plus a text
  span.

  ```r
  HTML('<span class="view-level-icon icon-summary"></span><span>Summary</span>')
  ```

  | Level | Label | Icon class | SVG |
  |---|---|---|---|
  | 1 | Summary | `.icon-summary` | `icons/Summary.svg` |
  | 2 | Details | `.icon-details` | `icons/Details.svg` |
  | 3 | Evidence | `.icon-evidence` | `icons/Evidence.svg` |

  The icons are 16px CSS masks (`mask-image`) filled with
  `currentColor`, so each icon always matches its button's text color.
  An inactive button is white with a `#D8DEE3` border and
  `--color-ink` text. The active button is filled with `--color-ink`
  and has white text and a white icon.

- **Right:** the dot progress indicator and a helper line.
  `output$view_level_dots` renders three 7px circles (`.view-dot`).
  Every dot up to and including the selected level gets `.active`, so
  the dots fill cumulatively (level 2 = two filled dots). Next to them,
  `.view-level-helper` (11px, `#687987`) holds a short hint. Its current
  text is "Some data may be delayed depending on the source".

- **How depth works.** The level-1 (Summary) content is always rendered.
  Deeper content is wrapped in `conditionalPanel`s further down the
  same page:

  ```r
  conditionalPanel(condition = "input.view_level >= 2", div(id = "details-section", ...))
  conditionalPanel(condition = "input.view_level >= 3", div(id = "evidence-section", ...))
  ```

  Levels are cumulative. Details adds sections below Summary, and
  Evidence adds more below Details. Nothing is hidden when you go
  deeper. Any new content belongs to one of the three levels: put it in
  the Summary flow or inside the matching `conditionalPanel`. Never
  create a separate page or tab for it.

### 5.3 Secondary in-card toggles

In-card view switches, such as "Actual values / Compared with typical"
in the chart card, use a **segmented control**, not underlined tabs.
The markup is `.view-toggle` wrapping a `radioGroupButtons()`. The track
is `#F3F4F6` with a 10px radius and 3px padding. Buttons are
transparent, 11px, `#687987`. The active button is white with
`--color-ink` text and a hairline shadow.

## 6. Components

### 6.1 `section_heading(eyebrow, title, subtitle)`

It renders `.section-eyebrow` (10px, 700, `#687987`), an `h2`
`.section-title` (24px, 650, `--color-ink`), and `.section-subtitle`
(13px, `#687987`). Use it for every section. Do not hand-write
headings.

### 6.2 `condition_card()` anatomy

`condition_card(id, title, value, unit, badge, badge_class, source,
comparison, icon = NULL)` returns a `<button>` with classes
`.condition-card .condition-card-interactive .condition-card-<id>`.
Clicking it marks the card `.selected` and sends
`input$selected_condition = id`. From top to bottom it contains:

1. **Top row** (`.condition-card-top`):
   - Left: the **icon chip** `.condition-icon`, a 36px rounded square
     (8px radius) with a 20px glyph. Its color class comes from the
     card id: `.icon-flow`, `.icon-rain`, `.icon-groundwater`,
     `.icon-pumping`, `.icon-ecology`. By default the glyph is a Font
     Awesome icon chosen by id. Pass `icon =` to override it, for
     example with an SVG `tags$img(class = "condition-icon-img")`.
   - Right: the **status badge** `.condition-badge.<badge_class>` and a
     `chevron-right` glyph (`.condition-chevron`).
2. **Title** (`.condition-card-title`): 11px, uppercase, gray.
3. **Measurement** (`.condition-value` > `.value-number` +
   `.value-unit`): 28px value with a smaller gray unit.
4. **Source** (`.condition-source-area` > `.card-source`): 12px gray,
   clamped to two lines, in a fixed 40px-tall area so values line up
   across cards.
5. **Comparison** (`.card-comparison.<badge_class>`): 12px text pinned
   to the card bottom, above a 1px divider. It takes the same
   `badge_class` as the badge, so its emphasis matches the status.
6. **Hover overlay** (`.condition-tooltip`): fills the card on hover
   with a `circle-info` heading, a plain-language explanation of the
   metric and its source, and a "CLICK TO EXPLORE" footer. The tooltip
   text lives in the `card_meta` switch inside `condition_card()`. A
   new card id needs a new `card_meta` entry.

Interaction states: on hover the border darkens to `#D1D5DB` and the
shadow grows. Pressed scales the card to 0.99. On keyboard focus a 2px
focus ring appears.

### 6.3 Status badges

Badges are small pills: 10px text, weight 500, `2px 8px` padding, fully
rounded, with a 1px border. Pick the class by meaning:

| `badge_class` | Meaning | Look |
|---|---|---|
| `badge-good` | Normal / within typical range | Light green fill, green text |
| `badge-warning` | Below normal, caution, or unavailable comparison | `#FFF3E8` fill, `--color-warning` text |
| `badge-critical` | Unusually low / severe | Light red fill, dark red text |

Inside `.water-budget-section`, badges switch to a quieter neutral style
(`#EEF3F5` fill, `#687987` text, 8px uppercase). They describe
accounting periods ("Annual total"), not status.

### 6.4 Illustrative and provenance labels

This is a load-bearing pattern. It is how the design stays honest about
where data comes from without a warning banner on every screen.

- Any metric that does not come from a live, verified feed says
  **"Illustrative"** in text in two places: in its badge and in its
  source line. The pumping card does this with badge "Illustrative" and
  source "Municipal data · Illustrative".
- Section-level illustrative content gets a `.wb-provenance-badge` pill
  reading "◇ Illustrative" (amber fill `#FEF3C7`, text `#92400E`) and a
  one-line `.wb-hero-disclosure`, for example "Illustrative example —
  values demonstrate the method and are not current watershed
  measurements."
- The chart stats bar marks each value with a small `.stat-provenance`
  glyph (● or ◆). The glyphs have no legend, so they never stand in for
  a text label.
- Store-sourced (live) numbers show a "Live" or "Current" tag and the
  last-updated time directly beside the source name, never implied.
  No dedicated class exists yet. Use the source line of the card or
  stats block (`.card-source`, `.stat-block-comparison`) instead of
  inventing a new pill.

### 6.5 Stress banner (`stress_status()` + `.current-conditions-*`)

`stress_status(score)` maps a 0–100 score to a label:

| Score | Label | Returned class |
|---|---|---|
| < 35 | Low Watershed Stress | `stress-low` |
| 35–54 | Moderate Watershed Stress | `stress-moderate` |
| 55–71 | High Watershed Stress | `stress-high` |
| ≥ 72 | Critical Watershed Stress | `stress-critical` |

The banner markup (`.current-conditions-section`) contains:

- A header line: "CURRENT CONDITIONS" label and an "Updated" time.
- A card with an amber tint (`#FFFBEB` fill, `#FDE68A` border, 12px
  radius, 20–24px vertical and 32–40px horizontal padding).
- The top row: the uppercase `.stress-status-badge` pill and a one-line
  `.stress-description` on the left, and the 48px score with a faded
  "/ 100" on the right.
- An 8px `.stress-gradient-track` with a 20px `.stress-indicator` dot
  positioned at `left: <score>%`, and four axis labels ("Typical",
  "Moderate Stress", "High Stress", "Critical Stress").
- A faded `.stress-source-line` naming the sources.

The score is a disclosed placeholder, so the banner shows "Preliminary —
not yet derived from live data" next to it.

### 6.6 Water budget hero and result

- `.wb-hero`: a `--color-water-light` card containing a year badge
  (`.wb-year-badge`) and a provenance badge, the title "Change in
  watershed storage (ΔS)", the monospace equation, the value on the
  right, and the balance bar.
- `.wb-balance-track`: an 8px bar from red (loss) to `--color-water`
  (gain), with a center tick and a white marker with a water-colored
  ring. It is labeled "Storage loss (ΔS < 0) / Balance / Storage gain
  (ΔS > 0)".
- `.wb-result-summary`: a white strip with the result value, its status,
  and a "+ gain / − loss" key.
- `.wb-about-estimate`: a very faint ink-tinted box (8px radius) with a
  9px uppercase title, 10.5px text, and a `--color-water` link.

### 6.7 Chart card (`.water-avail-card`)

This is the pattern for any chart. Every chart card has:

1. A header row: `.chart-title` (18px, 700) and `.chart-subtitle`
   (12px gray, naming the place and date range) on the left, and an
   optional `.view-toggle` on the right.
2. An optional `.chart-stats-bar`: a gray strip with labeled values.
   Values use `.stat-water` or `.stat-municipal` color, comparisons use
   `.stat-below` or `.stat-above`, and a `.stat-note` sits on the right.
3. `.chart-container` holding the `plotlyOutput`.
4. `.chart-html-legend`: an HTML legend with 22px line swatches
   (`.chart-legend-river`, `.chart-legend-municipal`, and dashed
   `.chart-legend-typical`). The Plotly legend is turned off.
5. A small gray note (`.chart-conversion-note`) for unit conversions and
   caveats.

### 6.8 Callouts and management cards

- **Neutral explainer callout** (`.local-management-explainer`):
  `#EEF3F5` fill, `#D8DEE3` border, 12px radius, 24px padding, a 14px
  title, and 12px gray text.
- **Status card** (`.local-management-card`): an uppercase label, an
  18px status line, gray meta text, and text links
  (`.local-management-link`, 12px, weight 600, `--color-water`,
  underlined on hover).
- **Caution callout:** the amber treatment of the stress banner
  (`#FFFBEB` / `#FDE68A`) is the one existing tinted caution surface.
  Reuse it rather than creating another.

### 6.9 Buttons and links

- Header buttons are 44px squares with an 8px radius and translucent
  white fill and border.
- The view-level pills are described in 5.2, and the segmented toggles
  in 5.3.
- Links are text links in `--color-water`, weight 600, with no
  underline until hover. The design has no solid, filled
  call-to-action button.

### 6.10 Evidence rows

Evidence and data-source rows are flat rows separated by 1px
`--color-border` dividers: a source name on the left, the title and a
status or last-updated line inline, and a link on the right. There are
no heavy grid lines and no zebra striping.

### 6.11 Icons

- **SVG files.** `www/icons/` holds 29 Lucide-style SVGs (24×24 viewBox,
  stroke `currentColor`). File names are **PascalCase and
  case-sensitive**, which matters on Linux/shinyapps.io. Write
  `icons/Summary.svg`, never `icons/summary.svg`. Available files:
  AlertTriangle, ArrowRight, CheckCircle2, ChevronDown, ChevronLeft,
  ChevronRight, CloudDrizzle, Details, Download, Droplet, Evidence,
  ExternalLink, Factory, Fish, House, Info, Landmark, Leaf, LogIn,
  Megaphone, Precipitation, Share2, ShowerHead, Sprout, Summary, Sun,
  TrendingDown, Waves, X.
- **Currently referenced:** `Summary.svg`, `Details.svg`, and
  `Evidence.svg` as CSS masks in `dashboard_new.css`, and
  `Precipitation.svg` as `tags$img(src = "icons/Precipitation.svg",
  class = "condition-icon-img")` in the water budget precipitation card.
- **Two ways to use an SVG:**
  - CSS mask (`mask-image: url("icons/Name.svg")` with
    `background-color: currentColor`). The icon takes the surrounding
    text color. Use this whenever the icon must match a token color.
  - `<img>` with `.condition-icon-img` (20px). This is simpler, but
    `currentColor` does not reach inside an `<img>`, so the icon renders
    in the SVG's default (black) stroke and ignores the chip color.
- **Font Awesome glyphs** (through `shiny::icon()`) are used inside
  condition cards and the header: `water` (flow, brand mark),
  `cloud-rain` (rain, precipitation), `droplet` (groundwater),
  `faucet-drip` (pumping, human use), `leaf` (ecology), `sun` (ET),
  `arrow-right-from-bracket` (outflow), `chevron-right` (card chevron,
  tooltip footer), `chevron-down` (watershed control), `circle-info`
  (tooltip heading), and `share-nodes` (share button). Keep this
  mapping. One metric keeps the same glyph everywhere.

## 7. Data visualization conventions

- **Plotly base style:** white `paper_bgcolor` and `plot_bgcolor`, no
  mode bar (`config(displayModeBar = FALSE)`),
  `hovermode = "x unified"`, `showlegend = FALSE` (use the HTML
  legend), small margins. The x-axis has no grid and a `#E5E7EB` axis
  line. The y-axis grid is `#F0F0F0`. Tick labels are 11px `#6B7280`
  and axis titles are 10px `#9CA3AF`.
- **Historical vs. current is always visually distinct:**

| Element | Style | Color |
|---|---|---|
| Current measurement (river flow) | Solid line, width 2.5 | `#336891` (`--color-water`) |
| Current municipal use | Solid line, width 2.5 | `#F45932` (`--color-municipal`) |
| Historical median / typical flow | Dashed (`dash = "dash"`), width 1.75 | `#687987` |
| Typical municipal use | Dashed, width 1.5 | `rgba(244,89,50,0.55)` |
| p10–p90 percentile range | Shaded ribbon, no outline, drawn first (behind lines) | `rgba(198,213,224,0.40)` (`--color-water-light` at 40%) |
| "100% = seasonal typical" line | Dashed, width 1.5, with right-aligned 10px annotation | `#94A3B8`, annotation `#64748B` |
| Ecological flow threshold | Dotted (`dash = "dot"`), width 1.5, labeled annotation | `#C1DB70` (`--color-ecology`), annotation `#7A9430` |

  In short: dashed = historical, solid = current, shaded ribbon =
  percentile range.

  The ecological flow threshold line is Parker River only:
  `config.yml`'s single `eco_flow_threshold_cfs` has never been confirmed
  for the Ipswich gauge, so the dotted line, its annotation, and the
  ecology card comparison are simply not drawn/shown when Ipswich is
  selected — never redrawn with Parker's number relabeled. See
  DEVELOPMENT_GUIDE.md §12.

- **"Today" markers:** a vertical dashed line in `--color-ink` at low
  opacity, always labeled "Today". The dot on the current series takes
  that series' color. Do not use `--color-municipal` for the marker,
  because orange already means municipal demand.
- **Bar charts** use a single muted color family that matches the
  metric's semantic token, for example `--color-water` shades for
  low-flow days. Never use rainbow or categorical palettes for a
  single-metric-over-time chart.
- **Small multiples** (such as three low-flow threshold charts) share
  the same axis style and have a one-line bold + gray caption that
  states the takeaway in plain language. Don't make the reader infer
  the point from the chart alone.
- **Drought (USDM D0–D4)** is an ordered severity scale. Build it from
  the neutral-to-warning ramp: light neutral for D0, stepping toward
  `--color-municipal` and `--color-warning` for D3–D4. Always label the
  category names in the legend.
- **Maps (leaflet):** a quiet, low-saturation basemap. The watershed
  boundary is a bold `--color-ink` outline. Monitoring points are
  circles colored by type from the tokens: `--color-water` for stream
  gauges and the precipitation gauge, a darker water shade or ink
  outline for the groundwater well. Put a small floating legend card
  (white, 12px radius, `--shadow-card`) in a map corner. Popup text is
  always escaped.

## 8. Microcopy and tone

- Every illustrative or unverified number is labeled as such in text,
  not only implied by a badge. Example: "Illustrative example only —
  not verified measurements."
- Source attribution is always visible directly under or beside a
  number (card source line, stats bar comparison, chart subtitle). It
  is never buried only in a separate "about" section.
- Disclaimers about authority ("Any official water use restrictions
  must come from your municipality...") are present but styled small
  and muted: informative without being alarmist.
- Every chart has a plain-language interpretation, for example "River
  flow was below the minimum ecological threshold for 18 days during
  August." Don't leave the reader to read the axis alone.
- Comparisons pair an arrow, a direction color, and a plain-language
  reference, for example "↓ 3.3 in below the 47.1 in long-term
  average". Never show a bare percentage.
- When data is missing, say so plainly ("No data", "Unavailable",
  "Comparison unavailable") in the same component. Never show a
  fabricated fallback value.

## 9. What to avoid

- No bright or saturated "dashboard-y" colors (no neon greens, no hot
  pinks), and no hex values outside the tokens and neutrals in Section 2.
- No heavy card shadows or 3D effects. `--shadow-card` is the only
  resting shadow: flat, quiet, paper-like cards.
- No dense data-grid tables as the primary presentation. Numbers are
  always paired with a label, a comparison, and a source.
- Don't give illustrative and live data the same visual weight. A
  fabricated number should never look as confident as a sourced one.
- No tabs, navbars, or separate pages. Depth comes from the view level
  (Section 5).
- Never use the light accents (`--color-water-light`, `--color-ecology`,
  `--color-ecology-light`) for text.

## 10. Adding new UI

New charts, cards, maps, and panels must look native to this design.

1. **Reuse components first.** Build with `section_heading()`,
   `condition_card()`, and `stress_status()` and the existing class
   patterns: `.water-avail-card` for charts, `.local-management-card` or
   `.local-management-explainer` for text panels, `.conditions-grid`
   for card rows, and the badge classes in 6.3. If `condition_card()`
   needs a new id, add a `card_meta` entry (icon, `icon_class`, tooltip)
   rather than a new card function.
2. **Colors come from the tokens.** In CSS, use `var(--color-*)`,
   `var(--radius-card)`, `var(--shadow-card)`, and
   `var(--content-max-width)`. In R (Plotly `line`/`fillcolor`/`marker`,
   leaflet `color`/`fillColor`), use the token hex values from 2.1 and
   the recipes in Section 7, and follow the semantic mapping in 2.2.
   Keep the hex values in one place in R (for example a named vector)
   so charts stay in sync with the CSS.
3. **Put new content at the right depth.** Summary content goes in the
   main flow. Details content goes inside the
   `conditionalPanel("input.view_level >= 2")` and Evidence content
   inside `conditionalPanel("input.view_level >= 3")`. Open each block
   with `section_heading()`.
4. **New CSS is a last resort.** If a rule is truly unavoidable, append
   it to the **end of `www/dashboard_new.css`** under a commented
   section header. Use only the existing tokens and the neutrals listed
   in 2.3. Do not edit `styles.css`. Do not reuse a generic class name
   (such as `.badge-warning`) without scoping it to your component.
5. **Never introduce a separate theme:** no `bslib::bs_theme()` or
   `bs_add_rules()`, no additional stylesheets (for example
   `custom.css`), no `value_box()`, `page_navbar()`, or `nav_panel()`,
   and no inline `style=` colors. The only exception is computed
   positions, such as `left: <score>%` on a marker.
6. **Icons:** use an existing SVG from `www/icons/` (exact PascalCase
   name) or the Font Awesome glyph already mapped to that metric
   (6.11). Add a new SVG only if nothing fits, keep the Lucide 24×24
   stroke style, and name it in PascalCase.
7. **Check before merging:** the new element looks right at 1280px,
   900px, and 560px. Every number has a source line. Illustrative
   content is labeled in text. Nothing gray represents live data.

## 11. Known quirks in the current CSS (do not copy)

These exist in the stylesheets as shipped. Leave them in place (RULE 1:
style stays as the prototype), but do not replicate them in new work.

- Several `dashboard_new.css` rules target `.condition-label`,
  `.condition-title`, `.condition-unit`, `.condition-source`, and
  `.condition-comparison`, but `condition_card()` emits
  `.condition-card-title`, `.value-unit`, `.card-source`, and
  `.card-comparison`. Card title, unit, source, and comparison
  therefore render with the `styles.css` rules (Tailwind grays such as
  `#6B7280` and `#9CA3AF`). Only `.condition-value` picks up the
  dashboard_new styling.
- Icon chip colors (`.icon-flow`, `.icon-rain`, `.icon-pumping`,
  `.icon-ecology`) and category label colors outside the water budget
  section come from `styles.css` and are not tokens. For example,
  `.icon-ecology` is red (`#EF4444`) and `.category-demand` is amber
  (`#F59E0B`).
- `.badge-warning` in `dashboard_new.css` is unscoped and uses
  `!important`. It also restyles `.card-comparison.badge-warning`,
  giving the comparison area the peach fill and a full border.
- The legacy teal `#1D4E5B` survives in a few places: active view dots,
  the selected-card border, the focus ring, and tooltip text.
- The stress banner uses its own amber palette and a saturated
  four-color gradient (`#4FA8B8 → #6BCB77 → #FFD166 → #FF6B6B`), none of
  them tokens. `stress_status()` returns `stress-*` classes that have no
  CSS and are not applied to the markup.
- `dashboard_new.css` defines `.wb-hero`, `.wb-hero-value`,
  `.wb-hero-equation`, `.wb-balance-track`, `.wb-balance-marker`,
  `.wb-balance-labels`, and `.time-period-control` twice. The later or
  more specific block wins. For example, the hero value renders at
  32px (from `.water-budget-section .wb-hero-value`), not 48px.
- The view-level buttons end up with a 10px radius and `8px 12px`
  padding. A more specific `!important` rule in `styles.css` beats the
  8px / `8px 14px` declared in `dashboard_new.css`.
- `.water-avail-card` repeats the token values as literal hex instead of
  `var()`.
- Outside the water budget, `.conditions-grid` has 4 columns while the
  `styles.css` category header grid above it has 5 (3 availability +
  1 demand + 1 context). A fifth card (for example ecology under
  "Water Context") will wrap to a new row unless the grid is widened,
  as the water budget section does.
