library(shiny)

required_packages <- c(
  "leaflet",
  "htmltools",
  "DiagrammeR"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop(
    "Missing required packages: ",
    paste(missing_packages, collapse = ", "),
    ". No packages are installed automatically.",
    call. = FALSE
  )
}

source(file.path("R", "connection.R"), local = TRUE)
source(file.path("R", "queries.R"), local = TRUE)

source(file.path("R", "server_home.R"), local = TRUE)
source(file.path("R", "server_catalog.R"), local = TRUE)
source(file.path("R", "server_map.R"), local = TRUE)

source(file.path("R", "server.R"), local = TRUE)


# Native disclosure panels do not require an additional UI package.
point_detail_panel <- div(
  class = "point-detail-panel",
  h3("Selected location"),
  textOutput("point_prompt"),
  uiOutput("sr_point_actions"),
  tags$details(
    open = NA,
    tags$summary("Location"),
    tableOutput("point_location")
  ),
  tags$details(
    open = NA,
    tags$summary("Sources / subsources"),
    tableOutput("point_provenance")
  ),
  tags$details(
    open = NA,
    tags$summary("Events and dates"),
    selectInput("point_event_id", "Event", choices = character(0)),
    div(
      class = "detail-buttons",
      actionButton("point_events_prev", "Previous events"),
      actionButton("point_events_next", "Next events")
    ),
    textOutput("point_event_page"),
    tableOutput("point_event_metadata")
  ),
  tags$details(
    tags$summary("Samples and depth"),
    selectInput("point_sample_id", "Sample", choices = character(0)),
    tableOutput("point_sample_metadata")
  ),
  tags$details(
    open = NA,
    tags$summary("Abundance records"),
    div(class = "detail-table", tableOutput("point_records")),
    div(
      class = "detail-buttons",
      actionButton("point_records_prev", "Previous records"),
      actionButton("point_records_next", "Next records")
    ),
    textOutput("point_record_page"),
    selectInput("point_record_id", "Record details", choices = character(0)),
    tableOutput("point_abundance_metadata")
  ),
  tags$details(tags$summary("Taxonomy"), tableOutput("point_taxonomy")),
  tags$details(tags$summary("Sampling protocol"), tableOutput("point_protocol"))
)

# Mount the detail panel directly; do not traverse or rewrite the UI tree.
# Flat-record exploration UI. DT receives only one bounded SQL page.
if (!requireNamespace("DT", quietly = TRUE)) {
  stop(
    "The DT package is required for selected records. Install dependencies before startup.",
    call. = FALSE
  )
}
selected_records_panel <- function() {
  div(
    class = "content-panel",
    id = "selected_records_panel",
    h3("Selected records"),
    div(
      style = "display:flex;gap:8px;align-items:center;flex-wrap:wrap;margin-bottom:12px;",
      textOutput("sr_selection_count", inline = TRUE),
      actionButton("sr_load", "Load selected records", class = "btn-primary"),
      actionButton("sr_clear", "Clear selection")
    ),
    textOutput("sr_status"),
    fluidRow(
      column(
        6,
        selectInput(
          "sr_column_group",
          "Table columns",
          choices = c(
            "Main columns" = "main",
            "Main + taxonomy" = "taxonomy",
            "Main + sampling protocol" = "protocol",
            "All populated columns" = "all"
          )
        )
      ),
      column(
        6,
        selectInput(
          "sr_csv_columns",
          "CSV columns",
          choices = c(
            "All columns (complete schema)" = "all",
            "Currently displayed columns" = "visible"
          )
        )
      )
    ),
    DT::DTOutput("sr_table"),
    div(
      style = "display:flex;gap:8px;align-items:center;flex-wrap:wrap;margin-top:12px;",
      actionButton("sr_prev", "Previous"),
      actionButton("sr_next", "Next"),
      textOutput("sr_page_status", inline = TRUE),
      uiOutput("sr_download_control", inline = TRUE)
    ),
    tags$details(
      style = "margin-top:10px;font-size:12px;color:#526776;",
      tags$summary("Selection and export details"),
      p(
        "Empty columns are hidden using the complete loaded selection, not just the current page. Only SQL NULL and empty strings count as empty; zero, unknown and undetermined are retained."
      ),
      p(
        "CSV downloads include every record in the loaded selection, not only the visible page. The limit is 100,000 records; larger selections are not truncated. CSV uses UTF-8, comma separators and \\N for SQL NULL. Empty strings remain empty strings."
      )
    )
  )
}

ui <- fluidPage(
  title = "PhytoMap Data Viewer",
  tags$head(
    tags$script(src = "catalog_interactions.js?v=3"),
    tags$script(src = "catalog_filters.js?v=1")
  ),
  tags$head(tags$style(HTML(
    "
body { background: #f5f7fa; color: #243746; }
.app-shell { position: relative; }
.app-header { padding: 20px 560px 20px 12px; margin-bottom: 18px;
  min-height: 106px; border-bottom: 1px solid #dce4eb; }
.app-header h1 { margin: 0 0 8px; font-size: 30px; font-weight: 600; color: #123b4a; }
.app-header p, .section-note { color: #526776; line-height: 1.5; }
.app-header p { margin-bottom: 0; }
.page-navigation > .tabbable > .nav { position: absolute; top: 25px; right: 12px;
  display: flex; flex-wrap: wrap; justify-content: flex-end; gap: 6px; margin: 0; }
.page-navigation > .tabbable > .nav > li { float: none; margin: 0; }
.page-navigation > .tabbable > .nav > li > a { border-radius: 6px;
  color: #294f60; padding: 12px 14px; }
.page-navigation > .tabbable > .nav > li.active > a,
.page-navigation > .tabbable > .nav > li.active > a:hover,
.page-navigation > .tabbable > .nav > li.active > a:focus { background: #176b80; color: white; }
.page-navigation > .tabbable > .nav > li > a:focus-visible { outline: 2px solid #123b4a; outline-offset: 2px; }
.content-panel { background: white; border: 1px solid #dce4eb; border-radius: 10px;
  padding: 20px; margin-bottom: 20px; }
.content-panel h2 { margin-top: 0; color: #123b4a; }
.intro-text { font-size: 17px; line-height: 1.6; max-width: 950px; }
.metric-card { background: white; border: 1px solid #dce4eb; border-top: 4px solid #176b80;
  border-radius: 8px; padding: 18px; margin-bottom: 20px; min-height: 125px; }
.metric-value { font-size: 29px; font-weight: 600; color: #123b4a; overflow-wrap: anywhere; }
.metric-label { margin-top: 9px; color: #526776; }
.development-note { padding: 12px 15px; background: #eef5f8; border-left: 4px solid #176b80; }
.map-layout { display: grid; grid-template-columns: 230px minmax(0, 1fr) 340px;
  align-items: start; gap: 16px; }
.map-filter-panel, .point-detail-panel { min-width: 0; background: #f8fafb;
  border: 1px solid #dce4eb; border-radius: 8px; padding: 14px; }
.map-filter-panel h3 { font-size: 17px; margin: 0 0 8px; }
.map-view-panel { min-width: 0; }
.choice-list { height: 205px; overflow-y: auto; padding: 0 8px 5px;
  border: 1px solid #dce4eb; border-radius: 5px; background: white; }
.choice-list .shiny-input-container { width: 100%; margin-bottom: 0; }
.choice-list .checkbox { margin: 7px 0; overflow-wrap: anywhere; }
.choice-list .shiny-options-group > .checkbox:first-child { position: sticky; top: 0;
  z-index: 1; background: white; margin-top: 0; padding-top: 8px; padding-bottom: 8px;
  border-bottom: 1px solid #dce4eb; font-weight: 600; }
.filter-count { font-size: 12px; color: #526776; margin: 5px 0 12px; }
.filter-actions { display: flex; gap: 8px; flex-wrap: wrap; margin-top: 10px; }
.map-frame { border: 1px solid #dce4eb; border-radius: 8px; overflow: hidden; }
.map-status { background: #eef5f8; border-left: 4px solid #176b80;
  padding: 10px 13px; margin-top: 10px; overflow-wrap: anywhere; }
.pending-note { color: #7b5200; margin-top: 6px; }
.map-details { color: #526776; font-size: 12px; margin-top: 8px; }
.map-details summary { cursor: pointer; }
.point-detail-panel { max-height: 70vh; overflow-y: auto; background: #fafcfd; }
.point-detail-panel h3 { margin-top: 0; font-size: 18px; color: #123b4a; }
.point-detail-panel details { border-top: 1px solid #dce4eb; padding: 10px 0; }
.point-detail-panel summary { cursor: pointer; font-weight: 600; margin-bottom: 8px; }
.point-detail-panel table { width: 100%; font-size: 12px; table-layout: fixed; }
.point-detail-panel td, .point-detail-panel th { overflow-wrap: anywhere; }
.point-detail-panel .shiny-input-container { width: 100%; }
.detail-table { overflow-x: auto; }
.detail-buttons { display: flex; gap: 5px; flex-wrap: wrap; margin: 7px 0; }
.detail-buttons button { font-size: 11px; }
.app-footer { padding: 8px 12px 18px; color: #526776; font-size: 12px; }
@media (max-width: 1199px) {
  .app-header { padding-right: 12px; min-height: 0; margin-bottom: 10px; }
  .page-navigation > .tabbable > .nav { position: static; justify-content: flex-start;
    margin: 0 0 16px; }
  .map-layout { grid-template-columns: 230px minmax(0, 1fr); }
  .point-detail-panel { grid-column: 2; max-height: 65vh; }
}
@media (max-width: 849px) {
  .app-header h1 { font-size: 26px; }
  .content-panel { padding: 14px; }
  .map-layout { grid-template-columns: minmax(0, 1fr); }
  .point-detail-panel { grid-column: 1; }
  .page-navigation > .tabbable > .nav > li > a { padding: 10px; }
}
"
  ))),
  div(
    class = "app-shell",
    div(
      class = "app-header",
      h1("PhytoMap Data Viewer"),
      p("Explore phytoplankton abundance, sampling coverage and metadata.")
    ),
    div(
      class = "page-navigation",
      tabsetPanel(
        id = "main_navigation",
        selected = "home",
        type = "pills",
        tabPanel(
          "Home",
          value = "home",
          div(
            class = "content-panel",
            h2("Explore the PhytoMap database"),
            p(
              class = "intro-text",
              paste(
                "PhytoMap brings together phytoplankton abundance records with information about",
                "sampling events, geographic locations, taxonomic identifications and analytical methods."
              )
            ),
            actionButton("open_map", "Open map explorer", class = "btn-primary")
          ),
          h3("Database at a glance"),
          uiOutput("overview_cards"),
          textOutput("overview_status"),
          br(),
          div(
            class = "content-panel",
            h3("Understanding the counts"),
            tags$ul(
              tags$li(
                "An abundance record is not an independent sample. A sample can contain multiple taxonomic records."
              ),
              tags$li(
                "Source entries identify source/subsource combinations, not necessarily distinct primary sources."
              ),
              tags$li(
                "Taxonomic entries include different ranks and must not be interpreted as a species count."
              )
            )
          )
        ),
        tabPanel(
          "Map explorer",
          value = "map",
          div(
            class = "content-panel",
            h2("Map explorer"),
            div(
              class = "map-layout",
              div(
                class = "map-filter-panel",
                h3("Sources"),
                textInput(
                  "map_source_search",
                  label = NULL,
                  placeholder = "Search sources"
                ),
                div(
                  class = "choice-list",
                  checkboxGroupInput(
                    "map_sources",
                    label = NULL,
                    choices = character(0)
                  )
                ),
                div(class = "filter-count", textOutput("source_count")),
                h3("Subsources"),
                textInput(
                  "map_subsource_search",
                  label = NULL,
                  placeholder = "Search subsources"
                ),
                div(
                  class = "choice-list",
                  checkboxGroupInput(
                    "map_subsources",
                    label = NULL,
                    choices = character(0)
                  )
                ),
                div(class = "filter-count", textOutput("subsource_count")),
                div(
                  class = "filter-actions",
                  actionButton(
                    "apply_map_filters",
                    "Apply filters",
                    class = "btn-primary"
                  ),
                  actionButton("reset_map_filters", "Reset filters")
                )
              ),
              div(
                class = "map-view-panel",
                div(
                  class = "map-frame",
                  leaflet::leafletOutput("coverage_map", height = "70vh")
                ),
                div(class = "map-status", textOutput("map_status")),
                div(class = "pending-note", textOutput("map_pending_status")),
                tags$details(
                  class = "map-details",
                  tags$summary("Map details"),
                  textOutput("map_details"),
                  p(
                    "Circles represent coordinate entries, not abundance. Zero abundance values are retained. Coordinate qualifiers are preserved; overlapping world copies do not duplicate event counts."
                  )
                )
              ),
              point_detail_panel
            )
          ),
          selected_records_panel()
        ),
        tabPanel(
          "Data catalog",
          value = "catalog",
          div(
            class = "content-panel",
            h2("Data catalog"),
            h3("Database structure"),
            p(
              class = "section-note",
              paste(
                "Scroll over the diagram to zoom and drag to pan.",
                "Click a table header to inspect its documentation.",
                "Table headers also support keyboard focus and Enter."
              )
            ),
            p(
              class = "section-note",
              paste(
                "Arrows connect referencing fields to referenced fields.",
                "This catalogue is independent of Map explorer filters."
              )
            ),
            textOutput("catalog_schema_status"),
            br(),
            fluidRow(
              column(
                8,
                div(
                  id = "catalog_diagram_panel",
                  div(
                    style = "display:flex;gap:8px;flex-wrap:wrap;margin-bottom:10px;",
                    tags$button(
                      type = "button",
                      class = "btn btn-default",
                      `data-catalog-action` = "in",
                      "Zoom in"
                    ),
                    tags$button(
                      type = "button",
                      class = "btn btn-default",
                      `data-catalog-action` = "out",
                      "Zoom out"
                    ),
                    tags$button(
                      type = "button",
                      class = "btn btn-default",
                      `data-catalog-action` = "fit",
                      "Fit diagram"
                    )
                  ),
                  div(
                    style = paste(
                      "height:850px;overflow:hidden;",
                      "border:1px solid #dce4eb;border-radius:6px;",
                      "background:white;"
                    ),
                    DiagrammeR::grVizOutput(
                      "catalog_ports_graph",
                      width = "100%",
                      height = "850px"
                    )
                  )
                )
              ),
              column(
                4,
                h3(
                  style = "font-size:19px;margin-top:0;",
                  "Table documentation"
                ),
                textOutput("catalog_documentation_table"),
                p(
                  class = "section-note",
                  "Select a variable row to inspect its additional metadata."
                ),
                div(
                  style = paste(
                    "max-height:460px;overflow-y:auto;",
                    "overflow-wrap:anywhere;"
                  ),
                  DT::DTOutput("catalog_metadata_table")
                ),
                h4("Variable details"),
                div(
                  style = paste(
                    "max-height:320px;overflow-y:auto;",
                    "overflow-wrap:anywhere;padding-right:10px;"
                  ),
                  uiOutput("catalog_variable_details")
                )
              )
            ),
            tags$hr(),
            h3("Selected columns"),
            p(
              class = "section-note",
              paste(
                "Click field rows inside the diagram to select or deselect columns.",
                "Selection does not run a database query."
              )
            ),
            actionButton("catalog_columns_clear", "Clear selection"),
            tableOutput("catalog_selected_columns"),
            h3("Generated view"),
            fluidRow(
              selectInput(
                "catalog_view_base",
                "Base table",
                choices = character(0),
                width = "100%"
              ),
              textOutput("catalog_base_help"),
              column(
                6,
                div(
                  style = "margin-top:25px;",
                  actionButton(
                    "catalog_view_generate",
                    "Generate view",
                    class = "btn-primary"
                  )
                )
              )
            ),
            p(
              class = "section-note",
              paste(
                "One row per base-table entry.",
                "This version follows child-to-parent relationships only",
                "and uses LEFT JOINs to declared primary keys.",
                "Date, coordinate and depth qualifiers are added when applicable.",
                "Abundance is stored in cells/L; depths in metres;",
                "protocol volumes remain text documented in mL."
              )
            ),
            textOutput("catalog_view_status"),
            textOutput("catalog_auto_qualifiers"),
            div(
              style = "display:flex;gap:8px;flex-wrap:wrap;margin:10px 0;",
              actionButton(
                "catalog_filters_apply",
                "Apply filters",
                class = "btn-primary"
              ),
              actionButton(
                "catalog_filters_reset",
                "Reset filters"
              )
            ),
            textOutput("catalog_filter_status"),
            checkboxInput(
              "catalog_view_show_ids",
              "Show technical identifiers",
              value = FALSE
            ),
            DT::DTOutput("catalog_view_table"),
            div(
              style = "display:flex;gap:8px;flex-wrap:wrap;margin-top:12px;",
              actionButton("catalog_view_prev", "Previous"),
              actionButton("catalog_view_next", "Next"),
              actionButton("catalog_csv_prepare", "Prepare CSV"),
              uiOutput("catalog_view_download_control", inline = TRUE)
            ),
            textOutput("catalog_csv_status"),
            p(
              class = "section-note",
              paste(
                "SQL pages contain at most 100 rows.",
                "Column filters cover the complete loaded view and use AND.",
                "Text comparisons are literal and case-sensitive.",
                "Apply pending filters before preparing a CSV.",
                "Technical identifiers are hidden by default.",
                "Prepared CSV files contain the complete applied result,",
                "qualifiers and real technical keys, with a 100,000-row limit.",
                "Larger results are refused rather than truncated.",
                "CSV uses UTF-8, comma separators and \\N for SQL NULL.",
                "Empty strings remain empty strings.",
                "Global sorting is not available."
              )
            ),
            p(
              class = "section-note",
              paste(
                "The metadata dictionary supplies documentation",
                "and is excluded from the diagram.",
                "PK and FK indicate declared key participation,",
                "not a completed integrity check."
              )
            )
          )
        ),
        tabPanel(
          "Metadata dictionary",
          value = "dictionary",
          div(
            class = "content-panel",
            h2("Metadata dictionary"),
            p(
              "Variable definitions, units, expected values and missing information."
            ),
            div(
              class = "development-note",
              "This section is not implemented yet."
            )
          )
        )
      )
    ),
    div(
      class = "app-footer",
      "PhytoMap Data Viewer | Application under development"
    )
  )
)

shinyApp(ui = ui, server = server)
