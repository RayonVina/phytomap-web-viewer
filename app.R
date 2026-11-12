library(shiny)

required_packages <- c(
  "leaflet",
  "htmltools",
  "DiagrammeR",
  "DT"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
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
source(file.path("R", "table_display.R"), local = TRUE)

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

    selectInput(
      "point_event_id",
      "Event",
      choices = character(0)
    ),

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

    selectInput(
      "point_sample_id",
      "Sample",
      choices = character(0)
    ),

    tableOutput("point_sample_metadata")
  ),

  tags$details(
    open = NA,
    tags$summary("Abundance records"),

    div(
      class = "detail-table",
      tableOutput("point_records")
    ),

    div(
      class = "detail-buttons",
      actionButton("point_records_prev", "Previous records"),
      actionButton("point_records_next", "Next records")
    ),

    textOutput("point_record_page"),

    selectInput(
      "point_record_id",
      "Record details",
      choices = character(0)
    ),

    tableOutput("point_abundance_metadata")
  ),

  tags$details(
    tags$summary("Taxonomy"),
    tableOutput("point_taxonomy")
  ),

  tags$details(
    tags$summary("Sampling protocol"),
    tableOutput("point_protocol")
  )
)


# DT receives only one bounded SQL page.
selected_records_panel <- function() {
  div(
    class = "content-panel",
    id = "selected_records_panel",

    h3("Selected records"),

    div(
      style = paste(
        "display:flex;gap:8px;align-items:center;",
        "flex-wrap:wrap;margin-bottom:12px;"
      ),

      textOutput("sr_selection_count", inline = TRUE),

      actionButton(
        "sr_load",
        "Load selected records",
        class = "btn-primary"
      ),

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
      style = paste(
        "display:flex;gap:8px;align-items:center;",
        "flex-wrap:wrap;margin-top:12px;"
      ),

      actionButton("sr_prev", "Previous"),
      actionButton("sr_next", "Next"),

      textOutput("sr_page_status", inline = TRUE),
      uiOutput("sr_download_control", inline = TRUE)
    ),

    tags$details(
      style = "margin-top:10px;font-size:12px;color:#526776;",

      tags$summary("Selection and export details"),

      p(
        paste(
          "Empty columns are hidden using the complete loaded selection,",
          "not just the current page.",
          "Only SQL NULL and empty strings count as empty;",
          "zero, unknown and undetermined are retained."
        )
      ),

      p(
        paste(
          "CSV downloads include every record in the loaded selection,",
          "not only the visible page.",
          "The limit is 100,000 records; larger selections are not truncated.",
          "CSV uses UTF-8, comma separators and \\N for SQL NULL.",
          "Empty strings remain empty strings."
        )
      ),

      p(
        paste(
          "Click a column header to sort the current SQL page.",
          "Sorting does not reorder the complete result or CSV exports.",
          "Numeric display is rounded to four decimal places;",
          "stored values and CSV precision are unchanged.",
          "Blank cells may represent SQL NULL or empty strings."
        )
      )
    )
  )
}


ui <- fluidPage(
  title = "PhytoMap Data Viewer",

  tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "styles.css?v=3"
    ),
    tags$script(src = "catalog_interactions.js?v=3"),
    tags$script(src = "catalog_filters.js?v=1")
  ),

  div(
    class = "app-shell",

    div(
      class = "app-header",
      div(
        class = "app-brand",
        tags$img(
          src = "phyto-map_icon.png",
          class = "app-logo",
          alt = "Phyto-MAP project logo"
        ),

        div(
          class = "app-brand-text",
          h1("PhytoMap Data Viewer"),
          p(
            "Explore phytoplankton abundance, sampling coverage and metadata."
          )
        )
      )
    ),

    div(
      class = "page-navigation",

      tabsetPanel(
        id = "main_navigation",
        selected = "home",
        type = "pills",

        # Home page.
        tabPanel(
          "Home",
          value = "home",

          div(
            class = "home-introduction",

            p(
              paste(
                "Phyto-MAP (Phytoplankton Marine Atlas Project) is a relational database of marine phytoplankton abundance records cfrom published and openly accessible sources. Abundance valstandardised to cells per litre and linked to sampling coordinates, dates, depths, source provenance and available sand analytical metadata."
              )
            ),
            p(
              paste(
                "Source-reported taxon designations and curation attributes are retained alongside harmonised classifications and World Register of Marine Species (WoRMS) AphiaIDs where resolved. The database supports studies of phytoplankton distributions, taxonomic composition and abundance, and the development and evaluation of ecological models using compatible observations."
              )
            )
          ),

          h3("Database at a glance"),
          uiOutput("overview_cards"),
          textOutput("overview_status"),

          h3("Dataset coverage"),

          p(
            class = "section-note",

            paste(
              "These summaries describe the coverage of the compiled observations,",
              "not standardised sampling effort or ecological representativeness.",
              "Events and samples are included below when associated with",
              "at least one abundance record."
            )
          ),

          textOutput("home_chart_status"),

          div(
            class = "home-chart-grid",

            div(
              class = "home-chart-card",

              h3("Sampling events by year"),

              plotOutput(
                "home_year_plot",
                height = "340px"
              ),

              div(
                class = "section-note",
                textOutput("home_year_note")
              )
            ),

            div(
              class = "home-chart-card",

              h3("Sampling events by source"),

              plotOutput(
                "home_source_plot",
                height = "340px"
              ),

              p(
                class = "section-note",

                paste(
                  "Events are grouped by the stored source label, combining",
                  "its subsources without harmonising different labels.",
                  "The eight largest categories are shown separately;",
                  "any remainder is combined. Each event is counted once."
                )
              )
            ),

            div(
              class = "home-chart-card",

              h3("Taxonomic resolution"),

              plotOutput(
                "home_rank_plot",
                height = "340px"
              ),

              p(
                class = "section-note",

                paste(
                  "Counts refer to abundance records, not distinct taxa or species.",
                  "The eight largest categories are shown separately;",
                  "any remainder is combined.",
                  "Unresolved AphiaIDs are not interpreted as a single taxon."
                )
              )
            ),

            div(
              class = "home-chart-card",

              h3("Sampling depth"),

              plotOutput(
                "home_depth_plot",
                height = "340px"
              ),

              div(
                class = "section-note",
                textOutput("home_depth_note")
              )
            )
          ),

          tags$details(
            class = "home-methods",

            tags$summary("Understanding the summaries"),

            tags$ul(
              tags$li(
                paste(
                  "The cards above count rows in database tables.",
                  "The charts use the represented events, samples and abundance",
                  "records described in their captions."
                )
              ),

              tags$li(
                paste(
                  "An abundance record is not an independent sample.",
                  "Stored sample identifiers represent event-depth combinations",
                  "and do not necessarily identify individual physical samples."
                )
              ),

              tags$li(
                paste(
                  "Source/subsource entries are not necessarily distinct primary",
                  "sources. Taxonomic entries include different ranks and must",
                  "not be interpreted as a species count."
                )
              ),

              tags$li(
                paste(
                  "Retained zeros describe source-specific non-detection under",
                  "the relevant protocol. Missing records must not be interpreted",
                  "automatically as zero abundance."
                )
              ),

              tags$li(
                paste(
                  "Common abundance units do not remove differences in sampling",
                  "methods, analytical procedures, precision or taxonomic resolution."
                )
              )
            )
          )
        ),

        # Map explorer page.
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

                div(
                  class = "filter-count",
                  textOutput("source_count")
                ),

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

                div(
                  class = "filter-count",
                  textOutput("subsource_count")
                ),

                div(
                  class = "filter-actions",

                  actionButton(
                    "apply_map_filters",
                    "Apply filters",
                    class = "btn-primary"
                  ),

                  actionButton(
                    "reset_map_filters",
                    "Reset filters"
                  )
                )
              ),

              div(
                class = "map-view-panel",

                div(
                  class = "map-frame",

                  leaflet::leafletOutput(
                    "coverage_map",
                    height = "70vh"
                  )
                ),

                div(
                  class = "map-status",
                  textOutput("map_status")
                ),

                div(
                  class = "pending-note",
                  textOutput("map_pending_status")
                ),

                tags$details(
                  class = "map-details",

                  tags$summary("Map details"),

                  textOutput("map_details"),

                  p(
                    paste(
                      "Circles represent coordinate entries, not abundance.",
                      "Zero abundance values are retained.",
                      "Coordinate qualifiers are preserved;",
                      "overlapping world copies do not duplicate event counts."
                    )
                  )
                )
              ),

              point_detail_panel
            )
          ),

          selected_records_panel()
        ),

        # Data catalog page.
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
                    style = paste(
                      "display:flex;gap:8px;",
                      "flex-wrap:wrap;margin-bottom:10px;"
                    ),

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

                  paste(
                    "Select a variable row to inspect",
                    "its additional metadata."
                  )
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

            actionButton(
              "catalog_columns_clear",
              "Clear selection"
            ),

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
              style = paste(
                "display:flex;gap:8px;",
                "flex-wrap:wrap;margin:10px 0;"
              ),

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
              style = paste(
                "display:flex;gap:8px;",
                "flex-wrap:wrap;margin-top:12px;"
              ),

              actionButton(
                "catalog_view_prev",
                "Previous"
              ),

              actionButton(
                "catalog_view_next",
                "Next"
              ),

              actionButton(
                "catalog_csv_prepare",
                "Prepare CSV"
              ),

              uiOutput(
                "catalog_view_download_control",
                inline = TRUE
              )
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
                "Column sorting applies only to the current SQL page."
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

        # Metadata dictionary placeholder.
        tabPanel(
          "Metadata dictionary",
          value = "dictionary",

          div(
            class = "content-panel",

            h2("Metadata dictionary"),

            p(
              paste(
                "Variable definitions, units, expected values",
                "and missing information."
              )
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
