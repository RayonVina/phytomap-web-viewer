# Home overview cache: shared across sessions in this R process.
get_cached_overview <- local({
  cached <- NULL
  function(con) {
    if (is.null(cached)) {
      data <- get_database_overview(con)
      if (nrow(data) != 1L || anyNA(data)) {
        stop("Invalid overview result.")
      }
      cached <<- list(data = data, counted_at = Sys.time())
    }
    cached
  }
})

# Register Home outputs and navigation for one session.
register_home_server <- function(input, output, session, con) {
  overview <- reactive({
    validate(need(!is.null(con), "The database is unavailable."))
    result <- tryCatch(get_cached_overview(con), error = function(e) {
      message("Overview failed: ", conditionMessage(e))
      NULL
    })
    validate(need(
      !is.null(result),
      "The database summary could not be loaded."
    ))
    result
  })
  metric_card <- function(value, label) {
    column(
      4,
      div(
        class = "metric-card",
        div(
          class = "metric-value",
          format(value, big.mark = ",", scientific = FALSE, trim = TRUE)
        ),
        div(class = "metric-label", label)
      )
    )
  }
  output$overview_cards <- renderUI({
    d <- overview()$data
    tagList(
      fluidRow(
        metric_card(d$abundance_records[[1]], "Abundance records"),
        metric_card(d$samples[[1]], "Samples"),
        metric_card(d$events[[1]], "Sampling events")
      ),
      fluidRow(
        metric_card(d$source_entries[[1]], "Source / subsource entries"),
        metric_card(d$sampling_protocols[[1]], "Sampling protocols"),
        metric_card(d$taxonomy_entries[[1]], "Taxonomic entries")
      )
    )
  })
  output$overview_status <- renderText(paste0(
    "Exact database row counts. Calculated: ",
    format(overview()$counted_at, "%Y-%m-%d %H:%M:%S %Z"),
    "."
  ))
  observeEvent(
    input$open_map,
    updateTabsetPanel(session, "main_navigation", selected = "map")
  )
}
