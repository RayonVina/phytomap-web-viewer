# Source explorer: browse the source table and its JSON metadata.

source_section_order <- c(
  "identity",
  "citation",
  "access",
  "coverage",
  "data_content",
  "methods",
  "quality_and_limitations",
  "curation",
  "event",
  "related_resources",
  "phyto_map_ingestion",
  "provenance_note"
)

source_label <- function(key) {
  text <- gsub("_", " ", key, fixed = TRUE)
  paste0(toupper(substr(text, 1L, 1L)), substr(text, 2L, nchar(text)))
}

source_is_container <- function(x) is.list(x) && length(x) > 0L

source_container_summary <- function(x) {
  n <- length(x)
  if (is.null(names(x))) {
    paste0("List, ", n, if (n == 1L) " item" else " items")
  } else {
    paste0("Object, ", n, if (n == 1L) " field" else " fields")
  }
}

# JSON null, empty string and empty containers are shown as distinct states.
source_scalar_ui <- function(x) {
  if (is.null(x)) {
    return(tags$span(class = "json-null", "null (not recorded)"))
  }
  if (is.list(x)) {
    return(tags$span(class = "json-null", "empty"))
  }
  if (is.character(x)) {
    if (!nzchar(x)) {
      return(tags$span(class = "json-null", "empty string"))
    }
    if (grepl("^https?://[^[:space:]]+$", x)) {
      return(
        tags$a(href = x, target = "_blank", rel = "noopener noreferrer", x)
      )
    }
    return(tags$span(x))
  }
  if (is.logical(x)) {
    return(tags$span(if (isTRUE(x)) "true" else "false"))
  }
  tags$span(format(x, digits = 15, scientific = FALSE, trim = TRUE))
}

source_node_ui <- function(x, depth = 0L) {
  if (!source_is_container(x)) {
    return(source_scalar_ui(x))
  }
  keys <- names(x)
  if (is.null(keys)) {
    items <- lapply(seq_along(x), function(i) {
      tags$li(source_node_ui(x[[i]], depth + 1L))
    })
    return(tags$ul(class = "json-list", items))
  }
  rows <- lapply(seq_along(x), function(i) {
    value <- x[[i]]
    content <- if (source_is_container(value)) {
      tags$details(
        open = if (depth < 1L) NA else NULL,
        tags$summary(source_container_summary(value)),
        source_node_ui(value, depth + 1L)
      )
    } else {
      source_scalar_ui(value)
    }
    list(tags$dt(source_label(keys[[i]])), tags$dd(content))
  })
  tags$dl(class = "json-dl", unlist(rows, recursive = FALSE))
}

source_summary_ui <- function(meta) {
  keys <- names(meta)
  hidden <- c("schema_version", "source", "subsource")
  ordered <- c(
    intersect(source_section_order, keys),
    setdiff(keys, c(source_section_order, hidden))
  )
  sections <- lapply(ordered, function(key) {
    tags$details(
      class = "source-section",
      open = if (key %in% c("identity", "citation", "access")) NA else NULL,
      tags$summary(source_label(key)),
      source_node_ui(meta[[key]], 0L)
    )
  })
  div(class = "source-json-tree", sections)
}

source_field_ui <- function(label, value) {
  tagList(tags$dt(label), tags$dd(value))
}

register_sources_server <- function(input, output, session, con) {
  source_list <- tryCatch(
    if (is.null(con)) NULL else get_source_list(con),
    error = function(e) {
      message("Source list failed: ", conditionMessage(e))
      NULL
    }
  )

  output$src_status <- renderText({
    if (is.null(source_list)) {
      return("The source catalogue is unavailable.")
    }
    paste0(
      nrow(source_list),
      " source/subsource entries from ",
      length(unique(source_list$source)),
      " sources."
    )
  })

  output$src_table <- DT::renderDT({
    req(!is.null(source_list))
    display <- data.frame(
      Source = source_list$source,
      Subsource = ifelse(
        is.na(source_list$subsource),
        "(none)",
        source_list$subsource
      ),
      Events = source_list$n_events,
      check.names = FALSE
    )
    DT::datatable(
      display,
      rownames = FALSE,
      selection = "single",
      filter = "top",
      options = list(
        pageLength = 15,
        lengthMenu = c(15, 50, 100),
        scrollX = TRUE
      )
    )
  })

  selected_meta <- reactive({
    row <- input$src_table_rows_selected
    req(!is.null(source_list), length(row) == 1L)
    data <- tryCatch(
      get_source_metadata(con, source_list$source_id[[row]]),
      error = function(e) NULL
    )
    validate(need(
      !is.null(data) && nrow(data) == 1L,
      "The selected source could not be loaded."
    ))
    data
  })

  parsed_meta <- reactive({
    row <- selected_meta()
    if (is.na(row$metadata)) {
      return(NULL)
    }
    tryCatch(
      jsonlite::fromJSON(row$metadata, simplifyVector = FALSE),
      error = function(e) NULL
    )
  })

  output$src_header <- renderUI({
    if (length(input$src_table_rows_selected) != 1L) {
      return(p(
        class = "section-note",
        "Select a source/subsource entry to inspect its metadata."
      ))
    }
    row <- selected_meta()
    title <- if (is.na(row$subsource)) {
      row$source
    } else {
      paste(row$source, "/", row$subsource)
    }
    na_text <- function(text) tags$span(class = "json-null", text)
    tagList(
      h3(title),
      tags$dl(
        class = "json-dl",
        source_field_ui(
          "URL",
          if (is.na(row$url)) {
            na_text("No URL recorded")
          } else {
            source_scalar_ui(row$url)
          }
        ),
        source_field_ui(
          "Notes",
          if (is.na(row$notes)) {
            na_text("No notes")
          } else {
            source_scalar_ui(row$notes)
          }
        ),
        source_field_ui("Events", format(row$n_events, big.mark = ",")),
        source_field_ui("Created", row$created),
        source_field_ui("Modified", row$modified),
        source_field_ui(
          "Metadata size",
          if (is.na(row$metadata_chars)) {
            na_text("No metadata recorded")
          } else {
            paste(format(row$metadata_chars, big.mark = ","), "characters")
          }
        )
      )
    )
  })

  output$src_summary <- renderUI({
    meta <- parsed_meta()
    validate(need(
      is.list(meta) && !is.null(names(meta)),
      "Metadata is missing, not valid JSON, or exceeds the display limit."
    ))
    source_summary_ui(meta)
  })

  output$src_raw <- renderText({
    row <- selected_meta()
    validate(need(
      !is.na(row$metadata),
      "Metadata is missing or exceeds the display limit."
    ))
    as.character(jsonlite::prettify(row$metadata, indent = 2))
  })
}
