# Build a field-port diagram from the inspected SQLite schema.
catalog_schema_dot <- function(schema) {
  tables <- schema$tables$table_name
  tables <- tables[tables != "metadata_dictionary"]

  if (!length(tables)) {
    stop("No tables are available for the diagram.", call. = FALSE)
  }

  fields <- schema$columns[
    schema$columns$table_name %in% tables,
    ,
    drop = FALSE
  ]

  fields <- fields[
    order(match(fields$table_name, tables), fields$cid),
    ,
    drop = FALSE
  ]

  fields$port_id <- paste0("p", seq_len(nrow(fields)))
  node_ids <- stats::setNames(paste0("t", seq_along(tables)), tables)

  # Presentation columns only; relationships still come from SQLite.
  layout_columns <- list(
    c("taxonomy"),
    c("taxon_record"),
    c("abundance", "sampling_protocol"),
    c("sample", "depth"),
    c("event"),
    c("event_datetime", "coordinates", "source")
  )

  layout_columns <- lapply(
    layout_columns,
    function(group) group[group %in% tables]
  )
  layout_columns <- Filter(length, layout_columns)

  unplaced <- setdiff(
    tables,
    unlist(layout_columns, use.names = FALSE)
  )

  if (length(unplaced)) {
    layout_columns <- c(layout_columns, list(unplaced))
  }

  table_rank <- stats::setNames(
    rep(seq_along(layout_columns), lengths(layout_columns)),
    unlist(layout_columns, use.names = FALSE)
  )

  rank_statements <- vapply(
    layout_columns,
    function(group) {
      ids <- unname(node_ids[group])

      ordering <- if (length(ids) > 1L) {
        paste0(
          paste(ids, collapse = " -> "),
          " [style=invis, weight=100];"
        )
      } else {
        ""
      }

      paste0(
        "{ rank=same; ",
        paste(ids, collapse = "; "),
        "; ",
        ordering,
        " }"
      )
    },
    character(1)
  )

  representatives <- vapply(
    layout_columns,
    function(group) {
      anchor <- if ("coordinates" %in% group) {
        "coordinates"
      } else {
        group[[1L]]
      }

      unname(node_ids[[anchor]])
    },
    character(1)
  )

  layout_chain <- if (length(representatives) > 1L) {
    paste0(
      paste(representatives, collapse = " -> "),
      " [style=invis, weight=100];"
    )
  } else {
    ""
  }

  keys <- schema$relationships[
    schema$relationships$source_table %in%
      tables &
      schema$relationships$referenced_table %in% tables,
    ,
    drop = FALSE
  ]

  escape_text <- function(x) {
    as.character(htmltools::htmlEscape(as.character(x)))
  }

  nodes <- vapply(
    tables,
    function(table_name) {
      columns <- fields[
        fields$table_name == table_name,
        ,
        drop = FALSE
      ]

      foreign_columns <- keys$source_column[
        keys$source_table == table_name
      ]

      rows <- vapply(
        seq_len(nrow(columns)),
        function(i) {
          column <- columns[i, , drop = FALSE]

          flags <- c(
            if (column$primary_key_position[[1L]] > 0L) "PK",
            if (column$column_name[[1L]] %in% foreign_columns) "FK"
          )

          marker <- if (length(flags)) {
            paste0(" [", paste(flags, collapse = ", "), "]")
          } else {
            ""
          }

          paste0(
            '<TR><TD PORT="',
            column$port_id[[1L]],
            '" ALIGN="LEFT">',
            escape_text(column$column_name[[1L]]),
            marker,
            "</TD></TR>"
          )
        },
        character(1)
      )

      paste0(
        node_ids[[table_name]],
        ' [label=<<TABLE BORDER="1" CELLBORDER="0" ',
        'CELLSPACING="0" CELLPADDING="5" COLOR="#176b80">',
        '<TR><TD BGCOLOR="#176b80">',
        '<FONT COLOR="white"><B>',
        escape_text(table_name),
        "</B></FONT></TD></TR>",
        paste(rows, collapse = "\n"),
        "</TABLE>>];"
      )
    },
    character(1)
  )

  edges <- vapply(
    seq_len(nrow(keys)),
    function(i) {
      key <- keys[i, , drop = FALSE]

      source_field <- fields[
        fields$table_name == key$source_table[[1L]] &
          fields$column_name == key$source_column[[1L]],
        ,
        drop = FALSE
      ]

      target_name <- key$referenced_column[[1L]]

      if (is.na(target_name) || target_name == "") {
        target_field <- fields[
          fields$table_name == key$referenced_table[[1L]] &
            fields$primary_key_position == key$column_sequence[[1L]] + 1L,
          ,
          drop = FALSE
        ]
      } else {
        target_field <- fields[
          fields$table_name == key$referenced_table[[1L]] &
            fields$column_name == target_name,
          ,
          drop = FALSE
        ]
      }

      if (nrow(source_field) != 1L || nrow(target_field) != 1L) {
        stop(
          "A relationship could not be resolved to its field ports.",
          call. = FALSE
        )
      }

      source_table <- key$source_table[[1L]]
      target_table <- key$referenced_table[[1L]]

      source_rank <- table_rank[[source_table]]
      target_rank <- table_rank[[target_table]]

      if (source_rank < target_rank) {
        source_side <- "e"
        target_side <- "w"
      } else if (source_rank > target_rank) {
        source_side <- "w"
        target_side <- "e"
      } else {
        # Same-column relationships must leave through cell side ports.
        # North/south ports can send the line through the table interior.
        source_side <- "e"
        target_side <- "e"
      }

      align_coordinates <- identical(source_table, "event") &&
        identical(key$source_column[[1L]], "coordinates_id") &&
        identical(target_table, "coordinates")

      edge_attributes <- if (align_coordinates) {
        "constraint=true, weight=1000"
      } else {
        "constraint=false"
      }

      paste0(
        node_ids[[source_table]],
        ":",
        source_field$port_id[[1L]],
        ":",
        source_side,
        " -> ",
        node_ids[[target_table]],
        ":",
        target_field$port_id[[1L]],
        ":",
        target_side,
        " [",
        edge_attributes,
        "];"
      )
    },
    character(1)
  )

  paste(
    "digraph catalog_schema {",
    paste(
      'graph [rankdir=LR, splines=spline,',
      'nodesep=0.65, ranksep=1.0, bgcolor="transparent"];'
    ),
    'node [shape=plain, fontname="Arial", fontsize=12];',
    'edge [color="#607d8b", arrowsize=0.7, penwidth=1.1];',
    paste(nodes, collapse = "\n"),
    paste(rank_statements, collapse = "\n"),
    layout_chain,
    paste(edges, collapse = "\n"),
    "}",
    sep = "\n"
  )
}

# Register diagram navigation, table inspection and variable metadata.
register_catalog_server <- function(input, output, session, con) {
  catalog_schema_data <- reactiveVal(NULL)
  catalog_schema_error <- reactiveVal("")
  inspected_table <- reactiveVal(NULL)
  inspected_variable <- reactiveVal(NULL)
  selected_fields <- reactiveVal(character(0))
  loaded_view <- reactiveVal(NULL)
  view_result <- reactiveVal(NULL)
  view_cursors <- reactiveVal(list(NULL))
  view_notice <- reactiveVal("")
  filter_revision <- reactiveVal(0L)
  prepared_csv <- reactiveVal(NULL)
  csv_notice <- reactiveVal("")

  next_filter_revision <- function() {
    revision <- isolate(filter_revision()) + 1L
    filter_revision(revision)
    revision
  }

  clear_prepared_csv <- function() {
    prepared <- isolate(prepared_csv())

    if (!is.null(prepared)) {
      unlink(prepared$file)
    }

    prepared_csv(NULL)
    csv_notice("")
  }

  session$onSessionEnded(function() {
    prepared <- isolate(prepared_csv())

    if (!is.null(prepared)) {
      unlink(prepared$file)
    }
  })

  observeEvent(
    loaded_view(),
    {
      clear_prepared_csv()
    },
    ignoreNULL = TRUE,
    priority = 100
  )

  catalogue_fields <- reactive({
    req(catalog_schema_data())
    cv_fields(catalog_schema_data())
  })

  eligible_base_tables <- reactive({
    fields <- catalogue_fields()
    ids <- selected_fields()

    if (!length(ids)) {
      return(character(0))
    }

    selected <- fields[
      match(ids, fields$field_id),
      ,
      drop = FALSE
    ]

    candidates <- unique(selected$table_name)

    compatible <- vapply(
      candidates,
      function(base_table) {
        tryCatch(
          {
            cv_plan(
              con,
              catalog_schema_data(),
              base_table,
              ids
            )
            TRUE
          },
          error = function(e) FALSE
        )
      },
      logical(1)
    )

    candidates[compatible]
  })

  observe({
    choices <- eligible_base_tables()
    current <- isolate(input$catalog_view_base)

    selected <- if (
      length(current) == 1L &&
        !is.na(current) &&
        current %in% choices
    ) {
      current
    } else if (length(choices)) {
      choices[[1L]]
    } else {
      character(0)
    }

    freezeReactiveValue(input, "catalog_view_base")

    updateSelectInput(
      session,
      "catalog_view_base",
      choices = stats::setNames(choices, choices),
      selected = selected
    )
  })

  output$catalog_base_help <- renderText({
    if (!length(selected_fields())) {
      return("Select fields in the diagram to choose a base table.")
    }

    if (!length(eligible_base_tables())) {
      return(paste(
        "No selected table can serve as the base for this combination.",
        "Select a field from a common child table, such as abundance.id",
        "when appropriate, or remove incompatible fields."
      ))
    }

    paste(
      "Only selected tables that can reach every selected field",
      "using supported child-to-parent joins are available."
    )
  })

  sync_selection <- function() {
    session$sendCustomMessage(
      "catalog_selection_state",
      list(ids = unname(selected_fields()))
    )
  }

  observeEvent(input$catalog_field_toggle, {
    event <- input$catalog_field_toggle
    fields <- catalogue_fields()

    if (
      !is.list(event) ||
        !is.character(event$field_id) ||
        length(event$field_id) != 1L ||
        is.na(event$field_id) ||
        !event$field_id %in% fields$field_id ||
        !is.logical(event$checked) ||
        length(event$checked) != 1L ||
        is.na(event$checked)
    ) {
      sync_selection()
      return()
    }

    current <- selected_fields()

    selected_fields(
      if (event$checked) {
        union(current, event$field_id)
      } else {
        setdiff(current, event$field_id)
      }
    )

    view_notice("")
    sync_selection()
  })

  observeEvent(input$catalog_columns_clear, {
    selected_fields(character(0))
    sync_selection()
  })

  output$catalog_selected_columns <- renderTable(
    {
      fields <- catalogue_fields()
      selected <- fields[
        match(selected_fields(), fields$field_id),
        ,
        drop = FALSE
      ]

      data.frame(
        Table = selected$table_name,
        Variable = selected$column_name
      )
    },
    rownames = FALSE
  )

  observeEvent(input$catalog_view_generate, {
    if (!length(selected_fields())) {
      view_notice("Select at least one column in the diagram.")
      return()
    }

    base_table <- input$catalog_view_base
    allowed <- eligible_base_tables()

    if (
      length(base_table) != 1L ||
        is.na(base_table) ||
        !base_table %in% allowed
    ) {
      view_notice(
        paste(
          "Choose a compatible base table.",
          "If none is available, revise the selected fields."
        )
      )
      return()
    }

    result <- tryCatch(
      {
        plan <- cv_plan(
          con,
          catalog_schema_data(),
          input$catalog_view_base,
          selected_fields()
        )

        page <- cv_page(con, plan)
        list(plan = plan, page = page)
      },
      error = function(e) {
        message("Catalogue view generation failed: ", conditionMessage(e))
        NULL
      }
    )

    if (is.null(result)) {
      view_notice(
        paste(
          "The view could not be generated.",
          "Check that the base table can reach all selected tables",
          "using child-to-parent relationships.",
          "The previous result is unchanged."
        )
      )
      return()
    }

    loaded_view(list(
      plan = result$plan,
      unfiltered_plan = result$plan,
      filters = list(),
      filter_revision = next_filter_revision(),
      selection = selected_fields(),
      base_table = input$catalog_view_base
    ))
    view_result(result$page)
    view_cursors(list(NULL))
    view_notice("")
  })

  move_view_page <- function(cursor, cursors) {
    loaded <- loaded_view()
    req(loaded)

    page <- tryCatch(
      cv_page(con, loaded$plan, cursor),
      error = function(e) {
        message("Catalogue page failed: ", conditionMessage(e))
        NULL
      }
    )

    if (is.null(page)) {
      view_notice(
        "The page could not be loaded. The previous page is unchanged."
      )
      return()
    }

    view_result(page)
    view_cursors(cursors)
    view_notice("")
  }

  observeEvent(input$catalog_view_next, {
    req(loaded_view(), view_result())

    page <- view_result()

    if (isTRUE(page$more) && nrow(page$data)) {
      cursor <- utils::tail(page$data[[".base_id"]], 1L)

      move_view_page(
        cursor,
        c(view_cursors(), list(cursor))
      )
    }
  })

  observeEvent(input$catalog_view_prev, {
    cursors <- view_cursors()

    if (length(cursors) > 1L) {
      previous <- utils::head(cursors, -1L)

      move_view_page(
        utils::tail(previous, 1L)[[1L]],
        previous
      )
    }
  })

  output$catalog_view_status <- renderText({
    if (nzchar(view_notice())) {
      return(view_notice())
    }

    loaded <- loaded_view()

    if (is.null(loaded)) {
      return("No view is loaded. Mark columns and choose a base table.")
    }

    pending <- !setequal(
      selected_fields(),
      loaded$selection
    ) ||
      !identical(
        input$catalog_view_base,
        loaded$base_table
      )

    paste0(
      "Base table: ",
      loaded$base_table,
      ". One row per base-table entry. Technical keys are retained internally and in CSV.",
      " LEFT JOIN path: ",
      paste(loaded$plan$tables, collapse = ", "),
      ". Page ",
      length(view_cursors()),
      "; ",
      nrow(view_result()$data),
      " rows",
      if (isTRUE(view_result()$more)) "; more available." else "; last page.",
      if (pending) " Selection changed: generate the view again." else ""
    )
  })

  draft_filter_plan <- reactive({
    loaded <- loaded_view()
    req(loaded)

    draft <- input$catalog_filter_draft

    valid_draft <- is.list(draft) &&
      is.character(draft$view_id) &&
      length(draft$view_id) == 1L &&
      identical(
        draft$view_id,
        as.character(loaded$filter_revision)
      )

    filters <- if (valid_draft) {
      draft$filters
    } else {
      loaded$filters
    }

    cv_filter_plan(
      con,
      loaded$unfiltered_plan,
      filters
    )
  })

  apply_catalogue_plan <- function(plan, filters) {
    loaded <- loaded_view()
    req(loaded)

    page <- tryCatch(
      cv_page(con, plan),
      error = function(e) {
        message("Filtered catalogue query failed: ", conditionMessage(e))
        NULL
      }
    )

    if (is.null(page)) {
      view_notice(
        "Filters could not be applied. The previous result is unchanged."
      )
      return()
    }

    loaded$plan <- plan
    loaded$filters <- filters
    loaded$filter_revision <- next_filter_revision()

    loaded_view(loaded)
    view_result(page)
    view_cursors(list(NULL))
    view_notice("")
  }

  observeEvent(input$catalog_filters_apply, {
    req(loaded_view())

    plan <- tryCatch(
      draft_filter_plan(),
      error = function(e) {
        view_notice(conditionMessage(e))
        NULL
      }
    )

    if (!is.null(plan)) {
      apply_catalogue_plan(plan, plan$filters)
    }
  })

  observeEvent(input$catalog_filters_reset, {
    loaded <- loaded_view()
    req(loaded)

    plan <- cv_filter_plan(
      con,
      loaded$unfiltered_plan,
      list()
    )

    apply_catalogue_plan(plan, list())
  })

  output$catalog_filter_status <- renderText({
    loaded <- loaded_view()

    if (is.null(loaded)) {
      return("")
    }

    draft <- tryCatch(
      draft_filter_plan(),
      error = function(e) e
    )

    if (inherits(draft, "error")) {
      return(paste(
        "Invalid pending filter:",
        conditionMessage(draft)
      ))
    }

    pending <- !identical(draft$filters, loaded$filters)

    paste0(
      length(loaded$filters),
      " applied column filters. Filters use AND and cover the complete view.",
      if (pending) " Unapplied filter changes." else ""
    )
  })

  output$catalog_auto_qualifiers <- renderText({
    loaded <- loaded_view()

    if (is.null(loaded)) {
      return("")
    }

    fields <- loaded$plan$fields
    labels <- paste(fields$table_name, fields$column_name, sep = ".")

    automatic <- labels[
      !fields$field_id %in% loaded$selection &
        !labels %in% loaded$plan$technical_columns
    ]

    if (!length(automatic)) {
      return("")
    }

    paste(
      "Automatically included qualifiers:",
      paste(automatic, collapse = ", ")
    )
  })

  output$catalog_view_table <- DT::renderDT(
    {
      req(view_result(), loaded_view())

      loaded <- loaded_view()
      data <- view_result()$data

      visible <- setdiff(names(data), ".base_id")

      if (!isTRUE(input$catalog_view_show_ids)) {
        visible <- setdiff(
          visible,
          loaded$plan$technical_columns
        )
      }

      validate(need(
        length(visible) > 0L,
        paste(
          "This view contains only technical identifiers.",
          "Enable 'Show technical identifiers' or prepare the CSV."
        )
      ))

      data <- data[, visible, drop = FALSE]
      plan <- loaded$plan

      controls <- lapply(visible, function(label) {
        position <- match(label, plan$export_columns)
        field <- plan$fields[position, , drop = FALSE]
        kind <- cv_filter_kind(field$declared_type[[1L]])

        options <- if (kind == "number") {
          c(
            "Any value" = "any",
            "Equals" = "eq",
            "At least" = "ge",
            "At most" = "le",
            "Between" = "between",
            "SQL NULL" = "null"
          )
        } else {
          c(
            "Any value" = "any",
            "Contains (case-sensitive)" = "contains",
            "Equals exactly" = "eq",
            "SQL NULL" = "null",
            "Empty string" = "empty"
          )
        }

        ids <- vapply(
          loaded$filters,
          function(filter) filter$field_id,
          character(1)
        )
        applied <- match(field$field_id[[1L]], ids)

        filter <- if (is.na(applied)) {
          list(op = "any", value = "", value2 = "")
        } else {
          loaded$filters[[applied]]
        }

        tags$th(
          tags$div(
            `data-cv-field` = field$field_id[[1L]],
            `data-cv-view` = as.character(loaded$filter_revision),
            style = "min-width:170px;font-weight:normal;",
            tags$select(
              `data-cv-part` = "op",
              `aria-label` = paste("Filter operator for", label),
              style = "width:100%;margin-bottom:4px;",
              lapply(seq_along(options), function(i) {
                tags$option(
                  value = unname(options[[i]]),
                  selected = if (identical(unname(options[[i]]), filter$op)) {
                    "selected"
                  } else {
                    NULL
                  },
                  names(options)[[i]]
                )
              })
            ),
            tags$input(
              type = "text",
              `data-cv-part` = "value",
              value = filter$value,
              placeholder = "Filter value",
              `aria-label` = paste("Filter value for", label),
              style = "width:100%;margin-bottom:4px;"
            ),
            tags$input(
              type = "text",
              `data-cv-part` = "value2",
              value = filter$value2,
              placeholder = "Upper bound for Between",
              `aria-label` = paste("Upper bound for", label),
              style = if (kind == "number") {
                "width:100%;"
              } else {
                "display:none;"
              }
            )
          )
        )
      })

      container <- tags$table(
        class = "display",
        tags$thead(
          tags$tr(lapply(visible, tags$th)),
          tags$tr(controls)
        )
      )

      table <- DT::datatable(
        data,
        container = container,
        rownames = FALSE,
        escape = TRUE,
        selection = "none",
        options = list(
          dom = "t",
          paging = FALSE,
          searching = FALSE,
          ordering = TRUE,
          order = list(),
          orderCellsTop = TRUE,
          scrollX = TRUE,
          columnDefs = list(
            list(
              targets = "_all",
              defaultContent = ""
            )
          )
        )
      )

      format_result_table(table, data)
    },
    server = FALSE
  )

  observeEvent(input$catalog_csv_prepare, {
    loaded <- loaded_view()
    req(loaded)

    clear_prepared_csv()
    destination <- tempfile(fileext = ".csv")

    result <- withProgress(
      message = "Preparing complete CSV",
      value = 0.1,
      {
        value <- tryCatch(
          {
            rows <- cv_write_csv(
              con,
              destination,
              loaded$plan,
              max_rows = 100000L
            )

            list(file = destination, rows = rows)
          },
          cv_export_limit = function(e) {
            csv_notice(paste(
              "The loaded view exceeds 100,000 rows.",
              "Apply more restrictive filters before preparing the CSV."
            ))
            NULL
          },
          error = function(e) {
            message("CSV preparation failed: ", conditionMessage(e))
            csv_notice(
              "The CSV could not be prepared. No download is available."
            )
            NULL
          }
        )

        incProgress(0.8)
        value
      }
    )

    if (is.null(result)) {
      unlink(destination)
      return()
    }

    result$base_table <- loaded$base_table
    result$filename <- paste0(
      "phytomap-catalog-",
      loaded$base_table,
      "-",
      format(Sys.time(), "%Y%m%d-%H%M%S"),
      ".csv"
    )

    prepared_csv(result)

    csv_notice(paste0(
      "CSV ready: ",
      format(result$rows, big.mark = ",", trim = TRUE),
      " rows. Includes the complete loaded result and real technical keys."
    ))
  })

  output$catalog_csv_status <- renderText(csv_notice())

  output$catalog_view_download_control <- renderUI({
    prepared <- prepared_csv()

    if (
      is.null(prepared) ||
        !file.exists(prepared$file)
    ) {
      return(NULL)
    }

    downloadButton(
      "catalog_view_download",
      "Download prepared CSV"
    )
  })

  output$catalog_view_download <- downloadHandler(
    filename = function() {
      prepared <- isolate(prepared_csv())
      req(prepared)
      prepared$filename
    },
    contentType = "text/csv; charset=utf-8",
    content = function(file) {
      prepared <- isolate(prepared_csv())
      req(prepared, file.exists(prepared$file))

      if (!file.copy(prepared$file, file, overwrite = TRUE)) {
        stop("The prepared CSV could not be delivered.", call. = FALSE)
      }
    }
  )

  display_metadata <- function(value) {
    if (is.na(value)) {
      return("Not recorded (SQL NULL)")
    }

    text <- as.character(value)

    if (identical(text, "")) {
      return("[Empty string]")
    }

    text
  }

  observeEvent(input$main_navigation, {
    if (!identical(input$main_navigation, "catalog")) {
      return()
    }

    if (!is.null(catalog_schema_data())) {
      return()
    }

    catalog_schema_error("")

    if (is.null(con)) {
      catalog_schema_error("The database is unavailable.")
      return()
    }

    schema <- tryCatch(
      catalog_get_schema(con),
      error = function(e) {
        message("Schema loading failed: ", conditionMessage(e))
        catalog_schema_error(
          "The database structure could not be loaded."
        )
        NULL
      }
    )

    if (!is.null(schema)) {
      catalog_schema_data(schema)
    }
  })

  displayed_tables <- reactive({
    schema <- catalog_schema_data()
    req(!is.null(schema))

    tables <- schema$tables$table_name
    tables[tables != "metadata_dictionary"]
  })

  output$catalog_schema_status <- renderText({
    error <- catalog_schema_error()

    if (nzchar(error)) {
      return(error)
    }

    schema <- catalog_schema_data()

    if (is.null(schema)) {
      return("Database structure loads when Data catalog opens.")
    }

    tables <- displayed_tables()

    fields <- schema$columns[
      schema$columns$table_name %in% tables,
      ,
      drop = FALSE
    ]

    relationships <- schema$relationships[
      schema$relationships$source_table %in%
        tables &
        schema$relationships$referenced_table %in% tables,
      ,
      drop = FALSE
    ]

    paste0(
      length(tables),
      " displayed tables; ",
      nrow(fields),
      " displayed fields; ",
      nrow(relationships),
      " foreign-key column mappings. Schema metadata only."
    )
  })

  catalog_ports_dot <- reactive({
    schema <- catalog_schema_data()
    req(!is.null(schema))

    value <- tryCatch(
      catalog_schema_dot(schema),
      error = function(e) {
        message("Field-port diagram failed: ", conditionMessage(e))
        NULL
      }
    )

    validate(need(
      !is.null(value),
      "The field-level diagram could not be prepared."
    ))

    value
  })

  output$catalog_ports_graph <- DiagrammeR::renderGrViz({
    widget <- DiagrammeR::grViz(
      diagram = catalog_ports_dot(),
      engine = "dot",
      width = "100%",
      height = "850px"
    )

    htmlwidgets::onRender(
      widget,
      "
      function(el, x, data) {
        if (window.PhytomapCatalog) {
          window.PhytomapCatalog.attach(el, data);
        } else {
          console.error('Catalogue interaction script is unavailable.');
        }
      }
      ",
      data = list(
        tables = unname(displayed_tables()),
        selected = unname(isolate(selected_fields())),
        fields = lapply(seq_len(nrow(catalogue_fields())), function(i) {
          field <- catalogue_fields()[i, , drop = FALSE]

          list(
            node_id = field$node_id[[1L]],
            field_id = field$field_id[[1L]],
            table_name = field$table_name[[1L]],
            column_name = field$column_name[[1L]]
          )
        })
      )
    )
  })

  observeEvent(input$catalog_table_click, {
    event <- input$catalog_table_click
    tables <- displayed_tables()

    if (!is.list(event)) {
      return()
    }

    node_id <- event$node_id
    table_name <- event$table_name
    allowed_ids <- paste0("t", seq_along(tables))

    if (
      !is.character(node_id) ||
        length(node_id) != 1L ||
        is.na(node_id) ||
        !is.character(table_name) ||
        length(table_name) != 1L ||
        is.na(table_name)
    ) {
      return()
    }

    position <- match(node_id, allowed_ids)

    if (
      is.na(position) ||
        !identical(table_name, tables[[position]])
    ) {
      return()
    }

    if (!identical(inspected_table(), table_name)) {
      inspected_variable(NULL)
      inspected_table(table_name)
    }
  })

  output$catalog_documentation_table <- renderText({
    selected <- inspected_table()

    if (is.null(selected)) {
      return(
        "Select a table header in the diagram to inspect its documentation."
      )
    }

    selected
  })

  table_documentation <- reactive({
    selected <- inspected_table()
    req(!is.null(selected))

    schema <- catalog_schema_data()
    req(selected %in% displayed_tables())

    dictionary <- tryCatch(
      catalog_get_table_metadata(con, selected),
      error = function(e) {
        message("Table metadata failed: ", conditionMessage(e))
        NULL
      }
    )

    validate(need(
      !is.null(dictionary),
      "The table documentation could not be loaded."
    ))

    fields <- schema$columns[
      schema$columns$table_name == selected,
      ,
      drop = FALSE
    ]
    fields <- fields[order(fields$cid), , drop = FALSE]

    positions <- match(fields$column_name, dictionary$variable)

    descriptions <- vapply(
      seq_len(nrow(fields)),
      function(i) {
        position <- positions[[i]]

        if (is.na(position)) {
          return(
            "No metadata dictionary entry is available for this field."
          )
        }

        display_metadata(dictionary$description[[position]])
      },
      character(1)
    )

    list(
      table_name = selected,
      variables = fields$column_name,
      dictionary = dictionary,
      shown = data.frame(
        Variable = fields$column_name,
        Description = descriptions,
        check.names = FALSE
      )
    )
  })

  output$catalog_metadata_table <- DT::renderDT(
    {
      req(!is.null(inspected_table()))
      documentation <- table_documentation()

      DT::datatable(
        documentation$shown,
        rownames = FALSE,
        escape = TRUE,
        selection = list(
          mode = "single",
          target = "row",
          selected = integer(0)
        ),
        options = list(
          dom = "t",
          paging = FALSE,
          searching = FALSE,
          ordering = FALSE,
          autoWidth = FALSE,
          columnDefs = list(
            list(
              width = "30%",
              targets = 0,
              className = "dt-left"
            ),
            list(
              width = "70%",
              targets = 1,
              className = "dt-left"
            )
          )
        )
      )
    },
    server = FALSE
  )

  observeEvent(
    input$catalog_metadata_table_rows_selected,
    {
      rows <- input$catalog_metadata_table_rows_selected
      req(!is.null(inspected_table()))

      documentation <- table_documentation()

      if (
        length(rows) != 1L ||
          is.na(rows) ||
          rows < 1L ||
          rows > length(documentation$variables)
      ) {
        inspected_variable(NULL)
        return()
      }

      inspected_variable(list(
        table_name = documentation$table_name,
        variable = documentation$variables[[rows]]
      ))
    },
    ignoreNULL = FALSE
  )

  output$catalog_variable_details <- renderUI({
    selected <- inspected_variable()

    if (
      is.null(inspected_table()) ||
        is.null(selected) ||
        !identical(selected$table_name, inspected_table())
    ) {
      return(p("Select a variable row to inspect its details."))
    }

    documentation <- table_documentation()
    dictionary <- documentation$dictionary
    position <- match(selected$variable, dictionary$variable)

    if (is.na(position)) {
      return(tagList(
        h4(selected$variable),
        p("No metadata dictionary entry is available for this field.")
      ))
    }

    row <- dictionary[position, , drop = FALSE]

    attributes <- c(
      data_type = "Documented data type",
      possible_values = "Possible values",
      missing_value_interpretation = "Missing-value interpretation",
      notes = "Notes"
    )

    tagList(
      h4(selected$variable),
      tags$dl(
        lapply(names(attributes), function(attribute) {
          tagList(
            tags$dt(attributes[[attribute]]),
            tags$dd(
              style = paste(
                "white-space:pre-wrap;",
                "margin-left:0;margin-bottom:12px;"
              ),
              display_metadata(row[[attribute]][[1L]])
            )
          )
        })
      )
    )
  })
}
