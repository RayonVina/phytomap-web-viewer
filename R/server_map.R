# Map rectangle selection and PNG export helpers.
phytomap_rectangle_ids <- function(data, feature) {
  if (is.null(data) || !nrow(data)) {
    return(numeric(0))
  }
  if (
    is.null(feature$geometry) || !identical(feature$geometry$type, "Polygon")
  ) {
    stop("A rectangular polygon was expected.", call. = FALSE)
  }
  ring <- feature$geometry$coordinates[[1L]]
  if (is.data.frame(ring)) {
    ring <- as.matrix(ring)
  }
  if (is.matrix(ring)) {
    if (ncol(ring) != 2L) {
      stop("Unexpected rectangle coordinate dimensions.", call. = FALSE)
    }
    vertices <- ring
    storage.mode(vertices) <- "double"
  } else {
    values <- as.numeric(unlist(ring, use.names = FALSE))
    if (length(values) < 8L || length(values) %% 2L != 0L) {
      stop("Invalid rectangle coordinates.", call. = FALSE)
    }
    vertices <- matrix(values, ncol = 2L, byrow = TRUE)
  }
  if (!all(is.finite(vertices))) {
    stop("Non-finite rectangle coordinates.", call. = FALSE)
  }
  longitude <- range(vertices[, 1L])
  latitude <- range(vertices[, 2L])
  tolerance <- 1e-9
  valid <- is.finite(data$longitude) &
    is.finite(data$latitude) &
    !is.na(data$coordinate_entry_id)
  # Test whether any longitude + 360*k lies inside the actual drawn interval.
  first_copy <- ceiling((longitude[[1L]] - data$longitude - tolerance) / 360)
  last_copy <- floor((longitude[[2L]] - data$longitude + tolerance) / 360)
  inside <- valid &
    first_copy <= last_copy &
    data$latitude >= latitude[[1L]] - tolerance &
    data$latitude <= latitude[[2L]] + tolerance
  unique(data$coordinate_entry_id[which(inside)])
}

phytomap_add_png_control <- function(map, basemap = "Street") {
  leaflet.extras2::addEasyprint(
    map,
    options = leaflet.extras2::easyprintOptions(
      title = "Map image export",
      position = "bottomleft",

      sizeModes = list(
        list(
          scale = 1,
          name = "Download current view",
          className = "ViewportPNG"
        )
      ),

      exportOnly = TRUE,
      tileLayer = basemap,
      tileWait = 1500,
      filename = "phytomap-map",

      hideControlContainer = FALSE,

      hideClasses = c(
        "leaflet-control-zoom",
        "leaflet-control-layers",
        "leaflet-draw",
        "leaflet-control-easyPrint"
      )
    )
  )
}

# Pure selection logic: the master row acts on the complete list, not search results.
reconcile_map_checks <- function(
  current,
  universe,
  visible,
  incoming,
  expected,
  master
) {
  normalize <- function(x) sort(unique(as.character(x)))
  universe <- normalize(universe)
  current <- intersect(normalize(current), universe)
  visible <- intersect(normalize(visible), universe)
  incoming <- normalize(incoming)
  expected <- normalize(expected)
  if (identical(incoming, expected)) {
    return(current)
  }
  had_master <- master %in% expected
  has_master <- master %in% incoming
  if (!had_master && has_master) {
    return(universe)
  }
  incoming_real <- intersect(incoming, visible)
  if (
    had_master &&
      !has_master &&
      setequal(incoming_real, intersect(current, visible))
  ) {
    return(character(0))
  }
  normalize(union(setdiff(current, visible), incoming_real))
}

get_cached_global_coverage <- local({
  cached <- NULL
  function(con) {
    if (is.null(cached)) {
      started <- proc.time()[["elapsed"]]
      data <- get_map_coverage(con)
      cached <<- list(
        data = data,
        counted_at = Sys.time(),
        query_seconds = proc.time()[["elapsed"]] - started
      )
    }
    cached
  }
})


# Register all existing Map explorer functionality for one session.
register_map_server <- function(input, output, session, con) {
  source_master <- "__ALL_SOURCES__"
  subsource_master <- "__ALL_SUBSOURCES__"
  catalogue <- reactiveVal(NULL)
  groups <- reactiveVal(NULL)
  parents <- reactiveVal(character(0))
  children <- reactiveVal(character(0))
  source_view <- reactiveVal(list(
    visible = character(0),
    expected = character(0)
  ))
  subsource_view <- reactiveVal(list(
    visible = character(0),
    expected = character(0)
  ))
  initialized <- reactiveVal(FALSE)
  points <- reactiveVal(NULL)
  applied <- reactiveVal(NULL)
  status <- reactiveVal("Loading global coverage when the map opens.")
  details <- reactiveVal("")
  notice <- reactiveVal("")
  fmt <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
  normalize <- function(x) sort(unique(as.character(x)))
  available_for <- function(keys) {
    data <- catalogue()
    if (is.null(data)) {
      return(character(0))
    }
    as.character(data$source_id[data$source_key %in% keys])
  }
  set_parents <- function(keys, reset_all = FALSE) {
    g <- groups()
    if (is.null(g)) {
      return(invisible(NULL))
    }
    keys <- intersect(normalize(keys), g$source_key)
    old_available <- available_for(parents())
    new_available <- available_for(keys)
    if (
      reset_all || (setequal(keys, g$source_key) && !setequal(keys, parents()))
    ) {
      next_children <- new_available
    } else {
      next_children <- union(
        intersect(children(), new_available),
        setdiff(new_available, old_available)
      )
    }
    parents(keys)
    children(normalize(next_children))
    invisible(NULL)
  }

  # Stable group inputs are updated in place; no per-row observer reconstruction.
  observe({
    g <- groups()
    if (is.null(g)) {
      return()
    }
    search <- input$map_source_search
    if (is.null(search)) {
      search <- ""
    }
    shown <- g[
      grepl(tolower(search), tolower(g$source), fixed = TRUE),
      ,
      drop = FALSE
    ]
    chosen <- parents()
    all_selected <- nrow(g) > 0L && setequal(chosen, g$source_key)
    choices <- c(
      stats::setNames(source_master, "All sources"),
      stats::setNames(shown$source_key, shown$source)
    )
    selected <- c(
      if (all_selected) source_master else character(0),
      intersect(chosen, shown$source_key)
    )
    source_view(list(
      visible = shown$source_key,
      expected = normalize(selected)
    ))
    freezeReactiveValue(input, "map_sources")
    updateCheckboxGroupInput(
      session,
      "map_sources",
      choices = choices,
      selected = selected
    )
  })
  observeEvent(
    input$map_sources,
    {
      if (is.null(groups())) {
        return()
      }
      view <- source_view()
      next_keys <- reconcile_map_checks(
        parents(),
        groups()$source_key,
        view$visible,
        input$map_sources,
        view$expected,
        source_master
      )
      if (!setequal(next_keys, parents())) {
        set_parents(next_keys)
        notice("")
      }
    },
    ignoreNULL = FALSE,
    priority = 10
  )

  observe({
    data <- catalogue()
    if (is.null(data)) {
      return()
    }
    eligible <- data[data$source_key %in% parents(), , drop = FALSE]
    universe <- as.character(eligible$source_id)
    search <- input$map_subsource_search
    if (is.null(search)) {
      search <- ""
    }
    shown <- eligible[
      grepl(tolower(search), tolower(eligible$display_label), fixed = TRUE),
      ,
      drop = FALSE
    ]
    visible <- as.character(shown$source_id)
    chosen <- intersect(children(), universe)
    all_selected <- length(universe) > 0L && setequal(chosen, universe)
    choices <- if (length(universe)) {
      c(
        stats::setNames(subsource_master, "All subsources"),
        stats::setNames(visible, shown$display_label)
      )
    } else {
      character(0)
    }
    selected <- c(
      if (all_selected) subsource_master else character(0),
      intersect(chosen, visible)
    )
    subsource_view(list(visible = visible, expected = normalize(selected)))
    freezeReactiveValue(input, "map_subsources")
    updateCheckboxGroupInput(
      session,
      "map_subsources",
      choices = choices,
      selected = selected
    )
  })
  observeEvent(
    input$map_subsources,
    {
      if (is.null(catalogue())) {
        return()
      }
      view <- subsource_view()
      next_ids <- reconcile_map_checks(
        children(),
        available_for(parents()),
        view$visible,
        input$map_subsources,
        view$expected,
        subsource_master
      )
      if (!setequal(next_ids, children())) {
        children(next_ids)
        notice("")
      }
    },
    ignoreNULL = FALSE
  )

  selection <- reactive({
    g <- groups()
    data <- catalogue()
    if (is.null(g) || is.null(data)) {
      return(list(valid = FALSE, message = "Source choices are unavailable."))
    }
    if (!length(parents())) {
      return(list(valid = FALSE, message = "Select at least one source."))
    }
    ids <- intersect(children(), available_for(parents()))
    if (!length(ids)) {
      return(list(valid = FALSE, message = "Select at least one subsource."))
    }
    all_selected <- setequal(ids, as.character(data$source_id)) &&
      setequal(parents(), g$source_key)
    values <- if (all_selected) NULL else sort(as.numeric(ids))
    used <- data$source_key[as.character(data$source_id) %in% ids]
    names <- g$source[match(unique(used), g$source_key)]
    label <- if (all_selected) {
      "All sources and subsources"
    } else {
      paste0(
        if (length(names) <= 3L) {
          paste(names, collapse = ", ")
        } else {
          paste(length(names), "sources")
        },
        " | ",
        length(ids),
        " source/subsource entries"
      )
    }
    list(
      valid = TRUE,
      ids = values,
      key = if (all_selected) "ALL" else paste(values, collapse = ","),
      label = label
    )
  })
  output$source_count <- renderText({
    g <- groups()
    if (is.null(g)) {
      "Loading..."
    } else {
      paste(length(parents()), "/", nrow(g), "selected")
    }
  })
  output$subsource_count <- renderText(paste(
    length(children()),
    "/",
    length(available_for(parents())),
    "selected"
  ))
  output$map_status <- renderText(status())
  output$map_details <- renderText(details())
  output$map_pending_status <- renderText({
    if (nzchar(notice())) {
      return(notice())
    }
    if (!initialized()) {
      return("")
    }
    current <- selection()
    if (!isTRUE(current$valid)) {
      return(current$message)
    }
    if (!is.null(applied()) && !identical(current$key, applied()$key)) {
      "Unapplied filter changes."
    } else {
      ""
    }
  })

  output$coverage_map <- leaflet::renderLeaflet({
    basemaps <- c(
      "Street" = "OpenStreetMap.Mapnik",
      "Satellite" = "Esri.WorldImagery"
    )
    map <- leaflet::leaflet(
      options = leaflet::leafletOptions(
        minZoom = 1,
        worldCopyJump = FALSE,
        preferCanvas = TRUE
      )
    )
    for (name in names(basemaps)) {
      map <- leaflet::addProviderTiles(
        map,
        provider = basemaps[[name]],
        group = name,
        options = leaflet::providerTileOptions(
          noWrap = FALSE,
          crossOrigin = TRUE
        )
      )
    }
    map <- leaflet::addLayersControl(
      map,
      baseGroups = names(basemaps),
      position = "topright",
      options = leaflet::layersControlOptions(collapsed = TRUE)
    )
    map <- leaflet::hideGroup(map, "Satellite")
    map <- leaflet.extras::addDrawToolbar(
      map,
      targetGroup = "selection_rectangle",
      position = "topleft",
      polylineOptions = FALSE,
      polygonOptions = FALSE,
      circleOptions = FALSE,
      markerOptions = FALSE,
      circleMarkerOptions = FALSE,
      rectangleOptions = leaflet.extras::drawRectangleOptions(
        showArea = FALSE,
        repeatMode = FALSE,
        shapeOptions = leaflet.extras::drawShapeOptions(
          color = "#d99300",
          weight = 2,
          opacity = 0.9,
          fillOpacity = 0.08
        )
      ),
      editOptions = FALSE,
      singleFeature = TRUE,
      toolbar = leaflet.extras::toolbarOptions(
        buttons = list(rectangle = "Select locations by rectangle")
      )
    )
    map <- phytomap_add_png_control(map, basemap = "Street")
    leaflet::setView(map, lng = 0, lat = 20, zoom = 2)
  })
  active_basemap <- reactive({
    visible_groups <- input$coverage_map_groups

    basemap <- intersect(
      c("Street", "Satellite"),
      visible_groups
    )

    if (length(basemap) == 1L) {
      basemap[[1L]]
    } else {
      "Street"
    }
  })

  draw_visible <- function() {
    data <- points()
    bounds <- input$coverage_map_bounds
    if (is.null(data) || is.null(bounds)) {
      return(invisible(NULL))
    }
    west <- as.numeric(bounds$west)
    east <- as.numeric(bounds$east)
    if (
      length(west) != 1L ||
        length(east) != 1L ||
        !is.finite(west) ||
        !is.finite(east)
    ) {
      return(invisible(NULL))
    }
    if (east < west) {
      east <- east + 360
    }
    padding <- max(1, min(10, (east - west) * 0.05))
    west <- west - padding
    east <- east + padding
    display <- NULL
    if (nrow(data)) {
      first <- ceiling((west - max(data$longitude)) / 360)
      last <- floor((east - min(data$longitude)) / 360)
      if (first <= last) {
        copies <- lapply(seq.int(first, last), function(k) {
          lng <- data$longitude + 360 * k
          keep <- which(lng >= west & lng <= east)
          if (!length(keep)) {
            return(NULL)
          }
          part <- data[keep, , drop = FALSE]
          part$display_longitude <- lng[keep]
          part$render_id <- paste(part$coordinate_entry_id, k, sep = ":")
          part
        })
        copies <- Filter(Negate(is.null), copies)
        if (length(copies)) display <- do.call(rbind, copies)
      }
    }
    map <- leaflet::leafletProxy("coverage_map", session = session)
    map <- leaflet::clearGroup(map, "sampling_coverage")
    if (!is.null(display) && nrow(display)) {
      leaflet::addCircleMarkers(
        map,
        lng = display$display_longitude,
        lat = display$latitude,
        layerId = display$render_id,
        group = "sampling_coverage",
        radius = 4,
        stroke = TRUE,
        color = "#202020",
        weight = 0.8,
        opacity = 0.5,
        fill = TRUE,
        fillColor = if (identical(active_basemap(), "Satellite")) {
          "#eeeeee"
        } else {
          "#777777"
        },
        fillOpacity = 0.66
      )
    }

    draw_point_highlights()

    invisible(NULL)
  }
  observeEvent(
    list(
      points(),
      input$coverage_map_bounds,
      active_basemap()
    ),
    draw_visible(),
    ignoreNULL = FALSE
  )

  load_coverage <- function(chosen) {
    notice("")
    if (!isTRUE(chosen$valid)) {
      notice(chosen$message)
      return(invisible(NULL))
    }
    if (is.null(con)) {
      notice("The database is unavailable.")
      return(invisible(NULL))
    }
    result <- withProgress(message = "Loading coverage", value = 0.1, {
      value <- tryCatch(
        {
          if (is.null(chosen$ids)) {
            get_cached_global_coverage(con)
          } else {
            started <- proc.time()[["elapsed"]]
            data <- get_map_coverage(con, chosen$ids)
            list(
              data = data,
              counted_at = Sys.time(),
              query_seconds = proc.time()[["elapsed"]] - started
            )
          }
        },
        error = function(e) {
          message("Coverage failed: ", conditionMessage(e))
          NULL
        }
      )
      incProgress(0.8)
      value
    })
    if (is.null(result)) {
      notice("Coverage could not be loaded. The previous map is unchanged.")
      return(invisible(NULL))
    }
    data <- result$data
    valid <- with(
      data,
      !is.na(coordinate_entry_id) &
        is.finite(latitude) &
        is.finite(longitude) &
        latitude >= -90 &
        latitude <= 90 &
        longitude >= -180 &
        longitude <= 180
    )
    mapped <- data[valid, , drop = FALSE]
    total <- sum(data$n_events)
    included <- sum(mapped$n_events)
    leaflet::clearPopups(leaflet::leafletProxy(
      "coverage_map",
      session = session
    ))
    # Clear details even when the same filter is applied again or reset to ALL.
    selected_point(NULL)
    event_page(0L)
    record_page(0L)
    updateSelectInput(
      session,
      "point_event_id",
      choices = character(0),
      selected = character(0)
    )
    updateSelectInput(
      session,
      "point_sample_id",
      choices = character(0),
      selected = character(0)
    )
    updateSelectInput(
      session,
      "point_record_id",
      choices = character(0),
      selected = character(0)
    )
    applied(chosen)
    points(mapped)
    clear_record_selection()
    status(paste0(
      chosen$label,
      " - ",
      fmt(included),
      " events; ",
      fmt(nrow(mapped)),
      " coordinate entries; ",
      fmt(total - included),
      " excluded events."
    ))
    details(paste0(
      "Events found: ",
      fmt(total),
      ". Query: ",
      sprintf("%.3f", result$query_seconds),
      " seconds. Calculated: ",
      format(result$counted_at, "%Y-%m-%d %H:%M:%S %Z"),
      ". Exclusions reflect missing coordinates or geographic range checks. No result was truncated."
    ))
    map <- leaflet::leafletProxy("coverage_map", session = session)
    if (is.null(chosen$ids)) {
      leaflet::setView(map, lng = 0, lat = 20, zoom = 2)
    } else if (nrow(mapped)) {
      lng <- range(mapped$longitude)
      lat <- range(mapped$latitude)
      if (diff(lng) == 0 && diff(lat) == 0) {
        leaflet::setView(map, lng[[1]], lat[[1]], zoom = 6)
      } else {
        leaflet::fitBounds(map, lng[[1]], lat[[1]], lng[[2]], lat[[2]])
      }
    }
    invisible(NULL)
  }

  observeEvent(input$coverage_map_bounds, {
    if (initialized()) {
      return()
    }
    initialized(TRUE)
    data <- if (is.null(con)) {
      NULL
    } else {
      tryCatch(get_sources(con), error = function(e) {
        message("Catalogue failed: ", conditionMessage(e))
        NULL
      })
    }
    if (!is.null(data)) {
      names <- unique(data$source)
      g <- data.frame(
        source_key = paste0("g", seq_along(names)),
        source = names,
        stringsAsFactors = FALSE
      )
      data$source_key <- g$source_key[match(data$source, g$source)]
      label <- data$subsource
      label[is.na(label)] <- "[No subsource recorded]"
      label[
        !is.na(data$subsource) & data$subsource == ""
      ] <- "[Empty subsource]"
      data$display_label <- paste(data$source, label, sep = " / ")
      catalogue(data)
      groups(g)
      set_parents(g$source_key, reset_all = TRUE)
    }
    load_coverage(list(
      valid = TRUE,
      ids = NULL,
      key = "ALL",
      label = "All sources and subsources"
    ))
    if (is.null(data)) {
      notice("Source choices could not be loaded. See the server log.")
    }
  })
  observeEvent(input$apply_map_filters, {
    if (!initialized()) {
      notice("Please wait for the map.")
      return()
    }
    load_coverage(selection())
  })
  observeEvent(input$reset_map_filters, {
    if (is.null(groups())) {
      notice("Source choices are unavailable.")
      return()
    }
    set_parents(groups()$source_key, reset_all = TRUE)
    updateTextInput(session, "map_source_search", value = "")
    updateTextInput(session, "map_subsource_search", value = "")
    load_coverage(list(
      valid = TRUE,
      ids = NULL,
      key = "ALL",
      label = "All sources and subsources"
    ))
  })

  # Circle-marker clicks use the marker event category.
  # Point-detail queries follow the applied filters, not draft selections.

  selected_point <- reactiveVal(NULL)
  event_page <- reactiveVal(0L)
  record_page <- reactiveVal(0L)
  # Successful map loads explicitly reset the detail panel in load_coverage().

  observeEvent(input$coverage_map_marker_click, {
    click <- input$coverage_map_marker_click
    data <- points()
    if (is.null(click$id) || is.null(data)) {
      return()
    }
    id <- strsplit(as.character(click$id), ":", fixed = TRUE)[[1]][[1]]
    position <- match(id, as.character(data$coordinate_entry_id))
    if (is.na(position)) {
      return()
    }
    event_page(0L)
    record_page(0L)
    selected_point(data[position, , drop = FALSE])
    leaflet::clearPopups(leaflet::leafletProxy(
      "coverage_map",
      session = session
    ))
  })

  display_value <- function(x) {
    if (is.numeric(x)) {
      result <- format(x, digits = 17, trim = TRUE)
    } else {
      result <- as.character(x)
    }
    result[is.na(x)] <- "Not recorded (SQL NULL)"
    result[!is.na(x) & result == ""] <- "[Empty string]"
    result
  }
  values_table <- function(row, labels = names(row)) {
    if (is.null(row) || !nrow(row)) {
      return(NULL)
    }
    data.frame(
      Field = labels,
      Value = vapply(row, function(x) display_value(x[[1]]), character(1)),
      check.names = FALSE
    )
  }
  safe_detail <- function(code) {
    value <- tryCatch(force(code), error = function(e) {
      message("Point details failed: ", conditionMessage(e))
      NULL
    })
    validate(need(!is.null(value), "These details could not be loaded."))
    value
  }
  point_id <- reactive({
    req(selected_point())
    selected_point()$coordinate_entry_id[[1]]
  })
  date_label <- function(row) {
    year <- display_value(row$year[[1]])
    precision <- row$date_precision[[1]]
    date <- if (!is.na(precision) && precision == "month") {
      paste(year, sprintf("%02d", row$month[[1]]), sep = "-")
    } else if (!is.na(precision) && precision %in% c("day", "timestamp")) {
      paste(
        year,
        sprintf("%02d", row$month[[1]]),
        sprintf("%02d", row$day[[1]]),
        sep = "-"
      )
    } else {
      paste0(
        "Stored components: ",
        year,
        "/",
        display_value(row$month[[1]]),
        "/",
        display_value(row$day[[1]])
      )
    }
    if (
      !is.na(precision) &&
        precision == "timestamp" &&
        !is.na(row$time_value[[1]])
    ) {
      date <- paste(date, row$time_value[[1]])
    }
    paste0(
      date,
      " | ",
      display_value(row$source[[1]]),
      " / ",
      display_value(row$subsource[[1]])
    )
  }

  output$point_prompt <- renderText({
    if (is.null(selected_point())) "Select a point on the map." else ""
  })
  output$point_location <- renderTable(
    {
      if (is.null(selected_point())) {
        return(NULL)
      }
      values_table(
        selected_point()[,
          c("latitude", "longitude", "coord_precision", "n_events"),
          drop = FALSE
        ],
        c("Latitude", "Longitude", "Coordinate precision", "Events")
      )
    },
    rownames = FALSE
  )
  output$point_provenance <- renderTable(
    {
      if (is.null(selected_point())) {
        return(NULL)
      }
      data <- safe_detail(get_point_sources(con, point_id(), applied()$ids))
      data.frame(
        Source = display_value(data$source),
        Subsource = display_value(data$subsource),
        Events = display_value(data$n_events)
      )
    },
    rownames = FALSE
  )

  point_events <- reactive({
    req(selected_point())
    data <- safe_detail(get_point_events(
      con,
      point_id(),
      applied()$ids,
      event_page() * 100L
    ))
    list(data = utils::head(data, 100L), more = nrow(data) > 100L)
  })
  observeEvent(point_events(), {
    data <- point_events()$data
    labels <- if (nrow(data)) {
      vapply(
        seq_len(nrow(data)),
        function(i) date_label(data[i, , drop = FALSE]),
        character(1)
      )
    } else {
      character(0)
    }
    updateSelectInput(
      session,
      "point_event_id",
      choices = stats::setNames(as.character(data$event_id), labels),
      selected = if (nrow(data)) {
        as.character(data$event_id[[1]])
      } else {
        character(0)
      }
    )
  })
  event_row <- reactive({
    data <- point_events()$data
    pos <- match(input$point_event_id, as.character(data$event_id))
    req(length(pos) == 1L, !is.na(pos))
    data[pos, , drop = FALSE]
  })
  observeEvent(input$point_events_prev, {
    if (event_page() > 0L) event_page(event_page() - 1L)
  })
  observeEvent(input$point_events_next, {
    if (isTRUE(point_events()$more)) event_page(event_page() + 1L)
  })
  output$point_event_page <- renderText({
    if (is.null(selected_point())) {
      return("")
    }
    paste(
      "Event page",
      event_page() + 1L,
      if (point_events()$more) "- more available" else "- last page"
    )
  })
  output$point_event_metadata <- renderTable(
    {
      if (is.null(selected_point())) {
        return(NULL)
      }
      row <- event_row()
      values_table(data.frame(
        Date = date_label(row),
        Date_precision = row$date_precision,
        Stored_day = row$day,
        Day_is_imputed = row$day_is_imputed,
        Stored_time = row$time_value,
        Time_reference = "Source clock time; not harmonized to UTC. Assigned times may be placeholders.",
        Notes = row$datetime_notes,
        check.names = FALSE
      ))
    },
    rownames = FALSE
  )

  event_samples <- reactive({
    req(event_row())
    safe_detail(get_event_samples(
      con,
      event_row()$event_id[[1]],
      point_id(),
      applied()$ids
    ))
  })
  observeEvent(event_samples(), {
    data <- event_samples()
    labels <- paste0(
      "Sample ",
      seq_len(nrow(data)),
      " | ",
      display_value(data$depth),
      " m | ",
      display_value(data$depth_label)
    )
    updateSelectInput(
      session,
      "point_sample_id",
      choices = stats::setNames(as.character(data$sample_id), labels),
      selected = if (nrow(data)) {
        as.character(data$sample_id[[1]])
      } else {
        character(0)
      }
    )
    record_page(0L)
  })
  sample_row <- reactive({
    data <- event_samples()
    pos <- match(input$point_sample_id, as.character(data$sample_id))
    req(length(pos) == 1L, !is.na(pos))
    data[pos, , drop = FALSE]
  })
  observeEvent(input$point_sample_id, record_page(0L), ignoreInit = TRUE)
  output$point_sample_metadata <- renderTable(
    {
      if (is.null(selected_point())) {
        return(NULL)
      }
      row <- sample_row()
      values_table(
        row[,
          c(
            "depth",
            "depth_label",
            "depth_label_min",
            "depth_label_max",
            "sample_notes",
            "depth_notes"
          ),
          drop = FALSE
        ],
        c(
          "Depth (m)",
          "Depth qualifier",
          "Minimum depth (m)",
          "Maximum depth (m)",
          "Sample notes",
          "Depth notes"
        )
      )
    },
    rownames = FALSE
  )

  sample_records <- reactive({
    req(sample_row())
    data <- safe_detail(get_sample_records(
      con,
      sample_row()$sample_id[[1]],
      point_id(),
      applied()$ids,
      record_page() * 50L
    ))
    list(data = utils::head(data, 50L), more = nrow(data) > 50L)
  })
  observeEvent(sample_records(), {
    data <- sample_records()$data
    labels <- paste0(seq_len(nrow(data)), ". ", display_value(data$taxon_raw))
    updateSelectInput(
      session,
      "point_record_id",
      choices = stats::setNames(as.character(data$abundance_id), labels),
      selected = if (nrow(data)) {
        as.character(data$abundance_id[[1]])
      } else {
        character(0)
      }
    )
  })
  output$point_records <- renderTable(
    {
      if (is.null(selected_point())) {
        return(NULL)
      }
      data <- sample_records()$data
      data.frame(
        Taxon = display_value(data$taxon_raw),
        `Abundance (cells/L)` = display_value(data$abundance_cells_per_litre),
        Rank = display_value(data$taxonomic_rank),
        check.names = FALSE
      )
    },
    rownames = FALSE
  )
  observeEvent(input$point_records_prev, {
    if (record_page() > 0L) record_page(record_page() - 1L)
  })
  observeEvent(input$point_records_next, {
    if (isTRUE(sample_records()$more)) record_page(record_page() + 1L)
  })
  output$point_record_page <- renderText({
    if (is.null(selected_point())) {
      return("")
    }
    paste(
      "Record page",
      record_page() + 1L,
      if (sample_records()$more) "- more available" else "- last page"
    )
  })
  record_details <- reactive({
    data <- sample_records()$data
    pos <- match(input$point_record_id, as.character(data$abundance_id))
    req(length(pos) == 1L, !is.na(pos))
    safe_detail(get_abundance_details(
      con,
      data$abundance_id[[pos]],
      sample_row()$sample_id[[1]],
      point_id(),
      applied()$ids
    ))
  })
  output$point_abundance_metadata <- renderTable(
    {
      if (is.null(selected_point())) {
        return(NULL)
      }
      row <- record_details()
      values_table(
        row[, c("a_abundance", "a_notes"), drop = FALSE],
        c("Abundance (cells/L)", "Record notes")
      )
    },
    rownames = FALSE
  )
  output$point_taxonomy <- renderTable(
    {
      if (is.null(selected_point())) {
        return(NULL)
      }
      row <- record_details()
      columns <- names(row)[grepl("^(tr_|tx_)", names(row))]
      labels <- gsub(
        "_",
        " ",
        sub(
          "^tr_",
          "Identification: ",
          sub("^tx_", "Classification: ", columns)
        )
      )
      values_table(row[, columns, drop = FALSE], labels)
    },
    rownames = FALSE
  )
  output$point_protocol <- renderTable(
    {
      if (is.null(selected_point())) {
        return(NULL)
      }
      row <- record_details()
      columns <- names(row)[startsWith(names(row), "sp_")]
      labels <- gsub("_", " ", sub("^sp_", "", columns))
      labels[
        columns %in%
          c("sp_volume_sampled", "sp_volume_concentrated", "sp_volume_counted")
      ] <-
        paste0(
          labels[
            columns %in%
              c(
                "sp_volume_sampled",
                "sp_volume_concentrated",
                "sp_volume_counted"
              )
          ],
          " (mL)"
        )
      values_table(row[, columns, drop = FALSE], labels)
    },
    rownames = FALSE
  )

  # Spatially selected locations and the loaded table are separate snapshots.
  sr_selected <- reactiveVal(numeric(0))
  sr_loaded <- reactiveVal(NULL)
  sr_cursors <- reactiveVal(list(NULL))
  sr_notice <- reactiveVal("")
  clear_record_selection <- function() {
    sr_selected(numeric(0))
    sr_loaded(NULL)
    sr_cursors(list(NULL))
    sr_notice("")
  }
  output$sr_point_actions <- renderUI({
    point <- selected_point()
    if (is.null(point)) {
      return(NULL)
    }
    included <- point$coordinate_entry_id[[1]] %in% sr_selected()
    div(
      style = "margin:8px 0 12px;",
      actionButton(
        "sr_toggle",
        if (included) "Remove from selection" else "Add to selection",
        class = "btn-primary"
      )
    )
  })
  observeEvent(input$sr_toggle, {
    req(selected_point())
    id <- selected_point()$coordinate_entry_id[[1]]
    current <- sr_selected()
    if (id %in% current) {
      sr_selected(setdiff(current, id))
    } else {
      if (length(current) >= 200L) {
        sr_notice("Selection limit: 200 locations.")
        return()
      }
      sr_selected(sort(unique(c(current, id))))
    }
    sr_notice("")
  })
  observeEvent(input$sr_clear, clear_record_selection())
  output$sr_selection_count <- renderText(paste(
    "Selected locations:",
    length(sr_selected())
  ))

  # Draw inspection and selection rings above the coverage markers.
  draw_point_highlights <- function() {
    bounds <- input$coverage_map_bounds
    data <- points()

    if (is.null(bounds) || is.null(data)) {
      return(invisible(NULL))
    }

    west <- as.numeric(bounds$west)
    east <- as.numeric(bounds$east)

    if (
      length(west) != 1L ||
        length(east) != 1L ||
        !is.finite(west) ||
        !is.finite(east)
    ) {
      return(invisible(NULL))
    }

    if (east < west) {
      east <- east + 360
    }

    padding <- max(1, min(10, (east - west) * 0.05))
    west <- west - padding
    east <- east + padding

    map <- leaflet::leafletProxy(
      "coverage_map",
      session = session
    )

    map <- leaflet::clearGroup(
      map,
      "record_selection"
    )

    map <- leaflet::clearGroup(
      map,
      "point_inspection"
    )

    if (!nrow(data)) {
      return(invisible(NULL))
    }

    draw_rings <- function(
      map,
      locations,
      group,
      radius,
      weight,
      dashed = FALSE
    ) {
      if (!nrow(locations)) {
        return(map)
      }

      first <- ceiling(
        (west - max(locations$longitude)) / 360
      )

      last <- floor(
        (east - min(locations$longitude)) / 360
      )

      if (first > last) {
        return(map)
      }

      for (k in seq.int(first, last)) {
        longitude <- locations$longitude + 360 * k
        keep <- longitude >= west & longitude <= east

        if (!any(keep)) {
          next
        }

        map <- leaflet::addCircleMarkers(
          map,
          lng = longitude[keep],
          lat = locations$latitude[keep],
          layerId = paste(
            group,
            locations$coordinate_entry_id[keep],
            k,
            sep = ":"
          ),
          group = group,
          radius = radius,
          stroke = TRUE,
          color = "#d99300",
          weight = weight,
          opacity = 1,
          fill = FALSE,
          options = leaflet::pathOptions(
            interactive = FALSE,
            dashArray = if (dashed) "3,3" else NULL
          )
        )
      }

      map
    }

    selected_locations <- data[
      data$coordinate_entry_id %in% sr_selected(),
      ,
      drop = FALSE
    ]

    map <- draw_rings(
      map = map,
      locations = selected_locations,
      group = "record_selection",
      radius = 7,
      weight = 2.5
    )

    inspected <- selected_point()

    if (!is.null(inspected)) {
      inspected_locations <- data[
        data$coordinate_entry_id %in%
          inspected$coordinate_entry_id,
        ,
        drop = FALSE
      ]

      map <- draw_rings(
        map = map,
        locations = inspected_locations,
        group = "point_inspection",
        radius = 10,
        weight = 2,
        dashed = TRUE
      )
    }

    invisible(NULL)
  }

  # Update rings without redrawing all coverage points.
  observeEvent(
    list(sr_selected(), selected_point()),
    {
      draw_point_highlights()
    },
    ignoreNULL = FALSE
  )

  observeEvent(input$sr_load, {
    ids <- sr_selected()
    if (!length(ids)) {
      sr_notice("Select at least one location.")
      return()
    }
    req(applied())
    snapshot <- list(
      coordinates = ids,
      sources = applied()$ids,
      label = applied()$label
    )
    started <- proc.time()[["elapsed"]]
    profile <- withProgress(
      message = "Checking selected records",
      value = 0.1,
      {
        value <- tryCatch(
          sr_profile(con, snapshot$coordinates, snapshot$sources),
          error = function(e) {
            message("Selected-record profile failed: ", conditionMessage(e))
            NULL
          }
        )
        incProgress(0.8)
        value
      }
    )
    if (is.null(profile)) {
      sr_notice(
        "Selected records could not be loaded. The previous result is unchanged."
      )
      return()
    }
    snapshot$profile <- profile
    snapshot$seconds <- proc.time()[["elapsed"]] - started
    sr_cursors(list(NULL))
    sr_loaded(snapshot)
    sr_notice("")
  })
  sr_page_result <- reactive({
    loaded <- sr_loaded()
    req(loaded)
    result <- tryCatch(
      sr_page(
        con,
        loaded$coordinates,
        loaded$sources,
        utils::tail(sr_cursors(), 1)[[1]]
      ),
      error = function(e) {
        message("Selected-record page failed: ", conditionMessage(e))
        NULL
      }
    )
    validate(need(!is.null(result), "This page could not be loaded."))
    result
  })
  sr_visible <- reactive({
    loaded <- sr_loaded()
    req(loaded)
    core <- c(
      "abundance_id",
      "source",
      "subsource",
      "year",
      "month",
      "day",
      "date_precision",
      "day_is_imputed",
      "latitude",
      "longitude",
      "coord_precision",
      "depth",
      "depth_label",
      "depth_label_min",
      "depth_label_max",
      "taxon_raw",
      "taxon_accepted",
      "kingdom",
      "phylum",
      "class",
      "order",
      "family",
      "genus",
      "species",
      "rank",
      "abundance"
    )
    taxonomy <- c(
      "aphiaid",
      "taxon_clean",
      "taxon_matched",
      "aphiaid_matched",
      "aphiaid_accepted",
      "resolution_method",
      "uncertain",
      "ltr",
      "taxonomic_status"
    )
    protocol <- c(
      "enumeration_method",
      "sampling_method",
      "count_method",
      "concentration_method",
      "microscopy_method",
      "preservation",
      "volume_sampled",
      "volume_concentrated",
      "volume_counted"
    )
    chosen <- switch(
      input$sr_column_group,
      taxonomy = c(core, taxonomy),
      protocol = c(core, protocol),
      all = names(sr_fields()),
      core
    )
    available <- intersect(chosen, loaded$profile$available)
    if (!length(available)) "abundance_id" else available
  })
  output$sr_status <- renderText({
    if (nzchar(sr_notice())) {
      return(sr_notice())
    }
    loaded <- sr_loaded()
    if (is.null(loaded)) {
      return("No records loaded.")
    }
    pending <- if (!setequal(sr_selected(), loaded$coordinates)) {
      " Selection changed - reload records."
    } else {
      ""
    }
    paste0(
      format(loaded$profile$n_records, big.mark = ",", scientific = FALSE),
      " abundance records in the loaded selection. ",
      loaded$label,
      ". Profile query: ",
      sprintf("%.3f", loaded$seconds),
      " seconds.",
      pending
    )
  })
  output$sr_table <- DT::renderDT(
    {
      if (is.null(sr_loaded())) {
        return(
          DT::datatable(
            data.frame(),
            options = list(dom = "t")
          )
        )
      }

      data <- sr_page_result()$data[, sr_visible(), drop = FALSE]

      table <- DT::datatable(
        data,
        rownames = FALSE,
        escape = TRUE,
        options = list(
          dom = "t",
          paging = FALSE,
          searching = FALSE,
          ordering = TRUE,
          order = list(),
          scrollX = TRUE,
          autoWidth = TRUE,
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
  observeEvent(input$sr_next, {
    req(sr_loaded())
    page <- sr_page_result()
    if (isTRUE(page$more) && nrow(page$data)) {
      sr_cursors(c(sr_cursors(), list(utils::tail(page$data$abundance_id, 1))))
    }
  })
  observeEvent(input$sr_prev, {
    cursors <- sr_cursors()
    if (length(cursors) > 1L) sr_cursors(utils::head(cursors, -1L))
  })
  output$sr_page_status <- renderText({
    if (is.null(sr_loaded())) {
      return("")
    }
    page <- sr_page_result()
    paste(
      "Page",
      length(sr_cursors()),
      "-",
      nrow(page$data),
      "records",
      if (isTRUE(page$more)) "- more available" else "- last page"
    )
  })
  output$sr_download_control <- renderUI({
    loaded <- sr_loaded()
    if (is.null(loaded) || loaded$profile$n_records == 0) {
      return(NULL)
    }
    if (loaded$profile$n_records > 100000) {
      return(span(
        "CSV limit exceeded (100,000 records). Refine the selection."
      ))
    }
    downloadButton("sr_download", "Download CSV")
  })
  output$sr_download <- downloadHandler(
    filename = function() {
      paste0(
        "phytomap-selected-records-",
        format(Sys.time(), "%Y%m%d-%H%M%S"),
        ".csv"
      )
    },
    contentType = "text/csv; charset=utf-8",
    content = function(file) {
      loaded <- isolate(sr_loaded())
      req(loaded)
      if (loaded$profile$n_records > 100000) {
        stop("CSV limit exceeded. Refine the selection.", call. = FALSE)
      }
      columns <- if (identical(isolate(input$sr_csv_columns), "visible")) {
        isolate(sr_visible())
      } else {
        names(sr_fields())
      }
      tryCatch(
        {
          written <- sr_write_csv(
            con,
            file,
            loaded$coordinates,
            loaded$sources,
            columns
          )
          if (written != loaded$profile$n_records) {
            stop("CSV row count does not match the loaded selection.")
          }
        },
        error = function(e) {
          message("CSV export failed: ", conditionMessage(e))
          stop(
            "CSV export failed; no complete download is available. See the server log.",
            call. = FALSE
          )
        }
      )
    }
  )

  # Rectangle selection adds whole coordinate identities, never world-copy duplicates.
  observeEvent(input$coverage_map_draw_new_feature, {
    feature <- input$coverage_map_draw_new_feature
    leaflet::clearGroup(
      leaflet::leafletProxy("coverage_map", session = session),
      "selection_rectangle"
    )
    ids <- tryCatch(
      phytomap_rectangle_ids(points(), feature),
      error = function(e) {
        message("Rectangle selection failed: ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(ids)) {
      showNotification(
        "The selection rectangle could not be read.",
        type = "error",
        duration = 8
      )
      return()
    }
    if (!length(ids)) {
      showNotification(
        "No mapped locations inside this rectangle.",
        type = "message",
        duration = 5
      )
      return()
    }
    current <- sr_selected()
    combined <- sort(unique(c(current, ids)))
    if (length(combined) > 200L) {
      text <- paste0(
        "This would select ",
        length(combined),
        " locations; the limit is 200. Draw a smaller rectangle. Selection unchanged."
      )
      sr_notice(text)
      showNotification(text, type = "warning", duration = 10)
      return()
    }
    added <- length(setdiff(combined, current))
    sr_selected(combined)
    sr_notice("")
    showNotification(
      paste(added, "locations added;", length(combined), "selected in total."),
      type = "message",
      duration = 5
    )
  })

  # Keep the PNG control's tile-wait target aligned with the active basemap.
  png_basemap <- reactiveVal("Street")
  observeEvent(
    input$coverage_map_groups,
    {
      active <- intersect(c("Street", "Satellite"), input$coverage_map_groups)
      if (length(active) != 1L || identical(active[[1L]], png_basemap())) {
        return()
      }
      map <- leaflet::leafletProxy("coverage_map", session = session)
      map <- leaflet.extras2::removeEasyprint(map)
      phytomap_add_png_control(map, basemap = active[[1L]])
      png_basemap(active[[1L]])
    },
    ignoreNULL = TRUE
  )
}
