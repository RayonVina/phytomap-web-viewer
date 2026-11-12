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

# Read only compact summaries; never run full-data aggregation in the UI.
get_cached_home_plots <- local({
  cached <- NULL
  cached_key <- NULL

  function() {
    database_path <- file.path("data", "phytomap.sqlite3")
    summary_path <- file.path("data", "home_summary.rds")

    if (!file.exists(database_path) || !file.exists(summary_path)) {
      stop("The Home summary is unavailable.", call. = FALSE)
    }

    database_info <- file.info(database_path)
    summary_info <- file.info(summary_path)

    key <- c(
      database_info$size,
      as.numeric(database_info$mtime),
      summary_info$size,
      as.numeric(summary_info$mtime)
    )

    if (is.null(cached) || !identical(key, cached_key)) {
      result <- readRDS(summary_path)

      if (
        !identical(result$schema_version, 2L) ||
          !identical(result$database_size, unname(database_info$size)) ||
          !identical(
            result$database_mtime,
            as.numeric(database_info$mtime)
          )
      ) {
        stop("The Home summary must be rebuilt.", call. = FALSE)
      }

      cached <<- result
      cached_key <<- key
    }

    cached
  }
})

home_count <- function(x) {
  format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
}

# Keep leading categories and explicitly aggregate the remainder.
home_top_categories <- function(labels, counts, maximum = 8L) {
  data <- data.frame(
    label = labels,
    n = as.numeric(counts),
    stringsAsFactors = FALSE
  )

  data <- data[order(-data$n, data$label), , drop = FALSE]

  if (nrow(data) > maximum) {
    remainder <- sum(data$n[-seq_len(maximum)])
    data <- data[seq_len(maximum), , drop = FALSE]
    data <- rbind(
      data,
      data.frame(label = "Other categories (combined)", n = remainder)
    )
  }

  data
}

home_barplot <- function(data, xlab, x_max, x_step) {
  validate(need(
    nrow(data) > 0L &&
      all(is.finite(data$n)) &&
      all(data$n >= 0),
    "No valid values are available for this chart."
  ))

  device_size <- grDevices::dev.size("in")

  # Wait until the output has enough drawing space.
  req(
    device_size[[1L]] >= 3,
    device_size[[2L]] >= 2.5
  )

  previous <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(previous), add = TRUE)

  data <- data[rev(seq_len(nrow(data))), , drop = FALSE]

  labels <- vapply(
    data$label,
    function(label) {
      paste(strwrap(label, width = 24L), collapse = "\n")
    },
    character(1)
  )

  maximum <- max(data$n)

  validate(need(
    is.finite(x_max) &&
      is.finite(x_step) &&
      x_step > 0 &&
      x_max > maximum,
    "The configured axis limit must exceed the largest count."
  ))

  upper <- x_max

  ticks <- sort(unique(c(
    seq(0, upper, by = x_step),
    upper
  )))

  graphics::par(
    ps = 12,
    cex = 1,
    mex = 1,
    mai = c(
      0.65,
      min(1.55, device_size[[1L]] * 0.30),
      0.15,
      0.45
    ),
    mgp = c(2.5, 0.7, 0),
    las = 1,
    fg = "#526776",
    col.axis = "#526776",
    col.lab = "#243746"
  )

  positions <- graphics::barplot(
    data$n,
    names.arg = labels,
    horiz = TRUE,
    col = "#176b80",
    border = NA,
    xlim = c(0, upper),
    xaxs = "i",
    axes = FALSE,
    cex.names = 0.8,
    xlab = xlab,
    cex.lab = 0.9,
    xpd = FALSE
  )

  graphics::axis(
    1,
    at = ticks,
    labels = home_count(ticks),
    cex.axis = 0.75
  )

  graphics::text(
    data$n,
    positions,
    labels = home_count(data$n),
    pos = 4,
    cex = 0.75,
    col = "#243746",
    xpd = FALSE
  )
}

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
  home_plots <- reactive({
    validate(need(!is.null(con), "The database is unavailable."))

    result <- tryCatch(
      get_cached_home_plots(),
      error = function(e) {
        message("Home summary failed: ", conditionMessage(e))
        NULL
      }
    )

    validate(need(
      !is.null(result),
      "Charts are unavailable. The Home summary must be prepared or rebuilt."
    ))

    result
  })

  output$home_chart_status <- renderText({
    result <- home_plots()

    paste0(
      "Whole-database summaries, independent of explorer filters. Calculated: ",
      format(result$calculated_at, "%Y-%m-%d %H:%M:%S %Z"),
      "."
    )
  })

  output$home_year_plot <- renderPlot({
    data <- home_plots()$summary$years
    data <- data[!is.na(data$year), , drop = FALSE]

    validate(need(
      nrow(data) > 0L,
      "No recorded years are available."
    ))

    years <- seq.int(min(data$year), max(data$year))
    counts <- data$n[match(years, data$year)]
    counts[is.na(counts)] <- 0

    upper <- max(1, counts) * 1.05

    graphics::par(
      mar = c(4.5, 6.5, 1, 1),
      mgp = c(2.8, 0.7, 0),
      las = 1,
      col.axis = "#526776",
      col.lab = "#243746"
    )

    graphics::plot(
      years,
      counts,
      type = "h",
      lwd = 2,
      lend = "butt",
      col = "#176b80",
      xlab = "Recorded sampling year",
      ylab = "",
      yaxt = "n",
      ylim = c(0, upper),
      cex.axis = 0.85,
      cex.lab = 0.9
    )

    ticks <- pretty(c(0, upper))
    ticks <- ticks[ticks >= 0 & ticks <= upper]

    graphics::axis(
      2,
      at = ticks,
      labels = home_count(ticks),
      las = 1,
      cex.axis = 0.85
    )

    graphics::mtext(
      "Sampling events",
      side = 2,
      line = 4.7,
      las = 0,
      cex = 0.9,
      col = "#243746"
    )
  })

  output$home_year_note <- renderText({
    data <- home_plots()$summary$years
    excluded <- sum(data$n[is.na(data$year)])

    paste0(
      "Each represented event is counted once; intervening years with no ",
      "represented events are shown as zero. Events without a usable recorded ",
      "year: ",
      home_count(excluded),
      ". Counts describe database coverage, ",
      "not trends in phytoplankton abundance."
    )
  })

  output$home_source_plot <- renderPlot(
    {
      data <- home_plots()$summary$sources

      validate(need(
        nrow(data) > 0L,
        "No represented sources are available."
      ))

      labels <- data$source
      labels[is.na(data$source)] <- "Source: SQL NULL"
      labels[
        !is.na(data$source) & data$source == ""
      ] <- "Source: empty string"

      shown <- home_top_categories(labels, data$n)

      home_barplot(
        shown,
        xlab = "Sampling events",
        x_max = 20000,
        x_step = 5000
      )
    },
    execOnResize = TRUE
  )

  output$home_rank_plot <- renderPlot(
    {
      data <- home_plots()$summary$ranks

      validate(need(
        nrow(data) > 0L,
        "No abundance records are available."
      ))

      shown <- home_top_categories(data$label, data$n)

      home_barplot(
        shown,
        xlab = "Abundance records",
        x_max = 4500000,
        x_step = 1000000
      )
    },
    execOnResize = TRUE
  )

  output$home_depth_plot <- renderPlot(
    {
      depth <- home_plots()$summary$depth

      validate(need(
        !is.null(depth) && is.matrix(depth$counts),
        "The Home depth summary must be rebuilt."
      ))

      validate(need(
        is.finite(depth$total) && depth$total > 0,
        "No represented event-depth combinations are available."
      ))

      device_size <- grDevices::dev.size("in")

      req(
        device_size[[1L]] >= 3,
        device_size[[2L]] >= 2.5
      )

      previous <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(previous), add = TRUE)

      counts <- depth$counts

      colours <- c(
        exact = "#176b80",
        range = "#75a8b5",
        surface = "#d9a441",
        wc = "#8064a2",
        pumphouse = "#b5654d"
      )

      qualifier_labels <- c(
        exact = "Exact",
        range = "Range",
        surface = "Surface",
        wc = "Water column",
        pumphouse = "Pumphouse"
      )

      shown <- counts[,
        rev(seq_len(ncol(counts))),
        drop = FALSE
      ]

      totals <- colSums(shown)

      upper <- 70000
      ticks <- seq(0, upper, by = 10000)

      validate(need(
        max(totals) < upper,
        "The configured depth axis limit must exceed the largest count."
      ))

      graphics::par(
        ps = 12,
        cex = 1,
        mex = 1,
        mai = c(
          0.65,
          min(1.05, device_size[[1L]] * 0.24),
          0.70,
          0.45
        ),
        mgp = c(2.5, 0.7, 0),
        las = 1,
        col.axis = "#526776",
        col.lab = "#243746"
      )

      positions <- graphics::barplot(
        shown,
        names.arg = colnames(shown),
        horiz = TRUE,
        beside = FALSE,
        col = unname(colours[rownames(shown)]),
        border = NA,
        axes = FALSE,
        xlim = c(0, upper),
        xaxs = "i",
        cex.names = 0.8,
        xlab = "Event-depth combinations",
        cex.lab = 0.9,
        xpd = FALSE
      )

      graphics::axis(
        1,
        at = ticks,
        labels = home_count(ticks),
        cex.axis = 0.75
      )

      graphics::text(
        totals,
        positions,
        labels = home_count(totals),
        pos = 4,
        cex = 0.75,
        col = "#243746",
        xpd = FALSE
      )

      graphics::legend(
        x = mean(graphics::par("usr")[1:2]),
        y = graphics::par("usr")[[4L]],
        xjust = 0.5,
        yjust = 0,
        legend = unname(
          qualifier_labels[rownames(shown)]
        ),
        fill = unname(colours[rownames(shown)]),
        border = NA,
        ncol = 3,
        bty = "n",
        cex = 0.75,
        xpd = NA
      )
    },
    execOnResize = TRUE
  )

  output$home_depth_note <- renderText({
    depth <- home_plots()$summary$depth

    validate(need(
      !is.null(depth) && !is.null(depth$total),
      "The Home depth summary must be rebuilt."
    ))

    paste0(
      "Counts describe ",
      home_count(depth$total),
      " event-depth combinations associated with abundance records, ",
      "not necessarily independent physical samples. ",
      "Intervals describe stored depth values; colours retain the original ",
      "depth qualifiers. Range and water-column values are representative ",
      "depths, not discrete collection depths. ",
      "Intervals have unequal widths; bars show counts, not density per metre."
    )
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
}
