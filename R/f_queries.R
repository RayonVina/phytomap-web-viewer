# Small source catalogue; never return a silently incomplete list.
get_sources <- function(con) {
  data <- DBI::dbGetQuery(
    con,
    "
    SELECT id AS source_id, source, subsource
    FROM source ORDER BY source, subsource, id LIMIT 5001
  "
  )
  if (nrow(data) > 5000L) {
    stop("Source catalogue safety bound exceeded.", call. = FALSE)
  }
  data
}

get_database_overview <- function(con) {
  DBI::dbGetQuery(
    con,
    "
    SELECT (SELECT COUNT(*) FROM abundance) AS abundance_records,
      (SELECT COUNT(*) FROM sample) AS samples,
      (SELECT COUNT(*) FROM event) AS events,
      (SELECT COUNT(*) FROM source) AS source_entries,
      (SELECT COUNT(*) FROM sampling_protocol) AS sampling_protocols,
      (SELECT COUNT(*) FROM taxonomy) AS taxonomy_entries
  "
  )
}

validate_map_source_ids <- function(source_ids) {
  if (is.null(source_ids)) {
    return(NULL)
  }
  if (
    !is.numeric(source_ids) ||
      anyNA(source_ids) ||
      any(!is.finite(source_ids)) ||
      any(source_ids != floor(source_ids)) ||
      length(unique(source_ids)) > 5000L
  ) {
    stop("Invalid source identifiers.", call. = FALSE)
  }
  unique(source_ids)
}

# All values are parameterized. NULL means all; numeric(0) means none.
map_source_filter <- function(source_ids) {
  ids <- validate_map_source_ids(source_ids)
  if (is.null(ids)) {
    return(list(sql = "1 = 1", params = list()))
  }
  if (!length(ids)) {
    return(list(sql = "1 = 0", params = list()))
  }
  list(
    sql = paste0(
      "e.source_id IN (",
      paste(rep("?", length(ids)), collapse = ","),
      ")"
    ),
    params = as.list(ids)
  )
}

map_event_exists_sql <- function() {
  "EXISTS (SELECT 1 FROM sample AS sm WHERE sm.event_id = e.id
      AND EXISTS (SELECT 1 FROM abundance AS a WHERE a.sample_id = sm.id))"
}

get_map_coverage <- function(con, source_ids = NULL, max_locations = 50000L) {
  if (
    length(max_locations) != 1L ||
      !is.numeric(max_locations) ||
      is.na(max_locations) ||
      !is.finite(max_locations) ||
      max_locations < 1 ||
      max_locations > 50000 ||
      max_locations != floor(max_locations)
  ) {
    stop("Invalid coordinate-group bound.", call. = FALSE)
  }
  filter <- map_source_filter(source_ids)
  sql <- paste0(
    "
    SELECT e.coordinates_id, c.id AS coordinate_entry_id,
      c.latitude, c.longitude, c.coord_precision, COUNT(*) AS n_events
    FROM event AS e
    LEFT JOIN coordinates AS c ON c.id = e.coordinates_id
    WHERE ",
    filter$sql,
    " AND ",
    map_event_exists_sql(),
    "
    GROUP BY e.coordinates_id, c.id, c.latitude, c.longitude, c.coord_precision
    ORDER BY e.coordinates_id LIMIT ?"
  )
  data <- DBI::dbGetQuery(
    con,
    sql,
    params = c(filter$params, list(as.integer(max_locations) + 1L))
  )
  if (nrow(data) > max_locations) {
    stop(
      "Coordinate-group bound exceeded; no partial map returned.",
      call. = FALSE
    )
  }
  data
}

# Point provenance uses the same event eligibility and APPLIED source filter as the map.
get_point_sources <- function(con, coordinate_id, source_ids = NULL) {
  if (
    length(coordinate_id) != 1L ||
      !is.numeric(coordinate_id) ||
      is.na(coordinate_id) ||
      !is.finite(coordinate_id) ||
      coordinate_id != floor(coordinate_id)
  ) {
    stop("Invalid coordinate identifier.", call. = FALSE)
  }
  filter <- map_source_filter(source_ids)
  sql <- paste0(
    "
    SELECT e.source_id, src.source, src.subsource, COUNT(*) AS n_events
    FROM event AS e
    LEFT JOIN source AS src ON src.id = e.source_id
    WHERE e.coordinates_id = ? AND ",
    filter$sql,
    " AND ",
    map_event_exists_sql(),
    "
    GROUP BY e.source_id, src.source, src.subsource
    ORDER BY src.source, src.subsource, e.source_id LIMIT 5001"
  )
  data <- DBI::dbGetQuery(
    con,
    sql,
    params = c(list(coordinate_id), filter$params)
  )
  if (nrow(data) > 5000L) {
    stop(
      "Point provenance bound exceeded; no partial list returned.",
      call. = FALSE
    )
  }
  data
}

# Legacy diagnostics retained for existing scripts.
get_source_preview <- function(con, source_id, limit = 100L) {
  ids <- validate_map_source_ids(source_id)
  if (is.null(ids) || length(ids) != 1L) {
    stop("Expected one source identifier.", call. = FALSE)
  }
  if (
    length(limit) != 1L ||
      !is.numeric(limit) ||
      is.na(limit) ||
      !is.finite(limit) ||
      limit < 1 ||
      limit > 1000 ||
      limit != floor(limit)
  ) {
    stop("Invalid preview limit.", call. = FALSE)
  }
  DBI::dbGetQuery(
    con,
    "
    SELECT a.id AS abundance_id, a.sample_id, a.taxon_record_id, src.source, src.subsource,
      dt.year, dt.month, dt.day, dt.date_precision, dt.day_is_imputed,
      c.latitude, c.longitude, c.coord_precision, d.depth, d.depth_label,
      tr.taxon_raw, tr.taxon_accepted, tx.rank AS taxonomic_rank,
      a.abundance AS abundance_cells_per_litre, sp.enumeration_method, sp.sampling_method
    FROM abundance AS a LEFT JOIN sample AS s ON s.id = a.sample_id
    LEFT JOIN event AS e ON e.id = s.event_id LEFT JOIN source AS src ON src.id = e.source_id
    LEFT JOIN event_datetime AS dt ON dt.id = e.event_datetime_id
    LEFT JOIN coordinates AS c ON c.id = e.coordinates_id LEFT JOIN depth AS d ON d.id = s.depth_id
    LEFT JOIN taxon_record AS tr ON tr.id = a.taxon_record_id
    LEFT JOIN taxonomy AS tx ON tx.id = tr.taxonomy_id
    LEFT JOIN sampling_protocol AS sp ON sp.id = a.sampling_protocol_id
    WHERE e.source_id = ? LIMIT ?",
    params = list(ids[[1]], as.integer(limit))
  )
}

get_source_map_events <- function(con, source_id, limit = 2000L) {
  ids <- validate_map_source_ids(source_id)
  if (is.null(ids) || length(ids) != 1L) {
    stop("Expected one source identifier.", call. = FALSE)
  }
  if (
    length(limit) != 1L ||
      !is.numeric(limit) ||
      is.na(limit) ||
      !is.finite(limit) ||
      limit < 1 ||
      limit > 2000 ||
      limit != floor(limit)
  ) {
    stop("Invalid event limit.", call. = FALSE)
  }
  DBI::dbGetQuery(
    con,
    paste0(
      "
    SELECT e.id AS event_id, e.coordinates_id, c.latitude, c.longitude, c.coord_precision
    FROM event AS e LEFT JOIN coordinates AS c ON c.id = e.coordinates_id
    WHERE e.source_id = ? AND ",
      map_event_exists_sql(),
      " ORDER BY e.id LIMIT ?"
    ),
    params = list(ids[[1]], as.integer(limit) + 1L)
  )
}

# Paged point details. The same applied provenance filter is used throughout.
get_point_events <- function(
  con,
  coordinate_id,
  source_ids = NULL,
  offset = 0L
) {
  if (
    length(coordinate_id) != 1L ||
      !is.numeric(coordinate_id) ||
      is.na(coordinate_id) ||
      !is.finite(coordinate_id) ||
      coordinate_id != floor(coordinate_id)
  ) {
    stop("Invalid coordinate identifier.")
  }
  validate_detail_offset(offset)
  f <- map_source_filter(source_ids)
  DBI::dbGetQuery(
    con,
    paste0(
      "
    SELECT e.id AS event_id, src.source, src.subsource,
      dt.year, dt.month, dt.day, dt.time_value, dt.date_precision, dt.day_is_imputed,
      dt.notes AS datetime_notes
    FROM event AS e
    LEFT JOIN source AS src ON src.id = e.source_id
    LEFT JOIN event_datetime AS dt ON dt.id = e.event_datetime_id
    WHERE e.coordinates_id = ? AND ",
      f$sql,
      " AND ",
      map_event_exists_sql(),
      "
    ORDER BY dt.year, dt.month, dt.day, e.id LIMIT 101 OFFSET ?"
    ),
    params = c(list(coordinate_id), f$params, list(as.integer(offset)))
  )
}

validate_detail_offset <- function(offset) {
  if (
    length(offset) != 1L ||
      !is.numeric(offset) ||
      is.na(offset) ||
      !is.finite(offset) ||
      offset < 0 ||
      offset > 100000000 ||
      offset != floor(offset)
  ) {
    stop("Invalid detail page offset.")
  }
  invisible(TRUE)
}

get_event_samples <- function(con, event_id, coordinate_id, source_ids = NULL) {
  f <- map_source_filter(source_ids)
  data <- DBI::dbGetQuery(
    con,
    paste0(
      "
    SELECT sm.id AS sample_id, d.depth, d.depth_label, d.depth_label_min, d.depth_label_max,
      sm.notes AS sample_notes, d.notes AS depth_notes
    FROM sample AS sm
    JOIN event AS e ON e.id = sm.event_id
    LEFT JOIN depth AS d ON d.id = sm.depth_id
    WHERE e.id = ? AND e.coordinates_id = ? AND ",
      f$sql,
      "
      AND EXISTS (SELECT 1 FROM abundance AS a WHERE a.sample_id = sm.id)
    ORDER BY d.depth, sm.id LIMIT 501"
    ),
    params = c(list(event_id, coordinate_id), f$params)
  )
  if (nrow(data) > 500L) {
    stop("Sample-list safety bound exceeded; no partial list returned.")
  }
  data
}

get_sample_records <- function(
  con,
  sample_id,
  coordinate_id,
  source_ids = NULL,
  offset = 0L
) {
  validate_detail_offset(offset)
  f <- map_source_filter(source_ids)
  DBI::dbGetQuery(
    con,
    paste0(
      "
    SELECT a.id AS abundance_id, a.abundance AS abundance_cells_per_litre,
      tr.taxon_raw, tr.taxon_accepted, tx.rank AS taxonomic_rank,
      sp.sampling_method, sp.enumeration_method
    FROM abundance AS a
    JOIN sample AS sm ON sm.id = a.sample_id
    JOIN event AS e ON e.id = sm.event_id
    LEFT JOIN taxon_record AS tr ON tr.id = a.taxon_record_id
    LEFT JOIN taxonomy AS tx ON tx.id = tr.taxonomy_id
    LEFT JOIN sampling_protocol AS sp ON sp.id = a.sampling_protocol_id
    WHERE sm.id = ? AND e.coordinates_id = ? AND ",
      f$sql,
      "
    ORDER BY a.id LIMIT 51 OFFSET ?"
    ),
    params = c(
      list(sample_id, coordinate_id),
      f$params,
      list(as.integer(offset))
    )
  )
}

get_abundance_details <- function(
  con,
  abundance_id,
  sample_id,
  coordinate_id,
  source_ids = NULL
) {
  fields <- list(
    a = c("abundance", "notes"),
    tr = c(
      "taxon_raw",
      "taxon_clean",
      "taxon_matched",
      "taxon_accepted",
      "aphiaid_matched",
      "aphiaid_accepted",
      "resolution_method",
      "uncertain",
      "ltr",
      "aggregates",
      "size_info",
      "morphotype",
      "life_stage",
      "infraspecific_taxa",
      "trophic",
      "local_authority",
      "notes"
    ),
    tx = c(
      "aphiaid",
      "kingdom",
      "phylum",
      "class",
      "order",
      "family",
      "genus",
      "species",
      "rank",
      "taxonomic_status"
    ),
    sp = c(
      "count_method",
      "enumeration_method",
      "sampling_method",
      "sampling_method_notes",
      "concentration_method",
      "concentration_method_notes",
      "volume_sampled",
      "microscopy_method",
      "microscopy_notes",
      "preservation",
      "volume_concentrated",
      "volume_counted",
      "magnification",
      "counting_chamber",
      "notes"
    )
  )
  projections <- unlist(
    lapply(names(fields), function(alias) {
      columns <- fields[[alias]]
      paste0(
        alias,
        ".",
        as.character(DBI::dbQuoteIdentifier(con, columns)),
        " AS ",
        as.character(DBI::dbQuoteIdentifier(con, paste0(alias, "_", columns)))
      )
    }),
    use.names = FALSE
  )
  f <- map_source_filter(source_ids)
  DBI::dbGetQuery(
    con,
    paste0(
      "SELECT ",
      paste(projections, collapse = ", "),
      "
    FROM abundance AS a
    JOIN sample AS sm ON sm.id = a.sample_id JOIN event AS e ON e.id = sm.event_id
    LEFT JOIN taxon_record AS tr ON tr.id = a.taxon_record_id
    LEFT JOIN taxonomy AS tx ON tx.id = tr.taxonomy_id
    LEFT JOIN sampling_protocol AS sp ON sp.id = a.sampling_protocol_id
    WHERE a.id = ? AND sm.id = ? AND e.coordinates_id = ? AND ",
      f$sql,
      " LIMIT 1"
    ),
    params = c(list(abundance_id, sample_id, coordinate_id), f$params)
  )
}


# Spatial selection, flattened queries and streamed CSV export.
sr_validate_ids <- function(ids, allow_all = FALSE, maximum = 200L) {
  if (is.null(ids) && allow_all) {
    return(NULL)
  }
  if (
    !is.numeric(ids) ||
      anyNA(ids) ||
      any(!is.finite(ids)) ||
      any(ids != floor(ids)) ||
      length(unique(ids)) > maximum
  ) {
    stop("Invalid or excessive selected identifiers.", call. = FALSE)
  }
  sort(unique(ids))
}

sr_fields <- function() {
  fields <- c(
    abundance_id = "ab.id",
    source_id = "ev.source_id",
    source = "src.source",
    subsource = "src.subsource",
    source_metadata = "src.metadata",
    source_url = "src.url",
    event_id = "ev.id",
    event_datetime_id = "ev.event_datetime_id"
  )
  add <- function(columns, alias) {
    stats::setNames(paste0(alias, ".", columns), columns)
  }
  fields <- c(
    fields,
    add(
      c(
        "year",
        "month",
        "day",
        "time_value",
        "date_precision",
        "day_is_imputed"
      ),
      "edt"
    ),
    coordinates_id = "ev.coordinates_id",
    add(c("latitude", "longitude", "coord_precision"), "c"),
    sample_id = "sm.id",
    depth_id = "sm.depth_id",
    add(c("depth", "depth_label", "depth_label_min", "depth_label_max"), "d"),
    abundance = "ab.abundance",
    abundance_notes = "ab.notes",
    sampling_protocol_id = "ab.sampling_protocol_id",
    add(
      c(
        "count_method",
        "enumeration_method",
        "sampling_method",
        "sampling_method_notes",
        "concentration_method",
        "concentration_method_notes",
        "volume_sampled",
        "microscopy_method",
        "microscopy_notes",
        "preservation",
        "volume_concentrated",
        "volume_counted",
        "magnification",
        "counting_chamber"
      ),
      "sp"
    ),
    taxon_record_id = "ab.taxon_record_id",
    add(
      c(
        "taxon_raw",
        "taxon_clean",
        "taxon_matched",
        "aphiaid_matched",
        "taxon_accepted",
        "aphiaid_accepted",
        "resolution_method",
        "uncertain",
        "ltr",
        "aggregates",
        "size_info",
        "morphotype",
        "life_stage",
        "infraspecific_taxa",
        "trophic",
        "local_authority"
      ),
      "tr"
    ),
    taxonomy_id = "tr.taxonomy_id",
    add(
      c(
        "aphiaid",
        "kingdom",
        "subkingdom",
        "infrakingdom",
        "phylum",
        "subphylum",
        "infraphylum",
        "parvphylum",
        "gigaclass",
        "superclass",
        "class",
        "subclass",
        "infraclass",
        "subterclass",
        "superorder",
        "order",
        "suborder",
        "infraorder",
        "parvorder",
        "superfamily",
        "family",
        "subfamily",
        "tribe",
        "genus",
        "subgenus",
        "section",
        "subsection",
        "species",
        "subspecies",
        "variety",
        "forma",
        "rank",
        "taxonomic_status"
      ),
      "tx"
    )
  )
  fields
}

sr_statement <- function(
  con,
  coordinate_ids,
  source_ids = NULL,
  columns = names(sr_fields())
) {
  coordinates <- sr_validate_ids(coordinate_ids)
  sources <- sr_validate_ids(source_ids, allow_all = TRUE, maximum = 5000L)
  fields <- sr_fields()
  if (
    !length(columns) ||
      anyDuplicated(columns) ||
      !all(columns %in% names(fields))
  ) {
    stop("Invalid flattened column selection.", call. = FALSE)
  }
  placeholders <- function(x) paste(rep("?", length(x)), collapse = ",")
  where <- if (length(coordinates)) {
    paste0("ev.coordinates_id IN (", placeholders(coordinates), ")")
  } else {
    "1 = 0"
  }
  params <- as.list(coordinates)
  if (!is.null(sources)) {
    where <- paste(
      where,
      "AND",
      if (length(sources)) {
        paste0("ev.source_id IN (", placeholders(sources), ")")
      } else {
        "1 = 0"
      }
    )
    params <- c(params, as.list(sources))
  }
  aliases <- as.character(DBI::dbQuoteIdentifier(con, columns))
  select <- paste(paste(fields[columns], "AS", aliases), collapse = ",\n")
  from <- paste0(
    "
    FROM abundance AS ab
    JOIN sample AS sm ON sm.id = ab.sample_id
    JOIN event AS ev ON ev.id = sm.event_id
    LEFT JOIN source AS src ON src.id = ev.source_id
    LEFT JOIN event_datetime AS edt ON edt.id = ev.event_datetime_id
    LEFT JOIN coordinates AS c ON c.id = ev.coordinates_id
    LEFT JOIN depth AS d ON d.id = sm.depth_id
    LEFT JOIN sampling_protocol AS sp ON sp.id = ab.sampling_protocol_id
    LEFT JOIN taxon_record AS tr ON tr.id = ab.taxon_record_id
    LEFT JOIN taxonomy AS tx ON tx.id = tr.taxonomy_id
    WHERE ",
    where
  )
  list(sql = paste0("SELECT ", select, from), from = from, params = params)
}

# Availability is assessed across the full selection, not a single SQL page.
sr_profile <- function(con, coordinate_ids, source_ids = NULL) {
  statement <- sr_statement(con, coordinate_ids, source_ids)
  columns <- names(sr_fields())
  quoted <- as.character(DBI::dbQuoteIdentifier(con, columns))
  expressions <- paste0(
    "COALESCE(MAX(CASE WHEN ",
    quoted,
    " IS NOT NULL AND ",
    quoted,
    " <> '' THEN 1 ELSE 0 END), 0) AS ",
    quoted
  )
  sql <- paste0(
    "SELECT COUNT(*) AS n_records, ",
    paste(expressions, collapse = ","),
    " FROM (",
    statement$sql,
    ") AS selected_flat"
  )
  data <- DBI::dbGetQuery(con, sql, params = statement$params)
  available <- columns[vapply(
    data[columns],
    function(x) isTRUE(x[[1]] == 1),
    logical(1)
  )]
  list(n_records = data$n_records[[1]], available = available)
}

sr_page <- function(
  con,
  coordinate_ids,
  source_ids = NULL,
  after_id = NULL,
  page_size = 100L
) {
  if (
    !is.null(after_id) &&
      (length(after_id) != 1L ||
        !is.numeric(after_id) ||
        is.na(after_id) ||
        !is.finite(after_id) ||
        after_id != floor(after_id))
  ) {
    stop("Invalid page cursor.", call. = FALSE)
  }
  if (
    length(page_size) != 1L ||
      !is.numeric(page_size) ||
      is.na(page_size) ||
      page_size < 1 ||
      page_size > 100 ||
      page_size != floor(page_size)
  ) {
    stop("Invalid page size.", call. = FALSE)
  }
  statement <- sr_statement(con, coordinate_ids, source_ids)
  cursor_sql <- if (is.null(after_id)) "" else " AND ab.id > ?"
  cursor_params <- if (is.null(after_id)) list() else list(after_id)
  data <- DBI::dbGetQuery(
    con,
    paste0(statement$sql, cursor_sql, " ORDER BY ab.id LIMIT ?"),
    params = c(
      statement$params,
      cursor_params,
      list(as.integer(page_size) + 1L)
    )
  )
  list(data = utils::head(data, page_size), more = nrow(data) > page_size)
}

# Read a bounded SQL result in chunks; do not materialize the full export in R.
sr_write_csv <- function(
  con,
  file,
  coordinate_ids,
  source_ids = NULL,
  columns = names(sr_fields()),
  max_rows = 100000L
) {
  if (
    length(max_rows) != 1L ||
      !is.numeric(max_rows) ||
      is.na(max_rows) ||
      !is.finite(max_rows) ||
      max_rows < 1 ||
      max_rows > 100000 ||
      max_rows != floor(max_rows)
  ) {
    stop("Invalid CSV safety bound.", call. = FALSE)
  }
  statement <- sr_statement(con, coordinate_ids, source_ids, columns)
  result <- DBI::dbSendQuery(
    con,
    paste0(statement$sql, " ORDER BY ab.id LIMIT ?")
  )
  on.exit(DBI::dbClearResult(result), add = TRUE)
  DBI::dbBind(result, c(statement$params, list(as.integer(max_rows) + 1L)))
  connection <- base::file(file, open = "wt", encoding = "UTF-8")
  on.exit(close(connection), add = TRUE)
  total <- 0L
  first <- TRUE
  repeat {
    data <- DBI::dbFetch(result, n = 5000L)
    total <- total + nrow(data)
    if (total > max_rows) {
      stop("CSV row limit exceeded; the export is not complete.", call. = FALSE)
    }
    for (name in names(data)) {
      values <- data[[name]]
      missing <- is.na(values)
      text <- if (is.numeric(values)) {
        sprintf("%.17g", values)
      } else {
        as.character(values)
      }
      text[missing] <- NA_character_
      if (any(text == "\\N", na.rm = TRUE)) {
        stop(
          "CSV missing-value token occurs in source text; export refused.",
          call. = FALSE
        )
      }
      data[[name]] <- text
    }
    if (nrow(data) || first) {
      utils::write.table(
        data,
        file = connection,
        sep = ",",
        row.names = FALSE,
        col.names = first,
        quote = TRUE,
        qmethod = "double",
        na = "\\N",
        eol = "\r\n"
      )
    }
    first <- FALSE
    if (DBI::dbHasCompleted(result)) break
  }
  invisible(total)
}

# Read schema metadata only; never retrieve observation records.
catalog_get_schema <- function(con) {
  tables <- DBI::dbGetQuery(
    con,
    "
    SELECT name AS table_name
    FROM sqlite_schema
    WHERE type = 'table'
      AND name NOT GLOB 'sqlite_*'
    ORDER BY name
    LIMIT ?
    ",
    params = list(101L)
  )

  if (!nrow(tables) || nrow(tables) > 100L) {
    stop("Invalid schema table count.", call. = FALSE)
  }

  columns <- vector("list", nrow(tables))
  relationships <- vector("list", nrow(tables))

  for (i in seq_len(nrow(tables))) {
    table_name <- tables$table_name[[i]]

    fields <- DBI::dbGetQuery(
      con,
      '
      SELECT
        cid,
        name AS column_name,
        type AS declared_type,
        "notnull" AS declared_not_null,
        dflt_value AS default_sql,
        pk AS primary_key_position,
        hidden AS hidden_code
      FROM pragma_table_xinfo(?)
      ORDER BY cid
      LIMIT ?
      ',
      params = list(table_name, 501L)
    )

    if (nrow(fields) > 500L) {
      stop("Schema column safety bound exceeded.", call. = FALSE)
    }

    fields$table_name <- rep(table_name, nrow(fields))
    columns[[i]] <- fields

    keys <- DBI::dbGetQuery(
      con,
      '
      SELECT
        id AS foreign_key_id,
        seq AS column_sequence,
        "table" AS referenced_table,
        "from" AS source_column,
        "to" AS referenced_column,
        on_update,
        on_delete,
        "match" AS match_rule
      FROM pragma_foreign_key_list(?)
      ORDER BY id, seq
      LIMIT ?
      ',
      params = list(table_name, 501L)
    )

    if (nrow(keys) > 500L) {
      stop("Schema relationship safety bound exceeded.", call. = FALSE)
    }

    keys$source_table <- rep(table_name, nrow(keys))
    relationships[[i]] <- keys
  }

  columns <- do.call(rbind, columns)
  relationships <- do.call(rbind, relationships)
  rownames(columns) <- NULL
  rownames(relationships) <- NULL

  if (
    !all(
      relationships$referenced_table %in% tables$table_name
    )
  ) {
    stop(
      "A referenced table is outside the displayed schema.",
      call. = FALSE
    )
  }

  list(
    tables = tables,
    columns = columns,
    relationships = relationships
  )
}

# Retrieve documentation for one table without reading observation records.
catalog_get_table_metadata <- function(con, table_name) {
  if (
    !is.character(table_name) ||
      length(table_name) != 1L ||
      is.na(table_name) ||
      !nzchar(table_name)
  ) {
    stop("Invalid catalogue table name.", call. = FALSE)
  }

  data <- DBI::dbGetQuery(
    con,
    "
    SELECT
      sort_order,
      table_name,
      variable,
      description,
      data_type,
      possible_values,
      missing_value_interpretation,
      notes
    FROM metadata_dictionary
    WHERE table_name = ?
    ORDER BY sort_order, variable
    LIMIT ?
    ",
    params = list(table_name, 501L)
  )

  if (nrow(data) > 500L) {
    stop(
      "Metadata safety bound exceeded; no partial result returned.",
      call. = FALSE
    )
  }

  if (anyDuplicated(data$variable)) {
    stop("Duplicate metadata variables were returned.", call. = FALSE)
  }

  data
}

# Stable field identifiers shared by R and the catalogue browser layer.
cv_fields <- function(schema) {
  tables <- schema$tables$table_name
  tables <- tables[tables != "metadata_dictionary"]

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

  fields$node_id <- paste0("t", match(fields$table_name, tables))
  fields$field_id <- paste0(fields$node_id, "_c", fields$cid)
  rownames(fields) <- NULL
  fields
}

# Build a read-only view using child-to-parent relationships only.
cv_plan <- function(con, schema, base_table, field_ids) {
  fields <- cv_fields(schema)
  tables <- unique(fields$table_name)

  if (
    !is.character(base_table) ||
      length(base_table) != 1L ||
      is.na(base_table) ||
      !base_table %in% tables
  ) {
    stop("Select a valid base table.", call. = FALSE)
  }

  if (
    !is.character(field_ids) ||
      !length(field_ids) ||
      anyNA(field_ids) ||
      anyDuplicated(field_ids) ||
      !all(field_ids %in% fields$field_id)
  ) {
    stop("Select valid catalogue fields.", call. = FALSE)
  }

  selected <- fields[match(field_ids, fields$field_id), , drop = FALSE]

  # Preserve qualifiers alongside selected scientific values.
  companions <- list(
    event_datetime = c("date_precision", "day_is_imputed"),
    coordinates = "coord_precision",
    depth = c("depth_label", "depth_label_min", "depth_label_max")
  )

  for (table_name in names(companions)) {
    if (table_name %in% selected$table_name) {
      extra <- fields[
        fields$table_name == table_name &
          fields$column_name %in% companions[[table_name]],
        ,
        drop = FALSE
      ]

      selected <- rbind(
        selected,
        extra[!extra$field_id %in% selected$field_id, , drop = FALSE]
      )
    }
  }

  keys <- schema$relationships[
    schema$relationships$source_table %in%
      tables &
      schema$relationships$referenced_table %in% tables,
    ,
    drop = FALSE
  ]

  # Find reachable parent tables and remember their join paths.
  discovered <- base_table
  parent_edge <- list()
  position <- 1L

  while (position <= length(discovered)) {
    current <- discovered[[position]]
    candidates <- which(keys$source_table == current)

    for (i in candidates) {
      target <- keys$referenced_table[[i]]

      if (!target %in% discovered) {
        parent_edge[[target]] <- i
        discovered <- c(discovered, target)
      }
    }

    position <- position + 1L
  }

  missing <- setdiff(unique(selected$table_name), discovered)

  if (length(missing)) {
    stop(
      paste0(
        "These tables cannot be reached from the selected base using ",
        "child-to-parent joins: ",
        paste(missing, collapse = ", "),
        ". Choose a more detailed base table."
      ),
      call. = FALSE
    )
  }

  needed <- base_table

  for (table_name in unique(selected$table_name)) {
    current <- table_name

    while (!identical(current, base_table)) {
      needed <- union(needed, current)
      current <- keys$source_table[[parent_edge[[current]]]]
    }
  }

  needed <- discovered[discovered %in% needed]
  aliases <- stats::setNames(paste0("r", seq_along(needed)), needed)

  quote_id <- function(x) {
    as.character(DBI::dbQuoteIdentifier(con, x))
  }

  field_sql <- function(table_name, column_name) {
    paste0(aliases[[table_name]], ".", quote_id(column_name))
  }

  base_pk <- fields[
    fields$table_name == base_table &
      fields$primary_key_position > 0L,
    ,
    drop = FALSE
  ]

  if (
    nrow(base_pk) != 1L ||
      !identical(base_pk$column_name[[1L]], "id")
  ) {
    stop(
      "This version requires a single id primary key on the base table.",
      call. = FALSE
    )
  }

  joins <- character(0)

  for (target in needed[-1L]) {
    key <- keys[parent_edge[[target]], , drop = FALSE]

    same_constraint <- keys[
      keys$source_table == key$source_table[[1L]] &
        keys$foreign_key_id == key$foreign_key_id[[1L]],
      ,
      drop = FALSE
    ]

    target_pk <- fields[
      fields$table_name == target &
        fields$primary_key_position > 0L,
      ,
      drop = FALSE
    ]

    if (nrow(same_constraint) != 1L || nrow(target_pk) != 1L) {
      stop(
        "Composite-key joins are not supported in this version.",
        call. = FALSE
      )
    }

    target_column <- key$referenced_column[[1L]]

    if (is.na(target_column) || target_column == "") {
      target_column <- target_pk$column_name[[1L]]
    }

    if (!identical(target_column, target_pk$column_name[[1L]])) {
      stop(
        "Only joins to a declared single-column primary key are supported.",
        call. = FALSE
      )
    }

    joins <- c(
      joins,
      paste(
        "LEFT JOIN",
        quote_id(target),
        "AS",
        aliases[[target]],
        "ON",
        field_sql(
          key$source_table[[1L]],
          key$source_column[[1L]]
        ),
        "=",
        field_sql(target, target_column)
      )
    )
  }
  # Include primary keys and join foreign keys for export traceability.
  technical_fields <- fields[
    fields$table_name %in% needed & fields$primary_key_position > 0L,
    ,
    drop = FALSE
  ]

  for (target in needed[-1L]) {
    key <- keys[parent_edge[[target]], , drop = FALSE]

    source_key <- fields[
      fields$table_name == key$source_table[[1L]] &
        fields$column_name == key$source_column[[1L]],
      ,
      drop = FALSE
    ]

    if (nrow(source_key) != 1L) {
      stop("A join key could not be resolved.", call. = FALSE)
    }

    technical_fields <- rbind(technical_fields, source_key)
  }

  technical_fields <- technical_fields[
    !duplicated(technical_fields$field_id),
    ,
    drop = FALSE
  ]

  selected <- rbind(
    selected,
    technical_fields[
      !technical_fields$field_id %in% selected$field_id,
      ,
      drop = FALSE
    ]
  )

  # Classify by schema role, never by matching values in a loaded page.
  is_foreign_key <- vapply(
    seq_len(nrow(selected)),
    function(i) {
      any(
        keys$source_table == selected$table_name[[i]] &
          keys$source_column == selected$column_name[[i]]
      )
    },
    logical(1)
  )

  column_names <- paste(
    selected$table_name,
    selected$column_name,
    sep = "."
  )

  technical_columns <- column_names[
    selected$primary_key_position > 0L | is_foreign_key
  ]

  projections <- vapply(
    seq_len(nrow(selected)),
    function(i) {
      paste(
        field_sql(selected$table_name[[i]], selected$column_name[[i]]),
        "AS",
        quote_id(paste0(
          selected$table_name[[i]],
          ".",
          selected$column_name[[i]]
        ))
      )
    },
    character(1)
  )

  sql <- paste(
    "SELECT",
    paste(
      c(
        paste(field_sql(base_table, "id"), "AS", quote_id(".base_id")),
        projections
      ),
      collapse = ", "
    ),
    "FROM",
    quote_id(base_table),
    "AS",
    aliases[[base_table]],
    paste(joins, collapse = "\n")
  )

  list(
    sql = sql,
    cursor_sql = field_sql(base_table, "id"),
    base_table = base_table,
    tables = needed,
    fields = selected,
    technical_columns = unique(technical_columns),
    export_columns = column_names
  )
}

# Retrieve one SQL page plus one look-ahead row.
cv_page <- function(con, plan, after_id = NULL) {
  statement <- cv_page_statement(
    plan,
    after_id = after_id,
    limit = 101L
  )

  data <- DBI::dbGetQuery(
    con,
    statement$sql,
    params = statement$params
  )

  list(
    data = utils::head(data, 100L),
    more = nrow(data) > 100L
  )
}

cv_validate_export_limit <- function(max_rows) {
  if (
    !is.numeric(max_rows) ||
      length(max_rows) != 1L ||
      is.na(max_rows) ||
      !is.finite(max_rows) ||
      max_rows < 1 ||
      max_rows > 100000 ||
      max_rows != floor(max_rows)
  ) {
    stop("Invalid catalogue CSV row limit.", call. = FALSE)
  }

  as.integer(max_rows)
}

# Count at most the export limit plus one, including applied filter values.
cv_export_count <- function(con, plan, max_rows = 100000L) {
  limit <- cv_validate_export_limit(max_rows)

  filter_params <- if (is.null(plan$params)) {
    list()
  } else {
    plan$params
  }

  sql <- paste0(
    "SELECT COUNT(*) AS n_rows FROM (",
    plan$sql,
    " LIMIT ?",
    ") AS bounded_catalog_export"
  )

  data <- DBI::dbGetQuery(
    con,
    sql,
    params = c(
      filter_params,
      list(limit + 1L)
    )
  )

  data$n_rows[[1L]]
}

# Stream the complete applied view to CSV.
cv_write_csv <- function(con, file, plan, max_rows = 100000L) {
  limit <- cv_validate_export_limit(max_rows)
  expected <- cv_export_count(con, plan, limit)

  if (expected > limit) {
    stop(
      structure(
        list(
          message = paste0(
            "CSV export exceeds the ",
            format(limit, big.mark = ",", trim = TRUE),
            "-row limit. No partial export is available."
          ),
          call = NULL
        ),
        class = c("cv_export_limit", "error", "condition")
      )
    )
  }

  columns <- plan$export_columns

  if (
    !is.character(columns) ||
      !length(columns) ||
      anyNA(columns) ||
      anyDuplicated(columns) ||
      ".base_id" %in% columns
  ) {
    stop("Invalid catalogue export columns.", call. = FALSE)
  }

  staging <- tempfile(
    pattern = "catalog-csv-",
    tmpdir = dirname(file)
  )

  on.exit(unlink(staging), add = TRUE)

  statement <- cv_page_statement(
    plan,
    limit = limit + 1L
  )

  # Supply all parameters once: filters followed by the retrieval limit.
  result <- DBI::dbSendQuery(
    con,
    statement$sql,
    params = statement$params
  )

  on.exit(
    {
      if (!is.null(result)) {
        DBI::dbClearResult(result)
      }
    },
    add = TRUE
  )

  connection <- base::file(
    staging,
    open = "wt",
    encoding = "UTF-8"
  )

  on.exit(
    {
      if (!is.null(connection)) {
        close(connection)
      }
    },
    add = TRUE
  )

  total <- 0L
  first <- TRUE

  repeat {
    data <- DBI::dbFetch(result, n = 5000L)

    if (!all(columns %in% names(data))) {
      stop(
        "CSV query columns do not match the loaded plan.",
        call. = FALSE
      )
    }

    total <- total + nrow(data)

    if (total > limit) {
      stop(
        "CSV row limit exceeded; export refused.",
        call. = FALSE
      )
    }

    data <- data[, columns, drop = FALSE]

    for (name in names(data)) {
      value <- data[[name]]
      missing <- is.na(value)

      text <- if (is.numeric(value)) {
        sprintf("%.17g", value)
      } else {
        as.character(value)
      }

      text[missing] <- NA_character_

      if (any(text == "\\N", na.rm = TRUE)) {
        stop(
          "The CSV NULL token occurs in source text; export refused.",
          call. = FALSE
        )
      }

      data[[name]] <- text
    }

    if (nrow(data) || first) {
      utils::write.table(
        data,
        file = connection,
        sep = ",",
        row.names = FALSE,
        col.names = first,
        quote = TRUE,
        qmethod = "double",
        na = "\\N",
        eol = "\r\n"
      )
    }

    first <- FALSE

    if (DBI::dbHasCompleted(result)) {
      break
    }
  }

  if (total != expected) {
    stop(
      "CSV row count changed during export; export refused.",
      call. = FALSE
    )
  }

  close(connection)
  connection <- NULL

  DBI::dbClearResult(result)
  result <- NULL

  if (!file.copy(staging, file, overwrite = TRUE)) {
    stop(
      "The completed CSV could not be delivered.",
      call. = FALSE
    )
  }

  invisible(total)
}

# Classify filters from declared SQL types, not from displayed values.
cv_filter_kind <- function(declared_type) {
  numeric_type <- grepl(
    "^(INT|INTEGER|BIGINT|REAL|FLOAT|DOUBLE|NUMERIC|DECIMAL)",
    toupper(trimws(declared_type))
  )

  ifelse(numeric_type, "number", "text")
}

# Apply validated filters to an unfiltered catalogue plan.
cv_filter_plan <- function(con, plan, filters = list()) {
  if (is.null(filters)) {
    filters <- list()
  }

  if (!is.list(filters) || length(filters) > nrow(plan$fields)) {
    stop("Invalid catalogue filters.", call. = FALSE)
  }

  clauses <- character(0)
  params <- list()
  normalized <- list()
  seen <- character(0)

  scalar_text <- function(value, label) {
    if (
      !is.character(value) ||
        length(value) != 1L ||
        is.na(value) ||
        nchar(value, type = "chars") > 1000L
    ) {
      stop(paste("Invalid", label, "in a column filter."), call. = FALSE)
    }

    value
  }

  for (filter in filters) {
    if (!is.list(filter)) {
      stop("Invalid column filter.", call. = FALSE)
    }

    id <- scalar_text(filter$field_id, "field")
    op <- scalar_text(filter$op, "operator")

    position <- match(id, plan$fields$field_id)

    if (is.na(position) || id %in% seen) {
      stop("Unknown or repeated filter field.", call. = FALSE)
    }

    seen <- c(seen, id)

    if (op == "any") {
      next
    }

    field <- plan$fields[position, , drop = FALSE]
    kind <- cv_filter_kind(field$declared_type[[1L]])

    allowed <- if (kind == "number") {
      c("eq", "ge", "le", "between", "null")
    } else {
      c("contains", "eq", "null", "empty")
    }

    if (!op %in% allowed) {
      stop("The filter operator is not valid for this field.", call. = FALSE)
    }

    label <- paste(
      field$table_name[[1L]],
      field$column_name[[1L]],
      sep = "."
    )

    column <- paste0(
      "cv_filtered.",
      as.character(DBI::dbQuoteIdentifier(con, label))
    )

    value <- ""
    value2 <- ""

    if (op == "null") {
      clause <- paste(column, "IS NULL")
    } else if (op == "empty") {
      clause <- paste(column, "= ?")
      params <- c(params, list(""))
    } else {
      value <- scalar_text(filter$value, "value")

      if (kind == "number") {
        number <- suppressWarnings(as.numeric(trimws(value)))

        if (!nzchar(trimws(value)) || !is.finite(number)) {
          stop(
            paste("Enter a finite numeric value for", label),
            call. = FALSE
          )
        }

        if (op == "between") {
          value2 <- scalar_text(filter$value2, "upper bound")
          upper <- suppressWarnings(as.numeric(trimws(value2)))

          if (
            !nzchar(trimws(value2)) ||
              !is.finite(upper) ||
              number > upper
          ) {
            stop(
              paste("Enter an ordered numeric interval for", label),
              call. = FALSE
            )
          }

          clause <- paste(column, "BETWEEN ? AND ?")
          params <- c(params, list(number, upper))
        } else {
          operator <- switch(op, eq = "=", ge = ">=", le = "<=")
          clause <- paste(column, operator, "?")
          params <- c(params, list(number))
        }
      } else {
        if (!nzchar(value)) {
          stop(
            paste("Enter text for", label, "or use Empty string."),
            call. = FALSE
          )
        }

        clause <- if (op == "contains") {
          paste0("instr(", column, ", ?) > 0")
        } else {
          paste(column, "= ?")
        }

        params <- c(params, list(value))
      }
    }

    clauses <- c(clauses, clause)

    normalized[[length(normalized) + 1L]] <- list(
      field_id = id,
      op = op,
      value = value,
      value2 = value2
    )
  }

  if (length(normalized)) {
    order_ids <- vapply(
      normalized,
      function(filter) filter$field_id,
      character(1)
    )

    normalized <- normalized[
      order(match(order_ids, plan$fields$field_id))
    ]
  }

  filtered <- plan
  filtered$sql <- paste0(
    "SELECT * FROM (",
    plan$sql,
    ") AS cv_filtered",
    if (length(clauses)) {
      paste0(" WHERE ", paste(clauses, collapse = " AND "))
    } else {
      ""
    }
  )

  filtered$params <- params
  filtered$filters <- normalized
  filtered
}

# Build a bounded SQL request from an already validated plan.
cv_page_statement <- function(plan, after_id = NULL, limit = 101L) {
  if (
    !is.numeric(limit) ||
      length(limit) != 1L ||
      is.na(limit) ||
      !is.finite(limit) ||
      limit < 1 ||
      limit > 100001 ||
      limit != floor(limit)
  ) {
    stop("Invalid catalogue retrieval limit.", call. = FALSE)
  }

  if (
    !is.null(after_id) &&
      (!is.numeric(after_id) ||
        length(after_id) != 1L ||
        is.na(after_id) ||
        !is.finite(after_id) ||
        after_id != floor(after_id))
  ) {
    stop("Invalid catalogue page cursor.", call. = FALSE)
  }

  params <- if (is.null(plan$params)) list() else plan$params
  cursor <- ""

  if (!is.null(after_id)) {
    cursor <- ' WHERE cv_rows.".base_id" > ?'
    params <- c(params, list(after_id))
  }

  list(
    sql = paste0(
      "SELECT * FROM (",
      plan$sql,
      ") AS cv_rows",
      cursor,
      ' ORDER BY cv_rows.".base_id" LIMIT ?'
    ),
    params = c(params, list(as.integer(limit)))
  )
}
