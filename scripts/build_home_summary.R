source(file.path("R", "connection.R"))
source(file.path("R", "queries.R"))

build_home_summary <- function() {
  database_path <- file.path("data", "phytomap.sqlite3")
  output_path <- file.path("data", "home_summary.rds")

  before <- file.info(database_path)

  con <- open_database(database_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  started <- proc.time()[["elapsed"]]
  summary <- get_home_plot_summary(con)

  after <- file.info(database_path)

  if (
    before$size != after$size ||
      as.numeric(before$mtime) != as.numeric(after$mtime)
  ) {
    stop("The database changed while building the Home summary.")
  }

  result <- list(
    schema_version = 2L,
    database_size = unname(after$size),
    database_mtime = as.numeric(after$mtime),
    calculated_at = Sys.time(),
    query_seconds = proc.time()[["elapsed"]] - started,
    summary = summary
  )

  saveRDS(result, output_path)

  message(
    "Home summary saved. Query time: ",
    sprintf("%.3f", result$query_seconds),
    " seconds."
  )

  invisible(result)
}

build_home_summary()
