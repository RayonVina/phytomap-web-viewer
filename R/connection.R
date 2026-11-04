# Open the project database in read-only mode.
open_database <- function(
  path = file.path("data", "phytomap.sqlite3")
) {
  required_packages <- c("DBI", "RSQLite")

  available <- vapply(
    required_packages,
    requireNamespace,
    quietly = TRUE,
    FUN.VALUE = logical(1)
  )

  if (any(!available)) {
    stop(
      "Missing required R packages: ",
      paste(required_packages[!available], collapse = ", "),
      ". Install them before running the project.",
      call. = FALSE
    )
  }

  if (!file.exists(path)) {
    stop(
      "Database file not found: ",
      path,
      ". Run this project from its root directory.",
      call. = FALSE
    )
  }

  if (dir.exists(path)) {
    stop(
      "The database path points to a directory: ",
      path,
      call. = FALSE
    )
  }

  if (file.access(path, mode = 4) != 0L) {
    stop(
      "Database file is not readable: ",
      path,
      call. = FALSE
    )
  }

  DBI::dbConnect(
    RSQLite::SQLite(),
    dbname = path,
    flags = RSQLite::SQLITE_RO
  )
}
