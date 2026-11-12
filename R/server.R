# Own the session connection and register page-specific server logic.
server <- function(input, output, session) {
  con <- tryCatch(open_database(), error = function(e) {
    message("Database connection failed: ", conditionMessage(e))
    NULL
  })
  if (!is.null(con)) {
    session$onSessionEnded(function() {
      if (DBI::dbIsValid(con)) DBI::dbDisconnect(con)
    })
  }
  register_home_server(input, output, session, con)
  register_catalog_server(input, output, session, con)
  register_map_server(input, output, session, con)
  register_sources_server(input, output, session, con)
}
