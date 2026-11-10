# Apply display formatting without changing the underlying table values.
format_result_table <- function(table, data) {
  decimal_columns <- names(data)[
    vapply(
      data,
      function(value) {
        is.double(value) &&
          !is.object(value) &&
          any(
            is.finite(value) & value != trunc(value),
            na.rm = TRUE
          )
      },
      logical(1)
    )
  ]

  if (length(decimal_columns)) {
    table <- DT::formatRound(
      table,
      columns = decimal_columns,
      digits = 4,
      mark = "",
      dec.mark = "."
    )
  }

  table
}
