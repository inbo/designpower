#' Get Design Identifier
#'
#' Retrieve or create a design identifier in the database.
#'
#' @param connection A `DuckDB`` connection object.
#' @param where_clause Character. SQL WHERE clause for filtering.
#' @param opti_clause Character. SQL clause for the optimization parameter.
#' @param design A list of design parameters.
#' @param opti Character. Name of the optimization parameter.
#' @param hashes Character vector. Hash values for design tracking.
#'
#' @return Integer. The design ID.
#' @importFrom DBI dbGetQuery
#' @importFrom duckdb dbListTables dbWriteTable
#' @keywords internal
#' @noRd
get_design_id <- function(
  connection,
  where_clause,
  opti_clause,
  design,
  opti,
  hashes
) {
  stopifnot(TRUE)
  if (!"design" %in% dbListTables(connection)) {
    design |>
      c(id = 1L, hashes) |>
      as.data.frame() |>
      dbWriteTable(conn = connection, name = "design")
    return(1L)
  }
  sprintf(
    "SELECT id FROM design AS d WHERE %s AND abs(%s - %f) < 1e-9",
    where_clause,
    opti_clause,
    design[[opti]]
  ) |>
    dbGetQuery(conn = connection) -> design_id
  stopifnot(nrow(design_id) <= 1)
  if (nrow(design_id) == 1) {
    return(design_id$id)
  }
  "SELECT max(id) + 1 AS id FROM design" |>
    dbGetQuery(conn = connection) -> design_id
  design |>
    c(id = design_id$id, hashes) |>
    as.data.frame() |>
    dbWriteTable(conn = connection, name = "design", append = TRUE)
  return(design_id$id)
}
