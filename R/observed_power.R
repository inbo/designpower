#' Calculate Observed Power
#'
#' Query a `DuckDB` database to calculate observed power from simulations.
#'
#' @param connection A `DuckDB` connection object.
#' @param alpha Numeric. Significance level (between 0 and 1).
#' @param opti_clause Character. SQL clause for the optimization parameter.
#' @param where_clause Character. SQL WHERE clause for filtering.
#'
#' @return A data frame with columns:
#'   - All columns specified in `opti_clause`
#'   - `signif`: Number of significant p-values
#'   - `non_signif`: Number of non-significant p-values
#'   - `n_sim`: Total number of simulations
#'   - `estimated_power`: Estimated power (`signif / n_sim`)
#'   - `lcl`: Lower confidence limit (0.025 quantile)
#'   - `ucl`: Upper confidence limit (0.975 quantile)
#'
#' @importFrom DBI dbGetQuery
#' @importFrom duckdb dbListTables
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @importFrom stats qbinom
#' @keywords internal
#' @noRd
observed_power <- function(connection, alpha, opti_clause, where_clause) {
  stopifnot(
    alpha > 0,
    alpha < 1
  )
  if (!"simulations" %in% dbListTables(connection)) {
    return(data.frame())
  }
  "SELECT %1$s, SUM(p < %2$f) AS signif, COUNT(p) AS n_sim
FROM simulations AS p
INNER JOIN design AS d ON p.design_id = d.id
WHERE %3$s
GROUP BY %1$s
ORDER BY %1$s" |>
    sprintf(opti_clause, alpha / 2, where_clause) |>
    dbGetQuery(conn = connection) |>
    mutate(
      non_signif = .data$n_sim - .data$signif,
      estimated_power = .data$signif / .data$n_sim,
      lcl = qbinom(0.025, size = .data$n_sim, prob = .data$estimated_power) /
        .data$n_sim,
      ucl = qbinom(0.975, size = .data$n_sim, prob = .data$estimated_power) /
        .data$n_sim
    )
}
