#' Find Optimal Design
#'
#' Iteratively search for optimal design parameters using adaptive simulation.
#'
#' Uses `DuckDB` to store simulations and gradually refines design parameters
#' to achieve target power.
#'
#' @param design List. Initial design parameters.
#' @param design_digits Named numeric. Precision (number of decimal places) for
#' each parameter.
#' The names must match those in `design`.
#' @param opti Character. Name of parameter to optimize.
#' @param sim_power Function. Simulation function (must accept `design`).
#' Must return a single p-value.
#' @param extra_args List. Optional additional arguments passed to `sim_power`.
#' Defaults to an empty list.
#' @param power Numeric. Target power (default 0.9).
#' @param alpha Numeric. Significance level (default 0.1).
#' @param filename Character. Path to `DuckDB` database file.
#' @param n_sim Integer.
#' Number of simulations to run per iteration (default 100).
#' One iteration consists of running `n_sim` simulations at the current design
#' parameter value, then updating the design parameter based on the observed
#' power.
#' @param max_sim Integer. Maximum number of simulations to consider for
#' candidate selection (default 1000).
#' @param opti_range Numeric of length 2. Optional lower and upper limit for the
#' parameter given in `opti`.
#' When specified, only values within this range are simulated and reported.
#' When `NULL` (default), the search space is unrestricted.
#'
#' @return Numeric vector. The optimized parameter value and confidence range.
#'
#' @examples
#' \dontrun{
#' find_power(
#'   design = list(trend = -0.03, n_site = 20, n_year = 12),
#'   design_digits = c(trend = 4, n_site = 0, n_year = 0),
#'   opti = "trend",
#'   sim_power = sim_power,
#'   extra_args = list()
#' )
#' }
#'
#' @importFrom digest sha1
#' @importFrom dplyr mutate
#' @importFrom duckdb dbConnect duckdb dbWriteTable
#' @importFrom mgcv gam
#' @export
find_power <- function(
  design,
  design_digits,
  opti,
  sim_power,
  extra_args = list(),
  power = 0.9,
  alpha = 0.1,
  filename = "power.duckdb",
  n_sim = 100,
  max_sim = 1000,
  opti_range = NULL
) {
  stopifnot(
    is.list(design),
    length(design) >= 1,
    "`design` has no names" = is.character(names(design)),
    "`design_digits` has no names" = is.character(names(design_digits)),
    "every name in `design` must be present in `design_digits`" = all(
      names(design) %in% names(design_digits)
    ),
    length(opti) == 1,
    is.character(opti),
    all(opti %in% names(design)),
    is.function(sim_power),
    "`sim_power` must have a `design` argument" = "design" %in%
      names(formals(sim_power)),
    is.list(extra_args),
    is.character(filename),
    length(filename) == 1,
    is.numeric(alpha),
    length(alpha) == 1,
    is.numeric(power),
    length(power) == 1,
    0 < alpha,
    alpha < power,
    power < 1,
    is.numeric(n_sim),
    length(n_sim) == 1,
    n_sim > 0,
    is.numeric(max_sim),
    length(max_sim) == 1,
    n_sim <= max_sim
  )
  if (!is.null(opti_range)) {
    stopifnot(
      "`opti_range` must be a numeric vector of length 2" = (is.numeric(
        opti_range
      ) &&
        length(opti_range) == 2),
      "`opti_range` must have two different values" = min(opti_range) <
        max(opti_range),
      "`opti_range` must not include zero" = min(abs(opti_range)) > 0,
      "`opti_range` must have the same sign" = prod(sign(opti_range)) == 1
    )
    opti_range <- round(opti_range, digits = design_digits[[opti]]) |>
      sort()
    # make sure the starting value lies within the requested range
    design[[opti]] <- min(max(design[[opti]], opti_range[1]), opti_range[2])
  }

  design <- vapply(
    names(design),
    function(x) {
      round(design[[x]], digits = design_digits[[x]]) |>
        list()
    },
    vector("list", 1)
  )
  connection <- dbConnect(duckdb(), filename)

  fixed_design <- design[!names(design) %in% opti]
  hashes <- c(
    extra_args_hash = sha1(extra_args),
    sim_power_hash = sha1(sim_power)
  )

  sprintf(
    "abs(d.%s - %f) < 1e%i",
    names(fixed_design),
    fixed_design,
    -design_digits[names(fixed_design)]
  ) |>
    c(
      sprintf("d.%s = '%s'", names(hashes), hashes),
      sprintf("%s %s 0", opti, ifelse(design[[opti]] > 0, ">", "<"))
    ) |>
    paste(collapse = " AND ") -> where_clause
  if (!is.null(opti_range)) {
    where_clause <- sprintf(
      "%s AND d.%s BETWEEN %.15g AND %.15g",
      where_clause,
      opti,
      min(opti_range),
      max(opti_range)
    )
  }

  paste0("d.", opti, collapse = ", ") -> opti_clause

  design_id <- get_design_id(
    connection = connection,
    where_clause = where_clause,
    opti_clause = opti_clause,
    design = design,
    opti = opti,
    hashes = hashes
  )
  observed_power(
    connection = connection,
    alpha = alpha,
    where_clause = where_clause,
    opti_clause = opti_clause
  ) |>
    sample_new_design(
      design = design,
      design_digits = design_digits,
      opti = opti,
      power = power,
      max_sample = max_sim,
      opti_range = opti_range
    ) -> new_design
  while (length(new_design) > 0) {
    design[[opti]] <- new_design
    message(design[[opti]])
    design_id <- get_design_id(
      connection = connection,
      where_clause = where_clause,
      opti_clause = opti_clause,
      design = design,
      opti = opti,
      hashes = hashes
    )
    replicate(n_sim, {
      list(design = design, n_sim = 100) |>
        c(extra_args) |>
        do.call(what = sim_power)
    }) |>
      unlist() -> p_values
    stopifnot(
      "`sim_power` must return a single p-value" = is.vector(p_values),
      "`sim_power` must return a number" = inherits(p_values, "numeric"),
      "`sim_power` must return non-negative p-values" = all(p_values >= 0),
      "`sim_power` must return p-values not above 1" = all(p_values <= 1)
    )
    data.frame(design_id = design_id, p = p_values) |>
      dbWriteTable(conn = connection, name = "simulations", append = TRUE)
    observed_power(
      connection = connection,
      alpha = alpha,
      where_clause = where_clause,
      opti_clause = opti_clause
    ) |>
      sample_new_design(
        design = design,
        design_digits = design_digits,
        opti = opti,
        power = power,
        max_sample = max_sim,
        opti_range = opti_range
      ) -> new_design
  }
  c(attr(new_design, "estimate"), attr(new_design, "range")) |>
    `attr<-`("design", design[names(design) != opti]) |>
    `attr<-`("opti", opti) |>
    `attr<-`("extra_args", extra_args)
}
