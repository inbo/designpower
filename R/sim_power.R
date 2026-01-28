#' Simple Power Simulation
#'
#' A basic example of a power simulation function.
#'
#' Simulates data under a simple model and returns p-values.
#'
#' @param design A named list with design parameters:
#'   - `trend`: The true trend parameter
#'   - `n_year`: Number of years
#'   - `n_sample`: Number of samples per year
#' @param n_sim Integer. Number of simulations.
#' @param intercept Numeric. Intercept of the model.
#' @param sigma_error Numeric. Standard deviation of the error term.
#'
#' @return A data frame with columns:
#'   - `p`: P-values from simulations
#'   - `trend`: Estimated trends
#'
#' @examples
#' \dontrun{
#' sim_power(
#'   design = list(trend = -0.03, n_year = 12, n_sample = 20),
#'   n_sim = 10
#' )
#' }
#'
#' @export
sim_power <- function(
  design = list(trend = -0.03, n_year = 12, n_sample = 20),
  n_sim = 100,
  intercept = 2,
  sigma_error = 0.1
) {
  # Simple simulation: generate random p-values and estimate trend
  replicate(n_sim, {
    dataset <- expand.grid(
      year = seq_len(design[["n_year"]]) - 1,
      sample = seq_len(design[["n_sample"]])
    )
    dataset$y <- intercept +
      design[["trend"]] * dataset$year +
      stats::rnorm(nrow(dataset), mean = 0, sd = sigma_error)
    model <- stats::lm(y ~ year, data = dataset)
    summary(model)[]$coefficients[2, 4]
  })
}
