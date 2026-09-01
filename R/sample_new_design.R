#' Sample New Design
#'
#' Determine next design parameters based on power summary using GAM modelling.
#'
#' @param power_summary Data frame. Summary of observed power from previous
#' simulations.
#' @param design List. Current design parameters.
#' @param opti Character. Name of parameter to optimize.
#' @param design_digits Named numeric. Precision for each design parameter.
#' @param power Numeric. Target power (default 0.9).
#' @param max_sample Numeric. Maximum number of simulations to consider for
#'  candidate selection (default 1000).
#'
#' @return Numeric. The next design parameter value to test, or empty vector if
#'   converged to target power.
#'
#' @importFrom ggplot2 aes geom_blank geom_errorbar geom_hline  geom_point
#' @importFrom ggplot2 ggplot scale_y_continuous
#' @importFrom scales percent
#' @importFrom rlang .data sym !!
#' @keywords internal
#' @noRd
sample_new_design <- function(
  power_summary,
  design,
  opti,
  design_digits,
  power = 0.9,
  max_sample = 1000
) {
  stopifnot(length(opti) == 1)
  # empty power summary means we are at the first iteration, so return the
  # initial design parameter
  if (nrow(power_summary) == 0) {
    return(design[[opti]])
  }
  if (nrow(power_summary) == 1) {
    return(round(2 * power_summary[, opti], digits = design_digits[opti]))
  }

  # determine if we have sufficient simulations for each design parameter value
  power_summary$samples <- ifelse(
    (power_summary$non_signif + power_summary$signif >= max_sample) |
      power_summary$ucl < power |
      power < power_summary$lcl,
    "sufficient",
    "insufficient"
  )
  # prepare the plot
  p <- ggplot(power_summary, aes(x = !!sym(opti))) +
    geom_hline(yintercept = power, linetype = 2) +
    geom_errorbar(aes(
      ymin = .data$lcl,
      ymax = .data$ucl,
      colour = .data$samples
    )) +
    geom_point(aes(
      y = .data$estimated_power,
      colour = .data$samples,
      shape = .data$samples
    )) +
    geom_blank(data = data.frame(x = 0, y = 0), aes(x = .data$x, y = .data$y)) +
    scale_y_continuous("Estimated power", limits = c(0, 1), labels = percent)
  # check if we have both low and high power estimates
  # if not expand the search space by doubling the largest or halving the
  # smallest design parameter value
  no_small <- 0.5 < min(power_summary$ucl)
  no_large <- max(power_summary$lcl) < power
  situation <- ifelse(
    no_small || no_large,
    "no_extremes",
    ifelse(nrow(power_summary) == 2, "midpoint", "model")
  )
  switch(
    situation,
    "no_extremes" = no_extremes(
      p = p,
      power_summary = power_summary,
      no_small = no_small,
      opti = opti,
      design = design,
      design_digits = design_digits
    ),
    "midpoint" = midpoint(
      p = p,
      power_summary = power_summary,
      power = power,
      design_digits = design_digits,
      opti = opti
    ),
    "model" = opti_model(
      p = p,
      power_summary = power_summary,
      power = power,
      design = design,
      design_digits = design_digits,
      opti = opti,
      max_sample = max_sample
    )
  )
}
