#' Get the midpoint
#'
#' in the corner case that we only have two design parameter values, one low
#' and one high, we cannot fit a model, so we just return the average of the
#' two design parameter values that are below and above the target power,
#' respectively
#' @noRd
#' @keywords internal
#' @importFrom ggplot2 geom_vline ggtitle
#' @importFrom utils flush.console
midpoint <- function(power_summary, p, power, design_digits, opti) {
  c(
    power_summary[power_summary$ucl < power, opti],
    power_summary[power_summary$lcl > power, opti]
  ) |>
    mean() |>
    round(digits = design_digits[opti]) -> new_design
  p <- p +
    geom_vline(xintercept = new_design, colour = "blue", linewidth = 1) +
    ggtitle(sprintf("next try: %s = %s", opti, as.character(new_design)))
  print(p)
  flush.console()
  return(new_design)
}
