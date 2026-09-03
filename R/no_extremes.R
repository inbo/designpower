#' @keywords internal
#' @importFrom ggplot2 geom_vline ggtitle
#' @importFrom stats as.formula binomial coef glm
#' @importFrom utils flush.console
no_extremes <- function(
  power_summary,
  no_small,
  p,
  opti,
  design,
  design_digits,
  opti_range
) {
  if (abs(no_small - min(power_summary$estimated_power)) < 1e-3) {
    # all estimates are either 1 or 0, so we cannot determine the direction of
    # change based on the model, so we randomly choose to increase or decrease
    # the design parameter
    decrease <- sample(c(TRUE, FALSE), 1)
  } else {
    sprintf("cbind(signif, non_signif) ~ %s", opti) |>
      as.formula() |>
      glm(
        data = power_summary,
        family = binomial
      ) -> power_model
    decrease <- xor(no_small, coef(power_model)[2] < 0)
  }
  power_summary[, opti] |>
    abs() |>
    min() -> current_min
  power_summary[, opti] |>
    abs() |>
    max() -> current_max
  ifelse(decrease, current_min / 2, current_max * 2) |>
    round(digits = design_digits[opti]) |>
    max(10^-design_digits[opti]) -> new_design
  new_design * sign(design[[opti]]) -> new_design
  if (!is.null(opti_range)) {
    new_design |>
      min(max(opti_range)) |>
      max(min(opti_range)) -> new_design
  }
  # add vertical line with the estimate to plot and print
  p <- p +
    geom_vline(xintercept = new_design, colour = "blue", linewidth = 1) +
    ggtitle(sprintf("next try: %s = %s", opti, as.character(new_design)))
  print(p)
  flush.console()
  return(new_design)
}
