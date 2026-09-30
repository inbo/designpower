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
  opti_range,
  power,
  max_sample
) {
  current_range <- range(power_summary[, opti])
  if (
    nrow(power_summary) < 5 || diff(range(power_summary$estimated_power)) < 0.1
  ) {
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
    xor(no_small, coef(power_model)[2] > 0) |>
      xor(current_range[1] > 0) -> decrease
  }
  current_range |>
    abs() |>
    min() -> current_min
  current_range |>
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
    existing <- which(power_summary[, opti] == new_design)
    if (
      length(existing) &&
        (
          power_summary[existing, "n_sim"] >= max_sample ||
            power_summary[existing, "lcl"] > power ||
            power_summary[existing, "ucl"] < power # fmt: skip
        )
    ) {
      if (
        abs(new_design) == abs(min(opti_range)) &&
          max(abs(power_summary[, opti])) < max(abs(opti_range))
      ) {
        new_design <- sign(design[[opti]]) *
          min(2 * max(abs(power_summary[, opti])), max(abs(opti_range)))
      } else {
        new_design <- integer(0)
      }
    }
  }
  # add vertical line with the estimate to plot and print
  p <- p +
    geom_vline(xintercept = new_design, colour = "blue", linewidth = 1) +
    ggtitle(sprintf("next try: %s = %s", opti, as.character(new_design)))
  print(p)
  flush.console()

  return(
    new_design |>
      `attr<-`("estimate", NA_real_) |>
      `attr<-`("range", c(NA_real_, NA_real_))
  )
}
