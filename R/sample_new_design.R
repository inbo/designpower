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
#'
#' @return Numeric. The next design parameter value to test, or empty vector if
#'   converged to target power.
#'
#' @importFrom dplyr across filter lag lead left_join mutate select slice_sample
#' @importFrom ggplot2 aes geom_errorbar geom_hline geom_line geom_point
#' geom_rect geom_ribbon geom_vline ggplot ggtitle scale_y_continuous
#' @importFrom mgcv gam
#' @importFrom scales percent
#' @importFrom rlang .data sym
#' @importFrom stats as.formula binomial plogis predict qnorm
#' @importFrom tidyr replace_na
#' @importFrom utils flush.console
#' @keywords internal
#' @noRd
sample_new_design <- function(
  power_summary,
  design,
  opti,
  design_digits,
  power = 0.9
) {
  stopifnot(
    length(opti) == 1
  )
  if (nrow(power_summary) == 0) {
    return(design[[opti]])
  }
  p <- ggplot(power_summary, aes(x = !!sym(opti))) +
    geom_hline(yintercept = power, linetype = 2) +
    geom_errorbar(aes(ymin = .data$lcl, ymax = .data$ucl)) +
    geom_point(aes(y = .data$estimated_power)) +
    scale_y_continuous("Estimated power", limits = c(0, 1), labels = percent)
  if (0.5 < min(power_summary$ucl)) {
    power_summary[, opti] |>
      abs() |>
      min() -> current_min
    round(current_min / 2, digits = design_digits[opti]) |>
      max(10^-design_digits[opti]) -> new_design
    new_design * sign(design[[opti]]) -> new_design
    p <- p +
      geom_vline(xintercept = new_design, colour = "blue", linewidth = 1) +
      ggtitle(sprintf("next try: %s = %s", opti, as.character(new_design)))
    print(p)
    flush.console()
    return(new_design)
  }
  if (max(power_summary$lcl) < power) {
    power_summary[, opti] |>
      abs() |>
      max() -> current_max
    round(current_max * 2, digits = design_digits[opti]) |>
      max(10^-design_digits[opti]) -> new_design
    new_design * sign(design[[opti]]) -> new_design
    p <- p +
      geom_vline(xintercept = new_design, colour = "blue", linewidth = 1) +
      ggtitle(sprintf("next try: %s = %s", opti, as.character(new_design)))
    print(p)
    flush.console()
    return(new_design)
  }
  if (nrow(power_summary) <= 2) {
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
  power_summary$non_signif <- pmax(power_summary$non_signif, 1)
  power_summary$signif <- pmax(power_summary$signif, 1)
  sprintf("cbind(signif, non_signif) ~ s(%s, bs = \"cs\", k = 3)", opti) |>
    as.formula() |>
    gam(data = power_summary, family = binomial()) -> power_model
  data.frame(
    x = seq(
      pmax(min(abs(power_summary[, opti])), 10^(-design_digits[[opti]])),
      max(abs(power_summary[, opti])) * 1.05,
      by = 10^(-design_digits[[opti]])
    ) *
      sign(design[[opti]])
  ) |>
    `colnames<-`(opti) |>
    left_join(
      power_summary |>
        select(!!opti, "n_sim"),
      by = opti
    ) |>
    mutate(n_sim = replace_na(.data$n_sim, 0)) -> predict_data
  prediction <- predict(
    object = power_model,
    newdata = predict_data,
    se.fit = TRUE
  )
  predict_data |>
    mutate(
      fit = qnorm(0.5, prediction$fit, prediction$se.fit),
      lcl = qnorm(0.025, prediction$fit, prediction$se.fit),
      ucl = qnorm(0.975, prediction$fit, prediction$se.fit),
      across(c("fit", "lcl", "ucl"), plogis)
    ) -> predict_data
  predict_data |>
    filter(
      lag(.data$lcl, 1, min(.data$lcl)) < power,
      lead(.data$ucl, 1, max(.data$ucl)) >= power,
      .data$n_sim < 1000
    ) -> candidate
  while (nrow(candidate) >= 50) {
    candidate |>
      mutate(
        subset = as.character(!!sym(opti)) |>
          nchar()
      ) |>
      filter(.data$subset < max(.data$subset)) -> candidate
  }
  candidate |>
    slice_sample(n = 1, weight_by = 1000 - .data$n_sim) -> new_design
  new_design <- unlist(new_design[[opti]])
  sign(design[[opti]]) *
    c(
      min(abs(predict_data[power < predict_data$lcl, opti])),
      min(abs(predict_data[power < predict_data$ucl, opti]))
    ) |>
      range() -> attr(new_design, "range")
  attr(new_design, "estimate") <- predict_data[
    which.min((predict_data$fit - power)^2),
    opti
  ]
  p <- p +
    geom_ribbon(
      data = predict_data,
      aes(ymin = .data$lcl, ymax = .data$ucl),
      alpha = 0.1
    ) +
    geom_line(data = predict_data, aes(y = .data$fit)) +
    geom_rect(
      xmin = attr(new_design, "range")[1],
      xmax = attr(new_design, "range")[2],
      ymin = -Inf,
      ymax = Inf,
      alpha = 0.05,
      colour = NA,
      fill = "darkgreen"
    ) +
    geom_vline(
      xintercept = attr(new_design, "estimate"),
      colour = "darkgreen"
    ) +
    geom_vline(xintercept = new_design, colour = "blue", linewidth = 1) +
    ggtitle(
      sprintf(
        "current estimate: %s = %s (%s; %s); next try: %s = %s",
        opti,
        as.character(attr(new_design, "estimate")),
        as.character(attr(new_design, "range")[1]),
        as.character(attr(new_design, "range")[2]),
        opti,
        as.character(new_design)
      )
    )
  print(p)
  flush.console()
  return(new_design)
}
