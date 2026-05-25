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
#' @importFrom dplyr across bind_rows filter first lag last lead left_join
#' @importFrom dplyr mutate select slice_sample
#' @importFrom ggplot2 aes geom_blank geom_errorbar geom_hline geom_line
#' @importFrom ggplot2 geom_point geom_rect geom_ribbon geom_vline ggplot
#' @importFrom ggplot2 ggtitle scale_x_continuous scale_y_continuous
#' @importFrom mgcv gam
#' @importFrom scales percent
#' @importFrom rlang .data sym !!
#' @importFrom stats as.formula binomial coef glm plogis predict qnorm
#' @importFrom tidyr replace_na
#' @importFrom utils head flush.console tail
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
  if (no_small || no_large) {
    # all estimates are either 1 or 0, so we cannot determine the direction of
    # change based on the model, so we randomly choose to increase or decrease
    # the design parameter
    if (abs(min(power_summary$estimated_power) - no_small) < 1e-9) {
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
    # add vertical line with the estimate to plot and print
    p <- p +
      geom_vline(xintercept = new_design, colour = "blue", linewidth = 1) +
      ggtitle(sprintf("next try: %s = %s", opti, as.character(new_design)))
    print(p)
    flush.console()
    return(new_design)
  }
  # in the corner case that we only have two design parameter values, one low
  # and one high, we cannot fit a model, so we just return the average of the
  # two design parameter values that are below and above the target power,
  # respectively
  if (nrow(power_summary) == 2) {
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
  power_summary <- preprare_model_data(power_summary)
  try(
    sprintf("cbind(signif, non_signif) ~ s(%s, bs = \"cs\", k = 3)", opti) |>
      as.formula() |>
      gam(data = power_summary, family = binomial()),
    silent = TRUE
  ) -> power_model
  if (inherits(power_model, "try-error")) {
    sprintf("cbind(signif, non_signif) ~ s(%s, bs = \"cs\", k = 4)", opti) |>
      as.formula() |>
      gam(data = power_summary, family = binomial()) -> power_model
  }
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
        select(!!opti, "n_sim", lower = "lcl", upper = "ucl"),
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
      .data$lower < power,
      power < .data$upper,
      .data$n_sim < max_sample
    ) |>
    bind_rows(
      predict_data |>
        filter(
          lag(.data$lcl, 1, first(.data$lcl)) < power,
          lead(.data$ucl, 1, last(.data$ucl)) >= power,
          .data$n_sim < max_sample
        )
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
    slice_sample(n = 1, weight_by = max_sample - .data$n_sim) -> new_design
  new_design <- unlist(new_design[[opti]])
  if (head(predict_data$fit, 1) < tail(predict_data$fit, 1)) {
    sign(design[[opti]]) *
      c(
        min(abs(predict_data[power < predict_data$lcl, opti])),
        min(abs(predict_data[power < predict_data$ucl, opti]))
      ) |>
        range() -> attr(new_design, "range")
  } else {
    sign(design[[opti]]) *
      c(
        max(abs(predict_data[power < predict_data$lcl, opti])),
        max(abs(predict_data[power < predict_data$ucl, opti]))
      ) |>
        range() -> attr(new_design, "range")
  }
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
    ) +
    scale_x_continuous(limits = range(c(0, predict_data[[opti]])))
  print(p)
  flush.console()
  return(new_design)
}
