#' @importFrom dplyr across bind_rows filter first lag last lead left_join
#' @importFrom dplyr mutate select slice_sample
#' @importFrom ggplot2 aes geom_line geom_rect geom_ribbon geom_vline ggtitle
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom mgcv gam
#' @importFrom stats as.formula binomial plogis predict qnorm
#' @importFrom rlang .data sym !!
#' @importFrom tidyr replace_na
#' @importFrom utils head tail
#' @keywords internal
opti_model <- function(
  power_summary,
  p,
  power,
  design,
  opti,
  max_sample,
  design_digits,
  opti_range = NULL
) {
  if (is.null(opti_range)) {
    opti_range <- c(0, Inf)
  } else {
    abs(opti_range) |>
      range() -> opti_range
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
      max(
        min(abs(power_summary[, opti])),
        10^(-design_digits[[opti]]),
        opti_range[1]
      ),
      min(max(abs(power_summary[, opti])) * 1.05, opti_range[2]),
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
  # only keep candidate values within the user defined range
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
