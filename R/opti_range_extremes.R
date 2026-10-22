#' @importFrom dplyr slice_max
opti_range_extremes <- function(
  new_design, opti_range = NULL, power_summary, opti, max_sample, design, power
) {
  if (is.null(opti_range)) {
    return(new_design)
  }
  new_design |>
    min(max(opti_range)) |>
    max(min(opti_range)) -> new_design
  existing <- which(power_summary[, opti] == new_design)
  if (length(existing) == 0) {
    return(new_design)
  }
  if (
    power_summary[existing, "n_sim"] < max_sample &&
      power_summary[existing, "lcl"] < power &&
      power_summary[existing, "ucl"] > power # fmt: skip
  ) {
    return(new_design)
  }
  opti_range_extremes_refine(
    new_design = new_design, opti_range = opti_range,
    power_summary = power_summary, opti = opti, design  = design
  )
}
