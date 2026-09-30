opti_range_extremes_refine <- function(
  new_design, opti_range, power_summary, opti, design
) {
  if (
    abs(new_design) == abs(min(opti_range)) &&
      max(abs(power_summary[, opti])) < max(abs(opti_range))
  ) {
    return(
      sign(design[[opti]]) *
        min(2 * max(abs(power_summary[, opti])), max(abs(opti_range)))
    )
  }
  if (
    abs(new_design) == abs(max(opti_range)) &&
      min(abs(power_summary[, opti])) > min(abs(opti_range))
  ) {
    return(
      sign(design[[opti]]) *
        max(min(abs(power_summary[, opti])) / 2, min(abs(opti_range)))
    )
  }
  return(integer(0))
}
