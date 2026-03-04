preprare_model_data <- function(power_summary) {
  to_remove <- max(sum(power_summary$lcl == 1) - 5, 0)
  if (to_remove > 0) {
    if (power_summary$lcl[1] == 1) {
      power_summary <- tail(power_summary, -to_remove)
    } else {
      power_summary <- head(power_summary, -to_remove)
    }
  }
  power_summary$non_signif <- power_summary$non_signif + 1
  power_summary$signif <- power_summary$signif + 1
  return(power_summary)
}
