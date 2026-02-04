get_p_values <- function(p_values) {
  if (is.vector(p_values)) {
    stopifnot(
      "`sim_power` returned a non-numeric vector of p-values" = is.numeric(
        p_values
      )
    )
    return(p_values)
  }
  stopifnot(
    "`sim_power` returned an unsupported object type for p-values" = inherits(
      p_values,
      "data.frame"
    ),
    "`sim_power` returned a data frame without a 'p' column" = "p" %in%
      colnames(p_values)
  )
  return(p_values$p)
}
