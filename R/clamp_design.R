#' Clamp a design parameter value to an optional range
#'
#' Restricts a candidate value for the parameter under optimisation to the
#' user defined range.
#' When no range is given, the value is returned unchanged.
#'
#' @param x Numeric. Candidate value for the parameter under optimisation.
#' @param opti_range Numeric of length 2 or `NULL`.
#' Lower and upper limit of the allowed range.
#'
#' @return Numeric. The value restricted to `opti_range`.
#' @keywords internal
#' @noRd
clamp_design <- function(x, opti_range = NULL) {
  if (is.null(opti_range) || length(x) == 0) {
    return(x)
  }
  # keep the attributes of `x` (e.g. `estimate` and `range`) intact
  attributes(x) -> x_attr
  min(max(x, min(opti_range)), max(opti_range)) -> x_clamped
  attributes(x_clamped) <- x_attr
  x_clamped
}
