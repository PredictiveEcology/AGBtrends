`%||%` <- getFromNamespace("%||%", "ggplot2")

#' Determine the number of available cores for parallel processing
#'
#' Will return at least one core, up to a maximum of `prop` the available CPU cores.
#'
#' @param maxCores the maximum number or proportion of cores to use.
#'
#' @return integer
#'
#' @export
getNumCores <- function(maxCores = 0.5) {
  stopifnot(maxCores > 0)

  if (maxCores < 1) {
    as.integer(maxCores * parallelly::availableCores(constraints = "connections"))
  } else {
    min(maxCores, parallelly::availableCores(constraints = "connections"))
  } |>
    min(getOption("parallelly.availableCores.fallback", NA_integer_), na.rm = TRUE) |>
    max(1L) ## ensure at least one core returned
}
