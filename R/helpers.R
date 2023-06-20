#' Derive slope of numerical vector across a time series
#'
#' @param x vector of timeseries values
#'
#' @return TODO
#'
#' @export
ts_slope <- function(x) {
  y <- which(!is.na(x))
  if (length(y) >= 2) {
    x <- unlist(x[!is.na(x)])
    if (length(unique(x)) == 1) {
      return(0)
    } else {
      return(sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2))
    }
  } else {
    return(NA)
  }
}

#' Number of non-`NA` values in a sample
#'
#' @param x vector of timeseries values
#'
#' @return TODO
#'
#' @export
ts_nsamp <- function(x) {
  x <- sum(!is.na(x))
  if (x > 1) {
    return(x)
  } else {
    return(NA)
  }
}
