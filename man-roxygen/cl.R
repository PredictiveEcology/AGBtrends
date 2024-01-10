#' @param cl cluster object. if `NULL` (default), a cluster will be created using up to
#'           `length(intervals)` number of CPU cores for parallel computation.
#'           Users can pass their own `cl` object or specify option
#'           `parallelly.availableCores.fallback` to reduce the number of cores used.
#'           See `?parallelly.options`.
