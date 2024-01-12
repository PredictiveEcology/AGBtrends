#' Calculate slopes and number of samples of timeseries
#'
#' - `ts_slope()` derives slope of numerical vector across a time series;
#' - `ts_nsamp()` calculates the number of non-`NA` values in a sample;
#'
#' @param x vector of timeseries values
#'
#' @return TODO
#'
#' @export
#' @rdname timeseries
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

#' @export
#' @rdname timeseries
ts_nsamp <- function(x) {
  x <- sum(!is.na(x))
  if (x > 1) {
    return(x)
  } else {
    return(NA)
  }
}

#' Geographically Weighted Regression
#'
#' Estimate cell-wise linear regression coefficients for undisrupted time series
#' (a.k.a. "local" or "geographically weighted regression (GWR)").
#'
#' `gwr()` operates on the entire timeseries, whereas `gwrt()` operates on the `intervals`.
#'
#' @param tileDirs character. vector of directory paths to input raster tiles.
#'
#' @param type character. one of `"sample_size"` or `"slopes"`.
#'
#' @param cores positive integer. number of cores to use for parallel processing.
#'
#' @param prefix character. output filename prefix.
#'               default `NULL` automatically sets this based on `type`.
#'
#' @param intervals named list. timeseries intervals, e.g. `list(t1 = 1:5, t2 = 6:10, ...)`.
#'
#' @param lyrs positive integer. indices corresponding to raster layers for specific `intervals`.
#'
#' @return character. vector of file paths corresponding to output rasters.
#'
#' @export
#' @rdname gwr
gwr <- function(tileDirs, type = NULL, cores = 1L, prefix = NULL, lyrs = NULL) {
  stopifnot(!is.null(type))
  stopifnot(type %in% c("sample_size", "slopes"))

  if (type == "sample_size") {
    dt <- "INT1U"
    fn <- ts_nsamp
  } else if (type == "slopes") {
    dt <- "FLT4S"
    fn <- ts_slope
  }

  prefix <- prefix %||% paste0("agb_", type)

  ## TODO: per `?terra::app` it usually better to parallelize tile processing rather
  ## than using multiple cores in app() when the function is simple/fast, as here.

  outputFiles <- vapply(tileDirs, function(d) {
    fin <- fs::dir_ls(d, regexp = "ragb", type = "file")
    r <- terra::rast(fin)

    if (!is.null(lyrs)) {
      r <- r[[lyrs]]
    }

    fout <- file.path(d, paste0(prefix, "_", basename(d), ".tif"))
    terra::app(
      r,
      fun = fn,
      cores = cores, ## TODO: per above, parallelize across tiles, not chunks within in tile
      filename = fout,
      overwrite = TRUE,
      wopt = list(datatype = dt)
    )
    return(fout)
  }, character(1))

  return(outputFiles)
}

#' @export
#' @rdname gwr
gwrt <- function(tileDirs, type = NULL, cores = 1L, prefix = NULL, intervals = NULL) {
  stopifnot(!is.null(type), !is.null(intervals))
  stopifnot(type %in% c("sample_size", "slopes"))

  vapply(seq_along(intervals), function(timestep) {
    prefix <- prefix %||% paste0("agb_", type, "_", names(intervals)[timestep])

    gwr(tileDirs = tileDirs, type = type, cores = cores, prefix = prefix, lyrs = intervals[[timestep]])
  }, character(length(tileDirs))) |>
    as.vector()
}
