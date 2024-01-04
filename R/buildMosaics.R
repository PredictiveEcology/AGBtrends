#' Build raster mosaics for each time interval
#'
#' @param type character string specifying the mosaic type to build.
#'             one of `"age", "slope", "sample_size"`.
#' @param intervals named list of time intervals over which to build mosaics
#'
#' @param paths named list of directory paths, specifying paths for at least:
#'              `scratch` and `tiles`.
#'
#' @param cl cluster object.
#'
#' @return character vector of output raster file names (`.tif`).
#'         Invoked for side effects of building and writing raster mosaics to disk.
#'
#' @export
buildMosaics <- function(type, intervals, paths, cl = NULL) {
  stopifnot(type %in% c("age", "slope", "sample_size"))

  cores <- length(intervals)

  if (is.null(cl)) {
    cl <- parallelly::makeClusterPSOCK(cores,
                                       default_packages = c("sf", "stringr", "terra"),
                                       rscript_libs = .libPaths(),
                                       autoStop = TRUE)
    on.exit(stopCluster(cl), add = TRUE)
  }

  clusterExport(cl, varlist = c("cores", "paths"))

  parallel::clusterEvalQ(cl, {
    terra::terraOptions(memmax = 25,
                        memfrac = 0.6 / cores,
                        progress = 1,
                        verbose = TRUE)
  })

  parLapply(cl, names(intervals), function(tp) {
    td <- terra::terraOptions(print = FALSE)[["tempdir"]]
    od <- paths[["outputs"]]

    ## Build virtual rasters
    flist <- sapply(paths[["tiles"]], function(dsn) fs::dir_ls(dsn, regexp = tp)) |> unname()

    if (length(intervals) == 1) {
      vrts <- file.path(td, paste0("AGB_", type, "_mosaic.vrt"))
      tifs <- file.path(od, paste0("AGB_", type , "_mosaic.tif"))
    } else {
      vrts <- file.path(td, paste0("AGB_", type, "_mosaic_", tp, ".vrt"))
      tifs <- file.path(od, paste0("AGB_", type , "_mosaic_", tp, ".tif"))
    }

    sf::gdal_utils(
      util = "buildvrt",
      source = flist[stringr::str_detect(flist, type)],
      destination = vrts
    )

    ## Write to raster mosaics
    sf::gdal_utils(util = "warp", source = vrts, destination = tifs)

    return(tifs)
  }) |>
    unlist()
}
