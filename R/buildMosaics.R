#' Build raster mosaics for each time interval
#'
#' @param intervals named list of time intervals over which to build mosaics
#'
#' @param paths named list of directory paths, specifying paths for at least:
#'              `scratch` and `tiles`.
#'
#' @param cores integer specifying the number of cores to use for parallel computations
#'
#' @return `NULL`, invisibly. Invoked for side effects of building and writing raster mosaics
#'         to disk.
#'
#' @export
buildMosaics <- function(intervals, paths, cores) {
  ## TODO: build non-time interval version too

  ## TODO: use future.apply
  cl <- parallelly::makeClusterPSOCK(cores,
                                     default_packages = c("sf", "stringr", "terra"),
                                     rscript_libs = .libPaths(),
                                     autoStop = TRUE)
  on.exit(stopCluster(cl), add = TRUE)

  clusterExport(cl, varlist = c("cores", "paths"))

  parallel::clusterEvalQ(cl, {
    terra::terraOptions(tempdir = paths[["scratch"]], ## TODO: use terra path?
                        memmax = 25,
                        memfrac = 0.6 / cores,
                        progress = 1,
                        verbose = TRUE)
  })

  parLapply(cl, names(intervals), function(tp) {
    td <- terra::terraOptions(print = FALSE)[["tempdir"]]
    od <- paths[["outputs"]]

    ## 2.2.1) Build virtual rasters
    flist <- unname(sapply(paths[["tiles"]], function(dsn) file.path(dsn, list.files(dsn, pattern = tp))))
    vrts <- file.path(td, c(
      paste0("AGB_slope_mosaic_", tp, ".vrt"),
      paste0("AGB_sample_size_mosaic_", tp, ".vrt")
    ))
    tifs <- file.path(od, c(
      paste0("AGB_slope_mosaic_", tp, ".tif"),
      paste0("AGB_sample_size_mosaic_", tp, ".tif")
    ))

    sf::gdal_utils(util = "buildvrt",
                   source = flist[str_detect(flist, "slope")],
                   destination = vrts[1])

    sf::gdal_utils(util = "buildvrt",
                   source = flist[str_detect(flist, "sample_size")],
                   destination = vrts[2])

    ## 2.2.2) Write to raster mosaics
    sf::gdal_utils(util = "warp", source = vrts[1], destination = tifs[1])

    sf::gdal_utils(util = "warp", source = vrts[2], destination = tifs[2])

    return(invisible(NULL))
  })
}
