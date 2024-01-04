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

  parLapply(cl, seq(length(intervals)), function(i) {
    td <- terra::terraOptions(print = FALSE)[["tempdir"]]
    od <- paths[["outputs"]]

    ## Build virtual rasters
    flist <- sapply(paths[["tiles"]], function(dsn) fs::dir_ls(dsn, regexp = i)) |> unname()

    if (length(intervals) == 1) {
      vrts <- file.path(td, paste0("AGB_", type, "_mosaic.vrt"))
      tifs <- file.path(od, paste0("AGB_", type , "_mosaic.tif"))
    } else {
      vrts <- file.path(td, paste0("AGB_", type, "_mosaic_t", i, ".vrt"))
      tifs <- file.path(od, paste0("AGB_", type , "_mosaic_t", i, ".tif"))
    }

    if (type == "age") {
      lyrs <- sapply(intervals, head, n = 1L) |> unname()
    }

    sf::gdal_utils(
      util = "buildvrt",
      source = flist[stringr::str_detect(flist, type)],
      destination = vrts,
      if (type == "age") options = c("-b", lyrs[i]) # time 1 = 1984 etc.
    )

    ## Write to raster mosaics
    sf::gdal_utils(util = "warp", source = vrts, destination = tifs)

    if (type == "age") {
      ## Group into discrete age classes
      ages_from <- c( 0, 25, 50,  80, 125) ## TODO: allow custom age classes
      ages_to   <- c(24, 49, 79, 124, 500) ## TODO: allow custom age classes
      lvls <- seq(length(ages_from)) |> as.integer()

      ageRast <- classify(
        rast(tifs),
        rcl = cbind(from = ages_from, to = ages_to, becomes = lvls),
        right = FALSE, others = NA_integer_
      )

      names(ageRast) <- "ageClass"
      levels(ageRast) <- data.frame(value = lvls, ageClass = paste0(ages_from, "-", to = ages_to))

      f_ageMosaicClass <- file.path(od, paste0("AGB_age_mosaic_classes_", i, ".tif"))
      terra::writeRaster(ageRast, f_ageMosaicClass, overwrite = TRUE)

      tifs <- c(tifs, f_ageMosaicClass)
    }

    return(tifs)
  }) |>
    unlist()
}
