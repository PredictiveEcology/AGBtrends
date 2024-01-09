#' Build raster mosaics for each time interval
#'
#'
#'
#' @param type character string specifying the mosaic type to build.
#'             one of `"age", "LandCover", "LandCover_Simplified, "slope", "sample_size"`.
#' @param intervals named list of time intervals over which to build mosaics
#'
#' @param src,dst character. vector of directory paths to source and destination raster files.
#'
#' @param cl cluster object. if `NULL` (default), a cluster will be created using up to
#'           `length(intervals)` number of CPU cores for parallel computation.
#'           Users can pass their own `cl` object or specify option
#'           `parallelly.availableCores.fallback` to reduce the number of cores used.
#'           See `?parallelly.options`.
#'
#' @return character vector of output raster file names (`.tif`).
#'         Invoked for side effects of building and writing raster mosaics to disk.
#'
#' @export
buildMosaics <- function(type, intervals, src, dst, cl = NULL) {
  stopifnot(type %in% c("age", "landcover", "landcover_simplified", "slope", "sample_size"))

  cores <- max(length(intervals), parallelly::availableCores())

  if (is.null(cl)) {
    cl <- parallelly::makeClusterPSOCK(cores,
                                       default_packages = c("sf", "stringr", "terra"),
                                       rscript_libs = .libPaths(),
                                       autoStop = TRUE)
    on.exit(stopCluster(cl), add = TRUE)
  }

  parallel::clusterExport(cl, varlist = c("cores", "dst", "src"))

  parallel::clusterEvalQ(cl, {
    terra::terraOptions(memmax = 25,
                        memfrac = 0.6 / cores,
                        progress = 0,
                        verbose = TRUE)
  })

  parLapply(cl, seq(length(intervals)), function(i) {
    td <- terra::terraOptions(print = FALSE)[["tempdir"]]

    ## Build virtual rasters
    flist <- sapply(src, function(d) {
      fs::dir_ls(dsn, regexp = ifelse(length(intervals) == 1,
                                      paste0(type, "_", basename(dsn)),
                                      paste0(type, "_t", i, basename(dsn))))
    }) |>
      unname()

    if (length(intervals) == 1) {
      vrts <- file.path(td, paste0("AGB_", type, "_mosaic.vrt"))
      tifs <- file.path(dst, paste0("AGB_", type , "_mosaic.tif"))
    } else {
      vrts <- file.path(td, paste0("AGB_", type, "_mosaic_t", i, ".vrt"))
      tifs <- file.path(dst, paste0("AGB_", type , "_mosaic_t", i, ".tif"))
    }

    if (type == "age") {
      lyrs <- sapply(intervals, utils::head, n = 1L) |> unname()
    }

    sf::gdal_utils(
      util = "buildvrt",
      source = flist,
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
