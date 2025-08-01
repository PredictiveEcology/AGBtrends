#' Build raster mosaics for each time interval
#'
#' @param type character string specifying the mosaic type to build.
#'             one of `"age"`, `"LandCover"`, `"LandCover_Simplified"`, `"binary_disturbed"`,
#'             `"slopes"`, `"sample_size"`.
#'
#' @param src,dst character. vector of directory paths to source and destination raster files.
#'
#' @param intervals named list of time intervals over which to build mosaics,
#'                  or `NULL` to indicate no intervals
#'
#' @template cl
#'
#' @return character vector of output raster file names (`.tif`).
#'         Invoked for side effects of building and writing raster mosaics to disk.
#'
#' @export
buildMosaics <- function(type, src, dst, intervals = NULL, cl = NULL) {
  stopifnot(type %in% c("age", "binary_disturbed", "LandCover", "LandCover_Simplified",
                        "slopes", "sample_size"))

  if (type %in% c("slopes", "sample_size")) {
    type <- paste0("agb_", type) ## AGB products use 'AGB' prefix; others don't
  }

  intervals <- intervals %||% 1L

  srcType <- fs::file_info(src)[["type"]] |>
    unique() |>
    as.character()
  stopifnot(length(srcType) == 1) ## TODO: improve messaging to user to pass either dirs OR files

  cores <- getNumCores(length(intervals))

  if (is.null(cl)) {
    cl <- parallelly::makeClusterPSOCK(cores,
                                       default_packages = c("fs", "sf", "stringr", "terra"),
                                       rscript_libs = .libPaths(),
                                       autoStop = TRUE)
  }

  parallel::clusterExport(cl, varlist = c("cores", "dst", "src", "type"), envir = environment())

  parallel::clusterEvalQ(cl, {
    mempercore <- as.integer(terra::free_RAM() / 1024^2 / cores)

    terra::terraOptions(
      memmax = min(25, mempercore),
      memmin = min(1, mempercore),
      memfrac = 0.6 / cores,
      progress = 0,
      verbose = TRUE,
      print = FALSE
    )

    invisible(NULL)
  })

  parallel::parLapply(cl, seq(length(intervals)), function(i) {
    td <- terraOptions(print = FALSE)[["tempdir"]]

    ## Build virtual rasters
    flist <- if (srcType == "directory") {
      sapply(src, function(d) {
        fs::dir_ls(d, regexp = ifelse(length(intervals) == 1 || type %in% c("age"),
                                      paste0(type, "_", basename(d)),
                                      paste0(type, "_t", i, "_", basename(d))))
      })
    } else if (srcType == "file") {
      if (type == "binary_disturbed") {
        grep("disturbed", src, value = TRUE)
      } else {
        grep(paste0("_", type, "_Bh"), src, value = TRUE)
      }
    } |>
      unname()

    if (length(intervals) == 1) {
      vrts <- file.path(td, paste0(tolower(type), "_mosaic.vrt"))
      tifs <- file.path(dst, paste0(tolower(type), "_mosaic.tif"))
    } else {
      vrts <- file.path(td, paste0(tolower(type), "_mosaic_t", i, ".vrt"))
      tifs <- file.path(dst, paste0(tolower(type) , "_mosaic_t", i, ".tif"))
    }

    if (type == "age") {
      lyrs <- sapply(intervals, utils::head, n = 1L) |> unname()
      sf::gdal_utils(
        util = "buildvrt",
        source = flist,
        destination = vrts,
        options = c("-b", lyrs[i]) # time 1 = 1984 etc.
      )
    } else {
      sf::gdal_utils(
        util = "buildvrt",
        source = flist,
        destination = vrts
      )
    }

    ## Write to raster mosaics
    sf::gdal_utils(
      util = "warp",
      source = vrts,
      destination = tifs,
      options = c("-overwrite")
    )
    file.remove(vrts) ## remove intermediate files

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

      f_ageMosaicClass <- file.path(dst, paste0("agb_age_mosaic_classes_", i, ".tif"))
      terra::writeRaster(ageRast, f_ageMosaicClass, overwrite = TRUE)

      tifs <- c(tifs, f_ageMosaicClass)
    }

    return(tifs)
  }) |>
    unlist()
}
