#' Estimate cell-specific stand age
#'
#' Uses a combination of ABoVE Disturbance Agents and the CaNFIR stand age mosaic (kNN 2020)
#'
#' @param agbfiles,distfiles character vector of filenames specifying ABoVE AGB and disturbance
#'        raster tiles.
#'
#' @param age_mosaic character string specifying the filepath to CaNFIR stand age mosaic.
#'
#' @param tilenames character vector of output tile directories.
#'
#' @param years integer vector of data years.
#'
#' @template cl
#'
#' @return Invisibly, a list of output files created by the function.
#'         Invoked for the side effect of writing these raster tiles to disk.
#'
#' @export
ABovE_CaNFIR_standAge <- function(agbfiles, distfiles, age_mosaic, tilenames, years, cl = NULL) {

  cores <- getNumCores()

  if (is.null(cl)) {
    cl <- parallelly::makeClusterPSOCK(
      cores,
      default_packages = c("sf", "stringr", "terra"),
      rscript_libs = .libPaths(),
      autoStop = TRUE
    )
    parallel::clusterExport(
      cl,
      varlist = c("agbfiles", "cores", "distfiles", "tilenames"),
      envir = environment()
    )
    parallel::clusterEvalQ(cl, {
      terraOptions(
        memmax = min(25, as.integer(terra::free_RAM() / 1024^2)),
        memfrac = 0.8 / cores,
        progress = 0,
        verbose = TRUE,
        print = FALSE
      )
    })
  }

  outFiles <- parLapply(cl, seq(length(tilenames)), function(i) {
    tdir <- tilenames[i]
    if (!file.exists(tdir)) dir.create(tdir)

    ## Import AGB raster tile (0 = NA)
    ragb <- rast(agbfiles[i])
    names(ragb) <- years

    ## Restrict selection to available disturbance history years
    NAflag(ragb) <- 0

    ## 1 c) step i) project stand age mosaic (kNN 2020) to ABoVE CRS & crop to tile
    ## 1 c) step ii) determine stand age as of year associated with raster layer (negative values become NA)
    ## 1 c) step iii) split into 5 year age categories
    rage <- classify(
      as.integer(names(ragb)) -
        (2020 - crop(project(rast(age_mosaic), ragb[[1]], method = "near" ), ragb[[1]])),
      rcl = cbind(-100, 0, NA), include.lowest = TRUE
    )

    names(rage) <- years # 3.63 min

    ## Import disturbance history (ABoVE) & mask pixels in PRE-disturbance years
    rdist <- rast(distfiles[i])
    names(rdist) <- 1987:2012

    ## identify year of earliest disturbance for each cell in tile (according to ABoVE product)
    chid <- app(
      rast(lapply(names(rdist), function(k) {
        classify(rdist[[k]], rcl = cbind(1, 3, as.integer(k)), include.lowest = TRUE)
      })),
      fun = min, na.rm = TRUE
    ) # 106 sec

    ## derive pixel- and year-specific age since disturbance
    ## (ABoVE disturbed pixels exclusively; *only possible for years 1987-2012*),
    ageSinceDist <- rast(lapply(years, function(k) k - chid))

    ## create mask serving to identify relevant cells
    ageMask <- ifel(!is.na(ageSinceDist), 9999, NA)

    ## replace rage values with 9999 where ABoVE disturbances overlap
    rageMask <- mask(rage, ageMask, maskvalue = 9999, updatevalue = 9999)
    rage <- cover(rageMask, ageSinceDist, values = 9999)

    ## verify that ABoVE Disturbance product is correctly integrated
    if (FALSE) {
      k <- sample(1:31, size = 1)
      x <- mask(rage[[k]], ageMask[[k]])
      if (global(x, fun = "sum", na.rm = TRUE)$sum !=
          global(ageSinceDist[[k]], fun = "sum", na.rm = TRUE)$sum) stop("PROBLEM ...!")
    }

    ## render NA all cell values pre-dating disturbances (where stand age cannot be known)
    f_rage <- file.path(tdir, paste0("rage_", tilenames[i], ".tif"))
    rage <- ifel(rage > 0, rage, NA, filename = f_rage, overwrite = TRUE)

    ## remove AGB values where (pre-disturbance) year unknown
    f_ragb <- file.path(tdir, paste0("ragb_", tilenames[i], ".tif"))
    mask(ragb, rage, filename = f_ragb, overwrite = TRUE)

    gc()

    return(list(f_ragb, f_rage))
  }) |>
    unlist(recursive = TRUE)

  ## cleanup
  parallel::stopCluster(cl)

  return(invisible(outFiles))
}
