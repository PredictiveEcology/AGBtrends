#' Calculate summary statistics by categorical zone of interest
#'
#' Weighted mean & standard deviation of slopes by age class, time period,
#' and study area zones of interest.
#'
#' @param slopeRaster TODO
#'
#' @param weightRaster TODO
#'
#' @param zoneRaster TODO
#'
#' @param cropRaster TODO
#'
#' @param maskRaster TODO
#'
#' @template fileID
#'
#' @template destinationPath
#'
#' @return (invisibly) character string corresponding to the file path of the saved raster.
#'
#' @export
zoneStats <- function(slopeRaster = NULL, weightRaster = NULL, zoneRaster = NULL,
                      cropRaster = NULL, maskRaster = NULL, fileID = NULL, destinationPath = NULL) {
  names(weightRaster) <- "w"
  names(slopeRaster) <- "slope"

  ## crop to smaller (e.g. test) area, if one provided
  if (!is.null(cropRaster)) {
    slopeRaster <- terra::crop(slopeRaster, cropRaster)
    weightRaster <- terra::crop(weightRaster, cropRaster)
    zoneRaster <- terra::crop(zoneRaster, cropRaster)
    if (!is.null(maskRaster)) {
      maskRaster <- terra::crop(maskRaster, cropRaster)
    }
  }

  ## apply mask, if provided
  if (!is.null(maskRaster)) {
    slopeRaster <- terra::mask(slopeRaster, maskRaster)
    weightRaster <- terra::mask(weightRaster, maskRaster)
    zoneRaster <- terra::mask(zoneRaster, maskRaster)
  }

  a <- terra::cats(zoneRaster)[[1]]
  levels(zoneRaster) <- NULL
  names(zoneRaster) <- "value"

  ## 1) count of slope observations by zone
  a <- a |>
    dplyr::left_join(terra::zonal(slopeRaster, zoneRaster, fun = "notNA"), by = "value") |>
    dplyr::rename(count = slope)

  if (any(!is.na(a$count))) {
    ## 2) geometric (unweighted) mean by zone
    meanRast <- terra::zonal(slopeRaster, zoneRaster, fun = "mean", as.raster = TRUE, na.rm = TRUE)

    a <- a |>
      dplyr::left_join(
        terra::zonal(meanRast, zoneRaster, fun = "min", na.rm = TRUE),
        by = "value"
      ) |>
      dplyr::rename(mean.slope = slope)

    ## 3) sd by zone
    a <- a |>
      dplyr::left_join(
        terra::zonal(
          x = (slopeRaster - meanRast)^2,
          z = zoneRaster, fun = "sum", na.rm = TRUE
        ), by = "value") |>
      dplyr::mutate(sd = sqrt(slope / count)) |>
      select(!slope)

    ## 4.1) sum of wi*xi by zone (numerator of weighted mean)
    b <- terra::zonal(slopeRaster * weightRaster, zoneRaster, fun = "sum", na.rm = TRUE) |>
      dplyr::rename(b = slope)

    ## 4.2) sum of weights by zone (denominator of weighted mean)
    w <- terra::zonal(weightRaster, zoneRaster, fun = "sum", na.rm = TRUE)

    ## 4.3) derive weighted mean
    a <- a |>
      dplyr::left_join(b, by = "value") |>
      dplyr::left_join(w, by = "value") |>
      dplyr::mutate(wtd.mean.slope = b / w, .before = w)

    rm(w)

    ## 5) derive weighted sd
    a <- a |>
      dplyr::left_join(
        terra::zonal(
          x = weightRaster *
            (slopeRaster - terra::classify(zoneRaster, rcl = a[, c("value", "wtd.mean.slope")]))^2,
          z = zoneRaster, fun = "sum", na.rm = TRUE
        ) |>
          dplyr::rename(x = w),
        by = "value"
      ) |>
      dplyr::mutate(wtd.sd = sqrt(x / w)) |>
      dplyr::filter(!is.na(count)) |>
      dplyr::select(!c(b, w, x))

    ##  6 b) write to file
    fout <- file.path(destinationPath, paste0("zoneStats_summary_", fileID, ".rds"))
    saveRDS(a, file = fout)
  }

  return(invisible(fout))
}
