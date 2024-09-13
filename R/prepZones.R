#' Prepare categorical zones for comparative analysis
#'
#' Create unique categorical zones for desired time period
#' (e.g. study area ZOI x age class).
#'
#' @param zoi zone-of-interest `sf` object created using `createAnalysisZones()`,
#'            or a character giving a file path to such an object that can be
#'            read using `st_read()`.
#'
#' @param field character string giving the field (column) in `zoi` to use for
#'              comparative analyses.
#'
#' @param ageClass `SpatRaster` object representing age classes
#'
#' @template fileID
#'
#' @param cropRaster `SpatRaster` to use to crop `ageClass`
#'
#' @template destinationPath
#'
#' @param overwrite logical. should the file be overwritten if it exists?
#'
#' @return (invisibly) character string giving the file path to the output raster file.
#'
#' @export
prepZones <- function(zoi,
                      field = "ECOZONE",
                      ageClass,
                      fileID = "WBI_ecozone_t1", ## TODO: use generic default
                      cropRaster = NULL,
                      destinationPath,
                      overwrite = TRUE) {
  field <- toupper(field)
  if (!is.null(cropRaster)) {
    ageClass <- terra::crop(ageClass, cropRaster)
  }
  if (is.character(zoi)) {
    zoi <- sf::st_read(zoi, quiet = TRUE)
    zoiName <- basename(zoi) |> tools::file_path_sans_ext()
  } else {
    zoiName <- "ZOI" ## TODO: get name from object?
  }

  ## QC of field to rasterize
  # if (!inherits(class(pull(zoi, field)), 'numeric')) {
  #   zoi <- mutate(zoi, !!field := as.integer(factor(!!as.name(field))))
  # }

  ## define comparative zones of interest (ZOI x age class for specified time period) within study area
  terra::rasterize(
    x = terra::vect(zoi),
    y = ageClass,
    field = field,
    filename = file.path(destinationPath, paste0(zoiName, "_", fileID, ".tif")),
    overwrite = overwrite
  )

  ## safeguard
  maxchar <- terra::levels(
    terra::rast(
      file.path(destinationPath, paste0(zoiName, "_", fileID, ".tif"))
    )
  )[[1]]$value |>
    nchar() |>
    max()
  rcoef <- as.numeric(stringr::str_c(c(1, rep(0, maxchar)), collapse = ""))

  ## combine study area zoi and age class mosaic into unique combined raster categories
  ## (when age class from 100 - 500 [n=5])
  zoneCat <- terra::rast(file.path(destinationPath, paste0(zoiName, "_", fileID, ".tif")))
  jointRast <- terra::as.int(rcoef * ageClass + zoneCat)

  ## re-assign factor levels
  ftab <- data.frame(value = terra::unique(jointRast)) |>
    dplyr::rename(value = ageClass) |>
    dplyr::mutate(
      ageClass = terra::levels(ageClass)[[1]]$ageClass[
        match(as.integer(stringr::str_sub(value, start = 1L, end = 1L)),
              terra::levels(ageClass)[[1]]$value)],
      !!field := terra::levels(zoneCat)[[1]][, field][
        match(as.integer(stringr::str_sub(value, start = -maxchar)),
              terra::levels(zoneCat)[[1]]$value)]
    )

  ftab <- dplyr::bind_cols(ftab, zoneCat = paste0(ftab[, 3], " (", ftab[, 2], " yrs)")) |>
    dplyr::relocate(zoneCat, .after = value)

  levels(jointRast) <- ftab

  fout <- file.path(destinationPath, paste0(zoiName, "xageClass_", fileID, ".tif"))
  terra::writeRaster(
    jointRast,
    filename = fout,
    overwrite = overwrite
  )

  return(invisible(fout))
}
