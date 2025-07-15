#' Create analysis zones
#'
#' Preforms the following operations:
#' 1. Downloads Canadian terrestrial ecodistricts, ecoregions, ecoprovince, and ecozones;
#' 2. Renames the corresponding zone id field to use the upper case version of the zone name;
#' 3. Performs geospatial intersections with `studyArea` to produce an updated `studyArea`
#'    object that includes these fields.
#'
#' @note Additional fields containing custom zones for analyses can be included by ensuring
#' these fields exist in the `studyArea` object prior to calling this function.
#'
#' @template studyArea
#'
#' @template targetCRS
#'
#' @template destinationPath
#'
#' @return `sf` multipolygons objects
#'
#' @examples
#' if (require("SpaDES.tools", quietly = TRUE)) {
#'   ## geospatial projection to use
#'   targetCRS <- Canada_Albers_Equal_Area_Conic
#'
#'   ## define a study area
#'   studyArea <- terra::vect(cbind(x = -113.530, y = 61.530), crs = "EPSG:4326") |>
#'     SpaDES.tools::randomStudyArea(size = 3e10, seed = 42) |>
#'     sf::st_as_sf() |>
#'     sf::st_transform(targetCRS)
#'
#'   ## create analysis zones
#'   analysisZones <- createAnalysisZones(
#'     studyArea = studyArea,
#'     targetCRS = targetCRS,
#'     destinationPath = tempdir()
#'   )
#'
#'   plot(analysisZones)
#' }
#'
#' @export
#'
createAnalysisZones <- function(studyArea, targetCRS, destinationPath) {
  ## NOTE: httr2 is needed by reproducible to download these files,
  ## but since it's only Suggested by that package, we'll use it here
  ## simply to ensure it's available.
  httr2::is_online()

  if (!is(studyArea, "sf")) {
    studyArea <- sf::st_as_sf(studyArea)
  }

  urlList <- list(
    ecodistrict = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/district/ecodistrict_shp.zip",
    ecoregion = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/region/ecoregion_shp.zip",
    ecoprovince = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/province/ecoprovince_shp.zip",
    ecozone = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip"
  )

  eco <- lapply(urlList, function(url) {
    suppressWarnings({
      reproducible::prepInputs(
        url = url,
        destinationPath = destinationPath,
        cropTo = studyArea,
        maskTo = studyArea,
        fun = "sf::st_read",
        overwrite = TRUE
      ) |>
        sf::st_make_valid() |>
        sf::st_transform(targetCRS) |>
        sf::st_cast("MULTIPOLYGON")
    })
  })

  eco[[1]] <- dplyr::select(eco[[1]], ECODISTRIC, geometry) |>
    dplyr::rename(ECODISTRICT = ECODISTRIC)
  eco[[2]] <- dplyr::select(eco[[2]], REGION_NAM, geometry) |>
    dplyr::rename(ECOREGION = REGION_NAM)
  eco[[3]] <- dplyr::select(eco[[3]], ECOPROVINC, geometry) |>
    dplyr::rename(ECOPROVINCE = ECOPROVINC)
  eco[[4]] <- dplyr::select(eco[[4]], ZONE_NAME, geometry) |>
    dplyr::rename(ECOZONE = ZONE_NAME) |>
    dplyr::mutate(ECOZONE = tools::toTitleCase(tolower(ECOZONE)))

  ## intersect them all, removing slivers, lines, points, etc.
  analysisZones <- eco[[4]] |>
    sf::st_intersection(eco[[3]]) |>
    sf::st_intersection(eco[[2]]) |>
    sf::st_intersection(eco[[1]]) |>
    sf::st_buffer(0.0)
  analysisZones <- analysisZones[which(!is.na(sf::st_dimension(analysisZones))), ] ## rm empty
  rownames(analysisZones) <- 1:nrow(analysisZones)

  ## intersect with user's studyArea, removing slivers, lines, points, etc.
  analysisZones <- sf::st_intersection(studyArea, analysisZones) |>
    sf::st_buffer(0.0)
  analysisZones <- analysisZones[which(!is.na(sf::st_dimension(analysisZones))), ] ## rm empty
  rownames(analysisZones) <- 1:nrow(analysisZones)

  return(sf::st_make_valid(analysisZones))
}
