test_that("createAnalysisZones works", {
  if (require("SpaDES.tools", quietly = TRUE)) {
    ## geospatial projection to use
    targetCRS <- Canada_Albers_Equal_Area_Conic

    ## define a study area
    studyArea <- terra::vect(cbind(x = -113.530, y = 61.530), crs = "EPSG:4326") |>
      SpaDES.tools::randomStudyArea(size = 3e10, seed = 42) |>
      sf::st_as_sf() |>
      sf::st_transform(targetCRS)

    ## create analysis zones
    analysisZones <- createAnalysisZones(
      studyArea = studyArea,
      targetCRS = targetCRS,
      destinationPath = tempdir()
    )

    expect_true(all(c("ECOZONE", "ECOPROVINCE", "ECOREGION", "ECODISTRICT", "geometry") %in%
                      colnames(analysisZones)))
  }
})
