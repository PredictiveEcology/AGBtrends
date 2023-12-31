% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analysisZones.R
\name{createAnalysisZones}
\alias{createAnalysisZones}
\title{Create analysis zones}
\usage{
createAnalysisZones(studyArea, targetCRS, destinationPath)
}
\arguments{
\item{studyArea}{an \code{sf} or \code{SpatialPolygons} object}

\item{targetCRS}{geospatial projection (as \code{CRS} object) to use for reprojection.}

\item{destinationPath}{character string providing the path to a directory where
downloaded data or prepared object(s) will be saved.}
}
\value{
\code{sf} multipolygons objects
}
\description{
Preforms the following operations:
\enumerate{
\item Downloads Canadian terrestrial ecodistricts, ecoregions, ecoprovince, and ecozones;
\item Renames the corresponding zone id field to use the upper case version of the zone name;
\item Performs geospatial intersections with \code{studyArea} to produce an updated \code{studyArea}
object that includes these fields.
}
}
\note{
Additional fields containing custom zones for analyses can be included by ensuring
these fields exist in the \code{studyArea} object prior to calling this function.
}
\examples{
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

  plot(analysisZones)
}

}
