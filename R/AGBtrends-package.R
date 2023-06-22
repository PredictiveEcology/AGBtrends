#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import methods
#'
#' @importFrom dplyr bind_cols filter left_join mutate relocate rename select
#'
#' @importFrom fs dir_ls
#'
#' @importFrom ggplot2 aes facet_grid geom_errorbar geom_hline geom_line ggplot ggtitle
#' @importFrom ggplot2 labs scale_x_discrete xlab ylab
#'
#' @importFrom reproducible prepInputs
#'
#' @importFrom sf st_as_sf st_buffer st_cast st_dimension st_intersection
#' @importFrom sf st_make_valid st_read st_transform
#'
#' @importFrom stringr str_c str_detect str_sub
#'
#' @importFrom terra as.int cats classify crop levels mask
#' @importFrom terra rast rasterize terraOptions vect writeRaster zonal
#'
#' @importFrom tools file_path_sans_ext
#'
#' @importFrom utils globalVariables
## usethis namespace: end
NULL

utils::globalVariables(c(
  ":=", "ageClass", "count", "ECODISTRIC", "ECOPROVINC", "ECOZONE", "geometry",
  "REGION_NAM", "slope", "tp", "value", "x", "yr1", "yr2", "ZONE_NAME"
))
