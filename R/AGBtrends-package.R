#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import methods
#' @importFrom dplyr bind_cols filter left_join mutate relocate rename select
#' @importFrom fs dir_ls
#' @importFrom ggplot2 aes facet_grid geom_errorbar geom_hline geom_line ggplot ggtitle
#' @importFrom ggplot2 labs scale_x_discrete xlab ylab
#' @importFrom httr2 is_online
#' @importFrom parallel clusterEvalQ clusterExport parLapply stopCluster
#' @importFrom parallelly availableCores makeClusterPSOCK
#' @importFrom reproducible prepInputs
#' @importFrom sf gdal_utils st_as_sf st_buffer st_cast st_dimension st_intersection
#' @importFrom sf st_make_valid st_read st_transform
#' @importFrom stringr str_c str_detect str_split str_sub
#' @importFrom terra app as.int cats classify cover crop free_RAM global ifel levels mask NAflag<-
#' @importFrom terra project rast rasterize terraOptions unique vect writeRaster zonal
#' @importFrom tools file_path_sans_ext
#' @importFrom utils globalVariables head tail
## usethis namespace: end
NULL

utils::globalVariables(c(
  ":=", "ageClass", "count", "ECODISTRIC", "ECOPROVINC", "ECOZONE", "geometry",
  "REGION_NAM", "slope", "tp", "value", "x", "yr1", "yr2", "ZONE_NAME"
))
