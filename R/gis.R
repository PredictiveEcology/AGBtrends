#' `Canada_Albers_Equal_Area_Conic` projection
#'
#' Unknown EPSG code associated with this projection, which is used for the ABoVE datasets.
#'
#' @export
#' @rdname projections
Canada_Albers_Equal_Area_Conic <- paste0(
  "PROJCRS[\"Canada_Albers_Equal_Area_Conic\",\n",
  "    BASEGEOGCRS[\"NAD83\",\n",
  "        DATUM[\"North American Datum 1983\",\n",
  "            ELLIPSOID[\"GRS 1980\",6378137,298.257222101004,\n",
  "                LENGTHUNIT[\"metre\",1]]],\n",
  "        PRIMEM[\"Greenwich\",0,\n",
  "            ANGLEUNIT[\"degree\",0.0174532925199433]],\n",
  "        ID[\"EPSG\",4269]],\n    CONVERSION[\"unnamed\",\n",
  "        METHOD[\"Albers Equal Area\",\n",
  "            ID[\"EPSG\",9822]],\n",
  "        PARAMETER[\"Latitude of false origin\",40,\n",
  "            ANGLEUNIT[\"degree\",0.0174532925199433],\n",
  "            ID[\"EPSG\",8821]],\n",
  "        PARAMETER[\"Longitude of false origin\",-96,\n",
  "            ANGLEUNIT[\"degree\",0.0174532925199433],\n",
  "            ID[\"EPSG\",8822]],\n",
  "        PARAMETER[\"Latitude of 1st standard parallel\",50,\n",
  "            ANGLEUNIT[\"degree\",0.0174532925199433],\n",
  "            ID[\"EPSG\",8823]],\n",
  "        PARAMETER[\"Latitude of 2nd standard parallel\",70,\n",
  "            ANGLEUNIT[\"degree\",0.0174532925199433],\n",
  "            ID[\"EPSG\",8824]],\n",
  "        PARAMETER[\"Easting at false origin\",0,\n",
  "            LENGTHUNIT[\"metre\",1],\n",
  "            ID[\"EPSG\",8826]],\n",
  "        PARAMETER[\"Northing at false origin\",0,\n",
  "            LENGTHUNIT[\"metre\",1],\n",
  "            ID[\"EPSG\",8827]]],\n",
  "    CS[Cartesian,2],\n",
  "        AXIS[\"easting\",east,\n",
  "            ORDER[1],\n",
  "            LENGTHUNIT[\"metre\",1,\n",
  "                ID[\"EPSG\",9001]]],\n",
  "        AXIS[\"northing\",north,\n",
  "            ORDER[2],\n",
  "            LENGTHUNIT[\"metre\",1,\n",
  "                ID[\"EPSG\",9001]]]]"
)
