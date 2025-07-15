# AGBtrends 0.0.11

* drop support for R 4.2 due to changes in packages dependencies;
* add `httr2` to Imports;

# AGBtrends 0.0.10

* fix interval extraction from file name in `plotZoneStats*()`;

# AGBtrends 0.0.9

* use `gdalwarp` option `-overwrite` with `buildMosaics()`;

# AGBtrends 0.0.8

* files2plot should be a single file for `plotZoneStats()`;
* pass argument `tref` to `plotZoneStats()` and `plotZoneStatsIntervals()`;
* new function `tref()` to generate `tref` data.frame from `timeint` and `years`;

# AGBtrends 0.0.7

* fix zonal data.frame creation in `prepZones()`;

# AGBtrends 0.0.6

* use `getNumCores()` instead of `parallelly::availableCores()` directly;

# AGBtrends 0.0.5

* add `ABovE_CaNFIR_standAge()`;
* fix sample size mosaics;
* multiple improvements to `gwr()` and `gwrt()`, with tests;
* reordered some arguments to `buildMosaics()`;

# AGBtrends 0.0.4

* use consistent types in `gwr()`/`gwrt()` as in `buildMosaics()`;
* use intersect of `studyArea` + `analysisZones` to keep user's fields;
* `buildMosaics()` fixes and improvements;
* `getNumCores()` now respects `parallelly.availableCores.fallback`;
* fix failures in multiple test and other testing improvements;
* documentation improvements;

# AGBtrends 0.0.3

* several improvements for `buildMosaics()`;
* allow user to adjust number of cores used in `buildMosaics()`;
* allow `type`s "`landCover"` and `"LandCover_Simplified"` in `buildMosaics(allow buildMosaics()`;
* explicitly pass source and destination directories to `buildMosaics()` instead of `paths`;
* improve documentation;

# AGBtrends 0.0.2

* add `fs` and  `parallely` to Imports;
* add `buildMosaics()` to handle multiple mosaic types;
* improve `zoneStats()` documentation;
* add `getNumCores()` for parallel processing;
* add geographically weighted regression functions;

# AGBtrends 0.0.1

* initial package version;
