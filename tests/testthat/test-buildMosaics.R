test_that("buildMosaics works for ABoVE AGB slope", {
  projOutPath <- "/mnt/projects/CBM/2BT/ForProd/outputs/studyArea_WBI"

  skip_if_not(dir.exists(projOutPath))

  ## define time intervals (year ranges between 1984-2014)
  timeint <- list(t1 = 1:5, t2 = 6:10, t3 = 11:15, t4 = 16:20, t5 = 21:25, t6 = 26:31)
  timeint_all <- timeint |> unlist() |> unname() |> list(all = _)

  tilePath <- file.path(projOutPath, "tiles") |>
    fs::dir_ls(regexp = "Bh07v07|Bh07v08|Bh08v07|Bh08v08", type = "directory") |>
    sort()

  tmpOutPath <- file.path(tempdir(), "test_buildMosaics_slope")
  dir.create(tmpOutPath, recursive = TRUE)

  expFileSize <- 576072544

  ## for entire timeseries
  fa <- buildMosaics("slopes", intervals = timeint_all, src = tilePath, dst = tmpOutPath)
  expect_identical(length(fa), 1L)
  expect_true(file.exists(fa))
  expect_equal(file.info(fa)$size, expFileSize)
  file.remove(fa)

  ## for each interval
  fb <- buildMosaics("slopes", intervals = timeint, src = tilePath, dst = tmpOutPath)
  expect_identical(length(fb), length(timeint))
  expect_true(all(file.exists(fb)))
  expect_equal(unique(file.info(fb)$size), expFileSize)
  file.remove(fb)

  ## cleanup
  unlink(tmpOutPath, recursive = TRUE)
})

test_that("buildMosaics works for ABoVE AGB sample_size", {
  projOutPath <- "/mnt/projects/CBM/2BT/ForProd/outputs/studyArea_WBI"

  skip() ## TODO: input tiles need to be rebuilt:
  ##  Warning messages:
  ##    1: In CPL_gdalbuildvrt(if (missing(source)) character(0) else source,  :
  ##       GDAL Message 1: gdalbuildvrt does not support heterogeneous band data type: expected Byte, got Float32.
  ##       Skipping /mnt/projects/CBM/2BT/ForProd/outputs/studyArea_WBI/tiles/Bh06v08/agb_sample_size_Bh06v08.tif

  skip_if_not(dir.exists(projOutPath))

  ## define time intervals (year ranges between 1984-2014)
  timeint <- list(t1 = 1:5, t2 = 6:10, t3 = 11:15, t4 = 16:20, t5 = 21:25, t6 = 26:31)
  timeint_all <- timeint |> unlist() |> unname() |> list(all = _)

  tilePath <- file.path(projOutPath, "tiles") |>
    fs::dir_ls(regexp = "Bh07v07|Bh07v08|Bh08v07|Bh08v08", type = "directory") |>
    sort()

  tmpOutPath <- file.path(tempdir(), "test_buildMosaics_sample_size")
  dir.create(tmpOutPath, recursive = TRUE)

  expFileSize <- 576072544

  ## for entire timeseries
  fa <- buildMosaics("sample_size", intervals = timeint_all, src = tilePath, dst = tmpOutPath)
  expect_identical(length(fa), 1L)
  expect_true(file.exists(fa))
  expect_equal(file.info(fa)$size, expFileSize)
  file.remove(fa)

  ## for each interval
  fb <- buildMosaics("sample_size", intervals = timeint, src = tilePath, dst = tmpOutPath)
  expect_identical(length(fb), length(timeint))
  expect_true(all(file.exists(fb)))
  expect_equal(unique(file.info(fb)$size), expFileSize)
  file.remove(fb)

  ## cleanup
  unlink(tmpOutPath, recursive = TRUE)
})

test_that("buildMosaics works for ABoVE AGB age", {
  projOutPath <- "/mnt/projects/CBM/2BT/ForProd/outputs/studyArea_WBI"

  skip_if_not(dir.exists(projOutPath))

  ## define time intervals (year ranges between 1984-2014)
  timeint <- list(t1 = 1:5, t2 = 6:10, t3 = 11:15, t4 = 16:20, t5 = 21:25, t6 = 26:31)
  timeint_all <- timeint |> unlist() |> unname() |> list(all = _)

  tilePath <- file.path(projOutPath, "tiles") |>
    fs::dir_ls(regexp = "Bh07v07|Bh07v08|Bh08v07|Bh08v08", type = "directory") |>
    sort()

  tmpOutPath <- file.path(tempdir(), "test_buildMosaics_age")
  dir.create(tmpOutPath, recursive = TRUE)

  ## for each interval only
  f <- buildMosaics("age", intervals = timeint, src = tilePath, dst = tmpOutPath)
  expect_true(all(file.exists(f)))

  f1 <- f[!grepl("classes", f)]
  expect_equal(unique(file.info(f1)$size), 576072544)
  file.remove(f1)

  f2 <- f[grepl("classes", f)]
  expect_equal(unique(file.info(f2)$size),
               c(44544281, 45161778, 45920624, 48241014, 47993583, 47728825))
  file.remove(f2)

  ## cleanup
  unlink(tmpOutPath, recursive = TRUE)
})

test_that("buildMosaics works for ABoVE landcover", {
  projInPath <- "/mnt/projects/CBM/2BT/ForProd/inputs"

  skip_if_not(dir.exists(projInPath))

  tileFiles <- file.path(projInPath, "ABoVE_LandCover") |>
    fs::dir_ls(regexp = "Bh07v07|Bh07v08|Bh08v07|Bh08v08", type = "file") |>
    sort()

  tmpOutPath <- file.path(tempdir(), "test_buildMosaics_landcover")
  dir.create(tmpOutPath, recursive = TRUE)

  fa <- buildMosaics("LandCover", intervals = NULL, src = tileFiles, dst = tmpOutPath)
  expect_identical(length(fa), 1L)
  expect_equal(file.info(fa)$size, 4464145023)
  file.remove(fa)

  fb <- buildMosaics("LandCover_Simplified", intervals = NULL, src = tileFiles, dst = tmpOutPath)
  expect_identical(length(fb), 1L)
  expect_equal(file.info(fb)$size, 4464145023)
  file.remove(fb)

  ## cleanup
  unlink(tmpOutPath, recursive = TRUE)
})

test_that("buildMosaics works for ABoVE binary_disturbed", {
  projOutPath <- "/mnt/projects/CBM/2BT/ForProd/outputs/studyArea_WBI"

  skip_if_not(dir.exists(projOutPath))

  ## define time intervals (year ranges between 1984-2014)
  timeint <- list(t1 = 1:5, t2 = 6:10, t3 = 11:15, t4 = 16:20, t5 = 21:25, t6 = 26:31)
  timeint_all <- timeint |> unlist() |> unname() |> list(all = _)

  tileFiles <- file.path(projOutPath, "binary_disturbed") |>
    fs::dir_ls(regexp = "Bh07v07|Bh07v08|Bh08v07|Bh08v08", type = "file") |>
    sort()

  tmpOutPath <- file.path(tempdir(), "test_buildMosaics_binary_disturbed")
  dir.create(tmpOutPath, recursive = TRUE)

  f <- buildMosaics("binary_disturbed", intervals = NULL, src = tileFiles, dst = tmpOutPath)
  expect_identical(length(f), 1L)
  expect_equal(unique(file.info(f)$size), 576072521)
  file.remove(f)

  ## cleanup
  unlink(tmpOutPath, recursive = TRUE)
})
