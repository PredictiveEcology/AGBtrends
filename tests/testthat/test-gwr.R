test_that("gwr and gwrt works", {
  projOutPath <- "/mnt/projects/CBM/2BT/ForProd/outputs/studyArea_WBI"

  skip_if_not(dir.exists(projOutPath))

  ## define time intervals (year ranges between 1984-2014)
  timeint <- list(t1 = 1:5, t2 = 6:10, t3 = 11:15, t4 = 16:20, t5 = 21:25, t6 = 26:31)
  timeint_all <- timeint |> unlist() |> unname() |> list(all = _)

  tileFiles <- file.path(projOutPath, "tiles") |>
    fs::dir_ls(regexp = "Bh08v07|Bh08v08", type = "directory") |>
    fs::dir_ls(regexp = "ragb", type = "file") |>
    sort()

  tmpOutPaths <- file.path(tempdir(), "test_gwr", basename(dirname(tileFiles)))
  vapply(tmpOutPaths, dir.create, recursive = TRUE, FUN.VALUE = logical(1))

  fs::file_copy(tileFiles, file.path(tmpOutPaths, basename(tileFiles)))

  ## sample_size for entire timeseries
  fa <- gwr(tileDirs = tmpOutPaths, type = "sample_size")
  for (i in seq(length(tileFiles))) {
    r <- terra::rast(fa[i])
    expect_identical(terra::datatype(r), "INT1U")
    mm <- r |> terra::minmax() |> as.vector()
    expect_true(all(mm %in% seq(length(unlist(timeint)))))
  }
  file.remove(fa)

  ## sample_size for each interval
  fb <- gwrt(tileDirs = tmpOutPaths, type = "sample_size", intervals = timeint)
  for (i in seq(length(tileFiles))) {
    r <- terra::rast(fb[i])
    expect_identical(terra::datatype(r), "INT1U")
    mm <- r |> terra::minmax() |> as.vector()
    expect_true(all(mm %in% seq(length(timeint[[i]]))))
  }
  file.remove(fb)

  ## slopes for entire timeseries
  fc <- gwr(tileDirs = tmpOutPaths, type = "slopes")
  for (i in seq(length(tileFiles))) {
    r <- terra::rast(fc[i])
    expect_identical(terra::datatype(r), "FLT4S")
  }
  file.remove(fc)

  ## slopes for each interval
  fd <- gwrt(tileDirs = tmpOutPaths, type = "slopes", intervals = timeint)
  for (i in seq(length(tileFiles))) {
    r <- terra::rast(fd[i])
    expect_identical(terra::datatype(r), "FLT4S")
  }
  file.remove(fd)

  ## cleanup
  unlink(tmpOutPaths, recursive = TRUE)
})
