crs <- 'EPSG:3154'

# yearly rasters 2018-2020 whose value is the year (or, for "fires", a burn year)
toy_rasters <- function(dir) {
  for (y in 2018:2020) {
    r <- terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 1000, ymin = 0, ymax = 1000,
                     crs = crs, vals = y)
    terra::writeRaster(r, file.path(dir, paste0('val_', y, '.tif')))
    f <- terra::rast(r, vals = c(rep(NA, 50), rep(y - 5, 50)))   # burned in the bottom half
    terra::writeRaster(f, file.path(dir, paste0('fires_', y, '.tif')))
  }
  terra::writeRaster(terra::rast(r, vals = 1:100), file.path(dir, 'elev.tif'))
}

toy_steps <- function() {
  data.table(year = c(2017L, 2018L, 2019L, 2021L),
             x1_ = c(50, 50, 50, NA), y1_ = c(950, 950, 950, NA),
             x2_ = c(950, 950, 50, 950), y2_ = c(50, 950, 50, 50))
}

test_that('read_layers reads a valid table and reports problems', {
  dir <- withr::local_tempdir()
  good <- data.table(name = c('a', 'b'), first_year = c(2018, NA), last_year = c(2020, NA),
                     path = c('x_{year}.tif', 'y.tif'), time_since = c(TRUE, NA), ts_nodata = c(100, NA))
  fwrite(good, file.path(dir, 'good.csv'))
  l <- read_layers(file.path(dir, 'good.csv'))
  expect_equal(l$time_since, c(TRUE, FALSE))
  expect_true(all(is.na(l$url)))

  bad <- data.table(name = c('a', 'a', '1b', 'c'), first_year = c(2018, NA, NA, NA),
                    last_year = c(2020, NA, NA, NA), path = c('x.tif', 'y.tif', 'z.tif', ''))
  fwrite(bad, file.path(dir, 'bad.csv'))
  err <- tryCatch(read_layers(file.path(dir, 'bad.csv')), error = conditionMessage)
  expect_match(err, 'more than once: a')
  expect_match(err, 'letters, numbers.*1b')
  expect_match(err, 'no path, url or build: c')
  expect_match(err, 'without \\{year\\}')
})

test_that('get_layer_files finds one file per year', {
  dir <- withr::local_tempdir()
  toy_rasters(dir)
  files <- get_layer_files('val_{year}.tif', NA, NA, NA, 2018L, 2020L, list(data_in = dir))
  expect_equal(basename(files), paste0('val_', 2018:2020, '.tif'))
  expect_equal(basename(get_layer_files('elev.tif', NA, NA, NA, NA, NA, list(data_in = dir))), 'elev.tif')
  expect_error(get_layer_files('val_{year}.tif', NA, NA, NA, 2018L, 2021L, list(data_in = dir)), 'not found')
})

test_that('extract_layer matches each step to its year, clamping outside years', {
  dir <- withr::local_tempdir()
  toy_rasters(dir)
  files <- file.path(dir, paste0('val_', 2018:2020, '.tif'))
  out <- extract_layer(toy_steps(), files, 2018L, 2020L, 'val', 'both', crs = crs)
  expect_named(out, c('val_start', 'val_end'))
  expect_equal(out$val_end, c(2018, 2018, 2019, 2020))
  expect_equal(out$val_start, c(2018, 2018, 2019, NA))   # missing start coordinates
  expect_named(extract_layer(toy_steps(), files, 2018L, 2020L, 'val', 'end'), 'val_end')
})

test_that('extract_layer adds years since disturbance', {
  dir <- withr::local_tempdir()
  toy_rasters(dir)
  files <- file.path(dir, paste0('fires_', 2018:2020, '.tif'))
  out <- extract_layer(toy_steps(), files, 2018L, 2020L, 'fires', 'end', time_since = TRUE, ts_nodata = 200)
  # bottom half burned (year - 5); top half never burned
  expect_equal(out$ts_fires_end, c(4, 200, 5, 6))
  # 0 = never disturbed, as in the 250 m harvest rasters
  z <- file.path(dir, 'zero.tif')
  terra::writeRaster(terra::rast(terra::rast(files[1]), vals = 0), z)
  out0 <- extract_layer(toy_steps(), rep(z, 3), 2018L, 2020L, 'harv', 'end', time_since = TRUE, ts_nodata = 100)
  expect_equal(out0$ts_harv_end, rep(100, 4))
})

test_that('extract_layer reads static layers', {
  dir <- withr::local_tempdir()
  toy_rasters(dir)
  out <- extract_layer(toy_steps(), file.path(dir, 'elev.tif'), NA, NA, 'elev', 'end', crs = crs)
  expect_equal(out$elev_end, c(100, 10, 91, 100))   # cells numbered from the top left
})

test_that('combine_extracted joins layers and catches mistakes', {
  steps <- toy_steps()
  a <- data.table(a_end = 1:4)
  b <- data.table(b_end = 5:8)
  out <- combine_extracted(steps, a, b)
  expect_equal(names(out), c(names(steps), 'a_end', 'b_end'))
  expect_error(combine_extracted(steps, a[1:2]), 'one row per step')
  expect_error(combine_extracted(steps, data.table(year = 1:4)), 'already in the steps')
})
