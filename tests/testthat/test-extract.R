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

test_that('extract_year matches each step to its year, clamping outside years', {
  dir <- withr::local_tempdir()
  toy_rasters(dir)
  index <- make_layer_index(data.table(name = c('val', 'elev'), first_year = c(2018L, NA), last_year = c(2020L, NA),
                                       time_since = FALSE, ts_nodata = NA_real_),
                            list(file.path(dir, paste0('val_', 2018:2020, '.tif')), file.path(dir, 'elev.tif')))
  out <- extract_all_years(toy_steps(), index)
  expect_named(out, c('row', 'val_start', 'val_end', 'elev_start', 'elev_end'))
  expect_equal(out$row, 1:4)
  expect_equal(out$val_end, c(2018, 2018, 2019, 2020))
  expect_equal(out$val_start, c(2018, 2018, 2019, NA))   # missing start coordinates
  expect_equal(out$elev_end, c(100, 10, 91, 100))         # cells numbered from the top left
  expect_named(extract_all_years(toy_steps(), index, 'end'), c('row', 'val_end', 'elev_end'))
  expect_error(extract_year(make_step_points(toy_steps()), index), 'one year')
})

test_that('extract_year adds years since disturbance', {
  dir <- withr::local_tempdir()
  toy_rasters(dir)
  z <- file.path(dir, 'zero.tif')
  terra::writeRaster(terra::rast(terra::rast(file.path(dir, 'val_2018.tif')), vals = 0), z)
  index <- make_layer_index(data.table(name = c('fires', 'harv'), first_year = 2018L, last_year = 2020L,
                                       time_since = TRUE, ts_nodata = c(200, 100)),
                            list(file.path(dir, paste0('fires_', 2018:2020, '.tif')), rep(z, 3)))
  out <- extract_all_years(toy_steps(), index, 'end')
  # bottom half burned (year - 5); top half never burned
  expect_equal(out$ts_fires_end, c(4, 200, 5, 6))
  # 0 = never disturbed, as in the 250 m harvest rasters
  expect_equal(out$ts_harv_end, rep(100, 4))
})

test_that('extract_year reads stacks on different grids and coordinate systems like terra::extract', {
  dir <- withr::local_tempdir()
  toy_rasters(dir)
  # a coarser grid in another coordinate system, covering the toy area
  a <- terra::project(terra::rast(file.path(dir, 'elev.tif')), 'EPSG:3005', res = 200)
  terra::writeRaster(a, file.path(dir, 'other.tif'))
  index <- make_layer_index(data.table(name = c('val', 'elev', 'other'), first_year = c(2018L, NA, NA),
                                       last_year = c(2020L, NA, NA), time_since = FALSE, ts_nodata = NA_real_),
                            list(file.path(dir, paste0('val_', 2018:2020, '.tif')), file.path(dir, 'elev.tif'),
                                 file.path(dir, 'other.tif')))
  set.seed(3)
  # made-up steps: shared starts, as for an observed step and its random steps
  steps <- data.table(year = 2019L, x1_ = rep(c(150, 820), each = 5), y1_ = rep(c(330, 610), each = 5),
                      x2_ = runif(10, 0, 1000), y2_ = runif(10, 0, 1000))
  out <- extract_all_years(steps, index)
  pts_start <- terra::vect(as.matrix(steps[, .(x1_, y1_)]), crs = crs)
  pts_end <- terra::vect(as.matrix(steps[, .(x2_, y2_)]), crs = crs)
  expect_equal(out$elev_end, terra::extract(terra::rast(file.path(dir, 'elev.tif')), pts_end)[[2]])
  expect_equal(out$other_end, terra::extract(a, terra::project(pts_end, 'EPSG:3005'))[[2]])
  expect_equal(out$other_start, terra::extract(a, terra::project(pts_start, 'EPSG:3005'))[[2]])
  expect_equal(out$val_start, rep(2019, 10))
})

test_that('the layer index changes when a layer is rebuilt into the same files', {
  dir <- withr::local_tempdir()
  toy_rasters(dir)
  lay <- data.table(name = 'val', first_year = 2018L, last_year = 2020L, time_since = FALSE, ts_nodata = NA_real_)
  files <- list(file.path(dir, paste0('val_', 2018:2020, '.tif')))
  before <- make_layer_index(lay, files)
  r <- terra::rast(files[[1]][2])
  terra::writeRaster(r + 1, files[[1]][2], overwrite = TRUE)
  after <- make_layer_index(lay, files)
  expect_identical(before$files, after$files)
  expect_false(identical(before$files_hash, after$files_hash))
  expect_identical(make_layer_index(lay, list('https://example.org/a.tif'))$files_hash,
                   make_layer_index(lay, list('https://example.org/a.tif'))$files_hash)
})

test_that('each year is saved as a Parquet file that can be read by column', {
  dir <- withr::local_tempdir()
  toy_rasters(dir)
  index <- make_layer_index(data.table(name = 'val', first_year = 2018L, last_year = 2020L,
                                       time_since = FALSE, ts_nodata = NA_real_),
                            list(file.path(dir, paste0('val_', 2018:2020, '.tif'))))
  pts <- make_step_points(toy_steps())
  f <- write_extracted_year(pts[year == 2019L], index, crs = crs, out_dir = file.path(dir, 'out'))
  expect_equal(basename(f), 'extracted_2019.parquet')
  back <- nanoparquet::read_parquet(f, col_select = c('row', 'val_end'))
  expect_equal(names(back), c('row', 'val_end'))
  expect_equal(back$val_end, extract_year(pts[year == 2019L], index, crs = crs)$val_end)
})

test_that('availability summarizes each layer for used and available steps, overall and by group', {
  dir <- withr::local_tempdir()
  toy_rasters(dir)
  index <- make_layer_index(data.table(name = c('elev', 'fires'), first_year = c(NA, 2018L),
                                       last_year = c(NA, 2020L), time_since = c(FALSE, TRUE),
                                       ts_nodata = c(NA, 200)),
                            list(file.path(dir, 'elev.tif'), file.path(dir, paste0('fires_', 2018:2020, '.tif'))))
  # made-up steps in two years and two jurisdictions; one end outside the rasters
  steps <- data.table(year = rep(c(2018L, 2019L), each = 4), case_ = rep(c(TRUE, FALSE, FALSE, FALSE), 2),
                      juris = rep(c('yt', 'bc'), 4), study = 'A',
                      x1_ = 50, y1_ = 950, x2_ = c(50, 150, 250, 2000, 50, 150, 250, 350), y2_ = 50)
  files <- purrr::map_chr(split(make_step_points(steps), by = 'tar_group'), write_extracted_year,
                          index = index, crs = crs, out_dir = dir)
  groups <- make_step_groups(steps, c('juris', 'study'))
  out <- summarize_availability(files, 'elev', groups, c('juris', 'study'))
  expect_equal(unique(out$column), c('elev_start', 'elev_end'))
  expect_named(out, c('layer', 'column', 'where', 'used', 'juris', 'study', 'n', 'min', 'mean', 'median',
                      'max', 'share_above_0', 'share_missing'))
  # overall available ends: 92, 93, NA (outside), 92, 93, 94 (cells along the bottom row)
  avail <- out[column == 'elev_end' & used == FALSE & juris == 'all']
  expect_equal(avail[, .(n, min, mean, median, max, share_above_0)],
               data.table(n = 6L, min = 92, mean = 92.8, median = 93, max = 94, share_above_0 = 1))
  expect_equal(avail$share_missing, 1 / 6)
  # by jurisdiction, then by jurisdiction and study
  expect_equal(out[column == 'elev_end' & juris == 'yt' & study == 'all', sort(n)], c(2L, 2L))
  expect_equal(nrow(out[column == 'elev_end' & study == 'A']), 3)   # used yt, available yt, available bc
  # time since fire is summarized too
  fa <- summarize_availability(files, 'fires', groups, 'juris')
  expect_true(all(c('ts_fires_start', 'ts_fires_end') %in% fa$column))
  expect_equal(unique(fa[startsWith(column, 'ts_')]$layer), 'ts_fires')   # listed as its own layer
  expect_error(make_step_groups(steps, 'herd'), 'keep_after_cleaning')
})
