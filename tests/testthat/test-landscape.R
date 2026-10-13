# Template rasters and the CanLaD harvest builder, with small made-up rasters

# A 2 x 1.5 km study area in EPSG:3154 (made up)
toy_area <- function() {
  terra::vect('POLYGON ((500100 6700100, 502100 6700100, 502100 6701600, 500100 6701600, 500100 6700100))',
              crs = 'EPSG:3154')
}
toy_templates <- function(dir, factors = list(2, 4)) {
  make_templates(toy_area(), list(base_res = 30, buffer_m = 100, factors = factors),
                 'EPSG:3154', list(), dir)
}

test_that('lcm finds the least common multiple', {
  expect_equal(lcm(c(8, 40, 160)), 160)
  expect_equal(lcm(c(4, 6)), 12)
})

test_that('make_templates builds nested, snapped templates', {
  dir <- withr::local_tempdir()
  tf <- toy_templates(dir)
  expect_equal(basename(tf), c('template_30m.tif', 'template_60m.tif', 'template_120m.tif'))
  fine <- terra::rast(tf[1])
  expect_equal(terra::res(fine), c(30, 30))
  expect_true(all(as.vector(terra::ext(fine)) %% 120 == 0))       # edges on the coarsest grid
  a <- as.vector(terra::ext(terra::buffer(toy_area(), 100))); e <- as.vector(terra::ext(fine))
  expect_true(e[1] <= a[1] && e[3] <= a[3] && e[2] >= a[2] && e[4] >= a[4])   # covers area + buffer
  c120 <- terra::rast(tf[3])
  expect_equal(terra::res(c120), c(120, 120))
  expect_equal(terra::ext(c120), terra::ext(fine))
  # a coarse cell holds the number of fine cells inside the area
  expect_equal(max(terra::values(c120), na.rm = TRUE), 16)
  expect_equal(sum(terra::values(c120), na.rm = TRUE), sum(terra::values(fine), na.rm = TRUE))
})

test_that('make_templates takes a template raster and checks its cell size', {
  dir <- withr::local_tempdir()
  r <- terra::rast(xmin = 0, xmax = 240, ymin = 0, ymax = 240, res = 30, crs = 'EPSG:3154', vals = 1)
  f <- file.path(dir, 'mine.tif'); terra::writeRaster(r, f)
  tf <- make_templates(NULL, list(source = list(path = f), base_res = 30, factors = list(8)),
                       'EPSG:3154', list(), file.path(dir, 'out'))
  expect_equal(terra::values(terra::rast(tf[2]))[1], 64)
  expect_error(make_templates(NULL, list(source = list(path = f), base_res = 25, factors = list(8)),
                              'EPSG:3154', list(), file.path(dir, 'out')), 'base_res')
})

test_that('canlad_annual_harvest keeps harvested cells inside the template, after reprojecting', {
  dir <- withr::local_tempdir()
  tf <- toy_templates(dir)
  fine <- terra::rast(tf[1])
  # a made-up "CanLaD" map in another projection (Canada Lambert), 30 m cells
  lam <- terra::project(fine, 'EPSG:3979', res = 30)
  terra::values(lam) <- 0
  ctr <- terra::project(terra::vect(cbind(501100, 6700850), crs = 'EPSG:3154'), 'EPSG:3979')
  lam[terra::cellFromXY(lam, terra::crds(ctr))] <- 2       # one harvested cell
  lam[1] <- 1                                               # a fire, not harvest
  src <- file.path(dir, 'canlad_annual_2015_v1.tif'); terra::writeRaster(lam, src, datatype = 'INT1U')
  out <- canlad_annual_harvest(src, 2015, tf[1], code = 2)
  expect_equal(names(out), c('cell', 'year'))
  expect_equal(unique(out$year), 2015L)
  expect_gte(nrow(out), 1)
  # the harvested cells are where the harvest was (within one 30 m cell)
  xy <- terra::xyFromCell(fine, out$cell)
  expect_true(all(abs(xy[, 1] - 501100) <= 45 & abs(xy[, 2] - 6700850) <= 45))
})

test_that('canlad_old_harvest takes the year from the year raster', {
  dir <- withr::local_tempdir()
  tf <- toy_templates(dir)
  fine <- terra::rast(tf[1])
  type <- terra::rast(fine); terra::values(type) <- 1
  year <- terra::rast(fine); terra::values(year) <- 1970
  cell <- terra::cellFromXY(fine, cbind(501100, 6700850))
  type[cell] <- 3; year[cell] <- 1978
  type[cell + 1] <- 2                                       # fire
  terra::writeRaster(type, file.path(dir, 'type.tif'), datatype = 'INT1U')
  terra::writeRaster(year, file.path(dir, 'year.tif'), datatype = 'INT2U')
  out <- canlad_old_harvest(file.path(dir, 'type.tif'), file.path(dir, 'year.tif'), tf[1], code = 3)
  expect_equal(out$cell, cell)
  expect_equal(out$year, 1978L)
})

test_that('raster_source streams by default and checks access', {
  expect_equal(raster_source('https://example.org/a.tif'), '/vsicurl/https://example.org/a.tif')
  expect_error(raster_source('https://example.org/a.tif', access = 'other'), 'stream')
  f <- withr::local_tempfile(fileext = '.tif'); writeLines('x', f)
  expect_equal(raster_source(f, access = 'download'), f)
})

# Fine grid 8 x 8 cells of 30 m; coarse 2 x 2 cells of 120 m (factor 4)
toy_grid <- function(dir) {
  fine <- terra::rast(xmin = 0, xmax = 240, ymin = 0, ymax = 240, res = 30, crs = 'EPSG:3154', vals = 1)
  f <- file.path(dir, 'mine.tif'); terra::writeRaster(fine, f)
  make_templates(NULL, list(source = list(path = f), base_res = 30, factors = list(4)),
                 'EPSG:3154', list(), file.path(dir, 'templates'))
}

test_that('build_harvest_layers: year keeps the most recent harvest up to each year', {
  dir <- withr::local_tempdir()
  tf <- toy_grid(dir)
  # cell 1 (top-left coarse cell) harvested 2005 and 2018; cell 8 (top-right) in 2016
  hist <- data.table(cell = c(1, 1, 8), year = c(2005L, 2018L, 2016L))
  files <- build_harvest_layers(hist, 'year', 120, c(2015, 2018), 2, tf, file.path(dir, 'out'))
  expect_equal(basename(files), c('harvest_year_120m_2015.tif', 'harvest_year_120m_2018.tif'))
  v15 <- terra::values(terra::rast(files[1]))[, 1]
  v18 <- terra::values(terra::rast(files[2]))[, 1]
  expect_equal(v15, c(2005, NA, NA, NA))   # the 2018 harvest does not hide the 2005 one
  expect_equal(v18, c(2018, 2016, NA, NA))
})

test_that('build_harvest_layers: pa and dens count only recent harvest', {
  dir <- withr::local_tempdir()
  tf <- toy_grid(dir)
  hist <- data.table(cell = c(1, 2, 3, 8), year = c(2014L, 2015L, 2015L, 2010L))
  pa <- build_harvest_layers(hist, 'pa', 120, 2015, 2, tf, file.path(dir, 'out'))
  dens <- build_harvest_layers(hist, 'dens', 120, 2015, 2, tf, file.path(dir, 'out'))
  expect_equal(terra::values(terra::rast(pa))[, 1], c(1, 0, 0, 0))     # 2010 is not recent
  expect_equal(terra::values(terra::rast(dens))[, 1], c(3 / 16, 0, 0, 0))
  pa1 <- build_harvest_layers(hist, 'pa', 120, 2015, 1, tf, file.path(dir, 'out1'))
  expect_equal(terra::values(terra::rast(pa1))[, 1], c(1, 0, 0, 0))
  dens1 <- build_harvest_layers(hist, 'dens', 120, 2015, 1, tf, file.path(dir, 'out1'))
  expect_equal(terra::values(terra::rast(dens1))[, 1], c(2 / 16, 0, 0, 0))
})

test_that('build_harvest_layers: dist is the mean distance over the fine cells', {
  dir <- withr::local_tempdir()
  tf <- toy_grid(dir)
  hist <- data.table(cell = 1, year = 2015L)
  f <- build_harvest_layers(hist, 'dist', 120, c(2015, 2020), 2, tf, file.path(dir, 'out'))
  fine <- terra::rast(tf[1])
  d <- terra::distance(terra::rasterize(terra::xyFromCell(fine, 1), terra::rast(fine), values = 1))
  expected <- terra::values(terra::aggregate(d, 4, mean))[, 1]
  expect_equal(terra::values(terra::rast(f[1]))[, 1], expected, tolerance = 1e-4)
  expect_gt(terra::values(terra::rast(f[1]))[4, 1], 0)
  expect_true(all(is.na(terra::values(terra::rast(f[2])))))  # no recent harvest in 2020
})

test_that('pick_layer_files picks one file per year and reports missing years', {
  files <- file.path('out', sprintf('harvest_pa_240m_%d.tif', 2012:2014))
  expect_equal(pick_layer_files(files, 'harvest_pa', 240, 2013, 2014), files[2:3])
  expect_error(pick_layer_files(files, 'harvest_pa', 240, 2013, 2016), '2015')
})

test_that('read_layers accepts built layers, with or without years', {
  dir <- withr::local_tempdir()
  writeLines(c('name,resolution,first_year,last_year,path,time_since,ts_nodata,build',
               'harv240,240,2012,2024,,TRUE,100,harvest_year',
               'pa240,240,,,,FALSE,,harvest_pa'), file.path(dir, 'l.csv'))
  l <- read_layers(file.path(dir, 'l.csv'))
  expect_equal(l$build, c('harvest_year', 'harvest_pa'))
  expect_equal(l$yearly, c(TRUE, TRUE))   # blank years: the layer year range
})
