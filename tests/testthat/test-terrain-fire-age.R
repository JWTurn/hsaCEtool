# Terrain, fire and forest age builders, with small made-up rasters and polygons

# a 2 x 1.5 km area in EPSG:3154 (made up), with 30, 60 and 120 m templates
small_templates <- function(dir) {
  area <- terra::vect('POLYGON ((500100 6700100, 502100 6700100, 502100 6701600, 500100 6701600, 500100 6700100))',
                      crs = 'EPSG:3154')
  make_templates(area, list(base_res = 30, buffer_m = 100, factors = list(2, 4)), 'EPSG:3154', list(), dir)
}

test_that('terrain: mean elevation, northness from true north, and flat ground', {
  dir <- withr::local_tempdir()
  tf <- small_templates(dir)
  margin <- make_margin_template(tf[1], 300, file.path(dir, 'margin.tif'))
  # made-up elevation falling toward the north, on a larger 10 m grid
  dem <- terra::rast(terra::extend(terra::ext(terra::rast(margin)), 200), resolution = 10, crs = 'EPSG:3154')
  terra::values(dem) <- 2000 - terra::xyFromCell(dem, seq_len(terra::ncell(dem)))[, 2] / 100
  f <- file.path(dir, 'dem.tif'); terra::writeRaster(dem, f)
  files <- build_terrain_layers(f, tf, margin, c(60, 120), 'fine', file.path(dir, 'out'))
  expect_setequal(basename(files), c('terrain_elevation_60m.tif', 'terrain_elevation_120m.tif',
                                     'terrain_northness_60m.tif', 'terrain_northness_120m.tif'))
  e60 <- terra::rast(files[basename(files) == 'terrain_elevation_60m.tif'])
  # each cell is the mean of its 30 m cells: on a plane, the height at its centre
  y <- terra::xyFromCell(e60, terra::cells(e60))[, 2]
  expect_equal(terra::values(e60, na.rm = TRUE)[, 1], 2000 - y / 100, tolerance = 1e-3)
  n <- terra::values(terra::rast(files[basename(files) == 'terrain_northness_120m.tif']), na.rm = TRUE)
  expect_true(all(abs(n - 1) < 0.01))   # near UTM 7's central meridian, grid north is true north
  coarse <- build_terrain_layers(f, tf, margin, 120, 'coarse', file.path(dir, 'coarse'))
  expect_true(all(abs(terra::values(terra::rast(coarse[2]), na.rm = TRUE) - 1) < 0.01))
  # flat ground: northness 0
  terra::values(dem) <- 500
  terra::writeRaster(dem, f, overwrite = TRUE)
  flat <- build_terrain_layers(f, tf, margin, 60, 'fine', file.path(dir, 'flat'))
  expect_equal(unique(terra::values(terra::rast(flat[2]), na.rm = TRUE)[, 1]), 0)
  # static layers are picked without a year
  expect_equal(basename(pick_layer_files(files, 'terrain_elevation', 60, NA, NA)), 'terrain_elevation_60m.tif')
  expect_error(pick_layer_files(files, 'terrain_elevation', 240, NA, NA), 'Not built')
})

test_that('northness is measured from true north, not grid north', {
  # far east of UTM 7's central meridian, grid north is about 14 degrees off
  r <- terra::rast(nrows = 40, ncols = 40, xmin = 1500000, xmax = 1501200, ymin = 6500000, ymax = 6501200,
                   crs = 'EPSG:3154')
  ll <- sf::sf_project('EPSG:3154', 'EPSG:4326', terra::xyFromCell(r, seq_len(terra::ncell(r))))
  terra::values(r) <- -ll[, 2] * 111000   # falls toward true north
  expect_equal(terra::global(northness_raster(r), 'mean', na.rm = TRUE)[[1]], 1, tolerance = 1e-3)
  expect_lt(terra::global(cos(terra::terrain(r, 'aspect', unit = 'radians')), 'mean', na.rm = TRUE)[[1]], 0.98)
  terra::values(r) <- -ll[, 1] * 50000    # falls toward true east
  expect_equal(terra::global(northness_raster(r), 'mean', na.rm = TRUE)[[1]], 0, tolerance = 1e-3)
})

test_that('fire polygons get their year, with older NFDB years fixed', {
  dir <- withr::local_tempdir()
  tf <- small_templates(dir)
  sq <- function(x0, y0, d = 200) sf::st_polygon(list(rbind(c(x0, y0), c(x0 + d, y0), c(x0 + d, y0 + d), c(x0, y0 + d), c(x0, y0))))
  nbac <- sf::st_sf(YEAR = c(2015L, 2020L), geometry = sf::st_sfc(sq(500200, 6700200), sq(501000, 6700200), crs = 'EPSG:3154'))
  nfdb <- sf::st_sf(YEAR = c(1960L, -9999L, -9999L, 1990L), REP_DATE = c(NA, '1965/07/01', NA, NA),
                    FIRE_ID = c('a', 'b', '1950-ABC', 'c'),
                    geometry = sf::st_sfc(sq(500200, 6701000), sq(500600, 6701000), sq(501000, 6701000),
                                          sq(501400, 6701000), crs = 'EPSG:3154'))
  sf::st_write(nbac, file.path(dir, 'nbac.shp'), quiet = TRUE)
  sf::st_write(nfdb, file.path(dir, 'nfdb.shp'), quiet = TRUE)
  out <- prep_fire_polygons(file.path(dir, 'nbac.shp'), file.path(dir, 'nfdb.shp'), tf[1], 'EPSG:3154',
                            file.path(dir, 'fires.gpkg'))
  # NFDB 1990 is after 1972 (NBAC covers it), so left out
  expect_setequal(sf::st_read(out, quiet = TRUE)$year, c(2015L, 2020L, 1960L, 1965L, 1950L))
  only_nbac <- prep_fire_polygons(file.path(dir, 'nbac.shp'), NULL, tf[1], 'EPSG:3154', file.path(dir, 'f2.gpkg'))
  expect_setequal(sf::st_read(only_nbac, quiet = TRUE)$year, c(2015L, 2020L))
  expect_equal(fire_year_groups(out, 2000), list(c(1950L, 1960L, 1965L), 2015L, 2020L))
})

test_that('fire cells keep every year from the cutoff, and windows count recent fires', {
  dir <- withr::local_tempdir()
  tf <- small_templates(dir)
  sq <- function(x0, y0, d) sf::st_polygon(list(rbind(c(x0, y0), c(x0 + d, y0), c(x0 + d, y0 + d), c(x0, y0 + d), c(x0, y0))))
  # one area burned in 1950 and 1960 (older group) and again in 2015; another in 2019
  fires <- sf::st_sf(year = c(1950L, 1960L, 2015L, 2019L),
                     geometry = sf::st_sfc(sq(500220, 6700220, 120), sq(500220, 6700220, 120),
                                           sq(500220, 6700220, 120), sq(501220, 6700220, 120), crs = 'EPSG:3154'))
  f <- file.path(dir, 'fires.gpkg'); sf::st_write(fires, f, layer = 'fire', quiet = TRUE)
  groups <- fire_year_groups(f, 2000)
  history <- data.table::rbindlist(purrr::map(groups, function(g) fire_cells(f, g, tf[1])))
  expect_equal(sort(unique(history$year)), c(1960L, 2015L, 2019L))   # 1950 replaced by 1960 in the older group
  expect_equal(nrow(history[year == 2015]), 16)                       # 120 x 120 m = 16 cells of 30 m
  out <- file.path(dir, 'out')
  dens2 <- terra::rast(build_harvest_layers(history, 'dens', 120, 2020, 2, tf, out, 'fire_w2'))
  dens60 <- terra::rast(build_harvest_layers(history, 'dens', 120, 2020, 60, tf, out, 'fire_w60'))
  # burned 30 m cells = share burned x 30 m cells per coarse cell (the 120 m template)
  n <- terra::values(terra::rast(tf[3]))
  expect_equal(sum(terra::values(dens2) * n, na.rm = TRUE), 16)    # only the 2019 fire is in the past 2 years
  expect_equal(sum(terra::values(dens60) * n, na.rm = TRUE), 32)   # the 2015 and 2019 fires
  yr <- terra::rast(build_harvest_layers(history, 'year', 120, 2016, Inf, tf, out, 'fire'))
  expect_equal(max(terra::values(yr), na.rm = TRUE), 2015)        # most recent fire up to 2016
})

test_that('forest age adds years since the map, resets after disturbance, and goes back before it', {
  dir <- withr::local_tempdir()
  tf <- small_templates(dir)
  fine <- terra::rast(tf[1])
  # made-up maps: age 50 everywhere in 2010, with one non-forest cell (NA)
  m10 <- terra::rast(fine); terra::values(m10) <- 50
  nf <- terra::cells(fine)[1]
  m10[nf] <- NA
  f10 <- file.path(dir, 'map_2010.tif'); terra::writeRaster(terra::mask(m10, fine), f10, datatype = 'INT2S')
  # a forested cell burned in 2012, and the non-forest cell too
  burned <- terra::cells(fine)[200]
  resets <- data.table::data.table(cell = c(burned, nf), year = 2012L)
  out <- file.path(dir, 'out')
  a13 <- build_forest_age_year(f10, 2010L, 2013L, resets, c(30, 120), tf, out)
  expect_equal(basename(a13), c('forest_age_30m_2013.tif', 'forest_age_120m_2013.tif'))
  v <- terra::rast(a13[1])
  expect_equal(v[burned][[1]], 1)          # 2013 - 2012
  expect_true(is.na(v[nf][[1]]))           # not forest stays NA
  expect_equal(median(terra::values(v), na.rm = TRUE), 53)
  # before the map: 2008 is 48, and the cell disturbed between 2008 and 2010 is unknown
  resets2 <- data.table::data.table(cell = burned, year = 2009L)
  v08 <- terra::rast(build_forest_age_year(f10, 2010L, 2008L, resets2, 30, tf, out))
  expect_true(is.na(v08[burned][[1]]))
  expect_equal(median(terra::values(v08), na.rm = TRUE), 48)
  # coarse cells are means of their forested 30 m cells
  c120 <- terra::rast(a13[2])
  expect_equal(max(terra::values(c120), na.rm = TRUE), 53)
  expect_lt(min(terra::values(c120), na.rm = TRUE), 53)   # the cell holding the burned 30 m cell
  # the latest map at or before each year, or the first for earlier years
  expect_equal(age_maps_used(c(2005, 2010, 2015, 2020), 2008, 2016), c(2005L, 2010L, 2015L))
  expect_equal(age_maps_used(2019, 2012, 2025), 2019L)
})

test_that('read_layers: terrain is not yearly, and windows are whole years', {
  dir <- withr::local_tempdir()
  ok <- data.table(name = c('elev', 'fd'), first_year = c(NA, 2012), last_year = c(NA, 2020),
                   build = c('elevation', 'fire_dens'), class = c(NA, '60'))
  fwrite(ok, file.path(dir, 'ok.csv'))
  expect_equal(nrow(read_layers(file.path(dir, 'ok.csv'))), 2)
  bad <- data.table(name = c('elev', 'fd'), first_year = c(2012, 2012), last_year = c(2020, 2020),
                    build = c('elevation', 'fire_dens'), class = c(NA, 'sixty'))
  fwrite(bad, file.path(dir, 'bad.csv'))
  err <- tryCatch(read_layers(file.path(dir, 'bad.csv')), error = conditionMessage)
  expect_match(err, 'do not change by year.*: elev')
  expect_match(err, 'whole number above 0: fd')
})

test_that('fires up to the first year put on together give the same layers as every year separately', {
  dir <- withr::local_tempdir()
  tf <- small_templates(dir)
  sq <- function(x0, y0, d) sf::st_polygon(list(rbind(c(x0, y0), c(x0 + d, y0), c(x0 + d, y0 + d), c(x0, y0 + d), c(x0, y0))))
  # overlapping made-up fires from 1950 to 2014
  fires <- sf::st_sf(year = c(1950L, 1990L, 2005L, 2011L, 2013L, 2014L),
                     geometry = sf::st_sfc(sq(500150, 6700150, 600), sq(500300, 6700300, 600), sq(500500, 6700200, 400),
                                           sq(500900, 6700900, 300), sq(500200, 6700800, 500), sq(501500, 6700500, 300),
                                           crs = 'EPSG:3154'))
  f <- file.path(dir, 'fires.gpkg'); sf::st_write(fires, f, layer = 'fire', quiet = TRUE)
  hist_of <- function(cutoff) data.table::rbindlist(purrr::map(fire_year_groups(f, cutoff), function(g) fire_cells(f, g, tf[1])))
  grouped <- hist_of(2013)        # first year built 2012
  every <- hist_of(1900)          # every year on its own
  expect_lt(nrow(grouped), nrow(every))
  for (w in c(2, 60)) for (m in c('year', 'dens', 'pa')) {
    a <- build_harvest_layers(grouped, m, c(60, 120), 2012:2014, if (m == 'year') Inf else w, tf, file.path(dir, 'a'), 'g')
    b <- build_harvest_layers(every, m, c(60, 120), 2012:2014, if (m == 'year') Inf else w, tf, file.path(dir, 'b'), 'g')
    for (i in seq_along(a)) expect_equal(terra::values(terra::rast(a[i])), terra::values(terra::rast(b[i])))
  }
})

test_that('layer years come from the layer table, then tracks: min_year/max_year, then the steps', {
  expect_equal(layer_year_range(2012, 2025, 1996:2026), c(2012L, 2025L))
  expect_equal(layer_year_range(NULL, NULL, c(1999L, 2004L, 2020L)), c(1999L, 2020L))
  expect_equal(layer_year_range(2015, NULL, 2010:2022), c(2015L, 2022L))
  expect_equal(fill_years(c(NA, 2014L), 2012L), c(2012L, 2014L))
  # commands: numbers when the table gives the years, layer_years otherwise
  expect_equal(year_bound(c(2012L, 2010L), 1), 2010L)
  expect_true(is.call(year_bound(c(2012L, NA), 1)))
  layer_years <- c(2008L, 2020L)
  # blanks filled from layer_years: first min(2008, 2012), last max(2024, 2020)
  expect_equal(eval(years_call(data.table(first_year = c(NA, 2012L), last_year = c(2024L, NA)))), 2008:2024)
  expect_equal(eval(years_call(data.table(first_year = c(2012L, 2013L), last_year = c(2024L, 2015L)))), 2012:2024)
  # blank tracks years keep every year
  DT <- toy_locations(2)[, t_ := as.POSIXct(c('2001-05-01', '2015-05-01', '2020-05-01', '2030-01-01'), tz = 'UTC')]
  expect_equal(nrow(filter_years(DT, NULL, NULL)), 4)
  expect_equal(nrow(filter_years(DT, 2010, NULL)), 3)
})

test_that('read_layers: built layers can leave their years blank; other layers need both or neither', {
  dir <- withr::local_tempdir()
  ok <- data.table(name = c('fy', 'hv', 'elev'), first_year = c(NA, NA, NA), last_year = c(NA, 2024, NA),
                   build = c('fire_year', 'harvest_year', 'elevation'), time_since = c(TRUE, TRUE, FALSE))
  fwrite(ok, file.path(dir, 'ok.csv'))
  l <- read_layers(file.path(dir, 'ok.csv'))
  expect_equal(l$yearly, c(TRUE, TRUE, FALSE))
  bad <- data.table(name = 'p', first_year = 2012, last_year = NA, path = 'x_{year}.tif')
  fwrite(bad, file.path(dir, 'bad.csv'))
  expect_error(read_layers(file.path(dir, 'bad.csv')), 'both first_year and last_year, or neither')
  # extraction fills blank years from the layer year range
  idx <- make_layer_index(l, list(paste0('f', 2012:2015), c('h', 'h'), 'e'), years = c(2012L, 2015L))
  expect_equal(idx$first_year, c(2012L, 2012L, NA))
  expect_equal(idx$last_year, c(2015L, 2024L, NA))
})
