# Human disturbance layers and the distance margin, with small made-up features

# Fine grid 8 x 8 cells of 30 m (240 m square); coarse template 120 m (factor 4);
# the template with a 60 m margin is 12 x 12 cells
d_grid <- function(dir) {
  fine <- terra::rast(xmin = 0, xmax = 240, ymin = 0, ymax = 240, res = 30, crs = 'EPSG:3154', vals = 1)
  f <- file.path(dir, 'fine.tif'); terra::writeRaster(fine, f)
  tf <- make_templates(NULL, list(source = list(path = f), base_res = 30, factors = list(4)),
                       'EPSG:3154', list(), file.path(dir, 'templates'))
  list(templates = tf, margin = make_margin_template(tf[1], 60, file.path(dir, 'templates', 'margin.tif')))
}

# Made-up features: a road across the top row of the template (2015, and again
# as a duplicate in 2018), a road in the margin with no year, a trail, and harvest
d_source <- function(dir) {
  x <- sf::st_sf(
    Type = c('Highway', 'Highway', 'Highway', 'Trail', 'Harvest'),
    yr = c('2015-06-01', '2018', NA, '2001', '2010'),
    geometry = sf::st_sfc(
      sf::st_linestring(rbind(c(5, 225), c(235, 225))),
      sf::st_linestring(rbind(c(5, 225), c(235, 225))),
      sf::st_linestring(rbind(c(-45, 5), c(-45, 295))),
      sf::st_linestring(rbind(c(5, 105), c(235, 105))),
      sf::st_linestring(rbind(c(5, 45), c(235, 45))),
      crs = 'EPSG:3154'))
  f <- file.path(dir, 'lines.gpkg')
  sf::st_write(x, f, quiet = TRUE)
  f
}
d_src <- list(url = 'made-up', files = list(list(
  file = 'lines.gpkg', year = list('yr'),
  rules = list(list(where = list(Type = 'Highway'), category = 'paved'),
               list(where = list(Type = 'Harvest'), category = 'drop'),
               list(category = 'lfother')))))

test_that('make_margin_template extends the template by whole cells', {
  dir <- withr::local_tempdir()
  g <- d_grid(dir)
  m <- terra::rast(g$margin)
  expect_equal(unname(as.vector(terra::ext(m))), c(-60, 300, -60, 300))
  expect_equal(terra::res(m), c(30, 30))
  expect_true(all(terra::values(m) == 1))
  expect_equal(unname(as.vector(terra::ext(terra::rast(make_margin_template(g$templates[1], 0, file.path(dir, 'm0.tif')))))),
               c(0, 240, 0, 240))
})

test_that('feature_year takes the first field with a year', {
  d <- data.table(a = c('1993-03-01', NA, 'unknown', NA),
                  b = c(2001, 2005, 20070207, NA),
                  c = as.POSIXct(c(NA, NA, NA, '2019-02-14 19:00:00'), tz = 'UTC'))
  expect_equal(feature_year(d, c('a', 'b', 'c')), c(1993L, 2005L, 2007L, 2019L))
  expect_equal(feature_year(d, character()), rep(NA_integer_, 4))
  expect_error(feature_year(d, 'nope', 'f.gpkg'), 'no year field "nope"')
})

test_that('disturbance_category uses the first matching rule', {
  d <- data.table(Industry = c('Transportation', 'Forestry', 'Urban/Rural', 'Mining'))
  fs <- list(file = 'roads', rules = list(
    list(where = list(Industry = list('Transportation', 'Urban/Rural')), category = 'paved'),
    list(where = list(Industry = 'Mining'), category = 'drop'),
    list(category = 'unpaved')))
  expect_equal(as.vector(disturbance_category(d, fs)), c('paved', 'unpaved', 'paved', 'drop'))
  expect_equal(as.vector(disturbance_category(d, list(category = 'lfother'))), rep('lfother', 4))
  fs$rules[[3]] <- NULL
  expect_warning(out <- disturbance_category(d, fs), '1 features match no rule')
  expect_true(is.na(out[2]))
})

test_that('add_lookup adds columns from a table and warns about missing types', {
  dir <- withr::local_tempdir()
  f <- file.path(dir, 'reclass.csv')
  fwrite(data.table(TYPE_INDUS = c('Transportation', 'Mining '), TYPE_DISTU = c('Highway', 'Trail'),
                    LF_Type = c('Highway', 'Other LF')), f)
  d <- data.table(INDUSTRY = c('Transportation', 'Mining', 'Mining'), DISTURB = c('Highway', 'Trail', 'Trench'))
  lookup <- list(table = 'yt', by = list(INDUSTRY = 'TYPE_INDUS', DISTURB = 'TYPE_DISTU'), add = 'LF_Type')
  expect_warning(out <- add_lookup(d, lookup, list(yt = f), 'lines.shp'), 'Mining / Trench \\(1\\)')
  expect_equal(out$LF_Type, c('Highway', 'Other LF', NA))
  expect_equal(out$DISTURB, d$DISTURB)            # rows stay in order
  fwrite(data.table(TYPE_INDUS = c('Mining', 'Mining'), TYPE_DISTU = c('Trail', 'Trail'),
                    LF_Type = c('Other LF', 'Unpaved Road')), f)
  expect_error(add_lookup(d, lookup, list(yt = f), 'lines.shp'), 'more than one row')
})

test_that('disturbance features are sorted, dated and put on the grid with the margin', {
  dir <- withr::local_tempdir()
  g <- d_grid(dir)
  files <- d_source(dir)
  feats <- prep_disturbance_features(files, d_src, list(), g$margin, 'EPSG:3154',
                                     file.path(dir, 'out', 'features.gpkg'))
  x <- sf::st_read(feats, layer = "disturbance", quiet = TRUE)
  expect_equal(sort(x$category), c('lfother', 'paved', 'paved', 'paved'))   # harvest dropped
  expect_equal(sort(x$year, na.last = TRUE), c(2001L, 2015L, 2018L, NA))

  h <- disturbance_cells(feats, 'paved', g$margin)
  # top-row road: 8 cells from 2015 (the 2018 copy does not replace it);
  # margin road (y 5 to 295): 10 cells, no year (0 = every year)
  expect_equal(nrow(h), 18)
  expect_equal(sum(h$year == 2015), 8)
  expect_equal(sum(h$year == 0), 10)
  expect_equal(nrow(disturbance_cells(feats, 'quartz', g$margin)), 0)
  expect_equal(disturbance_years(h, 2012, 2024), c(2012L, 2015L))
})

test_that('the summary counts the features of each type in each category, dropped ones too', {
  dir <- withr::local_tempdir()
  g <- d_grid(dir)
  src_files <- d_source(dir)
  feats <- prep_disturbance_features(src_files, d_src, list(), g$margin, 'EPSG:3154',
                                     file.path(dir, 'out', 'features_made.gpkg'))
  s <- summarize_disturbance(feats)
  expect_equal(unique(s$source), 'made')
  expect_equal(s$category, c('drop', 'lfother', 'paved'))
  expect_equal(s$type, c('Type=Harvest', 'Type=Trail', 'Type=Highway'))
  paved <- s[category == 'paved']
  expect_equal(paved$features, 3L)
  expect_equal(paved$length_km, 0.75)   # 230 + 230 + 290 m
  expect_true(is.na(paved$area_km2))
  expect_equal(paved$no_year, 1L)
  expect_equal(c(paved$first_year, paved$last_year), c(2015L, 2018L))

  # a file with one category for all its features has no type
  one <- list(url = 'made-up', files = list(list(file = 'lines.gpkg', category = 'lfother')))
  s1 <- summarize_disturbance(prep_disturbance_features(src_files, one, list(), g$margin, 'EPSG:3154',
                                                        file.path(dir, 'out', 'features_one.gpkg')))
  expect_equal(nrow(s1), 1)
  expect_true(is.na(s1$type))
  expect_equal(s1$features, 5L)
})

test_that('disturbance layers count every feature up to the year, and distances use the margin', {
  dir <- withr::local_tempdir()
  g <- d_grid(dir)
  feats <- prep_disturbance_features(d_source(dir), d_src, list(), g$margin, 'EPSG:3154',
                                     file.path(dir, 'out', 'features.gpkg'))
  h <- disturbance_cells(feats, 'paved', g$margin)
  out <- file.path(dir, 'layers')
  pa <- build_disturbance_layers(h, 'pa', 120, c(2012, 2016), g$templates, out, 'disturb_paved', g$margin)
  expect_equal(basename(pa), c('disturb_paved_pa_120m_2012.tif', 'disturb_paved_pa_120m_2016.tif'))
  expect_equal(as.vector(terra::values(terra::rast(pa[1]))), c(0, 0, 0, 0))   # margin road is outside
  expect_equal(as.vector(terra::values(terra::rast(pa[2]))), c(1, 1, 0, 0))
  dens <- build_disturbance_layers(h, 'dens', 120, 2016, g$templates, out, 'disturb_paved', g$margin)
  expect_equal(as.vector(terra::values(terra::rast(dens))), c(0.25, 0.25, 0, 0))
  # in 2012 only the margin road (cell centres at x = -45) counts: fine cells in
  # columns 0-3 are 60-150 m from it (mean 105), columns 4-7 180-270 m (mean 225)
  dist <- build_disturbance_layers(h, 'dist', 120, 2012, g$templates, out, 'disturb_paved', g$margin)
  expect_equal(as.vector(terra::values(terra::rast(dist))), c(105, 225, 105, 225), tolerance = 1e-4)
})

test_that('harvest cells numbered on the margin grid give the same layers', {
  dir <- withr::local_tempdir()
  g <- d_grid(dir)
  # fine cell 1 (row 0, col 0) is margin cell 2 * 12 + 2 + 1 = 27; margin cell 1 is outside
  plain <- build_harvest_layers(data.table(cell = 1, year = 2015L), 'pa', 120, 2015, 2,
                                g$templates, file.path(dir, 'a'))
  marg <- build_harvest_layers(data.table(cell = c(27, 1), year = 2015L), 'pa', 120, 2015, 2,
                               g$templates, file.path(dir, 'b'), margin = g$margin)
  expect_equal(as.vector(terra::values(terra::rast(marg))), as.vector(terra::values(terra::rast(plain))))
})

test_that('pick_layer_files can fill years from the latest earlier layer', {
  files <- file.path('out', sprintf('disturb_paved_pa_240m_%d.tif', c(2012, 2016)))
  expect_equal(pick_layer_files(files, 'disturb_paved_pa', 240, 2012, 2017, fill = TRUE),
               files[c(1, 1, 1, 1, 2, 2)])
  expect_error(pick_layer_files(files, 'disturb_paved_pa', 240, 2010, 2012, fill = TRUE), '2010, 2011')
})

test_that('check_config catches disturbance classes no rule gives', {
  cfg <- test_config()
  cfg$layers[name == 'dist_paved_240', class := 'pavement']
  expect_error(check_config(cfg), 'class not given to any feature.*dist_paved_240')
})


test_that('rules can match patterns and give unpaved roads a type', {
  d <- data.table(Surface = c('Paved 2 Lane', 'Paved 2  Lane', 'Loose 1 Lane', 'Winter Ice Road', 'Rough 2 Lane', 'Ferry'),
                  Use = c('Public', 'Public', 'Forestry', 'Oil', 'Public', 'Public'))
  fs <- list(file = 'roads.gpkg', rules = list(
    list(match = list(Surface = '^Ferry'), category = 'drop'),
    list(match = list(Surface = '^Paved'), category = 'paved'),
    list(match = list(Surface = '^Winter'), category = 'unpaved', subtype = 'winter'),
    list(where = list(Use = c('Forestry', 'Oil')), category = 'unpaved', subtype = 'resource'),
    list(category = 'unpaved', subtype = 'all-season')))
  cat <- disturbance_category(d, fs)
  expect_equal(as.vector(cat), c('paved', 'paved', 'unpaved', 'unpaved', 'unpaved', 'drop'))
  expect_equal(disturbance_subtype(d, fs, attr(cat, 'rule')), c(NA, NA, 'resource', 'winter', 'all-season', NA))
  # a whole file, or a field
  expect_equal(disturbance_subtype(d, list(file = 'x', category = 'unpaved', subtype = 'resource'), rep(NA_integer_, 6)),
               rep('resource', 6))
  expect_equal(disturbance_subtype(d, list(file = 'x', category = 'unpaved', subtype_field = 'Use'), rep(NA_integer_, 6)), d$Use)
})

test_that('kept columns and the combined lines and polygons files', {
  dir <- withr::local_tempdir()
  g <- d_grid(dir)
  src_files <- d_source(dir)
  ka <- d_src; ka$files[[1]]$keep <- list('Type')
  a <- prep_disturbance_features(src_files, ka, list(), g$margin, 'EPSG:3154', file.path(dir, 'out', 'features_a.gpkg'))
  expect_true('Type' %in% names(sf::st_read(a, layer = 'disturbance', quiet = TRUE)))
  p <- sf::st_sf(id = 1, geometry = sf::st_sfc(sf::st_polygon(list(rbind(c(10, 10), c(50, 10), c(50, 50), c(10, 10)))), crs = 'EPSG:3154'))
  pf <- file.path(dir, 'polys.gpkg'); sf::st_write(p, pf, quiet = TRUE)
  b <- prep_disturbance_features(pf, list(url = 'x', files = list(list(file = 'polys.gpkg', category = 'otherpoly'))), list(),
                                 g$margin, 'EPSG:3154', file.path(dir, 'out', 'features_b.gpkg'))
  f <- combine_disturbance_features(c(a = a, b = b), list(regions = list(a = 'YT', b = 'BC'),
                                    files = list(lines = 'lines.gpkg', areal = 'areal.gpkg')), file.path(dir, 'combined'))
  l <- sf::st_read(f[1], quiet = TRUE); r <- sf::st_read(f[2], quiet = TRUE)
  expect_equal(nrow(l), 4); expect_equal(nrow(r), 1)
  expect_equal(unique(l$region), 'YT'); expect_equal(r$region, 'BC')
  expect_true(is.na(r$Type))
  expect_equal(names(l)[1:6], c('region', 'source', 'file', 'category', 'subtype', 'year'))
  # sources with different kept columns stack fine for the cells
  expect_equal(nrow(disturbance_cells(c(a, b), 'paved', g$margin)), 18)
})
