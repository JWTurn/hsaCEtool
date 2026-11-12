# Layer stacks (model land), with made-up rasters

crs <- 'EPSG:3154'

# 240 m layers on a 10 x 10 grid and a 1200 m layer on the 2 x 2 grid it nests in;
# yearly layers for 2018-2020
toy_land <- function(dir) {
  fine <- terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 2400, ymin = 0, ymax = 2400, crs = crs)
  coarse <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 2400, ymin = 0, ymax = 2400, crs = crs)
  set.seed(1)
  w <- function(r, vals, f) { terra::writeRaster(terra::rast(r, vals = vals), file.path(dir, f)); file.path(dir, f) }
  files <- list(
    a_240 = purrr::map_chr(2018:2020, function(y) w(fine, round(runif(100), 2), paste0('a_240_', y, '.tif'))),
    b_240 = w(fine, c(rep(NA, 30), round(runif(70), 2)), 'b_240.tif'),
    a_1200 = purrr::map_chr(2018:2020, function(y) w(coarse, c(0.1, 0.2, 0.3, y / 10000), paste0('a_1200_', y, '.tif'))),
    b_1200 = w(coarse, c(NA, 0.5, 0.5, 0.5), 'b_1200.tif'),
    fire_240 = purrr::map_chr(2018:2020, function(y) w(fine, c(rep(NA, 40), rep(0, 10), rep(y - 3, 40), rep(2025, 10)), paste0('fire_240_', y, '.tif'))),
    dist_x_240 = w(fine, seq(0, 39600, by = 400), 'dist_x_240.tif')
  )
  idx <- data.table(name = names(files),
                    first_year = c(2018L, NA, 2018L, NA, 2018L, NA), last_year = c(2020L, NA, 2020L, NA, 2020L, NA),
                    time_since = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE), ts_nodata = c(NA, NA, NA, NA, 200, NA))
  idx[, files := unname(files)]
  idx[]
}

groups <- list(ab = list(sum = c('a', 'b')))
cap <- list(columns = '^dist_', beyond_km = 20, set_to_km = 21)
formula <- 'case_ ~ -1 + I(log(sl_)) + ab_240_end + ab_240_end:ab_1200_end + I(log(ts_fire_240_end + 1)) +
  I(log(dist_x_240_end + 1)) + (1 | indiv_step_id)'

test_that('the stack holds each covariate, named without _end, with capped distances after them', {
  dir <- withr::local_tempdir()
  idx <- toy_land(dir)
  spec <- make_model_land_spec(formula, groups, cap, idx)
  expect_equal(spec$name, c('ab_240', 'ab_1200', 'ts_fire_240', 'dist_x_240', 'dist_x_240_adj'))
  expect_equal(spec$cols[[1]], c('a_240', 'b_240'))
  expect_equal(spec$rule, c('sum', 'sum', NA, NA, NA))
  expect_equal(spec$ts, c(FALSE, FALSE, TRUE, FALSE, FALSE))
  expect_equal(spec$adj, c(FALSE, FALSE, FALSE, FALSE, TRUE))
  expect_equal(spec$resolution, c(240, 1200, 240, 240, 240))
  expect_error(make_model_land_spec('case_ ~ zz_240_end', groups, cap, idx), 'not in the layer table: zz_240')
  expect_false(any(make_model_land_spec(formula, groups, NULL, idx)$adj))
})

test_that('the stack has the same values as the model tables at the same points', {
  dir <- withr::local_tempdir()
  idx <- toy_land(dir)
  spec <- make_model_land_spec(formula, groups, cap, idx)
  for (yr in c(2017L, 2019L, 2022L)) {
    f <- make_model_land(yr, idx, spec, cap, out_dir = file.path(dir, 'out'))
    s <- terra::rast(f)
    expect_equal(names(s), spec$name)
    expect_equal(terra::res(s), c(240, 240))

    # every cell centre as a step end, extracted and combined as for the model tables
    xy <- terra::xyFromCell(s, seq_len(terra::ncell(s)))
    pts <- data.table(row = seq_len(nrow(xy)), year = yr, x1_ = xy[, 1], y1_ = xy[, 2], x2_ = xy[, 1], y2_ = xy[, 2])
    ex <- extract_year(pts, idx, where = 'end', crs = crs)
    stack <- terra::values(s, dataframe = TRUE)
    for (res in c(240, 1200)) {
      expect_equal(stack[[paste0('ab_', res)]],
                   combine_columns(ex, paste0(c('a_', 'b_'), res, '_end'), 'sum'), tolerance = 1e-6)
    }
    # as the model tables, but NA where they have ts_nodata (no fire, or one after the year)
    expect_equal(stack$ts_fire_240, ifelse(ex$ts_fire_240_end == 200, NA, ex$ts_fire_240_end))
    expect_equal(stack$dist_x_240, ex$dist_x_240_end)
    expect_equal(stack$dist_x_240_adj, ifelse(ex$dist_x_240_end > 20000, 21000, ex$dist_x_240_end))
  }
  # a later year uses the last raster, with its own year for time since fire
  s <- terra::rast(file.path(dir, 'out', 'model_land_2022.tif'))
  ts <- terra::values(s$ts_fire_240)
  expect_true(all(is.na(ts[1:40])))     # NA = no fire
  expect_true(all(is.na(ts[41:50])))    # 0 = no fire
  expect_true(all(ts[51:90] == 5))      # burned 2017
  expect_true(all(is.na(ts[91:100])))   # burned after the year
})

test_that('the stack is masked to the template of its cell size', {
  dir <- withr::local_tempdir()
  idx <- toy_land(dir)
  spec <- make_model_land_spec(formula, groups, cap, idx)
  tmpl <- terra::rast(terra::rast(idx$files[[2]]), vals = c(rep(1, 50), rep(NA, 50)))
  terra::writeRaster(tmpl, file.path(dir, 'template_240m.tif'))
  f <- make_model_land(2019, idx, spec, cap, templates = file.path(dir, 'template_240m.tif'), out_dir = dir)
  v <- terra::values(terra::rast(f))
  expect_true(all(is.na(v[51:100, ])))
  expect_false(anyNA(v[1:50, 'dist_x_240']))
})

test_that('stack upload years, record and zip', {
  ml <- list(years = list(2015, 2020, 2025), upload_years = list(2015, 2025))
  expect_equal(model_land_upload_years(ml), c(2015L, 2025L))
  expect_equal(model_land_upload_years(ml, 2020), 2020L)
  expect_equal(model_land_upload_years(list(years = list(2015, 2020))), c(2015L, 2020L))
  expect_error(model_land_upload_years(ml, 2010), 'Not in model_land: years: 2010')

  meta <- data.frame(name = c('model_land', 'model_land_ab12', 'model_land_cd34', 'other'),
                     data = c('h0', 'h1', 'h2', 'h3'), parent = c(NA, 'model_land', 'model_land', NA))
  meta$path <- list(c('a', 'b'), '/x/model_land_2015.tif', '/x/model_land_2020.tif', '/x/y.tif')
  st <- model_land_branches(meta)
  expect_equal(st$year, c(2015L, 2020L))
  expect_equal(st$hash, c('h1', 'h2'))

  dir <- withr::local_tempdir()
  writeLines('x', file.path(dir, 'model_land_2015.tif')); writeLines('x', file.path(dir, 'model_land_layers.csv'))
  z <- write_model_land_zip(file.path(dir, 'model_land_2015.tif'), file.path(dir, 'model_land_layers.csv'), 2015, 'TEST', file.path(dir, 'up'))
  expect_equal(basename(z), 'TEST_model_land_2015.zip')
  expect_setequal(utils::unzip(z, list = TRUE)$Name, c('model_land_2015.tif', 'model_land_layers.csv'))
  expect_equal(nrow(read_model_land_record(file.path(dir, 'none.csv'))), 0)
})
