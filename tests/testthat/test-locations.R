crs <- 'EPSG:3154'

test_that('standardize_locations renames, projects and types columns', {
  DT <- data.table(animal = c(1, 1), time = c('2020-01-01 00:00:00', '2020-01-01 11:00:00'),
                   lon = c(-135, -135.1), lat = c(61, 61.1), herd = 'A')
  src <- list(columns = list(id = 'animal', datetime = 'time', x = 'lon', y = 'lat'),
              input_crs = 'EPSG:4326', name = 'test')
  out <- standardize_locations(DT, src, list(tz = 'UTC', keep_columns = list()), crs)
  expect_true(all(c('id', 't_', 'x_', 'y_', 'herd', 'source') %in% names(out)))
  expect_type(out$id, 'character')
  expect_s3_class(out$t_, 'POSIXct')
  expect_gt(min(out$y_), 1e6)    # projected to metres
})

test_that('standardize_locations turns a date and a local hour into UTC', {
  crs <- 'EPSG:3154'
  DT <- data.table::data.table(animal = 'a', lon = -130, lat = 62,
                               day = as.Date(c('2016-07-01', '2016-01-15', '2021-01-15', '2016-03-13')),
                               hr = c('10', '10', '10', '2'))
  src <- list(columns = list(id = 'animal', datetime = 'day', hour = 'hr', x = 'lon', y = 'lat'),
              input_crs = 'EPSG:4326', tz = 'America/Whitehorse')
  out <- standardize_locations(DT, src, list(tz = 'UTC', keep_columns = list('animal')), crs)
  expect_equal(format(out$t_, '%Y-%m-%d %H', tz = 'UTC'),
               c('2016-07-01 17', '2016-01-15 18', '2021-01-15 17', '2016-03-13 09'))
  # in UTC, with no time zone given
  src$tz <- NULL
  out <- standardize_locations(DT, src, list(keep_columns = list()), crs)
  expect_equal(format(out$t_[1], '%H', tz = 'UTC'), '10')
  expect_error(standardize_locations(DT[, !'hr'], src, list(keep_columns = list()), crs), 'hr')
})

test_that('standardize_locations says which columns are missing', {
  DT <- data.table(a = 1)
  expect_error(standardize_locations(DT, list(), list(columns = list(id = 'id', datetime = 't_', x = 'x_', y = 'y_')), crs),
               'Location columns not found')
})

test_that('cleaning steps remove what they should', {
  DT <- toy_locations()
  expect_equal(nrow(clean_dop(copy(DT), list(column = 'gps_dop', max_dop = 10, keep_missing = TRUE), crs)), 80)
  expect_equal(nrow(clean_dop(copy(DT), list(column = 'gps_dop', max_dop = 10, keep_missing = FALSE), crs)), 60)
  # first day (12 fixes at 2 h) of each animal dropped
  expect_equal(nrow(clean_capture_effect(copy(DT), list(days = 1), crs)), 76)
  expect_equal(nrow(clean_min_points(copy(DT)[!(id == 'b' & seq_len(.N) > 52)], list(n = 10), crs)), 50)
  # 100 m per 2 h = 50 m/h
  expect_equal(nrow(clean_speed(copy(DT), list(max_m_per_hr = 40), crs)), 2)
  expect_equal(nrow(clean_speed(copy(DT), list(max_m_per_hr = 60), crs)), 100)
})

test_that('exclude_points drops fixes near a location', {
  DT <- toy_locations()
  ll <- sf::sf_project(sf::st_crs(crs), sf::st_crs('EPSG:4326'), matrix(c(500100, 6700000), ncol = 2))
  out <- clean_exclude_points(copy(DT), list(list(lon = ll[1], lat = ll[2], radius_m = 250)), crs)
  expect_equal(nrow(out), 97)
})

test_that('clean_locations runs steps in order, logs them, and accepts custom steps', {
  DT <- rbind(toy_locations(), toy_locations()[1])   # one exact duplicate
  cleaning <- list(steps = list('dop', 'mine'),
                   params = list(dop = list(column = 'gps_dop', max_dop = 10, keep_missing = TRUE)),
                   custom = list(mine = function(DT, params, crs) DT[id == 'a']))
  out <- clean_locations(DT, cleaning, crs)
  log <- attr(out, 'cleaning_log')
  expect_equal(log$step, c('same_time_duplicates', 'dop', 'mine'))
  expect_equal(log$removed, c(1, 20, 40))
  expect_equal(unique(out$id), 'a')
})

test_that('study area, cropping and subsetting', {
  DT <- toy_locations()
  sa <- make_study_area(DT, list(buffer_m = 1000, convex_hull = FALSE), crs = crs)
  expect_s4_class(sa, 'SpatVector')
  expect_equal(nrow(crop_locations(DT, sa, 'bbox')), 100)
  expect_equal(nrow(crop_locations(DT, sa, 'within')), 100)
  far <- rbind(DT, data.table(id = 'c', t_ = DT$t_[1], x_ = 0, y_ = 0, gps_dop = 1))
  expect_equal(nrow(crop_locations(far, sa, 'within')), 100)
  expect_equal(unique(subset_locations(DT, list(n_ids = 1))$id), 'a')
  expect_equal(unique(subset_locations(DT, list(ids = list('b')))$id), 'b')
  expect_equal(nrow(subset_locations(DT, list(years = list(2019)))), 0)
})

test_that('fixes another source also has are flagged, then dropped by the cleaning step', {
  a <- toy_locations(5)[id == 'a'][, source := 'movebank']
  b <- copy(a)[, `:=`(id = 'a_bc', source = 'bc')]
  b[, `:=`(x_ = x_ + c(0.5, 0.5, 30, 0.5, 0.5), t_ = t_ + c(0, 20, 0, 3600, 0))]
  out <- flag_source_duplicates(rbind(a, b), list(max_m = 2, max_s = 60), 'EPSG:3154')
  expect_true(all(is.na(out[source == 'movebank']$dup_source)))
  # moved 30 m (3rd) or an hour (4th): not the same fix
  expect_equal(out[source == 'bc']$dup_source, c('movebank', 'movebank', NA, NA, 'movebank'))
  expect_equal(nrow(clean_source_duplicates(out, NULL, 'EPSG:3154')), 7)
  expect_true(all(is.na(flag_source_duplicates(a, list(), 'EPSG:3154')$dup_source)))
})
