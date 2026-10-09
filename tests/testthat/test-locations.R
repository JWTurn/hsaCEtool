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
  expect_equal(log$step, c('exact_duplicates', 'dop', 'mine'))
  expect_equal(log$removed, c(1, 20, 40))
  expect_equal(unique(out$id), 'a')
  # the removed attribute agrees with the log
  removed <- attr(out, 'removed')
  expect_equal(removed[, .N, by = step]$N, c(1, 20, 40))
  expect_false('.row' %in% names(out))
})

test_that('fixes at the same time but different positions are resolved by DOP', {
  DT <- toy_locations(5)[id == 'a']
  clash <- copy(DT[1])[, `:=`(x_ = x_ + 50, gps_dop = 8)]   # same time as row 1 (DOP 2)
  DT <- rbind(DT, clash)
  cleaning <- list(steps = list('duplicates'),
                   params = list(duplicates = list(gamma_minutes = 15, dop_column = 'gps_dop')))
  out <- clean_locations(DT, cleaning, crs)
  expect_equal(nrow(out), 5)
  expect_equal(out$x_[1], DT$x_[1])   # lower DOP kept
  expect_equal(attr(out, 'removed'), data.table(row = 6L, step = 'duplicates'))
  expect_equal(attr(out, 'cleaning_log')$step, c('exact_duplicates', 'duplicates'))

  # without the duplicates step, the first is kept, with a warning
  expect_warning(out <- clean_locations(DT, list(steps = list()), crs), 'share their animal and time')
  expect_false(anyDuplicated(out, by = c('id', 't_')) > 0)
  expect_equal(attr(out, 'cleaning_log')$step, c('exact_duplicates', 'same_time_duplicates'))
  expect_equal(attr(out, 'removed')$step, 'same_time_duplicates')
})

test_that('check_mortality counts flagged fixes kept or removed by step', {
  raw <- toy_locations(5)[, `:=`(source = 's', mortality = c(TRUE, TRUE, FALSE, NA, FALSE,
                                                             FALSE, FALSE, FALSE, FALSE, TRUE))]
  cleaning <- list(steps = list('drop_first'),
                   custom = list(drop_first = function(DT, params, crs) DT[!(id == 'a' & t_ == min(t_))]))
  out <- suppressMessages(clean_locations(raw, cleaning, crs))
  expect_equal(check_mortality(raw, out),
               data.table(source = 's', id = c('a', 'a', 'b'), step = c('drop_first', 'kept', 'kept'), n = 1L))
  expect_equal(nrow(check_mortality(raw[, !'mortality'], out)), 0)
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

test_that('date-only fixes get noon UTC, light cleaning, and are dropped before tracks', {
  DT <- data.table::data.table(animal = 'd', day = as.Date(c('2005-06-01', '2005-06-01', '2005-06-02')),
                               lon = c(-130, -130.1, -150), lat = 62)
  src <- list(columns = list(id = 'animal', datetime = 'day', x = 'lon', y = 'lat'),
              input_crs = 'EPSG:4326', name = 'old', date_only = TRUE)
  dated <- standardize_locations(DT, src, list(keep_columns = list()), crs)
  expect_equal(format(dated$t_, '%Y-%m-%d %H:%M', tz = 'UTC'),
               c('2005-06-01 12:00', '2005-06-01 12:00', '2005-06-02 12:00'))
  expect_true(all(dated$date_only))

  raw <- data.table::rbindlist(list(toy_locations(5), dated), fill = TRUE)
  cleaning <- list(steps = list('mine'), date_only_steps = list('bounds'),
                   params = list(bounds = list(xmin = -142, ymin = 50, xmax = -120, ymax = 70)),
                   custom = list(mine = function(DT, params, crs) DT[id == 'a']))
  # two date-only fixes on one day are both kept, with no same-time warning
  expect_no_warning(out <- suppressMessages(clean_locations(raw, cleaning, crs)))
  expect_equal(nrow(out[(date_only)]), 2)   # the one outside bounds removed
  expect_equal(attr(out, 'cleaning_log')$step, c('exact_duplicates', 'mine', 'bounds (date only)'))
  expect_equal(attr(out, 'cleaning_log')$removed, c(0, 5, 1))
  timed <- drop_date_only(out)
  expect_equal(nrow(timed), 5)
  expect_false('date_only' %in% names(timed))
})

test_that('combine_locations makes units and factor columns plain', {
  a <- toy_locations(5)[id == 'a'][, `:=`(source = 'movebank', sex = factor('f'))]
  a[, gps_dop := units::set_units(gps_dop, 1)]
  b <- toy_locations(5)[id == 'b'][, `:=`(source = 'bc', sex = 'm')]
  out <- combine_locations(list(a, b), list(), crs)
  expect_type(out$gps_dop, 'double')
  expect_false(inherits(out$gps_dop, 'units'))
  expect_type(out$sex, 'character')
  expect_equal(nrow(out), 10)
  expect_true(inherits(a$gps_dop, 'units'))   # inputs left as they were
})

test_that('flag_duplicates_fast gives the same result as amt::flag_duplicates', {
  set.seed(5)
  n <- 300
  # made-up track: irregular gaps from 1 to 40 minutes, DOPs with many ties
  t <- as.POSIXct('2021-01-01', tz = 'UTC') + cumsum(sample(c(0, 60, 300, 600, 1200, 2400), n, replace = TRUE))
  DT <- data.table(t_ = t, x_ = 500000 + cumsum(rnorm(n, 0, 50)), y_ = 6700000 + cumsum(rnorm(n, 0, 50)),
                   dop = sample(c(1, 2, 2, 3, 99), n, replace = TRUE))
  amt_flag <- amt::flag_duplicates(amt::make_track(DT, x_, y_, t_, all_cols = TRUE),
                                   gamma = lubridate::minutes(15), DOP = 'dop')$duplicate_
  expect_equal(flag_duplicates_fast(DT$t_, DT$x_, DT$y_, DT$dop, 15 * 60), amt_flag)
  expect_gt(sum(amt_flag), 0)
})

test_that('end_clusters flags a stationary end, incl. a collar moved to an office, but not a live animal', {
  set.seed(3)
  t0 <- as.POSIXct('2021-06-01', tz = 'UTC')
  jitter <- function(n) rnorm(n, 0, 10)
  # made-up tracks, one fix an hour
  live <- data.table(id = 'live', x_ = 500000 + 50 * (1:100), y_ = 6700000)            # slow, steady walker
  moved <- data.table(id = 'moved',
                      x_ = c(500000 + 1000 * (1:50), 560000 + jitter(60), 580000, 600000, 620000 + jitter(60)),
                      y_ = 6700000 + c(rep(0, 50), jitter(60), 0, 0, jitter(60)))   # walks, dies, collar taken to an office
  short <- data.table(id = 'short', x_ = c(500000 + 1000 * (1:50), 560000 + jitter(20)), y_ = 6700000)
  # stationary for 60 h, then picked up: 3 fixes on the move before the data end
  tail <- data.table(id = 'tail', x_ = c(500000 + 1000 * (1:50), 560000 + jitter(60), 570000, 580000, 590000),
                     y_ = 6700000)
  DT <- rbindlist(list(live, moved, short, tail))[, t_ := t0 + (seq_len(.N) - 1) * 3600, by = id]
  params <- list(zeta = 200, theta_hours = 48, max_moves = 2, transit_fixes = 5)
  out <- clean_end_clusters(copy(DT), params, crs)
  expect_equal(nrow(out), nrow(DT))   # flags only
  expect_false(any(out[id == 'live']$end_cluster_))
  expect_false(any(out[id == 'short']$end_cluster_))   # stationary for under 48 h
  expect_equal(which(out[id == 'moved']$end_cluster_), 51:172)   # from the mortality site on
  expect_equal(which(out[id == 'tail']$end_cluster_), 51:113)
  expect_true(all(out$cluster_status[out$end_cluster_] == 'unknown-cluster'))   # no mortality column
  expect_equal(out[(end_of_track_)]$id, c('moved', 'tail'))
  expect_equal(which(out[id == 'moved']$end_of_track_), 51L)   # where it stopped
  # with no moves allowed, only the office stretch
  out1 <- clean_end_clusters(copy(DT), modifyList(params, list(max_moves = 0)), crs)
  expect_equal(which(out1[id == 'moved']$end_cluster_), 113:172)

  # amt's rule only flags too, and flags the live walker (all its steps are short)
  amt_out <- clean_clusters(copy(DT), list(zeta = 200, eta = 3, theta_hours = 48), crs)
  expect_equal(nrow(amt_out), nrow(DT))
  expect_true(any(amt_out[id == 'live']$defunct_cluster_))

  amt_out[, `:=`(source = 's', mortality = id == 'moved')]
  st <- clean_end_clusters(copy(amt_out), params, crs)
  expect_equal(sum(st$cluster_status %in% 'mortality-cluster'), 122)   # moved: flagged and in an end cluster
  expect_equal(sum(st$cluster_status %in% 'flag-only'), 50)            # moved: flagged before it died
  expect_equal(sum(st$cluster_status %in% 'unknown-cluster'), 63)      # tail: end cluster, no flag
  status <- unique(st[, .(id, mortality_status, mortality_basis)])
  expect_equal(nrow(status), 4)   # one per animal
  expect_equal(status[id == 'moved']$mortality_status, 'dead')
  expect_equal(status[id == 'tail']$mortality_status, 'unknown')
  expect_match(status[id == 'tail']$mortality_basis, '^inferred')
  expect_equal(status[id %in% c('live', 'short')]$mortality_status, c('alive', 'alive'))
  s <- summarize_flags(st)
  expect_equal(s[flag == 'mortality and amt clusters']$animals, 1)
  expect_equal(unique(s$of_fixes), nrow(DT))
})

test_that('mortality_status puts recorded outcomes first, and drop_end_clusters keeps where the animal stopped', {
  set.seed(4)
  t0 <- as.POSIXct('2021-06-01', tz = 'UTC')
  # made-up tracks, one fix an hour: 10 h walking then 60 h still, or 70 h walking
  still <- function(id) data.table(id = id, x_ = c(500000 + 1000 * (1:10), 520000 + rnorm(60, 0, 10)), y_ = 6700000)
  walk <- function(id) data.table(id = id, x_ = 500000 + 1000 * (1:70), y_ = 6700000)
  DT <- rbindlist(list(still('a'), still('b'), walk('c'), walk('d'), still('e')))
  DT[, `:=`(t_ = t0 + (rowid(id) - 1) * 3600, source = 's',
            deployment_end_type = c(a = 'dead', b = 'equipment-failure', c = 'unknown', d = NA, e = NA)[id],
            mortality = id == 'e' & rowid(id) > 30)]
  params <- list(zeta = 200, theta_hours = 48, max_moves = 2, transit_fixes = 5)
  status <- unique(clean_end_clusters(copy(DT), params, crs)[, .(id, mortality_status, mortality_basis)])
  expect_equal(status$mortality_status, c('dead', 'equipment-failure', 'unknown', 'alive', 'dead'))
  expect_equal(status$mortality_basis, mortality_status_rules()$mortality_basis[c(1, 3, 3, 5, 2)])

  cl <- clean_locations(DT, list(steps = c('end_clusters', 'drop_end_clusters'),
                                 params = list(end_clusters = params)), crs)
  expect_equal(cl[, .N, by = id]$N, c(11, 11, 70, 70, 11))
  expect_true(all(cl[id %in% c('a', 'b', 'e'), end_of_track_[.N], by = id]$V1))   # last fix is where it stopped
  expect_equal(attr(cl, 'cleaning_log')[step == 'drop_end_clusters']$removed, 3 * 59)
  s <- summarize_flags(cl, DT)
  expect_equal(s[flag == 'end clusters removed']$fixes, 3 * 59)
  expect_equal(s[flag == 'mortality in end clusters removed']$fixes, 40)
  expect_equal(sum(grepl('^mortality_status', s$flag)), 5)
  expect_error(clean_drop_end_clusters(copy(DT), NULL, crs), 'end_clusters')
})

test_that('the extended study area is one continuous polygon around the study area', {
  # made-up locations in two groups 100 km apart, so the study area has two parts
  DT <- data.table(x_ = c(500000, 501000, 600000, 601000), y_ = 6700000)
  sa <- make_study_area(DT, list(buffer_m = 1000, convex_hull = FALSE), crs = crs)
  expect_equal(nrow(terra::disagg(sa)), 2)
  ext <- make_study_area_extended(sa, list(buffer_m = 5000, convex_hull = TRUE))
  expect_equal(nrow(terra::disagg(ext)), 1)
  expect_true(terra::relate(ext, sa, 'covers')[1, 1])
  expect_equal(terra::expanse(ext), terra::expanse(terra::buffer(terra::hull(sa), 5000)), tolerance = 0.01)
  # without a hull, the buffer keeps the parts apart
  expect_equal(nrow(terra::disagg(make_study_area_extended(sa, list(buffer_m = 5000, convex_hull = FALSE)))), 2)
})
