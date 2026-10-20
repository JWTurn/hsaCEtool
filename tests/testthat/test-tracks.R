test_that('tracks, resampling and random steps work on a toy track', {
  skip_if_not_installed('amt')
  set.seed(1)
  n <- 200
  DT <- data.table(id = 'a',
                   t_ = as.POSIXct('2020-03-01', tz = 'UTC') + (seq_len(n) - 1) * 3600 * 11,
                   x_ = 500000 + cumsum(rnorm(n, 0, 500)),
                   y_ = 6700000 + cumsum(rnorm(n, 0, 500)))
  trk <- make_tracks(DT, 'EPSG:3154')
  s <- resample_tracks(trk, lubridate::hours(11), lubridate::minutes(120), probsfilter = 0.99)
  expect_gt(nrow(s), 150)
  expect_lte(max(s$sl_), quantile(s$sl_, 1))

  expect_null(resample_tracks(trk[1:10, ], lubridate::hours(11), lubridate::minutes(120), 0.99))

  rs <- make_random_steps(s, n_rand = 5,
                          amt::fit_distr(s$sl_, 'gamma'), amt::fit_distr(s$ta_, 'vonmises'))
  expect_equal(sum(!rs$case_), 5 * sum(rs$case_))

  steps <- prep_steps(rs, list(year_interval = 5, min_year = 2012, max_year = 2025))
  expect_true(all(c('year', 'int.year', 'indiv_step_id') %in% names(steps)))
  expect_equal(unique(steps$int.year), 2020L)
  expect_equal(calc_step_buffer(steps, 50) %% 50, 0)
})

test_that('filter_years and fit_step_distr', {
  DT <- toy_locations()
  expect_equal(nrow(filter_years(DT, 2020, 2020)), 100)
  expect_equal(nrow(filter_years(DT, 2012, 2019)), 0)
  expect_error(fit_step_distr(NULL, 'sl_', 'gamma'), 'No tracks are left')
})

test_that('make_random_steps skips an animal whose bursts all have fewer than 3 steps', {
  set.seed(5)
  t0 <- as.POSIXct('2020-03-01', tz = 'UTC')
  # made-up track: fixes in pairs 2 days apart, so every burst has 1 step
  DT <- data.table(id = 'a', t_ = t0 + rep(0:9 * 48, each = 2) * 3600 + rep(c(0, 3600), 10),
                   x_ = 500000 + cumsum(runif(20, 50, 500)), y_ = 6700000 + cumsum(runif(20, 50, 500)))
  trk <- amt::make_track(DT, x_, y_, t_, id = id, crs = 'EPSG:3154') |>
    amt::track_resample(rate = lubridate::hours(1), tolerance = lubridate::minutes(10))
  s <- amt::steps_by_burst(trk)
  expect_null(make_random_steps(s, n_rand = 2))
})

test_that('prep_steps drops tracks with fewer than min_steps used steps left', {
  t0 <- as.POSIXct('2020-03-01', tz = 'UTC')
  rs <- data.table(id = rep(c('a', 'b'), c(25 * 3, 10 * 3)), case_ = rep(c(TRUE, FALSE, FALSE), 35),
                   step_id_ = rep(c(1:25, 1:10), each = 3), t2_ = t0, sl_ = 1)
  out <- prep_steps(rs, list(year_interval = 5, min_steps = 20))
  expect_equal(unique(out$id), 'a')
  expect_equal(nrow(out[case_ == TRUE]), 25)
})

test_that('zero-length steps get the shortest step length and a direction, so no angle is missing', {
  t0 <- as.POSIXct('2020-03-01', tz = 'UTC')
  # fixes 3 and 4 are at the same place (a zero-length step), made-up positions
  xs <- c(0, 100, 250, 250, 400, 600, 650, 900)
  ys <- c(0, 50, 0, 0, 100, 50, 200, 100)
  trk <- amt::make_track(data.frame(x = 5e5 + xs, y = 6.7e6 + ys, t = t0 + (0:7) * 3600), x, y, t, crs = 3154)
  s <- amt::steps(trk)
  s$burst_ <- 1L
  expect_true(any(s$sl_ == 0))
  expect_gt(sum(is.na(s$ta_)), 1)   # amt leaves the zero step's angle and the next one's missing
  set.seed(1)
  out <- fix_zero_steps(s, 'random')
  expect_equal(out$sl_[s$sl_ == 0], min(s$sl_[s$sl_ > 0]))
  expect_equal(sum(is.na(out$ta_)), 1L)   # only the first step (no step before it)
  expect_true(all(abs(out$ta_[-1]) <= pi))
  expect_identical(out$x2_, s$x2_)        # positions unchanged
  z <- which(s$sl_ == 0)
  expect_equal(fix_zero_steps(s, 'zero')$ta_[z], 0)
  # the next step's angle follows from the direction given
  w <- function(a) (a + pi) %% (2 * pi) - pi
  expect_equal(out$ta_[z + 1], w(out$direction_p[z + 1] - out$direction_p[z]))
})

test_that('steps are kept only inside an area: whole steps by the used step, random steps by their end', {
  sq <- function(x0, y0, w) sf::st_polygon(list(rbind(c(x0, y0), c(x0 + w, y0), c(x0 + w, y0 + w), c(x0, y0 + w), c(x0, y0))))
  area <- sf::st_sfc(sq(0, 0, 1000), crs = 3154)   # made-up area; outside it is x > 1000
  DT <- data.table(id = 'a', step_id_ = rep(1:3, each = 3), case_ = rep(c(TRUE, FALSE, FALSE), 3),
                   x1_ = c(100, 100, 100, 500, 500, 500, 1500, 1500, 1500), y1_ = 500,
                   x2_ = c(200, 300, 1200,  1100, 600, 700,  900, 800, 700), y2_ = 500)
  # step 1: one random step ends outside; step 2: the used step ends outside; step 3: starts outside
  out <- keep_steps_within(DT, area)
  expect_equal(out$step_id_, c(1L, 1L))
  expect_equal(attr(out, 'outside_area')$n, c(2L, 1L))
  expect_equal(nrow(keep_steps_within(DT, NULL)), 9L)
  # the area from a polygon file, keeping some polygons by a field
  f <- file.path(withr::local_tempdir(), 'juris.gpkg')
  sf::st_write(sf::st_sf(name = c('In', 'Out'), geometry = sf::st_sfc(sq(0, 0, 1000), sq(1000, 0, 1000), crs = 3154)), f, quiet = TRUE)
  a <- make_steps_area(f, list(field = 'name', keep = list('In')), 'EPSG:3154')
  expect_equal(as.numeric(sf::st_area(a)), 1e6)
  expect_error(make_steps_area(f, list(field = 'name', keep = list('Elsewhere')), 'EPSG:3154'), 'not in name')
})
