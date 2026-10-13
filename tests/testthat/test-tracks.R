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
