# Made-up collars on a 4 h schedule; clock times written in different ways.

## clock digits (stored as UTC) of true times `utc` read in time zone `tz`
as_clock <- function(utc, tz) as.POSIXct(format(utc, '%Y-%m-%d %H:%M:%S', tz = tz), tz = 'UTC')
every4h <- function(from, to) {
  seq(as.POSIXct(from, tz = 'UTC'), as.POSIXct(to, tz = 'UTC'), by = '4 hours')
}

make_collars <- function() {
  t_d <- every4h('2021-01-01', '2021-12-31') # clock follows daylight saving
  t_s <- every4h('2021-01-01', '2021-12-31') + 3600 # standard time all year
  t_c <- c(
    every4h('2021-01-01', '2021-07-01'), # standard, then a new collar
    every4h('2021-07-11', '2022-12-31')
  ) #   that follows daylight saving
  t_n <- every4h('2021-06-01', '2021-06-20') # no change spanned
  ## a collar fixing at 00, 04, ... local daylight time, writing standard time
  days <- seq(as.Date('2021-01-01'), as.Date('2021-12-31'), by = 'day')
  loc <- as.POSIXct(
    paste(rep(days, each = 6), sprintf('%02d:00', seq(0, 20, 4))),
    tz = 'America/Vancouver'
  )
  t_l <- as.POSIXct(format(loc, tz = 'UTC'), tz = 'UTC')
  clk_c <- c(
    as_clock(t_c[t_c < as.POSIXct('2021-07-05', tz = 'UTC')], 'Etc/GMT+8'),
    as_clock(t_c[t_c > as.POSIXct('2021-07-05', tz = 'UTC')], 'America/Vancouver')
  )
  data.table::data.table(
    id = rep(
      c('d', 's', 'c', 'n', 'l', 'u'),
      c(length(t_d), length(t_s), length(t_c), length(t_n), length(t_l), 3)
    ),
    true = c(t_d, t_s, t_c, t_n, t_l, t_n[1:3]),
    t_ = c(
      as_clock(t_d, 'America/Vancouver'),
      as_clock(t_s, 'Etc/GMT+8'),
      clk_c,
      as_clock(t_n, 'Etc/GMT+8'),
      as_clock(t_l, 'Etc/GMT+8'),
      t_n[1:3]
    ),
    x_ = 0,
    y_ = 0,
    src = rep(
      c('L', 'L', 'L', 'L', 'L', 'U'),
      c(length(t_d), length(t_s), length(t_c), length(t_n), length(t_l), 3)
    )
  )
}

dst_params <- function(...) {
  list(
    column = 'src',
    tz = list(L = 'Etc/GMT+8'),
    dst = utils::modifyList(list(zone = 'America/Vancouver'), list(...))
  )
}

test_that('dst_switches follows the time zone database, incl. the 2007 rule change', {
  sw <- dst_switches(
    'America/Vancouver',
    as.POSIXct('2006-01-01', tz = 'UTC'),
    as.POSIXct('2007-12-31', tz = 'UTC')
  )
  expect_equal(
    format(sw$when, '%Y-%m-%d'),
    c('2006-04-02', '2006-10-29', '2007-03-11', '2007-11-04')
  )
  expect_equal(sw$spring, c(TRUE, FALSE, TRUE, FALSE))
  expect_equal(std_offset('America/Vancouver'), -8 * 3600)
  expect_equal(std_offset('Etc/GMT+8'), -8 * 3600)
})

test_that('local_times finds clocks that follow daylight saving, per animal and period', {
  DT <- make_collars()
  cleaning <- list(steps = list('local_times'), params = list(local_times = dst_params()))
  out <- suppressMessages(clean_locations(DT[, !'true'], cleaning, 'EPSG:3005'))
  dec <- attr(out, 'decisions')
  expect_setequal(dec$method[dec$id == 'd'], 'detected')
  expect_true(all(dec$dst[dec$id == 'd']))
  expect_false(any(dec$dst[dec$id %in% c('s', 'l')]))
  expect_equal(dec[id == 'l', local_schedule], 2L)
  ## the new collar's period starts after the gap
  cc <- dec[id == 'c']
  expect_equal(cc$dst, c(FALSE, TRUE))
  expect_equal(format(cc$from[2], '%Y-%m-%d'), '2021-07-10')
  expect_equal(dec[id == 'n', method], 'borrowed')
  expect_false('u' %in% dec$id)
  ## times back to the truth; the one clock time that occurs twice is counted
  for (a in c('d', 's', 'c', 'l')) {
    expect_true(all(out[id == a, t_] %in% DT[id == a, true]), label = a)
  }
  expect_equal(dec[id == 'd', sum(repeated_hour)], 1L)
  expect_true(all(out[id == 'u', t_] %in% DT[id == 'u', t_]))
})

test_that('local_times dst: overrides, values and checks', {
  DT <- make_collars()
  p <- dst_params(
    overrides = list(list(id = 'd', from = '2020-12-01', to = '2021-12-31', dst = FALSE))
  )
  out <- suppressMessages(clean_locations(
    DT[, !'true'],
    list(steps = list('local_times'), params = list(local_times = p)),
    'EPSG:3005'
  ))
  dec <- attr(out, 'decisions')
  expect_equal(dec[id == 'd', method], 'override')
  expect_false(dec[id == 'd', dst])

  p <- dst_params(values = list('Z'))
  expect_error(clean_local_times(DT[, !'true'], p, 'EPSG:3005'), 'not in tz')
  p <- list(column = 'src', tz = list(L = 'Etc/GMT+7'), dst = list(zone = 'America/Vancouver'))
  expect_error(clean_local_times(DT[, !'true'], p, 'EPSG:3005'), 'standard time')
  p <- list(column = 'src', tz = list(L = 'Etc/GMT+8'), dst = list())
  expect_error(clean_local_times(DT[, !'true'], p, 'EPSG:3005'), 'zone')
})
