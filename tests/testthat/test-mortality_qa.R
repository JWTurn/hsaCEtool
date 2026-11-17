# Made-up animals, 6 h fixes, positions in metres; recorded death on 2021-06-10 (local date).

make_animals <- function() {
  t <- seq(
    as.POSIXct('2021-06-01 02:00', tz = 'UTC'),
    as.POSIXct('2021-06-20 20:00', tz = 'UTC'),
    by = '6 hours'
  )
  death <- as.POSIXct('2021-06-11 08:00', tz = 'UTC') # midnight after 2021-06-10 at UTC-8
  one <- function(id, keep, x) {
    data.table::data.table(source = 'S', id = id, t_ = t[keep], x_ = x[keep], y_ = 0)
  }
  n <- length(t)
  walk <- seq(0, by = 1000, length.out = n)
  still <- pmin(walk, (sum(t < death) - 1) * 1000)
  data.table::rbindlist(list(
    one('a', t < death - 3 * 86400, walk), # ends 3 days before
    one('b', t < death, walk), # ends on the date
    one('c', rep(TRUE, n), walk), # keeps walking
    one('d', t < death + 0.3 * 86400, walk / 10), # 1 fix after, 100 m steps
    one('e', rep(TRUE, n), still + rep(c(0, 5), length.out = n)), # lies still after
    one('f', rep(TRUE, n), walk) # no record
  ))
}

make_records <- function() {
  d <- as.POSIXct('2021-06-10', tz = 'UTC')
  data.table::data.table(
    source = 'S',
    id = c('a', 'b', 'b', 'c', 'd', 'e', 'f', 'h'),
    mortality_date = c(d, d, d + 86400 * 30, d, d, d, d, d),
    mortality_cause = 'x',
    mortality_outcome = c(
      'Mortality',
      'Mortality',
      'Mortality',
      'Mortality',
      'Mortality',
      'Mortality',
      'Censored',
      'Mortality'
    )
  )
}

test_that('mortality_records qa flags records and holds back moves_after', {
  DT <- make_animals()
  params <- list(
    table = make_records(),
    after_days = 1,
    tz = 'Etc/GMT+8',
    dead = 'Mortality',
    qa = list()
  )
  out <- suppressWarnings(clean_mortality_records(data.table::copy(DT), params, 'EPSG:3005'))
  dec <- attr(out, 'decisions')
  fl <- dec[!is.na(flag), stats::setNames(flag, paste(id, action))]
  expect_equal(unname(fl['a mortality record applied']), 'ends_before')
  expect_equal(unname(fl['b mortality record applied']), 'ends_on_date')
  expect_equal(unname(fl['c mortality record not applied']), 'moves_after')
  expect_equal(unname(fl['d mortality record applied']), 'few_fixes_after')
  expect_equal(unname(fl['e mortality record applied']), 'many_fixes_no_move')
  expect_equal(unname(fl['f mortality record not applied']), 'not_a_death')
  expect_equal(unname(fl['h mortality record not matched']), 'not_matched')
  expect_equal(unname(fl['b more than one mortality record']), 'duplicate_records')
  ## moves_after: fixes kept, not marked dead
  expect_equal(nrow(out[id == 'c']), nrow(DT[id == 'c']))
  expect_true(all(is.na(out[id == 'c', mortality_date])))
  ## e: fixes more than a day after the start of the date (local midnight) are dropped
  cut <- as.POSIXct('2021-06-10 08:00', tz = 'UTC') + 86400
  expect_equal(max(out[id == 'e', t_]), max(DT[id == 'e' & t_ <= cut, t_]))
  expect_equal(
    dec[id == 'e' & action == 'mortality record applied', fixes],
    DT[id == 'e' & t_ > cut, .N]
  )
  expect_equal(dec[id == 'c', max_step_m], 1000)
  expect_equal(dec[id == 'a', days_last_fix_minus_date], -3L)
  ## the first listed record is used for b
  expect_equal(unique(out[id == 'b', mortality_date]), as.POSIXct('2021-06-10', tz = 'UTC'))
})

test_that('mortality_records without the new settings works as before', {
  DT <- make_animals()
  params <- list(table = make_records()[, !'mortality_outcome'], after_days = 1)
  out <- suppressWarnings(clean_mortality_records(data.table::copy(DT), params, 'EPSG:3005'))
  ## all matched records applied (c and f too), death at UTC midnight
  expect_true(all(!is.na(out[id %in% c('c', 'f'), mortality_date])))
  cut <- as.POSIXct('2021-06-11', tz = 'UTC')
  expect_equal(max(out[id == 'c', t_]), max(DT[id == 'c' & t_ <= cut, t_]))
  dec <- attr(out, 'decisions')
  expect_true(all(is.na(dec[action == 'mortality record applied', flag])))
})

test_that('read_mortality_records reads a named sheet and the outcome', {
  f <- readxl::readxl_example('datasets.xlsx')
  expect_true('mag' %in% names(read_table_file(f, 'quakes')))
  expect_true('mpg' %in% names(read_table_file(f)))
  csv <- tempfile(fileext = '.csv')
  data.table::fwrite(
    data.table::data.table(
      ID = c('A 1', 'B2'),
      Date = c('44357', 'NA'),
      Outcome = c('Mortality', 'Censored')
    ),
    csv
  )
  r <- read_mortality_records(csv, list(id = 'ID', date = 'Date', outcome = 'Outcome'), 'S')
  expect_equal(r$id, c('A1', 'B2'))
  expect_equal(r$mortality_outcome, c('Mortality', 'Censored'))
  expect_equal(format(r$mortality_date[1]), '2021-06-10')
  expect_true(is.na(r$mortality_date[2]))
})

test_that('moves_after ignores the step into the first fix after the date', {
  t <- as.POSIXct('2021-06-10 08:00', tz = 'UTC') + 3600 * c(-30, -12, 6, 18, 30, 42)
  DT <- data.table::data.table(
    source = 'S',
    id = 'g',
    t_ = t,
    x_ = c(0, 0, 3000, 3005, 3000, 3010),
    y_ = 0
  )
  rec <- data.table::data.table(
    source = 'S',
    id = 'g',
    mortality_date = as.POSIXct('2021-06-09', tz = 'UTC')
  )
  fl <- mortality_record_flags(DT, rec, 'Etc/GMT+8', list())
  expect_equal(fl$flag, 'many_fixes_no_move')
  expect_equal(fl$step_into_m, 3000)
  expect_equal(fl$max_step_m, 10)
})

test_that('records for herds not kept are dropped and counted', {
  rec <- data.table::data.table(
    source = 'S',
    id = c('a', 'b', 'c'),
    herd = c('Wells Gray North', 'Other', NA)
  )
  out <- keep_record_herds(rec, list(keep = list('WellsGrayNorth')))
  expect_equal(out$id, c('a', 'c'))
  expect_equal(attr(out, 'other_herds'), 1L)
  expect_identical(keep_record_herds(rec, list(keep = list())), rec)
  cl <- add_record_tables(
    list(params = list()),
    mortality = list(rec),
    herd_cfg = list(keep = list('WellsGrayNorth'))
  )
  expect_equal(cl$params$mortality_records$table$id, c('a', 'c'))
  DT <- make_animals()
  tab <- keep_record_herds(
    make_records()[, herd := 'Wells Gray North'][id == 'a', herd := 'Other'],
    list(keep = list('WellsGrayNorth'))
  )
  out <- suppressWarnings(clean_mortality_records(
    data.table::copy(DT),
    list(table = tab),
    'EPSG:3005'
  ))
  dec <- attr(out, 'decisions')
  expect_equal(
    dec[flag == 'other_herds', detail],
    '1 records for herds not in locations: herd: keep'
  )
  expect_false('a' %in% dec[action == 'mortality record applied', id])
})
