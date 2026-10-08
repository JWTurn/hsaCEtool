# Combining the B.C. telemetry exports, with made-up fixes

# Made-up positions 0.01 degrees (about 600-1100 m) apart
bc_pos <- function(i) c(-125 + i / 100, 58 + i / 100)

# New export: animal A1 (WLH W1, Lotek collar 100) and B2 (Vectronic collar 200)
bc_new <- function(dir) {
  d <- data.frame(
    Pop_Unt = 'Herd', WLH_ID = c(rep('W1', 4), rep(NA, 3)), Anml_ID = c(rep('A1', 4), rep('B2', 3)),
    Cllr_ID = c(rep('100', 4), rep('200', 3)), Vendor = c(rep('lotek', 4), rep('vectronic', 3)),
    Sex = 'Female', DOP = 2, Elevatn = 1000, SECURED = 'Yes',
    # the clock text is UTC; offsets are ignored
    Date = c('2020-06-01 10:00:00.000 -0700',   # 1: also in old (shifted to 17:00)
             '2019-01-10 04:00:00',             # 2: no offset; also in old (shifted to 12:00)
             '2017-03-12 02:30:00',             # 3: no offset, in B.C.'s skipped hour: still UTC
             '2020-06-02 10:15:00.000 -0700',   # 4: old has this position at 10:00 UTC
             '2024-09-22 13:00:00.000 -0700',   # 5: 7 h late; reference has 06:00 UTC
             '2024-09-23 00:00:00.000 -0700',   # 6 and 7: two positions at one time
             '2024-09-23 00:00:00.000 -0700'))
  pts <- sf::st_sfc(purrr::map(1:7, function(i) sf::st_point(bc_pos(i))), crs = 4326)
  f <- file.path(dir, 'Caribou_Telem.shp')
  sf::st_write(sf::st_sf(d, geometry = pts), f, quiet = TRUE)
  f
}
bc_old <- function(dir) {
  p <- t(sapply(c(1, 2, 4, 8, 8, 9, 10), bc_pos))
  d <- data.table(
    population_unit = 'Herd', species = c(rep('Caribou', 6), 'Grey Wolf'),
    wlh_id = c('W1', 'W1', 'W1', 'W1', 'W1', '', 'W9'), deviceid = c('100', '100', '100', '100', '100', '', '9'),
    latitude = p[, 2], longitude = p[, 1],
    # old times are UTC plus the Pacific offset (7 h daylight, 8 h standard time)
    acquisition_date = c('2020-06-01T17:00:00Z', '2019-01-10T12:00:00Z', '2020-06-02T17:00:00Z',
                         '2021-01-01T00:00:00Z', '2021-01-01T00:00:00Z',   # 8: only in old, twice
                         '2021-01-02T00:00:00Z',                           # 9: only in old, no IDs
                         '2021-01-03T00:00:00Z'),                          # 10: a wolf
    vendor = 'Lotek', elevation = 1000, mortality = c(FALSE, TRUE, FALSE, NA, NA, NA, NA),
    secure_ind = TRUE)
  f <- file.path(dir, 'BC_NMC_Telemetry.csv')
  fwrite(d, f)
  f
}
bc_ref <- function(dir) {
  f <- file.path(dir, 'ref.csv')
  fwrite(data.table(Collar_ID = 200L, Latitude = bc_pos(5)[2], Longitude = bc_pos(5)[1],
                    timestamp = '2024-09-22T06:00:00Z', Acquisition_Date = '2024-09-22T13:00:00Z'), f)
  f
}

test_that('read_bc_locations combines the exports and fixes times', {
  dir <- withr::local_tempdir()
  out <- read_bc_locations(bc_new(dir), bc_old(dir), bc_ref(dir), list(name = 'BC'), list(), 'EPSG:3154')
  utc <- function(x) as.POSIXct(x, tz = 'UTC')
  setorder(out, t_)
  expect_equal(nrow(out), 8)   # 7 new + 1 only in old; old duplicate, no-ID fix and wolf dropped
  expect_equal(out$t_, utc(c('2017-03-12 02:30:00', '2019-01-10 04:00:00', '2020-06-01 10:00:00',
                             '2020-06-02 10:00:00', '2020-12-31 16:00:00', '2024-09-22 06:00:00',
                             '2024-09-23 00:00:00', '2024-09-23 00:00:00')))
  expect_equal(out$bc_file, c(rep('new', 4), 'old', rep('new', 3)))
  expect_equal(out$time_note[c(2, 4, 6)], c('clock time as UTC',
                                            'time from old file (new file had this position at another time)',
                                            'time corrected from reference file'))
  expect_equal(out$mortality, c(NA, TRUE, FALSE, FALSE, NA, NA, NA, NA))
  expect_equal(out$id, c(rep('A1', 5), rep('B2', 3)))   # old-only fix gets new's animal ID
  expect_equal(out$time_clash, c(rep(FALSE, 6), TRUE, TRUE))
  log <- attr(out, 'merge_log')
  expect_equal(log[step == 'old: distinct fixes (duplicates removed)']$n, 5)
  expect_equal(log[step == 'old: fixes not in new without an animal or collar ID (dropped)']$n, 1)
})

test_that('read_bc_locations works with the new export alone', {
  dir <- withr::local_tempdir()
  out <- read_bc_locations(bc_new(dir), NULL, NULL, list(), list(), 'EPSG:3154')
  expect_equal(nrow(out), 7)
  expect_equal(sum(out$time_note == 'clock time as UTC'), 7)
  expect_equal(sort(out$t_)[1], as.POSIXct('2017-03-12 02:30:00', tz = 'UTC'))
  expect_true(all(is.na(out$mortality)))
})

test_that('remove_utc_offset undoes the Pacific offset', {
  utc <- function(x) as.POSIXct(x, tz = 'UTC')
  shifted <- utc(c('2020-07-01 17:00:00', '2020-01-01 08:00:00', '2020-11-01 15:30:00', '2020-11-01 17:30:00'))
  expect_equal(remove_utc_offset(shifted, 'America/Vancouver'),
               utc(c('2020-07-01 10:00:00', '2020-01-01 00:00:00', '2020-11-01 08:30:00', '2020-11-01 09:30:00')))
})

test_that('match_fixes finds pairs across grid cell edges', {
  # two fixes 1 m apart on either side of a 0.001 degree cell edge
  a <- data.table(g = 'h', t = as.POSIXct('2020-01-01', tz = 'UTC'), lat = 58.0009999, lon = -125.0009999)
  b <- data.table(g = 'h', t = a$t + 30, lat = 58.0010005, lon = -125.0010005)
  pr <- match_fixes(a, b, 60, 2)
  expect_equal(nrow(pr), 1)
  expect_equal(pr$dt, 30)
  expect_equal(nrow(match_fixes(a, b, 10, 2)), 0)
})

test_that('check_config accepts a bc source and asks for its new file', {
  cfg <- read_config('NMC', config_dir = testthat::test_path('..', '..', 'config'))
  cfg$locations$sources <- list(list(type = 'bc', old = list(path = 'old.csv')))
  expect_error(check_config(cfg), 'needs `new:`')
})

