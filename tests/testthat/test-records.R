# Capture logs, recorded deaths and long silences, with made-up animals and collars

day <- function(d) as.POSIXct('2020-01-01', tz = 'UTC') + d * 86400

test_that('record dates in different formats are read', {
  expect_equal(format(parse_record_dates(c('43831', '2020-01-15', '25/3/2020', '18 Feb 2019', 'unk', NA))),
               c('2020-01-01', '2020-01-15', '2020-03-25', '2019-02-18', NA, NA))
  expect_equal(format(parse_record_dates('3/25/2020', 'mdy')), '2020-03-25')
})

test_that('a capture log is read, with sex FALSE taken as female', {
  f <- withr::local_tempfile(fileext = '.csv')
  fwrite(data.table(Primary = c('A', 'B', 'A'), CapID = c('A', 'B', 'A-R'), Date = c('2020-01-06', '2022-07-09', '1/1/2023'),
                    Serial = c('c1', 'c1', 'c2'), Sex = c('FALSE', 'M', 'F'), Recap = c('N', 'N', 'Y')), f)
  cap <- read_capture_log(f, list(id = 'Primary', capture_id = 'CapID', date = 'Date', collar = 'Serial', sex = 'Sex',
                                  recapture = 'Recap'), 'BC')
  expect_equal(cap$sex, c('f', 'm', 'f'))
  expect_equal(format(cap$capture_date), c('2020-01-06', '2022-07-09', '2023-01-01'))
  expect_equal(unique(cap$source), 'BC')
})

test_that('captures moves a redeployed collar to the new animal and logs what it leaves', {
  cap <- data.table(source = 'BC', id = c('A', 'B', 'D'), capture_id = c('A', 'B', 'D'), capture_date = day(c(5, 920, 3)),
                    collar = c('c1', 'c1', 'c2'), sex_raw = c('FALSE', 'M', 'F'), recapture = 'N', herd = 'h', sex = c('f', 'm', 'f'))
  # collar c1: on A from day 5 (fixes from day 0), silent, then put on B on day 920 but still filed under A
  DT <- data.table(source = 'BC', id = 'A', t_ = day(c(0:10, 900:930)), x_ = 1, y_ = 1, collar_id = 'c1', sex = NA_character_)
  # collar c2 logged on D, but these fixes are filed under E (not in the log)
  DT <- rbind(DT, data.table(source = 'BC', id = 'E', t_ = day(10:14), x_ = 1, y_ = 1, collar_id = 'c2', sex = NA_character_),
              data.table(source = 'yk', id = 'Z', t_ = day(1:3), x_ = 1, y_ = 1, collar_id = 'c1', sex = 'm'))
  out <- clean_captures(DT, list(table = cap, source = 'BC'), 'EPSG:3154')
  expect_equal(nrow(out[id == 'A' & t_ < day(5)]), 5)            # before its logged capture: kept, logged
  expect_equal(nrow(out[id == 'B']), 11)                         # days 920-930 moved to B
  expect_equal(nrow(out[id == 'A' & t_ >= day(900)]), 20)        # days 900-919 stay with A (dropped later if A died)
  expect_equal(nrow(out[id == 'E']), 5)                          # log disagrees: kept, logged
  expect_equal(nrow(out[id == 'Z']), 3)                          # other sources untouched
  expect_equal(unique(out[id == 'B']$deployment_start), day(920))
  expect_equal(unique(out[id %in% c('A', 'B')]$sex), c('f', 'm'))  # filled from the log
  d <- attr(out, 'decisions')
  expect_true(all(c('fixes before logged capture kept (check)', 'fixes moved to another animal', 'capture log disagrees, fixes kept (check)',
                    'capture log: sex FALSE read as female', 'sex filled from capture log') %in% d$action))
  expect_equal(d[action == 'fixes moved to another animal']$fixes, 11L)
  # capture effect then applies after each capture
  ce <- clean_capture_effect(out, list(days = 2), 'EPSG:3154')
  expect_equal(min(ce[id == 'B']$t_), day(922))
})

test_that('mortality records drop fixes after the death and make the animal dead', {
  DT <- data.table(source = 'BC', id = rep(c('A', 'C'), each = 10), t_ = day(rep(1:10, 2)), x_ = 1, y_ = 1)
  rec <- data.table(source = 'BC', id = c('A', 'Q'), mortality_date = day(c(5, 3)), mortality_cause = c('wolves', NA))
  expect_warning(out <- clean_mortality_records(DT, list(table = rec, after_days = 1), 'EPSG:3154'), '1 recorded deaths')
  expect_equal(max(out[id == 'A']$t_), day(6))                   # up to 1 day after the death
  expect_equal(nrow(out[id == 'C']), 10)
  d <- attr(out, 'decisions')
  expect_equal(d[action == 'mortality record applied']$fixes, 4L)
  expect_equal(d[action == 'mortality record not matched']$id, 'Q')
  # mortality_status: a recorded death comes first
  out[, `:=`(x_ = 500000 + seq_len(.N) * 500, y_ = 6700000)]
  st <- clean_end_clusters(out, list(zeta = 200, theta_hours = 48), 'EPSG:3154')
  expect_equal(unique(st[id == 'A']$mortality_basis), 'recorded: mortality record')
  expect_equal(unique(st[id == 'A']$mortality_status), 'dead')
  expect_equal(unique(st[id == 'C']$mortality_status), 'alive')
})

test_that('long_gaps splits a track at long silences and drops stray fixes', {
  DT <- data.table(source = 'BC', id = c(rep('A', 10), rep('B', 12)),
                   t_ = day(c(1:5, 500:504, 1:10, 2000:2001)), x_ = 1, y_ = 1)
  out <- clean_long_gaps(DT, list(max_gap_days = 365, min_fixes = 3), 'EPSG:3154')
  expect_equal(out[, .N, keyby = id]$N, c(5L, 5L, 10L))
  expect_equal(sort(unique(out$id)), c('A', 'A_2', 'B'))          # B's 2 stray fixes dropped
  d <- attr(out, 'decisions')
  expect_equal(d[action == 'track split at a long silence']$id, 'A')
  expect_equal(d[action == 'stray fixes after a long silence dropped']$fixes, 2L)
})

test_that('clean_locations collects what the record steps did', {
  DT <- data.table(source = 'BC', id = 'A', t_ = day(c(1:5, 900:910)), x_ = 500000 + 1:16 * 100, y_ = 6700000)
  out <- suppressMessages(clean_locations(DT, list(steps = list('long_gaps'),
                                                   params = list(long_gaps = list(max_gap_days = 365, min_fixes = 3))), 'EPSG:3154'))
  expect_equal(attr(out, 'decisions')$step, 'long_gaps')
  expect_equal(sort(unique(out$id)), c('A', 'A_2'))
})

test_that('the lookup links animals across captures, collars and Movebank deployments', {
  cap <- data.table(source = 'BC', id = c('A', 'A', 'B'), capture_id = c('A', 'A-R', 'B'), capture_date = day(c(1, 400, 1)),
                    collar = c('c1', 'c2', 'c3'), recapture = c('N', 'Y', 'N'), herd = 'h', sex = 'f')
  loc <- data.table(source = 'BC', id = c('A', 'A', 'A_2', 'B', 'B', 'B'),
                    collar_id = c('c1', 'c1', 'c2', 'c3', 'c4', 'c4'), t_ = day(1:6), x_ = 1, y_ = 1)
  loc <- rbind(loc, data.table(source = 'yk', id = c('d1', 'd1', 'd2', 'd2'), collar_id = NA, t_ = day(1:4), x_ = 1, y_ = 1,
                                    individual_local_identifier = 'M', deployment_id = c(1, 1, 2, 2), tag_local_identifier = c('t1', 't1', 't2', 't2')), fill = TRUE)
  lk <- make_animal_lookup(cap, loc)
  expect_equal(lk[kind == 'capture log' & capture_id == 'A-R']$our_ids, 'A_2')
  expect_equal(lk[kind == 'several collars, no logged recapture']$id, 'B')
  expect_equal(lk[kind == 'Movebank deployments']$our_ids, c('d1', 'd2'))
})
