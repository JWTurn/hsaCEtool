# === Daylight saving in local clock times -------------------------------------
# Some collars write local clock time that follows daylight saving, others the
# same local time without it, even within one source. These helpers find which,
# per animal and period, from the collar's fix schedule around each daylight
# saving change, for the cleaning step local_times (see clean_local_times()).

#' @title Most common value
#' @param v vector.
#' @return the most common value of `v` (the first, if tied); NA of the same type if `v` is empty.
#' @keywords internal
#' @noRd
mode_value <- function(v) {
  v <- v[!is.na(v)]
  if (!length(v)) return(v[NA_integer_])
  u <- unique(v)
  u[which.max(tabulate(match(v, u)))]
}

#' @title Daylight saving changes of a time zone
#' @description Times when `zone` changes to or from daylight saving between
#'   `from` and `to`, from the time zone database (so rule changes over the
#'   years are followed, e.g. B.C. moved its dates in 2007), given as clock
#'   times in the zone's standard time.
#' @param zone character. Time zone with daylight saving, e.g. `America/Vancouver`.
#' @param from,to POSIXct. Period to cover.
#' @return data.table: `when` (POSIXct, standard clock time stored as UTC) and
#'   `spring` (TRUE when clocks go forward).
#' @keywords internal
#' @noRd
dst_switches <- function(zone, from, to) {
  hrs <- seq(trunc(from, 'days') - 86400, to + 86400, by = 'hour')
  dst <- as.POSIXlt(hrs, tz = zone)$isdst
  i <- which(diff(dst) != 0) + 1
  std_off <- std_offset(zone)
  data.table::data.table(when = hrs[i] + std_off, spring = dst[i] == 1)
}

#' @title Standard-time offset of a time zone from UTC
#' @param zone character. Time zone.
#' @return numeric, seconds (negative west of Greenwich), from the zone's
#'   offset in mid-January and mid-July, whichever is smaller (standard time
#'   in either hemisphere).
#' @keywords internal
#' @noRd
std_offset <- function(zone) {
  t <- as.POSIXct(c('2021-01-15 12:00', '2021-07-15 12:00'), tz = 'UTC')
  off <- as.numeric(as.POSIXct(format(t, tz = zone), tz = 'UTC')) - as.numeric(t)
  min(off)
}

#' @title Read the schedule shift of each animal at each daylight saving change
#' @description Collars mostly fix on a set schedule (e.g. every 4 h). For each
#'   animal and each change, compares the most common clock time of fixes,
#'   modulo the schedule's interval, in the `window_days` before and after.
#'   Read in standard time, a clock that follows daylight saving moves the
#'   schedule 1 h later when clocks go forward and 1 h earlier when they go
#'   back (`dst_clock`). The opposite (`local_schedule`) is a collar that fixes
#'   on local daylight time but writes the right times, so it needs no change.
#'   Other results: `same` (no shift: no daylight saving) and `unclear` (too
#'   few fixes, no whole-hour schedule of 2-24 h, a 2 h schedule where +1 h
#'   and -1 h look alike, or another shift, e.g. the collar was reprogrammed).
#' @param DT data.table: `id`, `clk` (clock times read as standard time, stored
#'   as UTC).
#' @param switches data.table from [dst_switches()].
#' @param window_days numeric. Days before and after each change to compare.
#' @param min_fixes integer. Fixes needed on each side of a change.
#' @return data.table: `id`, `when`, `spring`, `interval_h`, `shift_h`, `result`.
#' @keywords internal
#' @noRd
dst_switch_shifts <- function(DT, switches, window_days = 14, min_fixes = 5) {
  win <- window_days * 86400
  out <- purrr::map(seq_len(nrow(switches)), function(i) {
    w <- switches$when[i]
    d <- DT[clk >= w - win & clk < w + win, .(id, clk)]
    if (!nrow(d)) return(NULL)
    data.table::setorder(d, id, clk)
    d[, dt_h := round(as.numeric(clk - data.table::shift(clk), units = 'hours'), 2), by = id]
    d[, k := mode_value(dt_h), by = id]
    d[, phase := round(((as.numeric(clk) %% 86400) / 3600) %% k, 1)]
    d[, .(nb = sum(clk < w), na = sum(clk >= w), k = k[1],
          pb = mode_value(phase[clk < w]), pa = mode_value(phase[clk >= w])), by = id
      ][, `:=`(when = w, spring = switches$spring[i])]
  })
  s <- data.table::rbindlist(out)
  if (!nrow(s)) {
    return(data.table::data.table(id = character(), when = as.POSIXct(character(), tz = 'UTC'),
                                  spring = logical(), interval_h = numeric(), shift_h = numeric(),
                                  result = character()))
  }
  ok <- s$nb >= min_fixes & s$na >= min_fixes & !is.na(s$k) & s$k >= 2 & s$k <= 24 &
    s$k == round(s$k) & !is.na(s$pb) & !is.na(s$pa)
  s[, shift_h := data.table::fifelse(ok, round((pa - pb + k / 2) %% k - k / 2, 1), NA_real_)]
  later <- abs(s$shift_h - 1) < 0.2 & s$k > 2
  earlier <- abs(s$shift_h + 1) < 0.2 & s$k > 2
  s[, result := data.table::fcase(
    is.na(shift_h), 'unclear',
    abs(shift_h) < 0.2, 'same',
    (spring & later) | (!spring & earlier), 'dst_clock',
    (spring & earlier) | (!spring & later), 'local_schedule',
    default = 'unclear'
  )]
  s[, .(id, when, spring, interval_h = k, shift_h, result)]
}

#' @title Decide, per animal and period, whether clock times follow daylight saving
#' @description Uses each animal's schedule shifts at daylight saving changes
#'   ([dst_switch_shifts()]): `dst_clock` says yes; `same` and `local_schedule`
#'   say no; `unclear` says nothing. Runs of the same answer form periods. Where
#'   the answer changes between two changes, the new period starts after the
#'   longest gap between fixes in between (often a collar being replaced).
#'   Animals with no answer take the majority answer (ties: no) of other
#'   animals with the same `value` at the nearest change that has answers,
#'   within `borrow_days` of the middle of their track (`method` `borrowed`);
#'   failing that, no daylight saving (`method` `no evidence`).
#' @param DT data.table: `id`, `clk`, `value` (the step's column).
#' @param shifts data.table from [dst_switch_shifts()].
#' @param borrow_days numeric. How far from an animal's track to borrow answers.
#' @return data.table, one row per animal and period: `id`, `value`, `from`,
#'   `to` (clock times; `to` is the start of the next period, not included), `dst`, `method`, `switches` (changes the animal's
#'   fixes span), `dst_clock`, `same`, `local_schedule`, `unclear` (counts).
#' @keywords internal
#' @noRd
dst_periods <- function(DT, shifts, borrow_days = 365) {
  shifts <- data.table::copy(shifts)
  shifts[, dst := data.table::fcase(result == 'dst_clock', TRUE,
                                    result %in% c('same', 'local_schedule'), FALSE,
                                    default = NA)]
  tracks <- DT[, .(value = value[1], start = min(clk), end = max(clk)), by = id]
  shifts <- tracks[, .(id, value)][shifts, on = 'id']
  counts <- shifts[, .(switches = .N, dst_clock = sum(result == 'dst_clock'), same = sum(result == 'same'),
                       local_schedule = sum(result == 'local_schedule'),
                       unclear = sum(result == 'unclear')), by = id]
  known <- shifts[!is.na(dst)]
  data.table::setorder(known, id, when)

  ## periods of animals with answers
  per <- purrr::map(unique(known$id), function(a) {
    k <- known[id == a]
    run <- data.table::rleid(k$dst)
    tr <- tracks[id == a]
    starts <- tr$start
    for (r in seq_len(max(run))[-1]) {
      lo <- max(k$when[run == r - 1])
      hi <- min(k$when[run == r])
      t <- sort(DT[id == a & clk > lo & clk < hi, clk])
      t <- c(lo, t, hi)
      starts <- c(starts, t[which.max(diff(as.numeric(t))) + 1])
    }
    ends <- c(starts[-1], tr$end + 1)
    data.table::data.table(id = a, value = tr$value, from = starts, to = ends,
                           dst = k$dst[!duplicated(run)], method = 'detected')
  })

  ## animals without answers borrow from the nearest change with answers
  rest <- tracks[!id %in% known$id]
  if (nrow(rest)) {
    votes <- known[, .(yes = sum(dst), no = sum(!dst)), by = .(value, when)]
    bor <- purrr::map(seq_len(nrow(rest)), function(i) {
      r <- rest[i]
      mid <- r$start + (r$end - r$start) / 2
      v <- votes[value == r$value]
      v <- v[abs(as.numeric(when - mid, units = 'days')) <= borrow_days]
      if (nrow(v)) {
        v <- v[which.min(abs(as.numeric(when - mid)))]
        dst <- v$yes > v$no
        method <- 'borrowed'
      } else {
        dst <- FALSE
        method <- 'no evidence'
      }
      data.table::data.table(id = r$id, value = r$value, from = r$start, to = r$end + 1, dst = dst, method = method)
    })
    per <- c(per, bor)
  }
  per <- data.table::rbindlist(per)
  per <- counts[per, on = 'id']
  for (j in c('switches', 'dst_clock', 'same', 'local_schedule', 'unclear')) {
    data.table::set(per, which(is.na(per[[j]])), j, 0L)
  }
  data.table::setcolorder(per, c('id', 'value', 'from', 'to', 'dst', 'method'))
  data.table::setorder(per, id, from)
  per[]
}
