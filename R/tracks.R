# === Tracks, steps and random steps -----------------------------------------
# Adapted from NMC_CEtool and prepTracks R/ (resample_tracks, make_random_steps,
# calc_distribution_parameters), originally developed with Alec L. Robitaille.

#' @title Make an amt track
#' @param DT data.table of standard locations for one track.
#' @param crs character. Project coordinate system.
#' @return `track_xyt` with all columns kept.
#' @author Julie W. Turner
make_tracks <- function(DT, crs) {
  amt::make_track(DT, x_, y_, t_, crs = crs, all_cols = TRUE)
}

#' @title Resample a track to a regular rate and make steps
#' @description Resamples to one fix per `rate` (within `tolerance`), drops
#'   bursts (runs of regular fixes) with fewer than `min_burst_n` fixes, turns
#'   bursts into steps, and drops steps longer than the `probsfilter` quantile
#'   (unnaturally long steps).
#' @param tracks `track_xyt` for one animal.
#' @param rate,tolerance lubridate periods, e.g. `hours(11)`, `minutes(120)`.
#' @param probsfilter numeric. Quantile of step length above which steps are dropped.
#' @param min_burst_n integer. Fewest fixes in a burst. NMC_CEtool used 3
#'   (the `amt` default); prepTracks also dropped bursts with fewer than 4 steps.
#' @param min_steps integer. Tracks with fewer steps than this after resampling
#'   return NULL. 20 is semi-arbitrary but should give robust estimates
#'   (Street et al. MEE 2021).
#' @return `steps_xyt`, or NULL.
#' @author Julie W. Turner
resample_tracks <- function(tracks, rate, tolerance, probsfilter, min_burst_n = 3, min_steps = 20) {
  t <- amt::track_resample(tracks, rate = rate, tolerance = tolerance) |>
    amt::filter_min_n_burst(min_n = min_burst_n)

  if (nrow(t) < min_steps) return()

  s <- amt::steps_by_burst(t, lonlat = FALSE, keep_cols = 'start')
  s[!is.na(s$sl_) & s$sl_ <= stats::quantile(s$sl_, probs = probsfilter, na.rm = TRUE), ]
}

#' @title Make random steps
#' @description Pairs each observed step with `n_rand` random (available) steps
#'   drawn from the step-length and turning-angle distributions. Bursts with
#'   fewer than 3 steps are dropped first (as amt::random_steps() would), and an
#'   animal with none left gets NULL rather than an error.
#' @param DT `steps_xyt` for one animal, or NULL.
#' @param n_rand integer. Random steps per observed step.
#' @param sl.distr,ta.distr fitted distributions from `amt::fit_distr()`; if
#'   NULL, fitted to this animal's own steps.
#' @return `random_steps`, or NULL.
#' @author Julie W. Turner
make_random_steps <- function(DT, n_rand = 10, sl.distr = NULL, ta.distr = NULL) {
  if (is.null(DT) || nrow(DT) == 0) return()
  n <- table(DT$burst_)
  DT <- DT[DT$burst_ %in% names(n)[n >= 3], ]
  if (nrow(DT) == 0) return()
  if (is.null(sl.distr)) sl.distr <- amt::fit_distr(DT$sl_, 'gamma')
  if (is.null(ta.distr)) ta.distr <- amt::fit_distr(DT$ta_, 'vonmises')
  amt::random_steps(DT, n_control = n_rand, sl_distr = sl.distr, ta_distr = ta.distr)
}

#' @title Step-length and turning-angle distribution parameters
#' @param steps `random_steps` for one animal, or NULL.
#' @return data.table with one row: the distribution parameters and `id`.
#' @author Julie W. Turner
calc_distribution_parameters <- function(steps) {
  if (is.null(steps)) return()
  tas <- amt::ta_distr_params(steps)
  sls <- amt::sl_distr_params(steps)
  data.table::data.table(cbind(data.table::setDT(tas), data.table::setDT(sls), id = unique(steps$id)))
}

#' @title Finish the step table
#' @description Converts random steps to a data.table, adds `year` (of the
#'   step's end) and `int.year` (year rounded down to `year_interval`), keeps
#'   `min_year` to `max_year`, and adds `indiv_step_id`, a step ID that is
#'   unique across animals.
#' @param randsteps `random_steps` for all animals.
#' @param trk_cfg named list. The `tracks:` settings.
#' @return data.table.
#' @author Julie W. Turner
prep_steps <- function(randsteps, trk_cfg) {
  DT <- data.table::as.data.table(randsteps)
  DT[, `:=`(year = as.integer(format(t2_, '%Y', tz = 'UTC')))]
  DT[, int.year := year %/% trk_cfg$year_interval * trk_cfg$year_interval]
  DT <- DT[year >= trk_cfg$min_year & year <= trk_cfg$max_year]
  DT[, indiv_step_id := paste(id, step_id_, sep = '_')]
  DT[]
}

#' @title Neighbourhood size from step lengths
#' @description Median step length (observed and random steps, as in
#'   NMC_CEtool), rounded down to `round_m` metres. Used as the buffer for
#'   neighbourhood (proportion) layers.
#' @param steps data.table from [prep_steps()].
#' @param round_m numeric.
#' @return numeric.
#' @author Julie W. Turner
calc_step_buffer <- function(steps, round_m = 50) {
  floor(stats::median(steps$sl_, na.rm = TRUE) / round_m) * round_m
}

#' @title Keep locations within the years being analysed
#' @description Done before making tracks so that years outside the analysis
#'   are not resampled only to be dropped later. Uses the year of each fix.
#' @param DT data.table of standard locations.
#' @param min_year,max_year integer.
#' @return data.table.
#' @author Julie W. Turner
filter_years <- function(DT, min_year, max_year) {
  yr <- as.integer(format(DT$t_, '%Y', tz = 'UTC'))
  DT[yr >= min_year & yr <= max_year]
}

#' @title Fit a step-length or turning-angle distribution to all steps
#' @param resamples `steps_xyt` for all animals (NULL where a track was dropped).
#' @param column character. "sl_" or "ta_".
#' @param distr character. Distribution name for `amt::fit_distr()`.
#' @return fitted distribution.
#' @author Julie W. Turner
fit_step_distr <- function(resamples, column, distr) {
  if (NROW(resamples) == 0) {
    stop('No tracks are left after resampling: none had min_steps steps at rate_hours',
         ' (within tolerance_minutes) between min_year and max_year. Check the tracks:',
         ' settings, or the animals chosen by subset:.', call. = FALSE)
  }
  amt::fit_distr(resamples[[column]], distr)
}
