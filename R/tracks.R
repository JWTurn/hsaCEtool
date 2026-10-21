# === Tracks, steps and random steps -----------------------------------------
# Adapted from NMC_CEtool and prepTracks R/ (resample_tracks, make_random_steps,
# calc_distribution_parameters), originally developed with Alec L. Robitaille.

#' @title Make an amt track
#' @param DT data.table of standard locations for one track.
#' @param crs character. Project coordinate system.
#' @return `track_xyt` with all columns kept.
#' @author Julie W. Turner
#' @export
make_tracks <- function(DT, crs) {
  amt::make_track(DT, x_, y_, t_, crs = crs, all_cols = TRUE)
}

#' @title Resample a track to a regular rate and make steps
#' @description Resamples to one fix per `rate` (within `tolerance`), drops
#'   bursts (runs of regular fixes) with fewer than `min_burst_n` fixes, turns
#'   bursts into steps, fixes steps of zero length (`fix_zero_steps()`), and
#'   drops steps longer than the `probsfilter` quantile (unnaturally long steps).
#' @param tracks `track_xyt` for one animal.
#' @param rate,tolerance lubridate periods, e.g. `hours(11)`, `minutes(120)`.
#' @param probsfilter numeric. Quantile of step length above which steps are dropped.
#' @param min_burst_n integer. Fewest fixes in a burst. NMC_CEtool used 3
#'   (the `amt` default); prepTracks also dropped bursts with fewer than 4 steps.
#' @param min_steps integer. Tracks with fewer steps than this after resampling
#'   return NULL. 20 is semi-arbitrary but should give robust estimates
#'   (Street et al. MEE 2021).
#' @param zero_ta character. Direction given to zero-length steps: see `fix_zero_steps()`.
#' @return `steps_xyt`, or NULL.
#' @author Julie W. Turner
#' @export
resample_tracks <- function(tracks, rate, tolerance, probsfilter, min_burst_n = 3, min_steps = 20, zero_ta = 'random') {
  t <- amt::track_resample(tracks, rate = rate, tolerance = tolerance) |>
    amt::filter_min_n_burst(min_n = min_burst_n)

  if (nrow(t) < min_steps) return()

  s <- amt::steps_by_burst(t, lonlat = FALSE, keep_cols = 'start')
  s <- fix_zero_steps(s, zero_ta)
  s[!is.na(s$sl_) & s$sl_ <= stats::quantile(s$sl_, probs = probsfilter, na.rm = TRUE), ]
}

#' @title Give zero-length steps a length and a direction
#' @description A step where the animal is at exactly the same place at both
#'   fixes has no direction, so amt leaves its turning angle, and the next
#'   step's, missing, and the random steps for the next step get no end point.
#'   Such a step gets the animal's shortest non-zero step length, and a
#'   direction: random (uniform), or that of the step before (turning angle
#'   0). Its turning angle, and the next step's, are then worked out from that
#'   direction. Positions are not changed.
#' @param s `steps_xyt` from `amt::steps_by_burst()`, one animal, in time order.
#' @param ta character. "random" or "zero".
#' @return `s`, with `sl_`, `direction_p` and `ta_` filled in for zero-length steps.
#' @author Julie W. Turner
#' @keywords internal
#' @noRd
fix_zero_steps <- function(s, ta = 'random') {
  z <- which(s$sl_ == 0)
  pos <- s$sl_[!is.na(s$sl_) & s$sl_ > 0]
  if (!length(z) || !length(pos)) return(s)
  if (!ta %in% c('random', 'zero')) stop('zero-length step direction must be "random" or "zero"', call. = FALSE)
  wrap <- function(a) (a + pi) %% (2 * pi) - pi   # to -pi..pi, as amt's turning angles
  s$sl_[z] <- min(pos)
  # in time order, so runs of zero-length steps chain
  for (i in z) {
    prev <- if (i > 1 && s$burst_[i - 1] == s$burst_[i]) s$direction_p[i - 1] else NA_real_
    d <- if (ta == 'zero' && !is.na(prev)) prev else stats::runif(1, -pi, pi)
    s$direction_p[i] <- d
    if (!is.na(prev)) s$ta_[i] <- wrap(d - prev)
    if (i < nrow(s) && s$burst_[i + 1] == s$burst_[i] && !is.na(s$direction_p[i + 1])) {
      s$ta_[i + 1] <- wrap(s$direction_p[i + 1] - d)
    }
  }
  s
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
#' @export
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
#' @export
calc_distribution_parameters <- function(steps) {
  if (is.null(steps)) return()
  tas <- amt::ta_distr_params(steps)
  sls <- amt::sl_distr_params(steps)
  data.table::data.table(cbind(data.table::setDT(tas), data.table::setDT(sls), id = unique(steps$id)))
}

#' @title Finish the step table
#' @description Converts random steps to a data.table, adds `year` (of the
#'   step's end) and `int.year` (year rounded down to `year_interval`), keeps
#'   `min_year` to `max_year` (null = no limit), drops tracks with fewer than
#'   `min_steps` used steps left (after resampling, short bursts, long steps,
#'   years outside the range and steps outside `area` have been removed), and adds
#'   `indiv_step_id`, a step ID that is unique across animals.
#' @param randsteps `random_steps` for all animals.
#' @param trk_cfg named list. The `tracks:` settings.
#' @param area sfc or NULL. If given, only steps inside it are kept
#'   (`keep_steps_within()`); the counts dropped are in attribute `outside_area`.
#' @return data.table.
#' @author Julie W. Turner
#' @export
prep_steps <- function(randsteps, trk_cfg, area = NULL) {
  DT <- data.table::as.data.table(randsteps)
  DT[, `:=`(year = as.integer(format(t2_, '%Y', tz = 'UTC')))]
  DT[, int.year := year %/% trk_cfg$year_interval * trk_cfg$year_interval]
  DT <- DT[year >= (trk_cfg$min_year %||% -Inf) & year <= (trk_cfg$max_year %||% Inf)]
  # only steps inside the area (e.g. the jurisdictions the layers cover)
  DT <- keep_steps_within(DT, area)
  outside <- attr(DT, 'outside_area')
  # min_steps again, on the steps that are left
  if (length(trk_cfg$min_steps)) {
    n <- DT[case_ == TRUE, .N, by = id]
    DT <- DT[id %in% n[N >= trk_cfg$min_steps]$id]
  }
  DT[, indiv_step_id := paste(id, step_id_, sep = '_')]
  data.table::setattr(DT, 'outside_area', outside)
  DT[]
}

#' @title Keep only steps inside an area
#' @description Drops each step (the used step and all its random steps) whose
#'   used step starts or ends outside `area`, and each random step that ends
#'   outside it. Only points near the parts outside the area are tested, so
#'   this is quick even for millions of steps.
#' @param DT data.table of used and random steps (columns `id`, `step_id_`,
#'   `case_`, `x1_`, `y1_`, `x2_`, `y2_`), in the area's coordinate system.
#' @param area sfc or NULL. Polygon(s) to keep steps within; NULL keeps all.
#' @return `DT` without those rows, with attribute `outside_area`: a data.table
#'   of counts (used steps dropped, random steps dropped).
#' @author Julie W. Turner
#' @keywords internal
#' @noRd
keep_steps_within <- function(DT, area) {
  none <- data.table::data.table(what = c('used steps dropped: start or end outside the area',
                                          'random steps dropped: end outside the area'), n = 0L)
  if (is.null(area) || !nrow(DT)) return(data.table::setattr(DT, 'outside_area', if (is.null(area)) NULL else none))
  crs <- sf::st_crs(area)
  xy <- c(DT$x1_, DT$x2_); yy <- c(DT$y1_, DT$y2_)
  box <- sf::st_as_sfc(sf::st_bbox(c(xmin = min(xy, na.rm = TRUE), ymin = min(yy, na.rm = TRUE),
                                     xmax = max(xy, na.rm = TRUE), ymax = max(yy, na.rm = TRUE)), crs = crs))
  outside <- sf::st_difference(sf::st_buffer(box, 1000), sf::st_union(area))
  if (any(grepl('COLLECTION', sf::st_geometry_type(outside)))) outside <- sf::st_collection_extract(outside, 'POLYGON')
  outside <- suppressWarnings(sf::st_cast(outside, 'POLYGON'))   # one test per piece
  if (!length(outside)) return(data.table::setattr(DT, 'outside_area', none))
  is_out <- function(x, y) {
    out <- !is.na(x) & FALSE
    for (k in seq_along(outside)) {
      b <- sf::st_bbox(outside[k])
      cand <- which(!out & !is.na(x) & x >= b[['xmin']] & x <= b[['xmax']] & y >= b[['ymin']] & y <= b[['ymax']])
      if (!length(cand)) next
      pts <- sf::st_as_sf(data.frame(x = x[cand], y = y[cand]), coords = c('x', 'y'), crs = crs)
      out[cand] <- lengths(sf::st_intersects(pts, outside[k])) > 0
    }
    out
  }
  end_out <- is_out(DT$x2_, DT$y2_)
  start_out <- is_out(DT$x1_, DT$y1_)
  key <- paste(DT$id, DT$step_id_)
  bad <- unique(key[DT$case_ & (end_out | start_out)])
  drop_step <- key %in% bad
  drop_rand <- !DT$case_ & end_out & !drop_step
  counts <- data.table::data.table(what = none$what, n = c(length(bad), sum(drop_rand)))
  data.table::setattr(DT[!drop_step & !drop_rand], 'outside_area', counts)
}

#' @title The area steps are kept within
#' @description Reads a polygon file, keeps the polygons whose `field` is one of
#'   `keep` (e.g. the provinces and territories the landscape layers cover),
#'   and merges them, in the project's coordinate system.
#' @param file character. The polygon file (from [get_source()]).
#' @param within named list. `tracks: within:` settings: `field` and `keep`
#'   (both optional; without them every polygon is used).
#' @param crs character. The project's coordinate system.
#' @return sfc.
#' @author Julie W. Turner
#' @export
make_steps_area <- function(file, within, crs) {
  a <- sf::st_read(file[tolower(tools::file_ext(file)) %in% c('shp', 'gpkg', 'geojson')][1], quiet = TRUE)
  if (!is.null(within$field)) {
    if (!within$field %in% names(a)) {
      stop('tracks: within: field ', within$field, ' is not in ', basename(file[1]), ' (has ',
           paste(names(a), collapse = ', '), ')', call. = FALSE)
    }
    keep <- unlist(within$keep)
    miss <- setdiff(keep, a[[within$field]])
    if (length(miss)) stop('tracks: within: keep has values not in ', within$field, ': ', paste(miss, collapse = ', '), call. = FALSE)
    a <- a[a[[within$field]] %in% keep, ]
  }
  sf::st_union(sf::st_make_valid(sf::st_transform(sf::st_geometry(a), crs)))
}

#' @title Neighbourhood size from step lengths
#' @description Median step length (observed and random steps, as in
#'   NMC_CEtool), rounded down to `round_m` metres. Used as the buffer for
#'   neighbourhood (proportion) layers.
#' @param steps data.table from [prep_steps()].
#' @param round_m numeric.
#' @return numeric.
#' @author Julie W. Turner
#' @export
calc_step_buffer <- function(steps, round_m = 50) {
  floor(stats::median(steps$sl_, na.rm = TRUE) / round_m) * round_m
}

#' @title Keep locations within the years being analysed
#' @description Done before making tracks so that years outside the analysis
#'   are not resampled only to be dropped later. Uses the year of each fix.
#' @param DT data.table of standard locations.
#' @param min_year,max_year integer, or NULL for no limit.
#' @return data.table.
#' @author Julie W. Turner
#' @export
filter_years <- function(DT, min_year, max_year) {
  yr <- as.integer(format(DT$t_, '%Y', tz = 'UTC'))
  DT[yr >= (min_year %||% -Inf) & yr <= (max_year %||% Inf)]
}

#' @title First and last year of the landscape layers
#' @description `tracks: min_year` and `max_year` if set, otherwise the first
#'   and last year of the steps. Built layers whose `first_year` or
#'   `last_year` is blank in the layer table use these.
#' @param min_year,max_year integer, or NULL.
#' @param step_years integer. Years of the steps, or NULL when both are set.
#' @return integer: first and last year.
#' @author Julie W. Turner
#' @export
layer_year_range <- function(min_year, max_year, step_years = NULL) {
  as.integer(c(min_year %||% min(step_years), max_year %||% max(step_years)))
}

#' @title Fill blank years with the layer year range
#' @param x integer. Years from the layer table (NA where blank).
#' @param default integer. The year to use where blank.
#' @return integer.
#' @keywords internal
#' @export
fill_years <- function(x, default) {
  as.integer(ifelse(is.na(x), default, x))
}

#' @title Fit a step-length or turning-angle distribution to all steps
#' @param resamples `steps_xyt` for all animals (NULL where a track was dropped).
#' @param column character. "sl_" or "ta_".
#' @param distr character. Distribution name for `amt::fit_distr()`.
#' @return fitted distribution.
#' @author Julie W. Turner
#' @export
fit_step_distr <- function(resamples, column, distr) {
  if (NROW(resamples) == 0) {
    stop('No tracks are left after resampling: none had min_steps steps at rate_hours',
         ' (within tolerance_minutes) between min_year and max_year. Check the tracks:',
         ' settings, or the animals chosen by subset:.', call. = FALSE)
  }
  amt::fit_distr(resamples[[column]], distr)
}
