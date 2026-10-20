# === Targets: tracks and random steps -----------------------------------------
# Comparable to the prepTracks SpaDES module. From `splits` on, each track
# (by default each animal) is its own branch: pattern = map(<upstream>).

#' @title Targets: tracks, steps and random steps
#' @inheritParams targets_config
#' @return list of targets.
#' @author Julie W. Turner
targets_tracks <- function(cfg) {
  # steps kept only inside an area (tracks: within:), e.g. the jurisdictions the layers cover
  within <- cfg$tracks$within
  area_targets <- if (length(within)) list(
    tar_target_raw('steps_area_file', substitute(get_source(SRC, cfg_paths), list(SRC = within[setdiff(names(within), c('field', 'keep'))])),
                   format = 'file'),
    tar_target(steps_area, make_steps_area(steps_area_file, cfg_tracks$within, cfg_project$crs))
  )
  steps_target <- if (length(within)) {
    tar_target(steps, prep_steps(randsteps, cfg_tracks, steps_area))
  } else {
    tar_target(steps, prep_steps(randsteps, cfg_tracks))
  }
  out <- list(
    # only the years being analysed (and, for test runs, part of the data)
    tar_target(locations_years,
               filter_years(locations, cfg_tracks$min_year, cfg_tracks$max_year) |>
                 subset_locations(cfg_subset)),

    # the iteration units
    tar_target(splits,
               data.table::copy(locations_years)[, tar_group := .GRP, by = c(unlist(cfg_tracks$split_by))],
               iteration = 'group'),

    tar_target(tracks, make_tracks(splits, cfg_project$crs), pattern = map(splits)),

    # regular fix rate, long steps removed
    tar_target(resamples,
               resample_tracks(tracks,
                               rate = lubridate::hours(cfg_tracks$rate_hours),
                               tolerance = lubridate::minutes(cfg_tracks$tolerance_minutes),
                               probsfilter = cfg_tracks$max_step_quantile,
                               min_burst_n = cfg_tracks$min_burst_n,
                               min_steps = cfg_tracks$min_steps,
                               zero_ta = cfg_tracks$zero_step_ta %||% 'random'),
               pattern = map(tracks)),

    # distributions fitted to all animals' steps together
    tar_target(sl_distr, fit_step_distr(resamples, 'sl_', cfg_tracks$sl_distr)),
    tar_target(ta_distr, fit_step_distr(resamples, 'ta_', cfg_tracks$ta_distr)),

    tar_target(randsteps,
               make_random_steps(resamples, n_rand = cfg_tracks$n_random, sl_distr, ta_distr),
               pattern = map(resamples)),

    tar_target(distparams, calc_distribution_parameters(randsteps), pattern = map(randsteps)),

    steps_target,
    tar_target(steps_outside_area, attr(steps, 'outside_area')),
    tar_target(step_years, sort(unique(steps$year))),

    tar_target(step_buffer, calc_step_buffer(steps, cfg_tracks$buffer_round_m))
  )
  c(out, area_targets)
}
