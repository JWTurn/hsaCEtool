# === Cleaning GPS locations -------------------------------------------------
# Each cleaning step is a function clean_<step>(DT, params, crs) that takes and
# returns a data.table of standard locations (id, t_, x_, y_). Steps are run in
# the order set in `locations: cleaning: steps:`. A project can add its own step
# in config/<project>.R as cfg$locations$cleaning$custom$<step> <- function(DT, params, crs) ...
# Adapted from NMC_CEtool scripts/2-clean-data.R (amt-based steps) and
# caribouLocPrep R/filter_*.R (speed, quick versions).

#' @title Clean GPS locations
#' @description Removes exact duplicate time stamps, then runs each cleaning
#'   step in turn and reports how many fixes each step removed.
#' @param DT data.table of standard locations from [standardize_locations()].
#' @param cleaning named list. The `locations: cleaning:` settings (`steps`,
#'   `params`, optional `custom`).
#' @param crs character. Project coordinate system.
#' @return data.table of cleaned locations, with a `cleaning_log` attribute
#'   (fixes removed by each step).
#' @author Julie W. Turner
clean_locations <- function(DT, cleaning, crs) {
  DT <- data.table::copy(DT)
  data.table::setorder(DT, id, t_)
  n_start <- nrow(DT)
  DT <- unique(DT, by = c('id', 't_'))
  log <- data.table::data.table(step = 'same_time_duplicates', removed = n_start - nrow(DT))

  for (step in unlist(cleaning$steps)) {
    fun <- cleaning$custom[[step]] %||% get(paste0('clean_', step), mode = 'function')
    before <- nrow(DT)
    DT <- data.table::as.data.table(fun(DT, cleaning$params[[step]], crs))
    log <- rbind(log, data.table::data.table(step = step, removed = before - nrow(DT)))
    message(sprintf('[clean %s] removed %s of %s fixes', step, before - nrow(DT), before))
  }
  data.table::setattr(DT, 'cleaning_log', log)
  DT
}

#' @title Run an amt flagging function on each animal's track
#' @param DT data.table of standard locations, sorted by id and t_.
#' @param flag_fun function taking a `track_xyt` and returning it with a flag column.
#' @param flag_col character. Name of the logical column `flag_fun` adds.
#' @return `DT` without the flagged rows.
#' @keywords internal
#' @noRd
drop_amt_flagged <- function(DT, flag_fun, flag_col) {
  DT[, .flag := flag_fun(amt::make_track(.SD, x_, y_, t_, all_cols = TRUE))[[flag_col]], by = id]
  DT <- DT[is.na(.flag) | !.flag]
  DT[, .flag := NULL]
  DT
}

#' @title Cleaning step: keep fixes inside a box
#' @param DT data.table of standard locations.
#' @param params named list: `xmin`, `ymin`, `xmax`, `ymax`, and `crs` of those
#'   limits (default longitude/latitude, "EPSG:4326").
#' @param crs character. Project coordinate system.
#' @return data.table.
#' @author Julie W. Turner
clean_bounds <- function(DT, params, crs) {
  xy <- as.matrix(DT[, .(x_, y_)])
  box_crs <- params$crs %||% 'EPSG:4326'
  if (!identical(sf::st_crs(box_crs), sf::st_crs(crs))) {
    xy <- sf::sf_project(sf::st_crs(crs), sf::st_crs(box_crs), xy)
  }
  DT[xy[, 1] >= params$xmin & xy[, 1] <= params$xmax &
       xy[, 2] >= params$ymin & xy[, 2] <= params$ymax]
}

#' @title Cleaning step: drop fixes near known false locations
#' @description For example fixes recorded at a field office before a collar was
#'   deployed (in NMC, the Fort Nelson office).
#' @inheritParams clean_bounds
#' @param params list of points, each with `lon`, `lat`, `radius_m` (and an optional `name`).
#' @return data.table.
#' @author Julie W. Turner
clean_exclude_points <- function(DT, params, crs) {
  for (pt in params) {
    xy <- sf::sf_project(sf::st_crs('EPSG:4326'), sf::st_crs(crs), matrix(c(pt$lon, pt$lat), ncol = 2))
    DT <- DT[sqrt((x_ - xy[1, 1])^2 + (y_ - xy[1, 2])^2) > pt$radius_m]
  }
  DT
}

#' @title Cleaning step: drop fixes outside deployment dates
#' @description Uses the `deploy_on_timestamp` and `deploy_off_timestamp`
#'   columns (as from Movebank) when they exist; fixes with no dates are kept.
#' @inheritParams clean_bounds
#' @param params not used.
#' @return data.table.
#' @author Julie W. Turner
clean_deployment <- function(DT, params, crs) {
  if ('deploy_on_timestamp' %in% names(DT)) {
    DT <- DT[is.na(deploy_on_timestamp) | t_ >= deploy_on_timestamp]
  }
  if ('deploy_off_timestamp' %in% names(DT)) {
    DT <- DT[is.na(deploy_off_timestamp) | t_ <= deploy_off_timestamp]
  }
  DT
}

#' @title Cleaning step: drop the days just after capture
#' @description Animals may move unusually just after being collared. Drops
#'   fixes in the first `days` of each animal's data (as
#'   `amt::remove_capture_effect()`).
#' @inheritParams clean_bounds
#' @param params named list: `days`.
#' @return data.table.
#' @author Julie W. Turner
clean_capture_effect <- function(DT, params, crs) {
  DT[DT[, .I[t_ >= min(t_) + params$days * 86400], by = id]$V1]
}

#' @title Cleaning step: drop low-quality fixes
#' @description Drops fixes whose dilution of precision (DOP) is at or above
#'   `max_dop`. Fixes with no DOP are kept when `keep_missing` is TRUE (many
#'   collars don't record it).
#' @inheritParams clean_bounds
#' @param params named list: `column`, `max_dop`, `keep_missing`.
#' @return data.table.
#' @author Julie W. Turner
clean_dop <- function(DT, params, crs) {
  if (!params$column %in% names(DT)) {
    warning('[clean dop] no column "', params$column, '"; step skipped')
    return(DT)
  }
  dop <- as.numeric(DT[[params$column]])
  DT[(is.na(dop) & isTRUE(params$keep_missing)) | (!is.na(dop) & dop < params$max_dop)]
}

#' @title Cleaning step: drop near-duplicate fixes
#' @description Among fixes closer in time than `gamma_minutes`, keeps the one
#'   with the best DOP (`amt::flag_duplicates()`). Fixes with no DOP are ranked last.
#' @inheritParams clean_bounds
#' @param params named list: `gamma_minutes`, `dop_column`.
#' @return data.table.
#' @author Julie W. Turner
clean_duplicates <- function(DT, params, crs) {
  dop <- if (params$dop_column %in% names(DT)) as.numeric(DT[[params$dop_column]]) else rep(1, nrow(DT))
  DT[, .dop := data.table::fifelse(is.na(dop), 99, dop)]
  DT <- drop_amt_flagged(DT, function(trk) {
    amt::flag_duplicates(trk, gamma = lubridate::minutes(params$gamma_minutes), DOP = '.dop')
  }, 'duplicate_')
  DT[, .dop := NULL]
  DT
}

#' @title Cleaning step: drop unrealistically fast steps
#' @description Flags fixes reached by a step whose squared displacement rate is
#'   above `delta` (`amt::flag_fast_steps()`, rate in m^2/s).
#' @inheritParams clean_bounds
#' @param params named list: `delta`.
#' @return data.table.
#' @author Julie W. Turner
clean_fast_steps <- function(DT, params, crs) {
  drop_amt_flagged(DT, function(trk) amt::flag_fast_steps(trk, delta = params$delta), 'fast_step_')
}

#' @title Cleaning step: drop fast out-and-back spikes
#' @description `amt::flag_roundtrips()`: a fix whose steps in and out are both
#'   faster than `delta / epsilon` and return near the start.
#' @inheritParams clean_bounds
#' @param params named list: `delta`, `epsilon`.
#' @return data.table.
#' @author Julie W. Turner
clean_roundtrips <- function(DT, params, crs) {
  drop_amt_flagged(DT, function(trk) {
    amt::flag_roundtrips(trk, delta = params$delta, epsilon = params$epsilon)
  }, 'fast_roundtrip_')
}

#' @title Cleaning step: drop clusters from dropped or dead collars
#' @description `amt::flag_defunct_clusters()`, run on each animal: at least `eta`
#'   fixes within `zeta` metres lasting at least `theta_hours` at the end of a track.
#' @inheritParams clean_bounds
#' @param params named list: `zeta`, `eta`, `theta_hours`.
#' @return data.table.
#' @author Julie W. Turner
clean_clusters <- function(DT, params, crs) {
  drop_amt_flagged(DT, function(trk) {
    amt::flag_defunct_clusters(trk, zeta = params$zeta, eta = params$eta,
                               theta = lubridate::hours(params$theta_hours))
  }, 'defunct_cluster_')
}

#' @title Cleaning step: drop steps faster than a speed limit
#' @description Quick alternative to `fast_steps`: drops fixes reached at more
#'   than `max_m_per_hr` metres per hour.
#' @inheritParams clean_bounds
#' @param params named list: `max_m_per_hr`.
#' @return data.table.
#' @details Adapted from caribouLocPrep `filter_fast_steps()`.
#' @author Julie W. Turner
clean_speed <- function(DT, params, crs) {
  DT[, .speed := sqrt((x_ - data.table::shift(x_))^2 + (y_ - data.table::shift(y_))^2) /
       as.numeric(difftime(t_, data.table::shift(t_), units = 'hours')), by = id]
  DT <- DT[is.na(.speed) | .speed <= params$max_m_per_hr]
  DT[, .speed := NULL]
  DT
}

#' @title Cleaning step: drop animals with too few fixes
#' @inheritParams clean_bounds
#' @param params named list: `n`, the fewest fixes an animal may have.
#' @return data.table.
#' @details Adapted from NMC_CEtool `make_unique_complete()`.
#' @author Julie W. Turner
clean_min_points <- function(DT, params, crs) {
  DT[, .n := .N, by = id]
  DT <- DT[.n >= params$n]
  DT[, .n := NULL]
  DT
}
