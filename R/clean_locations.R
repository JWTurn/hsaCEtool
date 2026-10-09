# === Cleaning GPS locations -------------------------------------------------
# Each cleaning step is a function clean_<step>(DT, params, crs) that takes and
# returns a data.table of standard locations (id, t_, x_, y_). Steps are run in
# the order set in `locations: cleaning: steps:`. A project can add its own step
# in config/<project>.R as cfg$locations$cleaning$custom$<step> <- function(DT, params, crs) ...
# Adapted from NMC_CEtool scripts/2-clean-data.R (amt-based steps) and
# caribouLocPrep R/filter_*.R (speed, quick versions).

#' @title Clean GPS locations
#' @description Removes exact duplicates (same animal, time and position),
#'   then runs each cleaning step in turn and reports how many fixes each step
#'   removed. Fixes of one animal at the same time but different positions are
#'   left for the `duplicates` step, which keeps the one with the best DOP. If
#'   any remain after the steps (e.g. `duplicates` is not listed), the first of
#'   each is kept, with a warning, so that every animal has one fix per time.
#'
#'   Fixes with dates but no times (`date_only` TRUE, see
#'   [standardize_locations()]) are cleaned separately, with the steps in
#'   `date_only_steps` only (e.g. `bounds`, `exclude_points`); their log rows
#'   end in "(date only)". They are kept for the study area and removed
#'   before tracks are made (see [drop_date_only()]).
#' @param DT data.table of standard locations from [standardize_locations()].
#' @param cleaning named list. The `locations: cleaning:` settings (`steps`,
#'   `params`, optional `date_only_steps` and `custom`).
#' @param crs character. Project coordinate system.
#' @return data.table of cleaned locations, with attributes `cleaning_log`
#'   (fixes removed by each step) and `removed` (a data.table: `row`, the row
#'   number in `DT` of each removed fix, and `step`, the step that removed it).
#' @author Julie W. Turner
clean_locations <- function(DT, cleaning, crs) {
  DT <- data.table::copy(DT)
  DT[, .row := seq_len(.N)]
  data.table::setorder(DT, id, t_)
  removed <- list()
  log <- list()
  record <- function(step, before, after) {
    gone <- setdiff(before, after)
    removed[[length(removed) + 1]] <<- data.table::data.table(row = gone, step = rep(step, length(gone)))
    log[[length(log) + 1]] <<- data.table::data.table(step = step, removed = length(gone))
    message(sprintf('[clean %s] removed %s of %s fixes', step, length(gone), length(before)))
  }
  # named here, not looked up by name, so that targets reruns cleaning when a
  # step's code changes
  available <- list(bounds = clean_bounds, exclude_points = clean_exclude_points,
                    deployment = clean_deployment, source_duplicates = clean_source_duplicates,
                    capture_effect = clean_capture_effect, dop = clean_dop,
                    duplicates = clean_duplicates, fast_steps = clean_fast_steps,
                    roundtrips = clean_roundtrips, clusters = clean_clusters,
                    end_clusters = clean_end_clusters, speed = clean_speed,
                    min_points = clean_min_points)
  run_steps <- function(DT, steps, suffix = '') {
    for (step in unlist(steps)) {
      fun <- cleaning$custom[[step]] %||% available[[step]] %||%
        stop('Unknown cleaning step: ', step, '. Add it to clean_locations() or as a custom step.')
      before <- DT$.row
      DT <- data.table::as.data.table(fun(DT, cleaning$params[[step]], crs))
      if (!'.row' %in% names(DT)) stop('[clean ', step, '] the step dropped column .row; keep all columns')
      record(paste0(step, suffix), before, DT$.row)
    }
    DT
  }

  before <- DT$.row
  DT <- unique(DT, by = c('id', 't_', 'x_', 'y_'))
  record('exact_duplicates', before, DT$.row)

  has_dates <- 'date_only' %in% names(DT)
  if (has_dates) {
    DT[, date_only := date_only %in% TRUE]
    undated <- DT[(date_only)]
    DT <- DT[!(date_only)]
  }

  DT <- run_steps(DT, cleaning$steps)

  same_time <- duplicated(DT, by = c('id', 't_'))
  if (any(same_time)) {
    warning(sprintf('[clean] %s fixes share their animal and time with another fix; kept the first ',
                    sum(same_time)), '(list the duplicates step to choose by DOP)')
    before <- DT$.row
    DT <- DT[!same_time]
    record('same_time_duplicates', before, DT$.row)
  }

  if (has_dates && nrow(undated)) {
    undated <- run_steps(undated, cleaning$date_only_steps, ' (date only)')
    DT <- data.table::rbindlist(list(DT, undated), fill = TRUE)
    data.table::setorder(DT, id, t_)
  }

  removed <- data.table::rbindlist(removed)
  DT[, .row := NULL]
  data.table::setattr(DT, 'cleaning_log', data.table::rbindlist(log))
  data.table::setattr(DT, 'removed', removed)
  DT
}

#' @title Drop fixes that have dates but no times
#' @description Fixes from `date_only` sources help shape the study area but
#'   cannot be used for tracks or steps, so they are removed here, along with
#'   the `date_only` column.
#' @param DT data.table of cleaned locations from [clean_locations()].
#' @return data.table.
#' @author Julie W. Turner
drop_date_only <- function(DT) {
  if (!'date_only' %in% names(DT)) return(DT)
  DT <- DT[!(date_only %in% TRUE)]
  DT[, date_only := NULL]
  DT[]
}

#' @title Check what cleaning did with fixes flagged as mortalities
#' @description Some sources (e.g. the old B.C. export) flag fixes recorded
#'   after an animal died (column `mortality`). Counts, for each source and
#'   animal, the flagged fixes that each cleaning step removed (e.g. `clusters`,
#'   which removes clusters from dropped or dead collars), and those `kept`.
#' @param raw data.table of combined locations before cleaning (`locations_raw`).
#' @param clean data.table from [clean_locations()], with its `removed` attribute.
#' @return data.table with columns `source`, `id`, `step` and `n` (counts only,
#'   no positions or times). Empty if `raw` has no `mortality` column.
#' @author Julie W. Turner
check_mortality <- function(raw, clean) {
  out <- data.table::data.table(source = character(), id = character(), step = character(), n = integer())
  if (!'mortality' %in% names(raw)) return(out)
  rows <- which(raw$mortality %in% TRUE)
  if (!length(rows)) return(out)
  removed <- attr(clean, 'removed')
  step <- removed$step[match(rows, removed$row)]
  flagged <- data.table::data.table(
    source = if ('source' %in% names(raw)) raw$source[rows] else NA_character_,
    id = as.character(raw$id[rows]),
    step = data.table::fifelse(is.na(step), 'kept', step)
  )
  flagged[, .(n = .N), by = .(source, id, step)][order(source, id, step)]
}

#' @title Combine the location sources
#' @description Makes columns plain so that sources combine: columns with
#'   units (e.g. Movebank's `gps_dop`) become plain numbers and factors (e.g.
#'   Movebank's `sex`) become text. Then stacks the sources and flags fixes an
#'   earlier-listed source also has ([flag_source_duplicates()]).
#' @param sources list of data.tables of standard locations, in the order the
#'   sources are listed.
#' @inheritParams flag_source_duplicates
#' @return data.table.
#' @author Julie W. Turner
combine_locations <- function(sources, params, crs) {
  sources <- purrr::map(sources, function(DT) {
    DT <- data.table::copy(DT)
    for (col in names(DT)) {
      if (inherits(DT[[col]], 'units')) {
        data.table::set(DT, j = col, value = as.numeric(DT[[col]]))
      } else if (is.factor(DT[[col]])) {
        data.table::set(DT, j = col, value = as.character(DT[[col]]))
      }
    }
    DT
  })
  flag_source_duplicates(data.table::rbindlist(sources, fill = TRUE), params, crs)
}

#' @title Flag fixes that another source also has
#' @description When sources overlap (e.g. the same collars on Movebank and in a
#'   government export), the same fix appears twice, possibly under different
#'   animal IDs. Fixes within `max_m` metres and `max_s` seconds of a fix from
#'   a source listed earlier in `locations: sources:` get that source's name in
#'   `dup_source`; the earlier source's copy is kept unflagged. Nothing is
#'   removed here: the cleaning step `source_duplicates` removes flagged fixes.
#' @param DT data.table of standard locations from all sources, in the order the
#'   sources are listed, with a `source` column.
#' @param params named list: `max_m` (default 2) and `max_s` (default 60).
#' @param crs character. Project coordinate system.
#' @return `DT` with column `dup_source` (NA if no earlier source has the fix).
#' @author Julie W. Turner
flag_source_duplicates <- function(DT, params, crs) {
  DT <- data.table::as.data.table(DT)
  DT[, dup_source := NA_character_]
  srcs <- unique(DT$source)
  if (length(srcs) < 2) return(DT[])
  ll <- sf::sf_project(sf::st_crs(crs), sf::st_crs('EPSG:4326'), as.matrix(DT[, .(x_, y_)]))
  fixes <- data.table::data.table(g = 1L, t = DT$t_, lat = ll[, 2], lon = ll[, 1])
  rank <- match(DT$source, srcs)
  pr <- match_fixes(fixes, fixes, params$max_s %||% 60, params$max_m %||% 2)
  pr <- pr[rank[ia] < rank[ib]][order(rank[ia], abs(dt))][!duplicated(ib)]
  DT[pr$ib, dup_source := DT$source[pr$ia]]
  message(sprintf('[source duplicates] %s fixes also in an earlier-listed source', nrow(pr)))
  DT[]
}

#' @title Cleaning step: drop fixes another source also has
#' @description Removes the fixes [flag_source_duplicates()] flagged.
#' @inheritParams clean_bounds
#' @param params not used.
#' @return data.table.
#' @author Julie W. Turner
clean_source_duplicates <- function(DT, params, crs) {
  if (!'dup_source' %in% names(DT)) return(DT)
  DT[is.na(dup_source)]
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
#'   with the best DOP, by the same rules as `amt::flag_duplicates()` (see
#'   [flag_duplicates_fast()]). Fixes with no DOP are ranked last.
#' @inheritParams clean_bounds
#' @param params named list: `gamma_minutes`, `dop_column`.
#' @return data.table.
#' @author Julie W. Turner
clean_duplicates <- function(DT, params, crs) {
  dop <- if (params$dop_column %in% names(DT)) as.numeric(DT[[params$dop_column]]) else rep(1, nrow(DT))
  DT[, .dop := data.table::fifelse(is.na(dop), 99, dop)]
  DT[, .flag := flag_duplicates_fast(t_, x_, y_, .dop, params$gamma_minutes * 60), by = id]
  DT <- DT[!(.flag)]
  DT[, c('.dop', '.flag') := NULL]
  DT
}

#' @title Flag near-duplicate fixes (fast version of amt::flag_duplicates)
#' @description Walks along one animal's track, sorted by time, as
#'   `amt::flag_duplicates()` does: at each fix, looks at the fixes not yet
#'   flagged from that time to `gamma_s` seconds later and keeps the one with
#'   the lowest DOP. On a tie it keeps the fix nearest the previous kept fix
#'   (at the start of the track, the first fix) and looks again from the same
#'   place. Gives the same result as amt, but works on plain vectors instead of
#'   subsetting the whole track at every fix, which took hours for NMC.
#' @param t POSIXct. Times, sorted.
#' @param x,y numeric. Coordinates.
#' @param dop numeric. DOP, with no NAs.
#' @param gamma_s numeric. Time window in seconds.
#' @return logical. TRUE for fixes to drop.
#' @keywords internal
#' @noRd
flag_duplicates_fast <- function(t, x, y, dop, gamma_s) {
  t <- as.numeric(t)
  n <- length(t)
  dead <- logical(n)
  i <- 1L      # position among the fixes not yet flagged, as in amt
  cur <- 1L
  while (!is.na(cur)) {
    k <- cur + 1L
    while (k <= n && t[k] <= t[cur] + gamma_s) k <- k + 1L
    w <- cur:(k - 1L)
    w <- w[!dead[w]]
    good <- w[dop[w] == min(dop[w])]
    tie <- length(good) > 1
    if (!tie) {
      best <- good
    } else if (i == 1L) {
      best <- 1L
    } else {
      p <- cur - 1L
      while (dead[p]) p <- p - 1L
      d <- sqrt((x[good] - x[p])^2 + (y[good] - y[p])^2)
      best <- good[which(d == min(d))][1]
    }
    dead[w[w != best]] <- TRUE
    if (dead[cur]) {
      cur <- cur + 1L
      while (cur <= n && dead[cur]) cur <- cur + 1L
    }
    if (!tie && cur <= n) {
      i <- i + 1L
      cur <- cur + 1L
      while (cur <= n && dead[cur]) cur <- cur + 1L
    }
    if (cur > n) cur <- NA_integer_
  }
  dead
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

#' @title Cleaning step: flag clusters from dropped or dead collars (amt)
#' @description Adds column `defunct_cluster_` from `amt::flag_defunct_clusters()`,
#'   run on each animal. Nothing is removed. amt finds the last run of at
#'   least `eta` steps no longer than `zeta` metres that lasts more than
#'   `theta_hours`, and flags everything after its start, even if the animal
#'   moves on later (with frequent fixes, a slow-moving animal can take short
#'   steps for days). Compare with `end_clusters` ([clean_end_clusters()]).
#' @inheritParams clean_bounds
#' @param params named list: `zeta`, `eta`, `theta_hours`.
#' @return data.table with column `defunct_cluster_`.
#' @author Julie W. Turner
clean_clusters <- function(DT, params, crs) {
  DT[, defunct_cluster_ := amt::flag_defunct_clusters(
    amt::make_track(.SD, x_, y_, t_, all_cols = TRUE), zeta = params$zeta, eta = params$eta,
    theta = lubridate::hours(params$theta_hours))$defunct_cluster_, by = id]
  DT
}

#' @title Cleaning step: flag a stationary end of track
#' @description Adds column `end_cluster_` (nothing is removed): TRUE for the
#'   end of an animal's track when, from some fix to the last one, every fix
#'   stays within `zeta` metres of the last fix for at least `theta_hours`, as
#'   for a dead animal or a dropped collar. A collar picked up and taken
#'   elsewhere (e.g. to an office) shows as two stationary stretches joined by
#'   a few fixes on the move, so after each flagged stretch the step looks up
#'   to `transit_fixes` fixes earlier for another one, up to `max_moves` times,
#'   and flags it and the fixes in between. The last stretch may also be
#'   followed by up to `transit_fixes` fixes (e.g. a collar picked up and
#'   switched off soon after), which are flagged too. An animal that moves on
#'   after staying put is never flagged.
#'
#'   Also adds `mortality_status`, comparing these flags with a `mortality`
#'   column from the data (e.g. the B.C. exports): `likely` (a mortality flag
#'   in a stationary end of track), `unknown` (a stationary end of track with
#'   no mortality flag), `flag only` (a mortality flag elsewhere in the track),
#'   and NA otherwise.
#' @inheritParams clean_bounds
#' @param params named list: `zeta`, `theta_hours`, `max_moves`, `transit_fixes`.
#' @return data.table with columns `end_cluster_` and `mortality_status`.
#' @author Julie W. Turner
clean_end_clusters <- function(DT, params, crs) {
  DT[, end_cluster_ := flag_end_clusters(t_, x_, y_, params$zeta, params$theta_hours * 3600,
                                         params$max_moves %||% 2, params$transit_fixes %||% 5), by = id]
  mort <- if ('mortality' %in% names(DT)) DT$mortality %in% TRUE else rep(FALSE, nrow(DT))
  DT[, mortality_status := data.table::fcase(mort & end_cluster_, 'likely',
                                             end_cluster_, 'unknown',
                                             mort, 'flag only',
                                             default = NA_character_)]
  DT
}

#' @title Flag stationary stretches at the end of one track
#' @inheritParams clean_end_clusters
#' @param t POSIXct. Times, sorted.
#' @param x,y numeric. Coordinates in metres.
#' @param zeta numeric. Distance in metres.
#' @param theta_s numeric. Shortest stationary stretch, in seconds.
#' @param max_moves integer. Most times the collar may have been moved.
#' @param transit_fixes integer. Most fixes while being moved.
#' @return logical.
#' @keywords internal
#' @noRd
flag_end_clusters <- function(t, x, y, zeta, theta_s, max_moves, transit_fixes) {
  t <- as.numeric(t)
  n <- length(t)
  flag <- logical(n)
  if (n < 2) return(flag)
  # first fix of the stationary stretch ending at fix e
  stretch_start <- function(e) {
    k <- e
    while (k > 1 && sqrt((x[k - 1] - x[e])^2 + (y[k - 1] - y[e])^2) <= zeta) k <- k - 1
    k
  }
  # the last stationary stretch may be followed by up to transit_fixes fixes
  # (e.g. a collar picked up and switched off soon after)
  found <- FALSE
  for (e in n:max(2, n - transit_fixes)) {
    k <- stretch_start(e)
    if (t[e] - t[k] >= theta_s) {
      found <- TRUE
      break
    }
  }
  if (!found) return(flag)
  flag[k:n] <- TRUE
  moves <- 0
  while (moves < max_moves) {
    found <- FALSE
    for (g in 0:transit_fixes) {
      e2 <- k - 1 - g
      if (e2 < 2) break
      k2 <- stretch_start(e2)
      if (t[e2] - t[k2] >= theta_s) {
        flag[k2:(k - 1)] <- TRUE
        k <- k2
        found <- TRUE
        break
      }
    }
    if (!found) break
    moves <- moves + 1
  }
  flag
}

#' @title Count fixes flagged as possible mortalities or dropped collars
#' @description For each source, counts the cleaned fixes (and animals)
#'   flagged by `clusters` (`defunct_cluster_`), by `end_clusters`
#'   (`end_cluster_`), by both, and with a `mortality` flag from the data
#'   (e.g. the old B.C. export), alone and together with each rule, and each
#'   `mortality_status` (see [clean_end_clusters()]). Counts
#'   only, no positions or times. Flags that are missing count as FALSE.
#' @param clean data.table from [clean_locations()].
#' @return data.table with one row per source and flag.
#' @author Julie W. Turner
summarize_flags <- function(clean) {
  get_flag <- function(col) if (col %in% names(clean)) clean[[col]] %in% TRUE else rep(FALSE, nrow(clean))
  amt_f <- get_flag('defunct_cluster_')
  end_f <- get_flag('end_cluster_')
  mort <- get_flag('mortality')
  flags <- list(`amt clusters` = amt_f, `end clusters` = end_f, `both rules` = amt_f & end_f,
                mortality = mort, `mortality and amt clusters` = mort & amt_f,
                `mortality and end clusters` = mort & end_f)
  src <- if ('source' %in% names(clean)) clean$source else NA_character_
  if ('mortality_status' %in% names(clean)) {
    for (s in c('likely', 'unknown', 'flag only')) {
      flags[[paste('mortality_status:', s)]] <- clean$mortality_status %in% s
    }
  }
  data.table::rbindlist(purrr::imap(flags, function(f, nm) {
    data.table::data.table(source = src, id = clean$id, f = f)[
      , .(flag = nm, fixes = sum(f), animals = data.table::uniqueN(id[f]), of_fixes = .N), by = source]
  }))
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
