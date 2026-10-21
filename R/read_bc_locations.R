# === B.C. caribou telemetry ------------------------------------------------------
# The B.C. government's caribou telemetry comes as several exports that overlap:
# a recent shapefile (the base), an older csv with fixes and mortality flags the
# recent one lacks, and optionally a cleaned csv used only to correct times.
# Their times are not what they say: collar temperatures (warmest in the
# afternoon) show that the recent file's clock text is UTC despite its -0700 or
# -0800 labels, and that the older file's times are UTC plus the Pacific offset.
# The comparison behind these rules is summarised in
# inputs/bc_compare/BC_telemetry_comparison.md (kept with the downloads, not in git).

#' @title Combine the B.C. caribou telemetry exports
#' @description Builds one table of B.C. caribou fixes:
#'   \enumerate{
#'     \item **New file (the base):** the recent shapefile. The clock time in
#'       its `Date` text is read as UTC; the UTC offset some rows carry (e.g.
#'       -0700) is wrong and ignored.
#'     \item **Reference file (optional):** where it has the same collar's fix at
#'       the same position exactly 7 or 8 hours earlier than the new file's time
#'       (and none at the new file's time), its time replaces the new file's.
#'       This corrects fixes whose clock text is local time plus the offset.
#'     \item **Old file (optional):** caribou fixes from an older export. Its
#'       times are UTC plus the `local_tz` offset (7 or 8 hours), so the offset is
#'       subtracted. Duplicates are removed. An old fix matching a new fix (same herd, within
#'       `match_m`, same time) gives the new fix its mortality flag. An old fix at
#'       the same position as a new fix but at a different time, within
#'       `max_shift_h` (in the comparison, Lotek collars), also gives its flag,
#'       and its time replaces the new file's: in the comparison the old times
#'       were consistent, while the new file often had two positions at one time.
#'       The remaining old fixes are added.
#'   }
#'   Each row records `bc_file` (`new` or `old`) and `time_note`; `time_clash`
#'   is TRUE where the animal has another fix at the same time (left for the
#'   cleaning steps to sort out).
#' @param new_file character. The recent shapefile (e.g. `Caribou_Telem.shp`),
#'   from [get_source()]; only the first path is read.
#' @param old_file character or NULL. The older csv (e.g. `BC_NMC_Telemetry.csv`).
#' @param ref_file character or NULL. The csv used to correct times (e.g.
#'   `nmc_combined_data_cleaned.csv`), with `Collar_ID`, `Latitude`,
#'   `Longitude` and `timestamp` (UTC; its `Acquisition_Date` is shifted like
#'   the old file's).
#' @inheritParams read_locations_file
#' @param src named list. This source's settings: `name`, and in `params`:
#'   `local_tz` (the zone whose offset the old file's times carry, default
#'   America/Vancouver), `match_m` (default 2), `max_shift_h`
#'   (default 48) and `species` (old file species kept, default Caribou).
#' @return data.table of locations in standard form (see
#'   `standardize_locations()`), with attribute `merge_log`: a table counting
#'   each step.
#' @author Julie W. Turner
#' @export
read_bc_locations <- function(new_file, old_file, ref_file, src, loc_cfg, crs) {
  p <- utils::modifyList(list(local_tz = 'America/Vancouver', match_m = 2, max_shift_h = 48,
                              species = 'Caribou'), src$params %||% list())
  log <- list()
  note <- function(step, n) log[[length(log) + 1]] <<- data.table::data.table(step = step, n = n)

  # new file: the base
  nw <- sf::st_read(new_file[1], quiet = TRUE)
  nw <- sf::st_transform(nw, 4326)
  xy <- sf::st_coordinates(nw)
  nw <- data.table::as.data.table(sf::st_drop_geometry(nw))
  need_cols(nw, c('Pop_Unt', 'WLH_ID', 'Anml_ID', 'Cllr_ID', 'Vendor', 'Date'), new_file[1])
  nw[, `:=`(lon = xy[, 1], lat = xy[, 2])]
  # the clock text is UTC; any offset after it is ignored
  has_off <- grepl(' [+-][0-9]{4}$', nw$Date)
  t <- as.POSIXct(sub(' [+-][0-9]{4}$', '', nw$Date), format = '%Y-%m-%d %H:%M:%OS', tz = 'UTC')
  nw[, `:=`(t = t, bc_file = 'new', time_note = 'clock time as UTC', mortality = NA)]
  note('new file rows', nrow(nw))
  note('new: dates with a (wrong, ignored) UTC offset', sum(has_off))
  if (anyNA(nw$t)) stop('Could not read ', sum(is.na(nw$t)), ' dates in ', new_file[1], call. = FALSE)

  # reference file: correct times 7 or 8 h late
  if (length(ref_file)) {
    ref <- data.table::fread(ref_file[1])
    need_cols(ref, c('Collar_ID', 'Latitude', 'Longitude', 'timestamp'), ref_file[1])
    ref[, timestamp := as_utc(timestamp)]
    pr <- match_fixes(nw[, .(g = as.character(Cllr_ID), t, lat, lon)],
                      ref[, .(g = as.character(Collar_ID), t = timestamp, lat = Latitude, lon = Longitude)],
                      9 * 3600, p$match_m)
    exact <- unique(pr[abs(dt) < 60]$ia)
    fix <- pr[!ia %in% exact & (abs(dt + 7 * 3600) < 60 | abs(dt + 8 * 3600) < 60)]
    fix <- fix[order(abs(dt))][!duplicated(ia)]
    nw[fix$ia, `:=`(t = ref$timestamp[fix$ib], time_note = 'time corrected from reference file')]
    note('new: times corrected from the reference file', nrow(fix))
  }

  out <- nw[, .(id = data.table::fcoalesce(Anml_ID, paste0('collar_', Cllr_ID)),
                t, lon, lat, herd = Pop_Unt, wlh_id = WLH_ID, animal_id = Anml_ID,
                collar_id = as.character(Cllr_ID), vendor = tolower(Vendor),
                sex = if ('Sex' %in% names(nw)) Sex else NA_character_,
                gps_dop = if ('DOP' %in% names(nw)) DOP else NA_real_,
                elevation = if ('Elevatn' %in% names(nw)) Elevatn else NA_real_,
                secured = if ('SECURED' %in% names(nw)) SECURED else NA_character_,
                mortality, bc_file, time_note)]

  # old file: mortality flags and fixes the new file lacks
  if (length(old_file)) {
    old <- data.table::fread(old_file[1])
    need_cols(old, c('population_unit', 'species', 'wlh_id', 'deviceid', 'latitude', 'longitude',
                     'acquisition_date'), old_file[1])
    note('old file rows', nrow(old))
    old <- old[species %in% unlist(p$species)]
    note(paste('old: rows of', paste(unlist(p$species), collapse = ', ')), nrow(old))
    old[, acquisition_date := remove_utc_offset(as_utc(acquisition_date), p$local_tz)]
    old <- unique(old, by = c('acquisition_date', 'latitude', 'longitude'))
    note('old: distinct fixes (duplicates removed)', nrow(old))
    if (!'mortality' %in% names(old)) old[, mortality := NA]
    pr <- match_fixes(old[, .(g = population_unit, t = acquisition_date, lat = latitude, lon = longitude)],
                      out[, .(g = herd, t, lat, lon)], p$max_shift_h * 3600, p$match_m)
    pr <- pr[order(abs(dt))][!duplicated(ia)]
    same <- pr[abs(dt) < 60]
    shifted <- pr[abs(dt) >= 60 & !ib %in% same$ib][order(abs(dt))][!duplicated(ib)]
    note('old: same fix in new', nrow(same))
    note('old: same position in new at another time (old time used)', nrow(shifted))
    flag <- rbind(same, shifted)[, .(mort = old$mortality[ia]), by = ib][, .(mort = if (all(is.na(mort))) NA else any(mort, na.rm = TRUE)), by = ib]
    out[flag$ib, mortality := flag$mort]
    out[shifted$ib, `:=`(t = old$acquisition_date[shifted$ia],
                         time_note = 'time from old file (new file had this position at another time)')]
    note('new: fixes given a mortality flag from old', sum(!is.na(flag$mort)))

    add <- old[!seq_len(.N) %in% c(same$ia, shifted$ia)]
    # animal IDs as in the new file, through WLH_ID
    ids <- out[!is.na(wlh_id) & !is.na(animal_id), .N, by = .(wlh_id, animal_id)][order(-N)][!duplicated(wlh_id)]
    add[, wlh_id := data.table::fifelse(wlh_id == '', NA_character_, as.character(wlh_id))]
    add[, deviceid := data.table::fifelse(deviceid == '', NA_character_, as.character(deviceid))]
    add[, id := data.table::fcoalesce(ids$animal_id[match(wlh_id, ids$wlh_id)], wlh_id,
                                      data.table::fifelse(is.na(deviceid), NA_character_, paste0('collar_', deviceid)))]
    note('old: fixes not in new', nrow(add))
    note('old: fixes not in new without an animal or collar ID (dropped)', sum(is.na(add$id)))
    add <- add[!is.na(id)]
    out <- data.table::rbindlist(list(out, add[, .(
      id, t = acquisition_date, lon = longitude, lat = latitude, herd = population_unit,
      wlh_id, animal_id = ids$animal_id[match(wlh_id, ids$wlh_id)], collar_id = deviceid,
      vendor = if ('vendor' %in% names(add)) tolower(vendor) else NA_character_,
      elevation = if ('elevation' %in% names(add)) elevation else NA_real_,
      secured = if ('secure_ind' %in% names(add)) data.table::fifelse(secure_ind, 'Yes', 'No') else NA_character_,
      mortality, bc_file = 'old', time_note = 'only in old file')]), fill = TRUE)
  }
  out[vendor == '', vendor := NA_character_]
  out[, time_clash := .N > 1, by = .(id, t)]
  note('fixes sharing their animal and time with another fix (time_clash)', sum(out$time_clash))
  note('combined fixes', nrow(out))

  src$columns <- list(id = 'id', datetime = 't', x = 'lon', y = 'lat')
  src$input_crs <- 'EPSG:4326'
  src$name <- src$name %||% 'BC'
  out <- standardize_locations(out, src, loc_cfg, crs)
  data.table::setattr(out, 'merge_log', data.table::rbindlist(log))
  out
}

#' @title Pairs of fixes at the same position and close in time
#' @description For each fix in `a`, the fixes in `b` of the same group `g`
#'   within `max_m` metres and `window_s` seconds. Positions are compared on
#'   four grids of 0.001 degree cells offset by half a cell, so every pair
#'   closer than half a cell shares a cell on at least one of them.
#' @param a,b data.table with `g` (group, e.g. herd or collar), `t` (POSIXct),
#'   `lat` and `lon` (degrees).
#' @param window_s numeric. Largest time difference, in seconds.
#' @param max_m numeric. Largest distance, in metres.
#' @return data.table: `ia`, `ib` (row numbers in `a` and `b`), `dt` (time in
#'   `b` minus time in `a`, seconds) and `d` (metres).
#' @keywords internal
#' @noRd
match_fixes <- function(a, b, window_s, max_m) {
  cell <- 1e-3
  a <- a[, .(ia = .I, g, ta = as.numeric(t), la = lat, oa = lon)][!is.na(g)]
  b <- b[, .(ib = .I, g, tb = as.numeric(t), lb = lat, ob = lon)][!is.na(g)]
  a[, `:=`(lo = ta - window_s, hi = ta + window_s)]
  offsets <- list(c(0, 0), c(0, 0.5), c(0.5, 0), c(0.5, 0.5))
  pr <- data.table::rbindlist(purrr::map(offsets, function(o) {
    a[, `:=`(cy = floor(la / cell + o[1]), cx = floor(oa / cell + o[2]))]
    b[, `:=`(cy = floor(lb / cell + o[1]), cx = floor(ob / cell + o[2]))]
    b[a, on = .(g, cy, cx, tb >= lo, tb <= hi), nomatch = NULL,
      .(ia = i.ia, ib = x.ib, dt = x.tb - i.ta, d = metres_apart(i.la, i.oa, x.lb, x.ob))]
  }))
  unique(pr[d <= max_m], by = c('ia', 'ib'))
}

#' @title Distance in metres between nearby longitude/latitude points
#' @description Flat-earth approximation, accurate for points a few
#'   kilometres apart or less.
#' @keywords internal
#' @noRd
metres_apart <- function(lat1, lon1, lat2, lon2) {
  6371000 * pi / 180 * sqrt(((lon2 - lon1) * cos((lat1 + lat2) / 2 * pi / 180))^2 + (lat2 - lat1)^2)
}

#' @title Undo times that had a time zone's UTC offset added
#' @description For times stored as UTC plus the offset of `tz` at that time
#'   (e.g. 7 hours in B.C. daylight time, 8 in standard time), returns the UTC
#'   time, using the offset at the corrected time. In the hour after clocks
#'   spring forward a shifted time could come from either offset; the
#'   daylight-time one is used.
#' @param x POSIXct. Shifted times.
#' @param tz character. Time zone, e.g. America/Vancouver.
#' @return POSIXct in UTC.
#' @keywords internal
#' @noRd
remove_utc_offset <- function(x, tz) {
  off <- function(t) abs(as.numeric(as.POSIXct(format(t, tz = tz), tz = 'UTC')) - as.numeric(t))
  t0 <- x - off(x)
  x - off(t0)
}

#' @title Make a time column POSIXct in UTC
#' @keywords internal
#' @noRd
as_utc <- function(x) {
  if (!inherits(x, 'POSIXct')) x <- as.POSIXct(x, tz = 'UTC')
  attr(x, 'tzone') <- 'UTC'
  x
}

#' @title Stop if a table lacks columns
#' @keywords internal
#' @noRd
need_cols <- function(d, cols, file) {
  missing <- setdiff(cols, names(d))
  if (length(missing)) stop(basename(file), ' lacks columns: ', paste(missing, collapse = ', '), call. = FALSE)
}

