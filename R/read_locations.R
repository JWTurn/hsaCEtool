#' @title Read caribou locations from a file
#' @description Reads a `.rds`, `.csv`, `.shp`, `.gpkg` or `.geojson` of GPS
#'   fixes and puts it in the pipeline's standard form with
#'   `standardize_locations()`.
#' @param file character. Path from [get_source()] (only the first is read).
#' @param src named list. This source's settings; its `columns`, `input_crs` and
#'   `name` override the `locations:` defaults, and its `fun` (if set) reads the file.
#' @param loc_cfg named list. The `locations:` settings.
#' @param crs character. Project coordinate system.
#' @return data.table of locations.
#' @author Julie W. Turner
#' @export
read_locations_file <- function(file, src, loc_cfg, crs) {
  file <- file[1]
  obj <- if (is.function(as_reader(src$fun))) as_reader(src$fun)(file) else switch(tolower(tools::file_ext(file)),
                rds = readRDS(file),
                csv = data.table::fread(file),
                shp = , gpkg = , geojson = sf::st_read(file, quiet = TRUE),
                stop('Cannot read locations from a .', tools::file_ext(file), ' file: ', file))
  locations_from_object(obj, src, loc_cfg, crs)
}

#' @title Download caribou locations from a URL
#' @description Downloads and reads a locations file with
#'   [reproducible::prepInputs()], e.g. from a Google Drive link. The file is
#'   kept in `paths$inputs`, so it is only downloaded again if that copy is
#'   missing or incomplete, or if the source has `refresh: true` (see
#'   `prep_inputs_fresh()`). The function that reads it is the source's `fun`
#'   setting or, if not set, chosen from `target_file`'s type: `readRDS` for
#'   .rds, `data.table::fread` for .csv, `sf::st_read` for .shp/.gpkg/.geojson.
#' @inheritParams read_locations_file
#' @param src named list. `url`, `target_file`, optional `archive`, `fun` and `refresh`.
#' @param paths named list. The `paths:` settings.
#' @return data.table of locations.
#' @author Julie W. Turner
#' @export
download_locations <- function(src, loc_cfg, crs, paths) {
  fun <- prep_reader(src$fun %||% switch(tolower(tools::file_ext(src$target_file %||% '')),
                               rds = 'readRDS',
                               csv = 'data.table::fread',
                               shp = , gpkg = , geojson = 'sf::st_read',
                               stop('Set target_file (or fun) for ', src$url, call. = FALSE)))
  reproducible::checkPath(paths$inputs, create = TRUE)
  get <- if (isTRUE(src$refresh)) prep_inputs_fresh else reproducible::prepInputs
  obj <- get(url = src$url, targetFile = src$target_file, archive = src$archive,
             destinationPath = paths$inputs, fun = fun)
  locations_from_object(obj, src, loc_cfg, crs)
}

#' @title Turn a table or spatial object of locations into standard form
#' @description Spatial points (sf) have their coordinates and coordinate
#'   system taken from the geometry; tables use the `columns` settings.
#' @inheritParams read_locations_file
#' @param obj data.frame or sf object of locations.
#' @return data.table of locations.
#' @keywords internal
#' @noRd
locations_from_object <- function(obj, src, loc_cfg, crs) {
  if (inherits(obj, 'sf')) {
    xy <- sf::st_coordinates(obj)
    src$input_crs <- sf::st_crs(obj)$wkt
    obj <- data.table::as.data.table(sf::st_drop_geometry(obj))
    obj[, `:=`(.x = xy[, 1], .y = xy[, 2])]
    src$columns <- utils::modifyList(src$columns %||% loc_cfg$columns, list(x = '.x', y = '.y'))
  }
  standardize_locations(obj, src, loc_cfg, crs)
}

#' @title Download a Movebank study and save it
#' @description Downloads one Movebank study with \pkg{move2} and saves it,
#'   unchanged, as `<inputs>/movebank/<name>.rds`. Kept apart from
#'   [read_movebank()] so that changes to how locations are read or cleaned
#'   don't download the study again; only a change to the study, or
#'   `refresh: true`, does.
#'   The Movebank password is never stored in the settings: it is read from the
#'   system keyring, where it is saved once with
#'   `move2::movebank_store_credentials(<user>)`. You must have accepted the
#'   study's terms of use on movebank.org. If Movebank asks for fewer requests
#'   (its rate limit), the download is tried again after 1 and then 5 minutes.
#' @param src named list. `study` (name or ID) and optional `name`.
#' @param paths named list. The `paths:` settings (`inputs`).
#' @return character. Path to the saved file.
#' @details Adapted from NMC_CEtool `scripts/1-DL-data.R` and caribouLocPrep `dataPrep_YT()`.
#' @author Julie W. Turner
#' @export
download_movebank <- function(src, paths) {
  rlang::check_installed('move2', reason = 'to download Movebank studies.')
  options(move2_movebank_key_name = 'movebank')
  for (wait in c(0, 60, 300)) {
    if (wait) {
      message('[movebank ', src$name %||% src$study, '] rate limit reached; retrying in ', wait, ' s')
      Sys.sleep(wait)
    }
    dat <- tryCatch(move2::movebank_download_study(src$study, removeDuplicatedTimestamps = TRUE),
                    error = function(e) e)
    if (!inherits(dat, 'error') || !grepl('rate limit', conditionMessage(dat))) break
  }
  if (inherits(dat, 'error')) stop(dat)
  dir <- reproducible::checkPath(file.path(paths$inputs, 'movebank'), create = TRUE)
  file <- file.path(dir, paste0(src$name %||% src$study, '.rds'))
  saveRDS(dat, file)
  file
}

#' @title Read a saved Movebank study
#' @description Reads a study saved by [download_movebank()], drops records
#'   without a position, adds each deployment's animal ID, sex, start/end times,
#'   duty cycle, end type, study site, group and comments (from the study's
#'   deployment, or track, table; the last three can hold the herd), and
#'   puts it in standard form.
#' @inheritParams read_locations_file
#' @param file character. Path from [download_movebank()].
#' @param src named list. `study`, optional `name`, and `id_from_deployment`
#'   (TRUE to use the deployment ID as animal ID, as needed for e.g. the
#'   Southern Lakes and Clear Creek studies).
#' @return data.table of locations.
#' @author Julie W. Turner
#' @export
read_movebank <- function(file, src, loc_cfg, crs) {
  rlang::check_installed('move2', reason = 'to read Movebank downloads.')
  dat <- readRDS(file)
  dat <- dat[!sf::st_is_empty(dat), ]   # records without a position

  xy <- sf::st_coordinates(dat)
  DT <- data.table::as.data.table(sf::st_drop_geometry(dat))
  DT[, `:=`(longitude = xy[, 1], latitude = xy[, 2])]
  track_col <- move2::mt_track_id_column(dat)
  meta <- data.table::as.data.table(sf::st_drop_geometry(move2::mt_track_data(dat)))
  meta_cols <- intersect(c(track_col, 'deployment_id', 'individual_local_identifier', 'tag_local_identifier',
                           'sex', 'deploy_on_timestamp', 'deploy_off_timestamp', 'duty_cycle',
                           'deployment_end_type', 'individual_number_of_deployments',
                           'study_site', 'group_id', 'individual_comments'), names(meta))
  meta <- meta[, unique(meta_cols), with = FALSE]
  DT <- merge(DT[, setdiff(names(DT), setdiff(names(meta), track_col)), with = FALSE], meta,
              by = track_col, all.x = TRUE)

  DT[, id := if (isTRUE(src$id_from_deployment) || !'individual_local_identifier' %in% names(DT)) {
    as.character(deployment_id)
  } else {
    data.table::fifelse(is.na(individual_local_identifier), as.character(deployment_id),
                        as.character(individual_local_identifier))
  }]

  src$columns <- list(id = 'id', datetime = 'timestamp', x = 'longitude', y = 'latitude')
  src$input_crs <- 'EPSG:4326'
  src$name <- src$name %||% src$study
  standardize_locations(DT, src, loc_cfg, crs)
}

#' @title Read premade cleaned locations
#' @description Reads cleaned locations saved from the `locations_clean`
#'   target (a .rds data.table with `id`, `t_`, `x_`, `y_` and `source`), used
#'   instead of reading and cleaning `locations: sources:` when
#'   `locations: premade:` is set. Everything saved with it is kept: the
#'   `source` of each fix, the flag and `mortality_status` columns, any
#'   `date_only` fixes, and the `cleaning_log`. Coordinates are taken to be in
#'   the source's `input_crs` (by default the project's) and are projected to
#'   the project's coordinate system if that differs.
#' @param file character. Path from [get_source()].
#' @param src named list. The `locations: premade:` source.
#' @param crs character. Project coordinate system.
#' @return data.table of cleaned locations.
#' @author Julie W. Turner
#' @export
read_premade_locations <- function(file, src, crs) {
  file <- file[1]
  if (tolower(tools::file_ext(file)) != 'rds') {
    stop('locations: premade: must be a .rds file saved from locations_clean, not ', file, call. = FALSE)
  }
  DT <- readRDS(file)
  data.table::setDT(DT)
  missing <- setdiff(c('id', 't_', 'x_', 'y_', 'source'), names(DT))
  if (length(missing)) {
    stop('locations: premade: columns not found: ', paste(missing, collapse = ', '),
         '. Use a file saved from locations_clean.', call. = FALSE)
  }
  input_crs <- src$input_crs %||% crs
  if (!identical(sf::st_crs(input_crs), sf::st_crs(crs))) {
    xy <- sf::sf_project(sf::st_crs(input_crs), sf::st_crs(crs), as.matrix(DT[, .(x_, y_)]))
    DT[, `:=`(x_ = xy[, 1], y_ = xy[, 2])]
  }
  DT[]
}

#' @title Put locations in the pipeline's standard form
#' @description Renames the ID, time and coordinate columns to `id`, `t_`, `x_`
#'   and `y_` (the names \pkg{amt} uses), makes `id` text and `t_` a date-time
#'   in UTC,
#'   projects coordinates to the project's coordinate system, and adds a
#'   `source` column naming the data set.
#'
#'   Times stored as text are read in the time zone `tz` (the source's, else
#'   the `locations:` default). If `columns` also names an `hour` column,
#'   `datetime` is taken as the date and `hour` as the whole hour of the day,
#'   in time zone `tz` (e.g. `America/Whitehorse` for Yukon local time,
#'   which follows daylight saving until November 2020), and converted to UTC
#'   with `local_to_utc()`.
#'
#'   A source with `clock_time: true` has times that are clock time in `tz`
#'   whatever zone they are labelled with (e.g. local times written with a
#'   trailing `Z`, which `data.table::fread()` reads as UTC). The label is
#'   ignored and the clock time converted from `tz` to UTC with
#'   `local_to_utc()`.
#'
#'   A source with `date_only: true` has dates but no times of day (e.g. the
#'   Chisana and Redstone files). Its fixes are given noon UTC on their date
#'   and a `date_only` column (TRUE): they are cleaned with
#'   `cleaning: date_only_steps:` only, help shape the study area, and are left
#'   out of tracks and steps (see [clean_locations()]).
#' @inheritParams read_locations_file
#' @param DT data.frame of locations.
#' @return data.table.
#' @author Julie W. Turner
#' @keywords internal
#' @noRd
standardize_locations <- function(DT, src, loc_cfg, crs) {
  DT <- data.table::as.data.table(DT)
  cols <- src$columns %||% loc_cfg$columns
  missing <- setdiff(unlist(cols[c('id', 'datetime', 'x', 'y', 'hour')]), names(DT))
  if (length(missing)) {
    stop('Location columns not found: ', paste(missing, collapse = ', '),
         '. Set `columns:` in the settings to the names used in this data set.')
  }

  keep <- unlist(loc_cfg$keep_columns)
  if (length(keep)) {
    DT <- DT[, unique(c(unlist(cols[c('id', 'datetime', 'x', 'y', 'hour')]), intersect(keep, names(DT)))), with = FALSE]
  }
  # avoid clashes with columns that already use the standard names
  std <- c(id = 'id', datetime = 't_', x = 'x_', y = 'y_')
  clash <- setdiff(intersect(std, names(DT)), unlist(cols[c('id', 'datetime', 'x', 'y')]))
  if (length(clash)) data.table::setnames(DT, clash, paste0(clash, '_input'))
  data.table::setnames(DT, unlist(cols[c('id', 'datetime', 'x', 'y')]), std)

  DT[, id := as.character(id)]
  tz <- src$tz %||% loc_cfg$tz %||% 'UTC'
  if (isTRUE(src$date_only)) {
    # only the date is known: noon UTC stands in for the time of day
    DT[, t_ := as.POSIXct(format(as.Date(t_)), tz = 'UTC') + 12 * 3600]
  } else if (!is.null(cols$hour)) {
    # a date column plus a whole-hour column, as clock time in tz
    DT[, t_ := as.POSIXct(format(as.Date(t_)), tz = 'UTC') + as.numeric(DT[[cols$hour]]) * 3600]
    if (tz != 'UTC') DT[, t_ := local_to_utc(t_, tz)]
  } else if (isTRUE(src$clock_time)) {
    # clock time in tz, whatever zone the text or date-time is labelled with
    DT[, t_ := clock_digits(t_)]
    if (tz != 'UTC') DT[, t_ := local_to_utc(t_, tz)]
  } else if (!inherits(DT$t_, 'POSIXct')) {
    DT[, t_ := as.POSIXct(t_, tz = tz)]
  }
  attr(DT$t_, 'tzone') <- 'UTC'
  DT <- DT[!is.na(x_) & !is.na(y_) & !is.na(t_)]

  input_crs <- src$input_crs %||% loc_cfg$input_crs
  if (!identical(sf::st_crs(input_crs), sf::st_crs(crs))) {
    xy <- sf::sf_project(sf::st_crs(input_crs), sf::st_crs(crs), as.matrix(DT[, .(x_, y_)]))
    DT[, `:=`(x_ = xy[, 1], y_ = xy[, 2])]
  }
  DT[, source := src$name %||% NA_character_]
  if (isTRUE(src$date_only)) DT[, date_only := TRUE]
  DT[]
}

#' @title Clock time of date-times or text, stored as UTC
#' @description Drops any zone label (a trailing `Z` or UTC offset in text,
#'   the time zone of a date-time) and keeps the clock time, stored as UTC,
#'   ready for `local_to_utc()`.
#' @param x POSIXct or character (e.g. `2020-06-01T08:00:00Z`).
#' @return POSIXct in UTC.
#' @keywords internal
#' @noRd
clock_digits <- function(x) {
  if (inherits(x, 'POSIXct')) return(as.POSIXct(format(x, '%Y-%m-%d %H:%M:%OS6'), tz = 'UTC'))
  x <- sub('(Z|[+-][0-9]{2}:?[0-9]{2})$', '', trimws(as.character(x)))
  as.POSIXct(sub('T', ' ', x, fixed = TRUE), tz = 'UTC', tryFormats = c('%Y-%m-%d %H:%M:%OS', '%Y-%m-%d %H:%M', '%Y-%m-%d'))
}

#' @title Convert local clock times to UTC
#' @description For times read as if they were UTC but that are really the
#'   clock time in time zone `tz`, adds that zone's offset from UTC on each
#'   date (so daylight saving is followed). Clock times that do not exist (the
#'   hour skipped when clocks go forward) are read as daylight time, and those
#'   that occur twice (when clocks go back) as the first, daylight-time one.
#' @param x POSIXct. Clock times, stored as UTC.
#' @param tz character. Time zone the clock times are in.
#' @return POSIXct in UTC.
#' @keywords internal
#' @noRd
local_to_utc <- function(x, tz) {
  off <- function(t) as.numeric(t) - as.numeric(as.POSIXct(format(t, tz = tz), tz = 'UTC'))
  t0 <- x + off(x)
  x + off(t0)
}
