#' @title Read caribou locations from a file
#' @description Reads a `.rds`, `.csv`, `.shp`, `.gpkg` or `.geojson` of GPS
#'   fixes and puts it in the pipeline's standard form with
#'   [standardize_locations()].
#' @param file character. Path from [get_source()] (only the first is read).
#' @param src named list. This source's settings; its `columns`, `input_crs` and
#'   `name` override the `locations:` defaults.
#' @param loc_cfg named list. The `locations:` settings.
#' @param crs character. Project coordinate system.
#' @return data.table of locations.
#' @author Julie W. Turner
read_locations_file <- function(file, src, loc_cfg, crs) {
  file <- file[1]
  obj <- switch(tolower(tools::file_ext(file)),
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
#'   [prep_inputs_fresh()]). The function that reads it is the source's `fun`
#'   setting or, if not set, chosen from `target_file`'s type: `readRDS` for
#'   .rds, `data.table::fread` for .csv, `sf::st_read` for .shp/.gpkg/.geojson.
#' @inheritParams read_locations_file
#' @param src named list. `url`, `target_file`, optional `archive`, `fun` and `refresh`.
#' @param paths named list. The `paths:` settings.
#' @return data.table of locations.
#' @author Julie W. Turner
download_locations <- function(src, loc_cfg, crs, paths) {
  fun <- src$fun %||% switch(tolower(tools::file_ext(src$target_file %||% '')),
                             rds = 'readRDS',
                             csv = 'data.table::fread',
                             shp = , gpkg = , geojson = 'sf::st_read',
                             stop('Set target_file (or fun) for ', src$url, call. = FALSE))
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

#' @title Download caribou locations from Movebank
#' @description Downloads one Movebank study and its deployment table with
#'   \pkg{move2}, adds deployment start/end times, and puts it in standard form.
#'   The Movebank password is never stored in the settings: it is read from the
#'   system keyring, where it is saved once with
#'   `move2::movebank_store_credentials(<user>)`. You must have accepted the
#'   study's terms of use on movebank.org.
#' @inheritParams read_locations_file
#' @param src named list. `study` (name or ID), `user`, optional `name`, and
#'   `id_from_deployment` (TRUE to use the deployment ID as animal ID, as needed
#'   for e.g. the Southern Lakes and Clear Creek studies).
#' @return data.table of locations.
#' @details Adapted from NMC_CEtool `scripts/1-DL-data.R` and caribouLocPrep `dataPrep_YT()`.
#' @author Julie W. Turner
download_movebank <- function(src, loc_cfg, crs) {
  options(move2_movebank_key_name = 'movebank')
  dat <- move2::movebank_download_study(src$study, removeDuplicatedTimestamps = TRUE)
  meta <- data.table::as.data.table(move2::movebank_download_deployment(src$study))

  xy <- sf::st_coordinates(dat)
  DT <- data.table::as.data.table(sf::st_drop_geometry(dat))
  DT[, `:=`(longitude = xy[, 1], latitude = xy[, 2])]

  pick_id <- function(d) {
    if (isTRUE(src$id_from_deployment)) {
      as.character(d$deployment_id)
    } else {
      data.table::fifelse(is.na(d$individual_local_identifier),
                          as.character(d$deployment_id),
                          as.character(d$individual_local_identifier))
    }
  }
  DT[, id := pick_id(.SD)]
  meta[, id := pick_id(.SD)]
  meta_cols <- intersect(c('id', 'deploy_on_timestamp', 'deploy_off_timestamp', 'duty_cycle',
                           'deployment_end_type'), names(meta))
  DT <- merge(DT, unique(meta[, ..meta_cols], by = 'id'), by = 'id', all.x = TRUE)

  src$columns <- list(id = 'id', datetime = 'timestamp', x = 'longitude', y = 'latitude')
  src$input_crs <- 'EPSG:4326'
  src$name <- src$name %||% src$study
  standardize_locations(DT, src, loc_cfg, crs)
}

#' @title Put locations in the pipeline's standard form
#' @description Renames the ID, time and coordinate columns to `id`, `t_`, `x_`
#'   and `y_` (the names \pkg{amt} uses), makes `id` text and `t_` a date-time,
#'   projects coordinates to the project's coordinate system, and adds a
#'   `source` column naming the data set.
#' @inheritParams read_locations_file
#' @param DT data.frame of locations.
#' @return data.table.
#' @author Julie W. Turner
standardize_locations <- function(DT, src, loc_cfg, crs) {
  DT <- data.table::as.data.table(DT)
  cols <- src$columns %||% loc_cfg$columns
  missing <- setdiff(unlist(cols[c('id', 'datetime', 'x', 'y')]), names(DT))
  if (length(missing)) {
    stop('Location columns not found: ', paste(missing, collapse = ', '),
         '. Set `columns:` in the settings to the names used in this data set.')
  }

  keep <- unlist(loc_cfg$keep_columns)
  if (length(keep)) {
    DT <- DT[, unique(c(unlist(cols[c('id', 'datetime', 'x', 'y')]), intersect(keep, names(DT)))), with = FALSE]
  }
  # avoid clashes with columns that already use the standard names
  std <- c(id = 'id', datetime = 't_', x = 'x_', y = 'y_')
  clash <- setdiff(intersect(std, names(DT)), unlist(cols[c('id', 'datetime', 'x', 'y')]))
  if (length(clash)) data.table::setnames(DT, clash, paste0(clash, '_input'))
  data.table::setnames(DT, unlist(cols[c('id', 'datetime', 'x', 'y')]), std)

  DT[, id := as.character(id)]
  if (!inherits(DT$t_, 'POSIXct')) {
    DT[, t_ := as.POSIXct(t_, tz = loc_cfg$tz %||% 'UTC')]
  }
  DT <- DT[!is.na(x_) & !is.na(y_) & !is.na(t_)]

  input_crs <- src$input_crs %||% loc_cfg$input_crs
  if (!identical(sf::st_crs(input_crs), sf::st_crs(crs))) {
    xy <- sf::sf_project(sf::st_crs(input_crs), sf::st_crs(crs), as.matrix(DT[, .(x_, y_)]))
    DT[, `:=`(x_ = xy[, 1], y_ = xy[, 2])]
  }
  DT[, source := src$name %||% NA_character_]
  DT[]
}
