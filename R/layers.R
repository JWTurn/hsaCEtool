# === Landscape layer table -----------------------------------------------------
# Each landscape layer is one row of a CSV (config/layers_<project>.csv), so a
# layer is added or changed without editing code. See config/README.md.

#' @title Read and check the landscape layer table
#' @description Columns:
#'   \describe{
#'     \item{name}{column name for the layer's values in the step table, e.g.
#'       `prop_water_1000` gives `prop_water_1000_start` and `prop_water_1000_end`}
#'     \item{family, resolution}{for your reference (e.g. landcover, 1000)}
#'     \item{first_year, last_year}{years with a raster; blank for a layer that
#'       does not change (e.g. elevation)}
#'     \item{path}{local file; for a yearly layer, `{year}` marks where the year goes}
#'     \item{url, target_file, archive}{or a download (see `get_source()`); `{year}` allowed}
#'     \item{time_since}{TRUE if the raster holds the year of a disturbance (fire,
#'       harvest): adds `ts_<name>_<start/end>`, the years since}
#'     \item{ts_nodata}{years since, for places with no recorded disturbance}
#'     \item{build}{instead of path or url: the builder that makes the layer
#'       from source data, e.g. `harvest_year` (see config/README.md)}
#'     \item{class}{for land-cover builds: the class, as named in the settings}
#'   }
#' @param file character. Path to the CSV.
#' @return data.table, one row per layer.
#' @author Julie W. Turner
read_layers <- function(file) {
  if (!file.exists(file)) stop('Layer table not found: ', file, call. = FALSE)
  text_cols <- intersect(c('path', 'url', 'target_file', 'archive', 'build', 'class'), names(data.table::fread(file, nrows = 0)))
  layers <- data.table::fread(file, colClasses = list(character = text_cols))
  for (col in c('path', 'url', 'target_file', 'archive', 'build', 'class')) {
    if (!col %in% names(layers)) layers[, (col) := NA_character_]
    layers[get(col) == '', (col) := NA_character_]
  }
  if (!'time_since' %in% names(layers)) layers[, time_since := FALSE]
  if (!'ts_nodata' %in% names(layers)) layers[, ts_nodata := NA_real_]
  layers[is.na(time_since), time_since := FALSE]
  layers[, `:=`(first_year = as.integer(first_year), last_year = as.integer(last_year),
                time_since = as.logical(time_since), ts_nodata = as.numeric(ts_nodata))]

  problems <- character()
  add <- function(rows, msg) {
    if (any(rows)) problems <<- c(problems, paste0(msg, ': ', paste(layers$name[rows], collapse = ', ')))
  }
  if (!'name' %in% names(layers)) stop('Layer table ', file, ' needs a `name` column', call. = FALSE)
  add(duplicated(layers$name), 'names used more than once')
  add(make.names(layers$name) != layers$name, 'names must be letters, numbers, . and _ and not start with a number')
  add(is.na(layers$path) & is.na(layers$url) & is.na(layers$build), 'no path, url or build')
  yearly <- !is.na(layers$first_year)
  add(yearly & (is.na(layers$last_year) | layers$last_year < layers$first_year), 'last_year missing or before first_year')
  has_year <- grepl('{year}', paste(layers$path, layers$url, layers$target_file), fixed = TRUE)
  add(yearly & is.na(layers$build) & !has_year, 'yearly layer without {year} in its path, url or target_file')
  add(!is.na(layers$build) & !yearly, 'built layers need first_year and last_year')
  add(!yearly & has_year, '{year} in path but no first_year/last_year')
  add(layers$time_since & !yearly, 'time_since needs a yearly layer')

  if (length(problems)) {
    stop('Problems in layer table ', file, ':\n  - ', paste(problems, collapse = '\n  - '), call. = FALSE)
  }
  layers[]
}

#' @title Get the files for one landscape layer
#' @description Finds (or downloads) the layer's raster for each year from
#'   `first_year` to `last_year`, or its single raster if it does not change by year.
#' @param path,url,target_file,archive character. From the layer table; `{year}`
#'   is replaced by each year. NA if not used.
#' @param first_year,last_year integer. NA for a layer that does not change.
#' @param paths named list. The `paths:` settings.
#' @return character. One file per year, in year order.
#' @author Julie W. Turner
get_layer_files <- function(path, url, target_file, archive, first_year, last_year, paths) {
  fill <- function(x, y) if (is.na(x)) NULL else gsub('{year}', y, x, fixed = TRUE)
  years <- if (is.na(first_year)) NA else first_year:last_year
  purrr::map_chr(years, function(y) {
    get_source(list(path = fill(path, y), url = fill(url, y),
                    target_file = fill(target_file, y), archive = fill(archive, y)), paths)[1]
  })
}
