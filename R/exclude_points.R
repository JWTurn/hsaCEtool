# === Exclusion points table ------------------------------------------------
# Reading `locations: cleaning: params: exclude_points:` from a CSV

#' @title Read the exclusion points table
#' @description Reads a table of known false locations (e.g. a field office
#'   where collars are stored), one row per point, for the cleaning step
#'   `exclude_points`, which drops fixes within `radius_m` of each. Columns:
#'   \describe{
#'     \item{name}{what the point is (optional)}
#'     \item{lon, lat}{longitude and latitude, in decimal degrees (WGS 84)}
#'     \item{radius_m}{fixes closer than this, in metres, are dropped}
#'     \item{notes}{anything worth knowing about the point; not used by the pipeline}
#'   }
#' @param file character. Path to the CSV file.
#' @return list of points, each a list with `name` (if given), `lon`, `lat` and
#'   `radius_m`: the same as typing them in the YAML.
#' @author Julie W. Turner
#' @export
read_exclude_points <- function(file) {
  if (!file.exists(file)) stop('Exclusion points table not found: ', file, call. = FALSE)
  cols <- c('name', 'lon', 'lat', 'radius_m', 'notes')
  tab <- data.table::fread(file, colClasses = 'character', na.strings = '')
  unknown <- setdiff(names(tab), cols)
  if (length(unknown)) {
    stop('Exclusion points table ', file, ' has unknown columns: ', paste(unknown, collapse = ', '),
         '. Columns can be: ', paste(cols, collapse = ', '), call. = FALSE)
  }
  missing <- setdiff(c('lon', 'lat', 'radius_m'), names(tab))
  if (length(missing)) {
    stop('Exclusion points table ', file, ' needs columns: ', paste(missing, collapse = ', '), call. = FALSE)
  }
  if (!'name' %in% names(tab)) tab[, name := NA_character_]
  num <- function(x) suppressWarnings(as.numeric(trimws(x)))
  lon <- num(tab$lon)
  lat <- num(tab$lat)
  radius <- num(tab$radius_m)
  problems <- character()
  add <- function(rows, msg) {
    if (any(rows)) problems <<- c(problems, paste0(msg, ': rows ', paste(which(rows) + 1L, collapse = ', ')))
  }
  add(is.na(lon) | abs(lon) > 180, 'lon must be a longitude (-180 to 180)')
  add(is.na(lat) | abs(lat) > 90, 'lat must be a latitude (-90 to 90)')
  add(is.na(radius) | radius <= 0, 'radius_m must be a distance above 0')
  if (length(problems)) {
    stop('Problems in the exclusion points table ', file, ':\n  - ', paste(problems, collapse = '\n  - '), call. = FALSE)
  }
  # as in the YAML: whole numbers are integers
  whole <- function(x) if (x == round(x) && abs(x) < .Machine$integer.max) as.integer(x) else x
  purrr::map(seq_len(nrow(tab)), function(i) {
    nm <- trimws(tab$name[i])
    pt <- list(name = if (is.na(nm) || !nzchar(nm)) NULL else nm,
               lon = whole(lon[i]), lat = whole(lat[i]), radius_m = whole(radius[i]))
    purrr::compact(pt)
  })
}
