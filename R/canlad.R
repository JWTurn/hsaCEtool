# === CanLaD harvest -----------------------------------------------------------------
# Harvest history from the Canada Landsat Disturbance products (CanLaD; 30 m):
# 1965-1984 (disturbance type + year) and the annual maps from 1985 on.
# Replaces the NTEMS + CanLaD harvest in prepLandscape (prep_everything.R,
# make_CanLad_cumulative.R). Each national raster is read once, straight onto
# the template grid with one gdalwarp call, and only the harvested cells are
# kept, as a small table of cell number and year.

#' @title Where to read a national raster from
#' @description A local file is used as it is. For a web address, with
#'   `access = "stream"` the raster is read straight from the web, fetching only
#'   the parts that cover the template (nothing is stored). With
#'   `access = "download"` the whole file is downloaded once into `cache` (like
#'   `reproducible.inputPaths`, one folder can serve several projects) and read
#'   from there.
#' @param sourceFile character. Web address (URL) or local path of the raster.
#' @param access character. "stream" or "download".
#' @param cache character. Folder for downloads.
#' @return character. Something GDAL can open.
#' @author Julie W. Turner
raster_source <- function(sourceFile, access = 'stream', cache = 'inputs') {
  if (file.exists(sourceFile)) return(sourceFile)
  if (identical(access, 'stream')) return(paste0('/vsicurl/', sourceFile))
  if (!identical(access, 'download')) stop('access must be "stream" or "download"', call. = FALSE)
  reproducible::checkPath(cache, create = TRUE)
  dest <- file.path(cache, basename(sourceFile))
  if (!file.exists(dest)) {
    part <- paste0(dest, '.part')
    old <- options(timeout = max(3600, getOption('timeout')))
    on.exit(options(old), add = TRUE)
    utils::download.file(sourceFile, part, mode = 'wb', quiet = TRUE)
    file.rename(part, dest)
  }
  dest
}

#' @title Put a raster onto the template grid
#' @description One gdalwarp call crops, reprojects and resamples (nearest
#'   neighbour, so codes and years are kept as they are).
#' @param src character. Raster to read (see [raster_source()]).
#' @param template character. Template raster file.
#' @param dst character. File to write.
#' @return `dst`.
#' @keywords internal
#' @noRd
warp_to_template <- function(src, template, dst = tempfile(fileext = '.tif')) {
  tr <- terra::rast(template)
  e <- as.vector(terra::ext(tr))
  sf::gdal_utils('warp', src, dst, options = c(
    '-t_srs', terra::crs(tr), '-te', e[1], e[3], e[2], e[4], '-tr', terra::res(tr),
    '-r', 'near', '-co', 'COMPRESS=DEFLATE', '-co', 'TILED=YES', '-overwrite',
    '-multi', '-wo', 'NUM_THREADS=ALL_CPUS'))
  dst
}

#' @title Harvested cells in one annual CanLaD map
#' @param sourceFile character. The year's annual map (`canlad_annual_<year>_v1.tif`),
#'   a web address or local file.
#' @param year integer. Its year.
#' @param template character. Finest template raster file.
#' @param code integer. Value(s) that mean harvest (2 in the annual maps).
#' @inheritParams raster_source
#' @return data.table with columns `cell` (cell number in the template) and `year`.
#' @author Julie W. Turner
canlad_annual_harvest <- function(sourceFile, year, template, code = 2, access = 'stream', cache = 'inputs') {
  warped <- warp_to_template(raster_source(sourceFile, access, cache), template)
  on.exit(unlink(warped), add = TRUE)
  cells <- harvest_cells(warped, code, template)
  data.table::data.table(cell = cells, year = rep(as.integer(year), length(cells)))
}

#' @title Harvested cells in the 1965-1984 CanLaD maps
#' @param typeFile,yearFile character. The disturbance type and disturbance
#'   year rasters, web addresses or local files.
#' @param code integer. Type value(s) that mean harvest (3 in the 1965-1984 product).
#' @inheritParams canlad_annual_harvest
#' @return data.table with columns `cell` and `year`.
#' @author Julie W. Turner
canlad_old_harvest <- function(typeFile, yearFile, template, code = 3, access = 'stream', cache = 'inputs') {
  type_file <- warp_to_template(raster_source(typeFile, access, cache), template)
  year_file <- warp_to_template(raster_source(yearFile, access, cache), template)
  on.exit(unlink(c(type_file, year_file)), add = TRUE)
  cells <- harvest_cells(type_file, code, template)
  yrs <- if (length(cells)) terra::extract(terra::rast(year_file), cells)[[1]] else integer()
  out <- data.table::data.table(cell = cells, year = as.integer(yrs))
  out[!is.na(year) & year > 0]
}

#' @title Cells with given values, inside the template
#' @keywords internal
#' @noRd
harvest_cells <- function(file, code, template) {
  cells <- terra::cells(terra::rast(file), code)[[1]]
  if (length(cells) == 0) return(numeric())
  inside <- !is.na(terra::extract(terra::rast(template), cells)[[1]])
  as.numeric(cells[inside])
}

#' @title Fill `{year}` in a file name or web address
#' @param pattern character. With `{year}` where the year goes.
#' @param years integer.
#' @return character, one per year.
#' @keywords internal
#' @noRd
fill_year <- function(pattern, years) {
  purrr::map_chr(years, function(y) gsub('{year}', y, pattern, fixed = TRUE))
}
