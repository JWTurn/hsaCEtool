# === CanLaD harvest -----------------------------------------------------------------
# Harvest history from the Canada Landsat Disturbance products (CanLaD; 30 m):
# 1965-1984 (disturbance type + year) and the annual maps from 1985 on.
# Replaces the NTEMS + CanLaD harvest in prepLandscape (prep_everything.R,
# make_CanLad_cumulative.R). Each national raster is read once, straight onto
# the template grid with one gdalwarp call, and only the harvested cells are
# kept, as a small table of cell number and year (see read_national_rasters.R).

#' @title Harvested cells in one annual CanLaD map
#' @param sourceFile character. The year's annual map (`canlad_annual_<year>_v1.tif`),
#'   a web address or local file.
#' @param year integer. Its year.
#' @param template character. Template raster file giving the grid: the finest
#'   template, or the one with a margin (see [make_margin_template()]).
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
