# === Fire layers --------------------------------------------------------------------
# Fire history from the National Burned Area Composite (NBAC, 1972 on) and, for
# earlier fires, the National Fire Database polygons (NFDB). Replaces the fire
# section of NMC_CEtool scripts/3-prep-land-UPDATED.R and make_fire_rast(): the
# same sources and year fixes, but fires are put onto the 30 m template once
# (all fires up to the first year built together, then each later year), and
# the layers are then built like the harvest layers. Every layer needs only each
# cell's most recent fire up to its year: burned in the past n years means that
# fire is within n years.

#' @title Download and unzip a fire database
#' @description The fire databases are large zip files (NBAC is 1.3 GB) on a
#'   server that sometimes drops long downloads, so the zip is fetched in
#'   pieces that resume when they stall (see [raster_source()]), then unzipped.
#'   Nothing is downloaded when the files are already there.
#' @param url character. Web address of the zip.
#' @param files character. The shapefile(s) wanted from the zip.
#' @param dir character. Folder to keep the download in (e.g. in `paths: input_cache`).
#' @return character. Paths of `files` and their sidecar files (.shx, .dbf, .prj, .cpg).
#' @author Julie W. Turner
download_fire <- function(url, files, dir) {
  reproducible::checkPath(dir, create = TRUE)
  paths <- file.path(dir, files)
  if (!all(file.exists(paths))) {
    zip <- file.path(dir, basename(url))
    if (!file.exists(zip)) download_parallel(url, zip, connections = 4)
    utils::unzip(zip, exdir = dir)
    missing <- !file.exists(paths)
    if (any(missing)) stop('Not in ', basename(url), ': ', paste(files[missing], collapse = ', '), call. = FALSE)
  }
  sidecars <- as.character(unlist(purrr::map(tools::file_path_sans_ext(paths),
                                             function(b) paste0(b, c('.shx', '.dbf', '.prj', '.cpg')))))
  c(paths, sidecars[file.exists(sidecars)])
}

#' @title Fire polygons with their year, in the project's coordinate system
#' @description Reads the fires overlapping `template` from NBAC and,
#'   optionally, the NFDB fires before `pre_year`. As in NMC_CEtool, an NFDB
#'   fire with no valid year (missing or below 1900) takes it from its report
#'   date (`REP_DATE`) or, failing that, from its ID (`FIRE_ID`, e.g.
#'   "1965-..."); fires still without a year are left out. Written to one
#'   GeoPackage (layer `fire`, column `year`), so later steps only rerun when
#'   the fires change.
#' @param nbac character. NBAC shapefile (from [download_fire()]; the
#'   first `.shp` is used).
#' @param nfdb character. NFDB shapefile, or NULL to leave out older fires.
#' @param template character. Raster whose extent sets the area kept (the
#'   template with a margin).
#' @param crs character. Project coordinate system.
#' @param out character. GeoPackage to write.
#' @param pre_year integer. NFDB fires before this year are added (NBAC starts in 1972).
#' @return `out`.
#' @author Julie W. Turner
prep_fire_polygons <- function(nbac, nfdb = NULL, template, crs, out, pre_year = 1972) {
  reproducible::checkPath(dirname(out), create = TRUE)
  read_fires <- function(file) {
    shp <- file[tolower(tools::file_ext(file)) == 'shp'][1]
    lyr_crs <- sf::st_crs(sf::st_read(shp, query = sprintf('SELECT * FROM "%s" LIMIT 0',
                                                            tools::file_path_sans_ext(basename(shp))), quiet = TRUE))
    area <- sf::st_transform(sf::st_as_sfc(sf::st_bbox(terra::rast(template))), lyr_crs)
    sf::st_read(shp, wkt_filter = sf::st_as_text(area), quiet = TRUE)
  }
  x <- read_fires(nbac)
  fires <- sf::st_sf(year = as.integer(x$YEAR), geometry = sf::st_geometry(x))
  if (length(nfdb) && !is.null(nfdb)) {
    old <- read_fires(nfdb)
    yr <- suppressWarnings(as.integer(old$YEAR))
    bad <- is.na(yr) | yr < 1900
    rep_yr <- suppressWarnings(as.integer(substr(as.character(old$REP_DATE), 1, 4)))
    id_yr <- suppressWarnings(as.integer(stringr::str_extract(as.character(old$FIRE_ID), '^(18|19|20)[0-9]{2}')))
    yr[bad] <- ifelse(!is.na(rep_yr[bad]) & rep_yr[bad] >= 1900, rep_yr[bad], id_yr[bad])
    old <- sf::st_sf(year = yr, geometry = sf::st_geometry(old))
    old <- old[!is.na(old$year) & old$year < pre_year, ]
    fires <- rbind(sf::st_transform(fires, crs), sf::st_transform(old, crs))
  } else {
    fires <- sf::st_transform(fires, crs)
  }
  fires <- fires[!is.na(fires$year), ]
  fires <- sf::st_zm(fires)
  sf::st_write(fires, out, layer = 'fire', delete_dsn = TRUE, quiet = TRUE)
  out
}

#' @title Groups of fire years to put onto the grid
#' @description Years before `cutoff` form one group, in which only each
#'   cell's most recent fire is kept; later years each get their own group.
#'   With `cutoff` the year after the first year built, every cell's most
#'   recent fire up to any year built is kept, which is all the fire layers
#'   (and forest age resets) need.
#' @param fires character. GeoPackage from [prep_fire_polygons()].
#' @param cutoff integer. First year kept on its own.
#' @return list of integer vectors (years).
#' @author Julie W. Turner
fire_year_groups <- function(fires, cutoff) {
  yrs <- sort(unique(sf::st_read(fires, query = 'SELECT DISTINCT year FROM fire', quiet = TRUE)$year))
  older <- yrs[yrs < cutoff]
  c(if (length(older)) list(older), as.list(yrs[yrs >= cutoff]))
}

#' @title Burned 30 m cells for a group of fire years
#' @description Puts the fires of `years` onto the template grid (a cell burns
#'   when its centre is inside a fire). For several years, each cell keeps its
#'   most recent one.
#' @param fires character. GeoPackage from [prep_fire_polygons()].
#' @param years integer. Fire years.
#' @param template character. Template raster file (the one with a margin).
#' @return data.table with columns `cell` (cell number in `template`) and `year`.
#' @author Julie W. Turner
fire_cells <- function(fires, years, template) {
  empty <- data.table::data.table(cell = numeric(), year = integer())
  q <- sprintf('SELECT * FROM fire WHERE year IN (%s)', paste(years, collapse = ', '))
  x <- sf::st_read(fires, query = q, quiet = TRUE)
  if (nrow(x) == 0) return(empty)
  x <- x[order(x$year), ]   # most recent written last, so it is the one kept
  vec <- tempfile(fileext = '.gpkg')
  ras <- tempfile(fileext = '.tif')
  on.exit(unlink(c(vec, ras)), add = TRUE)
  sf::st_write(x, vec, layer = 'f', quiet = TRUE)
  tr <- terra::rast(template)
  e <- as.vector(terra::ext(tr))
  sf::gdal_utils('rasterize', vec, ras, options = c(
    '-l', 'f', '-a', 'year', '-te', e[1], e[3], e[2], e[4], '-tr', terra::res(tr),
    '-ot', 'UInt16', '-init', '0', '-a_nodata', '0', '-co', 'COMPRESS=DEFLATE', '-co', 'TILED=YES'))
  r <- terra::rast(ras)
  cells <- terra::cells(r)
  if (length(cells) == 0) return(empty)
  data.table::data.table(cell = as.numeric(cells), year = as.integer(terra::extract(r, cells)[[1]]))
}
