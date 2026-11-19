# === Tree species sources -----------------------------------------------------------------
# Reading tree species cover from two sources, cut to an area and written to files:
# SCANFI crown closure by species (rasters, one per species and year) and the BC Vegetation
# Resources Inventory (VRI) species composition (polygons). Projects summarize these files
# (e.g. to choose which species to model); the species-cover layers are built from them.

#' @title Read SCANFI crown closure by species for an area
#' @description For each species, cuts that species' national 30 m crown-closure raster for
#'   one year to `area`, and sets cells whose centre is outside `area` to missing. The
#'   source's own grid and projection are kept, so each 30 m cell is counted once, as in the
#'   source (layers on the project's grid are built from these files later). In SCANFI v2, a
#'   species' value is the percent of the ground covered by that species' crowns (0-100), so
#'   the species of one cell add up to its total crown closure.
#' @param pattern character. Web address or local path of the rasters, with `{species}` and
#'   `{year}` where the species name and year go.
#' @param species character. Species as named in the files (e.g. `ABIE_LAS`).
#' @param year integer. One year the source has a map for.
#' @param area character or SpatVector. The area: a polygon file, or polygons.
#' @param out_dir character. Folder to write to.
#' @inheritParams raster_source
#' @return character. Files written, `scanfi_<species>_<year>.tif`, named by species.
#' @author Julie W. Turner
#' @export
read_scanfi_species <- function(
  pattern,
  species,
  year,
  area,
  out_dir,
  access = 'stream',
  cache = 'inputs'
) {
  if (length(year) != 1) {
    stop('read_scanfi_species() reads one year at a time', call. = FALSE)
  }
  reproducible::checkPath(out_dir, create = TRUE)
  if (is.character(area)) {
    area <- terra::vect(area)
  }
  sources <- fill_species(fill_year(pattern, year), species)
  out <- file.path(out_dir, sprintf('scanfi_%s_%d.tif', species, year))
  purrr::walk(seq_along(species), function(k) {
    cut_to_area(raster_source(sources[k], access, cache), area, out[k])
  })
  stats::setNames(out, species)
}

#' @title Read BC VRI species composition for an area
#' @description Reads the polygons of a BC Vegetation Resources Inventory (VRI) layer (e.g.
#'   the Forest Vegetation Composite Rank 1 Layer) that overlap `area`, keeping only
#'   `columns`, and writes them to a GeoPackage. Each polygon lists up to six species
#'   (`SPECIES_CD_1` to `SPECIES_CD_6`, BC species codes) with their percent of the stand
#'   (`SPECIES_PCT_1` to `SPECIES_PCT_6`). The polygons are kept whole (not cut at the
#'   area's edge). A web source is downloaded once into `cache` and read from there.
#' @param sourceFile character. Web address or local path of the VRI file geodatabase,
#'   zipped (`.gdb.zip`) or not (`.gdb`).
#' @param area character or SpatVector. The area: a polygon file, or polygons.
#' @param out_file character. GeoPackage to write.
#' @param target_file character. For a zipped source, the geodatabase inside the zip (e.g.
#'   `VEG_COMP_LYR_R1_POLY_2025.gdb`); NULL: the zip's name without `.zip`.
#' @param layer character. Layer in the geodatabase; NULL: its only layer.
#' @param columns character. Columns to keep.
#' @param cache character. Folder for the download.
#' @return `out_file`.
#' @author Julie W. Turner
#' @export
read_vri_species <- function(
  sourceFile,
  area,
  out_file,
  target_file = NULL,
  layer = NULL,
  columns = c(
    'FEATURE_ID',
    'BCLCS_LEVEL_1',
    'BCLCS_LEVEL_2',
    'CROWN_CLOSURE',
    'PROJ_AGE_1',
    'PROJECTED_DATE',
    paste0('SPECIES_CD_', 1:6),
    paste0('SPECIES_PCT_', 1:6)
  ),
  cache = 'inputs'
) {
  if (!file.exists(sourceFile)) {
    reproducible::checkPath(cache, create = TRUE)
    dest <- file.path(cache, basename(sourceFile))
    if (!file.exists(dest)) {
      download_parallel(sourceFile, dest)
    }
    sourceFile <- dest
  }
  dsn <- if (grepl('\\.zip$', sourceFile)) {
    paste0('/vsizip/', sourceFile, '/', target_file %||% sub('\\.zip$', '', basename(sourceFile)))
  } else {
    sourceFile
  }
  if (is.null(layer)) {
    layers <- sf::st_layers(dsn)$name
    if (length(layers) != 1) {
      stop(
        'The VRI source has ',
        length(layers),
        ' layers; name one with layer: ',
        paste(layers, collapse = ', '),
        call. = FALSE
      )
    }
    layer <- layers
  }
  if (is.character(area)) {
    area <- terra::vect(area)
  }
  info <- sf::st_layers(dsn)
  crs_src <- info$crs[[match(layer, info$name)]]
  area <- sf::st_union(sf::st_transform(sf::st_as_sf(area), crs_src))
  vri <- sf::st_read(
    dsn,
    query = sprintf(
      'SELECT %s FROM "%s"',
      paste(c(columns, geometry_column(dsn, layer)), collapse = ', '),
      layer
    ),
    wkt_filter = sf::st_as_text(sf::st_as_sfc(sf::st_bbox(area))),
    quiet = TRUE
  )
  vri <- vri[lengths(sf::st_intersects(vri, area)) > 0, ]
  reproducible::checkPath(dirname(out_file), create = TRUE)
  sf::st_write(vri, out_file, delete_dsn = TRUE, quiet = TRUE)
  out_file
}

#' @title The name of a vector layer's geometry column
#' @description Needed to select it in a query: some formats (e.g. GeoPackage) return only
#'   the columns named. Read from the layer's first feature.
#' @param dsn,layer character. Data source and layer.
#' @return character.
#' @keywords internal
#' @noRd
geometry_column <- function(dsn, layer) {
  one <- sf::st_read(dsn, query = sprintf('SELECT * FROM "%s" LIMIT 1', layer), quiet = TRUE)
  attr(one, 'sf_column')
}

#' @title Cut a raster to an area, in the raster's own projection
#' @description Cuts to the area's extent (whole source cells, so the grid is kept), then
#'   sets cells whose centre is outside the area to missing. A raster read from the web is
#'   cut by GDAL, which fetches only the parts it needs.
#' @param src character. Something GDAL can open (see `raster_source()`).
#' @param area SpatVector. Polygons, in any projection.
#' @param dst character. File to write.
#' @return `dst`.
#' @keywords internal
#' @noRd
cut_to_area <- function(src, area, dst) {
  r <- terra::rast(src)
  area <- terra::project(area, terra::crs(r))
  b <- as.vector(terra::ext(area))
  tmp <- tempfile(fileext = '.tif')
  on.exit(unlink(tmp), add = TRUE)
  sf::gdal_utils(
    'translate',
    src,
    tmp,
    options = c('-projwin', b[1], b[4], b[2], b[3], '-co', 'COMPRESS=DEFLATE', '-co', 'TILED=YES'),
    config_options = c(
      GDAL_HTTP_MERGE_CONSECUTIVE_RANGES = 'YES',
      GDAL_HTTP_MULTIPLEX = 'YES',
      GDAL_NUM_THREADS = 'ALL_CPUS',
      CPL_VSIL_CURL_CHUNK_SIZE = '4194304',
      GDAL_DISABLE_READDIR_ON_OPEN = 'EMPTY_DIR'
    )
  )
  cut <- terra::rast(tmp)
  terra::mask(
    cut,
    area,
    touches = FALSE,
    filename = dst,
    datatype = terra::datatype(cut)[1],
    gdal = c('COMPRESS=DEFLATE', 'TILED=YES'),
    overwrite = TRUE
  )
  dst
}

#' @title Fill `{species}` in a file name or web address
#' @param pattern character. With `{species}` where the species goes.
#' @param species character.
#' @return character, one per species.
#' @keywords internal
#' @noRd
fill_species <- function(pattern, species) {
  purrr::map_chr(species, function(s) gsub('{species}', s, pattern, fixed = TRUE))
}
