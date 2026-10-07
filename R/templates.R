# === Template rasters ------------------------------------------------------------
# Every layer the pipeline builds is written onto these grids, so all layers
# line up cell for cell. Adapted from prepLandscape (rasterToMatch_extendedLandscape*)
# and NMC_CEtool scripts/3-prep-land-UPDATED.R.

#' @title Make the template rasters
#' @description The finest template has cells of `base_res` metres covering the
#'   study area plus `buffer_m`, with 1 inside and NA outside. Its edges are
#'   snapped to whole multiples of every coarser cell size, so each coarser
#'   template (`base_res` x factor) nests exactly: e.g. with 30 m cells and
#'   factors 8, 40 and 160, the coarser templates have 240, 1200 and 4800 m cells.
#'   A coarser template holds the number of fine cells inside the study area in
#'   each of its cells (used for proportions).
#'
#'   Instead of building the finest template, give one in `tmpl$source` (a
#'   source: path or url), or, in `<project>.R`, a function in `tmpl$fun` that
#'   takes `(study_area, tmpl, crs)` and returns a SpatRaster.
#' @param study_area SpatVector. The study area.
#' @param tmpl named list. The `landscape: template:` settings: `source`, `fun`,
#'   `base_res`, `buffer_m`, `factors`.
#' @param crs character. Coordinate system of every layer (the project's).
#' @param paths named list. The `paths:` settings.
#' @param out_dir character. Folder to write the templates to.
#' @return character. Template files: the finest first, then one per factor in
#'   the order of `tmpl$factors`.
#' @author Julie W. Turner
make_templates <- function(study_area, tmpl, crs, paths, out_dir) {
  reproducible::checkPath(out_dir, create = TRUE)
  fine_file <- file.path(out_dir, paste0('template_', tmpl$base_res, 'm.tif'))
  gdal_opts <- c('COMPRESS=DEFLATE', 'TILED=YES')

  if (is.function(tmpl$fun) || !is.null(tmpl$source)) {
    r <- if (is.function(tmpl$fun)) {
      tmpl$fun(study_area, tmpl, crs)
    } else {
      terra::rast(get_source(tmpl$source, paths)[1])
    }
    if (!terra::same.crs(r, crs)) stop('The template raster is not in ', crs, call. = FALSE)
    if (!isTRUE(all.equal(terra::res(r), rep(tmpl$base_res, 2)))) {
      stop('The template raster has ', paste(terra::res(r), collapse = ' x '),
           ' m cells; set landscape: template: base_res to match', call. = FALSE)
    }
    terra::ifel(is.na(r[[1]]), NA, 1, filename = fine_file, datatype = 'INT1U',
                gdal = gdal_opts, overwrite = TRUE)
  } else {
    area <- terra::aggregate(terra::buffer(terra::project(study_area, crs), tmpl$buffer_m))
    step <- tmpl$base_res * lcm(unlist(tmpl$factors))
    e <- as.vector(terra::ext(area))   # xmin, xmax, ymin, ymax
    te <- c(floor(e[1] / step), floor(e[3] / step), ceiling(e[2] / step), ceiling(e[4] / step)) * step
    area_file <- tempfile(fileext = '.gpkg')
    on.exit(unlink(area_file), add = TRUE)
    terra::writeVector(area, area_file)
    # gdal_rasterize streams to disk, so very large templates are fine
    sf::gdal_utils('rasterize', area_file, fine_file, options = c(
      '-burn', '1', '-init', '0', '-a_nodata', '0', '-ot', 'Byte',
      '-te', te, '-tr', tmpl$base_res, tmpl$base_res,
      '-co', gdal_opts[1], '-co', gdal_opts[2]))
  }

  fine <- terra::rast(fine_file)
  coarse_files <- purrr::map_chr(unlist(tmpl$factors), function(f) {
    out <- file.path(out_dir, paste0('template_', tmpl$base_res * f, 'm.tif'))
    terra::aggregate(fine, f, fun = 'sum', na.rm = TRUE, filename = out,
                     datatype = 'INT4U', gdal = gdal_opts, overwrite = TRUE)
    out
  })
  c(fine_file, coarse_files)
}

#' @title Make the template with a margin, for distances
#' @description The finest template's grid extended by `margin_m` (rounded up
#'   to whole cells) on every side, with every cell 1. Distances are measured
#'   over it and then cut back to the template, so that a road or cutblock just
#'   outside the template still counts.
#' @param template character. The finest template file, from [make_templates()].
#' @param margin_m numeric. Margin in metres (0 = none).
#' @param out character. File to write.
#' @return `out`.
#' @author Julie W. Turner
make_margin_template <- function(template, margin_m, out) {
  reproducible::checkPath(dirname(out), create = TRUE)
  r <- terra::rast(template)
  res <- terra::res(r)[1]
  m <- ceiling((margin_m %||% 0) / res) * res
  e <- as.vector(terra::ext(r)) + c(-m, m, -m, m)
  box <- tempfile(fileext = '.gpkg')
  on.exit(unlink(box), add = TRUE)
  terra::writeVector(terra::as.polygons(terra::ext(e), crs = terra::crs(r)), box)
  sf::gdal_utils('rasterize', box, out, options = c(
    '-burn', '1', '-init', '1', '-a_nodata', '0', '-ot', 'Byte',
    '-te', e[1], e[3], e[2], e[4], '-tr', res, res,
    '-co', 'COMPRESS=DEFLATE', '-co', 'TILED=YES'))
  out
}

#' @title Pick one template file by cell size
#' @param templates character. Files from [make_templates()].
#' @param res numeric. Cell size in metres.
#' @return character. The template file.
#' @keywords internal
#' @noRd
template_file <- function(templates, res) {
  f <- templates[basename(templates) == paste0('template_', res, 'm.tif')]
  if (length(f) != 1) stop('No ', res, ' m template; add its factor to landscape: template: factors', call. = FALSE)
  f
}

#' @title Least common multiple of whole numbers
#' @keywords internal
#' @noRd
lcm <- function(x) {
  gcd <- function(a, b) if (b == 0) a else gcd(b, a %% b)
  purrr::reduce(as.integer(x), function(a, b) a / gcd(a, b) * b)
}
