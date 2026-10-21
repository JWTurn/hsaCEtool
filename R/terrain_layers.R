# === Terrain layers ---------------------------------------------------------------
# Elevation and northness from a 30 m elevation model (by default NRCan's
# MRDEM-30). Replaces the elevation section of NMC_CEtool
# scripts/3-prep-land-UPDATED.R, which used elevatr web tiles resampled to each
# resolution and measured aspect from grid north rather than true north.

#' @title Build the terrain layers
#' @description Puts the elevation model onto the finest template's grid,
#'   including its margin (bilinear), then writes for each resolution:
#'   \describe{
#'     \item{elevation}{mean elevation (m) of the cell's 30 m cells}
#'     \item{northness}{cos(aspect): 1 facing north, -1 facing south, 0 facing
#'       east or west, and 0 on flat ground (see `northness_raster()`). With
#'       `northness = "fine"`, worked out for every 30 m cell and then averaged;
#'       with `"coarse"`, worked out from the cells' mean elevation, as in
#'       NMC_CEtool.}
#'   }
#' @param sourceFile character. Elevation raster, web address or local file.
#' @param templates character. Template files from [make_templates()], finest first.
#' @param margin character. The finest template with a margin
#'   ([make_margin_template()]), so slopes at the template's edge can use the
#'   elevation just beyond it.
#' @param resolutions numeric. Cell sizes (m) to build; each needs a template.
#' @param northness character. "fine" or "coarse".
#' @param out_dir character. Folder to write to.
#' @inheritParams raster_source
#' @return character. Files `terrain_elevation_<res>m.tif` and
#'   `terrain_northness_<res>m.tif`.
#' @author Julie W. Turner
#' @export
build_terrain_layers <- function(sourceFile, templates, margin, resolutions, northness = 'fine', out_dir,
                                 access = 'stream', cache = 'inputs') {
  northness <- match.arg(northness, c('fine', 'coarse'))
  reproducible::checkPath(out_dir, create = TRUE)
  dem_file <- warp_dem(raster_source(sourceFile, access, cache), margin)
  on.exit(unlink(dem_file), add = TRUE)
  dem <- terra::rast(dem_file)
  fine <- terra::rast(templates[1])
  resolutions <- sort(unique(resolutions))
  gdal_opts <- c('COMPRESS=DEFLATE', 'TILED=YES')
  files <- character()
  write_layer <- function(r, what, res) {
    out <- file.path(out_dir, sprintf('terrain_%s_%sm.tif', what, res))
    names(r) <- what
    terra::writeRaster(r, out, datatype = 'FLT4S', gdal = gdal_opts, overwrite = TRUE)
    files <<- c(files, out)
  }

  elev30 <- terra::mask(terra::crop(dem, fine, snap = 'near'), fine)
  for (res in resolutions) write_layer(mean_to_template(elev30, templates, res), 'elevation', res)

  if (northness == 'fine') {
    n30 <- terra::mask(terra::crop(northness_raster(dem), fine, snap = 'near'), fine)
    for (res in resolutions) write_layer(mean_to_template(n30, templates, res), 'northness', res)
  } else {
    for (res in resolutions) {
      tr <- terra::rast(template_file(templates, res))
      # mean elevation on the coarse grid, one cell wider all round so that edge
      # cells have neighbours, then northness, cut back to the template
      e <- terra::extend(terra::ext(tr), res)
      d <- terra::extend(terra::crop(dem, e, snap = 'near'), e)
      d <- terra::aggregate(d, round(res / terra::res(fine)[1]), fun = 'mean', na.rm = TRUE)
      n <- terra::crop(northness_raster(d), tr, snap = 'near')
      write_layer(terra::mask(n, tr), 'northness', res)
    }
  }
  files
}

#' @title Northness from elevation
#' @description cos(aspect), where aspect is the compass direction the slope
#'   faces measured from true north: 1 facing north, -1 facing south, 0 facing
#'   east or west. terra measures aspect from the grid's north, which away from
#'   the projection's central meridian differs from true north (by up to about
#'   20 degrees across the NMC study area in UTM zone 7), so aspect is first
#'   turned by that angle (`grid_convergence()`). Flat cells (slope under about
#'   0.0001 degrees, whose aspect is arbitrary) get 0.
#' @param dem SpatRaster. Elevation in a projected coordinate system.
#' @return SpatRaster.
#' @author Julie W. Turner
#' @keywords internal
#' @noRd
northness_raster <- function(dem) {
  slope <- terra::terrain(dem, 'slope', unit = 'radians')
  aspect <- terra::terrain(dem, 'aspect', unit = 'radians')
  n <- cos(aspect - grid_convergence(dem))
  terra::ifel(slope < 1e-6, 0, n)   # flat (rounding leaves a tiny slope with an arbitrary aspect)
}

#' @title Angle between grid north and true north
#' @description For each cell, the angle (radians, clockwise) from the grid's
#'   north to true north, so that true aspect = grid aspect - this angle.
#'   Worked out on a coarse grid from the projection, then interpolated.
#' @param r SpatRaster in a projected coordinate system.
#' @param n integer. Cells along the longer side of the coarse grid.
#' @return SpatRaster like `r`.
#' @keywords internal
#' @noRd
grid_convergence <- function(r, n = 200) {
  crs <- terra::crs(r)
  step <- max(diff(as.vector(terra::ext(r)))[c(1, 3)]) / n
  g <- terra::rast(terra::extend(terra::ext(r), 2 * step), resolution = step, crs = crs)
  xy <- terra::xyFromCell(g, seq_len(terra::ncell(g)))
  ll <- sf::sf_project(crs, 'EPSG:4326', xy)
  north <- sf::sf_project('EPSG:4326', crs, cbind(ll[, 1], ll[, 2] + 0.001))
  terra::values(g) <- atan2(north[, 1] - xy[, 1], north[, 2] - xy[, 2])
  terra::resample(g, r, method = 'bilinear')
}

#' @title Mean of a 30 m raster on a coarser template
#' @param r SpatRaster on the finest template's grid.
#' @param templates character. Template files.
#' @param res numeric. Cell size (m).
#' @return SpatRaster on the template of `res`, NA outside it.
#' @keywords internal
#' @noRd
mean_to_template <- function(r, templates, res) {
  tr <- terra::rast(template_file(templates, res))
  a <- terra::aggregate(r, round(res / terra::res(r)[1]), fun = 'mean', na.rm = TRUE)
  terra::mask(a, tr)
}

#' @title Put an elevation model onto a template grid
#' @description Like `warp_resample()` with bilinear resampling, for a large
#'   elevation model read from the web (e.g. MRDEM): the template's area is cut
#'   from the model in its own projection in `tiles` x `tiles` pieces, each
#'   retried up to `attempts` times if a web read fails (one dropped request
#'   would otherwise fail the whole step), then joined and put onto the grid.
#'   Every file is written as BigTIFF: elevation is stored as decimals, which
#'   compress poorly, so the area can pass the 4 GB limit of a plain TIFF.
#' @param src character. Raster to read (see `raster_source()`).
#' @param template character. Template raster file.
#' @param dst character. File to write.
#' @param tiles integer. Pieces along each side.
#' @param attempts integer. Tries per piece.
#' @return `dst`.
#' @keywords internal
#' @noRd
warp_dem <- function(src, template, dst = tempfile(fileext = '.tif'), tiles = 4, attempts = 5) {
  tr <- terra::rast(template)
  e <- as.vector(terra::ext(tr))
  big <- c('-co', 'COMPRESS=DEFLATE', '-co', 'TILED=YES', '-co', 'BIGTIFF=YES')
  if (grepl('/vsicurl/', src, fixed = TRUE)) {
    s_r <- terra::rast(src)
    area <- terra::project(terra::as.polygons(terra::ext(tr), crs = terra::crs(tr)), terra::crs(s_r))
    b <- as.vector(terra::ext(terra::buffer(area, 1000)))
    # tile edges on the model's own cell edges, so neighbouring tiles meet without gaps
    snap <- function(v, origin, res) origin + round((v - origin) / res) * res
    xs <- snap(seq(b[1], b[2], length.out = tiles + 1), terra::xmin(s_r), terra::res(s_r)[1])
    ys <- snap(seq(b[3], b[4], length.out = tiles + 1), terra::ymin(s_r), terra::res(s_r)[2])
    pieces <- character()
    on.exit(unlink(pieces), add = TRUE)
    for (i in seq_len(tiles)) for (j in seq_len(tiles)) {
      out <- tempfile(fileext = '.tif')
      for (k in seq_len(attempts)) {
        ok <- tryCatch({
          sf::gdal_utils('translate', src, out,
                         options = c('-projwin', xs[i], ys[j + 1], xs[i + 1], ys[j], big),
                         config_options = c(GDAL_HTTP_MERGE_CONSECUTIVE_RANGES = 'YES', GDAL_HTTP_MULTIPLEX = 'YES',
                                            GDAL_NUM_THREADS = 'ALL_CPUS', CPL_VSIL_CURL_CHUNK_SIZE = '4194304',
                                            GDAL_DISABLE_READDIR_ON_OPEN = 'EMPTY_DIR'))
          TRUE
        }, error = function(err) FALSE)
        if (ok) break
        unlink(out)
        if (k == attempts) stop('Could not read part of ', src, ' after ', attempts, ' tries', call. = FALSE)
        Sys.sleep(30 * k)
      }
      pieces <- c(pieces, out)
    }
    vrt <- tempfile(fileext = '.vrt')
    on.exit(unlink(vrt), add = TRUE)
    sf::gdal_utils('buildvrt', pieces, vrt)
    src <- vrt
  }
  sf::gdal_utils('warp', src, dst, options = c(
    '-t_srs', terra::crs(tr), '-te', e[1], e[3], e[2], e[4], '-tr', terra::res(tr),
    '-r', 'bilinear', '-ot', 'Float32', big, '-overwrite', '-multi', '-wo', 'NUM_THREADS=ALL_CPUS'))
  dst
}

#' @title Put a raster onto a template grid with a chosen resampling method
#' @description As `warp_to_template()`, but for continuous values (e.g.
#'   elevation) that are better interpolated ("bilinear") than copied from the
#'   nearest cell.
#' @param src character. Raster to read (see `raster_source()`).
#' @param template character. Template raster file.
#' @param method character. gdalwarp resampling method, e.g. "bilinear" or "near".
#' @param dst character. File to write.
#' @return `dst`.
#' @keywords internal
#' @noRd
warp_resample <- function(src, template, method = 'bilinear', dst = tempfile(fileext = '.tif')) {
  tr <- terra::rast(template)
  e <- as.vector(terra::ext(tr))
  if (grepl('/vsicurl/', src, fixed = TRUE)) {
    src <- clip_remote(src, tr)
    on.exit(unlink(src), add = TRUE)
  }
  sf::gdal_utils('warp', src, dst, options = c(
    '-t_srs', terra::crs(tr), '-te', e[1], e[3], e[2], e[4], '-tr', terra::res(tr),
    '-r', method, '-ot', 'Float32', '-co', 'COMPRESS=DEFLATE', '-co', 'TILED=YES', '-co', 'BIGTIFF=IF_SAFER',
    '-overwrite', '-multi', '-wo', 'NUM_THREADS=ALL_CPUS'))
  dst
}
