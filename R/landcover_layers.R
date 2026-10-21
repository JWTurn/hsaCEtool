# === Land-cover layers --------------------------------------------------------------
# Yearly proportion of each land-cover class, from a 30 m classification
# (SCANFI v3 or NTEMS VLCE2). Replaces make_landforest_prop() and
# aggregate_landforest() (prepLandscape, NMC_CEtool; Julie W. Turner): the same
# cell proportions and circular moving window, but the classes come from the
# settings and every class is counted in one pass over the 30 m map.

#' @title Build one year's land-cover proportion layers
#' @description Puts the year's classification onto the finest template, then
#'   for every class writes:
#'   \describe{
#'     \item{landcover_cell_<class>}{the proportion of each cell's classified
#'       30 m cells in that class, at every resolution in `resolutions` (the
#'       layers before any moving window, e.g. for masking water or rock)}
#'     \item{landcover_<class>}{at resolutions listed in `window_m`, the mean of
#'       those proportions within a circle of that radius (m) around each cell,
#'       as in make_landforest_prop()}
#'   }
#'   Counting is done at the finest resolution asked for; coarser ones are
#'   built from those counts, so they are exact.
#' @param sourceFile character. The year's classification, web address or local file.
#' @param year integer. Its year.
#' @param classes named list. Source code (name) -> class name (value), e.g.
#'   `list('1' = 'water', '2' = 'rock')`.
#' @param resolutions numeric. Cell sizes (m) to build; each needs a template.
#' @param window_m named list. Moving-window radius (m) by resolution, e.g.
#'   `list('240' = 500)`. Resolutions not listed get no window.
#' @param templates character. Template files from [make_templates()], finest first.
#' @param out_dir character. Folder to write to.
#' @param archiveFile character. For a zipped source (NTEMS), the file inside
#'   the zip; `{year}` is filled in. NULL otherwise.
#' @inheritParams raster_source
#' @return character. Files written, named `<prefix>_<class>_<res>m_<year>.tif`.
#' @author Julie W. Turner
#' @export
build_landcover_year <- function(sourceFile, year, classes, resolutions, window_m, templates, out_dir,
                                 access = 'stream', cache = 'inputs', archiveFile = NULL) {
  reproducible::checkPath(out_dir, create = TRUE)
  if (!is.null(archiveFile)) archiveFile <- fill_year(archiveFile, year)
  warped <- warp_to_template(in_archive(raster_source(sourceFile, access, cache), archiveFile), templates[1])
  on.exit(unlink(warped), add = TRUE)

  resolutions <- sort(unique(resolutions))
  classes <- Filter(Negate(is.null), classes)
  codes <- as.integer(names(classes))
  cls <- unlist(classes)
  props <- count_classes(warped, templates, resolutions[1], codes)
  names(props) <- c(cls, 'n')
  gdal_opts <- c('COMPRESS=DEFLATE', 'TILED=YES')

  files <- character()
  write_layers <- function(r, prefix, res) {
    out <- file.path(out_dir, sprintf('%s_%s_%sm_%d.tif', prefix, cls, res, year))
    for (k in seq_along(cls)) {
      terra::writeRaster(r[[k]], out[k], datatype = 'FLT4S', gdal = gdal_opts, overwrite = TRUE)
    }
    files <<- c(files, out)
  }
  for (res in resolutions) {
    p <- if (res == resolutions[1]) props else coarser_props(props, res / resolutions[1])
    write_layers(p[[cls]], 'landcover_cell', res)
    w <- window_m[[as.character(res)]]
    if (!is.null(w) && w > 0) {
      # as in make_landforest_prop(): circular weights, NA ignored, edges padded with 0
      p <- p[[cls]]
      wts <- terra::focalMat(p, d = w, type = 'circle')
      write_layers(terra::focal(p, wts, na.rm = TRUE, pad = TRUE, padValue = 0), 'landcover', res)
    }
  }
  files
}

#' @title Count the 30 m cells of each class in every coarser cell
#' @description Reads the 30 m classification one band of rows at a time (one
#'   coarse row each), so memory stays small however large the area. Only
#'   cells inside the template and with one of `codes` count.
#' @param file character. Classification on the finest template's grid.
#' @param templates character. Template files.
#' @param res numeric. Coarse cell size (m).
#' @param codes integer. Class codes to count.
#' @return SpatRaster at `res`: one layer per code with the proportion of the
#'   cell's classified 30 m cells, and a last layer `n` with how many there were.
#' @keywords internal
#' @noRd
count_classes <- function(file, templates, res, codes) {
  lc <- terra::rast(file)
  fine <- terra::rast(templates[1])
  coarse <- terra::rast(template_file(templates, res))
  f <- round(res / terra::res(fine)[1])
  nc <- terra::ncol(fine)
  ncc <- terra::ncol(coarse)
  K <- length(codes)
  cc <- (seq_len(nc) - 1) %/% f + 1            # coarse column of each fine column

  out <- terra::rast(coarse, nlyrs = K + 1)
  terra::readStart(lc); terra::readStart(fine)
  on.exit({terra::readStop(lc); terra::readStop(fine)}, add = TRUE)
  out_file <- tempfile(fileext = '.tif')
  b <- terra::writeStart(out, out_file, overwrite = TRUE, datatype = 'FLT4S')
  for (i in seq_len(terra::nrow(coarse))) {
    row <- (i - 1) * f + 1
    nr <- min(f, terra::nrow(fine) - row + 1)
    v <- terra::readValues(lc, row, nr, 1, nc)
    inside <- !is.na(terra::readValues(fine, row, nr, 1, nc))
    k <- match(v, codes)
    k[!inside] <- NA
    col <- rep(cc, nr)
    ok <- !is.na(k)
    counts <- matrix(tabulate((k[ok] - 1) * ncc + col[ok], nbins = K * ncc), ncol = K)
    n <- rowSums(counts)
    p <- counts / n
    p[n == 0, ] <- NA
    terra::writeValues(out, cbind(p, ifelse(n == 0, NA, n)), i, 1)
  }
  terra::writeStop(out)
  terra::rast(out_file)
}

#' @title Proportions at a coarser, nested resolution
#' @param props SpatRaster from `count_classes()` (last layer `n`).
#' @param fact integer. How many cells along each side make one coarser cell.
#' @return SpatRaster like `props`, at the coarser resolution.
#' @keywords internal
#' @noRd
coarser_props <- function(props, fact) {
  K <- terra::nlyr(props) - 1
  n <- props[[K + 1]]
  counts <- terra::aggregate(props[[seq_len(K)]] * n, fact, fun = 'sum', na.rm = TRUE)
  ntot <- terra::aggregate(n, fact, fun = 'sum', na.rm = TRUE)
  out <- c(counts / ntot, ntot)
  names(out) <- names(props)
  out
}
