# === Harvest layers -----------------------------------------------------------------
# Yearly harvest layers at each resolution, built from the harvest history (cell
# number and year of each harvested 30 m cell, see canlad.R). Replaces the
# harvest section of NMC_CEtool scripts/3-prep-land-UPDATED.R, fixing three
# problems found there: presence and density counted any harvest from any year;
# the coarse year of harvest dropped earlier harvests in a cell harvested again
# later; and the templates did not line up with the 30 m data.

#' @title Build yearly harvest layers
#' @description For each year in `years` and each resolution, writes one raster:
#'   \describe{
#'     \item{year}{most recent harvest year up to that year, in any of the cell's
#'       30 m cells (NA = no harvest). Gives time since harvest at extraction.}
#'     \item{pa}{1 if any of the cell's 30 m cells was harvested in the last
#'       `recent_years` years (that year included), otherwise 0.}
#'     \item{dens}{proportion of the cell's 30 m cells harvested in the last
#'       `recent_years` years.}
#'     \item{dist}{mean, over the cell's 30 m cells, of the distance (m) to the
#'       nearest 30 m cell harvested in the last `recent_years` years. NA in
#'       years with no recent harvest anywhere in the template.}
#'   }
#'   Distances are measured on the templates' grid, so in the project's
#'   coordinate system (e.g. UTM).
#' @param history data.table. Columns `cell` (cell number in the finest
#'   template) and `year`, one row per harvest.
#' @param metric character. "year", "pa", "dens" or "dist".
#' @param resolutions numeric. Cell sizes (m) to build; each needs a template.
#' @param years integer. Years to build.
#' @param recent_years integer. How many years count as recent harvest.
#' @param templates character. Template files from [make_templates()], finest first.
#' @param out_dir character. Folder to write to.
#' @return character. The files written, named `harvest_<metric>_<res>m_<year>.tif`.
#' @author Julie W. Turner
build_harvest_layers <- function(history, metric, resolutions, years, recent_years, templates, out_dir) {
  metric <- match.arg(metric, c('year', 'pa', 'dens', 'dist'))
  reproducible::checkPath(out_dir, create = TRUE)
  fine <- terra::rast(templates[1])
  base <- terra::res(fine)[1]
  h <- data.table::as.data.table(history)[, .(cell, year)]
  h[, `:=`(row = (cell - 1) %/% terra::ncol(fine), col = (cell - 1) %% terra::ncol(fine))]

  # each harvested cell's coarse cell number, one column per resolution
  coarse <- purrr::map(resolutions, function(res) {
    tr <- terra::rast(template_file(templates, res))
    f <- res / base
    id <- paste0('c', res)
    h[, (id) := (row %/% f) * terra::ncol(tr) + col %/% f + 1]
    list(res = res, f = f, id = id, rast = tr, n = terra::values(tr, mat = FALSE))
  })
  gdal_opts <- c('COMPRESS=DEFLATE', 'TILED=YES')
  type <- switch(metric, year = 'INT2U', pa = 'INT1U', 'FLT4S')

  files <- character()
  for (yr in years) {
    recent <- h[year <= yr & year > yr - recent_years]
    if (metric == 'dist') dist <- harvest_distance(recent$cell, fine)
    for (cc in coarse) {
      out <- file.path(out_dir, sprintf('harvest_%s_%sm_%d.tif', metric, cc$res, yr))
      if (metric == 'dist') {
        if (is.null(dist)) {
          terra::writeRaster(terra::init(cc$rast, NA), out, datatype = type, gdal = gdal_opts, overwrite = TRUE)
        } else {
          terra::aggregate(dist, cc$f, fun = 'mean', na.rm = TRUE, filename = out,
                           datatype = type, gdal = gdal_opts, overwrite = TRUE)
        }
      } else {
        v <- if (metric == 'year') rep(NA_real_, length(cc$n)) else ifelse(is.na(cc$n), NA_real_, 0)
        if (metric == 'year') {
          a <- h[year <= yr, .(v = max(year)), by = c(cc$id)]
          v[a[[1]]] <- a$v
        } else if (metric == 'pa') {
          v[unique(recent[[cc$id]])] <- 1
        } else {
          a <- recent[, .(k = data.table::uniqueN(cell)), by = c(cc$id)]
          v[a[[1]]] <- a$k / cc$n[a[[1]]]
        }
        r <- terra::rast(cc$rast)
        terra::values(r) <- v
        names(r) <- paste0('harvest_', metric)
        terra::writeRaster(r, out, datatype = type, gdal = gdal_opts, overwrite = TRUE)
      }
      files <- c(files, out)
    }
  }
  files
}

#' @title Distance to harvested cells on the finest template
#' @param cells numeric. Harvested cell numbers.
#' @param fine SpatRaster. Finest template.
#' @return SpatRaster of distances (m), NA outside the template; NULL if no cells.
#' @keywords internal
#' @noRd
harvest_distance <- function(cells, fine) {
  if (length(cells) == 0) return(NULL)
  pts <- terra::xyFromCell(fine, unique(cells))
  hr <- terra::rasterize(pts, terra::rast(fine), values = 1, filename = tempfile(fileext = '.tif'))
  d <- terra::distance(hr, filename = tempfile(fileext = '.tif'))
  terra::mask(d, fine, filename = tempfile(fileext = '.tif'))
}

#' @title Pick one layer's files from a builder's output
#' @param files character. Files from a builder, e.g. [build_harvest_layers()].
#' @param prefix character. File name start, e.g. "harvest_year".
#' @param res numeric. Cell size (m).
#' @param first_year,last_year integer. Years wanted.
#' @return character. One file per year, in year order.
#' @author Julie W. Turner
pick_layer_files <- function(files, prefix, res, first_year, last_year) {
  want <- sprintf('%s_%sm_%d.tif', prefix, res, first_year:last_year)
  found <- files[match(want, basename(files))]
  if (anyNA(found)) stop('Not built: ', paste(want[is.na(found)], collapse = ', '), call. = FALSE)
  found
}
