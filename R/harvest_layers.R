# === Harvest layers -----------------------------------------------------------------
# Yearly harvest layers at each resolution, built from the harvest history (cell
# number and year of each harvested 30 m cell, see canlad.R). Replaces the
# harvest section of NMC_CEtool scripts/3-prep-land-UPDATED.R, fixing three
# problems found there: presence and density counted any harvest from any year;
# the coarse year of harvest dropped earlier harvests in a cell harvested again
# later; and the templates did not line up with the 30 m data. The human
# disturbance layers (disturbance.R) are built the same way.

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
#'   coordinate system (e.g. UTM). With `margin`, harvest up to that far
#'   beyond the template counts for distance, so that harvest just outside it
#'   is not missed.
#' @param history data.table. Columns `cell` (cell number in the finest
#'   template, or in `margin` if given) and `year`, one row per harvest.
#' @param metric character. "year", "pa", "dens" or "dist".
#' @param resolutions numeric. Cell sizes (m) to build; each needs a template.
#' @param years integer. Years to build.
#' @param recent_years integer. How many years count as recent harvest.
#' @param templates character. Template files from [make_templates()], finest first.
#' @param out_dir character. Folder to write to.
#' @param prefix character. File name start.
#' @param margin character. Optional: the template with a margin (see
#'   [make_margin_template()]), which `history`'s cell numbers refer to.
#' @return character. The files written, named `<prefix>_<metric>_<res>m_<year>.tif`.
#' @author Julie W. Turner
#' @export
build_harvest_layers <- function(history, metric, resolutions, years, recent_years, templates, out_dir,
                                 prefix = 'harvest', margin = NULL) {
  metric <- match.arg(metric, c('year', 'pa', 'dens', 'dist'))
  reproducible::checkPath(out_dir, create = TRUE)
  fine <- terra::rast(templates[1])
  base <- terra::res(fine)[1]
  resolutions <- sort(unique(resolutions))
  h <- data.table::as.data.table(history)[, .(cell, year)]
  # with a margin, find each cell's row and column in the template; all cells
  # count for distance, those inside the template for the other metrics
  grid <- if (is.null(margin)) fine else terra::rast(margin)
  off_row <- round((terra::ymax(grid) - terra::ymax(fine)) / base)
  off_col <- round((terra::xmin(fine) - terra::xmin(grid)) / base)
  h[, `:=`(row = (cell - 1) %/% terra::ncol(grid) - off_row, col = (cell - 1) %% terra::ncol(grid) - off_col)]
  h_all <- h[, .(cell, year)]
  h <- h[row >= 0 & row < terra::nrow(fine) & col >= 0 & col < terra::ncol(fine)]
  if (!is.null(margin) && nrow(h)) {
    h <- h[!is.na(terra::extract(fine, row * terra::ncol(fine) + col + 1)[[1]])]
  }

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
    if (metric == 'dist') {
      dist <- harvest_distance(h_all[year <= yr & year > yr - recent_years]$cell, fine, grid)
      # mean at the finest resolution asked for; coarser ones from those means,
      # weighted by each cell's number of 30 m cells (same values, much faster)
      if (!is.null(dist)) dist <- terra::aggregate(dist, coarse[[1]]$f, fun = 'mean', na.rm = TRUE)
    }
    for (cc in coarse) {
      out <- file.path(out_dir, sprintf('%s_%s_%sm_%d.tif', prefix, metric, cc$res, yr))
      if (metric == 'dist') {
        if (is.null(dist)) {
          terra::writeRaster(terra::init(cc$rast, NA), out, datatype = type, gdal = gdal_opts, overwrite = TRUE)
        } else if (cc$res == coarse[[1]]$res) {
          terra::writeRaster(dist, out, datatype = type, gdal = gdal_opts, overwrite = TRUE)
        } else {
          n1 <- coarse[[1]]$rast
          fct <- round(cc$res / coarse[[1]]$res)
          d <- terra::aggregate(dist * n1, fct, fun = 'sum', na.rm = TRUE) /
            terra::aggregate(n1, fct, fun = 'sum', na.rm = TRUE)
          terra::writeRaster(d, out, datatype = type, gdal = gdal_opts, overwrite = TRUE)
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
        names(r) <- paste0(prefix, '_', metric)
        terra::writeRaster(r, out, datatype = type, gdal = gdal_opts, overwrite = TRUE)
      }
      files <- c(files, out)
    }
  }
  files
}

#' @title Distance to harvested cells on the finest template
#' @param cells numeric. Harvested cell numbers, in `grid`.
#' @param fine SpatRaster. Finest template.
#' @param grid SpatRaster. The grid `cells` refer to: `fine`, or the template
#'   with a margin (distances are measured over it, then cut to `fine`).
#' @return SpatRaster of distances (m), NA outside the template; NULL if no cells.
#' @keywords internal
#' @noRd
harvest_distance <- function(cells, fine, grid = fine) {
  if (length(cells) == 0) return(NULL)
  tmp <- tempfile(fileext = c('.tif', '.tif', '.tif'))
  on.exit(unlink(tmp), add = TRUE)
  pts <- terra::xyFromCell(grid, unique(cells))
  hr <- terra::rasterize(pts, terra::rast(grid), values = 1, filename = tmp[1])
  d <- terra::distance(hr, filename = tmp[2])
  if (!terra::compareGeom(d, fine, stopOnError = FALSE)) {
    d <- terra::crop(d, fine, snap = 'near', filename = tmp[3])
  }
  terra::mask(d, fine, filename = tempfile(fileext = '.tif'))
}

#' @title Pick one layer's files from a builder's output
#' @param files character. Files from a builder, e.g. [build_harvest_layers()].
#' @param prefix character. File name start, e.g. "harvest_year".
#' @param res numeric. Cell size (m).
#' @param first_year,last_year integer. Years wanted; NA for a layer that does
#'   not change (e.g. elevation), whose file has no year: `<prefix>_<res>m.tif`.
#' @param fill logical. For a year without its own file, use the latest
#'   earlier one (layers built only in years they change, e.g. disturbance).
#' @return character. One file per year, in year order.
#' @author Julie W. Turner
#' @export
pick_layer_files <- function(files, prefix, res, first_year, last_year, fill = FALSE) {
  if (is.na(first_year)) {
    want <- sprintf('%s_%sm.tif', prefix, res)
    found <- files[match(want, basename(files))]
    if (is.na(found)) stop('Not built: ', want, call. = FALSE)
    return(found)
  }
  if (isTRUE(fill)) {
    pat <- sprintf('^%s_%sm_([0-9]{4})[.]tif$', prefix, res)
    mine <- files[grepl(pat, basename(files))]
    built <- as.integer(sub(pat, '\\1', basename(mine)))
    years <- first_year:last_year
    use <- purrr::map_int(years, function(y) if (any(built <= y)) max(built[built <= y]) else NA_integer_)
    if (anyNA(use)) {
      stop('Not built: ', prefix, ' at ', res, ' m for ', paste(years[is.na(use)], collapse = ', '), call. = FALSE)
    }
    return(mine[match(use, built)])
  }
  want <- sprintf('%s_%sm_%d.tif', prefix, res, first_year:last_year)
  found <- files[match(want, basename(files))]
  if (anyNA(found)) stop('Not built: ', paste(want[is.na(found)], collapse = ', '), call. = FALSE)
  found
}
