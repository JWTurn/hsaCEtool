# === Extracting layer values at steps --------------------------------------------
# Replaces NMC_CEtool extract_by_year(), extract_pt() and calc_ts() (Julie W.
# Turner, Alec L. Robitaille), and the chain of ~80 extraction targets: each
# layer is extracted by its own target and the results are joined at the end.

#' @title Extract one layer's values at the start and/or end of each step
#' @description For a yearly layer, each step gets the raster for its `year`.
#'   Steps after `last_year` use the last raster, and steps before `first_year`
#'   use the first (in NMC_CEtool, `extract_by_year()` filled later years the
#'   same way). If `time_since` is TRUE, also adds the years since the
#'   disturbance year held in the raster; places with no disturbance, or a
#'   disturbance after the step's year, get `ts_nodata`. No disturbance can be
#'   NA or 0 in the raster (e.g. NA in harv1000 but 0 in harv250).
#' @param DT data.table of steps with `x1_`, `y1_`, `x2_`, `y2_` and `year`.
#' @param files character. Raster file(s) from [get_layer_files()], in year order.
#' @param first_year,last_year integer. NA for a layer that does not change.
#' @param name character. Layer name, used for the new columns.
#' @param where character. "start", "end" or "both".
#' @param time_since logical. Add years since disturbance?
#' @param ts_nodata numeric. Years since, where there is no disturbance.
#' @param crs character. Coordinate system of the steps.
#' @return data.table with the new columns only, one row per row of `DT`.
#' @author Julie W. Turner
extract_layer <- function(DT, files, first_year, last_year, name, where = 'both',
                          time_since = FALSE, ts_nodata = NA, crs = NULL) {
  ends <- switch(where, start = 'start', end = 'end', both = c('start', 'end'))
  coords <- list(start = c('x1_', 'y1_'), end = c('x2_', 'y2_'))
  out <- list()
  for (e in ends) {
    vals <- extract_values(DT, coords[[e]], files, first_year, last_year, crs)
    out[[paste(name, e, sep = '_')]] <- vals
    if (isTRUE(time_since)) {
      ts <- DT$year - vals
      ts[is.na(ts) | ts < 0 | vals <= 0] <- ts_nodata
      out[[paste('ts', name, e, sep = '_')]] <- ts
    }
  }
  data.table::as.data.table(out)
}

#' @title Raster values at one set of coordinates, matching each step's year
#' @inheritParams extract_layer
#' @param coords character. Names of the x and y columns.
#' @return numeric vector, one value per row of `DT`.
#' @keywords internal
#' @noRd
extract_values <- function(DT, coords, files, first_year, last_year, crs = NULL) {
  vals <- rep(NA_real_, nrow(DT))
  xy <- as.matrix(DT[, coords, with = FALSE])
  ok <- stats::complete.cases(xy)

  at <- function(file, i) {
    r <- terra::rast(file)
    pts <- xy[i, , drop = FALSE]
    if (!is.null(crs) && !terra::same.crs(r, crs)) {
      pts <- terra::crds(terra::project(terra::vect(pts, crs = crs), terra::crs(r)))
    }
    terra::extract(r[[1]], pts)[[1]]
  }

  if (is.na(first_year)) {
    vals[ok] <- at(files[1], which(ok))
    return(vals)
  }
  yr <- pmin(pmax(DT$year, first_year), last_year)
  for (y in sort(unique(yr[ok]))) {
    i <- which(ok & yr == y)
    vals[i] <- at(files[y - first_year + 1], i)
  }
  vals
}

#' @title Join the extracted layers to the steps
#' @param steps data.table of steps.
#' @param ... data.tables from [extract_layer()], one per layer.
#' @return data.table: `steps` with every layer's columns added.
#' @author Julie W. Turner
combine_extracted <- function(steps, ...) {
  parts <- list(...)
  n <- purrr::map_int(parts, nrow)
  if (any(n != nrow(steps))) stop('Extracted layers do not have one row per step')
  new <- unlist(purrr::map(parts, names))
  clash <- intersect(new, names(steps))
  if (length(clash)) stop('Extracted columns already in the steps: ', paste(clash, collapse = ', '))
  purrr::reduce(parts, cbind, .init = data.table::copy(steps))
}
