# === Extracting layer values at steps --------------------------------------------
# Replaces NMC_CEtool extract_by_year(), extract_pt() and calc_ts() (Julie W.
# Turner, Alec L. Robitaille). As in extractLand's extractLandscape(), steps are
# extracted one year at a time: that year's raster of every layer is stacked
# (one stack per grid) and read at the step points in one call, so each raster
# is opened once per year and no layer needs its own copy of the steps.

#' @title Layer table with each layer's files
#' @description Adds the files found for each layer ([get_layer_files()]) to
#'   the layer table, for [extract_year()].
#' @param layers data.table from [read_layers()].
#' @param files list of character vectors, one per row of `layers`, in order.
#' @return data.table: `name`, `first_year`, `last_year`, `time_since`,
#'   `ts_nodata` and list column `files`.
#' @author Julie W. Turner
make_layer_index <- function(layers, files) {
  if (length(files) != nrow(layers)) stop('Need one set of files per layer')
  idx <- data.table::as.data.table(layers)[, .(name, first_year, last_year, time_since, ts_nodata)]
  idx[, files := unname(files)]
  idx[]
}

#' @title Step points, grouped by year
#' @description Only what extraction needs, grouped so that each year is
#'   extracted by its own branch.
#' @param steps data.table from [prep_steps()].
#' @return data.table: `row` (row number in `steps`), `year`, `x1_`, `y1_`,
#'   `x2_`, `y2_` and `tar_group`, sorted by year.
#' @author Julie W. Turner
make_step_points <- function(steps) {
  pts <- steps[, .(row = .I, year, x1_, y1_, x2_, y2_)]
  data.table::setorder(pts, year, row)
  pts[, tar_group := .GRP, by = year]
  pts[]
}

#' @title Extract every layer at one year's steps
#' @description Each layer's raster for the steps' year is used. Steps after
#'   `last_year` use the last raster, and steps before `first_year` the first
#'   (in NMC_CEtool, `extract_by_year()` filled later years the same way);
#'   layers that do not change use their one raster. Rasters on the same grid
#'   are stacked and read in one call. Each distinct point is read once: start
#'   and end points are read together, and an observed step and its random
#'   steps share a start. Points are reprojected once per grid whose coordinate
#'   system differs from `crs`.
#'
#'   For a layer with `time_since`, also adds the years since the disturbance
#'   year held in the raster; places with no disturbance, or a disturbance
#'   after the step's year, get `ts_nodata`. No disturbance can be NA or 0 in
#'   the raster (e.g. NA in harv1000 but 0 in harv250).
#' @param pts data.table from [make_step_points()], for one year.
#' @param index data.table from [make_layer_index()].
#' @param where character. "start", "end" or "both".
#' @param crs character. Coordinate system of the steps.
#' @return data.table, one row per row of `pts`: `row`, then for each layer
#'   `<name>_start` (with `ts_<name>_start`) and `<name>_end` (with `ts_<name>_end`).
#' @author Julie W. Turner
extract_year <- function(pts, index, where = 'both', crs = NULL) {
  yr <- unique(pts$year)
  if (length(yr) != 1) stop('extract_year() needs the steps of one year, not ', length(yr))
  ends <- switch(where, start = 'start', end = 'end', both = c('start', 'end'))
  coords <- list(start = c('x1_', 'y1_'), end = c('x2_', 'y2_'))
  n <- nrow(pts)

  # distinct points, start and end together
  P <- data.table::rbindlist(purrr::map(ends, function(e) {
    data.table::setnames(pts[, coords[[e]], with = FALSE], c('x', 'y'))
  }))
  U <- unique(P[!is.na(x) & !is.na(y)])
  U[, u := .I]
  pos <- U[P, on = c('x', 'y'), u]
  xy <- as.matrix(U[, .(x, y)])

  # each layer's raster for this year, stacked by grid
  file_for <- function(files, first_year, last_year) {
    if (is.na(first_year)) return(files[1])
    files[min(max(yr, first_year), last_year) - first_year + 1]
  }
  files <- purrr::pmap_chr(list(index$files, index$first_year, index$last_year), file_for)
  rasts <- purrr::map(files, function(f) terra::rast(f)[[1]])
  grid <- purrr::map_chr(rasts, grid_key)
  vals <- list()
  for (g in unique(grid)) {
    i <- which(grid == g)
    s <- terra::rast(rasts[i])
    p <- xy
    if (!is.null(crs) && !terra::same.crs(s, crs)) {
      p <- terra::crds(terra::project(terra::vect(xy, crs = crs), terra::crs(s)))
    }
    v <- terra::extract(s, p)
    vals[index$name[i]] <- as.list(v)
  }

  out <- data.table::data.table(row = pts$row)
  for (k in seq_len(nrow(index))) {
    nm <- index$name[k]
    for (j in seq_along(ends)) {
      v <- vals[[nm]][pos[(j - 1) * n + seq_len(n)]]
      data.table::set(out, j = paste(nm, ends[j], sep = '_'), value = v)
      if (isTRUE(index$time_since[k])) {
        ts <- yr - v
        ts[is.na(ts) | ts < 0 | v <= 0] <- index$ts_nodata[k]
        data.table::set(out, j = paste('ts', nm, ends[j], sep = '_'), value = ts)
      }
    }
  }
  out[]
}

#' @title Extract one year and save it as a Parquet file
#' @description Runs [extract_year()] and writes the table to
#'   `extracted_<year>.parquet` in `out_dir`. Parquet stores data by column, so
#'   later steps read only the columns they need (`nanoparquet::read_parquet(
#'   col_select = )`). The files hold step row numbers with layer values, so
#'   they are confidential like the steps (kept in `outputs/`).
#' @inheritParams extract_year
#' @param out_dir character. Folder to write to.
#' @return character. The file written.
#' @author Julie W. Turner
write_extracted_year <- function(pts, index, where = 'both', crs = NULL, out_dir) {
  out <- extract_year(pts, index, where = where, crs = crs)
  reproducible::checkPath(out_dir, create = TRUE)
  file <- file.path(out_dir, paste0('extracted_', unique(pts$year), '.parquet'))
  nanoparquet::write_parquet(out, file)
  file
}

#' @title Text that is the same for rasters on the same grid
#' @param r SpatRaster.
#' @return character.
#' @keywords internal
#' @noRd
grid_key <- function(r) {
  paste(c(as.vector(terra::ext(r)), terra::nrow(r), terra::ncol(r), terra::crs(r, proj = TRUE)), collapse = '|')
}
