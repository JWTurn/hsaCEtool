# === Forest age layers --------------------------------------------------------------
# Yearly forest age from maps made every few years (SCANFI v2, 1985-2025 every
# 5 years) or once (NTEMS, 2019). Replaces the forest age section of NMC_CEtool
# scripts/3-prep-land-UPDATED.R, which shifted the 2019 NTEMS map by the years
# from 2019 and so missed stands burned or harvested since.

#' @title Put one forest age map onto the finest template
#' @param sourceFile character. The map, web address or local file.
#' @param templates character. Template files from [make_templates()], finest first.
#' @param out character. File to write.
#' @param archiveFile character. For a zipped map (NTEMS), the file inside the
#'   zip; NULL otherwise.
#' @param zero_is_na logical. TRUE where 0 means not forest (NTEMS).
#' @inheritParams raster_source
#' @return `out`.
#' @author Julie W. Turner
prep_forest_age_map <- function(sourceFile, templates, out, archiveFile = NULL, zero_is_na = FALSE,
                                access = 'stream', cache = 'inputs') {
  reproducible::checkPath(dirname(out), create = TRUE)
  warped <- warp_resample(in_archive(raster_source(sourceFile, access, cache), archiveFile),
                          templates[1], method = 'near')
  on.exit(unlink(warped), add = TRUE)
  r <- terra::rast(warped)
  if (isTRUE(zero_is_na)) r <- terra::subst(r, 0, NA)
  terra::writeRaster(terra::mask(r, terra::rast(templates[1])), out, datatype = 'INT2S',
                     gdal = c('COMPRESS=DEFLATE', 'TILED=YES', 'BIGTIFF=IF_SAFER'), overwrite = TRUE)
  out
}

#' @title Build one year's forest age layers
#' @description Uses the latest map at or before `year` (or, for earlier
#'   years, the first map), adding the years since the map (or taking away
#'   the years before it; ages below 0 become NA, the stand not yet there).
#'   With `resets`, a forested 30 m cell burned or harvested after the map
#'   year, up to `year`, gets the years since its most recent disturbance;
#'   going back before the map, a cell disturbed between `year` and the map
#'   year gets NA (its age then is unknown). Cells that are not forest in the
#'   map stay NA. Each coarser cell gets the mean age of its forested 30 m
#'   cells. Counting is done at the finest resolution asked for, one band of
#'   rows at a time, and coarser ones are built from those sums, so they are exact.
#' @param maps character. Forest age maps on the finest template ([prep_forest_age_map()]).
#' @param map_years integer. The year of each map.
#' @param year integer. Year to build.
#' @param resets data.table or NULL. Columns `cell` (in `margin`) and `year`:
#'   fire and harvest history (see [canlad_annual_harvest()], [fire_cells()]).
#' @param resolutions numeric. Cell sizes (m) to build; each needs a template.
#' @param templates character. Template files from [make_templates()], finest first.
#' @param out_dir character. Folder to write to.
#' @param margin character. The template with a margin, which `resets`' cell
#'   numbers refer to (NULL if they refer to the finest template).
#' @return character. Files written, named `forest_age_<res>m_<year>.tif`.
#' @author Julie W. Turner
build_forest_age_year <- function(maps, map_years, year, resets = NULL, resolutions, templates, out_dir,
                                  margin = NULL) {
  reproducible::checkPath(out_dir, create = TRUE)
  yr <- year   # (data.table columns are also called year and row)
  m <- if (any(map_years <= year)) max(map_years[map_years <= year]) else min(map_years)
  age <- terra::rast(maps[match(m, map_years)])
  fine <- terra::rast(templates[1])
  resolutions <- sort(unique(resolutions))
  res1 <- resolutions[1]
  coarse <- terra::rast(template_file(templates, res1))
  f <- round(res1 / terra::res(fine)[1])
  nc <- terra::ncol(fine)
  ncc <- terra::ncol(coarse)
  cc <- (seq_len(nc) - 1) %/% f + 1

  # disturbances that change the age, as rows and columns of the finest template
  rs <- NULL
  if (!is.null(resets) && nrow(resets)) {
    rs <- to_fine_cells(resets, fine, margin)
    rs <- if (year >= m) {
      rs[year > m & year <= yr, .(year = max(year)), by = .(row, col)]
    } else {
      unique(rs[year > yr & year <= m, .(row, col)])
    }
  }

  out <- terra::rast(coarse, nlyrs = 2)
  terra::readStart(age)
  on.exit(terra::readStop(age), add = TRUE)
  tmp <- tempfile(fileext = '.tif')
  terra::writeStart(out, tmp, overwrite = TRUE, datatype = 'FLT4S')
  for (i in seq_len(terra::nrow(coarse))) {
    r0 <- (i - 1) * f + 1
    nr <- min(f, terra::nrow(fine) - r0 + 1)
    v <- terra::readValues(age, r0, nr, 1, nc) + (year - m)
    if (year < m) v[!is.na(v) & v < 0] <- NA
    if (!is.null(rs)) {
      b <- rs[row >= r0 - 1 & row < r0 - 1 + nr]
      if (nrow(b)) {
        k <- (b$row - (r0 - 1)) * nc + b$col + 1
        if (year >= m) {
          forest <- !is.na(v[k])
          v[k[forest]] <- year - b$year[forest]
        } else {
          v[k] <- NA
        }
      }
    }
    ok <- !is.na(v)
    col <- rep(cc, nr)[ok]
    n <- tabulate(col, nbins = ncc)
    s <- numeric(ncc)
    sums <- rowsum(v[ok], col)
    s[as.integer(rownames(sums))] <- sums[, 1]
    terra::writeValues(out, cbind(ifelse(n > 0, s / n, NA), ifelse(n > 0, n, NA)), i, 1)
  }
  terra::writeStop(out)
  out <- terra::rast(tmp)

  gdal_opts <- c('COMPRESS=DEFLATE', 'TILED=YES')
  files <- character()
  for (res in resolutions) {
    p <- if (res == res1) out else coarser_props(out, round(res / res1))
    r <- terra::mask(p[[1]], terra::rast(template_file(templates, res)))
    names(r) <- 'forest_age'
    file <- file.path(out_dir, sprintf('forest_age_%sm_%d.tif', res, year))
    terra::writeRaster(r, file, datatype = 'FLT4S', gdal = gdal_opts, overwrite = TRUE)
    files <- c(files, file)
  }
  files
}

#' @title Row and column in the finest template of cells numbered in another grid
#' @param history data.table with `cell` and `year`.
#' @param fine SpatRaster. Finest template.
#' @param margin character or NULL. The grid `cell` refers to (NULL = `fine`).
#' @return data.table with `row`, `col` (from 0) and `year`, inside `fine` only.
#' @keywords internal
#' @noRd
to_fine_cells <- function(history, fine, margin = NULL) {
  grid <- if (is.null(margin)) fine else terra::rast(margin)
  base <- terra::res(fine)[1]
  off_row <- round((terra::ymax(grid) - terra::ymax(fine)) / base)
  off_col <- round((terra::xmin(fine) - terra::xmin(grid)) / base)
  h <- data.table::as.data.table(history)[, .(row = (cell - 1) %/% terra::ncol(grid) - off_row,
                                              col = (cell - 1) %% terra::ncol(grid) - off_col, year)]
  h[row >= 0 & row < terra::nrow(fine) & col >= 0 & col < terra::ncol(fine)]
}

#' @title Which forest age maps are needed for some years
#' @param map_years integer. Years with a map.
#' @param first_year,last_year integer. Years to build.
#' @return integer. The maps used: the latest at or before each year (or the
#'   first map, for years before it).
#' @author Julie W. Turner
age_maps_used <- function(map_years, first_year, last_year) {
  map_years <- sort(unique(as.integer(map_years)))
  used <- purrr::map_int(first_year:last_year, function(y) {
    if (any(map_years <= y)) max(map_years[map_years <= y]) else min(map_years)
  })
  sort(unique(used))
}
