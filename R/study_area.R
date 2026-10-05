#' @title Make the study area
#' @description Reads an existing study-area polygon, or buffers every location
#'   by `buffer_m` metres and joins the buffers into one polygon, optionally
#'   wrapped in a convex hull.
#' @param DT data.table of standard locations.
#' @param sa_cfg named list. The `locations: study_area:` settings.
#' @param file character or NULL. Path to an existing polygon from [get_source()].
#' @param crs character. Project coordinate system.
#' @return SpatVector polygon in the project coordinate system.
#' @details Adapted from NMC_CEtool `scripts/2-clean-data.R` (20 km buffer) and
#'   caribouLocPrep `studyareaFullextent()`.
#' @author Julie W. Turner
make_study_area <- function(DT, sa_cfg, file = NULL, crs) {
  if (length(file)) {
    sa <- terra::vect(file[1])
    return(terra::aggregate(terra::project(sa, crs)))
  }
  pts <- sf::st_as_sf(DT[, .(x_, y_)], coords = c('x_', 'y_'), crs = sf::st_crs(crs))
  sa <- sf::st_union(sf::st_buffer(pts, dist = sa_cfg$buffer_m))
  if (isTRUE(sa_cfg$convex_hull)) sa <- sf::st_convex_hull(sa)
  terra::vect(sa)
}

#' @title Keep locations inside the study area
#' @param DT data.table of standard locations.
#' @param study_area SpatVector polygon.
#' @param method character. "bbox" keeps locations inside the study area's
#'   bounding box (what NMC_CEtool `crop_pts()` did, via `sf::st_crop()`);
#'   "within" keeps those inside the polygon itself.
#' @return data.table.
#' @details Adapted from NMC_CEtool `crop_pts()`.
#' @author Julie W. Turner
crop_locations <- function(DT, study_area, method = 'bbox') {
  if (method == 'bbox') {
    e <- as.vector(terra::ext(study_area))
    return(DT[x_ >= e['xmin'] & x_ <= e['xmax'] & y_ >= e['ymin'] & y_ <= e['ymax']])
  }
  pts <- terra::vect(as.matrix(DT[, .(x_, y_)]), crs = terra::crs(study_area))
  DT[terra::is.related(pts, study_area, 'intersects')]
}

#' @title Use only part of the data, for test runs
#' @param DT data.table of standard locations.
#' @param subset named list. The `subset:` settings: `ids` (these animals),
#'   `n_ids` (first n animals, sorted by id) and `years`. NULL settings use everything.
#' @return data.table.
#' @author Julie W. Turner
subset_locations <- function(DT, subset) {
  if (!is.null(subset$years)) {
    DT <- DT[as.integer(format(t_, '%Y', tz = 'UTC')) %in% unlist(subset$years)]
  }
  if (!is.null(subset$ids)) {
    DT <- DT[id %in% unlist(subset$ids)]
  }
  if (!is.null(subset$n_ids)) {
    DT <- DT[id %in% utils::head(sort(unique(id)), subset$n_ids)]
  }
  DT
}
