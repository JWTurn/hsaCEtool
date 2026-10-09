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

#' @title Make the extended study area for the landscape layers
#' @description One continuous area the landscape layers (templates) cover:
#'   the study area, wrapped in a convex hull if `convex_hull` is TRUE, then
#'   buffered by `buffer_m` metres. Layers then include features beyond the
#'   study area that animals may respond to, and avoid edge effects. The hull
#'   is taken before buffering, which gives the same polygon as the hull of
#'   the buffer but is quicker. With `file` (from `extended: source:`), an
#'   existing polygon is used instead.
#' @param study_area SpatVector polygon from [make_study_area()].
#' @param ext_cfg named list. The `locations: study_area: extended:` settings:
#'   `buffer_m` and `convex_hull`.
#' @param file character or NULL. Path to an existing polygon from [get_source()].
#' @return SpatVector polygon in the study area's coordinate system.
#' @details Adapted from NMC_CEtool `scripts/3-prep-land-UPDATED.R` (there a
#'   100 km buffer, then a convex hull).
#' @author Julie W. Turner
make_study_area_extended <- function(study_area, ext_cfg, file = NULL) {
  if (length(file)) {
    return(terra::aggregate(terra::project(terra::vect(file[1]), terra::crs(study_area))))
  }
  sa <- sf::st_union(sf::st_as_sf(study_area))
  if (isTRUE(ext_cfg$convex_hull)) sa <- sf::st_convex_hull(sa)
  if ((ext_cfg$buffer_m %||% 0) > 0) sa <- sf::st_buffer(sa, dist = ext_cfg$buffer_m)
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
