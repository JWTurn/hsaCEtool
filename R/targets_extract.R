# === Targets: extract layer values at steps -------------------------------------
# Comparable to the extractLand SpaDES module. One branch per year (extracted):
# every layer's raster for that year is read at that year's steps. The tables
# hold the step's row number in `steps` and the layer values; the table for
# fitting models is made from them later, with only the layers a model uses.

#' @title Targets: extract every layer at the steps, one year at a time
#' @inheritParams targets_config
#' @return list of targets.
#' @author Julie W. Turner
targets_extract <- function(cfg) {
  if (is.null(cfg$layers) || nrow(cfg$layers) == 0) return(list())
  files <- as.call(c(quote(list), rlang::syms(paste0('layer_files_', cfg$layers$name))))
  layers <- cfg$layers[, .(name, first_year, last_year, time_since, ts_nodata)]
  list(
    tar_target_raw('layer_index', substitute(make_layer_index(LAYERS, FILES),
                                             list(LAYERS = as.list(layers), FILES = files))),
    tar_target(step_points, make_step_points(steps), iteration = 'group'),
    tar_target(extracted,
               extract_year(step_points, layer_index, where = cfg_extract$where, crs = cfg_project$crs),
               pattern = map(step_points))
  )
}
