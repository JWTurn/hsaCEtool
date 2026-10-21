# === Targets: extract layer values at steps -------------------------------------
# Comparable to the extractLand SpaDES module. One branch per year (extracted):
# every layer's raster for that year is read at that year's steps and saved as
# a Parquet file. The files hold the step's row number in `steps` and the layer
# values; availability summarizes each layer from them, and the table for
# fitting models will be made from them, with only the layers a model uses.

#' @title Targets: extract every layer at the steps, and summarize availability
#' @inheritParams targets_config
#' @return list of targets.
#' @author Julie W. Turner
#' @export
targets_extract <- function(cfg) {
  if (is.null(cfg$layers) || nrow(cfg$layers) == 0) return(list())
  files <- as.call(c(quote(list), rlang::syms(paste0('layer_files_', cfg$layers$name))))
  layers <- cfg$layers[, .(name, first_year, last_year, time_since, ts_nodata)]
  # built yearly layers with blank years take them from layer_years
  blank <- cfg$layers$yearly & (is.na(cfg$layers$first_year) | is.na(cfg$layers$last_year))
  index_cmd <- if (any(blank)) {
    layers[, yearly := cfg$layers$yearly]
    substitute(make_layer_index(LAYERS, FILES, layer_years), list(LAYERS = as.list(layers), FILES = files))
  } else {
    substitute(make_layer_index(LAYERS, FILES), list(LAYERS = as.list(layers), FILES = files))
  }
  list(
    tar_target_raw('layer_index', index_cmd),
    tar_target(step_points, make_step_points(steps), iteration = 'group'),
    tar_target(extracted_dir, file.path(cfg_paths$outputs, cfg_project$name, 'extracted')),
    tar_target(extracted,
               write_extracted_year(step_points, layer_index, where = cfg_extract$where,
                                    crs = cfg_project$crs, out_dir = extracted_dir),
               pattern = map(step_points), format = 'file'),

    # used and available values of each layer, by jurisdiction, study and herd
    tar_target(step_groups, make_step_groups(steps, cfg_availability_by)),
    tar_target(layer_names, layer_index$name),
    tar_target(availability,
               summarize_availability(extracted, layer_names, step_groups, cfg_availability_by),
               pattern = map(layer_names)),
    tar_target(availability_file,
               write_availability(availability, file.path(cfg_paths$outputs, cfg_project$name)),
               format = 'file')
  )
}
