# === Targets: extract layer values at steps -------------------------------------
# Comparable to the extractLand SpaDES module. One target per layer
# (extracted_<name>), so changing one layer re-extracts only that layer, then
# issa_data joins them all to the steps.

#' @title Targets: extract every layer at the steps and join them
#' @inheritParams targets_config
#' @return list of targets.
#' @author Julie W. Turner
targets_extract <- function(cfg) {
  if (is.null(cfg$layers) || nrow(cfg$layers) == 0) {
    return(list(tar_target(issa_data, data.table::copy(steps))))
  }
  values <- cfg$layers[, .(name, first_year, last_year, time_since, ts_nodata)]
  values$files <- rlang::syms(paste0('layer_files_', cfg$layers$name))

  mapped <- tarchetypes::tar_map(
    values = values,
    names = 'name',
    tar_target(extracted,
               extract_layer(steps, files, first_year, last_year, name,
                             where = cfg_extract$where, time_since = time_since,
                             ts_nodata = ts_nodata, crs = cfg_project$crs))
  )
  list(
    mapped,
    tarchetypes::tar_combine(issa_data, mapped, command = combine_extracted(steps, !!!.x))
  )
}
