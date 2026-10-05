# === Targets: landscape layers -------------------------------------------------
# Comparable to the prepLandscape SpaDES module. One target per row of the
# layer table: layer_files_<name>, the layer's raster file(s), tracked so that a
# changed file reruns that layer's extraction.

#' @title Targets: landscape layer files
#' @description With `landscape: mode: file`, finds or downloads each layer's
#'   existing rasters. (`mode: build`, making layers from source data, is still
#'   to be ported.)
#' @inheritParams targets_config
#' @return list of targets.
#' @author Julie W. Turner
targets_landscape <- function(cfg) {
  if (is.null(cfg$layers) || nrow(cfg$layers) == 0) return(list())
  if (!identical(cfg$landscape$mode, 'file')) {
    stop('landscape: mode "', cfg$landscape$mode, '" is not available yet; use mode: file', call. = FALSE)
  }
  values <- cfg$layers[, .(name, path, url, target_file, archive, first_year, last_year)]
  tarchetypes::tar_map(
    values = values,
    names = 'name',
    tar_target(layer_files,
               get_layer_files(path, url, target_file, archive, first_year, last_year, cfg_paths),
               format = 'file')
  )
}
