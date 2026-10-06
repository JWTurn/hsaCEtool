# === Targets: landscape layers -------------------------------------------------
# Comparable to the prepLandscape SpaDES module. One target per row of the
# layer table, layer_files_<name>: the layer's raster file(s), tracked so that a
# changed file reruns that layer's extraction. A row either points to existing
# rasters (path or url) or names a builder (column `build`) that makes the
# layer from source data on the template grids.

#' @title Targets: landscape layer files
#' @description Rows of the layer table without `build` use existing rasters
#'   (local or downloaded). Rows with `build` are made by the pipeline: first
#'   the template rasters, then the builder's source data and layers.
#' @inheritParams targets_config
#' @return list of targets.
#' @author Julie W. Turner
targets_landscape <- function(cfg) {
  if (is.null(cfg$layers) || nrow(cfg$layers) == 0) return(list())
  existing <- cfg$layers[is.na(build)]
  built <- cfg$layers[!is.na(build)]
  out <- list()
  if (nrow(existing)) {
    out <- c(out, list(tarchetypes::tar_map(
      values = existing[, .(name, path, url, target_file, archive, first_year, last_year)],
      names = 'name',
      tar_target(layer_files,
                 get_layer_files(path, url, target_file, archive, first_year, last_year, cfg_paths),
                 format = 'file')
    )))
  }
  if (nrow(built)) {
    out <- c(out, targets_templates(), targets_harvest(built))
  }
  out
}

#' @title Targets: template rasters
#' @return list of targets.
#' @keywords internal
#' @noRd
targets_templates <- function() {
  list(
    tar_target(landscape_dir, file.path(cfg_paths$outputs, cfg_project$name, 'landscape')),
    tar_target(templates,
               make_templates(study_area, cfg_template, cfg_project$crs, cfg_paths,
                              file.path(landscape_dir, 'templates')),
               format = 'file')
  )
}

#' @title Targets: harvest history from CanLaD and the harvest layers
#' @description One target per CanLaD annual map (each tracked by its web
#'   address, so a re-released year reruns only that year), one for the
#'   1965-1984 maps, the combined harvest history, one target per harvest
#'   metric (year, pa, dens, dist) building every resolution and year asked
#'   for, and one `layer_files_<name>` per layer-table row.
#' @param built data.table. The layer-table rows with a `build`.
#' @return list of targets.
#' @keywords internal
#' @noRd
targets_harvest <- function(built) {
  harv <- built[startsWith(build, 'harvest_')]
  if (nrow(harv) == 0) return(list())
  out <- list(
    tar_target(canlad_years,
               seq(cfg_harvest$canlad_annual$first_year, cfg_harvest$canlad_annual$last_year)),
    tar_target(canlad_annual_file, fill_year(cfg_harvest$canlad_annual$url, canlad_years),
               pattern = map(canlad_years), format = 'url'),
    tar_target(harvest_annual,
               canlad_annual_harvest(canlad_annual_file, canlad_years, templates[1],
                                     cfg_harvest$canlad_annual$code,
                                     cfg_access$access, cfg_access$cache),
               pattern = map(canlad_annual_file, canlad_years), iteration = 'list'),
    tar_target(canlad_old_files,
               c(cfg_harvest$canlad_old$type_url, cfg_harvest$canlad_old$year_url),
               format = 'url'),
    tar_target(harvest_old,
               canlad_old_harvest(canlad_old_files[1], canlad_old_files[2], templates[1],
                                  cfg_harvest$canlad_old$code,
                                  cfg_access$access, cfg_access$cache)),
    tar_target(harvest_history,
               unique(data.table::rbindlist(c(list(harvest_old), harvest_annual))))
  )

  for (b in unique(harv$build)) {
    rows <- harv[build == b]
    metric <- sub('^harvest_', '', b)
    files_nm <- paste0(b, '_files')
    out <- c(out, list(tar_target_raw(files_nm, substitute(
      build_harvest_layers(harvest_history, METRIC, RES, YEARS, cfg_harvest$recent_years,
                           templates, file.path(landscape_dir, 'harvest')),
      list(METRIC = metric, RES = sort(unique(rows$resolution)),
           YEARS = seq(min(rows$first_year), max(rows$last_year)))),
      format = 'file')))
    for (i in seq_len(nrow(rows))) {
      out <- c(out, list(tar_target_raw(paste0('layer_files_', rows$name[i]), substitute(
        pick_layer_files(FILES, PREFIX, RES, FY, LY),
        list(FILES = as.symbol(files_nm), PREFIX = b, RES = rows$resolution[i],
             FY = rows$first_year[i], LY = rows$last_year[i])),
        format = 'file')))
    }
  }
  out
}
