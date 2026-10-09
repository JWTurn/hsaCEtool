# === Targets: landscape layers -------------------------------------------------
# Comparable to the prepLandscape SpaDES module. One target per row of the
# layer table, layer_files_<name>: the layer's raster file(s), tracked so that a
# changed file reruns that layer's extraction. A row either points to existing
# rasters (path or url) or names a builder (column `build`) that makes the
# layer from source data on the template grids. Builders work one year per
# target, so years can run in parallel (run: workers) and a new year builds
# only that year.
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
    out <- c(out, targets_templates(), targets_harvest(built),
             targets_landcover(built, cfg$landscape$landcover$window_m),
             targets_disturbance(built, cfg$landscape$disturbance))
  }
  out
}
#' @title Targets: template rasters
#' @description The nested templates, and the finest one with a margin
#'   (`landscape: distance_margin_m`) that distances are measured over.
#' @return list of targets.
#' @keywords internal
#' @noRd
targets_templates <- function() {
  list(
    tar_target(landscape_dir, file.path(cfg_paths$outputs, cfg_project$name, 'landscape')),
    tar_target(templates,
               make_templates(study_area_extended, cfg_template, cfg_project$crs, cfg_paths,
                              file.path(landscape_dir, 'templates')),
               format = 'file'),
    tar_target(template_margin,
               make_margin_template(templates[1], cfg_margin,
                                    file.path(landscape_dir, 'templates', 'template_margin.tif')),
               format = 'file')
  )
}
#' @title Targets: harvest history from CanLaD and the harvest layers
#' @description One target per CanLaD annual map (each tracked by its web
#'   address, so a re-released year reruns only that year), one for the
#'   1965-1984 maps, the combined harvest history, then for each harvest
#'   metric (year, pa, dens, dist) one target per output year building every
#'   resolution asked for, and one `layer_files_<name>` per layer-table row.
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
               canlad_annual_harvest(canlad_annual_file, canlad_years, template_margin,
                                     cfg_harvest$canlad_annual$code,
                                     cfg_access$access, cfg_access$cache),
               pattern = map(canlad_annual_file, canlad_years), iteration = 'list'),
    tar_target(canlad_old_files,
               c(cfg_harvest$canlad_old$type_url, cfg_harvest$canlad_old$year_url),
               format = 'url'),
    tar_target(harvest_old,
               canlad_old_harvest(canlad_old_files[1], canlad_old_files[2], template_margin,
                                  cfg_harvest$canlad_old$code,
                                  cfg_access$access, cfg_access$cache)),
    tar_target(harvest_history,
               unique(data.table::rbindlist(c(list(harvest_old), harvest_annual))))
  )
  for (b in unique(harv$build)) {
    rows <- harv[build == b]
    metric <- sub('^harvest_', '', b)
    files_nm <- paste0(b, '_files')
    years_nm <- paste0(b, '_years')
    out <- c(out, list(
      tar_target_raw(years_nm, substitute(seq(A, B), list(A = min(rows$first_year), B = max(rows$last_year)))),
      tar_target_raw(files_nm, substitute(
        build_harvest_layers(harvest_history, METRIC, RES, YEARS, cfg_harvest$recent_years,
                             templates, file.path(landscape_dir, 'harvest'), margin = template_margin),
        list(METRIC = metric, RES = sort(unique(rows$resolution)), YEARS = as.symbol(years_nm))),
        pattern = substitute(map(YEARS), list(YEARS = as.symbol(years_nm))),
        format = 'file')
    ))
    out <- c(out, pick_targets(rows, files_nm, function(i) b))
  }
  out
}
#' @title Targets: yearly land-cover proportions
#' @description One target per year of the land-cover source (SCANFI v3 or
#'   NTEMS VLCE2, `landscape: landcover: source`), each tracked by its web
#'   address and building every class at every resolution asked for, then one
#'   `layer_files_<name>` per layer-table row.
#' @param built data.table. The layer-table rows with a `build`.
#' @param window_m named list. `landscape: landcover: window_m`.
#' @return list of targets.
#' @keywords internal
#' @noRd
targets_landcover <- function(built, window_m) {
  lc <- built[build %in% c('landcover', 'landcover_cell')]
  if (nrow(lc) == 0) return(list())
  out <- list(
    tar_target_raw('landcover_years', substitute(seq(A, B), list(A = min(lc$first_year), B = max(lc$last_year)))),
    tar_target(landcover_source, cfg_landcover[[cfg_landcover$source]]),
    tar_target(landcover_file, fill_year(landcover_source$url, landcover_years),
               pattern = map(landcover_years), format = 'url'),
    tar_target_raw('landcover_files', substitute(
      build_landcover_year(landcover_file, landcover_years, landcover_source$classes, RES,
                           cfg_landcover$window_m, templates,
                           file.path(landscape_dir, 'landcover', cfg_landcover$source),
                           cfg_access$access, cfg_access$cache, landcover_source$archive_file),
      list(RES = sort(unique(lc$resolution)))),
      pattern = quote(map(landcover_file, landcover_years)),
      format = 'file')
  )
  # a `landcover` row at a resolution without a moving window uses the cell proportions
  windows <- names(Filter(function(w) !is.null(w) && w > 0, window_m))
  prefix <- function(i) {
    if (lc$build[i] == 'landcover' && as.character(lc$resolution[i]) %in% windows) {
      paste0('landcover_', lc$class[i])
    } else {
      paste0('landcover_cell_', lc$class[i])
    }
  }
  c(out, pick_targets(lc, 'landcover_files', prefix))
}
#' @title Targets: human disturbance layers
#' @description For each lookup table and source in `landscape: disturbance:`,
#'   a download target (downloading only files not already in the input
#'   cache, or every run with `refresh: true` on that source or lookup)
#'   and, per source, its features sorted into categories. Then for each
#'   category in the layer table: the disturbed 30 m cells and their years, the
#'   years in which they change, and for each metric (dist, pa, dens) one target
#'   per such year, building every resolution asked for. Each source's settings
#'   are written into its own targets, so changing one source's rules reruns
#'   only that source.
#' @param built data.table. The layer-table rows with a `build`.
#' @param dcfg named list. `landscape: disturbance:`.
#' @return list of targets.
#' @keywords internal
#' @noRd
targets_disturbance <- function(built, dcfg) {
  rows <- built[startsWith(build, 'disturb_')]
  if (nrow(rows) == 0) return(list())
  download <- function(nm, url, files, folder, refresh) {
    refresh <- isTRUE(refresh)
    tar_target_raw(nm, substitute(
      download_disturbance(URL, FILES, file.path(cfg_access$cache, 'disturbance', FOLDER), REFRESH),
      list(URL = url, FILES = files, FOLDER = folder, REFRESH = refresh)),
      format = 'file', cue = tar_cue(mode = if (refresh) 'always' else 'thorough'))
  }
  out <- list()
  for (nm in names(dcfg$lookups)) {
    lk <- dcfg$lookups[[nm]]
    out <- c(out, list(download(paste0('disturb_lookup_', nm), lk$url, lk$target_file, nm, lk$refresh)))
  }
  features <- character()
  for (nm in names(dcfg$sources)) {
    src <- dcfg$sources[[nm]]
    dl_nm <- paste0('disturb_download_', nm)
    ft_nm <- paste0('disturb_features_', nm)
    used <- unique(unlist(purrr::map(src$files, function(f) f$lookup$table)))
    lookups <- as.call(c(quote(list), stats::setNames(purrr::map(paste0('disturb_lookup_', used), as.symbol), used)))
    out <- c(out, list(
      download(dl_nm, src$url, purrr::map_chr(src$files, 'file'), nm, src$refresh),
      tar_target_raw(ft_nm, substitute(
        prep_disturbance_features(DL, SRC, LOOKUPS, template_margin, cfg_project$crs,
                                  file.path(landscape_dir, 'disturbance', FILE)),
        list(DL = as.symbol(dl_nm), SRC = src, LOOKUPS = lookups, FILE = paste0('features_', nm, '.gpkg'))),
        format = 'file')
    ))
    features <- c(features, ft_nm)
  }
  all_features <- as.call(c(quote(c), purrr::map(features, as.symbol)))
  for (cl in unique(rows$class)) {
    cr <- rows[class == cl]
    hist_nm <- paste0('disturb_history_', cl)
    years_nm <- paste0('disturb_years_', cl)
    out <- c(out, list(
      tar_target_raw(hist_nm, substitute(disturbance_cells(FEATURES, CL, template_margin),
                                         list(FEATURES = all_features, CL = cl))),
      tar_target_raw(years_nm, substitute(disturbance_years(H, FY, LY),
                                          list(H = as.symbol(hist_nm), FY = min(cr$first_year),
                                               LY = max(cr$last_year))))
    ))
    for (b in unique(cr$build)) {
      metric <- sub('^disturb_', '', b)
      files_nm <- paste0('disturb_', metric, '_', cl, '_files')
      out <- c(out, list(
        tar_target_raw(files_nm, substitute(
          build_disturbance_layers(H, METRIC, RES, YEARS, templates,
                                   file.path(landscape_dir, 'disturbance'), PREFIX, template_margin),
          list(H = as.symbol(hist_nm), METRIC = metric, RES = sort(unique(cr[build == b]$resolution)),
               YEARS = as.symbol(years_nm), PREFIX = paste0('disturb_', cl))),
          pattern = substitute(map(YEARS), list(YEARS = as.symbol(years_nm))),
          format = 'file')
      ))
      out <- c(out, pick_targets(cr[build == b], files_nm,
                                 function(i) paste0('disturb_', cl, '_', metric), fill = TRUE))
    }
  }
  out
}
#' @title One layer_files_<name> target per row, picking its files from a builder
#' @param rows data.table. Layer-table rows.
#' @param files_nm character. Name of the builder's files target.
#' @param prefix function(i) giving row i's file name prefix.
#' @param fill logical. Years without their own file use the latest earlier one.
#' @return list of targets.
#' @keywords internal
#' @noRd
pick_targets <- function(rows, files_nm, prefix, fill = FALSE) {
  purrr::map(seq_len(nrow(rows)), function(i) {
    tar_target_raw(paste0('layer_files_', rows$name[i]), substitute(
      pick_layer_files(FILES, PREFIX, RES, FY, LY, FILL),
      list(FILES = as.symbol(files_nm), PREFIX = prefix(i), RES = rows$resolution[i],
           FY = rows$first_year[i], LY = rows$last_year[i], FILL = fill)),
      format = 'file')
  })
}
