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
  # years for built layers with blank first_year or last_year: tracks: min_year
  # and max_year, or (only if one is not set) the steps' years
  out <- if (length(cfg$tracks$min_year) && length(cfg$tracks$max_year)) {
    list(tar_target(layer_years, layer_year_range(cfg_tracks$min_year, cfg_tracks$max_year)))
  } else {
    list(tar_target(layer_years, layer_year_range(cfg_tracks$min_year, cfg_tracks$max_year, step_years)))
  }
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
    # web sources (format url) are checked for changes on every run unless
    # run: check_web_files is false, e.g. while the layers are not being remade
    web_cue <- tar_cue(file = !isFALSE(cfg$run$check_web_files))
    # forest age reset by fire and harvest needs both histories
    age <- built[build == 'forest_age']
    reset <- nrow(age) > 0 && isTRUE(cfg$landscape$forest_age$reset_by_disturbance)
    age_first <- if (reset) age$first_year
    out <- c(out, targets_templates(), targets_harvest(built, web_cue, need_history = reset),
             targets_landcover(built, cfg$landscape$landcover$window_m, web_cue),
             targets_disturbance(built, cfg$landscape$disturbance),
             targets_terrain(built, cfg$landscape$terrain, web_cue),
             targets_fire(built, cfg$landscape$fire, need_history = reset, age_first = age_first),
             targets_forest_age(built, cfg$landscape$forest_age, web_cue))
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
#'   A `harvest_pa`, `harvest_dens` or `harvest_dist` row with a `class` uses
#'   that many years as recent, instead of `landscape: harvest: recent_years`.
#' @param built data.table. The layer-table rows with a `build`.
#' @param web_cue tar_cue for the web sources (see `run: check_web_files`).
#' @param need_history logical. Make the harvest history even without harvest
#'   rows (forest age uses it).
#' @return list of targets.
#' @keywords internal
#' @noRd
targets_harvest <- function(built, web_cue = tar_cue(), need_history = FALSE) {
  harv <- built[startsWith(build, 'harvest_')]
  if (nrow(harv) == 0 && !need_history) return(list())
  out <- list(
    tar_target(canlad_years,
               seq(cfg_harvest$canlad_annual$first_year, cfg_harvest$canlad_annual$last_year)),
    tar_target(canlad_annual_file, fill_year(cfg_harvest$canlad_annual$url, canlad_years),
               pattern = map(canlad_years), format = 'url', cue = web_cue),
    tar_target(harvest_annual,
               canlad_annual_harvest(canlad_annual_file, canlad_years, template_margin,
                                     cfg_harvest$canlad_annual$code,
                                     cfg_access$access, cfg_access$cache),
               pattern = map(canlad_annual_file, canlad_years), iteration = 'list'),
    tar_target(canlad_old_files,
               c(cfg_harvest$canlad_old$type_url, cfg_harvest$canlad_old$year_url),
               format = 'url', cue = web_cue),
    tar_target(harvest_old,
               canlad_old_harvest(canlad_old_files[1], canlad_old_files[2], template_margin,
                                  cfg_harvest$canlad_old$code,
                                  cfg_access$access, cfg_access$cache)),
    tar_target(harvest_history,
               unique(data.table::rbindlist(c(list(harvest_old), harvest_annual))))
  )
  # rows with a window (class) of their own get their own targets and file names
  harv[, key := fifelse(is.na(class) | build == 'harvest_year', build, paste0(build, '_w', class))]
  for (k in unique(harv$key)) {
    rows <- harv[key == k]
    b <- rows$build[1]
    metric <- sub('^harvest_', '', b)
    files_nm <- paste0(k, '_files')
    years_nm <- paste0(k, '_years')
    if (k == b) {
      cmd <- substitute(
        build_harvest_layers(harvest_history, METRIC, RES, YEARS, cfg_harvest$recent_years,
                             templates, file.path(landscape_dir, 'harvest'), margin = template_margin),
        list(METRIC = metric, RES = sort(unique(rows$resolution)), YEARS = as.symbol(years_nm)))
      prefix <- b
    } else {
      win <- as.integer(rows$class[1])
      cmd <- substitute(
        build_harvest_layers(harvest_history, METRIC, RES, YEARS, WIN,
                             templates, file.path(landscape_dir, 'harvest'), PREFIX, margin = template_margin),
        list(METRIC = metric, RES = sort(unique(rows$resolution)), YEARS = as.symbol(years_nm),
             WIN = win, PREFIX = paste0('harvest_w', win)))
      prefix <- paste0('harvest_w', win, '_', metric)
    }
    out <- c(out, list(
      tar_target_raw(years_nm, years_call(rows)),
      tar_target_raw(files_nm, cmd,
        pattern = substitute(map(YEARS), list(YEARS = as.symbol(years_nm))),
        format = 'file')
    ))
    out <- c(out, pick_targets(rows, files_nm, function(i) prefix))
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
#' @param web_cue tar_cue for the web sources (see `run: check_web_files`).
#' @return list of targets.
#' @keywords internal
#' @noRd
targets_landcover <- function(built, window_m, web_cue = tar_cue()) {
  lc <- built[build %in% c('landcover', 'landcover_cell')]
  if (nrow(lc) == 0) return(list())
  out <- list(
    tar_target_raw('landcover_years', years_call(lc)),
    tar_target(landcover_source, cfg_landcover[[cfg_landcover$source]]),
    tar_target(landcover_file, fill_year(landcover_source$url, landcover_years),
               pattern = map(landcover_years), format = 'url', cue = web_cue),
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
                                          list(H = as.symbol(hist_nm), FY = year_bound(cr$first_year, 1),
                                               LY = year_bound(cr$last_year, 2))))
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
#' @title Targets: terrain layers
#' @description The elevation model (NRCan MRDEM-30, tracked by its web
#'   address, or your own raster), then elevation and northness at every
#'   resolution asked for, and one `layer_files_<name>` per layer-table row.
#' @param built data.table. The layer-table rows with a `build`.
#' @param tcfg named list. `landscape: terrain:`.
#' @param web_cue tar_cue for the web sources (see `run: check_web_files`).
#' @return list of targets.
#' @keywords internal
#' @noRd
targets_terrain <- function(built, tcfg, web_cue = tar_cue()) {
  rows <- built[build %in% c('elevation', 'northness')]
  if (nrow(rows) == 0) return(list())
  src <- if (identical(tcfg$source, 'mrdem')) {
    tar_target(terrain_source, cfg_terrain$mrdem$url, format = 'url', cue = web_cue)
  } else {
    tar_target_raw('terrain_source', substitute(get_source(SRC, cfg_paths)[1], list(SRC = tcfg$source)),
                   format = 'file')
  }
  out <- list(
    src,
    tar_target_raw('terrain_files', substitute(
      build_terrain_layers(terrain_source, templates, template_margin, RES, cfg_terrain$northness,
                           file.path(landscape_dir, 'terrain'), cfg_terrain$access, cfg_access$cache),
      list(RES = sort(unique(rows$resolution)))),
      format = 'file')
  )
  c(out, pick_targets(rows, 'terrain_files', function(i) paste0('terrain_', rows$build[i])))
}

#' @title Targets: fire history and the fire layers
#' @description Downloads NBAC and (unless `pre1972: false`) the NFDB fires
#'   before 1972, keeps the fires in the template's area, puts each year's fires
#'   onto the 30 m grid (those up to the first year built together, later
#'   years one target each, in parallel), then for each fire
#'   metric and window of years one target per output year, and one
#'   `layer_files_<name>` per layer-table row. The window of a `fire_pa`,
#'   `fire_dens` or `fire_dist` row is its `class` (years), or `landscape:
#'   fire: recent_years`.
#' @param built data.table. The layer-table rows with a `build`.
#' @param fcfg named list. `landscape: fire:`.
#' @param need_history logical. Make the fire history even without fire rows
#'   (forest age uses it).
#' @param age_first integer or NULL. With forest age reset by disturbance: the
#'   forest age rows' first years (NA = the layer year range's first).
#' @return list of targets.
#' @keywords internal
#' @noRd
targets_fire <- function(built, fcfg, need_history = FALSE, age_first = NULL) {
  rows <- built[startsWith(build, 'fire_')]
  if (nrow(rows) == 0 && !need_history) return(list())
  rows[, window := fifelse(build == 'fire_year', NA_integer_,
                           as.integer(fifelse(is.na(class), as.character(fcfg$recent_years), class)))]
  # every layer needs only each cell's most recent fire up to its year, so fires
  # up to the first year built go onto the grid together, later years one by one
  first <- year_bound(c(rows$first_year, age_first), 1)
  nfdb <- !isFALSE(fcfg$pre1972)
  out <- list(
    tar_target_raw('fire_nbac_download', substitute(
      download_fire(URL, FILE, file.path(cfg_access$cache, 'fire', 'nbac')),
      list(URL = fcfg$nbac$url, FILE = fcfg$nbac$target_file)), format = 'file'),
    if (nfdb) tar_target_raw('fire_nfdb_download', substitute(
      download_fire(URL, FILE, file.path(cfg_access$cache, 'fire', 'nfdb')),
      list(URL = fcfg$nfdb$url, FILE = fcfg$nfdb$target_file)), format = 'file'),
    tar_target_raw('fire_polygons', substitute(
      prep_fire_polygons(fire_nbac_download, NFDB, template_margin, cfg_project$crs,
                         file.path(landscape_dir, 'fire', 'fires.gpkg')),
      list(NFDB = if (nfdb) quote(fire_nfdb_download) else NULL)), format = 'file'),
    tar_target_raw('fire_groups', substitute(fire_year_groups(fire_polygons, FIRST + 1L), list(FIRST = first)),
                   iteration = 'list'),
    tar_target(fire_history_parts, fire_cells(fire_polygons, fire_groups, template_margin),
               pattern = map(fire_groups), iteration = 'list'),
    tar_target(fire_history, unique(data.table::rbindlist(fire_history_parts)))
  )
  out <- Filter(Negate(is.null), out)
  if (nrow(rows) == 0) return(out)
  rows[, key := fifelse(is.na(window), build, paste0(build, '_w', window))]
  for (k in unique(rows$key)) {
    kr <- rows[key == k]
    metric <- sub('^fire_', '', kr$build[1])
    win <- kr$window[1]
    prefix <- if (is.na(win)) 'fire' else paste0('fire_w', win)
    files_nm <- paste0(k, '_files')
    years_nm <- paste0(k, '_years')
    out <- c(out, list(
      tar_target_raw(years_nm, years_call(kr)),
      tar_target_raw(files_nm, substitute(
        build_harvest_layers(fire_history, METRIC, RES, YEARS, WIN, templates,
                             file.path(landscape_dir, 'fire'), PREFIX, margin = template_margin),
        list(METRIC = metric, RES = sort(unique(kr$resolution)), YEARS = as.symbol(years_nm),
             WIN = if (is.na(win)) Inf else win, PREFIX = prefix)),
        pattern = substitute(map(YEARS), list(YEARS = as.symbol(years_nm))),
        format = 'file')
    ))
    out <- c(out, pick_targets(kr, files_nm, function(i) paste0(prefix, '_', metric)))
  }
  out
}

#' @title Targets: yearly forest age
#' @description The forest age maps needed (`landscape: forest_age: source`,
#'   SCANFI every 5 years or NTEMS 2019), each tracked by its web address and
#'   put onto the finest template, then one target per year building every
#'   resolution asked for, and one `layer_files_<name>` per layer-table row.
#'   With `reset_by_disturbance`, uses the fire and harvest histories.
#' @param built data.table. The layer-table rows with a `build`.
#' @param acfg named list. `landscape: forest_age:`.
#' @param web_cue tar_cue for the web sources (see `run: check_web_files`).
#' @return list of targets.
#' @keywords internal
#' @noRd
targets_forest_age <- function(built, acfg, web_cue = tar_cue()) {
  rows <- built[build == 'forest_age']
  if (nrow(rows) == 0) return(list())
  resets <- if (isTRUE(acfg$reset_by_disturbance)) {
    quote(unique(data.table::rbindlist(list(harvest_history, fire_history))))
  } else {
    quote(NULL)
  }
  out <- list(
    tar_target_raw('forest_age_map_years', substitute(
      age_maps_used(unlist(cfg_forest_age[[cfg_forest_age$source]]$years), FY, LY),
      list(FY = year_bound(rows$first_year, 1), LY = year_bound(rows$last_year, 2)))),
    tar_target(forest_age_map_file, fill_year(cfg_forest_age[[cfg_forest_age$source]]$url, forest_age_map_years),
               pattern = map(forest_age_map_years), format = 'url', cue = web_cue),
    tar_target(forest_age_maps,
               prep_forest_age_map(forest_age_map_file, templates,
                                   file.path(landscape_dir, 'forest_age', cfg_forest_age$source,
                                             paste0('map_', forest_age_map_years, '.tif')),
                                   cfg_forest_age[[cfg_forest_age$source]]$archive_file,
                                   isTRUE(cfg_forest_age[[cfg_forest_age$source]]$zero_is_na),
                                   cfg_access$access, cfg_access$cache),
               pattern = map(forest_age_map_file, forest_age_map_years), format = 'file'),
    tar_target_raw('forest_age_resets', resets),
    tar_target_raw('forest_age_years', years_call(rows)),
    tar_target_raw('forest_age_files', substitute(
      build_forest_age_year(forest_age_maps, forest_age_map_years, forest_age_years, forest_age_resets,
                            RES, templates, file.path(landscape_dir, 'forest_age', cfg_forest_age$source),
                            margin = template_margin),
      list(RES = sort(unique(rows$resolution)))),
      pattern = quote(map(forest_age_years)), format = 'file')
  )
  c(out, pick_targets(rows, 'forest_age_files', function(i) 'forest_age'))
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
    yearly <- isTRUE(rows$yearly[i])
    tar_target_raw(paste0('layer_files_', rows$name[i]), substitute(
      pick_layer_files(FILES, PREFIX, RES, FY, LY, FILL),
      list(FILES = as.symbol(files_nm), PREFIX = prefix(i), RES = rows$resolution[i],
           FY = if (yearly) year_bound(rows$first_year[i], 1) else NA,
           LY = if (yearly) year_bound(rows$last_year[i], 2) else NA, FILL = fill)),
      format = 'file')
  })
}

#' @title First or last year of some layer-table rows, for a target's command
#' @description The earliest `first_year` (or latest `last_year`) as a number
#'   when all rows give one; otherwise a call that fills blanks from the
#'   `layer_years` target.
#' @param x integer. The rows' first_year (or last_year).
#' @param which 1 for first (earliest), 2 for last (latest).
#' @return integer, or a call.
#' @keywords internal
#' @noRd
year_bound <- function(x, which) {
  x <- as.integer(x)
  if (length(x) && !anyNA(x)) return(if (which == 1) min(x) else max(x))
  substitute(FUN(fill_years(X, layer_years[I])),
             list(FUN = as.name(if (which == 1) 'min' else 'max'), X = x, I = which))
}

#' @title The years some layer-table rows are built for, for a target's command
#' @param rows data.table. Layer-table rows.
#' @return call: `seq(first, last)`.
#' @keywords internal
#' @noRd
years_call <- function(rows) {
  substitute(seq(A, B), list(A = year_bound(rows$first_year, 1), B = year_bound(rows$last_year, 2)))
}
