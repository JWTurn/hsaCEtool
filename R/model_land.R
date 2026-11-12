# === Layer stacks for maps (model land) -----------------------------------------
# Replaces NMC_CEtool scripts/7-SSUDmaps.R (Julie W. Turner). For each year of
# interest, one GeoTIFF holding every covariate the models use, made from the
# layers the same way the model tables are (build_model_data()): the same
# year's raster of each layer, groups combined as combine_columns() does, time
# since disturbance (left NA where there was none, for caribou_SSUD to fill in
# for its forecasts; the model tables use ts_nodata), and distances capped as
# in model: distance_cap: (as a second layer, <name>_adj). Layers are named as
# in the formulas without _start or _end (e.g. prop_forest_240), so a model
# can be mapped for that year from the stack alone (as in caribou_SSUD).

#' @title Formulas whose covariates go in the stacks
#' @param models named list. `model: models:`.
#' @param which character. `all`, or model names (`model_land: models:`).
#' @return character. The formulas.
#' @author Julie W. Turner
#' @export
model_land_formulas <- function(models, which = 'all') {
  which <- unlist(which %||% 'all')
  if (!identical(which, 'all')) {
    unknown <- setdiff(which, names(models))
    if (length(unknown)) stop('model_land: models: not models in the settings: ', paste(unknown, collapse = ', '), call. = FALSE)
    models <- models[which]
  }
  unname(purrr::map_chr(models, 'formula'))
}

#' @title What each stack holds
#' @description Every covariate in the formulas that is read from the layers
#'   (named with `_start` or `_end`), named without `_start` or `_end`, with
#'   what it is made from:
#'   - a layer (e.g. `dist_paved_240`): that layer;
#'   - a group from `model: groups:` (e.g. `prop_forest_240`): its layers,
#'     combined by its rule as in the model tables;
#'   - time since disturbance (e.g. `ts_fire_240`): the year minus the layer
#'     `fire_240`, for a layer with `time_since` in the layer table.
#'   A covariate matching `distance_cap: columns` also gets a capped copy,
#'   `<name>_adj`, right after it.
#' @param formulas character. Model formulas.
#' @param groups named list. `model: groups:`.
#' @param cap named list or NULL. `model: distance_cap:`.
#' @param index data.table from [make_layer_index()].
#' @return data.table, one row per stack layer: `name`, `cols` (list: the
#'   layers it is made from), `rule` (NA for one layer), `ts` (time since),
#'   `adj` (capped copy) and `resolution` (cell size of its layers, from their
#'   names; NA if not in the name).
#' @author Julie W. Turner
#' @export
make_model_land_spec <- function(formulas, groups, cap, index) {
  vars <- unique(unlist(purrr::map(formulas, function(f) all.vars(model_formula(f)))))
  cov <- unique(sub('_(start|end)$', '', vars[grepl('_(start|end)$', vars)]))
  if (!length(cov)) stop('The model formulas use no layers (names ending in _start or _end)', call. = FALSE)
  src <- covariate_sources(cov, groups %||% list())
  ts_layers <- index$name[index$time_since %in% TRUE]
  spec <- data.table::rbindlist(purrr::imap(src, function(s, nm) {
    ts <- is.null(s$rule) && !nm %in% index$name && startsWith(nm, 'ts_') && sub('^ts_', '', nm) %in% ts_layers
    cols <- if (ts) sub('^ts_', '', nm) else s$cols
    data.table::data.table(name = nm, cols = list(cols), rule = s$rule %||% NA_character_, ts = ts, adj = FALSE)
  }))
  miss <- setdiff(unlist(spec$cols), index$name)
  if (length(miss)) stop('The model formulas use layers not in the layer table: ', paste(miss, collapse = ', '), call. = FALSE)
  if (length(cap)) {
    capped <- spec[grepl(cap$columns %||% '^dist_', name)]
    if (nrow(capped)) {
      capped[, `:=`(name = paste0(name, '_adj'), adj = TRUE)]
      spec[, .o := .I]
      capped[, .o := match(sub('_adj$', '', name), spec$name) + 0.5]
      spec <- data.table::rbindlist(list(spec, capped))
      data.table::setorder(spec, .o)
      spec[, .o := NULL]
    }
  }
  spec[, resolution := purrr::map_dbl(cols, function(cl) {
    r <- suppressWarnings(as.numeric(sub('.*_([0-9]+)$', '\\1', cl)))
    if (all(is.na(r))) NA_real_ else min(r, na.rm = TRUE)
  })]
  spec[]
}

#' @title Save the list of stack layers
#' @description A CSV describing each layer of the stacks, put in each
#'   uploaded zip so the stacks can be used without the pipeline.
#' @param spec data.table from [make_model_land_spec()].
#' @param cap named list or NULL. `model: distance_cap:`.
#' @param out_dir character. Folder to write to.
#' @return character. The file written (`model_land_layers.csv`).
#' @author Julie W. Turner
#' @export
write_model_land_spec <- function(spec, cap, out_dir) {
  reproducible::checkPath(out_dir, create = TRUE)
  made <- purrr::pmap_chr(list(spec$cols, spec$rule, spec$ts, spec$adj), function(cols, rule, ts, adj) {
    txt <- if (ts) paste0('years since the disturbance year in ', cols, '; NA where none (no ', cols, ' value, or one after the year)')
    else if (is.na(rule)) cols
    else paste0(rule, ' of ', paste(cols, collapse = ', '))
    if (adj) txt <- paste0(txt, '; more than ', cap$beyond_km, ' km set to ', cap$set_to_km, ' km')
    txt
  })
  out <- data.table::data.table(layer = seq_len(nrow(spec)), name = spec$name, made_from = made)
  file <- file.path(out_dir, 'model_land_layers.csv')
  data.table::fwrite(out, file)
  file
}

#' @title A layer's raster file for a year
#' @description As in extraction: years after `last_year` use the last raster,
#'   years before `first_year` the first, and a layer that does not change its
#'   one raster.
#' @param files character. The layer's files, one per year from `first_year`.
#' @param first_year,last_year integer or NA.
#' @param year integer.
#' @return character.
#' @keywords internal
#' @noRd
layer_file_for_year <- function(files, first_year, last_year, year) {
  if (is.na(first_year)) return(files[1])
  files[min(max(year, first_year), last_year) - first_year + 1]
}

#' @title Combine layers by a rule, cell by cell
#' @description The raster version of `combine_columns()` (used for the model
#'   tables), with the same result: missing values are skipped (a sum counts
#'   them as 0), and a cell is missing only where all layers are. Uses terra's
#'   own functions, as calling `combine_columns()` through [terra::app()] is
#'   thousands of times slower.
#' @param r SpatRaster. The layers to combine.
#' @param rule character. sum, min, max or mean.
#' @return SpatRaster, one layer.
#' @keywords internal
#' @noRd
combine_layers <- function(r, rule) {
  if (!rule %in% c('sum', 'mean', 'min', 'max')) stop('Unknown rule: ', rule, call. = FALSE)
  # the function by name, which terra runs in its compiled code
  out <- terra::app(r, rule, na.rm = TRUE)
  terra::mask(out, terra::allNA(r), maskvalues = 1)
}

#' @title Make one year's layer stack
#' @description Writes `model_land_<year>.tif` in `out_dir`: one layer per row
#'   of `spec`, named by `name`, made as the model tables are made from the
#'   extracted values:
#'   - each layer's raster for `year` (years outside a layer's years use its
#'     first or last raster; layers that do not change, their one raster);
#'   - groups combined cell by cell by the same rules as the model tables
#'     (`combine_layers()`, which gives what `combine_columns()` gives:
#'     missing layers skipped, sum counting them as 0; missing only where all are);
#'   - time since disturbance: `year` minus the raster's value; no disturbance
#'     (NA or 0), or one after `year`, is left NA (the model tables use
#'     `ts_nodata` there; caribou_SSUD fills it in, so forecasts from the
#'     stack's year can age the disturbances);
#'   - `_adj` layers: more than `beyond_km` gives `set_to_km`. In the model
#'     tables a step is capped by where it starts; for a map cell, the start is
#'     the cell itself.
#'   All layers are put on the grid of the finest layer used (coarser layers
#'   by nearest cell; the grids nest, so each fine cell gets the coarse
#'   cell it lies in, the value a point there would be given) and masked to
#'   the template of that cell size, when given.
#' @param year integer. One year.
#' @param index data.table from [make_layer_index()].
#' @param spec data.table from [make_model_land_spec()].
#' @param cap named list or NULL. `model: distance_cap:`.
#' @param templates character or NULL. Template files ([make_templates()]); the
#'   one with the stack's cell size masks it. NULL = no mask.
#' @param out_dir character. Folder to write to.
#' @return character. The file written.
#' @author Julie W. Turner
#' @export
make_model_land <- function(year, index, spec, cap, templates = NULL, out_dir) {
  year <- as.integer(year)
  if (length(year) != 1 || is.na(year)) stop('make_model_land() needs one year', call. = FALSE)
  used <- unique(unlist(spec$cols))
  idx <- index[match(used, index$name)]
  files <- stats::setNames(purrr::pmap_chr(list(idx$files, idx$first_year, idx$last_year), layer_file_for_year, year = year), used)
  rasts <- purrr::map(files, function(f) terra::rast(f)[[1]])

  # the grid: the finest layer's
  res <- purrr::map_dbl(rasts, function(r) terra::res(r)[1])
  ref <- rasts[[which.min(res)]]
  mask <- NULL
  if (length(templates)) {
    tf <- templates[basename(templates) == paste0('template_', min(res), 'm.tif')]
    if (length(tf) == 1) {
      mask <- terra::rast(tf)
      ref <- mask
    }
  }
  on_grid <- function(r) {
    if (!terra::same.crs(r, ref)) r <- terra::project(r, ref, method = 'near')
    if (!isTRUE(terra::compareGeom(r, ref, stopOnError = FALSE))) r <- terra::resample(r, ref, method = 'near')
    r
  }
  rasts <- purrr::map(rasts, on_grid)

  layers <- purrr::map(seq_len(nrow(spec)), function(k) {
    cols <- spec$cols[[k]]
    if (spec$ts[k]) {
      v <- rasts[[cols]]
      # no disturbance stays NA, not ts_nodata: filled in later (caribou_SSUD)
      ts <- year - v
      out <- terra::ifel(is.na(v) | v <= 0 | ts < 0, NA, ts)
    } else if (is.na(spec$rule[k])) {
      out <- rasts[[cols]]
    } else {
      out <- combine_layers(terra::rast(unname(rasts[cols])), spec$rule[k])
    }
    if (spec$adj[k]) out <- terra::ifel(out > cap$beyond_km * 1000, cap$set_to_km * 1000, out)
    out
  })
  s <- terra::rast(layers)
  if (!is.null(mask)) s <- terra::mask(s, mask, maskvalues = c(NA, 0))
  names(s) <- spec$name
  reproducible::checkPath(out_dir, create = TRUE)
  file <- file.path(out_dir, paste0('model_land_', year, '.tif'))
  terra::writeRaster(s, file, overwrite = TRUE, datatype = 'FLT4S',
                     gdal = c('COMPRESS=DEFLATE', 'TILED=YES', 'BIGTIFF=IF_SAFER'))
  file
}
