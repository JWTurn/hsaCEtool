# === Project settings ------------------------------------------------------
# Reading, combining and checking the settings in config/

#' @title Read a project's settings
#' @description Combines the package's default settings (`inst/config/default.yaml`),
#'   `config/<project>.yaml`,
#'   `config/local.yaml` (settings for this computer only, e.g. folder paths) and
#'   the optional `config/<project>.R`, in that order, later ones winning. If
#'   `locations: sources:` names a CSV file (e.g. `locations_NMC.csv`), the sources
#'   are read from it with [read_location_sources()], and if `locations: cleaning:
#'   params: exclude_points:` names one, the points are read with
#'   [read_exclude_points()], and each disturbance source with a `crosswalk:`
#'   gets its categories from that table ([apply_disturbance_crosswalk()]) (all
#'   before `<project>.R`). The
#'   result is checked with [check_config()].
#' @param project character. Project name, matching `config/<project>.yaml` (e.g. "NMC").
#' @param config_dir character. Folder holding the project's settings files.
#' @param defaults character. The default settings file; normally the one in this package.
#' @return Named list of settings.
#' @author Julie W. Turner
#' @export
read_config <- function(project, config_dir = 'config', defaults = system.file('config', 'default.yaml', package = 'hsaCEtool')) {
  if (!nzchar(project)) {
    stop('No project chosen. Set one first, e.g. Sys.setenv(TAR_PROJECT = "NMC")')
  }
  project_file <- file.path(config_dir, paste0(project, '.yaml'))
  local_file <- file.path(config_dir, 'local.yaml')
  override_file <- file.path(config_dir, paste0(project, '.R'))
  if (!file.exists(project_file)) {
    stop('No settings file for project "', project, '": expected ', project_file)
  }

  if (file.exists(file.path(config_dir, 'default.yaml'))) {
    warning(file.path(config_dir, 'default.yaml'), ' is not read: the defaults come from the hsaCEtool package. ',
            'Put settings that differ in ', project_file, call. = FALSE)
  }
  cfg <- read_settings_file(defaults)
  cfg <- merge_config(cfg, read_settings_file(project_file))
  if (file.exists(local_file)) {
    cfg <- merge_config(cfg, read_settings_file(local_file))
  }
  # sources given as a table: turned into the same settings as typed in the YAML
  src_file <- cfg$locations$sources
  if (is.character(src_file) && length(src_file) == 1) {
    cfg <- apply_location_sources(cfg, read_location_sources(file.path(config_dir, src_file)))
  }
  pts_file <- cfg$locations$cleaning$params$exclude_points
  if (is.character(pts_file) && length(pts_file) == 1) {
    cfg$locations$cleaning$params$exclude_points <- read_exclude_points(file.path(config_dir, pts_file))
  }
  if (length(cfg$landscape$disturbance$sources)) {
    cfg$landscape$disturbance <- apply_disturbance_crosswalks(cfg$landscape$disturbance, config_dir)
  }
  if (file.exists(override_file)) {
    override <- source(override_file, local = new.env())$value
    if (!is.function(override)) {
      stop(override_file, ' must end with a function(cfg) that returns the changed settings')
    }
    cfg <- override(cfg)
  }

  if (nzchar(cfg$landscape$layers %||% '')) {
    cfg$layers <- read_layers(file.path(config_dir, cfg$landscape$layers))
  }
  # seasons: the project's season table, or the package's
  season_file <- cfg$model$season_table %||% ''
  season_file <- if (nzchar(season_file)) {
    file.path(config_dir, season_file)
  } else {
    system.file('extdata', 'seasons_default.csv', package = 'hsaCEtool')
  }
  cfg$seasons <- read_seasons(season_file)
  cfg$project$name <- project
  cfg$config_dir <- config_dir
  check_config(cfg)
  cfg
}

#' @title Read one YAML settings file
#' @description Like `yaml::read_yaml()`, but a bare `y` or `n` stays text
#'   rather than becoming TRUE/FALSE, so `columns: {x: lon, y: lat}` works, and
#'   whole numbers too large for R's integers (e.g. Movebank study IDs) are kept
#'   as numbers rather than becoming NA.
#' @param file character. Path to a .yaml file.
#' @return named list.
#' @keywords internal
#' @noRd
read_settings_file <- function(file) {
  yaml::read_yaml(file, handlers = list(
    'bool#yes' = function(x) if (x %in% c('y', 'Y')) x else TRUE,
    'bool#no' = function(x) if (x %in% c('n', 'N')) x else FALSE,
    'int' = function(x) {
      i <- suppressWarnings(as.integer(x))
      if (is.na(i)) as.numeric(x) else i
    }
  ))
}

#' @title Combine two sets of settings
#' @description Settings in `y` replace those in `x`. Named sections (e.g.
#'   `tracks:`) are combined setting by setting; lists and single values are
#'   replaced whole, so a project can shorten a list such as `cleaning: steps:`.
#' @param x,y named lists of settings.
#' @return Named list.
#' @keywords internal
#' @noRd
merge_config <- function(x, y) {
  is_section <- function(l) is.list(l) && length(l) > 0 && !is.null(names(l)) && all(nzchar(names(l)))
  for (nm in names(y)) {
    if (is_section(x[[nm]]) && is_section(y[[nm]])) {
      x[[nm]] <- merge_config(x[[nm]], y[[nm]])
    } else {
      x[nm] <- list(y[[nm]])
    }
  }
  x
}

#' @title Check a project's settings
#' @description Stops with a plain-language message for the problems that would
#'   otherwise only show up part way through a run.
#' @param cfg named list of settings from [read_config()].
#' @return `cfg`, invisibly.
#' @author Julie W. Turner
#' @export
check_config <- function(cfg) {
  problems <- character()
  add <- function(...) problems <<- c(problems, paste0(...))

  # projection must exist and be in metres
  crs <- sf::st_crs(cfg$project$crs)
  if (is.na(crs)) {
    add('project: crs "', cfg$project$crs, '" is not a known coordinate system')
  } else if (!identical(crs$units_gdal, 'metre')) {
    add('project: crs must have units in metres (step lengths and buffers are in metres)')
  }

  # location sources
  premade <- cfg$locations$premade
  if (!is.null(premade) && is.null(premade$path) && is.null(premade$url)) {
    add('locations: premade: give a path or a url')
  }
  if (length(cfg$locations$sources) == 0 && is.null(premade)) {
    add('locations: sources is empty, so there are no caribou locations to read')
  }
  for (i in seq_along(cfg$locations$sources)) {
    src <- cfg$locations$sources[[i]]
    type <- src$type %||% 'file'
    if (!type %in% c('file', 'movebank', 'bc')) {
      add('locations: sources ', i, ': type must be "file", "movebank" or "bc", not "', type, '"')
    }
    if (type == 'bc' && is.null(src$new$path) && is.null(src$new$url)) {
      add('locations: sources ', i, ': a bc source needs `new:` with a path or a url')
    }
    if (type == 'file' && is.null(src$path) && is.null(src$url)) {
      add('locations: sources ', i, ': give a path or a url')
    }
    if (type == 'movebank' && is.null(src$study)) {
      add('locations: sources ', i, ': a movebank source needs a study')
    }
  }
  # each source's targets are named after it (see source_target_name())
  src_nms <- purrr::imap_chr(cfg$locations$sources, function(src, i) source_target_name(src, i))
  dup <- unique(src_nms[duplicated(src_nms)])
  if (length(dup)) {
    add('locations: sources: two sources have the same name (after replacing characters other than ',
        'letters, digits and _): ', paste(sub('locations_source_', '', dup, fixed = TRUE), collapse = ', '))
  }

  # cleaning steps must exist
  for (step in unlist(cfg$locations$cleaning$steps)) {
    if (is.null(cfg$locations$cleaning$custom[[step]]) &&
        !exists(paste0('clean_', step), mode = 'function')) {
      add('locations: cleaning: unknown step "', step, '"')
    }
  }
  steps <- unlist(cfg$locations$cleaning$steps)
  if ('captures' %in% steps && is.null(cfg$locations$captures)) {
    add('locations: cleaning step captures needs locations: captures (a capture log)')
  }
  if ('mortality_records' %in% steps && length(cfg$locations$mortality_records) == 0) {
    add('locations: cleaning step mortality_records needs locations: mortality_records')
  }

  if (!cfg$locations$crop_method %in% c('bbox', 'within')) {
    add('locations: crop_method must be "bbox" or "within"')
  }
  if (length(cfg$tracks$min_year) && length(cfg$tracks$max_year) && cfg$tracks$min_year > cfg$tracks$max_year) {
    add('tracks: min_year is after max_year')
  }
  if (!cfg$landscape$access %in% c('stream', 'download')) {
    add('landscape: access must be "stream" or "download"')
  }
  if (!is.null(cfg$layers)) {
    builds <- c('harvest_year', 'harvest_pa', 'harvest_dens', 'harvest_dist', 'landcover', 'landcover_cell',
                'disturb_dist', 'disturb_pa', 'disturb_dens', 'elevation', 'northness',
                'fire_year', 'fire_pa', 'fire_dens', 'fire_dist', 'forest_age')
    built <- cfg$layers[!is.na(build)]
    bad <- built[!build %in% builds]$name
    if (length(bad)) {
      add('layer table: unknown build for ', paste(bad, collapse = ', '),
          ' (available: ', paste(builds, collapse = ', '), ')')
    }
    res_ok <- cfg$landscape$template$base_res * unlist(cfg$landscape$template$factors)
    bad <- built[!resolution %in% res_ok]$name
    if (length(bad)) {
      add('layer table: built layers need a resolution of ', paste(res_ok, collapse = ', '),
          ' m (landscape: template: base_res x factors): ', paste(bad, collapse = ', '))
    }
    lc <- built[build %in% c('landcover', 'landcover_cell')]
    if (nrow(lc)) {
      lcs <- cfg$landscape$landcover
      src <- lcs[[lcs$source %||% '']]
      if (is.null(src)) {
        add('landscape: landcover: source "', lcs$source, '" has no settings (e.g. scanfi or ntems)')
      } else {
        bad <- lc[!class %in% unlist(src$classes)]$name
        if (length(bad)) {
          add('layer table: class not in landscape: landcover: ', lcs$source, ': classes: ',
              paste(bad, collapse = ', '))
        }
        fy <- fill_years(lc$first_year, cfg$tracks$min_year %||% NA)
        ly <- fill_years(lc$last_year, cfg$tracks$max_year %||% NA)
        bad <- lc$name[(!is.na(fy) & fy < src$first_year) | (!is.na(ly) & ly > src$last_year)]
        if (length(bad)) {
          add('layer table: ', lcs$source, ' land cover covers ', src$first_year, '-', src$last_year,
              ' only: ', paste(bad, collapse = ', '))
        }
      }
      bad <- setdiff(names(lcs$window_m), as.character(res_ok))
      if (length(bad)) {
        add('landscape: landcover: window_m has resolutions without a template: ', paste(bad, collapse = ', '))
      }
    }
    if (any(built$build %in% c('elevation', 'northness'))) {
      tc <- cfg$landscape$terrain
      if (!identical(tc$source, 'mrdem') && !is.list(tc$source)) {
        add('landscape: terrain: source must be mrdem or a source (path or url)')
      }
      if (!tc$northness %in% c('fine', 'coarse')) add('landscape: terrain: northness must be "fine" or "coarse"')
      if (!tc$access %in% c('stream', 'download')) add('landscape: terrain: access must be "stream" or "download"')
    }
    fr <- built[startsWith(build, 'fire_')]
    if (nrow(fr)) {
      fc <- cfg$landscape$fire
      if (!nzchar(fc$nbac$url %||% '')) add('landscape: fire: nbac needs a url')
      ly <- fill_years(fr$last_year, cfg$tracks$max_year %||% NA)
      bad <- fr$name[!is.na(ly) & ly > fc$last_year]
      if (length(bad)) add('layer table: fire layers go to ', fc$last_year, ' only (landscape: fire: last_year): ',
                           paste(bad, collapse = ', '))
      bad <- fr[build == 'fire_year' & !is.na(class)]$name
      if (length(bad)) add('layer table: fire_year layers take no class: ', paste(bad, collapse = ', '))
    }
    ar <- built[build == 'forest_age']
    if (nrow(ar)) {
      ac <- cfg$landscape$forest_age
      src <- ac[[ac$source %||% '']]
      if (is.null(src)) {
        add('landscape: forest_age: source "', ac$source, '" has no settings (scanfi or ntems)')
      } else if (length(src$years) == 0 || !nzchar(src$url %||% '')) {
        add('landscape: forest_age: ', ac$source, ' needs a url and years')
      }
      if (isTRUE(ac$reset_by_disturbance) && !nzchar(cfg$landscape$fire$nbac$url %||% '')) {
        add('landscape: forest_age: reset_by_disturbance needs landscape: fire: nbac')
      }
    }
    dr <- built[startsWith(build, 'disturb_')]
    if (nrow(dr)) {
      dcfg <- cfg$landscape$disturbance
      if (length(dcfg$sources) == 0) add('layer table: disturbance layers need landscape: disturbance: sources')
      cats <- character()
      for (nm in names(dcfg$sources)) {
        src <- dcfg$sources[[nm]]
        if (!nzchar(src$url %||% '')) add('landscape: disturbance: sources: ', nm, ' needs a url')
        for (f in src$files) {
          if (is.null(f$file)) add('landscape: disturbance: sources: ', nm, ': each of files needs a file')
          if (is.null(f$category) && length(f$rules) == 0) {
            add('landscape: disturbance: sources: ', nm, ': ', f$file, ' needs a category or rules')
          }
          if (!is.null(f$lookup) && is.null(dcfg$lookups[[f$lookup$table %||% '']])) {
            add('landscape: disturbance: sources: ', nm, ': ', f$file, ' uses lookup "', f$lookup$table,
                '", which is not in landscape: disturbance: lookups')
          }
          cats <- c(cats, f$category, purrr::map_chr(f$rules, function(r) r$category %||% NA_character_))
        }
      }
      bad <- dr[!class %in% setdiff(cats, 'drop')]$name
      if (length(bad)) {
        add('layer table: class not given to any feature by landscape: disturbance: sources: ',
            paste(bad, collapse = ', '))
      }
    }
  }
  if (!cfg$extract$where %in% c('start', 'end', 'both')) {
    add('extract: where must be "start", "end" or "both"')
  }
  # iSSA models (see R/models.R)
  for (p in check_model_settings(cfg$model %||% list(), cfg$tracks$n_random, cfg$seasons)) add(p)

  if (length(problems)) {
    stop('Problems in the settings for project "', cfg$project$name, '":\n  - ',
         paste(problems, collapse = '\n  - '), call. = FALSE)
  }
  invisible(cfg)
}
