# === Project settings ------------------------------------------------------
# Reading, combining and checking the settings in config/

#' @title Read a project's settings
#' @description Combines `config/default.yaml`, `config/<project>.yaml`,
#'   `config/local.yaml` (settings for this computer only, e.g. folder paths) and
#'   the optional `config/<project>.R`, in that order, later ones winning. The
#'   result is checked with [check_config()].
#' @param project character. Project name, matching `config/<project>.yaml` (e.g. "NMC").
#' @param config_dir character. Folder holding the settings files.
#' @return Named list of settings.
#' @author Julie W. Turner
read_config <- function(project, config_dir = 'config') {
  if (!nzchar(project)) {
    stop('No project chosen. Set one first, e.g. Sys.setenv(TAR_PROJECT = "NMC")')
  }
  project_file <- file.path(config_dir, paste0(project, '.yaml'))
  local_file <- file.path(config_dir, 'local.yaml')
  override_file <- file.path(config_dir, paste0(project, '.R'))
  if (!file.exists(project_file)) {
    stop('No settings file for project "', project, '": expected ', project_file)
  }

  cfg <- read_settings_file(file.path(config_dir, 'default.yaml'))
  cfg <- merge_config(cfg, read_settings_file(project_file))
  if (file.exists(local_file)) {
    cfg <- merge_config(cfg, read_settings_file(local_file))
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
  cfg$project$name <- project
  cfg$config_dir <- config_dir
  check_config(cfg)
  cfg
}

#' @title Read one YAML settings file
#' @description Like `yaml::read_yaml()`, but a bare `y` or `n` stays text
#'   rather than becoming TRUE/FALSE, so `columns: {x: lon, y: lat}` works.
#' @param file character. Path to a .yaml file.
#' @return named list.
#' @keywords internal
#' @noRd
read_settings_file <- function(file) {
  yaml::read_yaml(file, handlers = list(
    'bool#yes' = function(x) if (x %in% c('y', 'Y')) x else TRUE,
    'bool#no' = function(x) if (x %in% c('n', 'N')) x else FALSE
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
  if (length(cfg$locations$sources) == 0) {
    add('locations: sources is empty, so there are no caribou locations to read')
  }
  for (i in seq_along(cfg$locations$sources)) {
    src <- cfg$locations$sources[[i]]
    type <- src$type %||% 'file'
    if (!type %in% c('file', 'movebank')) {
      add('locations: sources ', i, ': type must be "file" or "movebank", not "', type, '"')
    }
    if (type == 'file' && is.null(src$path) && is.null(src$url)) {
      add('locations: sources ', i, ': give a path or a url')
    }
    if (type == 'movebank' && (is.null(src$study) || is.null(src$user))) {
      add('locations: sources ', i, ': a movebank source needs a study and a user')
    }
  }

  # cleaning steps must exist
  for (step in unlist(cfg$locations$cleaning$steps)) {
    if (is.null(cfg$locations$cleaning$custom[[step]]) &&
        !exists(paste0('clean_', step), mode = 'function')) {
      add('locations: cleaning: unknown step "', step, '"')
    }
  }

  if (!cfg$locations$crop_method %in% c('bbox', 'within')) {
    add('locations: crop_method must be "bbox" or "within"')
  }
  if (cfg$tracks$min_year > cfg$tracks$max_year) {
    add('tracks: min_year is after max_year')
  }
  if (!cfg$landscape$mode %in% c('file', 'build')) {
    add('landscape: mode must be "file" or "build"')
  }
  if (!cfg$extract$where %in% c('start', 'end', 'both')) {
    add('extract: where must be "start", "end" or "both"')
  }

  if (length(problems)) {
    stop('Problems in the settings for project "', cfg$project$name, '":\n  - ',
         paste(problems, collapse = '\n  - '), call. = FALSE)
  }
  invisible(cfg)
}
