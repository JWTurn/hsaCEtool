# === Targets: settings and caribou locations -----------------------------------
# Comparable to the caribouLocPrep SpaDES module.

#' @title Targets holding each section of the settings
#' @description Each stage reads its own settings section through one of these
#'   targets, so editing one section only reruns the stages that use it.
#' @param cfg named list of settings from [read_config()].
#' @return list of targets.
#' @author Julie W. Turner
targets_config <- function(cfg) {
  list(
    tar_target(cfg_project, cfg$project),
    tar_target(cfg_paths, cfg$paths),
    tar_target(cfg_locations, cfg$locations),
    tar_target(cfg_subset, cfg$subset),
    tar_target(cfg_tracks, cfg$tracks),
    tar_target(cfg_template, cfg$landscape$template),
    tar_target(cfg_harvest, cfg$landscape$harvest),
    tar_target(cfg_access, list(
      access = cfg$landscape$access,
      cache = if (nzchar(cfg$paths$input_cache %||% '')) cfg$paths$input_cache else cfg$paths$inputs
    )),
    tar_target(cfg_extract, cfg$extract)
  )
}

#' @title Targets: read, clean and crop caribou locations
#' @description One target per data source in `locations: sources:` (a local
#'   file, a URL, or a Movebank study), then: combine, clean, make the study
#'   area, and keep the locations inside it.
#' @inheritParams targets_config
#' @return list of targets.
#' @author Julie W. Turner
targets_locations <- function(cfg) {
  src_targets <- list()
  src_names <- character()
  for (i in seq_along(cfg$locations$sources)) {
    src <- cfg$locations$sources[[i]]
    nm <- paste0('locations_source_', i)
    cue <- tar_cue(mode = if (isTRUE(src$refresh)) 'always' else 'thorough')
    if (identical(src$type, 'movebank')) {
      src_targets <- c(src_targets, list(
        tar_target_raw(nm, substitute(download_movebank(SRC, cfg_locations, cfg_project$crs),
                                      list(SRC = src)), cue = cue)
      ))
    } else if (!is.null(src$url)) {
      src_targets <- c(src_targets, list(
        tar_target_raw(nm, substitute(download_locations(SRC, cfg_locations, cfg_project$crs, cfg_paths),
                                      list(SRC = src)), cue = cue)
      ))
    } else {
      file_nm <- paste0(nm, '_file')
      src_targets <- c(src_targets, list(
        tar_target_raw(file_nm, substitute(get_source(SRC, cfg_paths), list(SRC = src)),
                       format = 'file', cue = cue),
        tar_target_raw(nm, substitute(read_locations_file(FILE, SRC, cfg_locations, cfg_project$crs),
                                      list(FILE = as.symbol(file_nm), SRC = src)))
      ))
    }
    src_names <- c(src_names, nm)
  }

  combine <- substitute(data.table::rbindlist(SRCS, fill = TRUE),
                        list(SRCS = as.call(c(quote(list), purrr::map(src_names, as.symbol)))))

  sa_src <- cfg$locations$study_area$source
  sa_targets <- if (is.null(sa_src)) {
    list(
      tar_terra_vect(study_area, make_study_area(locations_clean, cfg_locations$study_area,
                                                 file = NULL, crs = cfg_project$crs))
    )
  } else {
    list(
      tar_target_raw('study_area_file', substitute(get_source(SRC, cfg_paths), list(SRC = sa_src)),
                     format = 'file'),
      tar_terra_vect(study_area, make_study_area(locations_clean, cfg_locations$study_area,
                                                 file = study_area_file, crs = cfg_project$crs))
    )
  }

  c(
    src_targets,
    list(
      tar_target_raw('locations_raw', combine),
      tar_target(locations_clean,
                 clean_locations(locations_raw, cfg_locations$cleaning, cfg_project$crs)),
      tar_target(cleaning_log, attr(locations_clean, 'cleaning_log'))
    ),
    sa_targets,
    list(
      tar_target(locations,
                 crop_locations(locations_clean, study_area, cfg_locations$crop_method))
    )
  )
}
