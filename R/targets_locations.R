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
    # only what the readers use, so cleaning or study area edits don't rerun downloads
    tar_target(cfg_locations, cfg$locations[intersect(c('columns', 'input_crs', 'keep_columns', 'tz'),
                                                      names(cfg$locations))]),
    tar_target(cfg_source_duplicates, cfg$locations$source_duplicates),
    tar_target(cfg_cleaning, cfg$locations$cleaning),
    tar_target(cfg_study_area, cfg$locations$study_area),
    tar_target(cfg_crop_method, cfg$locations$crop_method),
    # columns kept after cleaning (with the columns tracks are split by) and how study, herd and juris are made
    tar_target(cfg_finish, list(
      keep = if (length(cfg$locations$keep_after_cleaning)) unique(c(unlist(cfg$locations$keep_after_cleaning), unlist(cfg$tracks$split_by))),
      study = cfg$locations$study, herd = cfg$locations$herd, juris = cfg$locations$juris
    )),
    tar_target(cfg_subset, cfg$subset),
    tar_target(cfg_tracks, cfg$tracks),
    tar_target(cfg_template, cfg$landscape$template),
    tar_target(cfg_margin, cfg$landscape$distance_margin_m %||% 0),
    tar_target(cfg_harvest, cfg$landscape$harvest),
    tar_target(cfg_landcover, cfg$landscape$landcover),
    tar_target(cfg_access, list(
      access = cfg$landscape$access,
      cache = if (nzchar(cfg$paths$input_cache %||% '')) cfg$paths$input_cache else cfg$paths$inputs
    )),
    tar_target(cfg_extract, cfg$extract)
  )
}

#' @title Targets: read, clean and crop caribou locations
#' @description One target per data source in `locations: sources:` (a local
#'   file, a URL, a Movebank study, or the B.C. telemetry exports combined by
#'   [read_bc_locations()], with a `_log` target counting each step), then:
#'   combine (flagging fixes an earlier-listed source also has, see
#'   [flag_source_duplicates()]), clean (logging the fixes each step removed,
#'   what happened to fixes flagged as mortalities, see [check_mortality()],
#'   counts of possible mortalities or dropped collars, see [summarize_flags()],
#'   and how each animal's `mortality_status` was decided (`mortality_rules`, see [mortality_status_rules()])),
#'   make the study area, and keep the locations inside it. Fixes with dates
#'   but no times help make the study area but are then dropped
#'   ([drop_date_only()]). `locations` keeps only the columns needed later,
#'   with `sex` coded and `study`, `herd` and `juris` added ([finish_locations()]); `herd_check`
#'   counts animals by source, jurisdiction, study and herd.
#'
#'   With `locations: premade:` set, a cleaned locations file (saved from
#'   `locations_clean`) is read instead ([read_premade_locations()]) and the
#'   sources are neither read nor cleaned. The study area and extended study
#'   area are each built, or read from their `source:` if one is set.
#' @inheritParams targets_config
#' @return list of targets.
#' @author Julie W. Turner
targets_locations <- function(cfg) {
  src_targets <- list()
  src_names <- character()
  premade <- cfg$locations$premade
  # with premade cleaned locations, the sources are not read
  sources <- if (is.null(premade)) cfg$locations$sources else list()
  for (i in seq_along(sources)) {
    src <- sources[[i]]
    nm <- paste0('locations_source_', i)
    cue <- tar_cue(mode = if (isTRUE(src$refresh)) 'always' else 'thorough')
    if (identical(src$type, 'movebank')) {
      src_targets <- c(src_targets, list(
        # download one at a time, on the main process (Movebank limits parallel
        # requests); depends only on the study, so reading changes don't re-download
        tar_target_raw(paste0(nm, '_download'),
                       substitute(download_movebank(SRC, cfg_paths), list(SRC = src[intersect(c('study', 'user', 'name'), names(src))])),
                       format = 'file', cue = cue, deployment = 'main'),
        tar_target_raw(nm, substitute(read_movebank(FILE, SRC, cfg_locations, cfg_project$crs),
                                      list(FILE = as.symbol(paste0(nm, '_download')), SRC = src)))
      ))
    } else if (identical(src$type, 'bc')) {
      files <- list(NEW = quote(NULL), OLD = quote(NULL), REF = quote(NULL))
      for (part in intersect(c('new', 'old', 'reference'), names(src))) {
        part_nm <- paste0(nm, '_', part)
        part_cue <- tar_cue(mode = if (isTRUE(src$refresh) || isTRUE(src[[part]]$refresh)) 'always' else 'thorough')
        src_targets <- c(src_targets, list(
          tar_target_raw(part_nm, substitute(get_source(SRC, cfg_paths), list(SRC = src[[part]])),
                         format = 'file', cue = part_cue)
        ))
        files[[c(new = 'NEW', old = 'OLD', reference = 'REF')[[part]]]] <- as.symbol(part_nm)
      }
      src_targets <- c(src_targets, list(
        tar_target_raw(nm, substitute(read_bc_locations(NEW, OLD, REF, SRC, cfg_locations, cfg_project$crs),
                                      c(files, list(SRC = src)))),
        tar_target_raw(paste0(nm, '_log'), substitute(attr(X, 'merge_log'), list(X = as.symbol(nm))))
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

  clean_targets <- if (is.null(premade)) {
    combine <- substitute(combine_locations(SRCS, cfg_source_duplicates, cfg_project$crs),
                          list(SRCS = as.call(c(quote(list), purrr::map(src_names, as.symbol)))))
    list(
      tar_target_raw('locations_raw', combine),
      tar_target(locations_clean,
                 clean_locations(locations_raw, cfg_cleaning, cfg_project$crs)),
      tar_target(mortality_check, check_mortality(locations_raw, locations_clean)),
      tar_target(flag_summary, summarize_flags(locations_clean, locations_raw))
    )
  } else {
    # premade cleaned locations, downloaded apart from any older file of the same name
    premade <- utils::modifyList(list(subfolder = 'premade'), premade)
    list(
      tar_target_raw('locations_premade_file', substitute(get_source(SRC, cfg_paths), list(SRC = premade)),
                     format = 'file', cue = tar_cue(mode = if (isTRUE(premade$refresh)) 'always' else 'thorough')),
      tar_target_raw('locations_clean', substitute(read_premade_locations(locations_premade_file, SRC, cfg_project$crs),
                                                   list(SRC = premade))),
      tar_target(flag_summary, summarize_flags(locations_clean))
    )
  }

  sa_src <- cfg$locations$study_area$source
  sa_targets <- if (is.null(sa_src)) {
    list(
      tar_terra_vect(study_area, make_study_area(locations_clean, cfg_study_area,
                                                 file = NULL, crs = cfg_project$crs))
    )
  } else {
    list(
      tar_target_raw('study_area_file', substitute(get_source(SRC, cfg_paths), list(SRC = sa_src)),
                     format = 'file'),
      tar_terra_vect(study_area, make_study_area(locations_clean, cfg_study_area,
                                                 file = study_area_file, crs = cfg_project$crs))
    )
  }
  ext_src <- cfg$locations$study_area$extended$source
  ext_targets <- if (is.null(ext_src)) {
    list(
      tar_terra_vect(study_area_extended,
                     make_study_area_extended(study_area, cfg_study_area$extended))
    )
  } else {
    list(
      tar_target_raw('study_area_extended_file', substitute(get_source(SRC, cfg_paths), list(SRC = ext_src)),
                     format = 'file'),
      tar_terra_vect(study_area_extended,
                     make_study_area_extended(study_area, cfg_study_area$extended,
                                              file = study_area_extended_file))
    )
  }

  c(
    src_targets,
    clean_targets,
    list(
      tar_target(cleaning_log, attr(locations_clean, 'cleaning_log')),
      tar_target(mortality_rules, mortality_status_rules())
    ),
    sa_targets,
    ext_targets,
    list(
      tar_target(locations,
                 crop_locations(drop_date_only(locations_clean), study_area, cfg_crop_method) |>
                   finish_locations(cfg_finish)),
      # animals by source, jurisdiction, study and herd, to check the names (counts only)
      tar_target(herd_check, locations[, .(animals = data.table::uniqueN(id)), keyby = .(source, juris, study, herd)])
    )
  )
}
