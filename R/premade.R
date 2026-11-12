# === Premade files: cleaned locations and study areas, shared on Drive ---------
# So that later runs (or other projects and people) can start from the cleaned
# locations and study areas instead of downloading and cleaning every source
# (see locations: premade: and study_area: source: in the settings). The cleaned
# locations are caribou locations: they go only to a restricted folder. The study
# areas may be shared more widely.

#' @title Write the premade files from the current results
#' @description Zips, in `outputs/<project>/premade/`:
#'   \describe{
#'     \item{clean_<project>.zip}{`clean_<project>.rds`, the cleaned locations
#'       (target `locations_clean`, with its cleaning log), read by
#'       `locations: premade:`}
#'     \item{clean_<project>_<buffer>kmBuff.zip}{`.gpkg`, the study area}
#'     \item{<project>_studyArea_extended.zip}{`.gpkg`, the extended study area}
#'   }
#'   GeoPackages keep the geometry exactly, so an area read back from one
#'   gives the same templates (shapefiles can differ slightly).
#' @param project character or `NULL`. The settings name (e.g. "NMC", for
#'   `<config_dir>/NMC.yaml`) or a targets project listed in `_targets.yaml`;
#'   `NULL` (default) = the targets project in `TAR_PROJECT`.
#' @param config_dir character or `NULL`. Folder of the settings; `NULL`
#'   (default) = `config/` beside the targets project's script (`_targets.R`).
#' @details The settings and the targets store are found the same way in a
#'   repository with one targets project and its settings in `config/` (e.g.
#'   `TAR_PROJECT = "NMC"`, `config/NMC.yaml`) and in one with several, each
#'   with its settings beside its script (e.g. YT_caribou:
#'   `TAR_PROJECT = "dataprep-caribou"`,
#'   `pipelines/dataprep-caribou/config/NMC.yaml`). The targets project is
#'   `project` if `_targets.yaml` lists it, otherwise `TAR_PROJECT`; the
#'   settings are `<config_dir>/<project>.yaml`, or, if there is none, the
#'   targets project's `.yaml` or the only `.yaml` in `config_dir` (apart
#'   from `local.yaml`). `TAR_PROJECT` is not changed.
#' @return character. The zip files written.
#' @author Julie W. Turner
#' @export
write_premade <- function(project = NULL, config_dir = NULL) {
  ctx <- pipeline_context(project, config_dir)
  project <- ctx$project
  cfg <- ctx$cfg
  dir <- reproducible::checkPath(file.path(cfg$paths$outputs, project, 'premade'), create = TRUE)
  tmp <- tempfile(); reproducible::checkPath(tmp, create = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  km <- round((cfg$locations$study_area$buffer_m %||% 0) / 1000)
  names <- c(clean = paste0('clean_', project), area = sprintf('clean_%s_%skmBuff', project, km),
             extended = paste0(project, '_studyArea_extended'))

  saveRDS(targets::tar_read(locations_clean, store = ctx$store), file.path(tmp, paste0(names[['clean']], '.rds')))
  terra::writeVector(targets::tar_read(study_area, store = ctx$store), file.path(tmp, paste0(names[['area']], '.gpkg')),
                     overwrite = TRUE)
  terra::writeVector(targets::tar_read(study_area_extended, store = ctx$store),
                     file.path(tmp, paste0(names[['extended']], '.gpkg')),
                     overwrite = TRUE)
  files <- c(paste0(names[['clean']], '.rds'), paste0(names[['area']], '.gpkg'), paste0(names[['extended']], '.gpkg'))
  zips <- file.path(dir, paste0(names, '.zip'))
  for (i in seq_along(files)) {
    if (file.exists(zips[i])) file.remove(zips[i])
    utils::zip(zips[i], file.path(tmp, files[i]), flags = '-jq')
  }
  zips
}

#' @title Upload the premade files to the project's Drive folders
#' @description Puts each zip from [write_premade()] in a Drive folder of
#'   `locations: premade_folder:`: one folder for all of them, or file name
#'   patterns, each with its folder (see `upload_destinations()`), so that, for
#'   example, the cleaned locations go to a private folder and the study areas to
#'   one shared with other phases. A file of the same name already in its
#'   folder is replaced in place, so its link (e.g. in the settings) stays the
#'   same; a new name is uploaded as a new file. The cleaned locations
#'   (`clean_<project>.zip`) are caribou locations: their folder must be
#'   restricted (not "anyone with the link"), which is checked first. The study
#'   areas may go to a folder shared more widely. Uses the Google login of
#'   `run: google_email`.
#' @inheritParams write_premade
#' @details The settings and the targets store are found as in [write_premade()].
#' @param zips character. Files to upload (default: those [write_premade()] writes).
#' @param folders Drive folder link or ID, or a named list or vector of them by
#'   file name pattern, used instead of `locations: premade_folder:`.
#' @return data.table: `name`, `action` (updated or uploaded), `id`, `url`.
#' @author Julie W. Turner
#' @export
upload_premade <- function(project = NULL, zips = NULL, folders = NULL, config_dir = NULL) {
  ctx <- pipeline_context(project, config_dir)
  project <- ctx$project
  cfg <- ctx$cfg
  folders <- folders %||% cfg$locations$premade_folder
  if (!length(folders)) stop('Set locations: premade_folder: in ', file.path(ctx$config_dir, project), '.yaml', call. = FALSE)
  if (!is.null(cfg$run$google_email)) options(gargle_oauth_email = cfg$run$google_email)
  zips <- zips %||% list.files(file.path(cfg$paths$outputs, project, 'premade'), pattern = '[.]zip$', full.names = TRUE)
  dest <- upload_destinations(basename(zips), folders)
  # only the cleaned locations must stay restricted
  locs <- basename(zips) == paste0('clean_', project, '.zip')
  out <- data.table::rbindlist(purrr::map(unique(dest), function(folder) {
    msg <- if (any(locs[dest == folder])) {
      paste0('The Drive folder for ', paste0('clean_', project, '.zip'), ' (', folder,
             ') is shared with anyone with the link; caribou locations must not be. Restrict it first.')
    }
    upload_to_drive(zips[dest == folder], folder, msg)
  }))
  out[]
}

#' @title The Drive folder for each file to upload
#' @description For any upload setting that is either one Drive folder for every
#'   file, or file name patterns, each with its folder, e.g.
#'   `{clean_NMC.zip: <private folder>, "*": <shared folder>}`. Patterns are
#'   wildcards, as for file names (`*` any characters, `?` one character); a
#'   name with none is the file name itself. Patterns are tried in order and the
#'   first that matches a file gives its folder, so put `"*"` (everything else)
#'   last.
#' @param files character. File names (no folders).
#' @param folders character or list. One folder link or ID, or folders named by pattern.
#' @return character. The folder for each file, in order.
#' @author Julie W. Turner
#' @keywords internal
#' @noRd
upload_destinations <- function(files, folders) {
  folders <- unlist(folders)
  if (length(folders) == 1 && is.null(names(folders))) return(rep(unname(folders), length(files)))
  if (is.null(names(folders)) || any(!nzchar(names(folders)))) {
    stop('Give one Drive folder, or folders named by file name pattern (e.g. "*.zip")', call. = FALSE)
  }
  dest <- rep(NA_character_, length(files))
  for (pat in names(folders)) {
    hit <- is.na(dest) & grepl(utils::glob2rx(pat), files)
    dest[hit] <- folders[[pat]]
  }
  if (anyNA(dest)) {
    stop('No Drive folder for: ', paste(files[is.na(dest)], collapse = ', '),
         ' (add a pattern that matches, or "*" for everything else)', call. = FALSE)
  }
  dest
}

#' @title Upload files to a Drive folder, replacing files of the same name
#' @description A file of the same name already in the folder is replaced in
#'   place, so its link stays the same; a new name is uploaded as a new file.
#'   Unless `public_msg` is NULL, stops first if the folder is shared with anyone
#'   with the link.
#' @param files character. Files to upload.
#' @param folder character. Drive folder ID.
#' @param public_msg character. Error message if the folder is shared with anyone
#'   with the link; NULL for files that may be shared widely (no check).
#' @return data.table: `name`, `action` (updated or uploaded), `id`, `url`.
#' @keywords internal
#' @noRd
upload_to_drive <- function(files, folder, public_msg) {
  folder <- googledrive::drive_get(googledrive::as_id(folder))
  perms <- googledrive::drive_reveal(folder, 'permissions')$permissions_resource[[1]]$permissions
  if (!is.null(public_msg) && any(purrr::map_lgl(perms, function(p) identical(p$type, 'anyone')))) {
    stop(public_msg, call. = FALSE)
  }
  have <- googledrive::drive_ls(folder)
  out <- data.table::rbindlist(purrr::map(files, function(z) {
    hit <- have[have$name == basename(z), ]
    f <- if (nrow(hit) == 1) googledrive::drive_update(hit, media = z) else googledrive::drive_upload(z, path = folder)
    data.table::data.table(name = basename(z), action = if (nrow(hit) == 1) 'updated' else 'uploaded',
                           id = as.character(f$id), url = googledrive::drive_link(f))
  }))
  out[]
}
