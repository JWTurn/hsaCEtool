# === Premade files: cleaned locations and study areas, shared on Drive ---------
# So that later runs (or other projects and people) can start from the cleaned
# locations and study areas instead of downloading and cleaning every source
# (see locations: premade: and study_area: source: in the settings). The files
# hold caribou locations: they go only to a restricted folder.

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
#' @param project character. Project name, e.g. "NMC".
#' @return character. The zip files written.
#' @author Julie W. Turner
write_premade <- function(project = Sys.getenv('TAR_PROJECT')) {
  Sys.setenv(TAR_PROJECT = project)
  cfg <- read_config(project)
  dir <- reproducible::checkPath(file.path(cfg$paths$outputs, project, 'premade'), create = TRUE)
  tmp <- tempfile(); reproducible::checkPath(tmp, create = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  km <- round((cfg$locations$study_area$buffer_m %||% 0) / 1000)
  names <- c(clean = paste0('clean_', project), area = sprintf('clean_%s_%skmBuff', project, km),
             extended = paste0(project, '_studyArea_extended'))

  saveRDS(targets::tar_read(locations_clean), file.path(tmp, paste0(names[['clean']], '.rds')))
  terra::writeVector(targets::tar_read(study_area), file.path(tmp, paste0(names[['area']], '.gpkg')), overwrite = TRUE)
  terra::writeVector(targets::tar_read(study_area_extended), file.path(tmp, paste0(names[['extended']], '.gpkg')),
                     overwrite = TRUE)
  files <- c(paste0(names[['clean']], '.rds'), paste0(names[['area']], '.gpkg'), paste0(names[['extended']], '.gpkg'))
  zips <- file.path(dir, paste0(names, '.zip'))
  for (i in seq_along(files)) {
    if (file.exists(zips[i])) file.remove(zips[i])
    utils::zip(zips[i], file.path(tmp, files[i]), flags = '-jq')
  }
  zips
}

#' @title Upload the premade files to the project's Drive folder
#' @description Puts each zip from [write_premade()] in the Drive folder
#'   `locations: premade_folder:`. A file of the same name already there is
#'   replaced in place, so its link (e.g. in the settings) stays the same; a
#'   new name is uploaded as a new file. The folder must be restricted (not
#'   "anyone with the link"): this is checked first. Uses the Google login of
#'   `run: google_email`.
#' @param project character. Project name.
#' @param zips character. Files to upload (default: those [write_premade()] writes).
#' @return data.table: `name`, `action` (updated or uploaded), `id`, `url`.
#' @author Julie W. Turner
upload_premade <- function(project = Sys.getenv('TAR_PROJECT'), zips = NULL) {
  cfg <- read_config(project)
  if (is.null(cfg$locations$premade_folder)) stop('Set locations: premade_folder: in config/', project, '.yaml', call. = FALSE)
  if (!is.null(cfg$run$google_email)) options(gargle_oauth_email = cfg$run$google_email)
  zips <- zips %||% list.files(file.path(cfg$paths$outputs, project, 'premade'), pattern = '[.]zip$', full.names = TRUE)
  folder <- googledrive::drive_get(googledrive::as_id(cfg$locations$premade_folder))
  perms <- googledrive::drive_reveal(folder, 'permissions')$permissions_resource[[1]]$permissions
  if (any(purrr::map_lgl(perms, function(p) identical(p$type, 'anyone')))) {
    stop('The premade folder is shared with anyone with the link; caribou locations must not be. Restrict it first.',
         call. = FALSE)
  }
  have <- googledrive::drive_ls(folder)
  out <- data.table::rbindlist(purrr::map(zips, function(z) {
    hit <- have[have$name == basename(z), ]
    f <- if (nrow(hit) == 1) googledrive::drive_update(hit, media = z) else googledrive::drive_upload(z, path = folder)
    data.table::data.table(name = basename(z), action = if (nrow(hit) == 1) 'updated' else 'uploaded',
                           id = as.character(f$id), url = googledrive::drive_link(f))
  }))
  out[]
}
