# === Layer stacks: shared on Drive ----------------------------------------------
# So that the stacks can be used to map the models (e.g. in caribou_SSUD)
# without running the pipeline. Each year in `model_land: upload_years:` goes
# to the Drive folder `model_land: upload_folder:` as a zip of its stack and
# the list of its layers. A record of what was uploaded, by the stack's
# targets fingerprint, lets a later call skip years that have not changed.
# Run by hand after a pipeline run, never as a target: uploads are slow and
# leave the project's own storage.

#' @title Upload layer stacks to the project's Drive folder
#' @description For each year, writes `<project>_model_land_<year>.zip` in
#'   `outputs/<project>/upload/`, holding `model_land_<year>.tif` (target
#'   `model_land`, see [make_model_land()]) and `model_land_layers.csv` (what
#'   each layer is, [write_model_land_spec()]), and puts it in the Drive folder
#'   `model_land: upload_folder:`. A file of the same name already in the
#'   folder is replaced in place, so its link stays the same. The stacks hold
#'   landscape layers only (no caribou locations). The stacks and their layer
#'   list must be up to date.
#'
#'   A year is skipped if its stack is unchanged since its last upload (by its
#'   targets fingerprint), as recorded in
#'   `outputs/<project>/upload/uploaded_model_land.csv`. Each year is recorded
#'   as soon as its zip is up, so a call stopped part way can simply be run
#'   again. Uses the Google login of `run: google_email`.
#' @inheritParams upload_models
#' @param years integer. Years to upload; `NULL` (default) =
#'   `model_land: upload_years:`, or all of `model_land: years:` if that is empty.
#' @param force logical. Upload even stacks unchanged since their last upload.
#' @return data.table, one row per year: `year`, `action` (uploaded or
#'   skipped), and the Drive ID of the zip.
#' @author Julie W. Turner
#' @export
upload_model_land <- function(project = NULL, years = NULL, force = FALSE, config_dir = NULL) {
  ctx <- pipeline_context(project, config_dir)
  project <- ctx$project
  cfg <- ctx$cfg
  ml <- cfg$model_land
  folder <- ml$upload_folder
  if (!nzchar(folder %||% '')) stop('Set model_land: upload_folder: in ', file.path(ctx$config_dir, project), '.yaml', call. = FALSE)
  years <- model_land_upload_years(ml, years)
  if (!is.null(cfg$run$google_email)) options(gargle_oauth_email = cfg$run$google_email)

  # not the steps before (shortcut), which can look out of date on a computer
  # that cannot download the locations
  check <- c('cfg_model_land', 'model_land_spec', 'model_land_years', 'model_land_layers_file', 'model_land')
  stale <- with_tar_project(ctx$tar_project, targets::tar_outdated(
    names = targets::any_of(check), shortcut = TRUE, callr_function = NULL,
    script = ctx$script, store = ctx$store))
  if (length(stale)) stop('Not up to date (run the pipeline first): ', paste(stale, collapse = ', '), call. = FALSE)
  stacks <- model_land_branches(targets::tar_meta(fields = c('data', 'path', 'parent'), store = ctx$store))
  missing <- setdiff(years, stacks$year)
  if (length(missing)) stop('No stack built for: ', paste(missing, collapse = ', '), call. = FALSE)
  layers_csv <- targets::tar_read_raw('model_land_layers_file', store = ctx$store)

  dir <- reproducible::checkPath(file.path(cfg$paths$outputs, project, 'upload'), create = TRUE)
  record_file <- file.path(dir, 'uploaded_model_land.csv')
  record <- read_model_land_record(record_file)
  out <- list()
  for (y in years) {
    st <- stacks[year == y]
    prev <- record[record$year == as.character(y)]
    if (!force && nrow(prev) && identical(prev$hash[1], st$hash)) {
      out[[as.character(y)]] <- data.table::data.table(year = y, action = 'skipped', id = prev$id[1])
      message(y, ': unchanged since its last upload, skipped')
      next
    }
    zip <- write_model_land_zip(st$path, layers_csv, y, project, dir)
    up <- upload_to_drive(zip, folder, NULL)
    row <- data.table::data.table(year = as.character(y), hash = st$hash, id = up$id[1],
                                  uploaded = format(Sys.time(), '%Y-%m-%d %H:%M:%S %Z'))
    record <- rbind(record[year != as.character(y)], row)
    data.table::fwrite(record, record_file)
    out[[as.character(y)]] <- data.table::data.table(year = y, action = 'uploaded', id = up$id[1])
    message(y, ': uploaded')
  }
  data.table::rbindlist(out)
}

#' @title Years to upload
#' @param ml named list. `model_land:` settings.
#' @param years integer or NULL. See [upload_model_land()].
#' @return integer.
#' @keywords internal
#' @noRd
model_land_upload_years <- function(ml, years = NULL) {
  have <- as.integer(unlist(ml$years))
  years <- as.integer(unlist(years %||% ml$upload_years))
  if (!length(years)) years <- have
  if (!length(years)) stop('No years to upload: set model_land: years: (and upload_years:)', call. = FALSE)
  bad <- setdiff(years, have)
  if (length(bad)) stop('Not in model_land: years: ', paste(bad, collapse = ', '), call. = FALSE)
  years
}

#' @title The built stacks, from the targets metadata
#' @param meta data.frame from [targets::tar_meta()] with `name`, `data`,
#'   `path` and `parent`.
#' @return data.table: `year`, `path`, `hash` (the branch's fingerprint).
#' @keywords internal
#' @noRd
model_land_branches <- function(meta) {
  m <- data.table::as.data.table(meta)[parent %in% 'model_land']
  m[, path := purrr::map_chr(path, function(p) as.character(p)[1])]
  m <- m[grepl('^model_land_[0-9]{4}\\.tif$', basename(path))]
  m[, .(year = as.integer(sub('^model_land_([0-9]{4})\\.tif$', '\\1', basename(path))), path, hash = data)]
}

#' @title Zip a year's stack with its list of layers
#' @param stack character. `model_land_<year>.tif`.
#' @param layers_csv character. `model_land_layers.csv`.
#' @param year integer.
#' @param project character. Project name, for the file name.
#' @param dir character. Folder to write the zip to.
#' @return character. The zip.
#' @keywords internal
#' @noRd
write_model_land_zip <- function(stack, layers_csv, year, project, dir) {
  reproducible::checkPath(dir, create = TRUE)
  zip <- file.path(dir, paste0(project, '_model_land_', year, '.zip'))
  if (file.exists(zip)) file.remove(zip)
  utils::zip(zip, c(stack, layers_csv), flags = '-jq')
  zip
}

#' @title Read the record of uploaded stacks
#' @param file character. `outputs/<project>/upload/uploaded_model_land.csv`.
#' @return data.table (empty if there is no record yet).
#' @keywords internal
#' @noRd
read_model_land_record <- function(file) {
  cols <- c('year', 'hash', 'id', 'uploaded')
  if (!file.exists(file)) {
    return(data.table::setDT(stats::setNames(rep(list(character()), length(cols)), cols)))
  }
  data.table::fread(file, colClasses = 'character')[, cols, with = FALSE]
}
