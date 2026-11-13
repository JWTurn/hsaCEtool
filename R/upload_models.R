# === Fitted models: shared on Drive -------------------------------------------
# So that others can use the fitted models without running the pipeline. Each
# model marked `upload: true` (or named) goes to the Drive folder
# `model: upload_folder:` as two zips: the fit (.rds) and its coefficient
# tables (.csv). A record of what was uploaded, by the fit's and tables'
# targets fingerprints, lets a later call skip models that have not changed.
# Run by hand after a pipeline run, never as a target: uploads are slow and
# leave the project's own storage.

#' @title Upload fitted models to the project's Drive folder
#' @description For each model, writes two zips in `outputs/<project>/upload/`
#'   and puts them in the Drive folder `model: upload_folder:`:
#'   \describe{
#'     \item{<project>_model_fit_<name>.zip}{`<project>_model_fit_<name>.rds`,
#'       the glmmTMB fit (target `model_fit_<name>`)}
#'     \item{<project>_model_<name>.zip}{the coefficient tables as CSV
#'       (`<name>_fixed.csv`, `_random.csv`, `_by_group.csv`, `_data_log.csv`)
#'       and the model's season table (`_seasons.csv`, [model_season_table()])}
#'   }
#'   A file of the same name already in the folder is replaced in place, so
#'   its link stays the same. The folder may be shared widely (e.g. with
#'   anyone with the link): before anything is uploaded, every
#'   model must be fitted and up to date (its settings, seasons, table, fit
#'   and coefficient tables), and each fit's data is checked for
#'   positions, times and animal IDs that are not codes.
#'
#'   A model is skipped if it is unchanged since its last upload (same fit
#'   and tables, by their targets fingerprints), as recorded in
#'   `outputs/<project>/upload/uploaded_models.csv`. Each model is recorded as
#'   soon as both its zips are up, so a call stopped part way (e.g. by a lost
#'   connection) can simply be run again. Uses the Google login of
#'   `run: google_email`.
#' @inheritParams write_premade
#' @details The settings and the targets store are found as in [write_premade()].
#' @param models character. Models to upload; `NULL` (default) = those with
#'   `upload: true` in `model: models:`.
#' @param force logical. Upload even models unchanged since their last upload.
#' @return data.table, one row per model: `model`, `action` (uploaded or
#'   skipped), and the Drive IDs of the two zips.
#' @author Julie W. Turner
#' @export
upload_models <- function(project = NULL, models = NULL, force = FALSE, config_dir = NULL) {
  ctx <- pipeline_context(project, config_dir)
  project <- ctx$project
  cfg <- ctx$cfg
  folder <- cfg$model$upload_folder
  if (!nzchar(folder %||% '')) stop('Set model: upload_folder: in ', file.path(ctx$config_dir, project), '.yaml', call. = FALSE)
  models <- models_to_upload(cfg$model$models, models)
  if (!is.null(cfg$run$google_email)) options(gargle_oauth_email = cfg$run$google_email)

  fit_nm <- paste0('model_fit_', models)
  coefs_nm <- paste0('model_coefs_', models, '_files')
  # the model's settings, seasons, table, fit and tables, and what all models share;
  # not the steps before the model tables (shortcut), which can look out of date
  # on a computer that cannot download the locations
  check <- c('cfg_model', 'season_table', paste0(c('cfg_model_', 'seasons_', 'model_data_'), rep(models, each = 3)),
             fit_nm, paste0('model_coefs_', models), coefs_nm)
  stale <- with_tar_project(ctx$tar_project, targets::tar_outdated(
    names = targets::any_of(check), shortcut = TRUE, callr_function = NULL,
    script = ctx$script, store = ctx$store))
  if (length(stale)) {
    stop('Not up to date (run the pipeline first): ', paste(stale, collapse = ', '), call. = FALSE)
  }
  meta <- targets::tar_meta(names = targets::any_of(c(fit_nm, coefs_nm)), fields = 'data', store = ctx$store)
  hash <- stats::setNames(meta$data, meta$name)
  missing <- setdiff(c(fit_nm, coefs_nm), names(hash))
  if (length(missing)) stop('Not built yet: ', paste(missing, collapse = ', '), call. = FALSE)

  dir <- reproducible::checkPath(file.path(cfg$paths$outputs, project, 'upload'), create = TRUE)
  record_file <- file.path(dir, 'uploaded_models.csv')
  record <- read_upload_record(record_file)
  out <- list()
  for (i in seq_along(models)) {
    m <- models[i]
    h <- c(fit = hash[[fit_nm[i]]], coefs = hash[[coefs_nm[i]]])
    if (!force && !needs_upload(record, m, h)) {
      prev <- record[model == m]
      out[[m]] <- data.table::data.table(model = m, action = 'skipped', fit_id = prev$fit_id, coefs_id = prev$coefs_id)
      message(m, ': unchanged since its last upload, skipped')
      next
    }
    fit <- targets::tar_read_raw(fit_nm[i], store = ctx$store)
    check_fit_shareable(fit, m)
    zips <- write_model_zips(fit, targets::tar_read_raw(coefs_nm[i], store = ctx$store), m, project, dir)
    rm(fit); gc()
    # checked shareable above, so the folder need not be restricted
    up <- upload_to_drive(zips, folder, NULL)
    row <- data.table::data.table(model = m, fit_hash = h[['fit']], coefs_hash = h[['coefs']],
                                  fit_id = up$id[1], coefs_id = up$id[2],
                                  uploaded = format(Sys.time(), '%Y-%m-%d %H:%M:%S %Z'))
    record <- rbind(record[model != m], row)
    data.table::fwrite(record, record_file)
    out[[m]] <- data.table::data.table(model = m, action = 'uploaded', fit_id = up$id[1], coefs_id = up$id[2])
    message(m, ': uploaded')
  }
  data.table::rbindlist(out)
}

#' @title Models to upload
#' @description The models named, or those with `upload: true`; each must be a
#'   model in the settings that is fitted (not `fit: false`).
#' @param mods named list. `model: models:`.
#' @param models character or `NULL`. See [upload_models()].
#' @return character.
#' @keywords internal
#' @noRd
models_to_upload <- function(mods, models = NULL) {
  if (is.null(models)) {
    models <- names(mods)[purrr::map_lgl(mods, function(m) isTRUE(m$upload))]
    if (!length(models)) stop('No models to upload: mark some with upload: true, or name them', call. = FALSE)
  }
  unknown <- setdiff(models, names(mods))
  if (length(unknown)) stop('Not models in the settings: ', paste(unknown, collapse = ', '), call. = FALSE)
  unfit <- models[purrr::map_lgl(mods[models], function(m) isFALSE(m$fit))]
  if (length(unfit)) stop('Models with fit: false have no fit to upload: ', paste(unfit, collapse = ', '), call. = FALSE)
  models
}

#' @title Check a fit holds nothing confidential
#' @description glmmTMB keeps the data it was fitted to (`fit$frame`). Stops
#'   if it has positions or times, or animal IDs that are not codes (from
#'   [make_animal_codes()], e.g. A0001).
#' @param fit glmmTMB fit.
#' @param name character. The model's name, for the message.
#' @return `TRUE`, invisibly.
#' @keywords internal
#' @noRd
check_fit_shareable <- function(fit, name) {
  cols <- names(fit$frame)
  bad <- intersect(cols, model_private_columns)
  if (length(bad)) stop(name, ': the fit holds positions or times (', paste(bad, collapse = ', '), '); not uploaded', call. = FALSE)
  if ('id' %in% cols && !all(grepl('^A[0-9]+$', unique(as.character(fit$frame$id))))) {
    stop(name, ': the fit holds animal IDs that are not codes; not uploaded', call. = FALSE)
  }
  invisible(TRUE)
}

#' @title Zip a model's fit and coefficient tables
#' @param fit glmmTMB fit.
#' @param coef_files character. The coefficient CSVs ([write_model_coefficients()]).
#' @param name character. The model's name.
#' @param project character. Project name, for the file names.
#' @param dir character. Folder to write the zips to.
#' @return character. The two zips: fit, then coefficient tables.
#' @keywords internal
#' @noRd
write_model_zips <- function(fit, coef_files, name, project, dir) {
  tmp <- tempfile(); reproducible::checkPath(tmp, create = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  rds <- file.path(tmp, paste0(project, '_model_fit_', name, '.rds'))
  saveRDS(fit, rds)
  reproducible::checkPath(dir, create = TRUE)
  zips <- file.path(dir, paste0(project, c('_model_fit_', '_model_'), name, '.zip'))
  for (z in zips) if (file.exists(z)) file.remove(z)
  utils::zip(zips[1], rds, flags = '-jq')
  utils::zip(zips[2], coef_files, flags = '-jq')
  zips
}

#' @title Read the record of uploaded models
#' @param file character. `outputs/<project>/upload/uploaded_models.csv`.
#' @return data.table (empty if there is no record yet).
#' @keywords internal
#' @noRd
read_upload_record <- function(file) {
  cols <- c('model', 'fit_hash', 'coefs_hash', 'fit_id', 'coefs_id', 'uploaded')
  if (!file.exists(file)) {
    return(data.table::setDT(stats::setNames(rep(list(character()), length(cols)), cols)))
  }
  data.table::fread(file, colClasses = 'character')[, cols, with = FALSE]
}

#' @title Has a model changed since its last upload?
#' @param record data.table from `read_upload_record()`.
#' @param name character. The model's name.
#' @param hash character. Named `fit` and `coefs`: the targets fingerprints now.
#' @return logical. `TRUE` if never uploaded or either fingerprint differs.
#' @keywords internal
#' @noRd
needs_upload <- function(record, name, hash) {
  prev <- record[record$model == name]
  nrow(prev) == 0 || !identical(prev$fit_hash[1], hash[['fit']]) || !identical(prev$coefs_hash[1], hash[['coefs']])
}
