# === Targets: iSSA models -------------------------------------------------------------
# Comparable to the caribouiSSA SpaDES module. One set of targets per model in
# `model: models:` (config/<project>.yaml), named by the model's key there,
# e.g. a model called `full` gives model_data_full, model_fit_full and
# model_coefs_full, and outputs/<project>/models/full_*.csv.

#' @title Targets: model tables, fits and coefficients, one set per named model
#' @description For each model named under `model: models:`:
#'   - `cfg_model_<name>`: its settings (without `upload:`), so editing one model
#'     refits only that one;
#'   - `seasons_<name>`: its seasons ([season_dates()]): the set named by its
#'     `season_set:`, or `model: season_set:`, so editing one set of the season
#'     table rebuilds only the models that use it;
#'   - `model_data_<name>`: its table ([build_model_data()]): only the columns
#'     its formula uses, animal IDs as codes, no positions or times;
#'   - `model_log_<name>`: what the table kept, adjusted and dropped;
#'   - `model_table_<name>_file`: with `export: true`, the table saved to
#'     outputs/<project>/models/tables/<name>_table.rds ([write_model_table()]),
#'     to fit elsewhere with [fit_model_file()] (e.g. on an HPC cluster);
#'   - `model_fit_<name>`: the glmmTMB fit ([fit_issa()]), unless the model has `fit: false`;
#'     a model's `control:` goes to [glmmTMB::glmmTMBControl()], on top of
#'     `rank_check = 'adjust'`;
#'   - `model_coefs_<name>` and `model_coefs_<name>_files`: coefficient tables
#'     ([model_coefficients()]) saved as CSV with the table's log and the
#'     model's season table ([model_season_table()]).
#'   Shared by all models: `cfg_model` (groups, distance cap), `season_table`
#'   (the season table, [read_seasons()]),
#'   `animal_codes` and `animal_codes_file` (the confidential table linking
#'   codes to animal IDs, in outputs/<project>/models/).
#' @inheritParams targets_config
#' @return list of targets (empty if no models are set).
#' @author Julie W. Turner
#' @export
targets_models <- function(cfg) {
  models <- cfg$model$models
  if (!length(models)) return(list())
  out <- list(
    tar_target(cfg_model, cfg$model[c('groups', 'distance_cap')]),
    tar_target(season_table, cfg$seasons),
    tar_target(model_dir, file.path(cfg_paths$outputs, cfg_project$name, 'models')),
    tar_target(animal_codes, make_animal_codes(steps$id)),
    tar_target(animal_codes_file, write_animal_codes(animal_codes, model_dir), format = 'file')
  )
  for (nm in names(models)) {
    cfg_nm <- as.symbol(paste0('cfg_model_', nm))
    data_nm <- as.symbol(paste0('model_data_', nm))
    fit_nm <- as.symbol(paste0('model_fit_', nm))
    coefs_nm <- as.symbol(paste0('model_coefs_', nm))
    seasons_nm <- as.symbol(paste0('seasons_', nm))
    set_nm <- models[[nm]]$season_set %||% cfg$model$season_set
    out <- c(out, list(
      # without upload:, which only says whether upload_models() shares the fit,
      # so marking a model for upload does not refit it
      tar_target_raw(as.character(cfg_nm), substitute({
        s <- cfg$model$models[[NM]]
        s[setdiff(names(s), 'upload')]
      }, list(NM = nm))),
      tar_target_raw(as.character(seasons_nm), substitute(season_dates(season_table, SET), list(SET = set_nm))),
      tar_target_raw(as.character(data_nm), substitute(
        build_model_data(steps, extracted, animal_codes, c(cfg_model, list(seasons = SEASONS)), CFG),
        list(CFG = cfg_nm, SEASONS = seasons_nm))),
      tar_target_raw(paste0('model_log_', nm), substitute(DATA$log, list(DATA = data_nm)))
    ))
    if (isTRUE(models[[nm]]$export)) {
      out <- c(out, list(tar_target_raw(paste0('model_table_', nm, '_file'), substitute(
        write_model_table(DATA, CFG, NM, file.path(model_dir, 'tables'), SEASONS),
        list(DATA = data_nm, CFG = cfg_nm, NM = nm, SEASONS = seasons_nm)),
        format = 'file')))
    }
    # fit: false builds the table only (e.g. to check it before a long fit)
    if (isFALSE(models[[nm]]$fit)) next
    # control: only in the command of a model that sets it, so adding it to one
    # model does not change (and refit) the others
    fit_cmd <- if (is.null(models[[nm]]$control)) {
      substitute(fit_issa(DATA$data, CFG$formula, step_sd = CFG$step_sd %||% 1000), list(DATA = data_nm, CFG = cfg_nm))
    } else {
      substitute(fit_issa(DATA$data, CFG$formula, step_sd = CFG$step_sd %||% 1000,
                          control = utils::modifyList(list(rank_check = 'adjust'), CFG$control)),
                 list(DATA = data_nm, CFG = cfg_nm))
    }
    out <- c(out, list(
      tar_target_raw(as.character(fit_nm), fit_cmd),
      tar_target_raw(as.character(coefs_nm), substitute(model_coefficients(FIT), list(FIT = fit_nm))),
      tar_target_raw(paste0(coefs_nm, '_files'), substitute(
        write_model_coefficients(COEFS, LOG, NM, model_dir, SEASONS, CFG$seasons %||% 'all'),
        list(COEFS = coefs_nm, LOG = as.symbol(paste0('model_log_', nm)), NM = nm,
             SEASONS = seasons_nm, CFG = cfg_nm)),
        format = 'file')
    ))
  }
  out
}
