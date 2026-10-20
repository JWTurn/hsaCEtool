# === Targets: iSSA models -------------------------------------------------------------
# Comparable to the caribouiSSA SpaDES module. One set of targets per model in
# `model: models:` (config/<project>.yaml), named by the model's key there,
# e.g. a model called `full` gives model_data_full, model_fit_full and
# model_coefs_full, and outputs/<project>/models/full_*.csv.

#' @title Targets: model tables, fits and coefficients, one set per named model
#' @description For each model named under `model: models:`:
#'   - `cfg_model_<name>`: its settings, so editing one model refits only that one;
#'   - `model_data_<name>`: its table ([build_model_data()]): only the columns
#'     its formula uses, animal IDs as codes, no positions or times;
#'   - `model_log_<name>`: what the table kept, adjusted and dropped;
#'   - `model_fit_<name>`: the glmmTMB fit ([fit_issa()]), unless the model has `fit: false`;
#'   - `model_coefs_<name>` and `model_coefs_<name>_files`: coefficient tables
#'     ([model_coefficients()]) saved as CSV with the table's log.
#'   Shared by all models: `cfg_model` (seasons, groups, distance cap),
#'   `animal_codes` and `animal_codes_file` (the confidential table linking
#'   codes to animal IDs, in outputs/<project>/models/).
#' @inheritParams targets_config
#' @return list of targets (empty if no models are set).
#' @author Julie W. Turner
targets_models <- function(cfg) {
  models <- cfg$model$models
  if (!length(models)) return(list())
  out <- list(
    tar_target(cfg_model, cfg$model[c('seasons', 'groups', 'distance_cap')]),
    tar_target(model_dir, file.path(cfg_paths$outputs, cfg_project$name, 'models')),
    tar_target(animal_codes, make_animal_codes(steps$id)),
    tar_target(animal_codes_file, write_animal_codes(animal_codes, model_dir), format = 'file')
  )
  for (nm in names(models)) {
    cfg_nm <- as.symbol(paste0('cfg_model_', nm))
    data_nm <- as.symbol(paste0('model_data_', nm))
    fit_nm <- as.symbol(paste0('model_fit_', nm))
    coefs_nm <- as.symbol(paste0('model_coefs_', nm))
    out <- c(out, list(
      tar_target_raw(as.character(cfg_nm), substitute(cfg$model$models[[NM]], list(NM = nm))),
      tar_target_raw(as.character(data_nm), substitute(
        build_model_data(steps, extracted, animal_codes, cfg_model, CFG), list(CFG = cfg_nm))),
      tar_target_raw(paste0('model_log_', nm), substitute(DATA$log, list(DATA = data_nm)))
    ))
    # fit: false builds the table only (e.g. to check it before a long fit)
    if (isFALSE(models[[nm]]$fit)) next
    out <- c(out, list(
      tar_target_raw(as.character(fit_nm), substitute(
        fit_issa(DATA$data, CFG$formula, step_sd = CFG$step_sd %||% 1000), list(DATA = data_nm, CFG = cfg_nm))),
      tar_target_raw(as.character(coefs_nm), substitute(model_coefficients(FIT), list(FIT = fit_nm))),
      tar_target_raw(paste0(coefs_nm, '_files'), substitute(
        write_model_coefficients(COEFS, LOG, NM, model_dir),
        list(COEFS = coefs_nm, LOG = as.symbol(paste0('model_log_', nm)), NM = nm)),
        format = 'file')
    ))
  }
  out
}
